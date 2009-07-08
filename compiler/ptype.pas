{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Does parsing types for Free Pascal

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit ptype;

{$i fpcdefs.inc}

interface

    uses
       globtype,cclasses,
       symtype,symdef,symbase;

    var
       { hack, which allows to use the current parsed }
       { object type as function argument type  }
       testcurobject : byte;

    procedure resolve_forward_types;

    { reads a type identifier }
    procedure id_type(var def : tdef;isforwarddef:boolean);

    { reads a string, file type or a type identifier }
    procedure single_type(var def:tdef;isforwarddef,allowtypedef:boolean);

    { reads any type declaration, where the resulting type will get name as type identifier }
    procedure read_named_type(var def:tdef;const name : TIDString;genericdef:tstoreddef;genericlist:TFPObjectList;parseprocvardir:boolean);

    { reads any type declaration }
    procedure read_anon_type(var def : tdef;parseprocvardir:boolean);

    { generate persistent type information like VMT, RTTI and inittables }
    procedure write_persistent_type_info(st:tsymtable);


implementation

    uses
       { common }
       cutils,
       { global }
       globals,tokens,verbose,constexp,
       systems,
       { target }
       paramgr,procinfo,
       { symtable }
       symconst,symsym,symtable,
       defutil,defcmp,
       { modules }
       fmodule,
       { pass 1 }
       node,ncgrtti,nobj,
       nmat,nadd,ncal,nset,ncnv,ninl,ncon,nld,nflw,
       { parser }
       scanner,
       pbase,pexpr,pdecsub,pdecvar,pdecobj;


    procedure resolve_forward_types;
      var
        i: longint;
        hpd,
        def : tdef;
        srsym  : tsym;
        srsymtable : TSymtable;
        hs : string;
      begin
        for i:=0 to current_module.checkforwarddefs.Count-1 do
          begin
            def:=tdef(current_module.checkforwarddefs[i]);
            case def.typ of
              pointerdef,
              classrefdef :
                begin
                  { classrefdef inherits from pointerdef }
                  hpd:=tabstractpointerdef(def).pointeddef;
                  { still a forward def ? }
                  if hpd.typ=forwarddef then
                   begin
                     { try to resolve the forward }
                     if not assigned(tforwarddef(hpd).tosymname) then
                       internalerror(200211201);
                     hs:=tforwarddef(hpd).tosymname^;
                     searchsym(upper(hs),srsym,srsymtable);
                     { we don't need the forwarddef anymore, dispose it }
                     hpd.free;
                     tabstractpointerdef(def).pointeddef:=nil; { if error occurs }
                     { was a type sym found ? }
                     if assigned(srsym) and
                        (srsym.typ=typesym) then
                      begin
                        tabstractpointerdef(def).pointeddef:=ttypesym(srsym).typedef;
                        { avoid wrong unused warnings web bug 801 PM }
                        inc(ttypesym(srsym).refs);
                        { we need a class type for classrefdef }
                        if (def.typ=classrefdef) and
                           not(is_class(ttypesym(srsym).typedef)) then
                          MessagePos1(tsym(srsym).fileinfo,type_e_class_type_expected,ttypesym(srsym).typedef.typename);
                      end
                     else
                      begin
                        Message1(sym_e_forward_type_not_resolved,hs);
                        { try to recover }
                        tabstractpointerdef(def).pointeddef:=generrordef;
                      end;
                   end;
                end;
              objectdef :
                begin
                  { give an error as the implementation may follow in an
                    other type block which is allowed by FPC modes }
                  if not(m_fpc in current_settings.modeswitches) and
                     (oo_is_forward in tobjectdef(def).objectoptions) then
                    MessagePos1(def.typesym.fileinfo,type_e_type_is_not_completly_defined,def.typename);
                 end;
              else
                internalerror(200811071);
            end;
          end;
        current_module.checkforwarddefs.clear;
      end;



    procedure generate_specialization(var tt:tdef);
      var
        st  : TSymtable;
        srsym : tsym;
        pt2 : tnode;
        first,
        err : boolean;
        i   : longint;
        sym : tsym;
        genericdef : tstoreddef;
        generictype : ttypesym;
        generictypelist : TFPObjectList;
        oldsymtablestack   : tsymtablestack;
        hmodule : tmodule;
        pu : tused_unit;
        uspecializename,
        specializename : string;
        vmtbuilder : TVMTBuilder;
        onlyparsepara : boolean;
        specializest : tsymtable;
      begin
        { retrieve generic def that we are going to replace }
        genericdef:=tstoreddef(tt);
        tt:=nil;
        onlyparsepara:=false;

        if not(df_generic in genericdef.defoptions) then
          begin
            Message(parser_e_special_onlygenerics);
            tt:=generrordef;
            onlyparsepara:=true;
          end;

        { Only need to record the tokens, then we don't know the type yet }
        if parse_generic then
          begin
            tt:=cundefinedtype;
            onlyparsepara:=true;
          end;

        { Only parse the parameters for recovery or
          for recording in genericbuf }
        if onlyparsepara then
          begin
            consume(_LSHARPBRACKET);
            repeat
              pt2:=factor(false);
              pt2.free;
            until not try_to_consume(_COMMA);
            consume(_RSHARPBRACKET);
            exit;
          end;

        consume(_LSHARPBRACKET);
        { Parse generic parameters, for each undefineddef in the symtable of
          the genericdef we need to have a new def }
        err:=false;
        first:=true;
        generictypelist:=TFPObjectList.create(false);
        case genericdef.typ of
          procdef :
            st:=genericdef.GetSymtable(gs_para);
          objectdef,
          recorddef :
            st:=genericdef.GetSymtable(gs_record);
        end;
        if not assigned(st) then
          internalerror(200511182);

        { Parse type parameters }
        if not assigned(genericdef.typesym) then
          internalerror(200710173);
        specializename:=genericdef.typesym.realname;
        for i:=0 to st.SymList.Count-1 do
          begin
            sym:=tsym(st.SymList[i]);
            if (sp_generic_para in sym.symoptions) then
              begin
                if not first then
                  consume(_COMMA)
                else
                  first:=false;
                pt2:=factor(false);
                if pt2.nodetype=typen then
                  begin
                    if df_generic in pt2.resultdef.defoptions then
                      Message(parser_e_no_generics_as_params);
                    generictype:=ttypesym.create(sym.realname,pt2.resultdef);
                    generictypelist.add(generictype);
                    if not assigned(pt2.resultdef.typesym) then
                      internalerror(200710172);
                    specializename:=specializename+'$'+pt2.resultdef.typesym.realname;
                  end
                else
                  begin
                    Message(type_e_type_id_expected);
                    err:=true;
                  end;
                pt2.free;
              end;
          end;
        uspecializename:=upper(specializename);
        { force correct error location if too much type parameters are passed }
        if token<>_RSHARPBRACKET then
          consume(_RSHARPBRACKET);

        { Special case if we are referencing the current defined object }
        if assigned(current_objectdef) and
           (current_objectdef.objname^=uspecializename) then
          tt:=current_objectdef;

        { for units specializations can already be needed in the interface, therefor we
          will use the global symtable. Programs don't have a globalsymtable and there we
          use the localsymtable }
        if current_module.is_unit then
          specializest:=current_module.globalsymtable
        else
          specializest:=current_module.localsymtable;

        { Can we reuse an already specialized type? }
        if not assigned(tt) then
          begin
            srsym:=tsym(specializest.find(uspecializename));
            if assigned(srsym) then
              begin
                if srsym.typ<>typesym then
                  internalerror(200710171);
                tt:=ttypesym(srsym).typedef;
              end;
          end;

        if not assigned(tt) then
          begin
            { Setup symtablestack at definition time
              to get types right, however this is not perfect, we should probably record
              the resolved symbols }
            oldsymtablestack:=symtablestack;
            symtablestack:=tsymtablestack.create;
            if not assigned(genericdef) then
              internalerror(200705151);
            hmodule:=find_module_from_symtable(genericdef.owner);
            if hmodule=nil then
              internalerror(200705152);
            pu:=tused_unit(hmodule.used_units.first);
            while assigned(pu) do
              begin
                if not assigned(pu.u.globalsymtable) then
                  internalerror(200705153);
                symtablestack.push(pu.u.globalsymtable);
                pu:=tused_unit(pu.next);
              end;

            if assigned(hmodule.globalsymtable) then
              symtablestack.push(hmodule.globalsymtable);

            { hacky, but necessary to insert the newly generated class properly }
            symtablestack.push(oldsymtablestack.top);

            { Reparse the original type definition }
            if not err then
              begin
                { Firsta new typesym so we can reuse this specialization and
                  references to this specialization can be handled }
                srsym:=ttypesym.create(specializename,generrordef);
                specializest.insert(srsym);

                if not assigned(genericdef.generictokenbuf) then
                  internalerror(200511171);
                current_scanner.startreplaytokens(genericdef.generictokenbuf);
                read_named_type(tt,specializename,genericdef,generictypelist,false);
                ttypesym(srsym).typedef:=tt;
                tt.typesym:=srsym;
                { Consume the semicolon if it is also recorded }
                try_to_consume(_SEMICOLON);

                { Build VMT indexes for classes }
                if (tt.typ=objectdef) then
                  begin
                    vmtbuilder:=TVMTBuilder.Create(tobjectdef(tt));
                    vmtbuilder.generate_vmt;
                    vmtbuilder.free;
                  end;
              end;

            { Restore symtablestack }
            symtablestack.free;
            symtablestack:=oldsymtablestack;
          end;

        generictypelist.free;
        consume(_RSHARPBRACKET);
      end;


    procedure id_type(var def : tdef;isforwarddef:boolean);
    { reads a type definition }
    { to a appropriating tdef, s gets the name of   }
    { the type to allow name mangling          }
      var
        is_unit_specific : boolean;
        pos : tfileposinfo;
        srsym : tsym;
        srsymtable : TSymtable;
        s,sorg : TIDString;
        t : ttoken;
      begin
         s:=pattern;
         sorg:=orgpattern;
         pos:=current_tokenpos;
         { use of current parsed object:
            - classes can be used also in classes
            - objects can be parameters }
         if assigned(current_objectdef) and
            (current_objectdef.objname^=pattern) and
            (
             (testcurobject=2) or
             is_class_or_interface(current_objectdef)
            )then
           begin
             consume(_ID);
             def:=current_objectdef;
             exit;
           end;
         { Use the special searchsym_type that ignores records,objects and
           parameters }
         searchsym_type(s,srsym,srsymtable);
         { handle unit specification like System.Writeln }
         is_unit_specific:=try_consume_unitsym(srsym,srsymtable,t);
         consume(t);
         { Types are first defined with an error def before assigning
           the real type so check if it's an errordef. if so then
           give an error. Only check for typesyms in the current symbol
           table as forwarddef are not resolved directly }
         if assigned(srsym) and
            (srsym.typ=typesym) and
            (ttypesym(srsym).typedef.typ=errordef) then
          begin
            Message1(type_e_type_is_not_completly_defined,ttypesym(srsym).realname);
            def:=generrordef;
            exit;
          end;
         { are we parsing a possible forward def ? }
         if isforwarddef and
            not(is_unit_specific) then
           begin
             def:=tforwarddef.create(sorg,pos);
             exit;
           end;
         { unknown sym ? }
         if not assigned(srsym) then
          begin
            Message1(sym_e_id_not_found,sorg);
            def:=generrordef;
            exit;
          end;
         { type sym ? }
         if (srsym.typ<>typesym) then
          begin
            Message(type_e_type_id_expected);
            def:=generrordef;
            exit;
          end;
         { Give an error when referring to an errordef }
         if (ttypesym(srsym).typedef.typ=errordef) then
          begin
            Message(sym_e_error_in_type_def);
            def:=generrordef;
            exit;
          end;
        def:=ttypesym(srsym).typedef;
      end;


    procedure single_type(var def:tdef;isforwarddef,allowtypedef:boolean);
       var
         t2 : tdef;
         dospecialize,
         again : boolean;
       begin
         dospecialize:=false;
         repeat
           again:=false;
             case token of
               _STRING:
                 string_dec(def,allowtypedef);

               _FILE:
                 begin
                    consume(_FILE);
                    if (token=_OF) then
                      begin
                         if not(allowtypedef) then
                           Message(parser_e_no_local_para_def);
                         consume(_OF);
                         single_type(t2,false,false);
                         def:=tfiledef.createtyped(t2);
                      end
                    else
                      def:=cfiletype;
                 end;

               _ID:
                 begin
                   if try_to_consume(_SPECIALIZE) then
                     begin
                       dospecialize:=true;
                       again:=true;
                     end
                   else
                     begin
                       id_type(def,isforwarddef);
                       { handle types inside classes for generics, e.g. TNode.TLongint }
                       while (token=_POINT) do
                         begin
                           if parse_generic then
                             begin
                                consume(_POINT);
                                consume(_ID);
                             end
                            else if ((def.typ=objectdef) and (df_specialization in def.defoptions)) then
                              begin
                                symtablestack.push(tobjectdef(def).symtable);
                                consume(_POINT);
                                id_type(t2,isforwarddef);
                                symtablestack.pop(tobjectdef(def).symtable);
                                def:=t2;
                              end
                            else
                              break;
                         end;
                     end;
                 end;

               else
                 begin
                   message(type_e_type_id_expected);
                   def:=generrordef;
                 end;
            end;
        until not again;
        if dospecialize then
          generate_specialization(def)
        else
          begin
            if (df_generic in def.defoptions)  then
              begin
                Message(parser_e_no_generics_as_types);
                def:=generrordef;
              end;
          end;
      end;

    { reads a record declaration }
    function record_dec : tdef;
      var
         recst : trecordsymtable;
      begin
         { create recdef }
         recst:=trecordsymtable.create(current_settings.packrecords);
         record_dec:=trecorddef.create(recst);
         { insert in symtablestack }
         symtablestack.push(recst);
         { parse record }
         consume(_RECORD);
         read_record_fields([vd_record]);
         consume(_END);
         { make the record size aligned }
         recst.addalignmentpadding;
         { restore symtable stack }
         symtablestack.pop(recst);
         if trecorddef(record_dec).is_packed and
            record_dec.needs_inittable then
           Message(type_e_no_packed_inittable);
      end;


    { reads a type definition and returns a pointer to it }
    procedure read_named_type(var def : tdef;const name : TIDString;genericdef:tstoreddef;genericlist:TFPObjectList;parseprocvardir:boolean);
      var
        pt : tnode;
        tt2 : tdef;
        aktenumdef : tenumdef;
        s : TIDString;
        l,v : TConstExprInt;
        oldpackrecords : longint;
        defpos,storepos : tfileposinfo;

        procedure expr_type;
        var
           pt1,pt2 : tnode;
           lv,hv   : TConstExprInt;
           old_block_type : tblock_type;
           dospecialize : boolean;
        begin
           old_block_type:=block_type;
           dospecialize:=false;
           { use of current parsed object:
              - classes can be used also in classes
              - objects can be parameters }
           if (token=_ID) and
              assigned(current_objectdef) and
              (current_objectdef.objname^=pattern) and
              (
               (testcurobject=2) or
               is_class_or_interface(current_objectdef)
              )then
             begin
               consume(_ID);
               def:=current_objectdef;
               exit;
             end;
           { Generate a specialization? }
           if try_to_consume(_SPECIALIZE) then
             dospecialize:=true;
           { we can't accept a equal in type }
           pt1:=comp_expr(false);
           if not dospecialize and
              try_to_consume(_POINTPOINT) then
             begin
               { get high value of range }
               pt2:=comp_expr(false);
               { make both the same type or give an error. This is not
                 done when both are integer values, because typecasting
                 between -3200..3200 will result in a signed-unsigned
                 conflict and give a range check error (PFV) }
               if not(is_integer(pt1.resultdef) and is_integer(pt2.resultdef)) then
                 inserttypeconv(pt1,pt2.resultdef);
               { both must be evaluated to constants now }
               if (pt1.nodetype=ordconstn) and
                  (pt2.nodetype=ordconstn) then
                 begin
                   lv:=tordconstnode(pt1).value;
                   hv:=tordconstnode(pt2).value;
                   { Check bounds }
                   if hv<lv then
                     message(parser_e_upper_lower_than_lower)
                   else if (lv.signed and (lv.svalue<0)) and (not hv.signed and (hv.uvalue>qword(high(int64)))) then
                     message(type_e_cant_eval_constant_expr)
                   else
                     begin
                       { All checks passed, create the new def }
                       case pt1.resultdef.typ of
                         enumdef :
                           def:=tenumdef.create_subrange(tenumdef(pt1.resultdef),lv.svalue,hv.svalue);
                         orddef :
                           begin
                             if is_char(pt1.resultdef) then
                               def:=torddef.create(uchar,lv,hv)
                             else
                               if is_boolean(pt1.resultdef) then
                                 def:=torddef.create(pasbool,lv,hv)
                               else if is_signed(pt1.resultdef) then
                                 def:=torddef.create(range_to_basetype(lv,hv),lv,hv)
                               else
                                 def:=torddef.create(range_to_basetype(lv,hv),lv,hv);
                           end;
                       end;
                     end;
                 end
               else
                 Message(sym_e_error_in_type_def);
               pt2.free;
             end
           else
             begin
               { a simple type renaming or generic specialization }
               if (pt1.nodetype=typen) then
                 begin
                   def:=ttypenode(pt1).resultdef;
                   if dospecialize then
                     generate_specialization(def)
                   else
                     begin
                       if (df_generic in def.defoptions)  then
                         begin
                           Message(parser_e_no_generics_as_types);
                           def:=generrordef;
                         end;
                     end;
                 end
               else
                 Message(sym_e_error_in_type_def);
             end;
           pt1.free;
           block_type:=old_block_type;
        end;


      procedure set_dec;
        begin
          consume(_SET);
          consume(_OF);
          read_anon_type(tt2,true);
          if assigned(tt2) then
           begin
             case tt2.typ of
               { don't forget that min can be negativ  PM }
               enumdef :
                 if (tenumdef(tt2).min>=0) and
                    (tenumdef(tt2).max<=255) then
                  // !! def:=tsetdef.create(tt2,tenumdef(tt2.def).min,tenumdef(tt2.def).max))
                  def:=tsetdef.create(tt2,tenumdef(tt2).min,tenumdef(tt2).max)
                 else
                  Message(sym_e_ill_type_decl_set);
               orddef :
                 begin
                   if (torddef(tt2).ordtype<>uvoid) and
                      (torddef(tt2).ordtype<>uwidechar) and
                      (torddef(tt2).low>=0) then
                     // !! def:=tsetdef.create(tt2,torddef(tt2.def).low,torddef(tt2.def).high))
                     if Torddef(tt2).high>int64(high(byte)) then
                       message(sym_e_ill_type_decl_set)
                     else
                       def:=tsetdef.create(tt2,torddef(tt2).low.svalue,torddef(tt2).high.svalue)
                   else
                     Message(sym_e_ill_type_decl_set);
                 end;
               else
                 Message(sym_e_ill_type_decl_set);
             end;
           end
          else
           def:=generrordef;
        end;


      procedure array_dec(is_packed: boolean);
        var
          lowval,
          highval   : TConstExprInt;
          indexdef  : tdef;
          hdef      : tdef;
          arrdef    : tarraydef;

        procedure setdefdecl(def:tdef);
          begin
            case def.typ of
              enumdef :
                begin
                  lowval:=tenumdef(def).min;
                  highval:=tenumdef(def).max;
                  if (m_fpc in current_settings.modeswitches) and
                     (tenumdef(def).has_jumps) then
                   Message(type_e_array_index_enums_with_assign_not_possible);
                  indexdef:=def;
                end;
              orddef :
                begin
                  if torddef(def).ordtype in [uchar,
                    u8bit,u16bit,
                    s8bit,s16bit,s32bit,
{$ifdef cpu64bitaddr}
                    u32bit,s64bit,
{$endif cpu64bitaddr}
                    pasbool,bool8bit,bool16bit,bool32bit,bool64bit,
                    uwidechar] then
                    begin
                       lowval:=torddef(def).low;
                       highval:=torddef(def).high;
                       indexdef:=def;
                    end
                  else
                    Message1(parser_e_type_cant_be_used_in_array_index,def.GetTypeName);
                end;
              else
                Message(sym_e_error_in_type_def);
            end;
          end;

        begin
           arrdef:=nil;
           consume(_ARRAY);
           { open array? }
           if try_to_consume(_LECKKLAMMER) then
             begin
                { defaults }
                indexdef:=generrordef;
                { use defaults which don't overflow the compiler }
                lowval:=0;
                highval:=0;
                repeat
                  { read the expression and check it, check apart if the
                    declaration is an enum declaration because that needs to
                    be parsed by readtype (PFV) }
                  if token=_LKLAMMER then
                   begin
                     read_anon_type(hdef,true);
                     setdefdecl(hdef);
                   end
                  else
                   begin
                     pt:=expr;
                     if pt.nodetype=typen then
                       setdefdecl(pt.resultdef)
                     else
                       begin
                         if (pt.nodetype=rangen) then
                           begin
                             if (trangenode(pt).left.nodetype=ordconstn) and
                                (trangenode(pt).right.nodetype=ordconstn) then
                               begin
                                 { make both the same type or give an error. This is not
                                   done when both are integer values, because typecasting
                                   between -3200..3200 will result in a signed-unsigned
                                   conflict and give a range check error (PFV) }
                                 if not(is_integer(trangenode(pt).left.resultdef) and is_integer(trangenode(pt).left.resultdef)) then
                                   inserttypeconv(trangenode(pt).left,trangenode(pt).right.resultdef);
                                 lowval:=tordconstnode(trangenode(pt).left).value;
                                 highval:=tordconstnode(trangenode(pt).right).value;
                                 if highval<lowval then
                                  begin
                                    Message(parser_e_array_lower_less_than_upper_bound);
                                    highval:=lowval;
                                  end
                                 else if (lowval<int64(low(aint))) or
                                         (highval > high(aint)) then
                                   begin
                                     Message(parser_e_array_range_out_of_bounds);
                                     lowval :=0;
                                     highval:=0;
                                   end;
                                 if is_integer(trangenode(pt).left.resultdef) then
                                   range_to_type(lowval,highval,indexdef)
                                 else
                                   indexdef:=trangenode(pt).left.resultdef;
                               end
                             else
                               Message(type_e_cant_eval_constant_expr);
                           end
                         else
                           Message(sym_e_error_in_type_def)
                       end;
                     pt.free;
                   end;

                  { if the array is already created add the new arrray
                    as element of the existing array, otherwise create a new array }
                  if assigned(arrdef) then
                    begin
                      arrdef.elementdef:=tarraydef.create(lowval.svalue,highval.svalue,indexdef);
                      arrdef:=tarraydef(arrdef.elementdef);
                    end
                  else
                    begin
                      arrdef:=tarraydef.create(lowval.svalue,highval.svalue,indexdef);
                      def:=arrdef;
                    end;
                  if is_packed then
                    include(arrdef.arrayoptions,ado_IsBitPacked);

                  if token=_COMMA then
                    consume(_COMMA)
                  else
                    break;
                until false;
                consume(_RECKKLAMMER);
             end
           else
             begin
                if is_packed then
                  Message(parser_e_packed_dynamic_open_array);
                arrdef:=tarraydef.create(0,-1,s32inttype);
                include(arrdef.arrayoptions,ado_IsDynamicArray);
                def:=arrdef;
             end;
           consume(_OF);
           read_anon_type(tt2,true);
           { set element type of the last array definition }
           if assigned(arrdef) then
             begin
               arrdef.elementdef:=tt2;
               if is_packed and
                  tt2.needs_inittable then
                 Message(type_e_no_packed_inittable);
             end;
        end;

      var
        p  : tnode;
        hdef : tdef;
        pd : tabstractprocdef;
        is_func,
        enumdupmsg, first : boolean;
        newtype    : ttypesym;
        oldlocalswitches : tlocalswitches;
        bitpacking: boolean;
      begin
         def:=nil;
         case token of
            _STRING,_FILE:
              begin
                single_type(def,false,true);
              end;
           _LKLAMMER:
              begin
                consume(_LKLAMMER);
                first := true;
                { allow negativ value_str }
                l:=int64(-1);
                enumdupmsg:=false;
                aktenumdef:=tenumdef.create;
                repeat
                  s:=orgpattern;
                  defpos:=current_tokenpos;
                  consume(_ID);
                  { only allow assigning of specific numbers under fpc mode }
                  if not(m_tp7 in current_settings.modeswitches) and
                     (
                      { in fpc mode also allow := to be compatible
                        with previous 1.0.x versions }
                      ((m_fpc in current_settings.modeswitches) and
                       try_to_consume(_ASSIGNMENT)) or
                      try_to_consume(_EQUAL)
                     ) then
                    begin
                       oldlocalswitches:=current_settings.localswitches;
                       include(current_settings.localswitches,cs_allow_enum_calc);
                       p:=comp_expr(true);
                       current_settings.localswitches:=oldlocalswitches;
                       if (p.nodetype=ordconstn) then
                        begin
                          { we expect an integer or an enum of the
                            same type }
                          if is_integer(p.resultdef) or
                             is_char(p.resultdef) or
                             equal_defs(p.resultdef,aktenumdef) then
                           v:=tordconstnode(p).value
                          else
                           IncompatibleTypes(p.resultdef,s32inttype);
                        end
                       else
                        Message(parser_e_illegal_expression);
                       p.free;
                       { please leave that a note, allows type save }
                       { declarations in the win32 units ! }
                       if (not first) and (v<=l) and (not enumdupmsg) then
                        begin
                          Message(parser_n_duplicate_enum);
                          enumdupmsg:=true;
                        end;
                       l:=v;
                    end
                  else
                    inc(l.svalue);
                  first := false;
                  storepos:=current_tokenpos;
                  current_tokenpos:=defpos;
                  tstoredsymtable(aktenumdef.owner).insert(tenumsym.create(s,aktenumdef,longint(l.svalue)));
                  current_tokenpos:=storepos;
                until not try_to_consume(_COMMA);
                def:=aktenumdef;
                consume(_RKLAMMER);
              end;
            _ARRAY:
              begin
                array_dec(false);
              end;
            _SET:
              begin
                set_dec;
              end;
           _CARET:
              begin
                consume(_CARET);
                single_type(tt2,(block_type=bt_type),false);
                def:=tpointerdef.create(tt2);
                if tt2.typ=forwarddef then
                  current_module.checkforwarddefs.add(def);
              end;
            _RECORD:
              begin
                def:=record_dec;
              end;
            _PACKED,
            _BITPACKED:
              begin
                bitpacking :=
                  (cs_bitpacking in current_settings.localswitches) or
                  (token = _BITPACKED);
                consume(token);
                if token=_ARRAY then
                  array_dec(bitpacking)
                else if token=_SET then
                  set_dec
                else
                  begin
                    oldpackrecords:=current_settings.packrecords;
                    if (not bitpacking) or
                       (token in [_CLASS,_OBJECT]) then
                      current_settings.packrecords:=1
                    else
                      current_settings.packrecords:=bit_alignment;
                    case token of
                      _CLASS :
                        begin
                          consume(_CLASS);
                          def:=object_dec(odt_class,name,genericdef,genericlist,nil);
                        end;
                      _OBJECT :
                        begin
                          consume(_OBJECT);
                          def:=object_dec(odt_object,name,genericdef,genericlist,nil);
                        end;
                      else
                        def:=record_dec;
                    end;
                    current_settings.packrecords:=oldpackrecords;
                  end;
              end;
            _DISPINTERFACE :
              begin
                { need extra check here since interface is a keyword
                  in all pascal modes }
                if not(m_class in current_settings.modeswitches) then
                  Message(parser_f_need_objfpc_or_delphi_mode);
                consume(token);
                def:=object_dec(odt_dispinterface,name,genericdef,genericlist,nil);
              end;
            _CLASS :
              begin
                consume(token);
                { Delphi only allows class of in type blocks }
                if (token=_OF) and
                   (
                    not(m_delphi in current_settings.modeswitches) or
                    (block_type=bt_type)
                   ) then
                  begin
                    consume(_OF);
                    single_type(hdef,(block_type=bt_type),false);
                    if is_class(hdef) then
                      def:=tclassrefdef.create(hdef)
                    else
                      if hdef.typ=forwarddef then
                        begin
                          def:=tclassrefdef.create(hdef);
                          current_module.checkforwarddefs.add(def);
                        end
                    else
                      Message1(type_e_class_type_expected,hdef.typename);
                  end
                else
                  def:=object_dec(odt_class,name,genericdef,genericlist,nil);
              end;
            _CPPCLASS :
              begin
                consume(token);
                def:=object_dec(odt_cppclass,name,genericdef,genericlist,nil);
              end;
            _INTERFACE :
              begin
                { need extra check here since interface is a keyword
                  in all pascal modes }
                if not(m_class in current_settings.modeswitches) then
                  Message(parser_f_need_objfpc_or_delphi_mode);
                consume(token);
                if current_settings.interfacetype=it_interfacecom then
                  def:=object_dec(odt_interfacecom,name,genericdef,genericlist,nil)
                else {it_interfacecorba}
                  def:=object_dec(odt_interfacecorba,name,genericdef,genericlist,nil);
              end;
            _OBJECT :
              begin
                consume(token);
                def:=object_dec(odt_object,name,genericdef,genericlist,nil);
              end;
            _PROCEDURE,
            _FUNCTION:
              begin
                is_func:=(token=_FUNCTION);
                consume(token);
                pd:=tprocvardef.create(normal_function_level);
                if token=_LKLAMMER then
                  parse_parameter_dec(pd);
                if is_func then
                 begin
                   consume(_COLON);
                   single_type(pd.returndef,false,false);
                 end;
                if token=_OF then
                  begin
                    consume(_OF);
                    consume(_OBJECT);
                    include(pd.procoptions,po_methodpointer);
                  end;
                def:=pd;
                { possible proc directives }
                if parseprocvardir then
                  begin
                    if check_proc_directive(true) then
                      begin
                         newtype:=ttypesym.create('unnamed',def);
                         parse_var_proc_directives(tsym(newtype));
                         newtype.typedef:=nil;
                         def.typesym:=nil;
                         newtype.free;
                      end;
                    { Add implicit hidden parameters and function result }
                    handle_calling_convention(pd);
                  end;
              end;
            else
              expr_type;
         end;

         if def=nil then
          def:=generrordef;
      end;


    procedure read_anon_type(var def : tdef;parseprocvardir:boolean);
      begin
        read_named_type(def,'',nil,nil,parseprocvardir);
      end;


    procedure write_persistent_type_info(st:tsymtable);
      var
        i : longint;
        def : tdef;
        vmtwriter  : TVMTWriter;
      begin
        for i:=0 to st.DefList.Count-1 do
          begin
            def:=tdef(st.DefList[i]);
            case def.typ of
              recorddef :
                write_persistent_type_info(trecorddef(def).symtable);
              objectdef :
                begin
                  { Skip generics and forward defs }
                  if (df_generic in def.defoptions) or
                     (oo_is_forward in tobjectdef(def).objectoptions) then
                    continue;
                  write_persistent_type_info(tobjectdef(def).symtable);
                  { Write also VMT if not done yet }
                  if not(ds_vmt_written in def.defstates) then
                    begin
                      vmtwriter:=TVMTWriter.create(tobjectdef(def));
                      if is_interface(tobjectdef(def)) then
                        vmtwriter.writeinterfaceids;
                      if (oo_has_vmt in tobjectdef(def).objectoptions) then
                        vmtwriter.writevmt;
                      vmtwriter.free;
                      include(def.defstates,ds_vmt_written);
                    end;
                end;
              procdef :
                begin
                  if assigned(tprocdef(def).localst) and
                     (tprocdef(def).localst.symtabletype=localsymtable) then
                    write_persistent_type_info(tprocdef(def).localst);
                  if assigned(tprocdef(def).parast) then
                    write_persistent_type_info(tprocdef(def).parast);
                end;
            end;
            { generate always persistent tables for types in the interface so it can
              be reused in other units and give always the same pointer location. }
            { Init }
            if (
                assigned(def.typesym) and
                (st.symtabletype=globalsymtable)
               ) or
               def.needs_inittable or
               (ds_init_table_used in def.defstates) then
              RTTIWriter.write_rtti(def,initrtti);
            { RTTI }
            if (
                  assigned(def.typesym) and
                  (st.symtabletype=globalsymtable)
               ) or
               (ds_rtti_table_used in def.defstates) then
              RTTIWriter.write_rtti(def,fullrtti);
          end;
      end;

end.
