{
    $Id$
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
       globtype,symtype;

    const
       { forward types should only be possible inside a TYPE statement }
       typecanbeforward : boolean = false;

    var
       { hack, which allows to use the current parsed }
       { object type as function argument type  }
       testcurobject : byte;
       curobjectname : stringid;

    { reads a string, file type or a type id and returns a name and }
    { tdef }
    procedure single_type(var tt:ttype;var s : string;isforwarddef:boolean);

    procedure read_type(var tt:ttype;const name : stringid);

    { reads a type definition }
    { to a appropriating tdef, s gets the name of   }
    { the type to allow name mangling          }
    procedure id_type(var tt : ttype;var s : string;isforwarddef:boolean);


implementation

    uses
       { common }
       cutils,
       { global }
       globals,tokens,verbose,
       systems,
       { symtable }
       symconst,symbase,symdef,symsym,symtable,
       defutil,defcmp,
       { pass 1 }
       node,
       nmat,nadd,ncal,nset,ncnv,ninl,ncon,nld,nflw,
       { parser }
       scanner,
       pbase,pexpr,pdecsub,pdecvar,pdecobj;


    procedure id_type(var tt : ttype;var s : string;isforwarddef:boolean);
    { reads a type definition }
    { to a appropriating tdef, s gets the name of   }
    { the type to allow name mangling          }
      var
        is_unit_specific : boolean;
        pos : tfileposinfo;
        srsym : tsym;
        srsymtable : tsymtable;
        sorg : stringid;
      begin
         s:=pattern;
         sorg:=orgpattern;
         pos:=akttokenpos;
         { classes can be used also in classes }
         if (curobjectname=pattern) and is_class_or_interface(aktobjectdef) then
           begin
              tt.setdef(aktobjectdef);
              consume(_ID);
              exit;
           end;
         { objects can be parameters }
         if (testcurobject=2) and (curobjectname=pattern) then
           begin
              tt.setdef(aktobjectdef);
              consume(_ID);
              exit;
           end;
         { try to load the symbol to see if it's a unitsym }
         is_unit_specific:=false;
         searchsym(s,srsym,srsymtable);
         consume(_ID);
         if assigned(srsym) and
            (srsym.typ=unitsym) then
           begin
              is_unit_specific:=true;
              consume(_POINT);
              if srsym.owner.unitid=0 then
               begin
                 srsym:=searchsymonlyin(tunitsym(srsym).unitsymtable,pattern);
                 pos:=akttokenpos;
                 s:=pattern;
               end
              else
               srsym:=nil;
              consume(_ID);
           end;
         { Types are first defined with an error def before assigning
           the real type so check if it's an errordef. if so then
           give an error. Only check for typesyms in the current symbol
           table as forwarddef are not resolved directly }
         if assigned(srsym) and
            (srsym.typ=typesym) and
            (srsym.owner=symtablestack) and
            (ttypesym(srsym).restype.def.deftype=errordef) then
          begin
            Message1(type_e_type_is_not_completly_defined,ttypesym(srsym).realname);
            tt:=generrortype;
            exit;
          end;
         { are we parsing a possible forward def ? }
         if isforwarddef and
            not(is_unit_specific) then
          begin
            tt.setdef(tforwarddef.create(s,pos));
            exit;
          end;
         { unknown sym ? }
         if not assigned(srsym) then
          begin
            Message1(sym_e_id_not_found,sorg);
            tt:=generrortype;
            exit;
          end;
         { type sym ? }
         if (srsym.typ<>typesym) then
          begin
            Message(type_e_type_id_expected);
            tt:=generrortype;
            exit;
          end;
         { Give an error when referring to an errordef }
         if (ttypesym(srsym).restype.def.deftype=errordef) then
          begin
            Message(sym_e_error_in_type_def);
            tt:=generrortype;
            exit;
          end;
         { Use the definitions for current unit, becuase
           they can be refered from the parameters and symbols are not
           loaded at that time. Only write the definition when the
           symbol is the real owner of the definition (not a redefine) }
         if (ttypesym(srsym).owner.unitid=0) and
            ((ttypesym(srsym).restype.def.typesym=nil) or
             (srsym=ttypesym(srsym).restype.def.typesym)) then
          tt.setdef(ttypesym(srsym).restype.def)
         else
          tt.setsym(srsym);
      end;


    procedure single_type(var tt:ttype;var s : string;isforwarddef:boolean);
    { reads a string, file type or a type id and returns a name and }
    { tdef                                                        }
       var
          hs : string;
          t2 : ttype;
       begin
          case token of
            _STRING:
                begin
                   string_dec(tt);
                   s:='STRING';
                end;
            _FILE:
                begin
                   consume(_FILE);
                   if token=_OF then
                     begin
                        consume(_OF);
                        single_type(t2,hs,false);
                        tt.setdef(tfiledef.createtyped(t2));
                        s:='FILE$OF$'+hs;
                     end
                   else
                     begin
                        tt:=cfiletype;
                        s:='FILE';
                     end;
                end;
            _ID:
              begin
                id_type(tt,s,isforwarddef);
              end;
            else
              begin
                message(type_e_type_id_expected);
                s:='<unknown>';
                tt:=generrortype;
              end;
         end;
      end;

    { reads a record declaration }
    function record_dec : tdef;

      var
         symtable : tsymtable;
         storetypecanbeforward : boolean;
         old_object_option : tsymoptions;
      begin
         { create recdef }
         symtable:=trecordsymtable.create;
         record_dec:=trecorddef.create(symtable);
         { update symtable stack }
         symtable.next:=symtablestack;
         symtablestack:=symtable;
         { parse record }
         consume(_RECORD);
         old_object_option:=current_object_option;
         current_object_option:=[sp_public];
         storetypecanbeforward:=typecanbeforward;
         { for tp7 don't allow forward types }
         if m_tp7 in aktmodeswitches then
           typecanbeforward:=false;
         read_var_decs(true,false,false);
         consume(_END);
         typecanbeforward:=storetypecanbeforward;
         current_object_option:=old_object_option;
         { may be scale record size to a size of n*4 ? }
         symtablestack.datasize:=align(symtablestack.datasize,symtablestack.dataalignment);
         { restore symtable stack }
         symtablestack:=symtable.next;
      end;


    { reads a type definition and returns a pointer to it }
    procedure read_type(var tt : ttype;const name : stringid);
      var
        pt : tnode;
        tt2 : ttype;
        aktenumdef : tenumdef;
        ap : tarraydef;
        s : stringid;
        l,v : TConstExprInt;
        oldaktpackrecords : longint;
        hs : string;
        defpos,storepos : tfileposinfo;

        procedure expr_type;
        var
           pt1,pt2 : tnode;
           lv,hv   : TConstExprInt;
        begin
           { use of current parsed object ? }
           if (token=_ID) and (testcurobject=2) and (curobjectname=pattern) then
             begin
                consume(_ID);
                tt.setdef(aktobjectdef);
                exit;
             end;
           { classes can be used also in classes }
           if (curobjectname=pattern) and is_class_or_interface(aktobjectdef) then
             begin
                tt.setdef(aktobjectdef);
                consume(_ID);
                exit;
             end;
           { we can't accept a equal in type }
           pt1:=comp_expr(not(ignore_equal));
           if (token=_POINTPOINT) then
             begin
               consume(_POINTPOINT);
               { get high value of range }
               pt2:=comp_expr(not(ignore_equal));
               { make both the same type }
               inserttypeconv(pt1,pt2.resulttype);
               { both must be evaluated to constants now }
               if (pt1.nodetype=ordconstn) and
                  (pt2.nodetype=ordconstn) then
                 begin
                   lv:=tordconstnode(pt1).value;
                   hv:=tordconstnode(pt2).value;
                   { Check bounds }
                   if hv<lv then
                     Message(cg_e_upper_lower_than_lower)
                   else
                     begin
                       { All checks passed, create the new def }
                       case pt1.resulttype.def.deftype of
                         enumdef :
                           tt.setdef(tenumdef.create_subrange(tenumdef(pt1.resulttype.def),lv,hv));
                         orddef :
                           begin
                             if is_char(pt1.resulttype.def) then
                               tt.setdef(torddef.create(uchar,lv,hv))
                             else
                               if is_boolean(pt1.resulttype.def) then
                                 tt.setdef(torddef.create(bool8bit,l,hv))
                               else
                                 tt.setdef(torddef.create(range_to_basetype(lv,hv),lv,hv));
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
               { a simple type renaming }
               if (pt1.nodetype=typen) then
                 tt:=ttypenode(pt1).resulttype
               else
                 Message(sym_e_error_in_type_def);
             end;
           pt1.free;
        end;

        procedure array_dec;
        var
          lowval,
          highval   : longint;
          arraytype : ttype;
          ht        : ttype;

          procedure setdefdecl(const t:ttype);
          begin
            case t.def.deftype of
              enumdef :
                begin
                  lowval:=tenumdef(t.def).min;
                  highval:=tenumdef(t.def).max;
                  if tenumdef(t.def).has_jumps then
                   Message(type_e_array_index_enums_with_assign_not_possible);
                  arraytype:=t;
                end;
              orddef :
                begin
                  if torddef(t.def).typ in [uchar,
                    u8bit,u16bit,
                    s8bit,s16bit,s32bit,
                    bool8bit,bool16bit,bool32bit,
                    uwidechar] then
                    begin
                       lowval:=torddef(t.def).low;
                       highval:=torddef(t.def).high;
                       arraytype:=t;
                    end
                  else
                    Message1(parser_e_type_cant_be_used_in_array_index,t.def.gettypename);
                end;
              else
                Message(sym_e_error_in_type_def);
            end;
          end;

        begin
           consume(_ARRAY);
           { open array? }
           if token=_LECKKLAMMER then
             begin
                consume(_LECKKLAMMER);
                { defaults }
                arraytype:=generrortype;
                lowval:=longint($80000000);
                highval:=$7fffffff;
                tt.reset;
                repeat
                  { read the expression and check it, check apart if the
                    declaration is an enum declaration because that needs to
                    be parsed by readtype (PFV) }
                  if token=_LKLAMMER then
                   begin
                     read_type(ht,'');
                     setdefdecl(ht);
                   end
                  else
                   begin
                     pt:=expr;
                     if pt.nodetype=typen then
                      setdefdecl(pt.resulttype)
                     else
                       begin
                          if (pt.nodetype=rangen) then
                           begin
                             if (trangenode(pt).left.nodetype=ordconstn) and
                                (trangenode(pt).right.nodetype=ordconstn) then
                              begin
                                lowval:=tordconstnode(trangenode(pt).left).value;
                                highval:=tordconstnode(trangenode(pt).right).value;
                                if highval<lowval then
                                 begin
                                   Message(parser_e_array_lower_less_than_upper_bound);
                                   highval:=lowval;
                                 end;
                                arraytype:=trangenode(pt).right.resulttype;
                              end
                             else
                              Message(type_e_cant_eval_constant_expr);
                           end
                          else
                           Message(sym_e_error_in_type_def)
                       end;
                     pt.free;
                   end;

                { create arraydef }
                  if not assigned(tt.def) then
                   begin
                     ap:=tarraydef.create(lowval,highval,arraytype);
                     tt.setdef(ap);
                   end
                  else
                   begin
                     ap.elementtype.setdef(tarraydef.create(lowval,highval,arraytype));
                     ap:=tarraydef(ap.elementtype.def);
                   end;

                  if token=_COMMA then
                    consume(_COMMA)
                  else
                    break;
                until false;
                consume(_RECKKLAMMER);
             end
           else
             begin
                ap:=tarraydef.create(0,-1,s32bittype);
                ap.IsDynamicArray:=true;
                tt.setdef(ap);
             end;
           consume(_OF);
           read_type(tt2,'');
           { if no error, set element type }
           if assigned(ap) then
             ap.setelementtype(tt2);
        end;

      var
        p : tnode;
        enumdupmsg : boolean;
      begin
         tt.reset;
         case token of
            _STRING,_FILE:
              begin
                single_type(tt,hs,false);
              end;
           _LKLAMMER:
              begin
                consume(_LKLAMMER);
                { allow negativ value_str }
                l:=-1;
                enumdupmsg:=false;
                aktenumdef:=tenumdef.create;
                repeat
                  s:=orgpattern;
                  defpos:=akttokenpos;
                  consume(_ID);
                  { only allow assigning of specific numbers under fpc mode }
                  if (m_fpc in aktmodeswitches) and
                     (token=_ASSIGNMENT) then
                    begin
                       consume(_ASSIGNMENT);
                       p:=comp_expr(true);
                       if (p.nodetype=ordconstn) then
                        begin
                          { we expect an integer or an enum of the
                            same type }
                          if is_integer(p.resulttype.def) or
                             is_char(p.resulttype.def) or
                             equal_defs(p.resulttype.def,aktenumdef) then
                           v:=tordconstnode(p).value
                          else
                           Message2(type_e_incompatible_types,p.resulttype.def.typename,s32bittype.def.typename);
                        end
                       else
                        Message(cg_e_illegal_expression);
                       p.free;
                       { please leave that a note, allows type save }
                       { declarations in the win32 units ! }
                       if (v<=l) and (not enumdupmsg) then
                        begin
                          Message(parser_n_duplicate_enum);
                          enumdupmsg:=true;
                        end;
                       l:=v;
                    end
                  else if (m_delphi in aktmodeswitches) and
                     (token=_EQUAL) then
                    begin
                       consume(_EQUAL);
                       p:=comp_expr(true);
                       if (p.nodetype=ordconstn) then
                        begin
                          { we expect an integer or an enum of the
                            same type }
                          if is_integer(p.resulttype.def) or
                             equal_defs(p.resulttype.def,aktenumdef) then
                           l:=tordconstnode(p).value
                          else
                           Message2(type_e_incompatible_types,p.resulttype.def.typename,s32bittype.def.typename);
                        end
                       else
                        Message(cg_e_illegal_expression);
                       p.free;
                    end
                  else
                    inc(l);
                  storepos:=akttokenpos;
                  akttokenpos:=defpos;
                  constsymtable.insert(tenumsym.create(s,aktenumdef,l));
                  akttokenpos:=storepos;
                until not try_to_consume(_COMMA);
                tt.setdef(aktenumdef);
                consume(_RKLAMMER);
              end;
            _ARRAY:
              begin
                array_dec;
              end;
            _SET:
              begin
                consume(_SET);
                consume(_OF);
                read_type(tt2,'');
                if assigned(tt2.def) then
                 begin
                   case tt2.def.deftype of
                     { don't forget that min can be negativ  PM }
                     enumdef :
                       if tenumdef(tt2.def).min>=0 then
                        tt.setdef(tsetdef.create(tt2,tenumdef(tt2.def).max))
                       else
                        Message(sym_e_ill_type_decl_set);
                     orddef :
                       begin
                         case torddef(tt2.def).typ of
                           uchar :
                             tt.setdef(tsetdef.create(tt2,255));
                           u8bit,u16bit,u32bit,
                           s8bit,s16bit,s32bit :
                             begin
                               if (torddef(tt2.def).low>=0) then
                                tt.setdef(tsetdef.create(tt2,torddef(tt2.def).high))
                               else
                                Message(sym_e_ill_type_decl_set);
                             end;
                           else
                             Message(sym_e_ill_type_decl_set);
                         end;
                       end;
                     else
                       Message(sym_e_ill_type_decl_set);
                   end;
                 end
                else
                 tt:=generrortype;
              end;
           _CARET:
              begin
                consume(_CARET);
                single_type(tt2,hs,typecanbeforward);
                tt.setdef(tpointerdef.create(tt2));
              end;
            _RECORD:
              begin
                tt.setdef(record_dec);
              end;
            _PACKED:
              begin
                consume(_PACKED);
                if token=_ARRAY then
                  array_dec
                else
                  begin
                    oldaktpackrecords:=aktalignment.recordalignmax;
                    aktalignment.recordalignmax:=1;
                    if token in [_CLASS,_OBJECT] then
                      tt.setdef(object_dec(name,nil))
                    else
                      tt.setdef(record_dec);
                    aktalignment.recordalignmax:=oldaktpackrecords;
                  end;
              end;
            _CLASS,
            _CPPCLASS,
            _INTERFACE,
            _OBJECT:
              begin
                tt.setdef(object_dec(name,nil));
              end;
            _PROCEDURE:
              begin
                consume(_PROCEDURE);
                tt.setdef(tprocvardef.create);
                if token=_LKLAMMER then
                 parameter_dec(tprocvardef(tt.def));
                if token=_OF then
                  begin
                    consume(_OF);
                    consume(_OBJECT);
                    include(tprocvardef(tt.def).procoptions,po_methodpointer);
                    check_self_para(tprocvardef(tt.def));
                  end;
              end;
            _FUNCTION:
              begin
                consume(_FUNCTION);
                tt.def:=tprocvardef.create;
                if token=_LKLAMMER then
                 parameter_dec(tprocvardef(tt.def));
                consume(_COLON);
                single_type(tprocvardef(tt.def).rettype,hs,false);
                if token=_OF then
                  begin
                    consume(_OF);
                    consume(_OBJECT);
                    include(tprocvardef(tt.def).procoptions,po_methodpointer);
                  end;
              end;
            else
              expr_type;
         end;
         if tt.def=nil then
          tt:=generrortype;
      end;

end.
{
  $Log$
  Revision 1.48  2003-01-02 19:49:00  peter
    * update self parameter only for methodpointer and methods

  Revision 1.47  2002/12/21 13:07:34  peter
    * type redefine fix for tb0437

  Revision 1.46  2002/11/25 17:43:23  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.45  2002/09/27 21:13:29  carl
    * low-highval always checked if limit ober 2GB is reached (to avoid overflow)

  Revision 1.44  2002/09/10 16:26:39  peter
    * safety check for typesym added for incomplete type def check

  Revision 1.43  2002/09/09 19:34:07  peter
    * check for incomplete types in the current symtable when parsing
      forwarddef. Maybe this shall be delphi/tp only

  Revision 1.42  2002/07/20 11:57:56  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.41  2002/05/18 13:34:16  peter
    * readded missing revisions

  Revision 1.40  2002/05/16 19:46:44  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.38  2002/05/12 16:53:10  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.37  2002/04/19 15:46:03  peter
    * mangledname rewrite, tprocdef.mangledname is now created dynamicly
      in most cases and not written to the ppu
    * add mangeledname_prefix() routine to generate the prefix of
      manglednames depending on the current procedure, object and module
    * removed static procprefix since the mangledname is now build only
      on demand from tprocdef.mangledname

  Revision 1.36  2002/04/16 16:12:47  peter
    * give error when using enums with jumps as array index
    * allow char as enum value

  Revision 1.35  2002/04/04 19:06:04  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.34  2002/01/24 18:25:49  peter
   * implicit result variable generation for assembler routines
   * removed m_tp modeswitch, use m_tp7 or not(m_fpc) instead

  Revision 1.33  2002/01/15 16:13:34  jonas
    * fixed web bugs 1758 and 1760

  Revision 1.32  2002/01/06 12:08:15  peter
    * removed uauto from orddef, use new range_to_basetype generating
      the correct ordinal type for a range

}
