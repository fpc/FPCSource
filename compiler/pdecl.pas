{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Does declaration (but not type) parsing for Free Pascal

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
unit pdecl;

{$i fpcdefs.inc}

interface

    uses
      { global }
      globals,
      { symtable }
      symsym,
      { pass_1 }
      node;

    function  readconstant(const orgname:string;const filepos:tfileposinfo):tconstsym;

    procedure const_dec;
    procedure label_dec;
    procedure type_dec;
    procedure types_dec;
    procedure var_dec;
    procedure threadvar_dec;
    procedure property_dec;
    procedure resourcestring_dec;

implementation

    uses
       { common }
       cutils,cclasses,
       { global }
       globtype,tokens,verbose,widestr,
       systems,
       { aasm }
       aasmbase,aasmtai,aasmdata,fmodule,
       { symtable }
       symconst,symbase,symtype,symdef,symtable,paramgr,defutil,
       { pass 1 }
       nmat,nadd,ncal,nset,ncnv,ninl,ncon,nld,nflw,nobj,
       { codegen }
       ncgutil,
       { parser }
       scanner,
       pbase,pexpr,ptype,ptconst,pdecsub,pdecvar,pdecobj,
       { cpu-information }
       cpuinfo
       ;


    function readconstant(const orgname:string;const filepos:tfileposinfo):tconstsym;
      var
        hp : tconstsym;
        p : tnode;
        ps : pconstset;
        pd : pbestreal;
        pg : pguid;
        sp : pchar;
        pw : pcompilerwidestring;
        storetokenpos : tfileposinfo;
      begin
        readconstant:=nil;
        if orgname='' then
         internalerror(9584582);
        hp:=nil;
        p:=comp_expr(true);
        storetokenpos:=current_tokenpos;
        current_tokenpos:=filepos;
        case p.nodetype of
           ordconstn:
             begin
               if p.resultdef.typ=pointerdef then
                 hp:=tconstsym.create_ordptr(orgname,constpointer,tordconstnode(p).value,p.resultdef)
               else
                 hp:=tconstsym.create_ord(orgname,constord,tordconstnode(p).value,p.resultdef);
             end;
           stringconstn:
             begin
               if is_widestring(p.resultdef) then
                 begin
                   initwidestring(pw);
                   copywidestring(pcompilerwidestring(tstringconstnode(p).value_str),pw);
                   hp:=tconstsym.create_wstring(orgname,constwstring,pw);
                 end
               else
                 begin
                   getmem(sp,tstringconstnode(p).len+1);
                   move(tstringconstnode(p).value_str^,sp^,tstringconstnode(p).len+1);
                   hp:=tconstsym.create_string(orgname,conststring,sp,tstringconstnode(p).len);
                 end;
             end;
           realconstn :
             begin
                new(pd);
                pd^:=trealconstnode(p).value_real;
                hp:=tconstsym.create_ptr(orgname,constreal,pd,p.resultdef);
             end;
           setconstn :
             begin
               new(ps);
               ps^:=tsetconstnode(p).value_set^;
               hp:=tconstsym.create_ptr(orgname,constset,ps,p.resultdef);
             end;
           pointerconstn :
             begin
               hp:=tconstsym.create_ordptr(orgname,constpointer,tpointerconstnode(p).value,p.resultdef);
             end;
           niln :
             begin
               hp:=tconstsym.create_ord(orgname,constnil,0,p.resultdef);
             end;
           typen :
             begin
               if is_interface(p.resultdef) then
                begin
                  if assigned(tobjectdef(p.resultdef).iidguid) then
                   begin
                     new(pg);
                     pg^:=tobjectdef(p.resultdef).iidguid^;
                     hp:=tconstsym.create_ptr(orgname,constguid,pg,p.resultdef);
                   end
                  else
                   Message1(parser_e_interface_has_no_guid,tobjectdef(p.resultdef).objrealname^);
                end
               else
                Message(parser_e_illegal_expression);
             end;
           else
             Message(parser_e_illegal_expression);
        end;
        current_tokenpos:=storetokenpos;
        p.free;
        readconstant:=hp;
      end;


    procedure const_dec;
      var
         orgname : TIDString;
         hdef : tdef;
         sym : tsym;
         dummysymoptions : tsymoptions;
         storetokenpos,filepos : tfileposinfo;
         old_block_type : tblock_type;
         skipequal : boolean;
         tclist : tasmlist;
         varspez : tvarspez;
      begin
         consume(_CONST);
         old_block_type:=block_type;
         block_type:=bt_const;
         repeat
           orgname:=orgpattern;
           filepos:=current_tokenpos;
           consume(_ID);
           case token of

             _EQUAL:
                begin
                   consume(_EQUAL);
                   sym:=readconstant(orgname,filepos);
                   { Support hint directives }
                   dummysymoptions:=[];
                   try_consume_hintdirective(dummysymoptions);
                   if assigned(sym) then
                     begin
                       sym.symoptions:=sym.symoptions+dummysymoptions;
                       symtablestack.top.insert(sym);
                     end;
                   consume(_SEMICOLON);
                end;

             _COLON:
                begin
                   { set the blocktype first so a consume also supports a
                     caret, to support const s : ^string = nil }
                   block_type:=bt_type;
                   consume(_COLON);
                   read_anon_type(hdef,false);
                   block_type:=bt_const;
                   skipequal:=false;
                   { create symbol }
                   storetokenpos:=current_tokenpos;
                   current_tokenpos:=filepos;
                   if not (cs_typed_const_writable in current_settings.localswitches) then
                     varspez:=vs_const
                   else
                     varspez:=vs_value;
                   sym:=tstaticvarsym.create(orgname,varspez,hdef,[]);
                   current_tokenpos:=storetokenpos;
                   symtablestack.top.insert(sym);
                   { procvar can have proc directives, but not type references }
                   if (hdef.typ=procvardef) and
                      (hdef.typesym=nil) then
                    begin
                      { support p : procedure;stdcall=nil; }
                      if try_to_consume(_SEMICOLON) then
                       begin
                         if check_proc_directive(true) then
                          parse_var_proc_directives(sym)
                         else
                          begin
                            Message(parser_e_proc_directive_expected);
                            skipequal:=true;
                          end;
                       end
                      else
                      { support p : procedure stdcall=nil; }
                       begin
                         if check_proc_directive(true) then
                          parse_var_proc_directives(sym);
                       end;
                      { add default calling convention }
                      handle_calling_convention(tabstractprocdef(hdef));
                    end;
                   if not skipequal then
                    begin
                      { get init value }
                      consume(_EQUAL);
                      if (cs_typed_const_writable in current_settings.localswitches) then
                        tclist:=current_asmdata.asmlists[al_rotypedconsts]
                      else
                        tclist:=current_asmdata.asmlists[al_typedconsts];
                      read_typed_const(tclist,tstaticvarsym(sym));
                    end;
                end;

              else
                { generate an error }
                consume(_EQUAL);
           end;
         until token<>_ID;
         block_type:=old_block_type;
      end;


    procedure label_dec;
      begin
         consume(_LABEL);
         if not(cs_support_goto in current_settings.moduleswitches) then
           Message(sym_e_goto_and_label_not_supported);
         repeat
           if not(token in [_ID,_INTCONST]) then
             consume(_ID)
           else
             begin
                if token=_ID then
                 symtablestack.top.insert(tlabelsym.create(orgpattern))
                else
                 symtablestack.top.insert(tlabelsym.create(pattern));
                consume(token);
             end;
           if token<>_SEMICOLON then consume(_COMMA);
         until not(token in [_ID,_INTCONST]);
         consume(_SEMICOLON);
      end;


    { search in symtablestack used, but not defined type }
    procedure resolve_type_forward(p:TObject;arg:pointer);
      var
        hpd,pd : tdef;
        stpos  : tfileposinfo;
        again  : boolean;
        srsym  : tsym;
        srsymtable : TSymtable;

      begin
         { Check only typesyms or record/object fields }
         case tsym(p).typ of
           typesym :
             pd:=ttypesym(p).typedef;
           fieldvarsym :
             pd:=tfieldvarsym(p).vardef
           else
             exit;
         end;
         repeat
           again:=false;
           case pd.typ of
             arraydef :
               begin
                 { elementdef could also be defined using a forwarddef }
                 pd:=tarraydef(pd).elementdef;
                 again:=true;
               end;
             pointerdef,
             classrefdef :
               begin
                 { classrefdef inherits from pointerdef }
                 hpd:=tabstractpointerdef(pd).pointeddef;
                 { still a forward def ? }
                 if hpd.typ=forwarddef then
                  begin
                    { try to resolve the forward }
                    { get the correct position for it }
                    stpos:=current_tokenpos;
                    current_tokenpos:=tforwarddef(hpd).forwardpos;
                    resolving_forward:=true;
                    if not assigned(tforwarddef(hpd).tosymname) then
                      internalerror(20021120);
                    searchsym(tforwarddef(hpd).tosymname^,srsym,srsymtable);
                    resolving_forward:=false;
                    current_tokenpos:=stpos;
                    { we don't need the forwarddef anymore, dispose it }
                    hpd.free;
                    tabstractpointerdef(pd).pointeddef:=nil; { if error occurs }
                    { was a type sym found ? }
                    if assigned(srsym) and
                       (srsym.typ=typesym) then
                     begin
                       tabstractpointerdef(pd).pointeddef:=ttypesym(srsym).typedef;
                       { avoid wrong unused warnings web bug 801 PM }
                       inc(ttypesym(srsym).refs);
                       { we need a class type for classrefdef }
                       if (pd.typ=classrefdef) and
                          not(is_class(ttypesym(srsym).typedef)) then
                         Message1(type_e_class_type_expected,ttypesym(srsym).typedef.typename);
                     end
                    else
                     begin
                       MessagePos1(tsym(p).fileinfo,sym_e_forward_type_not_resolved,tsym(p).realname);
                       { try to recover }
                       tabstractpointerdef(pd).pointeddef:=generrordef;
                     end;
                  end;
               end;
             recorddef :
               trecorddef(pd).symtable.SymList.ForEachCall(@resolve_type_forward,nil);
             objectdef :
               begin
                 if not(m_fpc in current_settings.modeswitches) and
                    (oo_is_forward in tobjectdef(pd).objectoptions) then
                  begin
                    { only give an error as the implementation may follow in an
                      other type block which is allowed by FPC modes }
                    MessagePos1(tsym(p).fileinfo,sym_e_forward_type_not_resolved,tsym(p).realname);
                  end
                 else
                  begin
                    { Check all fields of the object declaration, but don't
                      check objectdefs in objects/records, because these
                      can't exist (anonymous objects aren't allowed) }
                    if not(tsym(p).owner.symtabletype in [ObjectSymtable,recordsymtable]) then
                     tobjectdef(pd).symtable.SymList.ForEachCall(@resolve_type_forward,nil);
                  end;
               end;
          end;
        until not again;
      end;


    procedure types_dec;

        function parse_generic_parameters:TFPObjectList;
        var
          generictype : ttypesym;
        begin
          result:=TFPObjectList.Create(false);
          repeat
            if token=_ID then
              begin
                generictype:=ttypesym.create(orgpattern,cundefinedtype);
                include(generictype.symoptions,sp_generic_para);
                result.add(generictype);
              end;
            consume(_ID);
          until not try_to_consume(_COMMA) ;
        end;

      var
         typename,orgtypename : TIDString;
         newtype  : ttypesym;
         sym      : tsym;
         srsymtable : TSymtable;
         hdef     : tdef;
         defpos,storetokenpos : tfileposinfo;
         old_block_type : tblock_type;
         isgeneric,
         isunique,
         istyperenaming : boolean;
         generictypelist : TFPObjectList;
         generictokenbuf : tdynamicarray;
         vmtbuilder : TVMTBuilder;
      begin
         old_block_type:=block_type;
         block_type:=bt_type;
         typecanbeforward:=true;
         repeat
           defpos:=current_tokenpos;
           istyperenaming:=false;
           generictypelist:=nil;
           generictokenbuf:=nil;

           { generic declaration? }
           isgeneric:=try_to_consume(_GENERIC);

           typename:=pattern;
           orgtypename:=orgpattern;
           consume(_ID);

           { Generic type declaration? }
           if isgeneric then
             begin
               consume(_LSHARPBRACKET);
               generictypelist:=parse_generic_parameters;
               consume(_RSHARPBRACKET);
             end;

           consume(_EQUAL);

           { support 'ttype=type word' syntax }
           isunique:=try_to_consume(_TYPE);

           { MacPas object model is more like Delphi's than like TP's, but }
           { uses the object keyword instead of class                      }
           if (m_mac in current_settings.modeswitches) and
              (token = _OBJECT) then
             token := _CLASS;

           { Start recording a generic template }
           if assigned(generictypelist) then
             begin
               generictokenbuf:=tdynamicarray.create(256);
               current_scanner.startrecordtokens(generictokenbuf);
             end;

           { is the type already defined? }
           searchsym(typename,sym,srsymtable);
           newtype:=nil;
           { found a symbol with this name? }
           if assigned(sym) then
            begin
              if (sym.typ=typesym) then
               begin
                 if ((token=_CLASS) or
                     (token=_INTERFACE) or
                     (token=_DISPINTERFACE)) and
                    (assigned(ttypesym(sym).typedef)) and
                    is_class_or_interface_or_dispinterface(ttypesym(sym).typedef) and
                    (oo_is_forward in tobjectdef(ttypesym(sym).typedef).objectoptions) then
                  begin
                    { we can ignore the result   }
                    { the definition is modified }
                    object_dec(orgtypename,nil,nil,tobjectdef(ttypesym(sym).typedef));
                    newtype:=ttypesym(sym);
                    hdef:=newtype.typedef;
                  end
                 else
                  message1(parser_h_type_redef,orgtypename);
               end;
            end;
           { no old type reused ? Then insert this new type }
           if not assigned(newtype) then
            begin
              { insert the new type first with an errordef, so that
                referencing the type before it's really set it
                will give an error (PFV) }
              hdef:=generrordef;
              storetokenpos:=current_tokenpos;
              newtype:=ttypesym.create(orgtypename,hdef);
              symtablestack.top.insert(newtype);
              current_tokenpos:=defpos;
              current_tokenpos:=storetokenpos;
              { read the type definition }
              read_named_type(hdef,orgtypename,nil,generictypelist,false);
              { update the definition of the type }
              if assigned(hdef) then
                begin
                  if assigned(hdef.typesym) then
                    istyperenaming:=true;
                  if isunique then
                    begin
                      hdef:=tstoreddef(hdef).getcopy;

                      { fix name, it is used e.g. for tables }
                      if is_class_or_interface_or_dispinterface(hdef) then
                        with tobjectdef(hdef) do
                          begin
                            stringdispose(objname);
                            stringdispose(objrealname);
                            objrealname:=stringdup(orgtypename);
                            objname:=stringdup(upper(orgtypename));
                          end;

                      include(hdef.defoptions,df_unique);
                    end;
                  if not assigned(hdef.typesym) then
                    hdef.typesym:=newtype;
                end;
              newtype.typedef:=hdef;
              { KAZ: handle TGUID declaration in system unit }
              if (cs_compilesystem in current_settings.moduleswitches) and not assigned(rec_tguid) and
                 (typename='TGUID') and { name: TGUID and size=16 bytes that is 128 bits }
                 assigned(hdef) and (hdef.typ=recorddef) and (hdef.size=16) then
                rec_tguid:=trecorddef(hdef);
            end;
           if assigned(hdef) then
            begin
              case hdef.typ of
                pointerdef :
                  begin
                    consume(_SEMICOLON);
                    if try_to_consume(_FAR) then
                     begin
                       tpointerdef(hdef).is_far:=true;
                       consume(_SEMICOLON);
                     end;
                  end;
                procvardef :
                  begin
                    { in case of type renaming, don't parse proc directives }
                    if istyperenaming then
                     consume(_SEMICOLON)
                    else
                     begin
                       if not check_proc_directive(true) then
                        consume(_SEMICOLON);
                       parse_var_proc_directives(tsym(newtype));
                       handle_calling_convention(tprocvardef(hdef));
                     end;
                  end;
                objectdef :
                  begin
                    { Build VMT indexes, skip for type renaming and forward classes }
                    if (hdef.typesym=newtype) and
                       not(oo_is_forward in tobjectdef(hdef).objectoptions) and
                       not(df_generic in hdef.defoptions) then
                      begin
                        vmtbuilder:=TVMTBuilder.Create(tobjectdef(hdef));
                        vmtbuilder.generate_vmt;
                        vmtbuilder.free;
                      end;
                    try_consume_hintdirective(newtype.symoptions);
                    consume(_SEMICOLON);
                  end;
                recorddef :
                  begin
                    try_consume_hintdirective(newtype.symoptions);
                    consume(_SEMICOLON);
                  end;
                else
                  consume(_SEMICOLON);
              end;
            end;

           if isgeneric and not(hdef.typ in [objectdef,recorddef]) then
             message(parser_e_cant_create_generics_of_this_type);

           { Stop recording a generic template }
           if assigned(generictypelist) then
             begin
               current_scanner.stoprecordtokens;
               tstoreddef(hdef).generictokenbuf:=generictokenbuf;
               { Generic is never a type renaming }
               hdef.typesym:=newtype;
             end;
           if assigned(generictypelist) then
             generictypelist.free;
         until token<>_ID;
         typecanbeforward:=false;
         symtablestack.top.SymList.ForEachCall(@resolve_type_forward,nil);
         block_type:=old_block_type;
      end;


    { reads a type declaration to the symbol table }
    procedure type_dec;
      begin
        consume(_TYPE);
        types_dec;
      end;


    procedure var_dec;
    { parses variable declarations and inserts them in }
    { the top symbol table of symtablestack         }
      begin
        consume(_VAR);
        read_var_decls([]);
      end;


    procedure property_dec;
      var
         old_block_type : tblock_type;
      begin
         consume(_PROPERTY);
         if not(symtablestack.top.symtabletype in [staticsymtable,globalsymtable]) then
           message(parser_e_resourcestring_only_sg);
         old_block_type:=block_type;
         block_type:=bt_const;
         repeat
           read_property_dec(nil);
           consume(_SEMICOLON);
         until token<>_ID;
         block_type:=old_block_type;
      end;


    procedure threadvar_dec;
    { parses thread variable declarations and inserts them in }
    { the top symbol table of symtablestack                }
      begin
        consume(_THREADVAR);
        if not(symtablestack.top.symtabletype in [staticsymtable,globalsymtable]) then
          message(parser_e_threadvars_only_sg);
        read_var_decls([vd_threadvar]);
      end;


    procedure resourcestring_dec;
      var
         orgname : TIDString;
         p : tnode;
         dummysymoptions : tsymoptions;
         storetokenpos,filepos : tfileposinfo;
         old_block_type : tblock_type;
         sp : pchar;
         sym : tsym;
      begin
         consume(_RESOURCESTRING);
         if not(symtablestack.top.symtabletype in [staticsymtable,globalsymtable]) then
           message(parser_e_resourcestring_only_sg);
         old_block_type:=block_type;
         block_type:=bt_const;
         repeat
           orgname:=orgpattern;
           filepos:=current_tokenpos;
           consume(_ID);
           case token of
             _EQUAL:
                begin
                   consume(_EQUAL);
                   p:=comp_expr(true);
                   storetokenpos:=current_tokenpos;
                   current_tokenpos:=filepos;
                   sym:=nil;
                   case p.nodetype of
                      ordconstn:
                        begin
                           if is_constcharnode(p) then
                             begin
                                getmem(sp,2);
                                sp[0]:=chr(tordconstnode(p).value);
                                sp[1]:=#0;
                                sym:=tconstsym.create_string(orgname,constresourcestring,sp,1);
                             end
                           else
                             Message(parser_e_illegal_expression);
                        end;
                      stringconstn:
                        with Tstringconstnode(p) do
                          begin
                             getmem(sp,len+1);
                             move(value_str^,sp^,len+1);
                             sym:=tconstsym.create_string(orgname,constresourcestring,sp,len);
                          end;
                      else
                        Message(parser_e_illegal_expression);
                   end;
                   current_tokenpos:=storetokenpos;
                   { Support hint directives }
                   dummysymoptions:=[];
                   try_consume_hintdirective(dummysymoptions);
                   if assigned(sym) then
                     begin
                       sym.symoptions:=sym.symoptions+dummysymoptions;
                       symtablestack.top.insert(sym);
                     end;
                   consume(_SEMICOLON);
                   p.free;
                end;
              else consume(_EQUAL);
           end;
         until token<>_ID;
         block_type:=old_block_type;
      end;

end.
