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
      globtype,
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
       globals,tokens,verbose,widestr,constexp,
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
                 hp:=tconstsym.create_ordptr(orgname,constpointer,tordconstnode(p).value.uvalue,p.resultdef)
               else
                 hp:=tconstsym.create_ord(orgname,constord,tordconstnode(p).value,p.resultdef);
             end;
           stringconstn:
             begin
               if is_wide_or_unicode_string(p.resultdef) then
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
                   block_type:=bt_const_type;
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


    procedure types_dec;

      procedure get_objc_class_or_protocol_external_status(od: tobjectdef);
        begin
          { Objective-C classes can be external -> all messages inside are
            external (defined at the class level instead of per method, so
            that you cannot define some methods as external and some not)
          }
          if (token=_ID) and
             (idtoken=_EXTERNAL) then
            begin
              consume(_EXTERNAL);
              if (token=_ID) and
                 (idtoken=_NAME) then
                begin
                  consume(_NAME);
                  od.objextname:=stringdup(get_stringconst);
                end
              else
                od.objextname:=stringdup(od.objrealname^);
              consume(_SEMICOLON);
              od.make_all_methods_external;
              include(od.objectoptions,oo_is_external);
            end
          else { or also allow "public name 'x'"? }
            od.objextname:=stringdup(od.objrealname^);
        end;


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
         objecttype : tobjecttyp;
         isgeneric,
         isunique,
         istyperenaming : boolean;
         generictypelist : TFPObjectList;
         generictokenbuf : tdynamicarray;
         vmtbuilder : TVMTBuilder;
      begin
         old_block_type:=block_type;
         block_type:=bt_type;
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
                     (token=_DISPINTERFACE) or
                     (token=_OBJCCLASS) or
                     (token=_OBJCPROTOCOL)) and
                    (assigned(ttypesym(sym).typedef)) and
                    is_class_or_interface_or_dispinterface_or_objc(ttypesym(sym).typedef) and
                    (oo_is_forward in tobjectdef(ttypesym(sym).typedef).objectoptions) then
                  begin
                    case token of
                      _CLASS :
                        objecttype:=odt_class;
                      _INTERFACE :
                        if current_settings.interfacetype=it_interfacecom then
                          objecttype:=odt_interfacecom
                        else
                          objecttype:=odt_interfacecorba;
                      _DISPINTERFACE :
                        objecttype:=odt_dispinterface;
                      _OBJCCLASS :
                        objecttype:=odt_objcclass;
                      _OBJCPROTOCOL :
                        objecttype:=odt_objcprotocol;
                      else
                        internalerror(200811072);
                    end;
                    consume(token);
                    { we can ignore the result, the definition is modified }
                    object_dec(objecttype,orgtypename,nil,nil,tobjectdef(ttypesym(sym).typedef));
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
                      if is_class_or_interface_or_dispinterface_or_objc(hdef) then
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
                    try_consume_hintdirective(newtype.symoptions);
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
                      begin
                        try_consume_hintdirective(newtype.symoptions);
                        consume(_SEMICOLON);
                      end
                    else
                     begin
                       if not check_proc_directive(true) then
                         begin
                           try_consume_hintdirective(newtype.symoptions);
                           consume(_SEMICOLON);
                         end;
                       parse_var_proc_directives(tsym(newtype));
                       handle_calling_convention(tprocvardef(hdef));
                       if try_consume_hintdirective(newtype.symoptions) then
                         consume(_SEMICOLON);
                     end;
                  end;
                objectdef :
                  begin
                    try_consume_hintdirective(newtype.symoptions);
                    consume(_SEMICOLON);

                    { we have to know whether the class or protocol is
                      external before the vmt is built, because some errors/
                      hints depend on this  }
                    if is_objc_class_or_protocol(hdef) then
                      get_objc_class_or_protocol_external_status(tobjectdef(hdef));

                    { Build VMT indexes, skip for type renaming and forward classes }
                    if (hdef.typesym=newtype) and
                       not(oo_is_forward in tobjectdef(hdef).objectoptions) and
                       not(df_generic in hdef.defoptions) then
                      begin
                        vmtbuilder:=TVMTBuilder.Create(tobjectdef(hdef));
                        vmtbuilder.generate_vmt;
                        vmtbuilder.free;
                      end;

                    { In case of an objcclass, verify that all methods have a message
                      name set. We only check this now, because message names can be set
                      during the protocol (interface) mapping. At the same time, set the
                      mangled names.
                    }
                    if is_objc_class_or_protocol(hdef) then
                      tobjectdef(hdef).check_and_finish_messages;

                  end;
                recorddef :
                  begin
                    try_consume_hintdirective(newtype.symoptions);
                    consume(_SEMICOLON);
                  end;
                else
                  begin
                    try_consume_hintdirective(newtype.symoptions);
                    consume(_SEMICOLON);
                  end;
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
         resolve_forward_types;
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
                                sp[0]:=chr(tordconstnode(p).value.svalue);
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
