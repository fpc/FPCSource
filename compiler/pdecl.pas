{
    $Id$
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
    procedure var_dec;
    procedure threadvar_dec;
    procedure resourcestring_dec;

implementation

    uses
       { common }
       cutils,cclasses,
       { global }
       globtype,tokens,verbose,
       systems,
       { aasm }
       aasmbase,aasmtai,aasmcpu,fmodule,
       { symtable }
       symconst,symbase,symtype,symdef,symtable,paramgr,
       { pass 1 }
       nmat,nadd,ncal,nset,ncnv,ninl,ncon,nld,nflw,nobj,
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
        storetokenpos : tfileposinfo;
      begin
        readconstant:=nil;
        if orgname='' then
         internalerror(9584582);
        hp:=nil;
        p:=comp_expr(true);
        storetokenpos:=akttokenpos;
        akttokenpos:=filepos;
        case p.nodetype of
           ordconstn:
             begin
                if is_constintnode(p) then
                  hp:=tconstsym.create_ord_typed(orgname,constint,tordconstnode(p).value,tordconstnode(p).resulttype)
                else if is_constcharnode(p) then
                  hp:=tconstsym.create_ord(orgname,constchar,tordconstnode(p).value)
                else if is_constboolnode(p) then
                  hp:=tconstsym.create_ord(orgname,constbool,tordconstnode(p).value)
                else if is_constwidecharnode(p) then
                  hp:=tconstsym.create_ord(orgname,constwchar,tordconstnode(p).value)
                else if p.resulttype.def.deftype=enumdef then
                  hp:=tconstsym.create_ord_typed(orgname,constord,tordconstnode(p).value,p.resulttype)
                else if p.resulttype.def.deftype=pointerdef then
                  hp:=tconstsym.create_ordptr_typed(orgname,constpointer,tordconstnode(p).value,p.resulttype)
                else internalerror(111);
             end;
           stringconstn:
             begin
                getmem(sp,tstringconstnode(p).len+1);
                move(tstringconstnode(p).value_str^,sp^,tstringconstnode(p).len+1);
                hp:=tconstsym.create_string(orgname,conststring,sp,tstringconstnode(p).len);
             end;
           realconstn :
             begin
                new(pd);
                pd^:=trealconstnode(p).value_real;
                hp:=tconstsym.create_ptr(orgname,constreal,pd);
             end;
           setconstn :
             begin
               new(ps);
               ps^:=tsetconstnode(p).value_set^;
               hp:=tconstsym.create_ptr_typed(orgname,constset,ps,p.resulttype);
             end;
           pointerconstn :
             begin
               hp:=tconstsym.create_ordptr_typed(orgname,constpointer,tpointerconstnode(p).value,p.resulttype);
             end;
           niln :
             begin
               hp:=tconstsym.create_ord_typed(orgname,constnil,0,p.resulttype);
             end;
           typen :
             begin
               if is_interface(p.resulttype.def) then
                begin
                  if tobjectdef(p.resulttype.def).isiidguidvalid then
                   begin
                     new(pg);
                     pg^:=tobjectdef(p.resulttype.def).iidguid;
                     hp:=tconstsym.create_ptr(orgname,constguid,pg);
                   end
                  else
                   Message1(parser_e_interface_has_no_guid,tobjectdef(p.resulttype.def).objrealname^);
                end
               else
                Message(cg_e_illegal_expression);
             end;
           else
             Message(cg_e_illegal_expression);
        end;
        akttokenpos:=storetokenpos;
        p.free;
        readconstant:=hp;
      end;


    procedure const_dec;
      var
         orgname : stringid;
         tt  : ttype;
         sym : tsym;
         storetokenpos,filepos : tfileposinfo;
         old_block_type : tblock_type;
         skipequal : boolean;
      begin
         consume(_CONST);
         old_block_type:=block_type;
         block_type:=bt_const;
         repeat
           orgname:=orgpattern;
           filepos:=akttokenpos;
           consume(_ID);
           case token of

             _EQUAL:
                begin
                   consume(_EQUAL);
                   sym:=readconstant(orgname,filepos);
                   if assigned(sym) then
                    symtablestack.insert(sym);
                   try_consume_hintdirective(sym.symoptions);
                   consume(_SEMICOLON);
                end;

             _COLON:
                begin
                   { set the blocktype first so a consume also supports a
                     caret, to support const s : ^string = nil }
                   block_type:=bt_type;
                   consume(_COLON);
                   ignore_equal:=true;
                   read_type(tt,'');
                   ignore_equal:=false;
                   block_type:=bt_const;
                   skipequal:=false;
                   { create symbol }
                   storetokenpos:=akttokenpos;
                   akttokenpos:=filepos;
                   sym:=ttypedconstsym.createtype(orgname,tt,(cs_typed_const_writable in aktlocalswitches));
                   akttokenpos:=storetokenpos;
                   symtablestack.insert(sym);
                   symtablestack.insertconstdata(sym);
                   { procvar can have proc directives }
                   if (tt.def.deftype=procvardef) then
                    begin
                      { support p : procedure;stdcall=nil; }
                      if (token=_SEMICOLON) then
                       begin
                         consume(_SEMICOLON);
                         if is_proc_directive(token) then
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
                         if is_proc_directive(token) then
                          parse_var_proc_directives(sym);
                       end;
                      { add default calling convention }
                      handle_calling_convention(nil,tabstractprocdef(tt.def));
                      paramanager.create_param_loc_info(tabstractprocdef(tt.def));
                    end;
                   if not skipequal then
                    begin
                      { get init value }
                      consume(_EQUAL);
                      readtypedconst(tt,ttypedconstsym(sym),(cs_typed_const_writable in aktlocalswitches));
                      try_consume_hintdirective(sym.symoptions);
                      consume(_SEMICOLON);
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
      var
         hl : tasmlabel;
      begin
         consume(_LABEL);
         if not(cs_support_goto in aktmoduleswitches) then
           Message(sym_e_goto_and_label_not_supported);
         repeat
           if not(token in [_ID,_INTCONST]) then
             consume(_ID)
           else
             begin
                if (cs_create_smart in aktmoduleswitches) then
                  begin
                    objectlibrary.getdatalabel(hl);
                    { we still want a warning if unused }
                    hl.refs:=0;
                  end
                else
                  objectlibrary.getlabel(hl);
                if token=_ID then
                 symtablestack.insert(tlabelsym.create(orgpattern,hl))
                else
                 symtablestack.insert(tlabelsym.create(pattern,hl));
                consume(token);
             end;
           if token<>_SEMICOLON then consume(_COMMA);
         until not(token in [_ID,_INTCONST]);
         consume(_SEMICOLON);
      end;


    { search in symtablestack used, but not defined type }
    procedure resolve_type_forward(p : tnamedindexitem;arg:pointer);
      var
        hpd,pd : tdef;
        stpos  : tfileposinfo;
        again  : boolean;
        srsym  : tsym;
        srsymtable : tsymtable;
      begin
         { Check only typesyms or record/object fields }
         case tsym(p).typ of
           typesym :
             pd:=ttypesym(p).restype.def;
           varsym :
             if (tsym(p).owner.symtabletype in [objectsymtable,recordsymtable]) then
               pd:=tvarsym(p).vartype.def
             else
               exit;
           else
             exit;
         end;
         repeat
           again:=false;
           case pd.deftype of
             arraydef :
               begin
                 { elementtype could also be defined using a forwarddef }
                 pd:=tarraydef(pd).elementtype.def;
                 again:=true;
               end;
             pointerdef,
             classrefdef :
               begin
                 { classrefdef inherits from pointerdef }
                 hpd:=tpointerdef(pd).pointertype.def;
                 { still a forward def ? }
                 if hpd.deftype=forwarddef then
                  begin
                    { try to resolve the forward }
                    { get the correct position for it }
                    stpos:=akttokenpos;
                    akttokenpos:=tforwarddef(hpd).forwardpos;
                    resolving_forward:=true;
                    make_ref:=false;
                    searchsym(tforwarddef(hpd).tosymname,srsym,srsymtable);
                    make_ref:=true;
                    resolving_forward:=false;
                    akttokenpos:=stpos;
                    { we don't need the forwarddef anymore, dispose it }
                    hpd.free;
                    tpointerdef(pd).pointertype.def:=nil; { if error occurs }
                    { was a type sym found ? }
                    if assigned(srsym) and
                       (srsym.typ=typesym) then
                     begin
                       tpointerdef(pd).pointertype.setsym(srsym);
                       { avoid wrong unused warnings web bug 801 PM }
                       inc(tstoredsym(srsym).refs);
{$ifdef GDB}
                       if (cs_debuginfo in aktmoduleswitches) and assigned(debuglist) and
                          (tsym(p).owner.symtabletype in [globalsymtable,staticsymtable]) then
                        begin
                          ttypesym(p).isusedinstab := true;
                          ttypesym(p).concatstabto(debuglist);
                        end;
{$endif GDB}
                       { we need a class type for classrefdef }
                       if (pd.deftype=classrefdef) and
                          not(is_class(ttypesym(srsym).restype.def)) then
                         Message1(type_e_class_type_expected,ttypesym(srsym).restype.def.typename);
                     end
                    else
                     begin
                       MessagePos1(tsym(p).fileinfo,sym_e_forward_type_not_resolved,tsym(p).realname);
                       { try to recover }
                       tpointerdef(pd).pointertype:=generrortype;
                     end;
                  end;
               end;
             recorddef :
               trecorddef(pd).symtable.foreach_static({$ifdef FPCPROCVAR}@{$endif}resolve_type_forward,nil);
             objectdef :
               begin
                 if not(m_fpc in aktmodeswitches) and
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
                    if not(tsym(p).owner.symtabletype in [objectsymtable,recordsymtable]) then
                     tobjectdef(pd).symtable.foreach_static({$ifdef FPCPROCVAR}@{$endif}resolve_type_forward,nil);
                  end;
               end;
          end;
        until not again;
      end;


    { reads a type declaration to the symbol table }
    procedure type_dec;
      var
         typename,orgtypename : stringid;
         newtype  : ttypesym;
         sym      : tsym;
         srsymtable : tsymtable;
         tt       : ttype;
         oldfilepos,
         defpos,storetokenpos : tfileposinfo;
         old_block_type : tblock_type;
         ch       : tclassheader;
         unique,istyperenaming : boolean;

      begin
         old_block_type:=block_type;
         block_type:=bt_type;
         consume(_TYPE);
         typecanbeforward:=true;
         repeat
           typename:=pattern;
           orgtypename:=orgpattern;
           defpos:=akttokenpos;
           istyperenaming:=false;
           consume(_ID);
           consume(_EQUAL);
           { support 'ttype=type word' syntax }
           if token=_TYPE then
             begin
                Consume(_TYPE);
                unique:=true;
             end
           else
             unique:=false;
           { is the type already defined? }
           searchsym(typename,sym,srsymtable);
           newtype:=nil;
           { found a symbol with this name? }
           if assigned(sym) then
            begin
              if (sym.typ=typesym) then
               begin
                 if ((token=_CLASS) or
                     (token=_INTERFACE)) and
                    (assigned(ttypesym(sym).restype.def)) and
                    is_class_or_interface(ttypesym(sym).restype.def) and
                    (oo_is_forward in tobjectdef(ttypesym(sym).restype.def).objectoptions) then
                  begin
                    { we can ignore the result   }
                    { the definition is modified }
                    object_dec(orgtypename,tobjectdef(ttypesym(sym).restype.def));
                    newtype:=ttypesym(sym);
                    tt:=newtype.restype;
                  end;
               end;
            end;
           { no old type reused ? Then insert this new type }
           if not assigned(newtype) then
            begin
              { insert the new type first with an errordef, so that
                referencing the type before it's really set it
                will give an error (PFV) }
              tt:=generrortype;
              storetokenpos:=akttokenpos;
              newtype:=ttypesym.create(orgtypename,tt);
              symtablestack.insert(newtype);
              akttokenpos:=defpos;
              akttokenpos:=storetokenpos;
              { read the type definition }
              read_type(tt,orgtypename);
              { update the definition of the type }
              newtype.restype:=tt;
              if assigned(tt.sym) then
                istyperenaming:=true
              else
                tt.sym:=newtype;
              if assigned(tt.def) and not assigned(tt.def.typesym) then
                tt.def.typesym:=newtype;
              { KAZ: handle TGUID declaration in system unit }
              if (cs_compilesystem in aktmoduleswitches) and not assigned(rec_tguid) and
                 (typename='TGUID') and { name: TGUID and size=16 bytes that is 128 bits }
                 assigned(tt.def) and (tt.def.deftype=recorddef) and (tt.def.size=16) then
                rec_tguid:=trecorddef(tt.def);
            end;
           if assigned(tt.def) then
            begin
              if unique then
                include(tt.def.defoptions,df_unique)
              else
                exclude(tt.def.defoptions,df_unique);
              case tt.def.deftype of
                pointerdef :
                  begin
                    consume(_SEMICOLON);
                    if try_to_consume(_FAR) then
                     begin
                       tpointerdef(tt.def).is_far:=true;
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
                       if not is_proc_directive(token) then
                        consume(_SEMICOLON);
                       parse_var_proc_directives(tsym(newtype));
                     end;
                    paramanager.create_param_loc_info(tabstractprocdef(tt.def));
                  end;
                objectdef,
                recorddef :
                  begin
                    try_consume_hintdirective(newtype.symoptions);
                    consume(_SEMICOLON);
                  end;
                else
                  consume(_SEMICOLON);
              end;
            end;

           { Write tables if we are the typesym that defines
             this type. This will not be done for simple type renamings }
           if (tt.def.typesym=newtype) then
            begin
              { file position }
              oldfilepos:=aktfilepos;
              aktfilepos:=newtype.fileinfo;

              { generate persistent init/final tables when it's declared in the interface so it can
                be reused in other used }
              if (not current_module.in_implementation) and
                 (tt.def.needs_inittable or
                  (is_class(tt.def) and
                   not(oo_is_forward in tobjectdef(tt.def).objectoptions)
                  )
                 ) then
                generate_inittable(newtype);

              { for objects we should write the vmt and interfaces.
                This need to be done after the rtti has been written, because
                it can contain a reference to that data (PFV)
                This is not for forward classes }
              if (tt.def.deftype=objectdef) and
                 not(oo_is_forward in tobjectdef(tt.def).objectoptions) then
               begin
                 ch:=cclassheader.create(tobjectdef(tt.def));
                 { generate and check virtual methods, must be done
                   before RTTI is written }
                 ch.genvmt;
                 { generate rtti info if published items are available }
                 if (oo_can_have_published in tobjectdef(tt.def).objectoptions) then
                   generate_rtti(newtype);
                 if is_interface(tobjectdef(tt.def)) then
                   ch.writeinterfaceids;
                 if (oo_has_vmt in tobjectdef(tt.def).objectoptions) then
                   ch.writevmt;
                 ch.free;
               end;

              aktfilepos:=oldfilepos;
            end;
         until token<>_ID;
         typecanbeforward:=false;
         symtablestack.foreach_static({$ifdef FPCPROCVAR}@{$endif}resolve_type_forward,nil);
         block_type:=old_block_type;
      end;


    procedure var_dec;
    { parses variable declarations and inserts them in }
    { the top symbol table of symtablestack         }
      begin
        consume(_VAR);
        read_var_decs(false,false,false);
      end;


    procedure threadvar_dec;
    { parses thread variable declarations and inserts them in }
    { the top symbol table of symtablestack                }
      begin
        consume(_THREADVAR);
        if not(symtablestack.symtabletype in [staticsymtable,globalsymtable]) then
          message(parser_e_threadvars_only_sg);
        read_var_decs(false,false,true);
      end;


    procedure resourcestring_dec;
      var
         orgname : stringid;
         p : tnode;
         storetokenpos,filepos : tfileposinfo;
         old_block_type : tblock_type;
         sp : pchar;
      begin
         consume(_RESOURCESTRING);
         if not(symtablestack.symtabletype in [staticsymtable,globalsymtable]) then
           message(parser_e_resourcestring_only_sg);
         old_block_type:=block_type;
         block_type:=bt_const;
         repeat
           orgname:=orgpattern;
           filepos:=akttokenpos;
           consume(_ID);
           case token of
             _EQUAL:
                begin
                   consume(_EQUAL);
                   p:=comp_expr(true);
                   storetokenpos:=akttokenpos;
                   akttokenpos:=filepos;
                   case p.nodetype of
                      ordconstn:
                        begin
                           if is_constcharnode(p) then
                             begin
                                getmem(sp,2);
                                sp[0]:=chr(tordconstnode(p).value);
                                sp[1]:=#0;
                                symtablestack.insert(tconstsym.create_string(orgname,constresourcestring,sp,1));
                             end
                           else
                             Message(cg_e_illegal_expression);
                        end;
                      stringconstn:
                        begin
                           getmem(sp,tstringconstnode(p).len+1);
                           move(tstringconstnode(p).value_str^,sp^,tstringconstnode(p).len+1);
                           symtablestack.insert(tconstsym.create_string(orgname,constresourcestring,sp,tstringconstnode(p).len));
                        end;
                      else
                        Message(cg_e_illegal_expression);
                   end;
                   akttokenpos:=storetokenpos;
                   consume(_SEMICOLON);
                   p.free;
                end;
              else consume(_EQUAL);
           end;
         until token<>_ID;
         block_type:=old_block_type;
      end;

end.
{
  $Log$
  Revision 1.54  2002-10-06 12:25:05  florian
    + proper support of type <id> = type <another id>;

  Revision 1.53  2002/08/25 19:25:19  peter
    * sym.insert_in_data removed
    * symtable.insertvardata/insertconstdata added
    * removed insert_in_data call from symtable.insert, it needs to be
      called separatly. This allows to deref the address calculation
    * procedures now calculate the parast addresses after the procedure
      directives are parsed. This fixes the cdecl parast problem
    * push_addr_param has an extra argument that specifies if cdecl is used
      or not

  Revision 1.52  2002/08/12 15:08:40  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.51  2002/08/11 14:32:27  peter
    * renamed current_library to objectlibrary

  Revision 1.50  2002/08/11 13:24:12  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.49  2002/07/29 21:23:43  florian
    * more fixes for the ppc
    + wrappers for the tcnvnode.first_* stuff introduced

  Revision 1.48  2002/07/01 18:46:25  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.47  2002/06/12 13:20:29  jonas
    * fix from Florian for init/final info of forward classes

  Revision 1.46  2002/05/18 13:34:12  peter
    * readded missing revisions

  Revision 1.45  2002/05/16 19:46:42  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.43  2002/05/12 16:53:08  peter
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

  Revision 1.42  2002/04/19 15:46:02  peter
    * mangledname rewrite, tprocdef.mangledname is now created dynamicly
      in most cases and not written to the ppu
    * add mangeledname_prefix() routine to generate the prefix of
      manglednames depending on the current procedure, object and module
    * removed static procprefix since the mangledname is now build only
      on demand from tprocdef.mangledname

  Revision 1.41  2002/03/04 17:54:59  peter
    * allow oridinal labels again

}
