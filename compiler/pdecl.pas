{
    $Id$
    Copyright (c) 1998-2001 by Florian Klaempfl

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

{$i defines.inc}

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
       aasm,fmodule,
       { symtable }
       symconst,symbase,symtype,symdef,symtable,
       { pass 1 }
       nmat,nadd,ncal,nset,ncnv,ninl,ncon,nld,nflw,nobj,
       { parser }
       scanner,
       pbase,pexpr,ptype,ptconst,pdecsub,pdecvar,pdecobj;


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
                    getdatalabel(hl);
                    { we still want a warning if unused }
                    hl.refs:=0;
                  end
                else
                  getlabel(hl);
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
         istyperenaming : boolean;
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
            Consume(_TYPE);
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
                  is_class(tt.def)) then
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
  Revision 1.43  2002-05-12 16:53:08  peter
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

  Revision 1.40  2001/12/06 17:57:35  florian
    + parasym to tparaitem added

  Revision 1.39  2001/10/25 21:22:35  peter
    * calling convention rewrite

  Revision 1.38  2001/10/24 10:26:53  marco
   * Don't parse proc directives after type renaming of procvars

  Revision 1.37  2001/10/20 20:30:21  peter
    * read only typed const support, switch $J-

  Revision 1.36  2001/10/20 19:28:39  peter
    * interface 2 guid support
    * guid constants support

  Revision 1.35  2001/10/20 17:20:13  peter
    * fixed generation of rtti for virtualmethods

  Revision 1.34  2001/09/19 11:06:03  michael
  * realname updated for some hints
  * realname used for consts,labels

  Revision 1.33  2001/09/02 21:18:28  peter
    * split constsym.value in valueord,valueordptr,valueptr. The valueordptr
      is used for holding target platform pointer values. As those can be
      bigger than the source platform.

  Revision 1.32  2001/08/30 20:13:53  peter
    * rtti/init table updates
    * rttisym for reusable global rtti/init info
    * support published for interfaces

  Revision 1.31  2001/06/03 21:57:35  peter
    + hint directive parsing support

  Revision 1.30  2001/05/08 21:06:31  florian
    * some more support for widechars commited especially
      regarding type casting and constants

  Revision 1.29  2001/04/13 01:22:11  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.28  2001/04/04 22:43:50  peter
    * remove unnecessary calls to firstpass

  Revision 1.27  2001/04/04 21:30:43  florian
    * applied several fixes to get the DD8 Delphi Unit compiled
     e.g. "forward"-interfaces are working now

  Revision 1.26  2001/04/02 21:20:31  peter
    * resulttype rewrite

  Revision 1.25  2001/03/11 22:58:49  peter
    * getsym redesign, removed the globals srsym,srsymtable

  Revision 1.24  2000/12/25 00:07:27  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.23  2000/12/07 17:19:42  jonas
    * new constant handling: from now on, hex constants >$7fffffff are
      parsed as unsigned constants (otherwise, $80000000 got sign extended
      and became $ffffffff80000000), all constants in the longint range
      become longints, all constants >$7fffffff and <=cardinal($ffffffff)
      are cardinals and the rest are int64's.
    * added lots of longint typecast to prevent range check errors in the
      compiler and rtl
    * type casts of symbolic ordinal constants are now preserved
    * fixed bug where the original resulttype.def wasn't restored correctly
      after doing a 64bit rangecheck

  Revision 1.22  2000/11/29 00:30:35  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.21  2000/11/12 22:17:46  peter
    * some realname updates for messages

  Revision 1.20  2000/11/11 16:19:11  peter
    * allow far directive for pointer type declarations

  Revision 1.19  2000/11/04 14:25:20  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.18  2000/10/31 22:02:49  peter
    * symtable splitted, no real code changes

  Revision 1.17  2000/10/14 10:14:51  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.16  2000/09/24 21:19:50  peter
    * delphi compile fixes

  Revision 1.15  2000/09/24 15:06:21  peter
    * use defines.inc

  Revision 1.14  2000/09/11 17:00:23  florian
    + first implementation of Netware Module support, thanks to
      Armin Diehl (diehl@nordrhein.de) for providing the patches

  Revision 1.13  2000/08/27 20:19:39  peter
    * store strings with case in ppu, when an internal symbol is created
      a '$' is prefixed so it's not automatic uppercased

  Revision 1.12  2000/08/27 16:11:51  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.11  2000/08/20 15:01:17  peter
    * don't allow forward class in separate type blocks for delphi (merged)

  Revision 1.10  2000/08/17 09:17:19  pierre
   * fix go32v2 cycle problem

  Revision 1.9  2000/08/16 18:33:53  peter
    * splitted namedobjectitem.next into indexnext and listnext so it
      can be used in both lists
    * don't allow "word = word" type definitions (merged)

  Revision 1.8  2000/08/13 13:11:28  peter
    * put defaultpara values in parast and changed the name to
      'def<Parameter name>'

  Revision 1.7  2000/08/13 08:42:59  peter
    * support absolute refering to funcret (merged)

  Revision 1.6  2000/08/02 19:49:59  peter
    * first things for default parameters

  Revision 1.5  2000/07/30 17:04:43  peter
    * merged fixes

  Revision 1.4  2000/07/14 05:11:49  michael
  + Patch to 1.1

  Revision 1.3  2000/07/13 12:08:26  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:44  michael
  + removed logs

}
