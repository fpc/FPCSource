{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
      cobjects,symsym,node;

    function  readconstant(const name:string;const filepos:tfileposinfo):pconstsym;

    procedure const_dec;
    procedure label_dec;
    procedure type_dec;
    procedure var_dec;
    procedure threadvar_dec;
    procedure resourcestring_dec;

implementation

    uses
       { common }
       cutils,
       { global }
       globtype,globals,tokens,verbose,
       systems,cpuinfo,
       { aasm }
       aasm,
       { symtable }
       symconst,symbase,symtype,symdef,symtable,types,
{$ifdef GDB}
       gdb,
{$endif}
       { pass 1 }
       pass_1,htypechk,
       nmat,nadd,ncal,nmem,nset,ncnv,ninl,ncon,nld,nflw,
       { parser }
       scanner,
       pbase,pexpr,ptype,ptconst,pdecsub,pdecvar,pdecobj,
       { codegen }
{$ifdef newcg}
       cgbase
{$else}
       hcodegen
{$endif}
       ;


    function readconstant(const name:string;const filepos:tfileposinfo):pconstsym;
      var
        hp : pconstsym;
        p : tnode;
        ps : pconstset;
        pd : pbestreal;
        sp : pchar;
        storetokenpos : tfileposinfo;
      begin
        readconstant:=nil;
        if name='' then
         internalerror(9584582);
        hp:=nil;
        p:=comp_expr(true);
        do_firstpass(p);
        storetokenpos:=akttokenpos;
        akttokenpos:=filepos;
        case p.nodetype of
           ordconstn:
             begin
                if is_constintnode(p) then
                  hp:=new(pconstsym,init_def(name,constint,tordconstnode(p).value,nil))
                else if is_constcharnode(p) then
                  hp:=new(pconstsym,init_def(name,constchar,tordconstnode(p).value,nil))
                else if is_constboolnode(p) then
                  hp:=new(pconstsym,init_def(name,constbool,tordconstnode(p).value,nil))
                else if p.resulttype^.deftype=enumdef then
                  hp:=new(pconstsym,init_def(name,constord,tordconstnode(p).value,p.resulttype))
                else if p.resulttype^.deftype=pointerdef then
                  hp:=new(pconstsym,init_def(name,constord,tordconstnode(p).value,p.resulttype))
                else internalerror(111);
             end;
           stringconstn:
             begin
                getmem(sp,tstringconstnode(p).len+1);
                move(tstringconstnode(p).value_str^,sp^,tstringconstnode(p).len+1);
                hp:=new(pconstsym,init_string(name,conststring,sp,tstringconstnode(p).len));
             end;
           realconstn :
             begin
                new(pd);
                pd^:=trealconstnode(p).value_real;
                hp:=new(pconstsym,init(name,constreal,longint(pd)));
             end;
           setconstn :
             begin
               new(ps);
               ps^:=tsetconstnode(p).value_set^;
               hp:=new(pconstsym,init_def(name,constset,longint(ps),p.resulttype));
             end;
           pointerconstn :
             begin
               hp:=new(pconstsym,init_def(name,constpointer,tordconstnode(p).value,p.resulttype));
             end;
           niln :
             begin
               hp:=new(pconstsym,init_def(name,constnil,0,p.resulttype));
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
         name : stringid;
         tt  : ttype;
         sym : psym;
         storetokenpos,filepos : tfileposinfo;
         old_block_type : tblock_type;
         skipequal : boolean;
      begin
         consume(_CONST);
         old_block_type:=block_type;
         block_type:=bt_const;
         repeat
           name:=pattern;
           filepos:=akttokenpos;
           consume(_ID);
           case token of

             _EQUAL:
                begin
                   consume(_EQUAL);
                   sym:=readconstant(name,filepos);
                   if assigned(sym) then
                    symtablestack^.insert(sym);
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
{$ifdef DELPHI_CONST_IN_RODATA}
                   if m_delphi in aktmodeswitches then
                     begin
                       if assigned(readtypesym) then
                        sym:=new(ptypedconstsym,initsym(name,readtypesym,true))
                       else
                        sym:=new(ptypedconstsym,init(name,def,true))
                     end
                   else
{$endif DELPHI_CONST_IN_RODATA}
                     begin
                       sym:=new(ptypedconstsym,inittype(name,tt,false))
                     end;
                   akttokenpos:=storetokenpos;
                   symtablestack^.insert(sym);
                   { procvar can have proc directives }
                   if (tt.def^.deftype=procvardef) then
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
                    end;
                   if not skipequal then
                    begin
                      { get init value }
                      consume(_EQUAL);
{$ifdef DELPHI_CONST_IN_RODATA}
                      if m_delphi in aktmodeswitches then
                       readtypedconst(tt.def,ptypedconstsym(sym),true)
                      else
{$endif DELPHI_CONST_IN_RODATA}
                       readtypedconst(tt.def,ptypedconstsym(sym),false);
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
         hl : pasmlabel;
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
                    hl^.refs:=0;
                  end
                else
                  getlabel(hl);
                symtablestack^.insert(new(plabelsym,init(pattern,hl)));
                consume(token);
             end;
           if token<>_SEMICOLON then consume(_COMMA);
         until not(token in [_ID,_INTCONST]);
         consume(_SEMICOLON);
      end;


    { search in symtablestack used, but not defined type }
    procedure resolve_type_forward(p : pnamedindexobject);
      var
        hpd,pd : pdef;
        stpos  : tfileposinfo;
        again  : boolean;
      begin
         { Check only typesyms or record/object fields }
         case psym(p)^.typ of
           typesym :
             pd:=ptypesym(p)^.restype.def;
           varsym :
             if (psym(p)^.owner^.symtabletype in [objectsymtable,recordsymtable]) then
               pd:=pvarsym(p)^.vartype.def
             else
               exit;
           else
             exit;
         end;
         repeat
           again:=false;
           case pd^.deftype of
             arraydef :
               begin
                 { elementtype could also be defined using a forwarddef }
                 pd:=parraydef(pd)^.elementtype.def;
                 again:=true;
               end;
             pointerdef,
             classrefdef :
               begin
                 { classrefdef inherits from pointerdef }
                 hpd:=ppointerdef(pd)^.pointertype.def;
                 { still a forward def ? }
                 if hpd^.deftype=forwarddef then
                  begin
                    { try to resolve the forward }
                    { get the correct position for it }
                    stpos:=akttokenpos;
                    akttokenpos:=pforwarddef(hpd)^.forwardpos;
                    resolving_forward:=true;
                    make_ref:=false;
                    getsym(pforwarddef(hpd)^.tosymname,false);
                    make_ref:=true;
                    resolving_forward:=false;
                    akttokenpos:=stpos;
                    { we don't need the forwarddef anymore, dispose it }
                    dispose(hpd,done);
                    { was a type sym found ? }
                    if assigned(srsym) and
                       (srsym^.typ=typesym) then
                     begin
                       ppointerdef(pd)^.pointertype.setsym(srsym);
                       { avoid wrong unused warnings web bug 801 PM }
                       inc(pstoredsym(srsym)^.refs);
{$ifdef GDB}
                       if (cs_debuginfo in aktmoduleswitches) and assigned(debuglist) and
                          (psym(p)^.owner^.symtabletype in [globalsymtable,staticsymtable]) then
                        begin
                          ptypesym(p)^.isusedinstab := true;
                          ptypesym(p)^.concatstabto(debuglist);
                        end;
{$endif GDB}
                       { we need a class type for classrefdef }
                       if (pd^.deftype=classrefdef) and
                          not((ptypesym(srsym)^.restype.def^.deftype=objectdef) and
                              pobjectdef(ptypesym(srsym)^.restype.def)^.is_class) then
                         Message1(type_e_class_type_expected,ptypesym(srsym)^.restype.def^.typename);
                     end
                    else
                     begin
                       MessagePos1(psym(p)^.fileinfo,sym_e_forward_type_not_resolved,p^.name);
                       { try to recover }
                       ppointerdef(pd)^.pointertype.def:=generrordef;
                     end;
                  end;
               end;
             recorddef :
               precorddef(pd)^.symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}resolve_type_forward);
             objectdef :
               begin
                 if not(m_fpc in aktmodeswitches) and
                    (oo_is_forward in pobjectdef(pd)^.objectoptions) then
                  begin
                    { only give an error as the implementation may follow in an
                      other type block which is allowed by FPC modes }
                    MessagePos1(psym(p)^.fileinfo,sym_e_forward_type_not_resolved,p^.name);
                  end
                 else
                  begin
                    { Check all fields of the object declaration, but don't
                      check objectdefs in objects/records, because these
                      can't exist (anonymous objects aren't allowed) }
                    if not(psym(p)^.owner^.symtabletype in [objectsymtable,recordsymtable]) then
                     pobjectdef(pd)^.symtable^.foreach({$ifdef FPCPROCVAR}@{$endif}resolve_type_forward);
                  end;
               end;
          end;
        until not again;
      end;


    { reads a type declaration to the symbol table }
    procedure type_dec;
      var
         typename,orgtypename : stringid;
         newtype  : ptypesym;
         sym      : psym;
         tt       : ttype;
         defpos,storetokenpos : tfileposinfo;
         old_block_type : tblock_type;
      begin
         old_block_type:=block_type;
         block_type:=bt_type;
         consume(_TYPE);
         typecanbeforward:=true;
         repeat
           typename:=pattern;
           orgtypename:=orgpattern;
           defpos:=akttokenpos;
           consume(_ID);
           consume(_EQUAL);
           { support 'ttype=type word' syntax }
           if token=_TYPE then
            Consume(_TYPE);
           { is the type already defined? }
           getsym(typename,false);
           sym:=srsym;
           newtype:=nil;
           { found a symbol with this name? }
           if assigned(sym) then
            begin
              if (sym^.typ=typesym) then
               begin
                 if (token=_CLASS) and
                    (assigned(ptypesym(sym)^.restype.def)) and
                    (ptypesym(sym)^.restype.def^.deftype=objectdef) and
                    pobjectdef(ptypesym(sym)^.restype.def)^.is_class and
                    (oo_is_forward in pobjectdef(ptypesym(sym)^.restype.def)^.objectoptions) then
                  begin
                    { we can ignore the result   }
                    { the definition is modified }
                    object_dec(orgtypename,pobjectdef(ptypesym(sym)^.restype.def));
                    newtype:=ptypesym(sym);
                  end;
               end;
            end;
           { no old type reused ? Then insert this new type }
           if not assigned(newtype) then
            begin
              { insert the new type first with an errordef, so that
                referencing the type before it's really set it
                will give an error (PFV) }
              tt.setdef(generrordef);
              storetokenpos:=akttokenpos;
              newtype:=new(ptypesym,init(orgtypename,tt));
              symtablestack^.insert(newtype);
              akttokenpos:=defpos;
              akttokenpos:=storetokenpos;
              { read the type definition }
              read_type(tt,orgtypename);
              { update the definition of the type }
              newtype^.restype:=tt;
              if not assigned(tt.sym) then
                tt.sym:=newtype;
              if assigned(tt.def) and not assigned(tt.def^.typesym) then
                tt.def^.typesym:=newtype;
            end;
           if assigned(newtype^.restype.def) and
              (newtype^.restype.def^.deftype=procvardef) then
            begin
              if not is_proc_directive(token) then
               consume(_SEMICOLON);
              parse_var_proc_directives(psym(newtype));
            end
           else
            consume(_SEMICOLON);
         until token<>_ID;
         typecanbeforward:=false;
         symtablestack^.foreach({$ifdef FPCPROCVAR}@{$endif}resolve_type_forward);
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
        if not(symtablestack^.symtabletype in [staticsymtable,globalsymtable]) then
          message(parser_e_threadvars_only_sg);
        read_var_decs(false,false,true);
      end;


    procedure resourcestring_dec;
      var
         name : stringid;
         p : tnode;
         storetokenpos,filepos : tfileposinfo;
         old_block_type : tblock_type;
         sp : pchar;
      begin
         consume(_RESOURCESTRING);
         if not(symtablestack^.symtabletype in [staticsymtable,globalsymtable]) then
           message(parser_e_resourcestring_only_sg);
         old_block_type:=block_type;
         block_type:=bt_const;
         repeat
           name:=pattern;
           filepos:=akttokenpos;
           consume(_ID);
           case token of
             _EQUAL:
                begin
                   consume(_EQUAL);
                   p:=comp_expr(true);
                   do_firstpass(p);
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
                                symtablestack^.insert(new(pconstsym,init_string(name,constresourcestring,sp,1)));
                             end
                           else
                             Message(cg_e_illegal_expression);
                        end;
                      stringconstn:
                        begin
                           getmem(sp,tstringconstnode(p).len+1);
                           move(tstringconstnode(p).value_str^,sp^,tstringconstnode(p).len+1);
                           symtablestack^.insert(new(pconstsym,init_string(name,constresourcestring,sp,tstringconstnode(p).len)));
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
  Revision 1.18  2000-10-31 22:02:49  peter
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