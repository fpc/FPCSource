{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Does the parsing of the statements

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
unit pstatmnt;

{$i fpcdefs.inc}

interface
    uses
      tokens,node;


    function statement_block(starttoken : ttoken) : tnode;

    { reads an assembler block }
    function assembler_block : tnode;


implementation

    uses
       { common }
       cutils,
       { global }
       globtype,globals,verbose,
       systems,cpuinfo,
       { aasm }
       cpubase,aasmbase,aasmtai,aasmcpu,
       { symtable }
       symconst,symbase,symtype,symdef,symsym,symtable,defbase,paramgr,
       { pass 1 }
       pass_1,htypechk,
       nbas,nmat,nadd,ncal,nmem,nset,ncnv,ninl,ncon,nld,nflw,
       { parser }
       scanner,
       pbase,pexpr,
       { codegen }
       rgobj,cgbase
{$ifdef i386}
  {$ifndef NoRa386Int}
       ,ra386int
  {$endif NoRa386Int}
  {$ifndef NoRa386Att}
       ,ra386att
  {$endif NoRa386Att}
  {$ifndef NoRa386Dir}
       ,ra386dir
  {$endif NoRa386Dir}
{$endif i386}
{$ifdef powerpc}
  {$ifndef NoRaPPCDir}
       ,rappcdir
  {$endif NoRaPPCDir}
{$endif powerpc}
{$ifdef x86_64}
  {$ifndef NoRax86Dir}
       ,rax86dir
  {$endif NoRax86Dir}
{$endif i386}
{$ifdef m68k}
  {$ifndef NoRa68kMot}
       ,ra68kmot
  {$endif NoRa68kMot}
{$endif m68k}
       { codegen }
{$ifdef newcg}
       ,cgbase
{$endif newcg}
       ;


    function statement : tnode;forward;


    function if_statement : tnode;
      var
         ex,if_a,else_a : tnode;
      begin
         consume(_IF);
         ex:=comp_expr(true);
         consume(_THEN);
         if token<>_ELSE then
           if_a:=statement
         else
           if_a:=nil;

         if try_to_consume(_ELSE) then
            else_a:=statement
         else
           else_a:=nil;
         if_statement:=genloopnode(ifn,ex,if_a,else_a,false);
      end;

    { creates a block (list) of statements, til the next END token }
    function statements_til_end : tnode;

      var
         first,last : tstatementnode;

      begin
         first:=nil;
         while token<>_END do
           begin
              if first=nil then
                begin
                   last:=cstatementnode.create(nil,statement);
                   first:=last;
                end
              else
                begin
                   last.left:=cstatementnode.create(nil,statement);
                   last:=tstatementnode(last.left);
                end;
              if not try_to_consume(_SEMICOLON) then
                break;
              consume_emptystats;
           end;
         consume(_END);
         statements_til_end:=cblocknode.create(first);
      end;


    function case_statement : tnode;
      var
         { contains the label number of currently parsed case block }
         aktcaselabel : tasmlabel;
         firstlabel : boolean;
         root : pcaserecord;

         { the typ of the case expression }
         casedef : tdef;

      procedure newcaselabel(l,h : TConstExprInt;first:boolean);

        var
           hcaselabel : pcaserecord;

        procedure insertlabel(var p : pcaserecord);

          begin
             if p=nil then p:=hcaselabel
             else
                if (p^._low>hcaselabel^._low) and
                   (p^._low>hcaselabel^._high) then
                  if (hcaselabel^.statement = p^.statement) and
                     (p^._low = hcaselabel^._high + 1) then
                    begin
                      p^._low := hcaselabel^._low;
                      dispose(hcaselabel);
                    end
                  else
                    insertlabel(p^.less)
                else
                  if (p^._high<hcaselabel^._low) and
                     (p^._high<hcaselabel^._high) then
                    if (hcaselabel^.statement = p^.statement) and
                       (p^._high+1 = hcaselabel^._low) then
                      begin
                        p^._high := hcaselabel^._high;
                        dispose(hcaselabel);
                      end
                    else
                      insertlabel(p^.greater)
                  else Message(parser_e_double_caselabel);
          end;

        begin
           new(hcaselabel);
           hcaselabel^.less:=nil;
           hcaselabel^.greater:=nil;
           hcaselabel^.statement:=aktcaselabel;
           hcaselabel^.firstlabel:=first;
           getlabel(hcaselabel^._at);
           hcaselabel^._low:=l;
           hcaselabel^._high:=h;
           insertlabel(root);
        end;

      var
         code,caseexpr,p,instruc,elseblock : tnode;
         hl1,hl2 : TConstExprInt;
         casedeferror : boolean;

      begin
         consume(_CASE);
         caseexpr:=comp_expr(true);
       { determines result type }
         rg.cleartempgen;
         do_resulttypepass(caseexpr);
         casedeferror:=false;
         casedef:=caseexpr.resulttype.def;
         if (not assigned(casedef)) or
            not(is_ordinal(casedef)) then
          begin
            CGMessage(type_e_ordinal_expr_expected);
            { create a correct tree }
            caseexpr.free;
            caseexpr:=cordconstnode.create(0,u32bittype);
            { set error flag so no rangechecks are done }
            casedeferror:=true;
          end;

         consume(_OF);
         inc(statement_level);
         root:=nil;
         instruc:=nil;
         repeat
           getlabel(aktcaselabel);
           firstlabel:=true;

           { maybe an instruction has more case labels }
           repeat
             p:=expr;
             if is_widechar(casedef) then
               begin
                  if (p.nodetype=rangen) then
                    begin
                       trangenode(p).left:=ctypeconvnode.create(trangenode(p).left,cwidechartype);
                       trangenode(p).right:=ctypeconvnode.create(trangenode(p).right,cwidechartype);
                       do_resulttypepass(trangenode(p).left);
                       do_resulttypepass(trangenode(p).right);
                    end
                  else
                    begin
                       p:=ctypeconvnode.create(p,cwidechartype);
                       do_resulttypepass(p);
                    end;
               end;

             hl1:=0;
             hl2:=0;
             if (p.nodetype=rangen) then
               begin
                  { type checking for case statements }
                  if is_subequal(casedef, trangenode(p).left.resulttype.def) and
                     is_subequal(casedef, trangenode(p).right.resulttype.def) then
                    begin
                      hl1:=get_ordinal_value(trangenode(p).left);
                      hl2:=get_ordinal_value(trangenode(p).right);
                      if hl1>hl2 then
                        CGMessage(parser_e_case_lower_less_than_upper_bound);
                      if not casedeferror then
                       begin
                         testrange(casedef,hl1,false);
                         testrange(casedef,hl2,false);
                       end;
                    end
                  else
                    CGMessage(parser_e_case_mismatch);
                  newcaselabel(hl1,hl2,firstlabel);
               end
             else
               begin
                  { type checking for case statements }
                  if not is_subequal(casedef, p.resulttype.def) then
                    CGMessage(parser_e_case_mismatch);
                  hl1:=get_ordinal_value(p);
                  if not casedeferror then
                    testrange(casedef,hl1,false);
                  newcaselabel(hl1,hl1,firstlabel);
               end;
             p.free;
             if token=_COMMA then
               consume(_COMMA)
             else
               break;
             firstlabel:=false;
           until false;
           consume(_COLON);

           { handles instruction block }
           p:=clabelnode.createcase(aktcaselabel,statement);

           { concats instruction }
           instruc:=cstatementnode.create(instruc,p);

           if not((token=_ELSE) or (token=_OTHERWISE) or (token=_END)) then
             consume(_SEMICOLON);
         until (token=_ELSE) or (token=_OTHERWISE) or (token=_END);

         if (token=_ELSE) or (token=_OTHERWISE) then
           begin
              if not try_to_consume(_ELSE) then
                consume(_OTHERWISE);
              elseblock:=statements_til_end;
           end
         else
           begin
              elseblock:=nil;
              consume(_END);
           end;
         dec(statement_level);

         code:=ccasenode.create(caseexpr,instruc,root);

         tcasenode(code).elseblock:=elseblock;

         case_statement:=code;
      end;


    function repeat_statement : tnode;

      var
         first,last,p_e : tnode;

      begin
         consume(_REPEAT);
         first:=nil;
         inc(statement_level);

         while token<>_UNTIL do
           begin
              if first=nil then
                begin
                   last:=cstatementnode.create(nil,statement);
                   first:=last;
                end
              else
                begin
                   tstatementnode(last).left:=cstatementnode.create(nil,statement);
                   last:=tstatementnode(last).left;
                end;
              if not try_to_consume(_SEMICOLON) then
                break;
              consume_emptystats;
           end;
         consume(_UNTIL);
         dec(statement_level);

         first:=cblocknode.create(first);
         p_e:=comp_expr(true);
         repeat_statement:=genloopnode(whilerepeatn,p_e,first,nil,true);
      end;


    function while_statement : tnode;

      var
         p_e,p_a : tnode;

      begin
         consume(_WHILE);
         p_e:=comp_expr(true);
         consume(_DO);
         p_a:=statement;
         while_statement:=genloopnode(whilerepeatn,p_e,p_a,nil,false);
      end;


    function for_statement : tnode;

      var
         p_e,tovalue,p_a : tnode;
         backward : boolean;

      begin
         { parse loop header }
         consume(_FOR);
         p_e:=expr;
         if token=_DOWNTO then
           begin
              consume(_DOWNTO);
              backward:=true;
           end
         else
           begin
              consume(_TO);
              backward:=false;
           end;
         tovalue:=comp_expr(true);
         consume(_DO);

         { ... now the instruction }
         p_a:=statement;
         for_statement:=genloopnode(forn,p_e,tovalue,p_a,backward);
      end;


    function _with_statement : tnode;

      var
         right,p : tnode;
         i,levelcount : longint;
         withsymtable,symtab : tsymtable;
         obj : tobjectdef;
         hp : tnode;
      begin
         p:=comp_expr(true);
         do_resulttypepass(p);
         set_varstate(p,false);
         right:=nil;
         if (not codegenerror) and
            (p.resulttype.def.deftype in [objectdef,recorddef]) then
          begin
            case p.resulttype.def.deftype of
             objectdef : begin
                           obj:=tobjectdef(p.resulttype.def);
                           symtab:=twithsymtable.Create(obj,obj.symtable.symsearch);
                           withsymtable:=symtab;
                           if (p.nodetype=loadn) and
                              (tloadnode(p).symtable=aktprocdef.localst) then
                             twithsymtable(symtab).direct_with:=true;
                           twithsymtable(symtab).withrefnode:=p;
                           levelcount:=1;
                           obj:=obj.childof;
                           while assigned(obj) do
                            begin
                              symtab.next:=twithsymtable.create(obj,obj.symtable.symsearch);
                              symtab:=symtab.next;
                              if (p.nodetype=loadn) and
                                 (tloadnode(p).symtable=aktprocdef.localst) then
                                twithsymtable(symtab).direct_with:=true;
                              twithsymtable(symtab).withrefnode:=p;
                              obj:=obj.childof;
                              inc(levelcount);
                            end;
                           symtab.next:=symtablestack;
                           symtablestack:=withsymtable;
                         end;
             recorddef : begin
                           symtab:=trecorddef(p.resulttype.def).symtable;
                           levelcount:=1;
                           withsymtable:=twithsymtable.create(trecorddef(p.resulttype.def),symtab.symsearch);
                           if (p.nodetype=loadn) and
                              (tloadnode(p).symtable=aktprocdef.localst) then
                           twithsymtable(withsymtable).direct_with:=true;
                           twithsymtable(withsymtable).withrefnode:=p;
                           withsymtable.next:=symtablestack;
                           symtablestack:=withsymtable;
                        end;
            end;
            if token=_COMMA then
             begin
               consume(_COMMA);
               right:=_with_statement{$ifdef FPCPROCVAR}(){$endif};
             end
            else
             begin
               consume(_DO);
               if token<>_SEMICOLON then
                right:=statement
               else
                right:=cerrornode.create;
             end;
            for i:=1 to levelcount do
             symtablestack:=symtablestack.next;
            _with_statement:=cwithnode.create(twithsymtable(withsymtable),p,right,levelcount);
          end
         else
          begin
            Message(parser_e_false_with_expr);
            { try to recover from error }
            if token=_COMMA then
             begin
               consume(_COMMA);
               hp:=_with_statement{$ifdef FPCPROCVAR}(){$endif};
               if (hp=nil) then; { remove warning about unused }
             end
            else
             begin
               consume(_DO);
               { ignore all }
               if token<>_SEMICOLON then
                statement;
             end;
            _with_statement:=nil;
          end;
      end;


    function with_statement : tnode;
      begin
         consume(_WITH);
         with_statement:=_with_statement;
      end;


    function raise_statement : tnode;

      var
         p,pobj,paddr,pframe : tnode;

      begin
         pobj:=nil;
         paddr:=nil;
         pframe:=nil;
         consume(_RAISE);
         if not(token in [_SEMICOLON,_END]) then
           begin
              { object }
              pobj:=comp_expr(true);
              if try_to_consume(_AT) then
                begin
                   paddr:=comp_expr(true);
                   if try_to_consume(_COMMA) then
                     pframe:=comp_expr(true);
                end;
           end
         else
           begin
              if (block_type<>bt_except) then
                Message(parser_e_no_reraise_possible);
           end;
         p:=craisenode.create(pobj,paddr,pframe);
         raise_statement:=p;
      end;


    function try_statement : tnode;

      var
         p_try_block,p_finally_block,first,last,
         p_default,p_specific,hp : tnode;
         ot : ttype;
         sym : tvarsym;
         old_block_type : tblock_type;
         exceptsymtable : tsymtable;
         objname,objrealname : stringid;
         srsym : tsym;
         srsymtable : tsymtable;
         oldaktexceptblock: integer;

      begin
         procinfo^.flags:=procinfo^.flags or pi_uses_exceptions;

         p_default:=nil;
         p_specific:=nil;

         { read statements to try }
         consume(_TRY);
         first:=nil;
         inc(exceptblockcounter);
         oldaktexceptblock := aktexceptblock;
         aktexceptblock := exceptblockcounter;
         inc(statement_level);

         while (token<>_FINALLY) and (token<>_EXCEPT) do
           begin
              if first=nil then
                begin
                   last:=cstatementnode.create(nil,statement);
                   first:=last;
                end
              else
                begin
                   tstatementnode(last).left:=cstatementnode.create(nil,statement);
                   last:=tstatementnode(last).left;
                end;
              if not try_to_consume(_SEMICOLON) then
                break;
              consume_emptystats;
           end;
         p_try_block:=cblocknode.create(first);

         if try_to_consume(_FINALLY) then
           begin
              inc(exceptblockcounter);
              aktexceptblock := exceptblockcounter;
              p_finally_block:=statements_til_end;
              try_statement:=ctryfinallynode.create(p_try_block,p_finally_block);
              dec(statement_level);
           end
         else
           begin
              consume(_EXCEPT);
              old_block_type:=block_type;
              block_type:=bt_except;
              inc(exceptblockcounter);
              aktexceptblock := exceptblockcounter;
              ot:=generrortype;
              p_specific:=nil;
              if (idtoken=_ON) then
                { catch specific exceptions }
                begin
                   repeat
                     consume(_ID);
                     if token=_ID then
                       begin
                          objname:=pattern;
                          objrealname:=orgpattern;
                          { can't use consume_sym here, because we need already
                            to check for the colon }
                          searchsym(objname,srsym,srsymtable);
                          consume(_ID);
                          { is a explicit name for the exception given ? }
                          if try_to_consume(_COLON) then
                            begin
                               consume_sym(srsym,srsymtable);
                               if (srsym.typ=typesym) and
                                  is_class(ttypesym(srsym).restype.def) then
                                 begin
                                    ot:=ttypesym(srsym).restype;
                                    sym:=tvarsym.create(objrealname,ot);
                                 end
                               else
                                 begin
                                    sym:=tvarsym.create(objrealname,generrortype);
                                    if (srsym.typ=typesym) then
                                      Message1(type_e_class_type_expected,ttypesym(srsym).restype.def.typename)
                                    else
                                      Message1(type_e_class_type_expected,ot.def.typename);
                                 end;
                               exceptsymtable:=tstt_exceptsymtable.create;
                               exceptsymtable.insert(sym);
                               { insert the exception symtable stack }
                               exceptsymtable.next:=symtablestack;
                               symtablestack:=exceptsymtable;
                            end
                          else
                            begin
                               { check if type is valid, must be done here because
                                 with "e: Exception" the e is not necessary }
                               if srsym=nil then
                                begin
                                  identifier_not_found(objrealname);
                                  srsym:=generrorsym;
                                end;
                               { support unit.identifier }
                               if srsym.typ=unitsym then
                                 begin
                                    consume(_POINT);
                                    srsym:=searchsymonlyin(tunitsym(srsym).unitsymtable,pattern);
                                    if srsym=nil then
                                     begin
                                       identifier_not_found(orgpattern);
                                       srsym:=generrorsym;
                                     end;
                                    consume(_ID);
                                 end;
                               { check if type is valid, must be done here because
                                 with "e: Exception" the e is not necessary }
                               if (srsym.typ=typesym) and
                                  is_class(ttypesym(srsym).restype.def) then
                                 ot:=ttypesym(srsym).restype
                               else
                                 begin
                                    ot:=generrortype;
                                    if (srsym.typ=typesym) then
                                      Message1(type_e_class_type_expected,ttypesym(srsym).restype.def.typename)
                                    else
                                      Message1(type_e_class_type_expected,ot.def.typename);
                                 end;
                               exceptsymtable:=nil;
                            end;
                       end
                     else
                       consume(_ID);
                     consume(_DO);
                     hp:=connode.create(nil,statement);
                     if ot.def.deftype=errordef then
                       begin
                          hp.free;
                          hp:=cerrornode.create;
                       end;
                     if p_specific=nil then
                       begin
                          last:=hp;
                          p_specific:=last;
                       end
                     else
                       begin
                          tonnode(last).left:=hp;
                          last:=tonnode(last).left;
                       end;
                     { set the informations }
                     { only if the creation of the onnode was succesful, it's possible }
                     { that last and hp are errornodes (JM)                            }
                     if last.nodetype = onn then
                       begin
                         tonnode(last).excepttype:=tobjectdef(ot.def);
                         tonnode(last).exceptsymtable:=exceptsymtable;
                       end;
                     { remove exception symtable }
                     if assigned(exceptsymtable) then
                       begin
                         dellexlevel;
                         if last.nodetype <> onn then
                           exceptsymtable.free;
                       end;
                     if not try_to_consume(_SEMICOLON) then
                        break;
                     consume_emptystats;
                   until (token=_END) or (token=_ELSE);
                   if token=_ELSE then
                     { catch the other exceptions }
                     begin
                        consume(_ELSE);
                        p_default:=statements_til_end;
                     end
                   else
                     consume(_END);
                end
              else
                { catch all exceptions }
                begin
                   p_default:=statements_til_end;
                end;
              dec(statement_level);

              block_type:=old_block_type;
              try_statement:=ctryexceptnode.create(p_try_block,p_specific,p_default);
           end;
         aktexceptblock := oldaktexceptblock;
      end;


    function _asm_statement : tnode;
      var
        asmstat : tasmnode;
        Marker : tai;
        r : tregister;
        found : boolean;
      begin
         Inside_asm_statement:=true;
         case aktasmmode of
           asmmode_none : ; { just be there to allow to a compile without
                              any assembler readers }
{$ifdef i386}
  {$ifndef NoRA386Att}
           asmmode_i386_att:
             asmstat:=tasmnode(ra386att.assemble);
  {$endif NoRA386Att}
  {$ifndef NoRA386Int}
           asmmode_i386_intel:
             asmstat:=tasmnode(ra386int.assemble);
  {$endif NoRA386Int}
  {$ifndef NoRA386Dir}
           asmmode_i386_direct:
             begin
               if not target_asm.allowdirect then
                 Message(parser_f_direct_assembler_not_allowed);
               if (aktprocdef.proccalloption=pocall_inline) then
                 Begin
                    Message1(parser_w_not_supported_for_inline,'direct asm');
                    Message(parser_w_inlining_disabled);
                    aktprocdef.proccalloption:=pocall_fpccall;
                 End;
               asmstat:=tasmnode(ra386dir.assemble);
             end;
  {$endif NoRA386Dir}
{$endif}

{$ifdef x86_64}
  {$ifndef NoRA386Dir}
           asmmode_i386_direct:
             begin
               if not target_asm.allowdirect then
                 Message(parser_f_direct_assembler_not_allowed);
               if (aktprocdef.proccalloption=pocall_inline) then
                 Begin
                    Message1(parser_w_not_supported_for_inline,'direct asm');
                    Message(parser_w_inlining_disabled);
                    aktprocdef.proccalloption:=pocall_fpccall;
                 End;
               asmstat:=tasmnode(rax86dir.assemble);
             end;
  {$endif NoRA386Dir}
{$endif x86_64}

{$ifdef powerpc}
  {$ifndef NoRAPPCDir}
           asmmode_ppc_direct:
             begin
               if not target_asm.allowdirect then
                 Message(parser_f_direct_assembler_not_allowed);
               if (aktprocdef.proccalloption=pocall_inline) then
                 Begin
                    Message1(parser_w_not_supported_for_inline,'direct asm');
                    Message(parser_w_inlining_disabled);
                    aktprocdef.proccalloption:=pocall_fpccall;
                 End;
               asmstat:=tasmnode(rappcdir.assemble);
             end;
  {$endif NoRAPPCDir}
{$endif powerpc}

{$ifdef m68k}
  {$ifndef NoRA68kMot}
           asmmode_m68k_mot:
             asmstat:=tasmnode(ra68kmot.assemble);
  {$endif NoRA68kMot}
{$endif}
         else
           Message(parser_f_assembler_reader_not_supported);
         end;

         { Read first the _ASM statement }
         consume(_ASM);

         { END is read }
         if try_to_consume(_LECKKLAMMER) then
           begin
              { it's possible to specify the modified registers }
              include(asmstat.flags,nf_object_preserved);
              if token<>_RECKKLAMMER then
                repeat
                { uppercase, because it's a CSTRING }
                  uppervar(pattern);
{$ifdef i386}
                  if pattern='EAX' then
                    include(rg.usedinproc,R_EAX)
                  else if pattern='EBX' then
                    include(rg.usedinproc,R_EBX)
                  else if pattern='ECX' then
                    include(rg.usedinproc,R_ECX)
                  else if pattern='EDX' then
                    include(rg.usedinproc,R_EDX)
                  else if pattern='ESI' then
                    begin
                       include(rg.usedinproc,R_ESI);
                       exclude(asmstat.flags,nf_object_preserved);
                    end
                  else if pattern='EDI' then
                    include(rg.usedinproc,R_EDI)
                  else consume(_RECKKLAMMER);
{$endif i386}
{$ifdef x86_64}
                  if pattern='RAX' then
                    include(usedinproc,R_RAX)
                  else if pattern='RBX' then
                    include(usedinproc,R_RBX)
                  else if pattern='RCX' then
                    include(usedinproc,R_RCX)
                  else if pattern='RDX' then
                    include(usedinproc,R_RDX)
                  else if pattern='RSI' then
                    begin
                       include(usedinproc,R_RSI);
                       exclude(asmstat.flags,nf_object_preserved);
                    end
                  else if pattern='RDI' then
                    include(usedinproc,R_RDI)
                  else consume(_RECKKLAMMER);
{$endif x86_64}
{$ifdef m68k}
                  if pattern='D0' then
                    include(rg.usedinproc,R_D0)
                  else if pattern='D1' then
                    include(rg.usedinproc,R_D1)
                  else if pattern='D2' then
                    include(rg.usedinproc,R_D2)
                  else if pattern='D3' then
                    include(rg.usedinproc,R_D3)
                  else if pattern='D4' then
                    include(rg.usedinproc,R_D4)
                  else if pattern='D5' then
                    include(rg.usedinproc,R_D5)
                  else if pattern='D6' then
                    include(rg.usedinproc,R_D6)
                  else if pattern='D7' then
                    include(rg.usedinproc,R_D7)
                  else if pattern='A0' then
                    include(rg.usedinproc,R_A0)
                  else if pattern='A1' then
                    include(rg.usedinproc,R_A1)
                  else if pattern='A2' then
                    include(rg.usedinproc,R_A2)
                  else if pattern='A3' then
                    include(rg.usedinproc,R_A3)
                  else if pattern='A4' then
                    include(rg.usedinproc,R_A4)
                  else if pattern='A5' then
                    include(rg.usedinproc,R_A5)
                  else consume(_RECKKLAMMER);
{$endif m68k}
{$ifdef powerpc}
                  found:=false;
                  for r:=low(tregister) to high(tregister) do
                    if pattern=upper(std_reg2str[r]) then
                      begin
                         include(rg.usedinproc,r);
                         include(rg.usedbyproc,r);
                         found:=true;
                         break;
                      end;
                  if not(found) then
                    consume(_RECKKLAMMER);
{$endif powerpc}
{$IFDEF SPARC}
                  if pattern<>'' then
                    internalerror(200108251)
                  else consume(_RECKKLAMMER);
{$ENDIF SPARC}
                  consume(_CSTRING);
                  if not try_to_consume(_COMMA) then
                    break;
                until false;
              consume(_RECKKLAMMER);
           end
         else
           begin
              rg.usedbyproc := ALL_REGISTERS;
              rg.usedinproc := ALL_REGISTERS;
           end;

         { mark the start and the end of the assembler block
           this is needed for the optimizer }
         If Assigned(AsmStat.p_asm) Then
           Begin
             Marker := Tai_Marker.Create(AsmBlockStart);
             AsmStat.p_asm.Insert(Marker);
             Marker := Tai_Marker.Create(AsmBlockEnd);
             AsmStat.p_asm.Concat(Marker);
           End;
         Inside_asm_statement:=false;
         _asm_statement:=asmstat;
      end;


    function statement : tnode;
      var
         p       : tnode;
         code    : tnode;
         filepos : tfileposinfo;
         srsym   : tsym;
         srsymtable : tsymtable;
         s       : stringid;
      begin
         filepos:=akttokenpos;
         case token of
           _GOTO :
             begin
                if not(cs_support_goto in aktmoduleswitches)then
                 Message(sym_e_goto_and_label_not_supported);
                consume(_GOTO);
                if (token<>_INTCONST) and (token<>_ID) then
                  begin
                     Message(sym_e_label_not_found);
                     code:=cerrornode.create;
                  end
                else
                  begin
                     if token=_ID then
                      consume_sym(srsym,srsymtable)
                     else
                      begin
                        searchsym(pattern,srsym,srsymtable);
                        if srsym=nil then
                         begin
                           identifier_not_found(pattern);
                           srsym:=generrorsym;
                           srsymtable:=nil;
                         end;
                        consume(token);
                      end;

                     if srsym.typ<>labelsym then
                       begin
                          Message(sym_e_id_is_no_label_id);
                          code:=cerrornode.create;
                       end
                     else
                       begin
                         code:=cgotonode.create(tlabelsym(srsym));
                         tgotonode(code).labsym:=tlabelsym(srsym);
                         { set flag that this label is used }
                         tlabelsym(srsym).used:=true;
                       end;
                  end;
             end;
           _BEGIN :
             code:=statement_block(_BEGIN);
           _IF :
             code:=if_statement;
           _CASE :
             code:=case_statement;
           _REPEAT :
             code:=repeat_statement;
           _WHILE :
             code:=while_statement;
           _FOR :
             code:=for_statement;
           _WITH :
             code:=with_statement;
           _TRY :
             code:=try_statement;
           _RAISE :
             code:=raise_statement;
           { semicolons,else until and end are ignored }
           _SEMICOLON,
           _ELSE,
           _UNTIL,
           _END:
             code:=cnothingnode.create;
           _FAIL :
             begin
                if (aktprocdef.proctypeoption<>potype_constructor) then
                  Message(parser_e_fail_only_in_constructor);
                consume(_FAIL);
                code:=cfailnode.create;
             end;
           _ASM :
             code:=_asm_statement;
           _EOF :
             Message(scan_f_end_of_file);
         else
           begin
             p:=expr;

             { When a colon follows a intconst then transform it into a label }
             if try_to_consume(_COLON) then
              begin
                s:=tostr(tordconstnode(p).value);
                p.free;
                searchsym(s,srsym,srsymtable);
                if assigned(srsym) then
                 begin
                   if tlabelsym(srsym).defined then
                    Message(sym_e_label_already_defined);
                   tlabelsym(srsym).defined:=true;
                   p:=clabelnode.create(tlabelsym(srsym),nil);
                 end
                else
                 begin
                   identifier_not_found(s);
                   p:=cnothingnode.create;
                 end;
              end;

             if p.nodetype=labeln then
              begin
                { the pointer to the following instruction }
                { isn't a very clean way                   }
                tlabelnode(p).left:=statement{$ifdef FPCPROCVAR}(){$endif};
                { be sure to have left also resulttypepass }
                resulttypepass(tlabelnode(p).left);
              end;

             { blockn support because a read/write is changed into a blocknode }
             { with a separate statement for each read/write operation (JM)    }
             { the same is true for val() if the third parameter is not 32 bit }
             if not(p.nodetype in [nothingn,calln,assignn,breakn,inlinen,
                                   continuen,labeln,blockn,exitn]) then
               Message(cg_e_illegal_expression);

             { specify that we don't use the value returned by the call }
             { Question : can this be also improtant
               for inlinen ??
               it is used for :
                - dispose of temp stack space
                - dispose on FPU stack }
             if p.nodetype=calln then
               exclude(p.flags,nf_return_value_used);

             code:=p;
           end;
         end;
         if assigned(code) then
          code.set_tree_filepos(filepos);
         statement:=code;
      end;


    function statement_block(starttoken : ttoken) : tnode;

      var
         first,last : tnode;
         filepos : tfileposinfo;

      begin
         first:=nil;
         filepos:=akttokenpos;
         consume(starttoken);
         inc(statement_level);

         while not(token in [_END,_FINALIZATION]) do
           begin
              if first=nil then
                begin
                   last:=cstatementnode.create(nil,statement);
                   first:=last;
                end
              else
                begin
                   tstatementnode(last).left:=cstatementnode.create(nil,statement);
                   last:=tstatementnode(last).left;
                end;
              if (token in [_END,_FINALIZATION]) then
                break
              else
                begin
                   { if no semicolon, then error and go on }
                   if token<>_SEMICOLON then
                     begin
                        consume(_SEMICOLON);
                        consume_all_until(_SEMICOLON);
                     end;
                   consume(_SEMICOLON);
                end;
              consume_emptystats;
           end;

         { don't consume the finalization token, it is consumed when
           reading the finalization block, but allow it only after
           an initalization ! }
         if (starttoken<>_INITIALIZATION) or (token<>_FINALIZATION) then
           consume(_END);

         dec(statement_level);

         last:=cblocknode.create(first);
         last.set_tree_filepos(filepos);
         statement_block:=last;
      end;


    function assembler_block : tnode;

      {# Optimize the assembler block by removing all references
         which are via the frame pointer by replacing them with
         references via the stack pointer.

         This is only available to certain cpu targets where
         the frame pointer saving must be done explicitly.
      }
      procedure OptimizeFramePointer(p:tasmnode);
      var
        hp : tai;
        parafixup,
        i : longint;
      begin
        { replace framepointer with stackpointer }
        procinfo^.framepointer:=STACK_POINTER_REG;
        { set the right value for parameters }
        dec(aktprocdef.parast.address_fixup,pointer_size);
        dec(procinfo^.para_offset,pointer_size);
        { replace all references to parameters in the instructions,
          the parameters can be identified by the parafixup option
          that is set. For normal user coded [ebp+4] this field is not
          set }
        parafixup:=aktprocdef.parast.address_fixup;
        hp:=tai(p.p_asm.first);
        while assigned(hp) do
         begin
           if hp.typ=ait_instruction then
            begin
              { fixup the references }
              for i:=1 to taicpu(hp).ops do
               begin
                 with taicpu(hp).oper[i-1] do
                  if typ=top_ref then
                   begin
                     case ref^.options of
                       ref_parafixup :
                         begin
                           ref^.offsetfixup:=parafixup;
                           ref^.base:=STACK_POINTER_REG;
                         end;
                     end;
                   end;
               end;
            end;
           hp:=tai(hp.next);
         end;
      end;

{$ifdef CHECKFORPUSH}
      function UsesPush(p:tasmnode):boolean;
      var
        hp : tai;
      begin
        hp:=tai(p.p_asm.first);
        while assigned(hp) do
         begin
           if (hp.typ=ait_instruction) and
              (taicpu(hp).opcode=A_PUSH) then
            begin
              UsesPush:=true;
              exit;
            end;
           hp:=tai(hp.next);
         end;
        UsesPush:=false;
      end;
{$endif CHECKFORPUSH}

      var
        p : tnode;
        haslocals,hasparas : boolean;
      begin
         { retrieve info about locals and paras before a result
           is inserted in the symtable }
         haslocals:=(aktprocdef.localst.datasize>0);
         hasparas:=(aktprocdef.parast.datasize>0);

         { temporary space is set, while the BEGIN of the procedure }
         if symtablestack.symtabletype=localsymtable then
           procinfo^.firsttemp_offset := -symtablestack.datasize
         else
           procinfo^.firsttemp_offset := 0;

         { assembler code does not allocate }
         { space for the return value       }
         if not is_void(aktprocdef.rettype.def) then
           begin
              aktprocdef.funcretsym:=tfuncretsym.create(aktprocsym.name,aktprocdef.rettype);
              { insert in local symtable }
              { but with another name, so that recursive calls are possible }
              symtablestack.insert(aktprocdef.funcretsym);
              symtablestack.rename(aktprocdef.funcretsym.name,'$result');
              { update the symtablesize back to 0 if there were no locals }
              if not haslocals then
               symtablestack.datasize:=0;
              { set the used flag for the return }
              if paramanager.ret_in_acc(aktprocdef.rettype.def) then
                 include(rg.usedinproc,accumulator);
            end;
         { force the asm statement }
         if token<>_ASM then
           consume(_ASM);
         procinfo^.Flags := procinfo^.Flags Or pi_is_assembler;
         p:=_asm_statement;


         { set the framepointer to esp for assembler functions when the
           following conditions are met:
           - if the are no local variables
           - no reference to the result variable (refcount<=1)
           - result is not stored as parameter
           - target processor has optional frame pointer save
             (vm, i386, vm only currently)
         }
         if (po_assembler in aktprocdef.procoptions) and
            (not haslocals) and
            (not hasparas) and
            (aktprocdef.owner.symtabletype<>objectsymtable) and
            (not assigned(aktprocdef.funcretsym) or
             (tfuncretsym(aktprocdef.funcretsym).refcount<=1)) and
            not(paramanager.ret_in_param(aktprocdef.rettype.def)) and
            (target_cpu in [cpu_i386,cpu_m68k,cpu_vm])
{$ifdef CHECKFORPUSH}
            and not(UsesPush(tasmnode(p)))
{$endif CHECKFORPUSH}
            then
           OptimizeFramePointer(tasmnode(p));

        { Flag the result as assigned when it is returned in the
          accumulator or on the fpu stack }
        if assigned(aktprocdef.funcretsym) and
           (is_fpu(aktprocdef.rettype.def) or
           paramanager.ret_in_acc(aktprocdef.rettype.def)) then
          tfuncretsym(aktprocdef.funcretsym).funcretstate:=vs_assigned;

        { because the END is already read we need to get the
          last_endtoken_filepos here (PFV) }
        last_endtoken_filepos:=akttokenpos;

        assembler_block:=p;
      end;

end.
{
  $Log$
  Revision 1.66  2002-08-06 20:55:22  florian
    * first part of ppc calling conventions fix

  Revision 1.65  2002/07/28 20:45:22  florian
    + added direct assembler reader for PowerPC

  Revision 1.64  2002/07/20 11:57:56  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.63  2002/07/19 11:41:36  daniel
  * State tracker work
  * The whilen and repeatn are now completely unified into whilerepeatn. This
    allows the state tracker to change while nodes automatically into
    repeat nodes.
  * Resulttypepass improvements to the notn. 'not not a' is optimized away and
    'not(a>b)' is optimized into 'a<=b'.
  * Resulttypepass improvements to the whilerepeatn. 'while not a' is optimized
    by removing the notn and later switchting the true and falselabels. The
    same is done with 'repeat until not a'.

  Revision 1.62  2002/07/16 15:34:20  florian
    * exit is now a syssym instead of a keyword

  Revision 1.61  2002/07/11 14:41:28  florian
    * start of the new generic parameter handling

  Revision 1.60  2002/07/04 20:43:01  florian
    * first x86-64 patches

  Revision 1.59  2002/07/01 18:46:25  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.58  2002/05/18 13:34:13  peter
    * readded missing revisions

  Revision 1.57  2002/05/16 19:46:44  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.55  2002/05/06 19:56:42  carl
  + added more patches from Mazen for SPARC port

  Revision 1.54  2002/04/21 19:02:05  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.53  2002/04/20 21:32:24  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

  Revision 1.52  2002/04/16 16:11:17  peter
    * using inherited; without a parent having the same function
      will do nothing like delphi

  Revision 1.51  2002/04/15 19:01:28  carl
  + target_info.size_of_pointer -> pointer_Size

  Revision 1.50  2002/04/14 16:53:54  carl
  + asm statement uses ALL_REGISTERS

  Revision 1.49  2002/03/31 20:26:36  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

  Revision 1.48  2002/03/11 19:10:28  peter
    * Regenerated with updated fpcmake

  Revision 1.47  2002/03/04 17:54:59  peter
    * allow oridinal labels again

  Revision 1.46  2002/01/29 21:32:03  peter
    * allow accessing locals in other lexlevel when the current assembler
      routine doesn't have locals.

  Revision 1.45  2002/01/24 18:25:49  peter
   * implicit result variable generation for assembler routines
   * removed m_tp modeswitch, use m_tp7 or not(m_fpc) instead
}
