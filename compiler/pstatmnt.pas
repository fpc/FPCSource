{
    $Id$
    Copyright (c) 1998 by Florian Klaempfl

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

  interface

    uses tree;

    var
       { true, if we are in a except block }
       in_except_block : boolean;

    { reads a block }
    function block(islibrary : boolean) : ptree;

    { reads an assembler block }
    function assembler_block : ptree;

  implementation

    uses
       globtype,systems,tokens,
       strings,cobjects,globals,files,verbose,
       symconst,symtable,aasm,pass_1,types,scanner,hcodegen,ppu
       ,pbase,pexpr,pdecl,cpubase,cpuasm
{$ifdef i386}
       ,tgeni386
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
{$ifdef m68k}
       ,m68k,tgen68k
  {$ifndef NoRa68kMot}
       ,ra68kmot
  {$endif NoRa68kMot}
{$endif m68k}
{$ifdef alpha}
       ,tgeni386  { this is a dummy!! }
{$endif alpha}
{$ifdef powerpc}
       ,tgeni386  { this is a dummy!! }
{$endif powerpc}
       ;


    const
      statement_level : longint = 0;

    function statement : ptree;forward;


    function if_statement : ptree;
      var
         ex,if_a,else_a : ptree;
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
    function statements_til_end : ptree;

      var
         first,last : ptree;

      begin
         first:=nil;
         while token<>_END do
           begin
              if first=nil then
                begin
                   last:=gennode(statementn,nil,statement);
                   first:=last;
                end
              else
                begin
                   last^.left:=gennode(statementn,nil,statement);
                   last:=last^.left;
                end;
              if not try_to_consume(_SEMICOLON) then
                break;
              emptystats;
           end;
         consume(_END);
         statements_til_end:=gensinglenode(blockn,first);
      end;

    function case_statement : ptree;

      var
         { contains the label number of currently parsed case block }
         aktcaselabel : pasmlabel;
         firstlabel : boolean;
         root : pcaserecord;

         { the typ of the case expression }
         casedef : pdef;

      procedure newcaselabel(l,h : longint;first:boolean);

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
                      freelabel(hcaselabel^._at);
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
                        freelabel(hcaselabel^._at);
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
         code,caseexpr,p,instruc,elseblock : ptree;
         hl1,hl2 : longint;
         casedeferror : boolean;
      begin
         consume(_CASE);
         caseexpr:=comp_expr(true);
       { determines result type }
         cleartempgen;
         do_firstpass(caseexpr);
         casedeferror:=false;
         casedef:=caseexpr^.resulttype;
         if not(is_ordinal(casedef) or is_64bitint(casedef)) then
          begin
            Message(type_e_ordinal_expr_expected);
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

           { may be an instruction has more case labels }
           repeat
             p:=expr;
             cleartempgen;
             do_firstpass(p);
             hl1:=0;
             hl2:=0;
             if (p^.treetype=rangen) then
               begin
                  { type checking for case statements }
                  if is_subequal(casedef, p^.left^.resulttype) and
                     is_subequal(casedef, p^.right^.resulttype) then
                    begin
                      hl1:=get_ordinal_value(p^.left);
                      hl2:=get_ordinal_value(p^.right);
                      if hl1>hl2 then
                        Message(parser_e_case_lower_less_than_upper_bound);
                      if not casedeferror then
                       begin
                         testrange(casedef,hl1);
                         testrange(casedef,hl2);
                       end;
                    end
                  else
                    Message(parser_e_case_mismatch);
                  newcaselabel(hl1,hl2,firstlabel);
               end
             else
               begin
                  { type checking for case statements }
                  if not is_subequal(casedef, p^.resulttype) then
                    Message(parser_e_case_mismatch);
                  hl1:=get_ordinal_value(p);
                  if not casedeferror then
                    testrange(casedef,hl1);
                  newcaselabel(hl1,hl1,firstlabel);
               end;
             disposetree(p);
             if token=_COMMA then
               consume(_COMMA)
             else
               break;
             firstlabel:=false;
           until false;
           consume(_COLON);

           { handles instruction block }
           p:=gensinglenode(labeln,statement);
           p^.labelnr:=aktcaselabel;

           { concats instruction }
           instruc:=gennode(statementn,instruc,p);

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

         code:=gencasenode(caseexpr,instruc,root);

         code^.elseblock:=elseblock;

         case_statement:=code;
      end;


    function repeat_statement : ptree;

      var
         first,last,p_e : ptree;

      begin
         consume(_REPEAT);
         first:=nil;
         inc(statement_level);

         while token<>_UNTIL do
           begin
              if first=nil then
                begin
                   last:=gennode(statementn,nil,statement);
                   first:=last;
                end
              else
                begin
                   last^.left:=gennode(statementn,nil,statement);
                   last:=last^.left;
                end;
              if not try_to_consume(_SEMICOLON) then
                break;
              emptystats;
           end;
         consume(_UNTIL);
         dec(statement_level);

         first:=gensinglenode(blockn,first);
         p_e:=comp_expr(true);
         repeat_statement:=genloopnode(repeatn,p_e,first,nil,false);
      end;


    function while_statement : ptree;

      var
         p_e,p_a : ptree;

      begin
         consume(_WHILE);
         p_e:=comp_expr(true);
         consume(_DO);
         p_a:=statement;
         while_statement:=genloopnode(whilen,p_e,p_a,nil,false);
      end;


    function for_statement : ptree;

      var
         p_e,tovalue,p_a : ptree;
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


    function _with_statement : ptree;

      var
         right,hp,p : ptree;
         i,levelcount : longint;
         withsymtable,symtab : psymtable;
         obj : pobjectdef;

      begin
         Must_be_valid:=false;
         p:=comp_expr(true);
         do_firstpass(p);
         right:=nil;
         if (not codegenerror) and
            (p^.resulttype^.deftype in [objectdef,recorddef]) then
          begin
            case p^.resulttype^.deftype of
             objectdef : begin
                           obj:=pobjectdef(p^.resulttype);
                           withsymtable:=new(pwithsymtable,init);
                           withsymtable^.symsearch:=obj^.symtable^.symsearch;
                           withsymtable^.defowner:=obj;
                           symtab:=withsymtable;
                           if (p^.treetype=loadn) and
                              (p^.symtable=aktprocsym^.definition^.localst) then
                             pwithsymtable(symtab)^.direct_with:=true;
                           {symtab^.withnode:=p; not yet allocated !! }
                           pwithsymtable(symtab)^.withrefnode:=p;
                           levelcount:=1;
                           obj:=obj^.childof;
                           while assigned(obj) do
                            begin
                              symtab^.next:=new(pwithsymtable,init);
                              symtab:=symtab^.next;
                              symtab^.symsearch:=obj^.symtable^.symsearch;
                              if (p^.treetype=loadn) and
                                 (p^.symtable=aktprocsym^.definition^.localst) then
                                pwithsymtable(symtab)^.direct_with:=true;
                              {symtab^.withnode:=p; not yet allocated !! }
                              pwithsymtable(symtab)^.withrefnode:=p;
                              symtab^.defowner:=obj;
                              obj:=obj^.childof;
                              inc(levelcount);
                            end;
                           symtab^.next:=symtablestack;
                           symtablestack:=withsymtable;
                         end;
             recorddef : begin
                           symtab:=precorddef(p^.resulttype)^.symtable;
                           levelcount:=1;
                           withsymtable:=new(pwithsymtable,init);
                           withsymtable^.symsearch:=symtab^.symsearch;
                           withsymtable^.next:=symtablestack;
                              if (p^.treetype=loadn) and
                                 (p^.symtable=aktprocsym^.definition^.localst) then
                                pwithsymtable(withsymtable)^.direct_with:=true;
                              {symtab^.withnode:=p; not yet allocated !! }
                              pwithsymtable(withsymtable)^.withrefnode:=p;
                           withsymtable^.defowner:=obj;
                           symtablestack:=withsymtable;
                        end;
            end;
            if token=_COMMA then
             begin
               consume(_COMMA);
             {$ifdef tp}
               right:=_with_statement;
             {$else}
               right:=_with_statement();
             {$endif}
             end
            else
             begin
               consume(_DO);
               if token<>_SEMICOLON then
                right:=statement
               else
                right:=nil;
             end;
            for i:=1 to levelcount do
             symtablestack:=symtablestack^.next;
            _with_statement:=genwithnode(pwithsymtable(withsymtable),p,right,levelcount);
          end
         else
          begin
            Message(parser_e_false_with_expr);
            { try to recover from error }
            if token=_COMMA then
             begin
               consume(_COMMA);
             {$ifdef tp}
               hp:=_with_statement;
             {$else}
               hp:=_with_statement();
             {$endif}
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


    function with_statement : ptree;
      begin
         consume(_WITH);
         with_statement:=_with_statement;
      end;


    function raise_statement : ptree;

      var
         p1,p2 : ptree;

      begin
         p1:=nil;
         p2:=nil;
         consume(_RAISE);
         if token<>_SEMICOLON then
           begin
              p1:=comp_expr(true);
              if (idtoken=_AT) then
                begin
                   consume(_ID);
                   p2:=comp_expr(true);
                end;
           end
         else
           begin
              if not(in_except_block) then
               Message(parser_e_no_reraise_possible);
           end;
         raise_statement:=gennode(raisen,p1,p2);
      end;


    function try_statement : ptree;

      var
         p_try_block,p_finally_block,first,last,
         p_default,p_specific,hp : ptree;
         ot : pobjectdef;
         sym : pvarsym;
         old_in_except_block : boolean;
         exceptsymtable : psymtable;
         objname : stringid;

      begin
         procinfo.flags:=procinfo.flags or
           pi_uses_exceptions;

         p_default:=nil;
         p_specific:=nil;

         { read statements to try }
         consume(_TRY);
         first:=nil;
         inc(statement_level);

         while (token<>_FINALLY) and (token<>_EXCEPT) do
           begin
              if first=nil then
                begin
                   last:=gennode(statementn,nil,statement);
                   first:=last;
                end
              else
                begin
                   last^.left:=gennode(statementn,nil,statement);
                   last:=last^.left;
                end;
              if not try_to_consume(_SEMICOLON) then
                break;
              emptystats;
           end;
         p_try_block:=gensinglenode(blockn,first);

         if try_to_consume(_FINALLY) then
           begin
              p_finally_block:=statements_til_end;
              try_statement:=gennode(tryfinallyn,p_try_block,p_finally_block);
              dec(statement_level);

           end
         else
           begin
              consume(_EXCEPT);
              old_in_except_block:=in_except_block;
              in_except_block:=true;
              p_specific:=nil;
              if token=_ON then
                { catch specific exceptions }
                begin
                   repeat
                     consume(_ON);
                     if token=_ID then
                       begin
                          getsym(pattern,false);
                          objname:=pattern;
                          consume(_ID);
                          { is a explicit name for the exception given ? }
                          if try_to_consume(_COLON) then
                            begin
                               getsym(pattern,true);
                               consume(_ID);
                               if srsym^.typ=unitsym then
                                 begin
                                    consume(_POINT);
                                    getsymonlyin(punitsym(srsym)^.unitsymtable,pattern);
                                    consume(_ID);
                                 end;
                               if (srsym^.typ=typesym) and
                                 (ptypesym(srsym)^.definition^.deftype=objectdef) and
                                 pobjectdef(ptypesym(srsym)^.definition)^.is_class then
                                 ot:=pobjectdef(ptypesym(srsym)^.definition)
                               else
                                 begin
                                    ot:=pobjectdef(generrordef);
                                    if (srsym^.typ=typesym) then
                                      Message1(type_e_class_type_expected,ptypesym(srsym)^.definition^.typename)
                                    else
                                      Message1(type_e_class_type_expected,ot^.typename);
                                 end;
                               sym:=new(pvarsym,init(objname,ot));
                               exceptsymtable:=new(psymtable,init(stt_exceptsymtable));
                               exceptsymtable^.insert(sym);
                               { insert the exception symtable stack }
                               exceptsymtable^.next:=symtablestack;
                               symtablestack:=exceptsymtable;
                            end
                          else
                            begin
                               { only exception type }
                               if srsym^.typ=unitsym then
                                 begin
                                    consume(_POINT);
                                    getsymonlyin(punitsym(srsym)^.unitsymtable,pattern);
                                    consume(_ID);
                                 end;
                               if (srsym^.typ=typesym) and
                                 (ptypesym(srsym)^.definition^.deftype=objectdef) and
                                 pobjectdef(ptypesym(srsym)^.definition)^.is_class then
                                 ot:=pobjectdef(ptypesym(srsym)^.definition)
                               else
                                 begin
                                    ot:=pobjectdef(generrordef);
                                    if (srsym^.typ=typesym) then
                                      Message1(type_e_class_type_expected,ptypesym(srsym)^.definition^.typename)
                                    else
                                      Message1(type_e_class_type_expected,ot^.typename);
                                 end;
                               exceptsymtable:=nil;
                            end;
                       end
                     else
                       consume(_ID);
                     consume(_DO);
                     hp:=gennode(onn,nil,statement);
                     if ot^.deftype=errordef then
                       begin
                          disposetree(hp);
                          hp:=genzeronode(errorn);
                       end;
                     if p_specific=nil then
                       begin
                          last:=hp;
                          p_specific:=last;
                       end
                     else
                       begin
                          last^.left:=hp;
                          last:=last^.left;
                       end;
                     { set the informations }
                     last^.excepttype:=ot;
                     last^.exceptsymtable:=exceptsymtable;
                     last^.disposetyp:=dt_onn;
                     { remove exception symtable }
                     if assigned(exceptsymtable) then
                       dellexlevel;
                     if not try_to_consume(_SEMICOLON) then
                        break;
                     emptystats;
                   until (token=_END) or(token=_ELSE);
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

              in_except_block:=old_in_except_block;
              try_statement:=genloopnode(tryexceptn,p_try_block,p_specific,p_default,false);
           end;
      end;


    function exit_statement : ptree;

      var
         p : ptree;

      begin
         consume(_EXIT);
         if try_to_consume(_LKLAMMER) then
           begin
              p:=comp_expr(true);
              consume(_RKLAMMER);
              if in_except_block then
                Message(parser_e_exit_with_argument_not__possible);
              if procinfo.retdef=pdef(voiddef) then
                Message(parser_e_void_function)
              else
                procinfo.funcret_is_valid:=true;
           end
         else
           p:=nil;
         p:=gensinglenode(exitn,p);
         p^.resulttype:=procinfo.retdef;
         exit_statement:=p;
      end;


    function _asm_statement : ptree;
      var
        asmstat : ptree;
        Marker : Pai;
      begin
         Inside_asm_statement:=true;
         case aktasmmode of
           asmmode_none : ; { just be there to allow to a compile without
                              any assembler readers }
{$ifdef i386}
  {$ifndef NoRA386Att}
           asmmode_i386_att:
             asmstat:=ra386att.assemble;
  {$endif NoRA386Att}
  {$ifndef NoRA386Int}
           asmmode_i386_intel:
             asmstat:=ra386int.assemble;
  {$endif NoRA386Int}
  {$ifndef NoRA386Dir}
           asmmode_i386_direct:
             begin
               if not target_asm.allowdirect then
                 Message(parser_f_direct_assembler_not_allowed);
               if (pocall_inline in aktprocsym^.definition^.proccalloptions) then
                 Begin
                    Message1(parser_w_not_supported_for_inline,'direct asm');
                    Message(parser_w_inlining_disabled);
{$ifdef INCLUDEOK}
                    exclude(aktprocsym^.definition^.proccalloptions,pocall_inline);
{$else}
                    aktprocsym^.definition^.proccalloptions:=aktprocsym^.definition^.proccalloptions-[pocall_inline];
{$endif}
                 End;
               asmstat:=ra386dir.assemble;
             end;
  {$endif NoRA386Dir}
{$endif}
{$ifdef m68k}
  {$ifndef NoRA68kMot}
           asmmode_m68k_mot:
             asmstat:=ra68kmot.assemble;
  {$endif NoRA68kMot}
{$endif}
         else
           Message(parser_f_assembler_reader_not_supported);
         end;

         { Read first the _ASM statement }
         consume(_ASM);

{$ifndef newcg}
         { END is read }
         if try_to_consume(_LECKKLAMMER) then
           begin
              { it's possible to specify the modified registers }
              asmstat^.object_preserved:=true;
              if token<>_RECKKLAMMER then
                repeat
                { uppercase, because it's a CSTRING }
                  uppervar(pattern);
{$ifdef i386}
                  if pattern='EAX' then
                    usedinproc:=usedinproc or ($80 shr byte(R_EAX))
                  else if pattern='EBX' then
                    usedinproc:=usedinproc or ($80 shr byte(R_EBX))
                  else if pattern='ECX' then
                    usedinproc:=usedinproc or ($80 shr byte(R_ECX))
                  else if pattern='EDX' then
                    usedinproc:=usedinproc or ($80 shr byte(R_EDX))
                  else if pattern='ESI' then
                    begin
                       usedinproc:=usedinproc or ($80 shr byte(R_ESI));
                       asmstat^.object_preserved:=false;
                    end
                  else if pattern='EDI' then
                    usedinproc:=usedinproc or ($80 shr byte(R_EDI))
{$endif i386}
{$ifdef m68k}
                  if pattern='D0' then
                    usedinproc:=usedinproc or ($800 shr word(R_D0))
                  else if pattern='D1' then
                    usedinproc:=usedinproc or ($800 shr word(R_D1))
                  else if pattern='D6' then
                    usedinproc:=usedinproc or ($800 shr word(R_D6))
                  else if pattern='A0' then
                    usedinproc:=usedinproc or ($800 shr word(R_A0))
                  else if pattern='A1' then
                    usedinproc:=usedinproc or ($800 shr word(R_A1))
{$endif m68k}
                  else consume(_RECKKLAMMER);
                  consume(_CSTRING);
                  if not try_to_consume(_COMMA) then
                    break;
                until false;
              consume(_RECKKLAMMER);
           end
         else usedinproc:=$ff;
{$endif newcg}

{ mark the start and the end of the assembler block for the optimizer }

         If Assigned(AsmStat^.p_asm) Then
           Begin
             Marker := New(Pai_Marker, Init(AsmBlockStart));
             AsmStat^.p_asm^.Insert(Marker);
             Marker := New(Pai_Marker, Init(AsmBlockEnd));
             AsmStat^.p_asm^.Concat(Marker);
           End;
         Inside_asm_statement:=false;

         _asm_statement:=asmstat;
      end;


        function new_dispose_statement : ptree;
        var
          p,p2 : ptree;
          ht : ttoken;
          again : boolean; { dummy for do_proc_call }
          destrukname : stringid;
          sym : psym;
          classh : pobjectdef;
          pd,pd2 : pdef;
          store_valid : boolean;
          tt : ttreetyp;
        begin
          ht:=token;
          if try_to_consume(_NEW) then
            tt:=hnewn
          else
            begin
                consume(_DISPOSE);
                tt:=hdisposen;
            end;
          consume(_LKLAMMER);

          { displaced here to avoid warnings in BP mode (PM) }
          Store_valid := Must_be_valid;
          if tt=hnewn then
            Must_be_valid := False
          else
            Must_be_valid:=true;

          p:=comp_expr(true);

          { calc return type }
          cleartempgen;
          do_firstpass(p);
          Must_be_valid := Store_valid;

  {var o:Pobject;
           begin
               new(o,init);     (*Also a valid new statement*)
           end;}

          if try_to_consume(_COMMA) then
            begin
                   { extended syntax of new and dispose }
                   { function styled new is handled in factor }
                   { destructors have no parameters }
                   destrukname:=pattern;
                   consume(_ID);

                   pd:=p^.resulttype;
                   if pd=nil then
                    pd:=generrordef;
                   pd2:=pd;
                   if (pd^.deftype<>pointerdef) then
                     begin
                        Message1(type_e_pointer_type_expected,pd^.typename);
                        p:=factor(false);
                        consume(_RKLAMMER);
                        new_dispose_statement:=genzeronode(errorn);
                        exit;
                     end;
                   { first parameter must be an object or class }
                   if ppointerdef(pd)^.definition^.deftype<>objectdef then
                     begin
                        Message(parser_e_pointer_to_class_expected);
                        new_dispose_statement:=factor(false);
                        consume_all_until(_RKLAMMER);
                        consume(_RKLAMMER);
                        exit;
                     end;
                   { check, if the first parameter is a pointer to a _class_ }
                   classh:=pobjectdef(ppointerdef(pd)^.definition);
                   if classh^.is_class then
                     begin
                        Message(parser_e_no_new_or_dispose_for_classes);
                        new_dispose_statement:=factor(false);
                        consume_all_until(_RKLAMMER);
                        consume(_RKLAMMER);
                        exit;
                     end;
                   { search cons-/destructor, also in parent classes }
                   sym:=search_class_member(classh,pattern);
                   { the second parameter of new/dispose must be a call }
                   { to a cons-/destructor                              }
                   if (not assigned(sym)) or (sym^.typ<>procsym) then
                         begin
                            Message(parser_e_expr_have_to_be_destructor_call);
                            new_dispose_statement:=genzeronode(errorn);
                         end
                   else
                         begin
                           p2:=gensinglenode(tt,p);
                           if ht=_NEW then
                                 begin
                                    { Constructors can take parameters.}
                                    p2^.resulttype:=ppointerdef(pd)^.definition;
                                    do_member_read(false,sym,p2,pd,again);
                                 end
                           else
                             { destructors can't.}
                             p2:=genmethodcallnode(pprocsym(sym),srsymtable,p2);

                           { we need the real called method }
                           cleartempgen;
                           do_firstpass(p2);

                           if not codegenerror then
                            begin
                              if (ht=_NEW) and (p2^.procdefinition^.proctypeoption<>potype_constructor) then
                                Message(parser_e_expr_have_to_be_constructor_call);
                              if (ht=_DISPOSE) and (p2^.procdefinition^.proctypeoption<>potype_destructor) then
                                Message(parser_e_expr_have_to_be_destructor_call);

                              if ht=_NEW then
                               begin
                                 p2:=gennode(assignn,getcopy(p),gensinglenode(newn,p2));
                                 p2^.right^.resulttype:=pd2;
                               end;
                            end;
                           new_dispose_statement:=p2;
                         end;
            end
          else
            begin
               if p^.resulttype=nil then
                p^.resulttype:=generrordef;
               if (p^.resulttype^.deftype<>pointerdef) then
                 Begin
                    Message1(type_e_pointer_type_expected,p^.resulttype^.typename);
                    new_dispose_statement:=genzeronode(errorn);
                 end
               else
                 begin
                    if (ppointerdef(p^.resulttype)^.definition^.deftype=objectdef) and
                       (oo_has_vmt in pobjectdef(ppointerdef(p^.resulttype)^.definition)^.objectoptions) then
                      Message(parser_w_use_extended_syntax_for_objects);
                    if (ppointerdef(p^.resulttype)^.definition^.deftype=orddef) and
                       (porddef(ppointerdef(p^.resulttype)^.definition)^.typ=uvoid) then
                      if (m_tp in aktmodeswitches) or
                         (m_delphi in aktmodeswitches) then
                       Message(parser_w_no_new_dispose_on_void_pointers)
                      else
                       Message(parser_e_no_new_dispose_on_void_pointers);

                     case ht of
                        _NEW : new_dispose_statement:=gensinglenode(simplenewn,p);
                        _DISPOSE : new_dispose_statement:=gensinglenode(simpledisposen,p);
                     end;
                 end;
            end;
          consume(_RKLAMMER);
      end;


    function statement_block(starttoken : ttoken) : ptree;

      var
         first,last : ptree;
         filepos : tfileposinfo;

      begin
         first:=nil;
         filepos:=tokenpos;
         consume(starttoken);
         inc(statement_level);

         while not(token in [_END,_FINALIZATION]) do
           begin
              if first=nil then
                begin
                   last:=gennode(statementn,nil,statement);
                   first:=last;
                end
              else
                begin
                   last^.left:=gennode(statementn,nil,statement);
                   last:=last^.left;
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
              emptystats;
           end;

         { don't consume the finalization token, it is consumed when
           reading the finalization block, but allow it only after
           an initalization ! }
         if (starttoken<>_INITIALIZATION) or (token<>_FINALIZATION) then
           consume(_END);

         dec(statement_level);

         last:=gensinglenode(blockn,first);
         set_tree_filepos(last,filepos);
         statement_block:=last;
      end;


    function statement : ptree;

      var
         p : ptree;
         code : ptree;
         labelnr : pasmlabel;
         filepos : tfileposinfo;

      label
         ready;

      begin
         filepos:=tokenpos;
         case token of
            _GOTO : begin
                       if not(cs_support_goto in aktmoduleswitches)then
                        Message(sym_e_goto_and_label_not_supported);
                       consume(_GOTO);
                       if (token<>_INTCONST) and (token<>_ID) then
                         begin
                            Message(sym_e_label_not_found);
                            code:=genzeronode(errorn);
                         end
                       else
                         begin
                            getsym(pattern,true);
                            consume(token);
                            if srsym^.typ<>labelsym then
                              begin
                                 Message(sym_e_id_is_no_label_id);
                                 code:=genzeronode(errorn);
                              end
                            else
                              code:=genlabelnode(goton,
                                plabelsym(srsym)^.lab);
                         end;
                    end;
            _BEGIN : code:=statement_block(_BEGIN);
            _IF    : code:=if_statement;
            _CASE  : code:=case_statement;
            _REPEAT : code:=repeat_statement;
            _WHILE : code:=while_statement;
            _FOR : code:=for_statement;
            _NEW,_DISPOSE : code:=new_dispose_statement;

            _WITH : code:=with_statement;
            _TRY : code:=try_statement;
            _RAISE : code:=raise_statement;
            { semicolons,else until and end are ignored }
            _SEMICOLON,
            _ELSE,
            _UNTIL,
            _END:
              code:=genzeronode(niln);
            _FAIL : begin
                       { internalerror(100); }
                       if (aktprocsym^.definition^.proctypeoption<>potype_constructor) then
                        Message(parser_e_fail_only_in_constructor);
                       consume(_FAIL);
                       code:=genzeronode(failn);
                    end;
            _EXIT : code:=exit_statement;
            _ASM : begin
                      code:=_asm_statement;
                   end;
            _EOF : begin
                     Message(scan_f_end_of_file);
                   end;
         else
           begin
              if (token=_INTCONST) or
                 ((token=_ID) and not((m_result in aktmodeswitches) and (idtoken=_RESULT))) then
                begin
                   getsym(pattern,true);
                   lastsymknown:=true;
                   lastsrsym:=srsym;
                   { it is NOT necessarily the owner
                     it can be a withsymtable !!! }
                   lastsrsymtable:=srsymtable;
                   if assigned(srsym) and (srsym^.typ=labelsym) then
                     begin
                        consume(token);
                        consume(_COLON);
                        if plabelsym(srsym)^.defined then
                          Message(sym_e_label_already_defined);
                        plabelsym(srsym)^.defined:=true;

                        { statement modifies srsym }
                        labelnr:=plabelsym(srsym)^.lab;
                        lastsymknown:=false;
                        { the pointer to the following instruction }
                        { isn't a very clean way                   }
{$ifdef tp}
                        code:=gensinglenode(labeln,statement);
{$else}
                        code:=gensinglenode(labeln,statement());
{$endif}
                        code^.labelnr:=labelnr;
                        { sorry, but there is a jump the easiest way }
                        goto ready;
                     end;
                end;
              p:=expr;
              if not(p^.treetype in [calln,assignn,breakn,inlinen,
                continuen]) then
                Message(cg_e_illegal_expression);
              { specify that we don't use the value returned by the call }
              { Question : can this be also improtant
                for inlinen ??
                it is used for :
                 - dispose of temp stack space
                 - dispose on FPU stack }
              if p^.treetype=calln then
                p^.return_value_used:=false;
              code:=p;
           end;
         end;
         ready:
         if assigned(code) then
          set_tree_filepos(code,filepos);
         statement:=code;
      end;

    function block(islibrary : boolean) : ptree;

      var
         funcretsym : pfuncretsym;
         storepos : tfileposinfo;

      begin
         if procinfo.retdef<>pdef(voiddef) then
           begin
              { if the current is a function aktprocsym is non nil }
              { and there is a local symtable set }
              storepos:=tokenpos;
              tokenpos:=aktprocsym^.fileinfo;
              funcretsym:=new(pfuncretsym,init(aktprocsym^.name,@procinfo));
              { insert in local symtable }
              symtablestack^.insert(funcretsym);
              tokenpos:=storepos;
              if ret_in_acc(procinfo.retdef) or (procinfo.retdef^.deftype=floatdef) then
                procinfo.retoffset:=-funcretsym^.address;
              procinfo.funcretsym:=funcretsym;
           end;
         read_declarations(islibrary);

         { temporary space is set, while the BEGIN of the procedure }
         if (symtablestack^.symtabletype=localsymtable) then
           procinfo.firsttemp := -symtablestack^.datasize
         else procinfo.firsttemp := 0;

         { space for the return value }
         { !!!!!   this means that we can not set the return value
         in a subfunction !!!!! }
         { because we don't know yet where the address is }
         if procinfo.retdef<>pdef(voiddef) then
           begin
              if ret_in_acc(procinfo.retdef) or (procinfo.retdef^.deftype=floatdef) then
              { if (procinfo.retdef^.deftype=orddef) or
                 (procinfo.retdef^.deftype=pointerdef) or
                 (procinfo.retdef^.deftype=enumdef) or
                 (procinfo.retdef^.deftype=procvardef) or
                 (procinfo.retdef^.deftype=floatdef) or
                 (
                   (procinfo.retdef^.deftype=setdef) and
                   (psetdef(procinfo.retdef)^.settype=smallset)
                 ) then  }
                begin
                   { the space has been set in the local symtable }
                   procinfo.retoffset:=-funcretsym^.address;
                   if ((procinfo.flags and pi_operator)<>0) and
                     assigned(opsym) then
                     {opsym^.address:=procinfo.call_offset; is wrong PM }
                     opsym^.address:=-procinfo.retoffset;
                   { eax is modified by a function }
{$ifndef newcg}
{$ifdef i386}
                   usedinproc:=usedinproc or ($80 shr byte(R_EAX));

                   if is_64bitint(procinfo.retdef) then
                     usedinproc:=usedinproc or ($80 shr byte(R_EDX))
{$endif}
{$ifdef m68k}
                   usedinproc:=usedinproc or ($800 shr word(R_D0))

                   if is_64bitint(procinfo.retdef) then
                     usedinproc:=usedinproc or ($800 shr byte(R_D1))
{$endif}
{$endif newcg}
                end;
           end;

         {Unit initialization?.}
         if (lexlevel=unit_init_level) and (current_module^.is_unit) then
           if (token=_END) then
             begin
                consume(_END);
                block:=nil;
             end
           else
             begin
                if token=_INITIALIZATION then
                  begin
                     current_module^.flags:=current_module^.flags or uf_init;
                     block:=statement_block(_INITIALIZATION);
                  end
                else if (token=_FINALIZATION) then
                  begin
                     if (current_module^.flags and uf_finalize)<>0 then
                       block:=statement_block(_FINALIZATION)
                     else
                       begin
                          block:=nil;
                          exit;
                       end;
                  end
                else
                  begin
                     current_module^.flags:=current_module^.flags or uf_init;
                     block:=statement_block(_BEGIN);
                  end;
             end
         else
            block:=statement_block(_BEGIN);
      end;

    function assembler_block : ptree;

      begin
         read_declarations(false);
         { temporary space is set, while the BEGIN of the procedure }
         if symtablestack^.symtabletype=localsymtable then
           procinfo.firsttemp := -symtablestack^.datasize
         else
           procinfo.firsttemp := 0;

         { assembler code does not allocate }
         { space for the return value       }
          if procinfo.retdef<>pdef(voiddef) then
           begin
              if ret_in_acc(procinfo.retdef) then
                begin
                   { in assembler code the result should be directly in %eax
                   procinfo.retoffset:=procinfo.firsttemp-procinfo.retdef^.size;
                   procinfo.firsttemp:=procinfo.retoffset;                 }

{$ifndef newcg}
{$ifdef i386}
                   usedinproc:=usedinproc or ($80 shr byte(R_EAX))
{$endif}
{$ifdef m68k}
                   usedinproc:=usedinproc or ($800 shr word(R_D0))
{$endif}
{$endif newcg}
                end
              {
              else if not is_fpu(procinfo.retdef) then
               should we allow assembler functions of big elements ?
                YES (FK)!!
               Message(parser_e_asm_incomp_with_function_return);
              }
            end;
           { set the framepointer to esp for assembler functions }
           { but only if the are no local variables           }
           { added no parameter also (PM)                       }
           { disable for methods, because self pointer is expected }
           { at -8(%ebp) (JM)                                      }
           if not(assigned(procinfo._class)) and
              (po_assembler in aktprocsym^.definition^.procoptions) and
              (aktprocsym^.definition^.localst^.datasize=0) and
              (aktprocsym^.definition^.parast^.datasize=0) and
              not(ret_in_param(aktprocsym^.definition^.retdef)) then
             begin
               procinfo.framepointer:=stack_pointer;
               { set the right value for parameters }
               dec(aktprocsym^.definition^.parast^.address_fixup,target_os.size_of_pointer);
               dec(procinfo.call_offset,target_os.size_of_pointer);
             end;
          { force the asm statement }
            if token<>_ASM then
             consume(_ASM);
            Procinfo.Flags := ProcInfo.Flags Or pi_is_assembler;
            assembler_block:=_asm_statement;
          { becuase the END is already read we need to get the
            last_endtoken_filepos here (PFV) }
            last_endtoken_filepos:=tokenpos;
          end;

end.
{
  $Log$
  Revision 1.100  1999-09-07 14:12:36  jonas
    * framepointer cannot be changed to esp for methods

  Revision 1.99  1999/08/26 21:10:08  peter
    * better error recovery for case

  Revision 1.98  1999/08/05 16:53:05  peter
    * V_Fatal=1, all other V_ are also increased
    * Check for local procedure when assigning procvar
    * fixed comment parsing because directives
    * oldtp mode directives better supported
    * added some messages to errore.msg

  Revision 1.97  1999/08/04 13:02:59  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.96  1999/08/04 00:23:19  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.95  1999/08/03 22:03:03  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.94  1999/08/03 17:09:39  florian
    * the alpha compiler can be compiled now

  Revision 1.93  1999/08/02 21:28:59  florian
    * the main branch psub.pas is now used for
      newcg compiler

  Revision 1.92  1999/07/26 09:42:14  florian
    * bugs 494-496 fixed

  Revision 1.91  1999/06/30 22:16:22  florian
    * use of is_ordinal checked: often a qword/int64 isn't allowed (case/for ...)
    * small qword problems fixed

  Revision 1.90  1999/06/22 16:24:43  pierre
   * local browser stuff corrected

  Revision 1.89  1999/06/17 13:19:54  pierre
   * merged from 0_99_12 branch

  Revision 1.88.2.1  1999/06/17 12:51:46  pierre
   * changed is_assignment_overloaded into
      function assignment_overloaded : pprocdef
      to allow overloading of assignment with only different result type

  Revision 1.88  1999/06/15 13:19:46  pierre
   * better uninitialized var tests for TP mode

  Revision 1.87  1999/05/27 19:44:50  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.86  1999/05/21 13:55:08  peter
    * NEWLAB for label as symbol

  Revision 1.85  1999/05/17 23:51:40  peter
    * with temp vars now use a reference with a persistant temp instead
      of setting datasize

  Revision 1.84  1999/05/13 21:59:38  peter
    * removed oldppu code
    * warning if objpas is loaded from uses
    * first things for new deref writing

  Revision 1.83  1999/05/05 22:21:58  peter
    * updated messages

  Revision 1.82  1999/05/01 13:24:35  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.81  1999/04/26 13:31:42  peter
    * release storenumber,double_checksum

  Revision 1.80  1999/04/21 09:43:48  peter
    * storenumber works
    * fixed some typos in double_checksum
    + incompatible types type1 and type2 message (with storenumber)

  Revision 1.79  1999/04/16 12:14:49  pierre
   * void pointer accepted with warning in tp and delphi mode

  Revision 1.78  1999/04/15 12:58:14  pierre
   * fix for bug0234

  Revision 1.77  1999/04/15 09:01:33  peter
    * fixed set loading
    * object inheritance support for browser

  Revision 1.76  1999/04/14 18:41:25  daniel
  * Better use of routines in pbase and symtable. 4k code removed.

  Revision 1.75  1999/04/14 09:14:53  peter
    * first things to store the symbol/def number in the ppu

  Revision 1.74  1999/04/09 12:22:06  pierre
   * bug found by Peter for DirectWith code fixed

  Revision 1.73  1999/04/06 11:21:57  peter
    * more use of ttoken

  Revision 1.72  1999/03/31 13:55:15  peter
    * assembler inlining working for ag386bin

  Revision 1.71  1999/03/10 11:23:29  pierre
   * typecheck for exit(value) : resulttype was not set

  Revision 1.70  1999/03/04 13:55:45  pierre
    * some m68k fixes (still not compilable !)
    * new(tobj) does not give warning if tobj has no VMT !

  Revision 1.69  1999/03/02 02:56:15  peter
    + stabs support for binary writers
    * more fixes and missing updates from the previous commit :(

  Revision 1.68  1999/02/26 00:48:23  peter
    * assembler writers fixed for ag386bin

  Revision 1.67  1999/02/22 13:07:01  pierre
    + -b and -bl options work !
    + cs_local_browser ($L+) is disabled if cs_browser ($Y+)
      is not enabled when quitting global section
    * local vars and procedures are not yet stored into PPU

  Revision 1.66  1999/02/22 02:15:31  peter
    * updates for ag386bin

  Revision 1.65  1999/02/15 13:13:15  pierre
   * fix for bug0216

  Revision 1.64  1999/02/11 09:46:26  pierre
    * fix for normal method calls inside static methods :
      WARNING there were both parser and codegen errors !!
      added static_call boolean to calln tree

  Revision 1.63  1999/02/09 15:45:47  florian
    + complex results for assembler functions, fixes bug0155

  Revision 1.62  1999/01/27 13:06:57  pierre
   * memory leak in case optimization fixed

  Revision 1.61  1999/01/25 22:49:09  peter
    * more fixes for the on bug with unknown id

  Revision 1.60  1999/01/23 23:29:38  florian
    * first running version of the new code generator
    * when compiling exceptions under Linux fixed

  Revision 1.59  1999/01/21 16:41:02  pierre
   * fix for constructor inside with statements

  Revision 1.58  1999/01/05 08:20:07  florian
    * mainly problem with invalid case ranges fixed (reported by Jonas)

  Revision 1.57  1998/12/29 18:48:15  jonas
    + optimize pascal code surrounding assembler blocks

  Revision 1.56  1998/12/23 22:52:56  peter
    * fixed new(x) crash if x contains an error

  Revision 1.55  1998/12/16 12:30:59  jonas
    * released CaseRange

  Revision 1.54  1998/12/15 22:32:24  jonas
    + convert consecutive case labels to a single range (-dCaseRange)

  Revision 1.53  1998/12/15 11:52:18  peter
    * fixed dup release of statement label in case

  Revision 1.52  1998/12/11 00:03:37  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.51  1998/12/10 09:47:24  florian
    + basic operations with int64/qord (compiler with -dint64)
    + rtti of enumerations extended: names are now written

  Revision 1.50  1998/11/13 15:40:25  pierre
    + added -Se in Makefile cvstest target
    + lexlevel cleanup
      normal_function_level main_program_level and unit_init_level defined
    * tins_cache grown to A_EMMS (gave range check error in asm readers)
      (test added in code !)
    * -Un option was wrong
    * _FAIL and _SELF only keyword inside
      constructors and methods respectively

  Revision 1.49  1998/11/12 12:55:17  pierre
   * fix for bug0176 and bug0177

  Revision 1.48  1998/11/05 23:43:24  peter
    * fixed assembler directive and then not an ASM statement

  Revision 1.47  1998/10/30 16:20:22  peter
    * fixed dispose(destructor) crash when destructor didn't exists

  Revision 1.46  1998/10/20 08:06:53  pierre
    * several memory corruptions due to double freemem solved
      => never use p^.loc.location:=p^.left^.loc.location;
    + finally I added now by default
      that ra386dir translates global and unit symbols
    + added a first field in tsymtable and
      a nextsym field in tsym
      (this allows to obtain ordered type info for
      records and objects in gdb !)

  Revision 1.45  1998/10/19 08:55:01  pierre
    * wrong stabs info corrected once again !!
    + variable vmt offset with vmt field only if required
      implemented now !!!

  Revision 1.44  1998/10/13 13:10:27  peter
    * new style for m68k/i386 infos and enums

  Revision 1.43  1998/10/08 13:46:22  peter
    * added eof message
    * fixed unit init section parsing with finalize

  Revision 1.42  1998/09/26 17:45:38  peter
    + idtoken and only one token table

  Revision 1.41  1998/09/24 23:49:15  peter
    + aktmodeswitches

  Revision 1.40  1998/09/23 21:53:04  florian
    * the following doesn't work: on texception do, was a parser error, fixed

  Revision 1.39  1998/09/21 10:26:07  peter
    * merged fix

  Revision 1.38.2.1  1998/09/21 10:24:43  peter
    * fixed error recovery with with

  Revision 1.38  1998/09/04 08:42:04  peter
    * updated some error messages

  Revision 1.37  1998/08/21 14:08:52  pierre
    + TEST_FUNCRET now default (old code removed)
      works also for m68k (at least compiles)

  Revision 1.36  1998/08/20 21:36:41  peter
    * fixed 'with object do' bug

  Revision 1.35  1998/08/20 09:26:42  pierre
    + funcret setting in underproc testing
      compile with _dTEST_FUNCRET

  Revision 1.34  1998/08/17 10:10:09  peter
    - removed OLDPPU

  Revision 1.33  1998/08/12 19:39:30  peter
    * fixed some crashes

  Revision 1.32  1998/08/10 14:50:17  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.31  1998/08/02 16:41:59  florian
    * on o : tobject do should also work now, the exceptsymtable shouldn't be
      disposed by dellexlevel

  Revision 1.30  1998/07/30 16:07:10  florian
    * try ... expect <statement> end; works now

  Revision 1.29  1998/07/30 13:30:37  florian
    * final implemenation of exception support, maybe it needs
      some fixes :)

  Revision 1.28  1998/07/30 11:18:18  florian
    + first implementation of try ... except on .. do end;
    * limitiation of 65535 bytes parameters for cdecl removed

  Revision 1.27  1998/07/28 21:52:55  florian
    + implementation of raise and try..finally
    + some misc. exception stuff

  Revision 1.26  1998/07/27 21:57:14  florian
    * fix to allow tv like stream registration:
        @tmenu.load doesn't work if load had parameters or if load was only
        declared in an anchestor class of tmenu

  Revision 1.25  1998/07/14 21:46:53  peter
    * updated messages file

  Revision 1.24  1998/07/10 10:48:42  peter
    * fixed realnumber scanning
    * [] after asmblock was not uppercased anymore

  Revision 1.23  1998/06/25 08:48:18  florian
    * first version of rtti support

  Revision 1.22  1998/06/24 14:48:36  peter
    * ifdef newppu -> ifndef oldppu

  Revision 1.21  1998/06/24 14:06:34  peter
    * fixed the name changes

  Revision 1.20  1998/06/23 14:00:16  peter
    * renamed RA* units

  Revision 1.19  1998/06/08 22:59:50  peter
    * smartlinking works for win32
    * some defines to exclude some compiler parts

  Revision 1.18  1998/06/05 14:37:35  pierre
    * fixes for inline for operators
    * inline procedure more correctly restricted

  Revision 1.17  1998/06/04 09:55:43  pierre
    * demangled name of procsym reworked to become independant of the mangling scheme

  Revision 1.16  1998/06/02 17:03:04  pierre
    *  with node corrected for objects
    * small bugs for SUPPORT_MMX fixed

  Revision 1.15  1998/05/30 14:31:06  peter
    + $ASMMODE

  Revision 1.14  1998/05/29 09:58:14  pierre
    * OPR_REGISTER for 1 arg was missing in ratti386.pas
      (probably a merging problem)
    * errors at start of line were lost

  Revision 1.13  1998/05/28 17:26:50  peter
    * fixed -R switch, it didn't work after my previous akt/init patch
    * fixed bugs 110,130,136

  Revision 1.12  1998/05/21 19:33:33  peter
    + better procedure directive handling and only one table

  Revision 1.11  1998/05/20 09:42:35  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.10  1998/05/11 13:07:56  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.9  1998/05/06 08:38:46  pierre
    * better position info with UseTokenInfo
      UseTokenInfo greatly simplified
    + added check for changed tree after first time firstpass
      (if we could remove all the cases were it happen
      we could skip all firstpass if firstpasscount > 1)
      Only with ExtDebug

  Revision 1.8  1998/05/05 12:05:42  florian
    * problems with properties fixed
    * crash fixed:  i:=l when i and l are undefined, was a problem with
      implementation of private/protected

  Revision 1.7  1998/05/01 16:38:46  florian
    * handling of private and protected fixed
    + change_keywords_to_tp implemented to remove
      keywords which aren't supported by tp
    * break and continue are now symbols of the system unit
    + widestring, longstring and ansistring type released

  Revision 1.6  1998/04/30 15:59:42  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.5  1998/04/29 10:33:59  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.4  1998/04/08 16:58:05  pierre
    * several bugfixes
      ADD ADC and AND are also sign extended
      nasm output OK (program still crashes at end
      and creates wrong assembler files !!)
      procsym types sym in tdef removed !!
}

