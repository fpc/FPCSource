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
       cobjects,scanner,globals,symtable,aasm,pass_1,
       types,hcodegen,files,verbose,systems
{$ifdef NEWPPU}
       ,ppu
{$endif}
       { processor specific stuff }
{$ifdef i386}
       ,i386
       ,rai386
       ,ratti386
       ,radi386
       ,tgeni386
{$endif}
{$ifdef m68k}
       ,m68k
       ,tgen68k
       ,ag68kmit
       ,ra68k
       ,ag68kgas
       ,ag68kmot
{$endif}
       { parser specific stuff, be careful consume is also defined to }
       { read assembler tokens                                        }
       ,pbase,pexpr,pdecl;

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

         if token=_ELSE then
           begin
              consume(_ELSE);
              else_a:=statement;
           end
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
              if token<>SEMICOLON then
                break
              else
                consume(SEMICOLON);
              while token=SEMICOLON do
                consume(SEMICOLON);

           end;
         consume(_END);
         statements_til_end:=gensinglenode(blockn,first);
      end;

    function case_statement : ptree;

      var
         { contains the label number of currently parsed case block }
         aktcaselabel : plabel;
         root : pcaserecord;

         { the typ of the case expression }
         casedef : pdef;

      procedure newcaselabel(l,h : longint);

        var
           hcaselabel : pcaserecord;

        procedure insertlabel(var p : pcaserecord);

          begin
             if p=nil then p:=hcaselabel
             else
                if (p^._low>hcaselabel^._low) and
                   (p^._low>hcaselabel^._high) then
                  insertlabel(p^.less)
                else if (p^._high<hcaselabel^._low) and
                   (p^._high<hcaselabel^._high) then
                  insertlabel(p^.greater)
                else Message(parser_e_double_caselabel);
          end;

        begin
           new(hcaselabel);
           hcaselabel^.less:=nil;
           hcaselabel^.greater:=nil;
           hcaselabel^.statement:=aktcaselabel;
           getlabel(hcaselabel^._at);
           hcaselabel^._low:=l;
           hcaselabel^._high:=h;
           insertlabel(root);
        end;

      var
         code,caseexpr,p,instruc,elseblock : ptree;
         hl1,hl2 : longint;
         ranges : boolean;

      begin
         consume(_CASE);
         caseexpr:=comp_expr(true);
       { determines result type }
         cleartempgen;
         do_firstpass(caseexpr);
         casedef:=caseexpr^.resulttype;
         if not(is_ordinal(casedef)) then
           Message(parser_e_ordinal_expected);

         consume(_OF);
         inc(statement_level);
         root:=nil;
         ranges:=false;
         instruc:=nil;
         repeat
           getlabel(aktcaselabel);
           {aktcaselabel^.is_used:=true; }

           { may be an instruction has more case labels }
           repeat
             p:=expr;
             cleartempgen;
             do_firstpass(p);

             if (p^.treetype=rangen) then
               begin
                  { type checking for case statements }
                  if not is_subequal(casedef, p^.left^.resulttype) then
                    Message(parser_e_case_mismatch);
                  { type checking for case statements }
                  if not is_subequal(casedef, p^.right^.resulttype) then
                    Message(parser_e_case_mismatch);
                  hl1:=get_ordinal_value(p^.left);
                  hl2:=get_ordinal_value(p^.right);
                  testrange(casedef,hl1);
                  testrange(casedef,hl2);
                  newcaselabel(hl1,hl2);
                  ranges:=true;
               end
             else
               begin
                  { type checking for case statements }
                  if not is_subequal(casedef, p^.resulttype) then
                    Message(parser_e_case_mismatch);
                    hl1:=get_ordinal_value(p);
                    testrange(casedef,hl1);
                    newcaselabel(hl1,hl1);
               end;
             disposetree(p);
             if token=COMMA then consume(COMMA)
               else break;
           until false;
           consume(COLON);

           { handles instruction block }
           p:=gensinglenode(labeln,statement);
           p^.labelnr:=aktcaselabel;

           { concats instruction }
           instruc:=gennode(statementn,instruc,p);

           if not((token=_ELSE) or (token=_OTHERWISE) or (token=_END)) then
             consume(SEMICOLON);
         until (token=_ELSE) or (token=_OTHERWISE) or (token=_END);

         if (token=_ELSE) or (token=_OTHERWISE) then
           begin
              if token=_ELSE then consume(_ELSE)
                else consume(_OTHERWISE);
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
              if token<>SEMICOLON then
                break;
              consume(SEMICOLON);
              while token=SEMICOLON do
                consume(SEMICOLON);
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
         case p^.resulttype^.deftype of
            objectdef : begin
                          obj:=pobjectdef(p^.resulttype);
                          { this creates the stack in the wrong order !!
                          levelcount:=0;
                          while assigned(obj) do
                            begin
                               symtab:=obj^.publicsyms;
                               withsymtable:=new(psymtable,init(symtable.withsymtable));
                               withsymtable^.root:=symtab^.root;
                               withsymtable^.next:=symtablestack;
                               symtablestack:=withsymtable;
                               obj:=obj^.childof;
                               inc(levelcount);
                            end; }

                          withsymtable:=new(psymtable,init(symtable.withsymtable));
                          withsymtable^.root:=obj^.publicsyms^.root;
                          symtab:=withsymtable;
                          levelcount:=1;
                          obj:=obj^.childof;
                          while assigned(obj) do
                            begin
                               symtab^.next:=new(psymtable,init(symtable.withsymtable));
                               symtab:=symtab^.next;
                               symtab^.root:=obj^.publicsyms^.root;
                               obj:=obj^.childof;
                               inc(levelcount);
                            end;
                          symtab^.next:=symtablestack;
                          symtablestack:=withsymtable;
                       end;
            recorddef : begin
                           symtab:=precdef(p^.resulttype)^.symtable;
                           levelcount:=1;
                           withsymtable:=new(psymtable,init(symtable.withsymtable));
                           withsymtable^.root:=symtab^.root;
                           withsymtable^.next:=symtablestack;
                           symtablestack:=withsymtable;
                        end;
            else
              begin
                 Message(parser_e_false_with_expr);
                 { try to recover from error }
                 if token=COMMA then
                   begin
                      consume(COMMA);
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
                      if token<>SEMICOLON then
                      statement;
                   end;
                 _with_statement:=nil;
                 exit;
              end;
         end;
         if token=COMMA then
           begin
              consume(COMMA);
{$ifdef tp}
              right:=_with_statement;
{$else}
              right:=_with_statement();
{$endif}
           end
         else
           begin
              consume(_DO);
              if token<>SEMICOLON then
                right:=statement
              else
                right:=nil;
           end;
         for i:=1 to levelcount do
           symtablestack:=symtablestack^.next;

         _with_statement:=genwithnode(withsymtable,p,right,levelcount);
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
         if token<>SEMICOLON then
           begin
              p1:=comp_expr(true);
              if (token=ID) and (pattern='AT') then
                begin
                   consume(ID);
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
         p_default,e1,e2,p_specific : ptree;

         old_in_except_block : boolean;

      begin
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
              if token<>SEMICOLON then
                break;
              consume(SEMICOLON);
              emptystats;
           end;
         p_try_block:=gensinglenode(blockn,first);

         if token=_FINALLY then
           begin
              consume(_FINALLY);
              p_finally_block:=statements_til_end;
              try_statement:=gennode(tryfinallyn,p_try_block,p_finally_block);
              dec(statement_level);

           end
         else
           begin
              consume(_EXCEPT);
              old_in_except_block:=in_except_block;
              in_except_block:=true;

              if token=_ON then
                { catch specific exceptions }
                begin
                   repeat
                     consume(_ON);
                     e1:=comp_expr(true);
                     if token=COLON then
                       begin
                          consume(COLON);
                          e2:=comp_expr(true);
                          { !!!!! }
                       end
                     else
                       begin
                          { !!!!! }
                       end;
                     consume(_DO);
                                         statement;
                                         if token<>SEMICOLON then
                                           break;
                                         emptystats;
                                   until false;
                   if token=_ELSE then
                     { catch the other exceptions }
                     begin
                        consume(_ELSE);
                        p_default:=statements_til_end;
                     end;
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
         if token=LKLAMMER then
           begin
              consume(LKLAMMER);
              p:=comp_expr(true);
              consume(RKLAMMER);
              if procinfo.retdef=pdef(voiddef) then
                Message(parser_e_void_function)
              else
                procinfo.funcret_is_valid:=true;
           end
         else
           p:=nil;
         exit_statement:=gensinglenode(exitn,p);
      end;


{$ifdef i386}
    function _asm_statement : ptree;

      var asm_stat : ptree;
      
      begin
         if (aktprocsym^.definition^.options and poinline)<>0 then
           Begin
              Comment(V_Warning,'asm statement inside inline procedure/function not yet supported');
              Comment(V_Warning,'inlining disabled');
              aktprocsym^.definition^.options:= aktprocsym^.definition^.options and not poinline;
           End;
         case aktasmmode of
            I386_ATT : asm_stat:=ratti386.assemble;
            I386_INTEL : asm_stat:=rai386.assemble;
            I386_DIRECT : asm_stat:=radi386.assemble;
            else internalerror(30004);
         end;

         { Erst am Ende _ASM konsumieren, da der Scanner sonst die }
         { erste Assemblerstatement zu lesen versucht! }
         consume(_ASM);

         { (END is read) }
         if token=LECKKLAMMER then
           begin
              { it's possible to specify the modified registers }
              consume(LECKKLAMMER);
              asm_stat^.object_preserved:=true;
              if token<>RECKKLAMMER then
                repeat
                  pattern:=upper(pattern);
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
                       asm_stat^.object_preserved:=false;
                    end
                  else if pattern='EDI' then
                    usedinproc:=usedinproc or ($80 shr byte(R_EDI))
                  else consume(RECKKLAMMER);
                  consume(CSTRING);
                  if token=COMMA then consume(COMMA)
                    else break;
                until false;
              consume(RECKKLAMMER);
           end
         else usedinproc:=$ff;
         _asm_statement:=asm_stat;
      end;
{$endif}

{$ifdef m68k}
    function _asm_statement : ptree;
    begin
         _asm_statement:= ra68k.assemble;
         { Erst am Ende _ASM konsumieren, da der Scanner sonst die }
         { erste Assemblerstatement zu lesen versucht! }
         consume(_ASM);

         { (END is read) }
         if token=LECKKLAMMER then
           begin
              { it's possible to specify the modified registers }
              { we only check the registers which are not reserved }
              { and which can be used. This is done for future     }
              { optimizations.                                     }
              consume(LECKKLAMMER);
              if token<>RECKKLAMMER then
                repeat
                  pattern:=upper(pattern);
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
                  else consume(RECKKLAMMER);
                  consume(CSTRING);
                  if token=COMMA then consume(COMMA)
                    else break;
                until false;
              consume(RECKKLAMMER);
           end
         else usedinproc:=$ffff;
    end;
{$endif}


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
                 if token=_NEW then consume(_NEW)
                   else consume(_DISPOSE);
                 if ht=_NEW then
                   tt:=hnewn
                 else
                   tt:=hdisposen;
                 consume(LKLAMMER);
                 p:=comp_expr(true);

                 { calc return type }
                 cleartempgen;
                 Store_valid := Must_be_valid;
                 Must_be_valid := False;
                 do_firstpass(p);
                 Must_be_valid := Store_valid;

         {var o:Pobject;

                  begin
                      new(o,init);        (*Also a valid new statement*)
                  end;}

                 if token=COMMA then
                   begin
                          { extended syntax of new and dispose }
                          { function styled new is handled in factor }
                          consume(COMMA);
                          { destructors have no parameters }
                          destrukname:=pattern;
                          consume(ID);

                          pd:=p^.resulttype;
                          pd2:=pd;
                          if (p^.resulttype = nil) or (pd^.deftype<>pointerdef) then
                            begin
                               Message(parser_e_pointer_type_expected);
                               p:=factor(false);
                               consume(RKLAMMER);
                               new_dispose_statement:=genzeronode(errorn);
                               exit;
                            end;
                          { first parameter must be an object or class }
                          if ppointerdef(pd)^.definition^.deftype<>objectdef then
                            begin
                               Message(parser_e_pointer_to_class_expected);
                               new_dispose_statement:=factor(false);
                               consume_all_until(RKLAMMER);
                               consume(RKLAMMER);
                               exit;
                            end;
                          { check, if the first parameter is a pointer to a _class_ }
                          classh:=pobjectdef(ppointerdef(pd)^.definition);
                          if (classh^.options and oois_class)<>0 then
                                begin
                                   Message(parser_e_no_new_or_dispose_for_classes);
                                   new_dispose_statement:=factor(false);
                                   { while token<>RKLAMMER do
                                         consume(token); }
                                   consume_all_until(RKLAMMER);
                                   consume(RKLAMMER);
                                   exit;
                                end;
                          { search cons-/destructor, also in parent classes }
                          sym:=nil;
                          while assigned(classh) do
                                begin
                                   sym:=classh^.publicsyms^.search(pattern);
                                   srsymtable:=classh^.publicsyms;
                                   if assigned(sym) then
                                         break;
                                   classh:=classh^.childof;
                                end;
                          { the second parameter of new/dispose must be a call }
                          { to a cons-/destructor                                }
                          if (sym^.typ<>procsym) then
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
                                           do_member_read(sym,p2,pd,again);
                                        end
                                  else
                                    { destructors can't.}
                                    p2:=genmethodcallnode(pprocsym(sym),srsymtable,p2);

                                  { we need the real called method }
                                  cleartempgen;
                                  do_firstpass(p2);

                                  if (ht=_NEW) and ((p2^.procdefinition^.options and poconstructor)=0) then
                                         Message(parser_e_expr_have_to_be_constructor_call);
                                  if (ht=_DISPOSE) and ((p2^.procdefinition^.options and podestructor)=0) then
                                         Message(parser_e_expr_have_to_be_destructor_call);

                                  if ht=_NEW then
                                        begin
                                                p2:=gennode(assignn,getcopy(p),gensinglenode(newn,p2));
                                                p2^.right^.resulttype:=pd2;
                                        end;
                                  new_dispose_statement:=p2;
                                end;
                   end
                 else
                   begin
                      if (p^.resulttype=nil) or (p^.resulttype^.deftype<>pointerdef) then
                        Begin
                           Message(parser_e_pointer_type_expected);
                           new_dispose_statement:=genzeronode(errorn);
                        end
                      else
                        begin
                           if (ppointerdef(p^.resulttype)^.definition^.deftype=objectdef) then
                            Message(parser_w_use_extended_syntax_for_objects);

                            case ht of
                               _NEW : new_dispose_statement:=gensinglenode(simplenewn,p);
                               _DISPOSE : new_dispose_statement:=gensinglenode(simpledisposen,p);
                            end;
                        end;
                   end;
                 consume(RKLAMMER);
          end;

    function statement_block : ptree;

      var
         first,last : ptree;
         filepos : tfileposinfo;

      begin
         first:=nil;
         filepos:=tokenpos;
         consume(_BEGIN);
         inc(statement_level);

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
              if token=_END then
                break
              else
                begin
                   { if no semicolon, then error and go on }
                   if token<>SEMICOLON then
                     begin
                        consume(SEMICOLON);
                        { while token<>SEMICOLON do
                          consume(token); }
                        consume_all_until(SEMICOLON);
                     end;
                   consume(SEMICOLON);
                end;
              emptystats;
           end;
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
         labelnr : plabel;
         filepos : tfileposinfo;

      label
         ready;

      begin
         filepos:=tokenpos;
         case token of
            _GOTO : begin
                       if not(cs_support_goto in aktswitches)then
                        Message(sym_e_goto_and_label_not_supported);
                       consume(_GOTO);
                       if (token<>INTCONST) and (token<>ID) then
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
                                plabelsym(srsym)^.number);
                         end;
                    end;
            _BEGIN : code:=statement_block;
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
            SEMICOLON,
            _ELSE,
            _UNTIL,
            _END : code:=genzeronode(niln);
            _CONTINUE : begin
                           consume(_CONTINUE);
                           code:=genzeronode(continuen);
                        end;
            _FAIL : begin
                       { internalerror(100); }
                       if (aktprocsym^.definition^.options and poconstructor)=0 then
                        Message(parser_e_fail_only_in_constructor);
                       consume(_FAIL);
                       code:=genzeronode(failn);
                    end;
            {
            _BREAK:
              begin
                 consume(_BREAK);
                 code:=genzeronode(breakn);
              end;
             }
            _EXIT : code:=exit_statement;
            _ASM : begin
                      code:=_asm_statement;
                   end;
         else
           begin
              if (token=INTCONST) or
                ((token=ID) and
                not((cs_delphi2_compatible in aktswitches) and
                (pattern='RESULT'))) then
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
                        consume(COLON);
                        if plabelsym(srsym)^.defined then
                          Message(sym_e_label_already_defined);
                        plabelsym(srsym)^.defined:=true;

                        { statement modifies srsym }
                        labelnr:=plabelsym(srsym)^.number;
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
         set_tree_filepos(code,filepos);
         statement:=code;
      end;

    function block(islibrary : boolean) : ptree;

{$ifdef TEST_FUNCRET }
      var
         funcretsym : pfuncretsym;
{$endif TEST_FUNCRET }

      begin
{$ifdef TEST_FUNCRET }
         if procinfo.retdef<>pdef(voiddef) then
           begin
              { if the current is a function aktprocsym is non nil }
              { and there is a local symtable set }
              funcretsym:=new(pfuncretsym,init(aktprocsym^.name),@procinfo);
              procinfo.retoffset:=-funcretsym^.address;
              { insert in local symtable }
              symtablestack^.insert(funcretsym);
           end;
{$endif TEST_FUNCRET }
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
{$ifdef TEST_FUNCRET }
                   { the space has been set in the local symtable }
                   procinfo.retoffset:=-funcretsym^.address;
                   strdispose(funcretsym^._name);
                   { lowercase name unreachable }
                   { as it is handled differently }
                   funcretsym^._name:=strpnew('func_result');
{$else  TEST_FUNCRET }
                   { align func result at 4 byte }
                   procinfo.retoffset:=
                     -((-procinfo.firsttemp+(procinfo.retdef^.size+3)) div 4)*4;
                   procinfo.firsttemp:=procinfo.retoffset;
{$endif TEST_FUNCRET }
                   if (procinfo.flags and pi_operator)<>0 then
                     {opsym^.address:=procinfo.call_offset; is wrong PM }
                     opsym^.address:=-procinfo.retoffset;
                   { eax is modified by a function }
{$ifdef i386}
                   usedinproc:=usedinproc or ($80 shr byte(R_EAX))
{$endif}
{$ifdef m68k}
                   usedinproc:=usedinproc or ($800 shr word(R_D0))
{$endif}
                end;
           end;

         {Unit initialization?.}
         if (lexlevel=1) then
            if (token=_END) then
                begin
                    consume(_END);
                    block:=nil;
                end
            else
                begin
                    current_module^.flags:=current_module^.flags or uf_init;
                    block:=statement_block;
                end
         else
            block:=statement_block;
      end;

    function assembler_block : ptree;

      begin
         read_declarations(false);
         { temporary space is set, while the BEGIN of the procedure }
         if symtablestack^.symtabletype=localsymtable then
           procinfo.firsttemp := -symtablestack^.datasize
         else procinfo.firsttemp := 0;

         { assembler code does not allocate }
         { space for the return value       }
          if procinfo.retdef<>pdef(voiddef) then
           begin
              if ret_in_acc(procinfo.retdef) then
                begin
                   { in assembler code the result should be directly in %eax
                   procinfo.retoffset:=procinfo.firsttemp-procinfo.retdef^.size;
                   procinfo.firsttemp:=procinfo.retoffset;                   }

{$ifdef i386}
                   usedinproc:=usedinproc or ($80 shr byte(R_EAX))
{$endif}
{$ifdef m68k}
                   usedinproc:=usedinproc or ($800 shr word(R_D0))
{$endif}
                end
              else if not is_fpu(procinfo.retdef) then
              { should we allow assembler functions of big elements ? }
               Message(parser_e_asm_incomp_with_function_return);
           end;
           { set the framepointer to esp for assembler functions }
           { but only if the are no local variables              }
           { added no parameter also (PM)                        }
           if ((aktprocsym^.definition^.options and poassembler)<>0) and
               (aktprocsym^.definition^.localst^.datasize=0) and
               (aktprocsym^.definition^.parast^.datasize=0) then
               begin
{$ifdef i386}
                  procinfo.framepointer:=R_ESP;
{$endif}
{$ifdef m68k}
                  procinfo.framepointer:=R_SP;
{$endif}
                  { set the right value for parameters }
                  dec(aktprocsym^.definition^.parast^.call_offset,sizeof(pointer));
                  dec(procinfo.call_offset,sizeof(pointer));
              end;
            assembler_block:=_asm_statement;
          { becuase the END is already read we need to get the
            last_endtoken_filepos here (PFV) }
            last_endtoken_filepos:=tokenpos;
          end;

end.
{
  $Log$
  Revision 1.18  1998-06-05 14:37:35  pierre
    * fixes for inline for operators
    * inline procedure more correctly restricted

  Revision 1.17  1998/06/04 09:55:43  pierre
    * demangled name of procsym reworked to become independant of the mangling scheme

  Come test_funcret improvements (not yet working)S: ----------------------------------------------------------------------

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

