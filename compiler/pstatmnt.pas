{
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
       cutils,cclasses,
       { global }
       globtype,globals,verbose,
       systems,
       { aasm }
       cpubase,aasmbase,aasmtai,
       { symtable }
       symconst,symbase,symtype,symdef,symsym,symtable,defutil,defcmp,
       paramgr,symutil,
       { pass 1 }
       pass_1,htypechk,
       nutils,nbas,nmat,nadd,ncal,nmem,nset,ncnv,ninl,ncon,nld,nflw,
       { parser }
       scanner,
       pbase,pexpr,
       { codegen }
       procinfo,cgbase,
       { assembler reader }
       rabase
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
         result:=cifnode.create(ex,if_a,else_a);
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
                   last:=cstatementnode.create(statement,nil);
                   first:=last;
                end
              else
                begin
                   last.right:=cstatementnode.create(statement,nil);
                   last:=tstatementnode(last.right);
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
         casedef : tdef;
         caseexpr,p : tnode;
         blockid : longint;
         hl1,hl2 : TConstExprInt;
         casedeferror : boolean;
         casenode : tcasenode;
      begin
         consume(_CASE);
         caseexpr:=comp_expr(true);
         { determines result type }
         do_resulttypepass(caseexpr);
         set_varstate(caseexpr,vs_read,[vsf_must_be_valid]);
         casedeferror:=false;
         casedef:=caseexpr.resulttype.def;
         if (not assigned(casedef)) or
            not(is_ordinal(casedef)) then
          begin
            CGMessage(type_e_ordinal_expr_expected);
            { create a correct tree }
            caseexpr.free;
            caseexpr:=cordconstnode.create(0,u32inttype,false);
            { set error flag so no rangechecks are done }
            casedeferror:=true;
          end;
         { Create casenode }
         casenode:=ccasenode.create(caseexpr);
         consume(_OF);
         { Parse all case blocks }
         blockid:=0;
         repeat
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
                  casenode.addlabel(blockid,hl1,hl2);
               end
             else
               begin
                  { type checking for case statements }
                  if not is_subequal(casedef, p.resulttype.def) then
                    CGMessage(parser_e_case_mismatch);
                  hl1:=get_ordinal_value(p);
                  if not casedeferror then
                    testrange(casedef,hl1,false);
                  casenode.addlabel(blockid,hl1,hl1);
               end;
             p.free;
             if token=_COMMA then
               consume(_COMMA)
             else
               break;
           until false;
           consume(_COLON);

           { add instruction block }
           casenode.addblock(blockid,statement);

           { next block }
           inc(blockid);

           if not(token in [_ELSE,_OTHERWISE,_END]) then
             consume(_SEMICOLON);
         until (token in [_ELSE,_OTHERWISE,_END]);

         if (token in [_ELSE,_OTHERWISE]) then
           begin
              if not try_to_consume(_ELSE) then
                consume(_OTHERWISE);
              casenode.addelseblock(statements_til_end);
           end
         else
           consume(_END);

         result:=casenode;
      end;


    function repeat_statement : tnode;

      var
         first,last,p_e : tnode;

      begin
         consume(_REPEAT);
         first:=nil;

         while token<>_UNTIL do
           begin
              if first=nil then
                begin
                   last:=cstatementnode.create(statement,nil);
                   first:=last;
                end
              else
                begin
                   tstatementnode(last).right:=cstatementnode.create(statement,nil);
                   last:=tstatementnode(last).right;
                end;
              if not try_to_consume(_SEMICOLON) then
                break;
              consume_emptystats;
           end;
         consume(_UNTIL);

         first:=cblocknode.create(first);
         p_e:=comp_expr(true);
         result:=cwhilerepeatnode.create(p_e,first,false,true);
      end;


    function while_statement : tnode;

      var
         p_e,p_a : tnode;

      begin
         consume(_WHILE);
         p_e:=comp_expr(true);
         consume(_DO);
         p_a:=statement;
         result:=cwhilerepeatnode.create(p_e,p_a,true,false);
      end;


    function for_statement : tnode;

        procedure check_range(hp:tnode);
        begin
{$ifndef cpu64bit}
          if hp.nodetype=ordconstn then
            begin
              if (tordconstnode(hp).value<low(longint)) or
                 (tordconstnode(hp).value>high(longint)) then
                begin
                  CGMessage(parser_e_range_check_error);
                  { recover, prevent more warnings/errors }
                  tordconstnode(hp).value:=0;
                end;
            end;
{$endif cpu64bit}
        end;

      var
         hp,
         hloopvar,
         hblock,
         hto,hfrom : tnode;
         backward : boolean;
         loopvarsym : tabstractvarsym;
      begin
         { parse loop header }
         consume(_FOR);

         hloopvar:=factor(false);
         valid_for_assignment(hloopvar,true);

         { Check loop variable }
         loopvarsym:=nil;

         { variable must be an ordinal, int64 is not allowed for 32bit targets }
         if not(is_ordinal(hloopvar.resulttype.def))
{$ifndef cpu64bit}
            or is_64bitint(hloopvar.resulttype.def)
{$endif cpu64bit}
            then
           MessagePos(hloopvar.fileinfo,type_e_ordinal_expr_expected);

         hp:=hloopvar;
         while assigned(hp) and
               (
                { record/object fields are allowed in tp7 mode only }
                (
                 (m_tp7 in aktmodeswitches) and
                 (hp.nodetype=subscriptn) and
                 ((tsubscriptnode(hp).left.resulttype.def.deftype=recorddef) or
                  is_object(tsubscriptnode(hp).left.resulttype.def))
                ) or
                { constant array index }
                (
                 (hp.nodetype=vecn) and
                 is_constintnode(tvecnode(hp).right)
                ) or
                { equal typeconversions }
                (
                 (hp.nodetype=typeconvn) and
                 (ttypeconvnode(hp).convtype=tc_equal)
                )
               ) do
           begin
             { Use the recordfield for loopvarsym }
             if not assigned(loopvarsym) and
                (hp.nodetype=subscriptn) then
               loopvarsym:=tsubscriptnode(hp).vs;
             hp:=tunarynode(hp).left;
           end;

         if assigned(hp) and
            (hp.nodetype=loadn) then
           begin
             case tloadnode(hp).symtableentry.typ of
               globalvarsym,
               localvarsym,
               paravarsym :
                 begin
                   { we need a simple loadn and the load must be in a global symtable or
                     in the same level as the para of the current proc }
                   if (
                       (tloadnode(hp).symtable.symtablelevel=main_program_level) or
                       (tloadnode(hp).symtable.symtablelevel=current_procinfo.procdef.parast.symtablelevel)
                      ) and
                      not(
                          ((tabstractvarsym(tloadnode(hp).symtableentry).varspez in [vs_var,vs_out]) or
                           (vo_is_thread_var in tabstractvarsym(tloadnode(hp).symtableentry).varoptions))
                         ) then
                     begin
                       { Assigning for-loop variable is only allowed in tp7 }
                       if not(m_tp7 in aktmodeswitches) then
                         begin
                           if not assigned(loopvarsym) then
                             loopvarsym:=tabstractvarsym(tloadnode(hp).symtableentry);
                           include(loopvarsym.varoptions,vo_is_loop_counter);
                         end;
                     end
                   else
                     MessagePos(hp.fileinfo,type_e_illegal_count_var);
                 end;
               typedconstsym :
                 begin
                   { Bad programming, only allowed in tp7 mode }
                   if not(m_tp7 in aktmodeswitches) then
                     MessagePos(hp.fileinfo,type_e_illegal_count_var);
                 end;
               else
                 MessagePos(hp.fileinfo,type_e_illegal_count_var);
             end;
           end
         else
           MessagePos(hloopvar.fileinfo,type_e_illegal_count_var);

         consume(_ASSIGNMENT);

         hfrom:=comp_expr(true);

         if try_to_consume(_DOWNTO) then
           backward:=true
         else
           begin
             consume(_TO);
             backward:=false;
           end;

         hto:=comp_expr(true);
         consume(_DO);

         { Check if the constants fit in the range }
         check_range(hfrom);
         check_range(hto);

         { first set the varstate for from and to, so
           uses of loopvar in those expressions will also
           trigger a warning when it is not used yet. This
           needs to be done before the instruction block is
           parsed to have a valid hloopvar }
         resulttypepass(hfrom);
         set_varstate(hfrom,vs_read,[vsf_must_be_valid]);
         resulttypepass(hto);
         set_varstate(hto,vs_read,[vsf_must_be_valid]);
         resulttypepass(hloopvar);
         set_varstate(hloopvar,vs_readwritten,[]);

         { ... now the instruction block }
         hblock:=statement;

         { variable is not used for loop counter anymore }
         if assigned(loopvarsym) then
           exclude(loopvarsym.varoptions,vo_is_loop_counter);

         result:=cfornode.create(hloopvar,hfrom,hto,hblock,backward);
      end;


    function _with_statement : tnode;

      var
         p   : tnode;
         i   : longint;
         st  : tsymtable;
         newblock : tblocknode;
         newstatement : tstatementnode;
         calltempnode,
         tempnode : ttempcreatenode;
         valuenode,
         hp,
         refnode  : tnode;
         htype : ttype;
         hasimplicitderef : boolean;
         withsymtablelist : TFPObjectList;

         procedure pushobjchild(obj:tobjectdef);
         begin
           if not assigned(obj) then
             exit;
           pushobjchild(obj.childof);
           { keep the original tobjectdef as owner, because that is used for
             visibility of the symtable }
           st:=twithsymtable.create(tobjectdef(p.resulttype.def),obj.symtable.symsearch,refnode.getcopy);
           symtablestack.push(st);
           withsymtablelist.add(st);
         end;

      begin
         p:=comp_expr(true);
         do_resulttypepass(p);

         if (p.nodetype=vecn) and
            (nf_memseg in p.flags) then
           CGMessage(parser_e_no_with_for_variable_in_other_segments);

         if (p.resulttype.def.deftype in [objectdef,recorddef]) then
          begin
            newblock:=nil;
            valuenode:=nil;
            tempnode:=nil;

            { ignore nodes that don't add instructions in the tree }
            hp:=p;
            while { equal type conversions }
                  (
                   (hp.nodetype=typeconvn) and
                   (ttypeconvnode(hp).convtype=tc_equal)
                  ) or
                  { constant array index }
                  (
                   (hp.nodetype=vecn) and
                   (tvecnode(hp).right.nodetype=ordconstn)
                  ) do
              hp:=tunarynode(hp).left;
            if (hp.nodetype=loadn) and
               (
                (tloadnode(hp).symtable=current_procinfo.procdef.localst) or
                (tloadnode(hp).symtable=current_procinfo.procdef.parast) or
                (tloadnode(hp).symtable.symtabletype in [staticsymtable,globalsymtable])
               ) then
              begin
                { simple load, we can reference direct }
                refnode:=p;
              end
            else
              begin
                calltempnode:=nil;
                { complex load, load in temp first }
                newblock:=internalstatements(newstatement);
                { when right is a call then load it first in a temp }
                if p.nodetype=calln then
                  begin
                    calltempnode:=ctempcreatenode.create(p.resulttype,p.resulttype.def.size,tt_persistent,false);
                    addstatement(newstatement,calltempnode);
                    addstatement(newstatement,cassignmentnode.create(
                        ctemprefnode.create(calltempnode),
                        p));
                    p:=ctemprefnode.create(calltempnode);
                    resulttypepass(p);
                  end;
                { classes and interfaces have implicit dereferencing }
                hasimplicitderef:=is_class_or_interface(p.resulttype.def);
                if hasimplicitderef then
                  htype:=p.resulttype
                else
                  htype.setdef(tpointerdef.create(p.resulttype));
                { load address of the value in a temp }
                tempnode:=ctempcreatenode.create(htype,sizeof(aint),tt_persistent,true);
                resulttypepass(tempnode);
                valuenode:=p;
                refnode:=ctemprefnode.create(tempnode);
                fillchar(refnode.fileinfo,sizeof(tfileposinfo),0);
                { add address call for valuenode and deref for refnode if this
                  is not done implicitly }
                if not hasimplicitderef then
                  begin
                    valuenode:=caddrnode.create_internal(valuenode);
                    refnode:=cderefnode.create(refnode);
                    fillchar(refnode.fileinfo,sizeof(tfileposinfo),0);
                  end;
                addstatement(newstatement,tempnode);
                addstatement(newstatement,cassignmentnode.create(
                    ctemprefnode.create(tempnode),
                    valuenode));
                resulttypepass(refnode);
              end;

            withsymtablelist:=TFPObjectList.create(true);
            case p.resulttype.def.deftype of
              objectdef :
                begin
                   { push symtables of all parents in reverse order }
                   pushobjchild(tobjectdef(p.resulttype.def).childof);
                   { push object symtable }
                   st:=twithsymtable.Create(tobjectdef(p.resulttype.def),tobjectdef(p.resulttype.def).symtable.symsearch,refnode);
                   symtablestack.push(st);
                   withsymtablelist.add(st);
                 end;
              recorddef :
                begin
                   st:=twithsymtable.create(trecorddef(p.resulttype.def),trecorddef(p.resulttype.def).symtable.symsearch,refnode);
                   symtablestack.push(st);
                   withsymtablelist.add(st);
                end;
              else
                internalerror(200601271);
            end;

            if try_to_consume(_COMMA) then
              p:=_with_statement()
            else
              begin
                consume(_DO);
                if token<>_SEMICOLON then
                  p:=statement
                else
                  p:=cerrornode.create;
              end;

            { remove symtables in reverse order from the stack }
            for i:=withsymtablelist.count-1 downto 0 do
              symtablestack.pop(tsymtable(withsymtablelist[i]));
            withsymtablelist.free;

//            p:=cwithnode.create(right,twithsymtable(withsymtable),levelcount,refnode);

            { Finalize complex withnode with destroy of temp }
            if assigned(newblock) then
             begin
               addstatement(newstatement,p);
               if assigned(tempnode) then
                 addstatement(newstatement,ctempdeletenode.create(tempnode));
               if assigned(calltempnode) then
                 addstatement(newstatement,ctempdeletenode.create(calltempnode));
               p:=newblock;
             end;
            result:=p;
          end
         else
          begin
            p.free;
            Message(parser_e_false_with_expr);
            { try to recover from error }
            if try_to_consume(_COMMA) then
             begin
               hp:=_with_statement();
               if (hp=nil) then; { remove warning about unused }
             end
            else
             begin
               consume(_DO);
               { ignore all }
               if token<>_SEMICOLON then
                statement;
             end;
            result:=nil;
          end;
      end;


    function with_statement : tnode;
      begin
         consume(_WITH);
         with_statement:=_with_statement();
      end;


    function raise_statement : tnode;

      var
         p,pobj,paddr,pframe : tnode;

      begin
         pobj:=nil;
         paddr:=nil;
         pframe:=nil;
         consume(_RAISE);
         if not(token in endtokens) then
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
         sym : tlocalvarsym;
         old_block_type : tblock_type;
         exceptsymtable : tsymtable;
         objname,objrealname : stringid;
         srsym : tsym;
         srsymtable : tsymtable;
         oldaktexceptblock: integer;

      begin
         include(current_procinfo.flags,pi_uses_exceptions);

         p_default:=nil;
         p_specific:=nil;

         { read statements to try }
         consume(_TRY);
         first:=nil;
         inc(exceptblockcounter);
         oldaktexceptblock := aktexceptblock;
         aktexceptblock := exceptblockcounter;

         while (token<>_FINALLY) and (token<>_EXCEPT) do
           begin
              if first=nil then
                begin
                   last:=cstatementnode.create(statement,nil);
                   first:=last;
                end
              else
                begin
                   tstatementnode(last).right:=cstatementnode.create(statement,nil);
                   last:=tstatementnode(last).right;
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
                     consume(_ON);
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
                                    sym:=tlocalvarsym.create(objrealname,vs_value,ot,[]);
                                 end
                               else
                                 begin
                                    sym:=tlocalvarsym.create(objrealname,vs_value,generrortype,[]);
                                    if (srsym.typ=typesym) then
                                      Message1(type_e_class_type_expected,ttypesym(srsym).restype.def.typename)
                                    else
                                      Message1(type_e_class_type_expected,ot.def.typename);
                                 end;
                               exceptsymtable:=tstt_exceptsymtable.create;
                               exceptsymtable.insert(sym);
                               symtablestack.push(exceptsymtable);
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
                                    searchsym_in_module(tunitsym(srsym).module,pattern,srsym,srsymtable);
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
                         symtablestack.pop(exceptsymtable);
                         if last.nodetype <> onn then
                           exceptsymtable.free;
                       end;
                     if not try_to_consume(_SEMICOLON) then
                        break;
                     consume_emptystats;
                   until (token in [_END,_ELSE]);
                   if try_to_consume(_ELSE) then
                     begin
                       { catch the other exceptions }
                       p_default:=statements_til_end;
                     end
                   else
                     consume(_END);
                end
              else
                begin
                   { catch all exceptions }
                   p_default:=statements_til_end;
                end;

              block_type:=old_block_type;
              try_statement:=ctryexceptnode.create(p_try_block,p_specific,p_default);
           end;
         aktexceptblock := oldaktexceptblock;
      end;


    function _asm_statement : tnode;
      var
        asmstat : tasmnode;
        Marker  : tai;
        reg     : tregister;
        asmreader : tbaseasmreader;
      begin
         Inside_asm_statement:=true;
         if assigned(asmmodeinfos[aktasmmode]) then
           begin
             asmreader:=asmmodeinfos[aktasmmode]^.casmreader.create;
             asmstat:=casmnode.create(asmreader.assemble as taasmoutput);
             asmreader.free;
           end
         else
           Message(parser_f_assembler_reader_not_supported);

         { Mark procedure that it has assembler blocks }
         include(current_procinfo.flags,pi_has_assembler_block);

         { Read first the _ASM statement }
         consume(_ASM);

         { END is read, got a list of changed registers? }
         if try_to_consume(_LECKKLAMMER) then
           begin
             asmstat.used_regs_fpu:=[0..first_fpu_imreg-1];
             if token<>_RECKKLAMMER then
              begin
                repeat
                  { it's possible to specify the modified registers }
                  reg:=std_regnum_search(lower(pattern));
                  if reg<>NR_NO then
                    begin
                      if getregtype(reg)=R_INTREGISTER then
                        include(asmstat.used_regs_int,getsupreg(reg));
                    end
                  else
                    Message(asmr_e_invalid_register);
                  consume(_CSTRING);
                  if not try_to_consume(_COMMA) then
                    break;
                until false;
              end;
             consume(_RECKKLAMMER);
           end
         else
           begin
              asmstat.used_regs_int:=paramanager.get_volatile_registers_int(current_procinfo.procdef.proccalloption);
              asmstat.used_regs_fpu:=paramanager.get_volatile_registers_fpu(current_procinfo.procdef.proccalloption);
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
                         { goto is only allowed to labels within the current scope }
                         if srsym.owner<>current_procinfo.procdef.localst then
                           CGMessage(parser_e_goto_outside_proc);
                         code:=cgotonode.create_sym(tlabelsym(srsym));
                         tgotonode(code).labelsym:=tlabelsym(srsym);
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
                if (current_procinfo.procdef.proctypeoption<>potype_constructor) then
                  Message(parser_e_fail_only_in_constructor);
                consume(_FAIL);
                code:=call_fail_node;
             end;
           _ASM :
             code:=_asm_statement;
           _EOF :
             Message(scan_f_end_of_file);
         else
           begin
             p:=expr;
             { save the pattern here for latter usage, the label could be "000",
               even if we read an expression, the pattern is still valid if it's really
               a label (FK)
               if you want to mess here, take care of
               tests/webtbs/tw3546.pp
             }
             s:=pattern;

             { When a colon follows a intconst then transform it into a label }
             if (p.nodetype=ordconstn) and
                try_to_consume(_COLON) then
              begin
                p.free;
                searchsym(s,srsym,srsymtable);
                if assigned(srsym) and
                   (srsym.typ=labelsym) then
                 begin
                   if tlabelsym(srsym).defined then
                    Message(sym_e_label_already_defined);
                   tlabelsym(srsym).defined:=true;
                   p:=clabelnode.create(nil);
                   tlabelsym(srsym).code:=p;
                 end
                else
                 begin
                   Message1(sym_e_label_used_and_not_defined,s);
                   p:=cnothingnode.create;
                 end;
              end;

             if p.nodetype=labeln then
               begin
                 { the pointer to the following instruction }
                 { isn't a very clean way                   }
                 if token in endtokens then
                   tlabelnode(p).left:=cnothingnode.create
                 else
                   tlabelnode(p).left:=statement();
                 { be sure to have left also resulttypepass }
                 resulttypepass(tlabelnode(p).left);
               end
             else

             { change a load of a procvar to a call. this is also
               supported in fpc mode }
             if p.nodetype in [vecn,derefn,typeconvn,subscriptn,loadn] then
               maybe_call_procvar(p,false);

             { blockn support because a read/write is changed into a blocknode }
             { with a separate statement for each read/write operation (JM)    }
             { the same is true for val() if the third parameter is not 32 bit }
             if not(p.nodetype in [nothingn,calln,ifn,assignn,breakn,inlinen,
                                   continuen,labeln,blockn,exitn]) then
               Message(parser_e_illegal_expression);

             { Specify that we don't use the value returned by the call.
               This is used for :
                - dispose of temp stack space
                - dispose on FPU stack }
             if (p.nodetype=calln) then
               exclude(tcallnode(p).callnodeflags,cnf_return_value_used);

             code:=p;
           end;
         end;
         if assigned(code) then
           begin
             resulttypepass(code);
             code.fileinfo:=filepos;
           end;
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

         while not(token in [_END,_FINALIZATION]) do
           begin
              if first=nil then
                begin
                   last:=cstatementnode.create(statement,nil);
                   first:=last;
                end
              else
                begin
                   tstatementnode(last).right:=cstatementnode.create(statement,nil);
                   last:=tstatementnode(last).right;
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

         last:=cblocknode.create(first);
         last.fileinfo:=filepos;
         statement_block:=last;
      end;


    function assembler_block : tnode;
      var
        p : tnode;
        locals : longint;
      begin
         { Rename the funcret so that recursive calls are possible }
         if not is_void(current_procinfo.procdef.rettype.def) then
           current_procinfo.procdef.localst.rename(current_procinfo.procdef.resultname,'$hiddenresult');

         { delphi uses register calling for assembler methods }
         if (m_delphi in aktmodeswitches) and
            (po_assembler in current_procinfo.procdef.procoptions) and
            not(po_hascallingconvention in current_procinfo.procdef.procoptions) then
           current_procinfo.procdef.proccalloption:=pocall_register;

         { force the asm statement }
         if token<>_ASM then
           consume(_ASM);
         include(current_procinfo.flags,pi_is_assembler);
         p:=_asm_statement;

{$ifndef sparc}
{$ifndef arm}
         if (po_assembler in current_procinfo.procdef.procoptions) then
           begin
             { set the framepointer to esp for assembler functions when the
               following conditions are met:
               - if the are no local variables and parameters (except the allocated result)
               - no reference to the result variable (refcount<=1)
               - result is not stored as parameter
               - target processor has optional frame pointer save
                 (vm, i386, vm only currently)
             }
             locals:=0;
             current_procinfo.procdef.localst.foreach_static(@count_locals,@locals);
             current_procinfo.procdef.parast.foreach_static(@count_locals,@locals);
             if (locals=0) and
                (current_procinfo.procdef.owner.symtabletype<>objectsymtable) and
                (not assigned(current_procinfo.procdef.funcretsym) or
                 (tabstractvarsym(current_procinfo.procdef.funcretsym).refcount<=1)) and
                not(paramanager.ret_in_param(current_procinfo.procdef.rettype.def,current_procinfo.procdef.proccalloption)) then
               begin
                 { Only need to set the framepointer, the locals will
                   be inserted with the correct reference in tcgasmnode.pass_2 }
                 current_procinfo.framepointer:=NR_STACK_POINTER_REG;
               end;
           end;
{$endif arm}
{$endif sparc}

        { Flag the result as assigned when it is returned in a
          register.
        }
        if assigned(current_procinfo.procdef.funcretsym) and
           (not paramanager.ret_in_param(current_procinfo.procdef.rettype.def,current_procinfo.procdef.proccalloption)) then
          tabstractvarsym(current_procinfo.procdef.funcretsym).varstate:=vs_initialised;

        { because the END is already read we need to get the
          last_endtoken_filepos here (PFV) }
        last_endtoken_filepos:=akttokenpos;

        assembler_block:=p;
      end;

end.
