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
       cutils,cclasses,
       { global }
       globtype,globals,verbose,
       systems,cpuinfo,
       { aasm }
       cpubase,aasmbase,aasmtai,
       { symtable }
       symconst,symbase,symtype,symdef,symsym,symtable,defutil,defcmp,
       paramgr,
       { pass 1 }
       pass_1,htypechk,
       nutils,nbas,nmat,nadd,ncal,nmem,nset,ncnv,ninl,ncon,nld,nflw,
       { parser }
       scanner,
       pbase,pexpr,
       { codegen }
       procinfo,rgobj,cgbase
       ,radirect
{$ifdef i386}
  {$ifndef NoRa386Int}
       ,ra386int
  {$endif NoRa386Int}
  {$ifndef NoRa386Att}
       ,ra386att
  {$endif NoRa386Att}
{$else}
       ,rasm
{$endif i386}
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
         statements_til_end:=cblocknode.create(first,true);
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
           objectlibrary.getlabel(hcaselabel^._at);
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
         do_resulttypepass(caseexpr);
         set_varstate(caseexpr,vs_used,true);
         casedeferror:=false;
         casedef:=caseexpr.resulttype.def;
         if (not assigned(casedef)) or
            not(is_ordinal(casedef)) then
          begin
            CGMessage(type_e_ordinal_expr_expected);
            { create a correct tree }
            caseexpr.free;
            caseexpr:=cordconstnode.create(0,u32bittype,false);
            { set error flag so no rangechecks are done }
            casedeferror:=true;
          end;

         consume(_OF);
         root:=nil;
         instruc:=nil;
         repeat
           objectlibrary.getlabel(aktcaselabel);
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
           instruc:=cstatementnode.create(p,instruc);

           if not(token in [_ELSE,_OTHERWISE,_END]) then
             consume(_SEMICOLON);
         until (token in [_ELSE,_OTHERWISE,_END]);

         if (token in [_ELSE,_OTHERWISE]) then
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

         first:=cblocknode.create(first,true);
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
         newblock : tblocknode;
         newstatement : tstatementnode;
         calltempp,
         loadp : ttempcreatenode;
         refp : tnode;
         htype : ttype;
         hasimplicitderef : boolean;
      begin
         p:=comp_expr(true);
         do_resulttypepass(p);
         set_varstate(p,vs_used,false);
         right:=nil;
         if (not codegenerror) and
            (p.resulttype.def.deftype in [objectdef,recorddef]) then
          begin
            newblock:=nil;
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
               loadp:=nil;
               refp:=p;
             end
            else
             begin
               calltempp:=nil;
               { complex load, load in temp first }
               newblock:=internalstatements(newstatement,false);
               { when right is a call then load it first in a temp }
               if p.nodetype=calln then
                 begin
                   calltempp:=ctempcreatenode.create(p.resulttype,p.resulttype.def.size,tt_persistent);
                   addstatement(newstatement,calltempp);
                   addstatement(newstatement,cassignmentnode.create(
                       ctemprefnode.create(calltempp),
                       p));
                   p:=ctemprefnode.create(calltempp);
                   resulttypepass(p);
                 end;
               { classes and interfaces have implicit dereferencing }
               hasimplicitderef:=is_class_or_interface(p.resulttype.def);
               if hasimplicitderef then
                 htype:=p.resulttype
               else
                 htype.setdef(tpointerdef.create(p.resulttype));
               loadp:=ctempcreatenode.create(htype,POINTER_SIZE,tt_persistent);
               resulttypepass(loadp);
               if hasimplicitderef then
                begin
                  hp:=p;
                  refp:=ctemprefnode.create(loadp);
                end
               else
                begin
                  hp:=caddrnode.create(p);
                  refp:=cderefnode.create(ctemprefnode.create(loadp));
                end;
               addstatement(newstatement,loadp);
               addstatement(newstatement,cassignmentnode.create(
                   ctemprefnode.create(loadp),
                   hp));
               resulttypepass(refp);
             end;

            case p.resulttype.def.deftype of
              objectdef :
                begin
                   obj:=tobjectdef(p.resulttype.def);
                   withsymtable:=twithsymtable.Create(obj,obj.symtable.symsearch,refp);
                   { include also all parent symtables }
                   levelcount:=1;
                   obj:=obj.childof;
                   symtab:=withsymtable;
                   while assigned(obj) do
                    begin
                      symtab.next:=twithsymtable.create(obj,obj.symtable.symsearch,refp);
                      symtab:=symtab.next;
                      obj:=obj.childof;
                      inc(levelcount);
                    end;
                   symtab.next:=symtablestack;
                   symtablestack:=withsymtable;
                 end;
              recorddef :
                begin
                   symtab:=trecorddef(p.resulttype.def).symtable;
                   levelcount:=1;
                   withsymtable:=twithsymtable.create(trecorddef(p.resulttype.def),symtab.symsearch,refp);
                   withsymtable.next:=symtablestack;
                   symtablestack:=withsymtable;
                end;
            end;
            if try_to_consume(_COMMA) then
              right:=_with_statement{$ifdef FPCPROCVAR}(){$endif}
            else
              begin
                consume(_DO);
                if token<>_SEMICOLON then
                  right:=statement
                else
                  right:=cerrornode.create;
              end;
            { remove symtables from the stack }
            for i:=1 to levelcount do
              symtablestack:=symtablestack.next;
            p:=cwithnode.create(right,twithsymtable(withsymtable),levelcount,refp);
            { Finalize complex withnode with destroy of temp }
            if assigned(newblock) then
             begin
               addstatement(newstatement,p);
               addstatement(newstatement,ctempdeletenode.create(loadp));
               if assigned(calltempp) then
                 addstatement(newstatement,ctempdeletenode.create(calltempp));
               p:=newblock;
             end;
            _with_statement:=p;
          end
         else
          begin
            Message(parser_e_false_with_expr);
            { try to recover from error }
            if try_to_consume(_COMMA) then
             begin
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
         sym : tvarsym;
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
         p_try_block:=cblocknode.create(first,true);

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
                                    sym:=tvarsym.create(objrealname,vs_value,ot);
                                 end
                               else
                                 begin
                                    sym:=tvarsym.create(objrealname,vs_value,generrortype);
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
                         symtablestack:=symtablestack.next;
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
      begin
         Inside_asm_statement:=true;
         case aktasmmode of
           asmmode_none : ; { just be there to allow to compile a compiler without
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
{$else not i386}
           asmmode_standard:
             asmstat:=tasmnode(rasm.assemble);
{$endif i386}
           asmmode_direct:
             begin
               if not target_asm.allowdirect then
                 Message(parser_f_direct_assembler_not_allowed);
               if (current_procinfo.procdef.proccalloption=pocall_inline) then
                 Begin
                    Message1(parser_w_not_supported_for_inline,'direct asm');
                    Message(parser_w_inlining_disabled);
                    current_procinfo.procdef.proccalloption:=pocall_default;
                 End;
               asmstat:=tasmnode(radirect.assemble);
             end;
         else
           Message(parser_f_assembler_reader_not_supported);
         end;

         { Read first the _ASM statement }
         consume(_ASM);

         { END is read, got a list of changed registers? }
         if try_to_consume(_LECKKLAMMER) then
           begin
             asmstat.used_regs_fpu:=ALL_OTHERREGISTERS;
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
              asmstat.used_regs_fpu:=ALL_OTHERREGISTERS;
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
                   Message1(sym_e_label_used_and_not_defined,s);
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
             if not(p.nodetype in [nothingn,calln,ifn,assignn,breakn,inlinen,
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

         last:=cblocknode.create(first,true);
         last.set_tree_filepos(filepos);
         statement_block:=last;
      end;


    procedure count_locals(p:tnamedindexitem;arg:pointer);
      begin
        { Count only varsyms, but ignore the funcretsym }
        if (tsym(p).typ=varsym) and
           (tsym(p)<>current_procinfo.procdef.funcretsym) then
          inc(plongint(arg)^);
      end;


    function assembler_block : tnode;
      var
        p : tnode;
        locals : longint;
      begin
         { Rename the funcret so that recursive calls are possible }
         if not is_void(current_procinfo.procdef.rettype.def) then
           symtablestack.rename(current_procinfo.procdef.resultname,'$hiddenresult');

         { assembler routines use stdcall instead of register }
{$warning Temporary hack for force stdcall for assembler}
         if (po_assembler in current_procinfo.procdef.procoptions) and
            (current_procinfo.procdef.proccalloption=pocall_register) then
           current_procinfo.procdef.proccalloption:=pocall_stdcall;

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
             current_procinfo.procdef.localst.foreach_static({$ifdef FPCPROCVAR}@{$endif}count_locals,@locals);
             current_procinfo.procdef.parast.foreach_static({$ifdef FPCPROCVAR}@{$endif}count_locals,@locals);
             if (locals=0) and
                (current_procinfo.procdef.owner.symtabletype<>objectsymtable) and
                (not assigned(current_procinfo.procdef.funcretsym) or
                 (tvarsym(current_procinfo.procdef.funcretsym).refcount<=1)) and
                not(paramanager.ret_in_param(current_procinfo.procdef.rettype.def,current_procinfo.procdef.proccalloption)) then
               begin
                 { Only need to set the framepointer, the locals will
                   be inserted with the correct reference in tcgasmnode.pass_2 }
                 current_procinfo.framepointer:=NR_STACK_POINTER_REG;
               end;
           end;
{$endif sparc}

        { Flag the result as assigned when it is returned in a
          register.
        }
        if assigned(current_procinfo.procdef.funcretsym) and
           (not paramanager.ret_in_param(current_procinfo.procdef.rettype.def,current_procinfo.procdef.proccalloption)) then
          tvarsym(current_procinfo.procdef.funcretsym).varstate:=vs_assigned;

        { because the END is already read we need to get the
          last_endtoken_filepos here (PFV) }
        last_endtoken_filepos:=akttokenpos;

        assembler_block:=p;
      end;

end.
{
  $Log$
  Revision 1.114  2003-10-08 19:19:45  peter
    * set_varstate cleanup

  Revision 1.113  2003/10/07 20:06:37  peter
    * set calling convention before assembler block is parsed

  Revision 1.112  2003/10/02 21:15:59  peter
    * delphi mode uses register calling by default for assembler

  Revision 1.111  2003/10/01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.110  2003/09/23 17:56:05  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.109  2003/09/16 16:17:01  peter
    * varspez in calls to push_addr_param

  Revision 1.108  2003/09/07 22:09:35  peter
    * preparations for different default calling conventions
    * various RA fixes

  Revision 1.107  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.106.2.3  2003/08/31 15:46:26  peter
    * more updates for tregister

  Revision 1.106.2.2  2003/08/29 17:28:59  peter
    * next batch of updates

  Revision 1.106.2.1  2003/08/28 18:35:08  peter
    * tregister changed to cardinal

  Revision 1.106  2003/07/08 21:24:59  peter
    * sparc fixes

  Revision 1.105  2003/06/17 16:34:44  jonas
    * lots of newra fixes (need getfuncretparaloc implementation for i386)!
    * renamed all_intregisters to paramanager.get_volatile_registers_int(pocall_default) and made it
      processor dependent

  Revision 1.104  2003/06/13 21:19:31  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.103  2003/06/09 18:27:14  peter
    * load calln in temprefn in with statement

  Revision 1.102  2003/05/23 22:33:48  florian
    * fix some small flaws which prevent sparc linux system unit from compiling
    * some reformatting done

  Revision 1.101  2003/05/23 15:15:36  peter
    * better error for undefined ordinal labels

  Revision 1.100  2003/05/17 13:30:08  jonas
    * changed tt_persistant to tt_persistent :)
    * tempcreatenode now doesn't accept a boolean anymore for persistent
      temps, but a ttemptype, so you can also create ansistring temps etc

  Revision 1.99  2003/05/15 18:58:53  peter
    * removed selfpointer_offset, vmtpointer_offset
    * tvarsym.adjusted_address
    * address in localsymtable is now in the real direction
    * removed some obsolete globals

  Revision 1.98  2003/05/13 19:14:41  peter
    * failn removed
    * inherited result code check moven to pexpr

  Revision 1.97  2003/05/11 14:45:12  peter
    * tloadnode does not support objectsymtable,withsymtable anymore
    * withnode cleanup
    * direct with rewritten to use temprefnode

  Revision 1.96  2003/05/09 17:47:03  peter
    * self moved to hidden parameter
    * removed hdisposen,hnewn,selfn

  Revision 1.95  2003/04/30 22:15:59  florian
    * some 64 bit adaptions in ncgadd
    * x86-64 now uses ncgadd
    * tparamanager.ret_in_acc doesn't return true anymore for a void-def

  Revision 1.94  2003/04/27 11:21:34  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.93  2003/04/27 07:29:50  peter
    * current_procinfo.procdef cleanup, current_procinfo.procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.92  2003/04/26 11:30:59  florian
    * fixed the powerpc to work with the new function result handling

  Revision 1.91  2003/04/25 20:59:34  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.90  2002/04/25 20:15:40  florian
    * block nodes within expressions shouldn't release the used registers,
      fixed using a flag till the new rg is ready

  Revision 1.89  2003/04/25 08:25:26  daniel
    * Ifdefs around a lot of calls to cleartempgen
    * Fixed registers that are allocated but not freed in several nodes
    * Tweak to register allocator to cause less spills
    * 8-bit registers now interfere with esi,edi and ebp
      Compiler can now compile rtl successfully when using new register
      allocator

  Revision 1.88  2003/03/28 19:16:57  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.87  2003/03/17 18:55:30  peter
    * allow more tokens instead of only semicolon after inherited

  Revision 1.86  2003/02/19 22:00:14  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.85  2003/01/08 18:43:56  daniel
   * Tregister changed into a record

  Revision 1.84  2003/01/01 21:05:24  peter
    * fixed assembler methods stackpointer optimization that was
      broken after the previous change

  Revision 1.83  2002/12/29 18:59:34  peter
    * fixed parsing of declarations before asm statement

  Revision 1.82  2002/12/27 18:18:56  peter
    * check for else after empty raise statement

  Revision 1.81  2002/11/27 02:37:14  peter
    * case statement inlining added
    * fixed inlining of write()
    * switched statementnode left and right parts so the statements are
      processed in the correct order when getcopy is used. This is
      required for tempnodes

  Revision 1.80  2002/11/25 17:43:22  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.79  2002/11/18 17:31:58  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.78  2002/09/07 19:34:08  florian
    + tcg.direction is used now

  Revision 1.77  2002/09/07 15:25:07  peter
    * old logs removed and tabs fixed

  Revision 1.76  2002/09/07 12:16:03  carl
    * second part bug report 1996 fix, testrange in cordconstnode
      only called if option is set (also make parsing a tiny faster)

  Revision 1.75  2002/09/02 18:40:52  peter
    * fixed parsing of register names with lowercase

  Revision 1.74  2002/09/01 14:43:12  peter
    * fixed direct assembler for i386

  Revision 1.73  2002/08/25 19:25:20  peter
    * sym.insert_in_data removed
    * symtable.insertvardata/insertconstdata added
    * removed insert_in_data call from symtable.insert, it needs to be
      called separatly. This allows to deref the address calculation
    * procedures now calculate the parast addresses after the procedure
      directives are parsed. This fixes the cdecl parast problem
    * push_addr_param has an extra argument that specifies if cdecl is used
      or not

  Revision 1.72  2002/08/17 09:23:40  florian
    * first part of procinfo rewrite

  Revision 1.71  2002/08/16 14:24:58  carl
    * issameref() to test if two references are the same (then emit no opcodes)
    + ret_in_reg to replace ret_in_acc
      (fix some register allocation bugs at the same time)
    + save_std_register now has an extra parameter which is the
      usedinproc registers

  Revision 1.70  2002/08/11 14:32:27  peter
    * renamed current_library to objectlibrary

  Revision 1.69  2002/08/11 13:24:12  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.68  2002/08/10 14:46:30  carl
    + moved target_cpu_string to cpuinfo
    * renamed asmmode enum.
    * assembler reader has now less ifdef's
    * move from nppcmem.pas -> ncgmem.pas vec. node.

  Revision 1.67  2002/08/09 19:11:44  carl
    + reading of used registers in assembler routines is now
      cpu-independent

  Revision 1.66  2002/08/06 20:55:22  florian
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

}
