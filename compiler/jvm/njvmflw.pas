{
    Copyright (c) 1998-2011 by Florian Klaempfl and Jonas Maebe

    Generate assembler for nodes that influence the flow for the JVM

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
unit njvmflw;

{$i fpcdefs.inc}

interface

    uses
      aasmbase,node,nflw,ncgflw,compilerbase;

    type
       tjvmfornode = class(tcgfornode)
          function pass_1: tnode; override;
       end;

       tjvmraisenode = class(traisenode)
          function pass_typecheck: tnode; override;
          function pass_1: tnode; override;
          procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
       end;

       tjvmtryexceptnode = class(ttryexceptnode)
          procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
         protected
          procedure adjust_estimated_stack_size; override;
       end;

       tjvmtryfinallynode = class(ttryfinallynode)
          procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
         protected
          procedure adjust_estimated_stack_size; override;
       end;

       tjvmonnode = class(tonnode)
          procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
       end;

implementation

    uses
      verbose,globals,systems,globtype,constexp,
      symconst,symdef,symsym,aasmtai,aasmdata,aasmcpu,defutil,jvmdef,defcmp,
      procinfo,cgbase,pass_1,pass_2,pass_2_context,parabase,
      cpubase,cpuinfo,
      nbas,nld,ncon,ncnv,
      tgobj,paramgr,
      cgutils,nodehelper,hlcgcpu,
      compiler
      ;

{*****************************************************************************
                             TFJVMFORNODE
*****************************************************************************}

    function tjvmfornode.pass_1: tnode;
      var
        iteratortmp: ttempcreatenode;
        olditerator: tnode;
        block,
        newbody: tblocknode;
        stat,
        newbodystat: tstatementnode;
      begin
        { transform for-loops with enums to:
            for tempint:=ord(lowval) to ord(upperval) do
              begin
                originalctr:=tenum(tempint);
                <original loop body>
              end;

          enums are class instances in Java and hence can't be increased or so.
          The type conversion consists of an array lookup in a final method,
          so it shouldn't be too expensive.
        }
        if left.resultdef.typ=enumdef then
          begin
            block:=internalstatements(compiler,stat);
            iteratortmp:=compiler.ctempcreatenode(compiler.deftypes.s32inttype,left.resultdef.size,tt_persistent,true);
            addstatement(stat,iteratortmp);
            olditerator:=left;
            left:=compiler.ctemprefnode(iteratortmp);
            inserttypeconv_explicit(right,compiler.deftypes.s32inttype,compiler);
            inserttypeconv_explicit(t1,compiler.deftypes.s32inttype,compiler);
            newbody:=internalstatements(compiler,newbodystat);
            addstatement(newbodystat,compiler.cassignmentnode(olditerator,
              compiler.ctypeconvnode_explicit(compiler.ctemprefnode(iteratortmp),
                olditerator.resultdef)));
            addstatement(newbodystat,t2);
            addstatement(stat,compiler.cfornode(left,right,t1,newbody,lnf_backward in loopflags));
            addstatement(stat,compiler.ctempdeletenode(iteratortmp));
            left:=nil;
            right:=nil;
            t1:=nil;
            t2:=nil;
            result:=block
          end
        else
          result:=inherited pass_1;
      end;

{*****************************************************************************
                             SecondRaise
*****************************************************************************}

    var
      current_except_loc: tlocation;

    function tjvmraisenode.pass_typecheck: tnode;
      begin
         Result:=inherited pass_typecheck;
         if compiler.verbose.codegenerror then
           exit;
         { Java exceptions must descend from java.lang.Throwable }
         if assigned(left) and
            not def_is_related(left.resultdef,compiler.deftypes.java_jlthrowable) then
           compiler.verbose.MessagePos2(left.fileinfo,type_e_incompatible_types,left.resultdef.typename,'class(JLThrowable)');
         { Java exceptions cannot be raised "at" a specific location }
         if assigned(right) then
           compiler.verbose.MessagePos(right.fileinfo,parser_e_illegal_expression);
      end;


    function tjvmraisenode.pass_1: tnode;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         if assigned(left) then
           firstpass(left);
      end;


    procedure tjvmraisenode.pass_generate_code(ctx:tpassgeneratecodecontext);
      begin
        if assigned(left) then
          begin
            secondpass(left,ctx);
            thlcgjvm(ctx.hlcg).a_load_loc_stack(ctx.CurrAsmList,left.resultdef,left.location);
          end
        else
          thlcgjvm(ctx.hlcg).a_load_loc_stack(ctx.CurrAsmList,compiler.deftypes.java_jlthrowable,current_except_loc);
        ctx.CurrAsmList.Concat(taicpu.op_none(a_athrow));
        thlcgjvm(ctx.hlcg).decstack(ctx.CurrAsmList,1);
      end;


{*****************************************************************************
                             SecondTryExcept
*****************************************************************************}

    var
       begintrylabel,
       endtrylabel: tasmlabel;
       endexceptlabel : tasmlabel;


    procedure tjvmtryexceptnode.pass_generate_code(ctx:tpassgeneratecodecontext);

      var
         oldendexceptlabel,
         oldbegintrylabel,
         oldendtrylabel,
         defaultcatchlabel: tasmlabel;
         oldflowcontrol,tryflowcontrol,
         exceptflowcontrol : tflowcontrol;
         prev_except_loc: tlocation;
      begin
         location_reset(location,LOC_VOID,OS_NO);

         oldflowcontrol:=flowcontrol;
         flowcontrol:=[fc_inflowcontrol];
         { this can be called recursively }
         oldbegintrylabel:=begintrylabel;
         oldendtrylabel:=endtrylabel;
         oldendexceptlabel:=endexceptlabel;

         { get new labels for the control flow statements }
         ctx.CurrAsmList.AsmData.getaddrlabel(begintrylabel);
         ctx.CurrAsmList.AsmData.getaddrlabel(endtrylabel);
         ctx.CurrAsmList.AsmData.getjumplabel(endexceptlabel);

         { try block }
         { set control flow labels for the try block }

         ctx.hlcg.a_label(ctx.CurrAsmList,begintrylabel);
         secondpass(left,ctx);
         ctx.hlcg.a_label(ctx.CurrAsmList,endtrylabel);
         tryflowcontrol:=flowcontrol;

         { jump over exception handling blocks }
         ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));
         ctx.hlcg.a_jmp_always(ctx.CurrAsmList,endexceptlabel);
         ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

         { set control flow labels for the except block }
         { and the on statements                        }

         flowcontrol:=[fc_inflowcontrol];
         { on-statements }
         if assigned(right) then
           secondpass(right,ctx);

         { default handling except handling }
         if assigned(t1) then
           begin
             ctx.CurrAsmList.AsmData.getaddrlabel(defaultcatchlabel);
             ctx.CurrAsmList.concat(tai_jcatch.create(
               'all',begintrylabel,endtrylabel,defaultcatchlabel));
             ctx.hlcg.a_label(ctx.CurrAsmList,defaultcatchlabel);
             { here we don't have to reset flowcontrol           }
             { the default and on flowcontrols are handled equal }

             { get the exception object from the stack and store it for use by
               the exception code (in case of an anonymous "raise") }
             ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));
             prev_except_loc:=current_except_loc;
             location_reset_ref(current_except_loc,LOC_REFERENCE,OS_ADDR,4,[]);
             ctx.tg.GetLocal(ctx.CurrAsmList,sizeof(pint),compiler.deftypes.java_jlthrowable,current_except_loc.reference);
             thlcgjvm(ctx.hlcg).incstack(ctx.CurrAsmList,1);
             thlcgjvm(ctx.hlcg).a_load_stack_loc(ctx.CurrAsmList,compiler.deftypes.java_jlthrowable,current_except_loc);
             ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

             { and generate the exception handling code }
             secondpass(t1,ctx);

             { free the temp containing the exception and invalidate }
             ctx.tg.UngetLocal(ctx.CurrAsmList,current_except_loc.reference);
             current_except_loc:=prev_except_loc;

             exceptflowcontrol:=flowcontrol;
           end
         else
           exceptflowcontrol:=flowcontrol;
         ctx.hlcg.a_label(ctx.CurrAsmList,endexceptlabel);

         { restore all saved labels }
         begintrylabel:=oldbegintrylabel;
         endtrylabel:=oldendtrylabel;
         endexceptlabel:=oldendexceptlabel;

         { return all used control flow statements }
         flowcontrol:=oldflowcontrol+(exceptflowcontrol +
           tryflowcontrol - [fc_inflowcontrol]);
      end;


    procedure tjvmtryexceptnode.adjust_estimated_stack_size;
      begin
        { do nothing }
      end;


    {*****************************************************************************
                                   SecondOn
    *****************************************************************************}

    procedure tjvmonnode.pass_generate_code(ctx:tpassgeneratecodecontext);
      var
         thisonlabel : tasmlabel;
         oldflowcontrol : tflowcontrol;
         exceptvarsym : tlocalvarsym;
         prev_except_loc : tlocation;
      begin
         location_reset(location,LOC_VOID,OS_NO);

         oldflowcontrol:=flowcontrol;
         flowcontrol:=[fc_inflowcontrol];
         ctx.CurrAsmList.AsmData.getjumplabel(thisonlabel);

         ctx.hlcg.a_label(ctx.CurrAsmList,thisonlabel);

         if assigned(excepTSymtable) then
           exceptvarsym:=tlocalvarsym(excepTSymtable.SymList[0])
         else
           internalerror(2011020402);

         { add exception catching information for the JVM: exception type
           (will have to be adjusted if/when support for catching class
            reference types is added), begin/end of code in which the exception
            can be raised, and start of this exception handling code }
         ctx.CurrAsmList.concat(tai_jcatch.create(
           tobjectdef(exceptvarsym.vardef).jvm_full_typename(true),
           begintrylabel,endtrylabel,thisonlabel));

         { Retrieve exception variable }
         { 1) prepare the location where we'll store it }
         location_reset_ref(exceptvarsym.localloc,LOC_REFERENCE,OS_ADDR,sizeof(pint),[]);
         ctx.tg.GetLocal(ctx.CurrAsmList,sizeof(pint),exceptvarsym.vardef,exceptvarsym.localloc.reference);
         prev_except_loc:=current_except_loc;
         current_except_loc:=exceptvarsym.localloc;
         { 2) the exception variable is at the top of the evaluation stack
           (placed there by the JVM) -> adjust stack count, then store it }
         thlcgjvm(ctx.hlcg).incstack(ctx.CurrAsmList,1);
         thlcgjvm(ctx.hlcg).a_load_stack_loc(ctx.CurrAsmList,exceptvarsym.vardef,current_except_loc);

         if assigned(right) then
           secondpass(right,ctx);

         { clear some stuff }
         ctx.tg.UngetLocal(ctx.CurrAsmList,exceptvarsym.localloc.reference);
         exceptvarsym.localloc.loc:=LOC_INVALID;
         current_except_loc:=prev_except_loc;
         ctx.hlcg.a_jmp_always(ctx.CurrAsmList,endexceptlabel);

         flowcontrol:=oldflowcontrol+(flowcontrol-[fc_inflowcontrol]);

         { next on node }
         if assigned(left) then
           secondpass(left,ctx);
      end;

{*****************************************************************************
                             SecondTryFinally
*****************************************************************************}

    procedure tjvmtryfinallynode.pass_generate_code(ctx:tpassgeneratecodecontext);
      var
         begintrylabel,
         endtrylabel,
         reraiselabel,
         finallylabel,
         finallyexceptlabel,
         endfinallylabel,
         exitfinallylabel,
         continuefinallylabel,
         breakfinallylabel,
         oldCurrExitLabel,
         oldContinueLabel,
         oldBreakLabel : tasmlabel;
         oldflowcontrol,tryflowcontrol : tflowcontrol;
         finallycodecopy: tnode;
         reasonbuf,
         exceptreg: tregister;
      begin
         oldBreakLabel:=nil;
         oldContinueLabel:=nil;
         finallycodecopy:=nil;
         continuefinallylabel:=nil;
         breakfinallylabel:=nil;

         { not necessary on a garbage-collected platform }
         if implicitframe then
           internalerror(2011031803);
         location_reset(location,LOC_VOID,OS_NO);

         { check if child nodes do a break/continue/exit }
         oldflowcontrol:=flowcontrol;
         flowcontrol:=[fc_inflowcontrol];
         ctx.CurrAsmList.AsmData.getjumplabel(finallylabel);
         ctx.CurrAsmList.AsmData.getjumplabel(endfinallylabel);
         ctx.CurrAsmList.AsmData.getjumplabel(reraiselabel);

         { the finally block must catch break, continue and exit }
         { statements                                            }
         oldCurrExitLabel:=compiler.current_procinfo.CurrExitLabel;
         ctx.CurrAsmList.AsmData.getjumplabel(exitfinallylabel);
         compiler.current_procinfo.CurrExitLabel:=exitfinallylabel;
         if assigned(compiler.current_procinfo.CurrBreakLabel) then
          begin
            oldContinueLabel:=compiler.current_procinfo.CurrContinueLabel;
            oldBreakLabel:=compiler.current_procinfo.CurrBreakLabel;
            ctx.CurrAsmList.AsmData.getjumplabel(breakfinallylabel);
            ctx.CurrAsmList.AsmData.getjumplabel(continuefinallylabel);
            compiler.current_procinfo.CurrContinueLabel:=continuefinallylabel;
            compiler.current_procinfo.CurrBreakLabel:=breakfinallylabel;
          end;

         { allocate reg to store the reason why the finally block was entered
           (no exception, break, continue, exit), so we can continue to the
           right label afterwards. In case of an exception, we use a separate
           (duplicate) finally block because otherwise the JVM's bytecode
           verification cannot statically prove that the exception reraise code
           will only execute in case an exception actually happened }
         reasonbuf:=ctx.hlcg.getaddressregister(ctx.CurrAsmList,compiler.deftypes.s32inttype);

         { try code }
         begintrylabel:=nil;
         endtrylabel:=nil;
         if assigned(left) then
           begin
              ctx.CurrAsmList.AsmData.getaddrlabel(begintrylabel);
              ctx.CurrAsmList.AsmData.getaddrlabel(endtrylabel);
              ctx.hlcg.a_label(ctx.CurrAsmList,begintrylabel);
              secondpass(left,ctx);
              ctx.hlcg.a_label(ctx.CurrAsmList,endtrylabel);
              tryflowcontrol:=flowcontrol;
              if compiler.verbose.codegenerror then
                exit;
              { reason: no exception occurred }
              ctx.hlcg.a_load_const_reg(ctx.CurrAsmList,compiler.deftypes.s32inttype,0,reasonbuf);
           end
         else
           tryflowcontrol:=[fc_inflowcontrol];

         { begin of the finally code }
         ctx.hlcg.a_label(ctx.CurrAsmList,finallylabel);
         { finally code }
         flowcontrol:=[fc_inflowcontrol];
         { duplicate finally code for case when exception happened }
         if assigned(begintrylabel) then
           finallycodecopy:=right.getcopy;
         secondpass(right,ctx);
         { goto is allowed if it stays inside the finally block,
           this is checked using the exception block number }
         if (flowcontrol-[fc_gotolabel])<>[fc_inflowcontrol] then
           compiler.verbose.CGMessage(cg_e_control_flow_outside_finally);
         if compiler.verbose.codegenerror then
           begin
             if assigned(begintrylabel) then
               finallycodecopy.free;
             exit;
           end;

         { don't generate line info for internal cleanup }
         ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));

         { the reasonbuf holds the reason why this (non-exception) finally code
           was executed:
             0 = try code simply finished
             1 = (unused) exception raised
             2 = exit called
             3 = break called
             4 = continue called }
         ctx.hlcg.a_cmp_const_reg_label(ctx.CurrAsmList,compiler.deftypes.s32inttype,OC_EQ,0,reasonbuf,endfinallylabel);
         if fc_exit in tryflowcontrol then
           if ([fc_break,fc_continue]*tryflowcontrol)<>[] then
             ctx.hlcg.a_cmp_const_reg_label(ctx.CurrAsmList,compiler.deftypes.s32inttype,OC_EQ,2,reasonbuf,oldCurrExitLabel)
           else
             ctx.hlcg.a_jmp_always(ctx.CurrAsmList,oldCurrExitLabel);
         if fc_break in tryflowcontrol then
           if fc_continue in tryflowcontrol then
             ctx.hlcg.a_cmp_const_reg_label(ctx.CurrAsmList,compiler.deftypes.s32inttype,OC_EQ,3,reasonbuf,oldBreakLabel)
           else
             ctx.hlcg.a_jmp_always(ctx.CurrAsmList,oldBreakLabel);
         if fc_continue in tryflowcontrol then
           ctx.hlcg.a_jmp_always(ctx.CurrAsmList,oldContinueLabel);
         { now generate the trampolines for exit/break/continue to load the reasonbuf }
         if fc_exit in tryflowcontrol then
           begin
              ctx.hlcg.a_label(ctx.CurrAsmList,exitfinallylabel);
              ctx.hlcg.a_load_const_reg(ctx.CurrAsmList,compiler.deftypes.s32inttype,2,reasonbuf);
              ctx.hlcg.a_jmp_always(ctx.CurrAsmList,finallylabel);
           end;
         if fc_break in tryflowcontrol then
          begin
              ctx.hlcg.a_label(ctx.CurrAsmList,breakfinallylabel);
              ctx.hlcg.a_load_const_reg(ctx.CurrAsmList,compiler.deftypes.s32inttype,3,reasonbuf);
              ctx.hlcg.a_jmp_always(ctx.CurrAsmList,finallylabel);
           end;
         if fc_continue in tryflowcontrol then
           begin
              ctx.hlcg.a_label(ctx.CurrAsmList,continuefinallylabel);
              ctx.hlcg.a_load_const_reg(ctx.CurrAsmList,compiler.deftypes.s32inttype,4,reasonbuf);
              ctx.hlcg.a_jmp_always(ctx.CurrAsmList,finallylabel);
           end;
         { jump over finally-code-in-case-an-exception-happened }
         ctx.hlcg.a_jmp_always(ctx.CurrAsmList,endfinallylabel);

         { generate finally code in case an exception occurred }
         if assigned(begintrylabel) then
           begin
             ctx.CurrAsmList.AsmData.getaddrlabel(finallyexceptlabel);
             ctx.hlcg.a_label(ctx.CurrAsmList,finallyexceptlabel);
             { catch the exceptions }
             ctx.CurrAsmList.concat(tai_jcatch.create(
               'all',begintrylabel,endtrylabel,finallyexceptlabel));
             { store the generated exception object to a temp }
             exceptreg:=ctx.hlcg.getaddressregister(ctx.CurrAsmList,compiler.deftypes.java_jlthrowable);
             thlcgjvm(ctx.hlcg).incstack(ctx.CurrAsmList,1);
             thlcgjvm(ctx.hlcg).a_load_stack_reg(ctx.CurrAsmList,compiler.deftypes.java_jlthrowable,exceptreg);
             { generate the finally code again }
             secondpass(finallycodecopy,ctx);
             finallycodecopy.free;
             { reraise the exception }
             thlcgjvm(ctx.hlcg).a_load_reg_stack(ctx.CurrAsmList,compiler.deftypes.java_jlthrowable,exceptreg);
             ctx.CurrAsmList.Concat(taicpu.op_none(a_athrow));
             thlcgjvm(ctx.hlcg).decstack(ctx.CurrAsmList,1);
           end;
         ctx.hlcg.a_label(ctx.CurrAsmList,endfinallylabel);

         { end cleanup }
         ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

         compiler.current_procinfo.CurrExitLabel:=oldCurrExitLabel;
         if assigned(compiler.current_procinfo.CurrBreakLabel) then
          begin
            compiler.current_procinfo.CurrContinueLabel:=oldContinueLabel;
            compiler.current_procinfo.CurrBreakLabel:=oldBreakLabel;
          end;
         flowcontrol:=oldflowcontrol+(tryflowcontrol-[fc_inflowcontrol]);
      end;


    procedure tjvmtryfinallynode.adjust_estimated_stack_size;
      begin
        { do nothing }
      end;

begin
   cfornode:=tjvmfornode;
   craisenode:=tjvmraisenode;
   ctryexceptnode:=tjvmtryexceptnode;
   ctryfinallynode:=tjvmtryfinallynode;
   connode:=tjvmonnode;
end.

