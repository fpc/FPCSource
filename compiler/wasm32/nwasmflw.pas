{
    Copyright (c) 2019 by Dmitry Boyarintsev

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
unit nwasmflw;

{$i fpcdefs.inc}

interface

    uses
      aasmbase,node,nflw,ncgflw,cutils,compilerbase;

    type

      { twasmifnode }

      { Wasm doesn't have any jump(+offset) operations
        It only provide structured blockes to handle jumps
        (It's possible to jump-out-of-block at any time)
        "If" is also implemented as a block, identical to high-level language. }
      twasmifnode = class(tcgifnode)
      public
        procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
      end;

      { twasmwhilerepeatnode }

      twasmwhilerepeatnode = class(tcgwhilerepeatnode)
      public
        procedure pass_generate_code_condition(ctx:tpassgeneratecodecontext);
        procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
      end;

      { twasmraisenode }

      twasmraisenode = class(tcgraisenode)
      private
        function pass_1_no_exceptions : tnode;
        function pass_1_native_exnref_exceptions : tnode;
        function pass_1_native_legacy_exceptions : tnode;
        function pass_1_bf_exceptions : tnode;
      public
        function pass_1 : tnode;override;
      end;

      { twasmtryexceptnode }

      twasmtryexceptnode = class(tcgtryexceptnode)
      private
        procedure pass_generate_code_no_exceptions(ctx:tpassgeneratecodecontext);
        procedure pass_generate_code_native_exnref_exceptions(ctx:tpassgeneratecodecontext);
        procedure pass_generate_code_native_legacy_exceptions(ctx:tpassgeneratecodecontext);
        procedure pass_generate_code_bf_exceptions(ctx:tpassgeneratecodecontext);
      public
        procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
      end;

      { twasmtryfinallynode }

      twasmtryfinallynode = class(tcgtryfinallynode)
      private
        procedure pass_generate_code_no_exceptions(ctx:tpassgeneratecodecontext);
        procedure pass_generate_code_native_exnref_exceptions(ctx:tpassgeneratecodecontext);
        procedure pass_generate_code_native_legacy_exceptions(ctx:tpassgeneratecodecontext);
        procedure pass_generate_code_bf_exceptions(ctx:tpassgeneratecodecontext);
      public
        procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
      end;

      { twasmonnode }

      twasmonnode = class(tcgonnode)
      private
        procedure pass_generate_code_no_exceptions(ctx:tpassgeneratecodecontext);
        procedure pass_generate_code_native_exnref_exceptions(ctx:tpassgeneratecodecontext);
        procedure pass_generate_code_native_legacy_exceptions(ctx:tpassgeneratecodecontext);
        procedure pass_generate_code_bf_exceptions(ctx:tpassgeneratecodecontext);
      public
        procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
      end;

implementation

    uses
      verbose,globals,systems,globtype,constexp,
      symconst,symdef,symsym,symtype,aasmtai,aasmdata,aasmcpu,defutil,defcmp,
      procinfo,cgbase,cgexcept,pass_1,pass_2,pass_2_context,parabase,compinnr,
      cpubase,cpuinfo,cpupi,
      nbas,nld,ncon,ncnv,ncal,ninl,nmem,nadd,nutils,
      tgobj,paramgr,
      cgutils,nodehelper,hlcgcpu,compiler;

{*****************************************************************************
                           twasmwhilerepeatnode
*****************************************************************************}

    procedure twasmwhilerepeatnode.pass_generate_code_condition(ctx:tpassgeneratecodecontext);
      begin
        secondpass(left,ctx);
        thlcgwasm(ctx.hlcg).a_load_loc_stack(ctx.CurrAsmList,left.resultdef,left.location);

        // reversing the condition
        if not (lnf_checknegate in loopflags) then
          ctx.CurrAsmList.concat(taicpu.op_none(a_i32_eqz));

        ctx.CurrAsmList.concat(taicpu.op_const(a_br_if,1) );
        thlcgwasm(ctx.hlcg).decstack(ctx.CurrAsmList,1);
      end;


    procedure twasmwhilerepeatnode.pass_generate_code(ctx:tpassgeneratecodecontext);
      var
         lcont,lbreak,lloop,
         oldclabel,oldblabel : tasmlabel;
         truelabel,falselabel : tasmlabel;
         oldflowcontrol : tflowcontrol;
      begin
        location_reset(location,LOC_VOID,OS_NO);

        current_asmdata.getjumplabel(lloop);
        current_asmdata.getjumplabel(lcont);
        current_asmdata.getjumplabel(lbreak);

        oldflowcontrol:=flowcontrol;

        oldclabel:=compiler.current_procinfo.CurrContinueLabel;
        oldblabel:=compiler.current_procinfo.CurrBreakLabel;

        include(flowcontrol,fc_inflowcontrol);
        exclude(flowcontrol,fc_unwind_loop);

        ctx.CurrAsmList.concat(taicpu.op_none(a_block));
        ctx.CurrAsmList.concat(taicpu.op_none(a_loop));

        if lnf_testatbegin in loopflags then
        begin
          ctx.hlcg.a_label(ctx.CurrAsmList,lcont);
          pass_generate_code_condition(ctx);
        end;

        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        compiler.current_procinfo.CurrContinueLabel:=lcont;
        compiler.current_procinfo.CurrBreakLabel:=lbreak;

        secondpass(right,ctx);

        if (lnf_testatbegin in loopflags) then
          ctx.CurrAsmList.concat(taicpu.op_const(a_br,1) ); // jump back to the external loop

        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if not (lnf_testatbegin in loopflags) then
          begin
            ctx.hlcg.a_label(ctx.CurrAsmList,lcont);
            pass_generate_code_condition(ctx);
          end;
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,0) ); // jump back to loop

        ctx.CurrAsmList.concat(taicpu.op_none(a_end_loop));
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
        ctx.hlcg.a_label(ctx.CurrAsmList,lbreak);

        compiler.current_procinfo.CurrContinueLabel:=oldclabel;
        compiler.current_procinfo.CurrBreakLabel:=oldblabel;

        { a break/continue in a while/repeat block can't be seen outside }
        flowcontrol:=oldflowcontrol+(flowcontrol-[fc_break,fc_continue,fc_inflowcontrol]);
      end;

{*****************************************************************************
                               twasmifnode
*****************************************************************************}

    procedure twasmifnode.pass_generate_code(ctx:tpassgeneratecodecontext);
      var
        oldflowcontrol: tflowcontrol;
      begin
        // left  - condition
        // right - then
        // t1    - else (optional)

        location_reset(location,LOC_VOID,OS_NO);

        oldflowcontrol := flowcontrol;
        include(flowcontrol,fc_inflowcontrol);

        //todo: MOVE all current_asm_data actions to Wasm HL CodeGen

        secondpass(left,ctx); // condition expressions
        thlcgwasm(ctx.hlcg).a_load_loc_stack(ctx.CurrAsmList,left.resultdef,left.location);

        if is_64bit(left.resultdef) then
          begin
            thlcgwasm(ctx.hlcg).a_load_const_stack(ctx.CurrAsmList,left.resultdef,0,R_INTREGISTER);
            ctx.CurrAsmList.Concat(taicpu.op_none(a_i64_ne));
            thlcgwasm(ctx.hlcg).decstack(ctx.CurrAsmList,1);
          end;

        ctx.CurrAsmList.concat(taicpu.op_none(a_if));
        thlcgwasm(ctx.hlcg).decstack(ctx.CurrAsmList,1);

        if Assigned(right) then
          secondpass(right,ctx); // then branchs

        if Assigned(t1) then // else branch
          begin
            ctx.CurrAsmList.concat(taicpu.op_none(a_else));
            secondpass(t1,ctx);
          end;

        ctx.CurrAsmList.concat(taicpu.op_none(a_end_if));

        flowcontrol := oldflowcontrol + (flowcontrol - [fc_inflowcontrol]);
      end;

{*****************************************************************************
                             twasmraisenode
*****************************************************************************}

    function twasmraisenode.pass_1_no_exceptions : tnode;
      var
        statements : tstatementnode;
        //current_addr : tlabelnode;
        raisenode : tcallnode;
      begin
        result:=internalstatements(compiler,statements);

        if assigned(left) then
          begin
            { first para must be a class }
            firstpass(left);
            { insert needed typeconvs for addr,frame }
            if assigned(right) then
              begin
                { addr }
                firstpass(right);
                { frame }
                if assigned(third) then
                  firstpass(third)
                else
                  third:=compiler.cpointerconstnode(0,compiler.deftypes.voidpointertype);
              end
            else
              begin
                third:=compiler.cinlinenode(in_get_frame,false,nil);
                //current_addr:=compiler.clabelnode(compiler.cnothingnode,clabelsym.create('$raiseaddr'));
                //addstatement(statements,current_addr);
                //right:=compiler.caddrnode(compiler.cloadnode(current_addr.labsym,current_addr.labsym.owner));
                right:=compiler.cnilnode;

                { raise address off by one so we are for sure inside the action area for the raise }
                if tf_use_psabieh in compiler.target.info.flags then
                  right:=compiler.caddnode_internal(addn,right,compiler.cordconstnode(1,compiler.deftypes.sizesinttype,false));
              end;

            raisenode:=compiler.ccallnode_intern('fpc_raiseexception',
              compiler.ccallparanode(third,
              compiler.ccallparanode(right,
              compiler.ccallparanode(left,nil)))
              );
            include(raisenode.callnodeflags,cnf_call_never_returns);
            addstatement(statements,raisenode);
          end
        else
          begin
            addstatement(statements,compiler.ccallnode_intern('fpc_popaddrstack',nil));
            raisenode:=compiler.ccallnode_intern('fpc_reraise',nil);
            include(raisenode.callnodeflags,cnf_call_never_returns);
            addstatement(statements,raisenode);
          end;
        left:=nil;
        right:=nil;
        third:=nil;
      end;


    function twasmraisenode.pass_1_native_exnref_exceptions: tnode;
      var
        statements : tstatementnode;
        //current_addr : tlabelnode;
        raisenode : tcallnode;
      begin
        result:=internalstatements(compiler,statements);

        if assigned(left) then
          begin
            { first para must be a class }
            firstpass(left);
            { insert needed typeconvs for addr,frame }
            if assigned(right) then
              begin
                { addr }
                firstpass(right);
                { frame }
                if assigned(third) then
                  firstpass(third)
                else
                  third:=compiler.cpointerconstnode(0,compiler.deftypes.voidpointertype);
              end
            else
              begin
                third:=compiler.cinlinenode(in_get_frame,false,nil);
                //current_addr:=compiler.clabelnode(compiler.cnothingnode,clabelsym.create('$raiseaddr'));
                //addstatement(statements,current_addr);
                //right:=compiler.caddrnode(compiler.cloadnode(current_addr.labsym,current_addr.labsym.owner));
                right:=compiler.cnilnode;

                { raise address off by one so we are for sure inside the action area for the raise }
                if tf_use_psabieh in compiler.target.info.flags then
                  right:=compiler.caddnode_internal(addn,right,compiler.cordconstnode(1,compiler.deftypes.sizesinttype,false));
              end;

            raisenode:=compiler.ccallnode_intern('fpc_raiseexception',
              compiler.ccallparanode(third,
              compiler.ccallparanode(right,
              compiler.ccallparanode(left,nil)))
              );
            include(raisenode.callnodeflags,cnf_call_never_returns);
            addstatement(statements,raisenode);
          end
        else
          begin
            //addstatement(statements,compiler.ccallnode_intern('fpc_popaddrstack',nil));
            raisenode:=compiler.ccallnode_intern('fpc_reraise2',nil);
            include(raisenode.callnodeflags,cnf_call_never_returns);
            addstatement(statements,raisenode);
          end;
        left:=nil;
        right:=nil;
        third:=nil;
      end;


    function twasmraisenode.pass_1_native_legacy_exceptions : tnode;
      var
        statements : tstatementnode;
        //current_addr : tlabelnode;
        raisenode : tcallnode;
      begin
        result:=internalstatements(compiler,statements);

        if assigned(left) then
          begin
            { first para must be a class }
            firstpass(left);
            { insert needed typeconvs for addr,frame }
            if assigned(right) then
              begin
                { addr }
                firstpass(right);
                { frame }
                if assigned(third) then
                  firstpass(third)
                else
                  third:=compiler.cpointerconstnode(0,compiler.deftypes.voidpointertype);
              end
            else
              begin
                third:=compiler.cinlinenode(in_get_frame,false,nil);
                //current_addr:=compiler.clabelnode(compiler.cnothingnode,clabelsym.create('$raiseaddr'));
                //addstatement(statements,current_addr);
                //right:=compiler.caddrnode(compiler.cloadnode(current_addr.labsym,current_addr.labsym.owner));
                right:=compiler.cnilnode;

                { raise address off by one so we are for sure inside the action area for the raise }
                if tf_use_psabieh in compiler.target.info.flags then
                  right:=compiler.caddnode_internal(addn,right,compiler.cordconstnode(1,compiler.deftypes.sizesinttype,false));
              end;

            raisenode:=compiler.ccallnode_intern('fpc_raiseexception',
              compiler.ccallparanode(third,
              compiler.ccallparanode(right,
              compiler.ccallparanode(left,nil)))
              );
            include(raisenode.callnodeflags,cnf_call_never_returns);
            addstatement(statements,raisenode);
          end
        else
          begin
            //addstatement(statements,compiler.ccallnode_intern('fpc_popaddrstack',nil));
            raisenode:=compiler.ccallnode_intern('fpc_reraise2',nil);
            include(raisenode.callnodeflags,cnf_call_never_returns);
            addstatement(statements,raisenode);
          end;
        left:=nil;
        right:=nil;
        third:=nil;
      end;


    function twasmraisenode.pass_1_bf_exceptions : tnode;
      var
        statements : tstatementnode;
        //current_addr : tlabelnode;
        raisenode : tcallnode;
      begin
        result:=internalstatements(compiler,statements);

        if assigned(left) then
          begin
            { first para must be a class }
            firstpass(left);
            { insert needed typeconvs for addr,frame }
            if assigned(right) then
              begin
                { addr }
                firstpass(right);
                { frame }
                if assigned(third) then
                  firstpass(third)
                else
                  third:=compiler.cpointerconstnode(0,compiler.deftypes.voidpointertype);
              end
            else
              begin
                third:=compiler.cinlinenode(in_get_frame,false,nil);
                //current_addr:=compiler.clabelnode(compiler.cnothingnode,clabelsym.create('$raiseaddr'));
                //addstatement(statements,current_addr);
                //right:=compiler.caddrnode(compiler.cloadnode(current_addr.labsym,current_addr.labsym.owner));
                right:=compiler.cnilnode;

                { raise address off by one so we are for sure inside the action area for the raise }
                if tf_use_psabieh in compiler.target.info.flags then
                  right:=compiler.caddnode_internal(addn,right,compiler.cordconstnode(1,compiler.deftypes.sizesinttype,false));
              end;

            raisenode:=compiler.ccallnode_intern('fpc_raiseexception',
              compiler.ccallparanode(third,
              compiler.ccallparanode(right,
              compiler.ccallparanode(left,nil)))
              );
            include(raisenode.callnodeflags,cnf_call_never_returns);
            addstatement(statements,raisenode);
          end
        else
          begin
            //addstatement(statements,compiler.ccallnode_intern('fpc_popaddrstack',nil));
            raisenode:=compiler.ccallnode_intern('fpc_reraise2',nil);
            include(raisenode.callnodeflags,cnf_call_never_returns);
            addstatement(statements,raisenode);
          end;
        left:=nil;
        right:=nil;
        third:=nil;
      end;


    function twasmraisenode.pass_1 : tnode;
      begin
        if ts_wasm_no_exceptions in compiler.globals.current_settings.targetswitches then
          result:=pass_1_no_exceptions
        else if ts_wasm_native_exnref_exceptions in compiler.globals.current_settings.targetswitches then
          result:=pass_1_native_exnref_exceptions
        else if ts_wasm_native_legacy_exceptions in compiler.globals.current_settings.targetswitches then
          result:=pass_1_native_legacy_exceptions
        else if ts_wasm_bf_exceptions in compiler.globals.current_settings.targetswitches then
          result:=pass_1_bf_exceptions
        else
          result:=inherited;
      end;

{*****************************************************************************
                             twasmtryexceptnode
*****************************************************************************}

    procedure twasmtryexceptnode.pass_generate_code_no_exceptions(ctx:tpassgeneratecodecontext);
      begin
        location_reset(location,LOC_VOID,OS_NO);
        secondpass(left,ctx);
      end;

    procedure twasmtryexceptnode.pass_generate_code_native_exnref_exceptions(ctx:tpassgeneratecodecontext);
      var
        trystate,doobjectdestroyandreraisestate: tcgexceptionstatehandler.texceptionstate;
        destroytemps,
        excepttemps: tcgexceptionstatehandler.texceptiontemps;
        afteronflowcontrol: tflowcontrol;
        oldCurrExitLabel,
        oldContinueLabel,
        oldBreakLabel, NewContinueLabel, NewBreakLabel,
        NewCurrExitLabel: tasmlabel;
        in_loop: Boolean;
      label
        errorexit;
      begin
        oldCurrExitLabel:=nil;
        oldContinueLabel:=nil;
        oldBreakLabel:=nil;
        NewContinueLabel:=nil;
        NewBreakLabel:=nil;
        location_reset(location,LOC_VOID,OS_NO);
        doobjectdestroyandreraisestate:=Default(tcgexceptionstatehandler.texceptionstate);

        { Exception temps? We don't need no stinking exception temps! :) }
        fillchar(excepttemps,sizeof(excepttemps),0);
        reference_reset(excepttemps.envbuf,0,[]);
        reference_reset(excepttemps.jmpbuf,0,[]);
        reference_reset(excepttemps.reasonbuf,0,[]);

        in_loop:=assigned(compiler.current_procinfo.CurrBreakLabel);

        compiler.exceptionstatehandler.new_exception(ctx.CurrAsmList,excepttemps,tek_except,trystate);

        ctx.CurrAsmList.concat(taicpu.op_none(a_block));
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));
        ctx.CurrAsmList.concat(taicpu.op_catch(a_try_table,[taicpu.op_sym_const(a_catch,current_asmdata.WeakRefAsmSymbol(FPC_EXCEPTION_TAG_SYM,AT_WASM_EXCEPTION_TAG),0)]));

        { try block }
        secondpass(left,ctx);
        if compiler.verbose.codegenerror then
          goto errorexit;

        compiler.exceptionstatehandler.end_try_block(ctx.CurrAsmList,tek_except,excepttemps,trystate,nil);

        ctx.CurrAsmList.concat(taicpu.op_none(a_end_try_table));
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,1));
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));

        flowcontrol:=[fc_inflowcontrol]+trystate.oldflowcontrol*[fc_catching_exceptions];
        { on statements }
        if assigned(right) then
          secondpass(right,ctx);

        afteronflowcontrol:=flowcontrol;

        { default handling except handling }
        if assigned(t1) then
          begin
            { FPC_CATCHES with 'default handler' flag (=-1) need no longer be called,
              it doesn't change any state and its return value is ignored (Sergei)
            }

            { the destruction of the exception object must be also }
            { guarded by an exception frame, but it can be omitted }
            { if there's no user code in 'except' block            }

            if not (has_no_code(t1)) then
              begin
                { if there is an outer frame that catches exceptions, remember this for the "except"
                  part of this try/except }
                flowcontrol:=trystate.oldflowcontrol*[fc_inflowcontrol,fc_catching_exceptions];
                { Exception temps? We don't need no stinking exception temps! :) }
                fillchar(excepttemps,sizeof(destroytemps),0);
                reference_reset(destroytemps.envbuf,0,[]);
                reference_reset(destroytemps.jmpbuf,0,[]);
                reference_reset(destroytemps.reasonbuf,0,[]);
                compiler.exceptionstatehandler.new_exception(ctx.CurrAsmList,destroytemps,tek_except,doobjectdestroyandreraisestate);
                { the flowcontrol from the default except-block must be merged
                  with the flowcontrol flags potentially set by the
                  on-statements handled above (secondpass(right)), as they are
                  at the same program level }
                flowcontrol:=
                  flowcontrol+
                  afteronflowcontrol;

                ctx.CurrAsmList.concat(taicpu.op_none(a_block));
                ctx.CurrAsmList.concat(taicpu.op_none(a_block));
                ctx.CurrAsmList.concat(taicpu.op_catch(a_try_table,[taicpu.op_sym_const(a_catch,current_asmdata.WeakRefAsmSymbol(FPC_EXCEPTION_TAG_SYM,AT_WASM_EXCEPTION_TAG),0)]));

                { the 'exit' block }
                ctx.CurrAsmList.concat(taicpu.op_none(a_block));

                oldCurrExitLabel:=compiler.current_procinfo.CurrExitLabel;
                current_asmdata.getjumplabel(NewCurrExitLabel);
                compiler.current_procinfo.CurrExitLabel:=NewCurrExitLabel;

                { the 'break' block }
                ctx.CurrAsmList.concat(taicpu.op_none(a_block));

                if in_loop then
                  begin
                    oldBreakLabel:=compiler.current_procinfo.CurrBreakLabel;
                    current_asmdata.getjumplabel(NewBreakLabel);
                    compiler.current_procinfo.CurrBreakLabel:=NewBreakLabel;
                  end;

                { the 'continue' block }
                ctx.CurrAsmList.concat(taicpu.op_none(a_block));

                if in_loop then
                  begin
                    oldContinueLabel:=compiler.current_procinfo.CurrContinueLabel;
                    current_asmdata.getjumplabel(NewContinueLabel);
                    compiler.current_procinfo.CurrContinueLabel:=NewContinueLabel;
                  end;

                secondpass(t1,ctx);

                compiler.exceptionstatehandler.end_try_block(ctx.CurrAsmList,tek_except,destroytemps,doobjectdestroyandreraisestate,nil);

                ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
                ctx.CurrAsmList.concat(taicpu.op_const(a_br,6));

                { exit the 'continue' block }
                ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
                if in_loop then
                  ctx.hlcg.a_label(ctx.CurrAsmList,NewContinueLabel);
                if fc_continue in doobjectdestroyandreraisestate.newflowcontrol then
                  begin
                    ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
                    ctx.CurrAsmList.concat(taicpu.op_sym(a_br,oldContinueLabel));
                  end;

                { exit the 'break' block }
                ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));  // break
                if in_loop then
                  ctx.hlcg.a_label(ctx.CurrAsmList,NewBreakLabel);
                if fc_break in doobjectdestroyandreraisestate.newflowcontrol then
                  begin
                    ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
                    ctx.CurrAsmList.concat(taicpu.op_sym(a_br,oldBreakLabel));
                  end;

                { exit the 'exit' block }
                ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));  // exit
                ctx.hlcg.a_label(ctx.CurrAsmList,NewCurrExitLabel);
                if fc_exit in doobjectdestroyandreraisestate.newflowcontrol then
                  begin
                    ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
                    ctx.CurrAsmList.concat(taicpu.op_sym(a_br,oldCurrExitLabel));
                  end;

                compiler.current_procinfo.CurrExitLabel:=oldCurrExitLabel;
                if in_loop then
                  begin
                    compiler.current_procinfo.CurrContinueLabel:=oldContinueLabel;
                    compiler.current_procinfo.CurrBreakLabel:=oldBreakLabel;
                  end;

                ctx.CurrAsmList.concat(taicpu.op_none(a_end_try_table));
                ctx.CurrAsmList.concat(taicpu.op_const(a_br,1));
                ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));

                ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_raise_nested',[],nil).resetiftemp;

                ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
              end
            else
              begin
                doobjectdestroyandreraisestate.newflowcontrol:=afteronflowcontrol;
                ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
              end;
          end
        else
          begin
            ctx.CurrAsmList.Concat(taicpu.op_sym(a_throw,current_asmdata.WeakRefAsmSymbol(FPC_EXCEPTION_TAG_SYM,AT_WASM_EXCEPTION_TAG)));
            doobjectdestroyandreraisestate.newflowcontrol:=afteronflowcontrol;
          end;

        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));

      errorexit:
        { return all used control flow statements }
        flowcontrol:=trystate.oldflowcontrol+(doobjectdestroyandreraisestate.newflowcontrol +
          trystate.newflowcontrol - [fc_inflowcontrol,fc_catching_exceptions]);
      end;

    procedure twasmtryexceptnode.pass_generate_code_native_legacy_exceptions(ctx:tpassgeneratecodecontext);
      var
        trystate,doobjectdestroyandreraisestate: tcgexceptionstatehandler.texceptionstate;
        destroytemps,
        excepttemps: tcgexceptionstatehandler.texceptiontemps;
        afteronflowcontrol: tflowcontrol;
        oldCurrExitLabel,
        oldContinueLabel,
        oldBreakLabel, NewContinueLabel, NewBreakLabel,
        NewCurrExitLabel: tasmlabel;
        in_loop: Boolean;
      label
        errorexit;
      begin
        oldCurrExitLabel:=nil;
        oldContinueLabel:=nil;
        oldBreakLabel:=nil;
        NewContinueLabel:=nil;
        NewBreakLabel:=nil;
        location_reset(location,LOC_VOID,OS_NO);
        doobjectdestroyandreraisestate:=Default(tcgexceptionstatehandler.texceptionstate);

        { Exception temps? We don't need no stinking exception temps! :) }
        fillchar(excepttemps,sizeof(excepttemps),0);
        reference_reset(excepttemps.envbuf,0,[]);
        reference_reset(excepttemps.jmpbuf,0,[]);
        reference_reset(excepttemps.reasonbuf,0,[]);

        in_loop:=assigned(compiler.current_procinfo.CurrBreakLabel);

        compiler.exceptionstatehandler.new_exception(ctx.CurrAsmList,excepttemps,tek_except,trystate);

        ctx.CurrAsmList.concat(taicpu.op_none(a_legacy_try));

        { try block }
        secondpass(left,ctx);
        if compiler.verbose.codegenerror then
          goto errorexit;

        compiler.exceptionstatehandler.end_try_block(ctx.CurrAsmList,tek_except,excepttemps,trystate,nil);

        ctx.CurrAsmList.concat(taicpu.op_sym(a_legacy_catch,current_asmdata.WeakRefAsmSymbol(FPC_EXCEPTION_TAG_SYM,AT_WASM_EXCEPTION_TAG)));

        flowcontrol:=[fc_inflowcontrol]+trystate.oldflowcontrol*[fc_catching_exceptions];
        { on statements }
        if assigned(right) then
          secondpass(right,ctx);

        afteronflowcontrol:=flowcontrol;

        { default handling except handling }
        if assigned(t1) then
          begin
            { FPC_CATCHES with 'default handler' flag (=-1) need no longer be called,
              it doesn't change any state and its return value is ignored (Sergei)
            }

            { the destruction of the exception object must be also }
            { guarded by an exception frame, but it can be omitted }
            { if there's no user code in 'except' block            }

            if not (has_no_code(t1)) then
              begin
                { if there is an outer frame that catches exceptions, remember this for the "except"
                  part of this try/except }
                flowcontrol:=trystate.oldflowcontrol*[fc_inflowcontrol,fc_catching_exceptions];
                { Exception temps? We don't need no stinking exception temps! :) }
                fillchar(excepttemps,sizeof(destroytemps),0);
                reference_reset(destroytemps.envbuf,0,[]);
                reference_reset(destroytemps.jmpbuf,0,[]);
                reference_reset(destroytemps.reasonbuf,0,[]);
                compiler.exceptionstatehandler.new_exception(ctx.CurrAsmList,destroytemps,tek_except,doobjectdestroyandreraisestate);
                { the flowcontrol from the default except-block must be merged
                  with the flowcontrol flags potentially set by the
                  on-statements handled above (secondpass(right)), as they are
                  at the same program level }
                flowcontrol:=
                  flowcontrol+
                  afteronflowcontrol;

                ctx.CurrAsmList.concat(taicpu.op_none(a_legacy_try));

                { the 'exit' block }
                ctx.CurrAsmList.concat(taicpu.op_none(a_block));

                oldCurrExitLabel:=compiler.current_procinfo.CurrExitLabel;
                current_asmdata.getjumplabel(NewCurrExitLabel);
                compiler.current_procinfo.CurrExitLabel:=NewCurrExitLabel;

                { the 'break' block }
                ctx.CurrAsmList.concat(taicpu.op_none(a_block));

                if in_loop then
                  begin
                    oldBreakLabel:=compiler.current_procinfo.CurrBreakLabel;
                    current_asmdata.getjumplabel(NewBreakLabel);
                    compiler.current_procinfo.CurrBreakLabel:=NewBreakLabel;
                  end;

                { the 'continue' block }
                ctx.CurrAsmList.concat(taicpu.op_none(a_block));

                if in_loop then
                  begin
                    oldContinueLabel:=compiler.current_procinfo.CurrContinueLabel;
                    current_asmdata.getjumplabel(NewContinueLabel);
                    compiler.current_procinfo.CurrContinueLabel:=NewContinueLabel;
                  end;

                secondpass(t1,ctx);

                compiler.exceptionstatehandler.end_try_block(ctx.CurrAsmList,tek_except,destroytemps,doobjectdestroyandreraisestate,nil);

                ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
                ctx.CurrAsmList.concat(taicpu.op_const(a_br,4));

                { exit the 'continue' block }
                ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
                if in_loop then
                  ctx.hlcg.a_label(ctx.CurrAsmList,NewContinueLabel);
                if fc_continue in doobjectdestroyandreraisestate.newflowcontrol then
                  begin
                    ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
                    ctx.CurrAsmList.concat(taicpu.op_sym(a_br,oldContinueLabel));
                  end;

                { exit the 'break' block }
                ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));  // break
                if in_loop then
                  ctx.hlcg.a_label(ctx.CurrAsmList,NewBreakLabel);
                if fc_break in doobjectdestroyandreraisestate.newflowcontrol then
                  begin
                    ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
                    ctx.CurrAsmList.concat(taicpu.op_sym(a_br,oldBreakLabel));
                  end;

                { exit the 'exit' block }
                ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));  // exit
                ctx.hlcg.a_label(ctx.CurrAsmList,NewCurrExitLabel);
                if fc_exit in doobjectdestroyandreraisestate.newflowcontrol then
                  begin
                    ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
                    ctx.CurrAsmList.concat(taicpu.op_sym(a_br,oldCurrExitLabel));
                  end;

                compiler.current_procinfo.CurrExitLabel:=oldCurrExitLabel;
                if in_loop then
                  begin
                    compiler.current_procinfo.CurrContinueLabel:=oldContinueLabel;
                    compiler.current_procinfo.CurrBreakLabel:=oldBreakLabel;
                  end;

                ctx.CurrAsmList.concat(taicpu.op_sym(a_legacy_catch,current_asmdata.WeakRefAsmSymbol(FPC_EXCEPTION_TAG_SYM,AT_WASM_EXCEPTION_TAG)));

                ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_raise_nested',[],nil).resetiftemp;

                ctx.CurrAsmList.concat(taicpu.op_none(a_end_legacy_try));
              end
            else
              begin
                doobjectdestroyandreraisestate.newflowcontrol:=afteronflowcontrol;
                ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
              end;
          end
        else
          begin
            ctx.CurrAsmList.concat(taicpu.op_const(a_legacy_rethrow,0));
            doobjectdestroyandreraisestate.newflowcontrol:=afteronflowcontrol;
          end;

        ctx.CurrAsmList.concat(taicpu.op_none(a_end_legacy_try));

      errorexit:
        { return all used control flow statements }
        flowcontrol:=trystate.oldflowcontrol+(doobjectdestroyandreraisestate.newflowcontrol +
          trystate.newflowcontrol - [fc_inflowcontrol,fc_catching_exceptions]);
      end;

    procedure twasmtryexceptnode.pass_generate_code_bf_exceptions(ctx:tpassgeneratecodecontext);
      var
        trystate,doobjectdestroyandreraisestate: tcgexceptionstatehandler.texceptionstate;
        destroytemps,
        excepttemps: tcgexceptionstatehandler.texceptiontemps;
        afteronflowcontrol: tflowcontrol;
        oldCurrRaiseLabel,
        oldCurrExitLabel,
        oldContinueLabel,
        oldBreakLabel, NewContinueLabel, NewBreakLabel,
        NewCurrExitLabel, NewCurrRaiseLabel: tasmlabel;
        in_loop: Boolean;
      label
        errorexit;
      begin
        oldCurrRaiseLabel:=nil;
        oldCurrExitLabel:=nil;
        oldContinueLabel:=nil;
        oldBreakLabel:=nil;
        NewContinueLabel:=nil;
        NewBreakLabel:=nil;
        NewCurrRaiseLabel:=nil;
        location_reset(location,LOC_VOID,OS_NO);
        doobjectdestroyandreraisestate:=Default(tcgexceptionstatehandler.texceptionstate);

        { Exception temps? We don't need no stinking exception temps! :) }
        fillchar(excepttemps,sizeof(excepttemps),0);
        reference_reset(excepttemps.envbuf,0,[]);
        reference_reset(excepttemps.jmpbuf,0,[]);
        reference_reset(excepttemps.reasonbuf,0,[]);

        in_loop:=assigned(compiler.current_procinfo.CurrBreakLabel);

        compiler.exceptionstatehandler.new_exception(ctx.CurrAsmList,excepttemps,tek_except,trystate);

        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        ctx.CurrAsmList.concat(taicpu.op_none(a_block));
        oldCurrRaiseLabel:=tcpuprocinfo(compiler.current_procinfo).CurrRaiseLabel;
        current_asmdata.getjumplabel(NewCurrRaiseLabel);
        tcpuprocinfo(compiler.current_procinfo).CurrRaiseLabel:=NewCurrRaiseLabel;

        { try block }
        secondpass(left,ctx);
        if compiler.verbose.codegenerror then
          goto errorexit;

        compiler.exceptionstatehandler.end_try_block(ctx.CurrAsmList,tek_except,excepttemps,trystate,nil);

        ctx.CurrAsmList.concat(taicpu.op_const(a_br,1));
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
        ctx.hlcg.a_label(ctx.CurrAsmList,NewCurrRaiseLabel);

        ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_clear_exception_flag',[],nil).resetiftemp;
        tcpuprocinfo(compiler.current_procinfo).CurrRaiseLabel:=OldCurrRaiseLabel;

        flowcontrol:=[fc_inflowcontrol]+trystate.oldflowcontrol*[fc_catching_exceptions];
        { on statements }
        if assigned(right) then
          secondpass(right,ctx);

        afteronflowcontrol:=flowcontrol;

        { default handling except handling }
        if assigned(t1) then
          begin
            { FPC_CATCHES with 'default handler' flag (=-1) need no longer be called,
              it doesn't change any state and its return value is ignored (Sergei)
            }

            { the destruction of the exception object must be also }
            { guarded by an exception frame, but it can be omitted }
            { if there's no user code in 'except' block            }

            if not (has_no_code(t1)) then
              begin
                { if there is an outer frame that catches exceptions, remember this for the "except"
                  part of this try/except }
                flowcontrol:=trystate.oldflowcontrol*[fc_inflowcontrol,fc_catching_exceptions];
                { Exception temps? We don't need no stinking exception temps! :) }
                fillchar(excepttemps,sizeof(destroytemps),0);
                reference_reset(destroytemps.envbuf,0,[]);
                reference_reset(destroytemps.jmpbuf,0,[]);
                reference_reset(destroytemps.reasonbuf,0,[]);
                compiler.exceptionstatehandler.new_exception(ctx.CurrAsmList,destroytemps,tek_except,doobjectdestroyandreraisestate);
                { the flowcontrol from the default except-block must be merged
                  with the flowcontrol flags potentially set by the
                  on-statements handled above (secondpass(right)), as they are
                  at the same program level }
                flowcontrol:=
                  flowcontrol+
                  afteronflowcontrol;

                ctx.CurrAsmList.concat(taicpu.op_none(a_block));

                ctx.CurrAsmList.concat(taicpu.op_none(a_block));
                oldCurrRaiseLabel:=tcpuprocinfo(compiler.current_procinfo).CurrRaiseLabel;
                current_asmdata.getjumplabel(NewCurrRaiseLabel);
                tcpuprocinfo(compiler.current_procinfo).CurrRaiseLabel:=NewCurrRaiseLabel;

                { the 'exit' block }
                ctx.CurrAsmList.concat(taicpu.op_none(a_block));

                oldCurrExitLabel:=compiler.current_procinfo.CurrExitLabel;
                current_asmdata.getjumplabel(NewCurrExitLabel);
                compiler.current_procinfo.CurrExitLabel:=NewCurrExitLabel;

                { the 'break' block }
                ctx.CurrAsmList.concat(taicpu.op_none(a_block));

                if in_loop then
                  begin
                    oldBreakLabel:=compiler.current_procinfo.CurrBreakLabel;
                    current_asmdata.getjumplabel(NewBreakLabel);
                    compiler.current_procinfo.CurrBreakLabel:=NewBreakLabel;
                  end;

                { the 'continue' block }
                ctx.CurrAsmList.concat(taicpu.op_none(a_block));

                if in_loop then
                  begin
                    oldContinueLabel:=compiler.current_procinfo.CurrContinueLabel;
                    current_asmdata.getjumplabel(NewContinueLabel);
                    compiler.current_procinfo.CurrContinueLabel:=NewContinueLabel;
                  end;

                secondpass(t1,ctx);

                compiler.exceptionstatehandler.end_try_block(ctx.CurrAsmList,tek_except,destroytemps,doobjectdestroyandreraisestate,nil);

                ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
                ctx.CurrAsmList.concat(taicpu.op_const(a_br,4));

                { exit the 'continue' block }
                ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
                if in_loop then
                  ctx.hlcg.a_label(ctx.CurrAsmList,NewContinueLabel);
                if fc_continue in doobjectdestroyandreraisestate.newflowcontrol then
                  begin
                    ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
                    ctx.CurrAsmList.concat(taicpu.op_sym(a_br,OldContinueLabel));
                  end;

                { exit the 'break' block }
                ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));  // break
                if in_loop then
                  ctx.hlcg.a_label(ctx.CurrAsmList,NewBreakLabel);
                if fc_break in doobjectdestroyandreraisestate.newflowcontrol then
                  begin
                    ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
                    ctx.CurrAsmList.concat(taicpu.op_sym(a_br,OldBreakLabel));
                  end;

                { exit the 'exit' block }
                ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));  // exit
                ctx.hlcg.a_label(ctx.CurrAsmList,NewCurrExitLabel);
                if fc_exit in doobjectdestroyandreraisestate.newflowcontrol then
                  begin
                    ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
                    ctx.CurrAsmList.concat(taicpu.op_sym(a_br,oldCurrExitLabel));
                  end;

                ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
                ctx.hlcg.a_label(ctx.CurrAsmList,NewCurrRaiseLabel);

                compiler.current_procinfo.CurrExitLabel:=oldCurrExitLabel;
                if in_loop then
                  begin
                    compiler.current_procinfo.CurrContinueLabel:=oldContinueLabel;
                    compiler.current_procinfo.CurrBreakLabel:=oldBreakLabel;
                  end;
                tcpuprocinfo(compiler.current_procinfo).CurrRaiseLabel:=OldCurrRaiseLabel;

                ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_clear_exception_flag',[],nil).resetiftemp;
                ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_raise_nested',[],nil).resetiftemp;
                ctx.hlcg.g_maybe_checkforexceptions(ctx.CurrAsmList);

                ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
              end
            else
              begin
                doobjectdestroyandreraisestate.newflowcontrol:=afteronflowcontrol;
                ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
              end;
          end
        else
          begin
            ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_reraise',[],nil).resetiftemp;
            ctx.hlcg.g_maybe_checkforexceptions(ctx.CurrAsmList);
            doobjectdestroyandreraisestate.newflowcontrol:=afteronflowcontrol;
          end;

        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));

      errorexit:
        { return all used control flow statements }
        flowcontrol:=trystate.oldflowcontrol+(doobjectdestroyandreraisestate.newflowcontrol +
          trystate.newflowcontrol - [fc_inflowcontrol,fc_catching_exceptions]);
        tcpuprocinfo(compiler.current_procinfo).CurrRaiseLabel:=OldCurrRaiseLabel;
      end;

    procedure twasmtryexceptnode.pass_generate_code(ctx:tpassgeneratecodecontext);
      begin
        if ts_wasm_no_exceptions in compiler.globals.current_settings.targetswitches then
          pass_generate_code_no_exceptions(ctx)
        else if ts_wasm_native_exnref_exceptions in compiler.globals.current_settings.targetswitches then
          pass_generate_code_native_exnref_exceptions(ctx)
        else if ts_wasm_native_legacy_exceptions in compiler.globals.current_settings.targetswitches then
          pass_generate_code_native_legacy_exceptions(ctx)
        else if ts_wasm_bf_exceptions in compiler.globals.current_settings.targetswitches then
          pass_generate_code_bf_exceptions(ctx)
        else
          internalerror(2021091705);
      end;

{*****************************************************************************
                             twasmtryfinallynode
*****************************************************************************}

    procedure twasmtryfinallynode.pass_generate_code_no_exceptions(ctx:tpassgeneratecodecontext);
      var
        exitfinallylabel,
        continuefinallylabel,
        breakfinallylabel,
        oldCurrExitLabel,
        oldContinueLabel,
        oldBreakLabel: tasmlabel;
        finallyexceptionstate: tcgexceptionstatehandler.texceptionstate;
        excepttemps : tcgexceptionstatehandler.texceptiontemps;
        exceptframekind: tcgexceptionstatehandler.texceptframekind;
        in_loop: Boolean;

        procedure generate_exceptreason_check_br(reason: tcgint; br: aint);
          var
            reasonreg : tregister;
          begin
            reasonreg:=ctx.hlcg.getintregister(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype);
            ctx.hlcg.g_exception_reason_load(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,compiler.deftypes.exceptionreasontype,excepttemps.reasonbuf,reasonreg);
            thlcgwasm(ctx.hlcg).a_cmp_const_reg_stack(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,OC_EQ,reason,reasonreg);
            ctx.CurrAsmList.concat(taicpu.op_const(a_br_if,br));
            thlcgwasm(ctx.hlcg).decstack(ctx.CurrAsmList,1);
          end;

        procedure generate_exceptreason_check_br(reason: tcgint; l: TAsmLabel);
          var
            reasonreg : tregister;
          begin
            reasonreg:=ctx.hlcg.getintregister(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype);
            ctx.hlcg.g_exception_reason_load(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,compiler.deftypes.exceptionreasontype,excepttemps.reasonbuf,reasonreg);
            thlcgwasm(ctx.hlcg).a_cmp_const_reg_stack(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,OC_EQ,reason,reasonreg);
            ctx.CurrAsmList.concat(taicpu.op_sym(a_br_if,l));
            thlcgwasm(ctx.hlcg).decstack(ctx.CurrAsmList,1);
          end;

      begin
        location_reset(location,LOC_VOID,OS_NO);
        oldBreakLabel:=nil;
        oldContinueLabel:=nil;
        continuefinallylabel:=nil;
        breakfinallylabel:=nil;

        in_loop:=assigned(compiler.current_procinfo.CurrBreakLabel);

        if not implicitframe then
          exceptframekind:=tek_normalfinally
        else
          exceptframekind:=tek_implicitfinally;

        { in 'no exceptions' mode, we still want to handle properly exit,
          continue and break (they still need to execute the 'finally'
          statements), so for this we need excepttemps.reasonbuf, and for this
          reason, we need to allocate excepttemps }
        compiler.exceptionstatehandler.get_exception_temps(ctx.CurrAsmList,excepttemps);
        compiler.exceptionstatehandler.new_exception(ctx.CurrAsmList,excepttemps,exceptframekind,finallyexceptionstate);

        { the finally block must catch break, continue and exit }
        { statements                                            }

        { the outer 'try..finally' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        { the 'exit' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        oldCurrExitLabel:=compiler.current_procinfo.CurrExitLabel;
        exitfinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
        compiler.current_procinfo.CurrExitLabel:=exitfinallylabel;

        { the 'break' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldBreakLabel:=compiler.current_procinfo.CurrBreakLabel;
            breakfinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
            compiler.current_procinfo.CurrBreakLabel:=breakfinallylabel;
          end;

        { the 'continue' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldContinueLabel:=compiler.current_procinfo.CurrContinueLabel;
            continuefinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
            compiler.current_procinfo.CurrContinueLabel:=continuefinallylabel;
          end;

        { try code }
        if assigned(left) then
          begin
            secondpass(left,ctx);
            if compiler.verbose.codegenerror then
              exit;
          end;

        { don't generate line info for internal cleanup }
        ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));

        compiler.exceptionstatehandler.end_try_block(ctx.CurrAsmList,exceptframekind,excepttemps,finallyexceptionstate,nil);

        { we've reached the end of the 'try' block, with no exceptions/exit/break/continue, so set exceptionreason:=0 }
        ctx.hlcg.g_exception_reason_save_const(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,0,excepttemps.reasonbuf);
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,3)); // jump to the 'finally' section

        { exit the 'continue' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if in_loop then
          ctx.hlcg.a_label(ctx.CurrAsmList,continuefinallylabel);
        { exceptionreason:=4 (continue) }
        ctx.hlcg.g_exception_reason_save_const(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,4,excepttemps.reasonbuf);
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,2)); // jump to the 'finally' section

        { exit the 'break' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if in_loop then
          ctx.hlcg.a_label(ctx.CurrAsmList,breakfinallylabel);
        { exceptionreason:=3 (break) }
        ctx.hlcg.g_exception_reason_save_const(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,3,excepttemps.reasonbuf);
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,1)); // jump to the 'finally' section

        { exit the 'exit' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
        ctx.hlcg.a_label(ctx.CurrAsmList,exitfinallylabel);
        { exceptionreason:=2 (exit) }
        ctx.hlcg.g_exception_reason_save_const(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,2,excepttemps.reasonbuf);
        { proceed to the 'finally' section, which follow immediately, no need for jumps }

        { exit the outer 'try..finally' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));

        { end cleanup }
        ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

        { finally code (don't unconditionally set fc_inflowcontrol, since the
          finally code is unconditionally executed; we do have to filter out
          flags regarding break/contrinue/etc. because we have to give an
          error in case one of those is used in the finally-code }
        flowcontrol:=finallyexceptionstate.oldflowcontrol*[fc_inflowcontrol,fc_catching_exceptions];
        secondpass(right,ctx);
        { goto is allowed if it stays inside the finally block,
          this is checked using the exception block number }
        if (flowcontrol-[fc_gotolabel])<>(finallyexceptionstate.oldflowcontrol*[fc_inflowcontrol,fc_catching_exceptions]) then
          compiler.verbose.CGMessage(cg_e_control_flow_outside_finally);
        if compiler.verbose.codegenerror then
          exit;

        { don't generate line info for internal cleanup }
        ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));

        if fc_exit in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(2,oldCurrExitLabel);
        if fc_break in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(3,oldBreakLabel);
        if fc_continue in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(4,oldContinueLabel);

        compiler.exceptionstatehandler.unget_exception_temps(ctx.CurrAsmList,excepttemps);

        { end cleanup }
        ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

        compiler.current_procinfo.CurrExitLabel:=oldCurrExitLabel;
        if assigned(compiler.current_procinfo.CurrBreakLabel) then
         begin
           compiler.current_procinfo.CurrContinueLabel:=oldContinueLabel;
           compiler.current_procinfo.CurrBreakLabel:=oldBreakLabel;
         end;
        flowcontrol:=finallyexceptionstate.oldflowcontrol+(finallyexceptionstate.newflowcontrol-[fc_inflowcontrol,fc_catching_exceptions]);
      end;

    procedure twasmtryfinallynode.pass_generate_code_native_exnref_exceptions(ctx:tpassgeneratecodecontext);
      var
        exitfinallylabel,
        continuefinallylabel,
        breakfinallylabel,
        oldCurrExitLabel,
        oldContinueLabel,
        oldBreakLabel: tasmlabel;
        finallyexceptionstate: tcgexceptionstatehandler.texceptionstate;
        excepttemps : tcgexceptionstatehandler.texceptiontemps;
        exceptframekind: tcgexceptionstatehandler.texceptframekind;
        in_loop: Boolean;

      procedure generate_exceptreason_check_br(reason: tcgint; br: aint);
        var
          reasonreg : tregister;
        begin
          reasonreg:=ctx.hlcg.getintregister(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype);
          ctx.hlcg.g_exception_reason_load(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,compiler.deftypes.exceptionreasontype,excepttemps.reasonbuf,reasonreg);
          thlcgwasm(ctx.hlcg).a_cmp_const_reg_stack(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,OC_EQ,reason,reasonreg);
          ctx.CurrAsmList.concat(taicpu.op_const(a_br_if,br));
          thlcgwasm(ctx.hlcg).decstack(ctx.CurrAsmList,1);
        end;

      procedure generate_exceptreason_check_br(reason: tcgint; l: tasmlabel);
        var
          reasonreg : tregister;
        begin
          reasonreg:=ctx.hlcg.getintregister(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype);
          ctx.hlcg.g_exception_reason_load(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,compiler.deftypes.exceptionreasontype,excepttemps.reasonbuf,reasonreg);
          thlcgwasm(ctx.hlcg).a_cmp_const_reg_stack(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,OC_EQ,reason,reasonreg);
          ctx.CurrAsmList.concat(taicpu.op_sym(a_br_if,l));
          thlcgwasm(ctx.hlcg).decstack(ctx.CurrAsmList,1);
        end;

      procedure generate_exceptreason_throw(reason: tcgint);
        var
          reasonreg : tregister;
        begin
          reasonreg:=ctx.hlcg.getintregister(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype);
          ctx.hlcg.g_exception_reason_load(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,compiler.deftypes.exceptionreasontype,excepttemps.reasonbuf,reasonreg);
          thlcgwasm(ctx.hlcg).a_cmp_const_reg_stack(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,OC_EQ,reason,reasonreg);
          ctx.CurrAsmList.concat(taicpu.op_none(a_if));
          thlcgwasm(ctx.hlcg).decstack(ctx.CurrAsmList,1);
          ctx.CurrAsmList.Concat(taicpu.op_sym(a_throw,current_asmdata.WeakRefAsmSymbol(FPC_EXCEPTION_TAG_SYM,AT_WASM_EXCEPTION_TAG)));
          ctx.CurrAsmList.concat(taicpu.op_none(a_end_if));
        end;

      begin
        location_reset(location,LOC_VOID,OS_NO);
        oldBreakLabel:=nil;
        oldContinueLabel:=nil;
        continuefinallylabel:=nil;
        breakfinallylabel:=nil;

        in_loop:=assigned(compiler.current_procinfo.CurrBreakLabel);

        if not implicitframe then
          exceptframekind:=tek_normalfinally
        else
          exceptframekind:=tek_implicitfinally;

        { in 'no exceptions' mode, we still want to handle properly exit,
          continue and break (they still need to execute the 'finally'
          statements), so for this we need excepttemps.reasonbuf, and for this
          reason, we need to allocate excepttemps }
        compiler.exceptionstatehandler.get_exception_temps(ctx.CurrAsmList,excepttemps);
        compiler.exceptionstatehandler.new_exception(ctx.CurrAsmList,excepttemps,exceptframekind,finallyexceptionstate);

        { the finally block must catch break, continue and exit }
        { statements                                            }

        { the outer 'try..finally' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        { the 'exit' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        oldCurrExitLabel:=compiler.current_procinfo.CurrExitLabel;
        exitfinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
        compiler.current_procinfo.CurrExitLabel:=exitfinallylabel;

        { the 'break' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldBreakLabel:=compiler.current_procinfo.CurrBreakLabel;
            breakfinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
            compiler.current_procinfo.CurrBreakLabel:=breakfinallylabel;
          end;

        { the 'continue' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldContinueLabel:=compiler.current_procinfo.CurrContinueLabel;
            continuefinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
            compiler.current_procinfo.CurrContinueLabel:=continuefinallylabel;
          end;

        { the 'catch' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        { the inner 'try_table..end_try_table' block }
        ctx.CurrAsmList.concat(taicpu.op_catch(a_try_table,[taicpu.op_sym_const(a_catch,current_asmdata.WeakRefAsmSymbol(FPC_EXCEPTION_TAG_SYM,AT_WASM_EXCEPTION_TAG),0)]));

        { try code }
        if assigned(left) then
          begin
            secondpass(left,ctx);
            if compiler.verbose.codegenerror then
              exit;
          end;

        { don't generate line info for internal cleanup }
        ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));

        compiler.exceptionstatehandler.end_try_block(ctx.CurrAsmList,exceptframekind,excepttemps,finallyexceptionstate,nil);

        { exit the inner 'try_table..end_try_table' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_try_table));

        { we've reached the end of the 'try' block, with no exceptions/exit/break/continue, so set exceptionreason:=0 }
        ctx.hlcg.g_exception_reason_save_const(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,0,excepttemps.reasonbuf);
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,4)); // jump to the 'finally' section

        { exit the 'catch' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));

        { exceptionreason:=1 (exception) }
        ctx.hlcg.g_exception_reason_save_const(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,1,excepttemps.reasonbuf);
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,3)); // jump to the 'finally' section

        { exit the 'continue' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if in_loop then
          ctx.hlcg.a_label(ctx.CurrAsmList,continuefinallylabel);
        { exceptionreason:=4 (continue) }
        ctx.hlcg.g_exception_reason_save_const(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,4,excepttemps.reasonbuf);
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,2)); // jump to the 'finally' section

        { exit the 'break' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if in_loop then
          ctx.hlcg.a_label(ctx.CurrAsmList,breakfinallylabel);
        { exceptionreason:=3 (break) }
        ctx.hlcg.g_exception_reason_save_const(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,3,excepttemps.reasonbuf);
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,1)); // jump to the 'finally' section

        { exit the 'exit' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
        ctx.hlcg.a_label(ctx.CurrAsmList,exitfinallylabel);
        { exceptionreason:=2 (exit) }
        ctx.hlcg.g_exception_reason_save_const(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,2,excepttemps.reasonbuf);
        { proceed to the 'finally' section, which follow immediately, no need for jumps }

        { exit the outer 'try..finally' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));

        { end cleanup }
        ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

        { finally code (don't unconditionally set fc_inflowcontrol, since the
          finally code is unconditionally executed; we do have to filter out
          flags regarding break/contrinue/etc. because we have to give an
          error in case one of those is used in the finally-code }
        flowcontrol:=finallyexceptionstate.oldflowcontrol*[fc_inflowcontrol,fc_catching_exceptions];
        secondpass(right,ctx);
        { goto is allowed if it stays inside the finally block,
          this is checked using the exception block number }
        if (flowcontrol-[fc_gotolabel])<>(finallyexceptionstate.oldflowcontrol*[fc_inflowcontrol,fc_catching_exceptions]) then
          compiler.verbose.CGMessage(cg_e_control_flow_outside_finally);
        if compiler.verbose.codegenerror then
          exit;

        { don't generate line info for internal cleanup }
        ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));

        if fc_exit in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(2,oldCurrExitLabel);
        if fc_break in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(3,oldBreakLabel);
        if fc_continue in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(4,oldContinueLabel);
        generate_exceptreason_throw(1);

        compiler.exceptionstatehandler.unget_exception_temps(ctx.CurrAsmList,excepttemps);

        { end cleanup }
        ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

        compiler.current_procinfo.CurrExitLabel:=oldCurrExitLabel;
        if assigned(compiler.current_procinfo.CurrBreakLabel) then
         begin
           compiler.current_procinfo.CurrContinueLabel:=oldContinueLabel;
           compiler.current_procinfo.CurrBreakLabel:=oldBreakLabel;
         end;
        flowcontrol:=finallyexceptionstate.oldflowcontrol+(finallyexceptionstate.newflowcontrol-[fc_inflowcontrol,fc_catching_exceptions]);
      end;

    procedure twasmtryfinallynode.pass_generate_code_native_legacy_exceptions(ctx:tpassgeneratecodecontext);
      var
        exitfinallylabel,
        continuefinallylabel,
        breakfinallylabel,
        oldCurrExitLabel,
        oldContinueLabel,
        oldBreakLabel: tasmlabel;
        finallyexceptionstate: tcgexceptionstatehandler.texceptionstate;
        excepttemps : tcgexceptionstatehandler.texceptiontemps;
        exceptframekind: tcgexceptionstatehandler.texceptframekind;
        in_loop: Boolean;

      procedure generate_exceptreason_check_br(reason: tcgint; br: aint);
        var
          reasonreg : tregister;
        begin
          reasonreg:=ctx.hlcg.getintregister(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype);
          ctx.hlcg.g_exception_reason_load(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,compiler.deftypes.exceptionreasontype,excepttemps.reasonbuf,reasonreg);
          thlcgwasm(ctx.hlcg).a_cmp_const_reg_stack(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,OC_EQ,reason,reasonreg);
          ctx.CurrAsmList.concat(taicpu.op_const(a_br_if,br));
          thlcgwasm(ctx.hlcg).decstack(ctx.CurrAsmList,1);
        end;

      procedure generate_exceptreason_check_br(reason: tcgint; l: tasmlabel);
        var
          reasonreg : tregister;
        begin
          reasonreg:=ctx.hlcg.getintregister(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype);
          ctx.hlcg.g_exception_reason_load(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,compiler.deftypes.exceptionreasontype,excepttemps.reasonbuf,reasonreg);
          thlcgwasm(ctx.hlcg).a_cmp_const_reg_stack(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,OC_EQ,reason,reasonreg);
          ctx.CurrAsmList.concat(taicpu.op_sym(a_br_if,l));
          thlcgwasm(ctx.hlcg).decstack(ctx.CurrAsmList,1);
        end;

      procedure generate_exceptreason_throw(reason: tcgint);
        var
          reasonreg : tregister;
        begin
          reasonreg:=ctx.hlcg.getintregister(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype);
          ctx.hlcg.g_exception_reason_load(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,compiler.deftypes.exceptionreasontype,excepttemps.reasonbuf,reasonreg);
          thlcgwasm(ctx.hlcg).a_cmp_const_reg_stack(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,OC_EQ,reason,reasonreg);
          ctx.CurrAsmList.concat(taicpu.op_none(a_if));
          thlcgwasm(ctx.hlcg).decstack(ctx.CurrAsmList,1);
          ctx.CurrAsmList.Concat(taicpu.op_sym(a_legacy_throw,current_asmdata.WeakRefAsmSymbol(FPC_EXCEPTION_TAG_SYM,AT_WASM_EXCEPTION_TAG)));
          ctx.CurrAsmList.concat(taicpu.op_none(a_end_if));
        end;

      begin
        location_reset(location,LOC_VOID,OS_NO);
        oldBreakLabel:=nil;
        oldContinueLabel:=nil;
        continuefinallylabel:=nil;
        breakfinallylabel:=nil;

        in_loop:=assigned(compiler.current_procinfo.CurrBreakLabel);

        if not implicitframe then
          exceptframekind:=tek_normalfinally
        else
          exceptframekind:=tek_implicitfinally;

        { in 'no exceptions' mode, we still want to handle properly exit,
          continue and break (they still need to execute the 'finally'
          statements), so for this we need excepttemps.reasonbuf, and for this
          reason, we need to allocate excepttemps }
        compiler.exceptionstatehandler.get_exception_temps(ctx.CurrAsmList,excepttemps);
        compiler.exceptionstatehandler.new_exception(ctx.CurrAsmList,excepttemps,exceptframekind,finallyexceptionstate);

        { the finally block must catch break, continue and exit }
        { statements                                            }

        { the outer 'try..finally' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        { the 'exit' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        oldCurrExitLabel:=compiler.current_procinfo.CurrExitLabel;
        exitfinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
        compiler.current_procinfo.CurrExitLabel:=exitfinallylabel;

        { the 'break' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldBreakLabel:=compiler.current_procinfo.CurrBreakLabel;
            breakfinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
            compiler.current_procinfo.CurrBreakLabel:=breakfinallylabel;
          end;

        { the 'continue' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldContinueLabel:=compiler.current_procinfo.CurrContinueLabel;
            continuefinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
            compiler.current_procinfo.CurrContinueLabel:=continuefinallylabel;
          end;

        { the inner 'try..end_try' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_legacy_try));

        { try code }
        if assigned(left) then
          begin
            secondpass(left,ctx);
            if compiler.verbose.codegenerror then
              exit;
          end;

        { don't generate line info for internal cleanup }
        ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));

        compiler.exceptionstatehandler.end_try_block(ctx.CurrAsmList,exceptframekind,excepttemps,finallyexceptionstate,nil);

        { we've reached the end of the 'try' block, with no exceptions/exit/break/continue, so set exceptionreason:=0 }
        ctx.hlcg.g_exception_reason_save_const(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,0,excepttemps.reasonbuf);
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,4)); // jump to the 'finally' section

        ctx.CurrAsmList.concat(taicpu.op_sym(a_legacy_catch,current_asmdata.WeakRefAsmSymbol(FPC_EXCEPTION_TAG_SYM,AT_WASM_EXCEPTION_TAG)));
        { exceptionreason:=1 (exception) }
        ctx.hlcg.g_exception_reason_save_const(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,1,excepttemps.reasonbuf);
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,4)); // jump to the 'finally' section

        { exit the inner 'try..end_try' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_legacy_try));

        { exit the 'continue' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if in_loop then
          ctx.hlcg.a_label(ctx.CurrAsmList,continuefinallylabel);
        { exceptionreason:=4 (continue) }
        ctx.hlcg.g_exception_reason_save_const(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,4,excepttemps.reasonbuf);
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,2)); // jump to the 'finally' section

        { exit the 'break' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if in_loop then
          ctx.hlcg.a_label(ctx.CurrAsmList,breakfinallylabel);
        { exceptionreason:=3 (break) }
        ctx.hlcg.g_exception_reason_save_const(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,3,excepttemps.reasonbuf);
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,1)); // jump to the 'finally' section

        { exit the 'exit' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
        ctx.hlcg.a_label(ctx.CurrAsmList,exitfinallylabel);
        { exceptionreason:=2 (exit) }
        ctx.hlcg.g_exception_reason_save_const(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,2,excepttemps.reasonbuf);
        { proceed to the 'finally' section, which follow immediately, no need for jumps }

        { exit the outer 'try..finally' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));

        { end cleanup }
        ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

        { finally code (don't unconditionally set fc_inflowcontrol, since the
          finally code is unconditionally executed; we do have to filter out
          flags regarding break/contrinue/etc. because we have to give an
          error in case one of those is used in the finally-code }
        flowcontrol:=finallyexceptionstate.oldflowcontrol*[fc_inflowcontrol,fc_catching_exceptions];
        secondpass(right,ctx);
        { goto is allowed if it stays inside the finally block,
          this is checked using the exception block number }
        if (flowcontrol-[fc_gotolabel])<>(finallyexceptionstate.oldflowcontrol*[fc_inflowcontrol,fc_catching_exceptions]) then
          compiler.verbose.CGMessage(cg_e_control_flow_outside_finally);
        if compiler.verbose.codegenerror then
          exit;

        { don't generate line info for internal cleanup }
        ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));

        if fc_exit in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(2,oldCurrExitLabel);
        if fc_break in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(3,oldBreakLabel);
        if fc_continue in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(4,oldContinueLabel);
        generate_exceptreason_throw(1);

        compiler.exceptionstatehandler.unget_exception_temps(ctx.CurrAsmList,excepttemps);

        { end cleanup }
        ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

        compiler.current_procinfo.CurrExitLabel:=oldCurrExitLabel;
        if assigned(compiler.current_procinfo.CurrBreakLabel) then
         begin
           compiler.current_procinfo.CurrContinueLabel:=oldContinueLabel;
           compiler.current_procinfo.CurrBreakLabel:=oldBreakLabel;
         end;
        flowcontrol:=finallyexceptionstate.oldflowcontrol+(finallyexceptionstate.newflowcontrol-[fc_inflowcontrol,fc_catching_exceptions]);
      end;

    procedure twasmtryfinallynode.pass_generate_code_bf_exceptions(ctx:tpassgeneratecodecontext);
      var
        raisefinallylabel,
        exitfinallylabel,
        continuefinallylabel,
        breakfinallylabel,
        oldCurrRaiseLabel,
        oldCurrExitLabel,
        oldContinueLabel,
        oldBreakLabel: tasmlabel;
        finallyexceptionstate: tcgexceptionstatehandler.texceptionstate;
        excepttemps : tcgexceptionstatehandler.texceptiontemps;
        exceptframekind: tcgexceptionstatehandler.texceptframekind;
        in_loop: Boolean;

      procedure generate_exceptreason_check_br(reason: tcgint; br: aint);
        var
          reasonreg : tregister;
        begin
          reasonreg:=ctx.hlcg.getintregister(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype);
          ctx.hlcg.g_exception_reason_load(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,compiler.deftypes.exceptionreasontype,excepttemps.reasonbuf,reasonreg);
          thlcgwasm(ctx.hlcg).a_cmp_const_reg_stack(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,OC_EQ,reason,reasonreg);
          ctx.CurrAsmList.concat(taicpu.op_const(a_br_if,br));
          thlcgwasm(ctx.hlcg).decstack(ctx.CurrAsmList,1);
        end;

      procedure generate_exceptreason_check_br(reason: tcgint; l: tasmsymbol);
        var
          reasonreg : tregister;
        begin
          reasonreg:=ctx.hlcg.getintregister(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype);
          ctx.hlcg.g_exception_reason_load(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,compiler.deftypes.exceptionreasontype,excepttemps.reasonbuf,reasonreg);
          thlcgwasm(ctx.hlcg).a_cmp_const_reg_stack(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,OC_EQ,reason,reasonreg);
          ctx.CurrAsmList.concat(taicpu.op_sym(a_br_if,l));
          thlcgwasm(ctx.hlcg).decstack(ctx.CurrAsmList,1);
        end;

      procedure generate_exceptreason_reraise(reason: tcgint);
        var
          reasonreg : tregister;
        begin
          reasonreg:=ctx.hlcg.getintregister(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype);
          ctx.hlcg.g_exception_reason_load(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,compiler.deftypes.exceptionreasontype,excepttemps.reasonbuf,reasonreg);
          thlcgwasm(ctx.hlcg).a_cmp_const_reg_stack(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,OC_EQ,reason,reasonreg);
          ctx.CurrAsmList.concat(taicpu.op_none(a_if));
          thlcgwasm(ctx.hlcg).decstack(ctx.CurrAsmList,1);
          ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_reraise',[],nil).resetiftemp;
          ctx.hlcg.g_maybe_checkforexceptions(ctx.CurrAsmList);
          ctx.CurrAsmList.concat(taicpu.op_none(a_end_if));
        end;

      begin
        location_reset(location,LOC_VOID,OS_NO);
        oldCurrRaiseLabel:=nil;
        oldBreakLabel:=nil;
        oldContinueLabel:=nil;
        continuefinallylabel:=nil;
        breakfinallylabel:=nil;

        in_loop:=assigned(compiler.current_procinfo.CurrBreakLabel);

        if not implicitframe then
          exceptframekind:=tek_normalfinally
        else
          exceptframekind:=tek_implicitfinally;

        { in 'no exceptions' mode, we still want to handle properly exit,
          continue and break (they still need to execute the 'finally'
          statements), so for this we need excepttemps.reasonbuf, and for this
          reason, we need to allocate excepttemps }
        compiler.exceptionstatehandler.get_exception_temps(ctx.CurrAsmList,excepttemps);
        compiler.exceptionstatehandler.new_exception(ctx.CurrAsmList,excepttemps,exceptframekind,finallyexceptionstate);

        { the finally block must catch break, continue and exit }
        { statements                                            }

        { the outer 'try..finally' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        { the 'exit' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        oldCurrExitLabel:=compiler.current_procinfo.CurrExitLabel;
        exitfinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
        compiler.current_procinfo.CurrExitLabel:=exitfinallylabel;

        { the 'break' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldBreakLabel:=compiler.current_procinfo.CurrBreakLabel;
            breakfinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
            compiler.current_procinfo.CurrBreakLabel:=breakfinallylabel;
          end;

        { the 'continue' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldContinueLabel:=compiler.current_procinfo.CurrContinueLabel;
            continuefinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
            compiler.current_procinfo.CurrContinueLabel:=continuefinallylabel;
          end;

        { the inner 'try..end_try' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));
        oldCurrRaiseLabel:=tcpuprocinfo(compiler.current_procinfo).CurrRaiseLabel;
        current_asmdata.getjumplabel(raisefinallylabel);
        tcpuprocinfo(compiler.current_procinfo).CurrRaiseLabel:=raisefinallylabel;

        { try code }
        if assigned(left) then
          begin
            secondpass(left,ctx);
            if compiler.verbose.codegenerror then
              exit;
          end;

        { don't generate line info for internal cleanup }
        ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));

        compiler.exceptionstatehandler.end_try_block(ctx.CurrAsmList,exceptframekind,excepttemps,finallyexceptionstate,nil);

        { we've reached the end of the 'try' block, with no exceptions/exit/break/continue, so set exceptionreason:=0 }
        ctx.hlcg.g_exception_reason_save_const(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,0,excepttemps.reasonbuf);
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,4)); // jump to the 'finally' section

        { exit the inner 'try..end_try' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
        ctx.hlcg.a_label(ctx.CurrAsmList,raisefinallylabel);

        { exceptionreason:=1 (exception) }
        ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_clear_exception_flag',[],nil).resetiftemp;
        ctx.hlcg.g_exception_reason_save_const(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,1,excepttemps.reasonbuf);
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,3)); // jump to the 'finally' section

        { exit the 'continue' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if in_loop then
          ctx.hlcg.a_label(ctx.CurrAsmList,continuefinallylabel);
        { exceptionreason:=4 (continue) }
        ctx.hlcg.g_exception_reason_save_const(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,4,excepttemps.reasonbuf);
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,2)); // jump to the 'finally' section

        { exit the 'break' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if in_loop then
          ctx.hlcg.a_label(ctx.CurrAsmList,breakfinallylabel);
        { exceptionreason:=3 (break) }
        ctx.hlcg.g_exception_reason_save_const(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,3,excepttemps.reasonbuf);
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,1)); // jump to the 'finally' section

        { exit the 'exit' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
        ctx.hlcg.a_label(ctx.CurrAsmList,exitfinallylabel);
        { exceptionreason:=2 (exit) }
        ctx.hlcg.g_exception_reason_save_const(ctx.CurrAsmList,compiler.deftypes.exceptionreasontype,2,excepttemps.reasonbuf);
        { proceed to the 'finally' section, which follow immediately, no need for jumps }

        { exit the outer 'try..finally' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));

        { end cleanup }
        ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

        tcpuprocinfo(compiler.current_procinfo).CurrRaiseLabel:=oldCurrRaiseLabel;

        { finally code (don't unconditionally set fc_inflowcontrol, since the
          finally code is unconditionally executed; we do have to filter out
          flags regarding break/contrinue/etc. because we have to give an
          error in case one of those is used in the finally-code }
        flowcontrol:=finallyexceptionstate.oldflowcontrol*[fc_inflowcontrol,fc_catching_exceptions];
        secondpass(right,ctx);
        { goto is allowed if it stays inside the finally block,
          this is checked using the exception block number }
        if (flowcontrol-[fc_gotolabel])<>(finallyexceptionstate.oldflowcontrol*[fc_inflowcontrol,fc_catching_exceptions]) then
          compiler.verbose.CGMessage(cg_e_control_flow_outside_finally);
        if compiler.verbose.codegenerror then
          exit;

        { don't generate line info for internal cleanup }
        ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));

        if fc_exit in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(2,oldCurrExitLabel);
        if fc_break in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(3,oldBreakLabel);
        if fc_continue in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(4,oldContinueLabel);
        generate_exceptreason_reraise(1);

        compiler.exceptionstatehandler.unget_exception_temps(ctx.CurrAsmList,excepttemps);

        { end cleanup }
        ctx.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

        compiler.current_procinfo.CurrExitLabel:=oldCurrExitLabel;
        if assigned(compiler.current_procinfo.CurrBreakLabel) then
         begin
           compiler.current_procinfo.CurrContinueLabel:=oldContinueLabel;
           compiler.current_procinfo.CurrBreakLabel:=oldBreakLabel;
         end;
        flowcontrol:=finallyexceptionstate.oldflowcontrol+(finallyexceptionstate.newflowcontrol-[fc_inflowcontrol,fc_catching_exceptions]);
      end;

    procedure twasmtryfinallynode.pass_generate_code(ctx:tpassgeneratecodecontext);
      begin
        if ts_wasm_no_exceptions in compiler.globals.current_settings.targetswitches then
          pass_generate_code_no_exceptions(ctx)
        else if ts_wasm_native_exnref_exceptions in compiler.globals.current_settings.targetswitches then
          pass_generate_code_native_exnref_exceptions(ctx)
        else if ts_wasm_native_legacy_exceptions in compiler.globals.current_settings.targetswitches then
          pass_generate_code_native_legacy_exceptions(ctx)
        else if ts_wasm_bf_exceptions in compiler.globals.current_settings.targetswitches then
          pass_generate_code_bf_exceptions(ctx)
        else
          internalerror(2021091704);
      end;

{*****************************************************************************
                                  twasmonnode
*****************************************************************************}

    procedure twasmonnode.pass_generate_code_no_exceptions(ctx:tpassgeneratecodecontext);
      begin
        { should not be called }
        internalerror(2021092803);
      end;

    procedure twasmonnode.pass_generate_code_native_exnref_exceptions(ctx:tpassgeneratecodecontext);
      var
        exceptvarsym : tlocalvarsym;
        exceptlocdef: tdef;
        exceptlocreg: tregister;
        oldCurrExitLabel,
        oldContinueLabel,
        oldBreakLabel, NewContinueLabel, NewBreakLabel,
        NewCurrExitLabel: tasmlabel;
        in_loop: Boolean;
        doobjectdestroyandreraisestate: tcgexceptionstatehandler.texceptionstate;
        excepttemps: tcgexceptionstatehandler.texceptiontemps;
      begin
        oldCurrExitLabel:=nil;
        oldContinueLabel:=nil;
        oldBreakLabel:=nil;
        NewBreakLabel:=nil;
        NewContinueLabel:=nil;
        location_reset(location,LOC_VOID,OS_NO);

        { Exception temps? We don't need no stinking exception temps! :) }
        fillchar(excepttemps,sizeof(excepttemps),0);
        reference_reset(excepttemps.envbuf,0,[]);
        reference_reset(excepttemps.jmpbuf,0,[]);
        reference_reset(excepttemps.reasonbuf,0,[]);

        in_loop:=assigned(compiler.current_procinfo.CurrBreakLabel);

        compiler.exceptionstatehandler.begin_catch(ctx.CurrAsmList,excepttype,nil,exceptlocdef,exceptlocreg);

        { Retrieve exception variable }
        if assigned(excepTSymtable) then
          exceptvarsym:=tlocalvarsym(excepTSymtable.SymList[0])
        else
          internalerror(2011020401);

        if assigned(exceptvarsym) then
          begin
            location_reset_ref(exceptvarsym.localloc, LOC_REFERENCE, def_cgsize(compiler.deftypes.voidpointertype), compiler.deftypes.voidpointertype.alignment, []);
            ctx.tg.GetLocal(ctx.CurrAsmList, exceptvarsym.vardef.size, exceptvarsym.vardef, exceptvarsym.localloc.reference);
            ctx.hlcg.a_load_reg_ref(ctx.CurrAsmList, exceptlocdef, exceptvarsym.vardef, exceptlocreg, exceptvarsym.localloc.reference);
          end;

        compiler.exceptionstatehandler.new_exception(ctx.CurrAsmList,excepttemps,tek_except,doobjectdestroyandreraisestate);

        { in the case that another exception is risen
          we've to destroy the old one, so create a new
          exception frame for the catch-handler }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));
        ctx.CurrAsmList.concat(taicpu.op_catch(a_try_table,[taicpu.op_sym_const(a_catch,current_asmdata.WeakRefAsmSymbol(FPC_EXCEPTION_TAG_SYM,AT_WASM_EXCEPTION_TAG),0)]));

        { the 'exit' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        oldCurrExitLabel:=compiler.current_procinfo.CurrExitLabel;
        current_asmdata.getjumplabel(NewCurrExitLabel);
        compiler.current_procinfo.CurrExitLabel:=NewCurrExitLabel;

        { the 'break' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldBreakLabel:=compiler.current_procinfo.CurrBreakLabel;
            current_asmdata.getjumplabel(NewBreakLabel);
            compiler.current_procinfo.CurrBreakLabel:=NewBreakLabel;
          end;

        { the 'continue' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldContinueLabel:=compiler.current_procinfo.CurrContinueLabel;
            current_asmdata.getjumplabel(NewContinueLabel);
            compiler.current_procinfo.CurrContinueLabel:=NewContinueLabel;
          end;

        if assigned(right) then
          secondpass(right,ctx);

        compiler.exceptionstatehandler.end_try_block(ctx.CurrAsmList,tek_except,excepttemps,doobjectdestroyandreraisestate,nil);

        ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,7));

        { exit the 'continue' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if in_loop then
          ctx.hlcg.a_label(ctx.CurrAsmList,NewContinueLabel);
        if fc_continue in doobjectdestroyandreraisestate.newflowcontrol then
          begin
            ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
            ctx.CurrAsmList.concat(taicpu.op_sym(a_br,oldContinueLabel));
          end;

        { exit the 'break' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));  // break
        if in_loop then
          ctx.hlcg.a_label(ctx.CurrAsmList,NewBreakLabel);
        if fc_break in doobjectdestroyandreraisestate.newflowcontrol then
          begin
            ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
            ctx.CurrAsmList.concat(taicpu.op_sym(a_br,oldBreakLabel));
          end;

        { exit the 'exit' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));  // exit
        ctx.hlcg.a_label(ctx.CurrAsmList,NewCurrExitLabel);
        if fc_exit in doobjectdestroyandreraisestate.newflowcontrol then
          begin
            ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
            ctx.CurrAsmList.concat(taicpu.op_sym(a_br,oldCurrExitLabel));
          end;

        compiler.current_procinfo.CurrExitLabel:=oldCurrExitLabel;
        if in_loop then
          begin
            compiler.current_procinfo.CurrContinueLabel:=oldContinueLabel;
            compiler.current_procinfo.CurrBreakLabel:=oldBreakLabel;
          end;

        ctx.CurrAsmList.concat(taicpu.op_none(a_end_try_table));
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,1));
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));

        ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_raise_nested',[],nil).resetiftemp;

        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));

        { clear some stuff }
        if assigned(exceptvarsym) then
          begin
            ctx.tg.UngetLocal(ctx.CurrAsmList,exceptvarsym.localloc.reference);
            exceptvarsym.localloc.loc:=LOC_INVALID;
          end;
        compiler.exceptionstatehandler.end_catch(ctx.CurrAsmList);

        { propagate exit/break/continue }
        flowcontrol:=doobjectdestroyandreraisestate.oldflowcontrol+(doobjectdestroyandreraisestate.newflowcontrol-[fc_inflowcontrol,fc_catching_exceptions]);

        { next on node }
        if assigned(left) then
          secondpass(left,ctx);
      end;

    procedure twasmonnode.pass_generate_code_native_legacy_exceptions(ctx:tpassgeneratecodecontext);
      var
        exceptvarsym : tlocalvarsym;
        exceptlocdef: tdef;
        exceptlocreg: tregister;
        oldCurrExitLabel,
        oldContinueLabel,
        oldBreakLabel, NewContinueLabel, NewBreakLabel,
        NewCurrExitLabel: tasmlabel;
        in_loop: Boolean;
        doobjectdestroyandreraisestate: tcgexceptionstatehandler.texceptionstate;
        excepttemps: tcgexceptionstatehandler.texceptiontemps;
      begin
        oldCurrExitLabel:=nil;
        oldContinueLabel:=nil;
        oldBreakLabel:=nil;
        NewBreakLabel:=nil;
        NewContinueLabel:=nil;
        location_reset(location,LOC_VOID,OS_NO);

        { Exception temps? We don't need no stinking exception temps! :) }
        fillchar(excepttemps,sizeof(excepttemps),0);
        reference_reset(excepttemps.envbuf,0,[]);
        reference_reset(excepttemps.jmpbuf,0,[]);
        reference_reset(excepttemps.reasonbuf,0,[]);

        in_loop:=assigned(compiler.current_procinfo.CurrBreakLabel);

        compiler.exceptionstatehandler.begin_catch(ctx.CurrAsmList,excepttype,nil,exceptlocdef,exceptlocreg);

        { Retrieve exception variable }
        if assigned(excepTSymtable) then
          exceptvarsym:=tlocalvarsym(excepTSymtable.SymList[0])
        else
          internalerror(2011020401);

        if assigned(exceptvarsym) then
          begin
            location_reset_ref(exceptvarsym.localloc, LOC_REFERENCE, def_cgsize(compiler.deftypes.voidpointertype), compiler.deftypes.voidpointertype.alignment, []);
            ctx.tg.GetLocal(ctx.CurrAsmList, exceptvarsym.vardef.size, exceptvarsym.vardef, exceptvarsym.localloc.reference);
            ctx.hlcg.a_load_reg_ref(ctx.CurrAsmList, exceptlocdef, exceptvarsym.vardef, exceptlocreg, exceptvarsym.localloc.reference);
          end;

        compiler.exceptionstatehandler.new_exception(ctx.CurrAsmList,excepttemps,tek_except,doobjectdestroyandreraisestate);

        { in the case that another exception is risen
          we've to destroy the old one, so create a new
          exception frame for the catch-handler }
        ctx.CurrAsmList.concat(taicpu.op_none(a_legacy_try));

        { the 'exit' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        oldCurrExitLabel:=compiler.current_procinfo.CurrExitLabel;
        current_asmdata.getjumplabel(NewCurrExitLabel);
        compiler.current_procinfo.CurrExitLabel:=NewCurrExitLabel;

        { the 'break' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldBreakLabel:=compiler.current_procinfo.CurrBreakLabel;
            current_asmdata.getjumplabel(NewBreakLabel);
            compiler.current_procinfo.CurrBreakLabel:=NewBreakLabel;
          end;

        { the 'continue' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldContinueLabel:=compiler.current_procinfo.CurrContinueLabel;
            current_asmdata.getjumplabel(NewContinueLabel);
            compiler.current_procinfo.CurrContinueLabel:=NewContinueLabel;
          end;

        if assigned(right) then
          secondpass(right,ctx);

        compiler.exceptionstatehandler.end_try_block(ctx.CurrAsmList,tek_except,excepttemps,doobjectdestroyandreraisestate,nil);

        ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,5));

        { exit the 'continue' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if in_loop then
          ctx.hlcg.a_label(ctx.CurrAsmList,NewContinueLabel);
        if fc_continue in doobjectdestroyandreraisestate.newflowcontrol then
          begin
            ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
            ctx.CurrAsmList.concat(taicpu.op_sym(a_br,oldContinueLabel));
          end;

        { exit the 'break' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));  // break
        if in_loop then
          ctx.hlcg.a_label(ctx.CurrAsmList,NewBreakLabel);
        if fc_break in doobjectdestroyandreraisestate.newflowcontrol then
          begin
            ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
            ctx.CurrAsmList.concat(taicpu.op_sym(a_br,oldBreakLabel));
          end;

        { exit the 'exit' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));  // exit
        ctx.hlcg.a_label(ctx.CurrAsmList,NewCurrExitLabel);
        if fc_exit in doobjectdestroyandreraisestate.newflowcontrol then
          begin
            ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
            ctx.CurrAsmList.concat(taicpu.op_sym(a_br,oldCurrExitLabel));
          end;

        compiler.current_procinfo.CurrExitLabel:=oldCurrExitLabel;
        if in_loop then
          begin
            compiler.current_procinfo.CurrContinueLabel:=oldContinueLabel;
            compiler.current_procinfo.CurrBreakLabel:=oldBreakLabel;
          end;

        ctx.CurrAsmList.concat(taicpu.op_sym(a_legacy_catch,current_asmdata.WeakRefAsmSymbol(FPC_EXCEPTION_TAG_SYM,AT_WASM_EXCEPTION_TAG)));

        ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_raise_nested',[],nil).resetiftemp;

        ctx.CurrAsmList.concat(taicpu.op_none(a_end_legacy_try));

        { clear some stuff }
        if assigned(exceptvarsym) then
          begin
            ctx.tg.UngetLocal(ctx.CurrAsmList,exceptvarsym.localloc.reference);
            exceptvarsym.localloc.loc:=LOC_INVALID;
          end;
        compiler.exceptionstatehandler.end_catch(ctx.CurrAsmList);

        { propagate exit/break/continue }
        flowcontrol:=doobjectdestroyandreraisestate.oldflowcontrol+(doobjectdestroyandreraisestate.newflowcontrol-[fc_inflowcontrol,fc_catching_exceptions]);

        { next on node }
        if assigned(left) then
          secondpass(left,ctx);
      end;

    procedure twasmonnode.pass_generate_code_bf_exceptions(ctx:tpassgeneratecodecontext);
      var
        exceptvarsym : tlocalvarsym;
        exceptlocdef: tdef;
        exceptlocreg: tregister;
        oldCurrRaiseLabel,
        oldCurrExitLabel,
        oldContinueLabel,
        oldBreakLabel, NewContinueLabel, NewBreakLabel,
        NewCurrRaiseLabel, NewCurrExitLabel: tasmlabel;
        in_loop: Boolean;
        doobjectdestroyandreraisestate: tcgexceptionstatehandler.texceptionstate;
        excepttemps: tcgexceptionstatehandler.texceptiontemps;
      begin
        oldCurrRaiseLabel:=nil;
        oldCurrExitLabel:=nil;
        oldContinueLabel:=nil;
        oldBreakLabel:=nil;
        NewCurrRaiseLabel:=nil;
        NewBreakLabel:=nil;
        NewContinueLabel:=nil;
        location_reset(location,LOC_VOID,OS_NO);

        { Exception temps? We don't need no stinking exception temps! :) }
        fillchar(excepttemps,sizeof(excepttemps),0);
        reference_reset(excepttemps.envbuf,0,[]);
        reference_reset(excepttemps.jmpbuf,0,[]);
        reference_reset(excepttemps.reasonbuf,0,[]);

        in_loop:=assigned(compiler.current_procinfo.CurrBreakLabel);

        compiler.exceptionstatehandler.begin_catch(ctx.CurrAsmList,excepttype,nil,exceptlocdef,exceptlocreg);

        { Retrieve exception variable }
        if assigned(excepTSymtable) then
          exceptvarsym:=tlocalvarsym(excepTSymtable.SymList[0])
        else
          internalerror(2011020401);

        if assigned(exceptvarsym) then
          begin
            location_reset_ref(exceptvarsym.localloc, LOC_REFERENCE, def_cgsize(compiler.deftypes.voidpointertype), compiler.deftypes.voidpointertype.alignment, []);
            ctx.tg.GetLocal(ctx.CurrAsmList, exceptvarsym.vardef.size, exceptvarsym.vardef, exceptvarsym.localloc.reference);
            ctx.hlcg.a_load_reg_ref(ctx.CurrAsmList, exceptlocdef, exceptvarsym.vardef, exceptlocreg, exceptvarsym.localloc.reference);
          end;

        compiler.exceptionstatehandler.new_exception(ctx.CurrAsmList,excepttemps,tek_except,doobjectdestroyandreraisestate);

        { in the case that another exception is risen
          we've to destroy the old one, so create a new
          exception frame for the catch-handler }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        ctx.CurrAsmList.concat(taicpu.op_none(a_block));
        oldCurrRaiseLabel:=tcpuprocinfo(compiler.current_procinfo).CurrRaiseLabel;
        current_asmdata.getjumplabel(NewCurrRaiseLabel);
        tcpuprocinfo(compiler.current_procinfo).CurrRaiseLabel:=NewCurrRaiseLabel;

        { the 'exit' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        oldCurrExitLabel:=compiler.current_procinfo.CurrExitLabel;
        current_asmdata.getjumplabel(NewCurrExitLabel);
        compiler.current_procinfo.CurrExitLabel:=NewCurrExitLabel;

        { the 'break' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldBreakLabel:=compiler.current_procinfo.CurrBreakLabel;
            current_asmdata.getjumplabel(NewBreakLabel);
            compiler.current_procinfo.CurrBreakLabel:=NewBreakLabel;
          end;

        { the 'continue' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldContinueLabel:=compiler.current_procinfo.CurrContinueLabel;
            current_asmdata.getjumplabel(NewContinueLabel);
            compiler.current_procinfo.CurrContinueLabel:=NewContinueLabel;
          end;

        if assigned(right) then
          secondpass(right,ctx);

        compiler.exceptionstatehandler.end_try_block(ctx.CurrAsmList,tek_except,excepttemps,doobjectdestroyandreraisestate,nil);

        ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
        ctx.CurrAsmList.concat(taicpu.op_const(a_br,6));

        { exit the 'continue' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if in_loop then
          ctx.hlcg.a_label(ctx.CurrAsmList,NewContinueLabel);
        if fc_continue in doobjectdestroyandreraisestate.newflowcontrol then
          begin
            ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
            ctx.CurrAsmList.concat(taicpu.op_sym(a_br,oldContinueLabel));
          end;

        { exit the 'break' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));  // break
        if in_loop then
          ctx.hlcg.a_label(ctx.CurrAsmList,NewBreakLabel);
        if fc_break in doobjectdestroyandreraisestate.newflowcontrol then
          begin
            ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
            ctx.CurrAsmList.concat(taicpu.op_sym(a_br,oldBreakLabel));
          end;

        { exit the 'exit' block }
        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));  // exit
        ctx.hlcg.a_label(ctx.CurrAsmList,NewCurrExitLabel);
        if fc_exit in doobjectdestroyandreraisestate.newflowcontrol then
          begin
            ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
            ctx.CurrAsmList.concat(taicpu.op_sym(a_br,oldCurrExitLabel));
          end;

        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));
        ctx.hlcg.a_label(ctx.CurrAsmList,NewCurrRaiseLabel);

        compiler.current_procinfo.CurrExitLabel:=oldCurrExitLabel;
        if in_loop then
          begin
            compiler.current_procinfo.CurrContinueLabel:=oldContinueLabel;
            compiler.current_procinfo.CurrBreakLabel:=oldBreakLabel;
          end;
        tcpuprocinfo(compiler.current_procinfo).CurrRaiseLabel:=oldCurrRaiseLabel;

        ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_clear_exception_flag',[],nil).resetiftemp;
        ctx.hlcg.g_call_system_proc(ctx.CurrAsmList,'fpc_raise_nested',[],nil).resetiftemp;
        ctx.hlcg.g_maybe_checkforexceptions(ctx.CurrAsmList);

        ctx.CurrAsmList.concat(taicpu.op_none(a_end_block));

        { clear some stuff }
        if assigned(exceptvarsym) then
          begin
            ctx.tg.UngetLocal(ctx.CurrAsmList,exceptvarsym.localloc.reference);
            exceptvarsym.localloc.loc:=LOC_INVALID;
          end;
        compiler.exceptionstatehandler.end_catch(ctx.CurrAsmList);

        { propagate exit/break/continue }
        flowcontrol:=doobjectdestroyandreraisestate.oldflowcontrol+(doobjectdestroyandreraisestate.newflowcontrol-[fc_inflowcontrol,fc_catching_exceptions]);

        { next on node }
        if assigned(left) then
          secondpass(left,ctx);
      end;

    procedure twasmonnode.pass_generate_code(ctx:tpassgeneratecodecontext);
      begin
        if ts_wasm_no_exceptions in compiler.globals.current_settings.targetswitches then
          pass_generate_code_no_exceptions(ctx)
        else if ts_wasm_native_exnref_exceptions in compiler.globals.current_settings.targetswitches then
          pass_generate_code_native_exnref_exceptions(ctx)
        else if ts_wasm_native_legacy_exceptions in compiler.globals.current_settings.targetswitches then
          pass_generate_code_native_legacy_exceptions(ctx)
        else if ts_wasm_bf_exceptions in compiler.globals.current_settings.targetswitches then
          pass_generate_code_bf_exceptions(ctx)
        else
          internalerror(2021092802);
      end;

initialization
  cifnode:=twasmifnode;
  cwhilerepeatnode:=twasmwhilerepeatnode;
  craisenode:=twasmraisenode;
  ctryexceptnode:=twasmtryexceptnode;
  ctryfinallynode:=twasmtryfinallynode;
  connode:=twasmonnode;
end.
