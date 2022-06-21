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
      aasmbase,node,nflw,ncgflw, cutils;

    type

      { twasmifnode }

      { Wasm doesn't have any jump(+offset) operations
        It only provide structured blockes to handle jumps
        (It's possible to jump-out-of-block at any time)
        "If" is also implemented as a block, identical to high-level language. }
      twasmifnode = class(tcgifnode)
      public
        procedure pass_generate_code;override;
      end;

      { twasmwhilerepeatnode }

      twasmwhilerepeatnode = class(tcgwhilerepeatnode)
      public
        procedure pass_generate_code_condition;
        procedure pass_generate_code;override;
      end;

      { twasmraisenode }

      twasmraisenode = class(tcgraisenode)
      private
        function pass_1_no_exceptions : tnode;
        function pass_1_native_exceptions : tnode;
        function pass_1_bf_exceptions : tnode;
      public
        function pass_1 : tnode;override;
      end;

      { twasmtryexceptnode }

      twasmtryexceptnode = class(tcgtryexceptnode)
      private
        procedure pass_generate_code_no_exceptions;
        procedure pass_generate_code_js_exceptions;
        procedure pass_generate_code_native_exceptions;
        procedure pass_generate_code_bf_exceptions;
      public
        procedure pass_generate_code;override;
      end;

      { twasmtryfinallynode }

      twasmtryfinallynode = class(tcgtryfinallynode)
      private
        procedure pass_generate_code_no_exceptions;
        procedure pass_generate_code_js_exceptions;
        procedure pass_generate_code_native_exceptions;
        procedure pass_generate_code_bf_exceptions;
      public
        procedure pass_generate_code;override;
      end;

      { twasmonnode }

      twasmonnode = class(tcgonnode)
      private
        procedure pass_generate_code_no_exceptions;
        procedure pass_generate_code_js_exceptions;
        procedure pass_generate_code_native_exceptions;
        procedure pass_generate_code_bf_exceptions;
      public
        procedure pass_generate_code;override;
      end;

implementation

    uses
      verbose,globals,systems,globtype,constexp,
      symconst,symdef,symsym,symtype,aasmtai,aasmdata,aasmcpu,defutil,defcmp,
      procinfo,cgbase,cgexcept,pass_1,pass_2,parabase,compinnr,
      cpubase,cpuinfo,cpupi,
      nbas,nld,ncon,ncnv,ncal,ninl,nmem,nadd,nutils,
      tgobj,paramgr,
      cgutils,hlcgobj,hlcgcpu;

{*****************************************************************************
                           twasmwhilerepeatnode
*****************************************************************************}

    procedure twasmwhilerepeatnode.pass_generate_code_condition;
      begin
        secondpass(left);
        thlcgwasm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);

        // reversing the condition
        if not (lnf_checknegate in loopflags) then
          current_asmdata.CurrAsmList.concat(taicpu.op_none(a_i32_eqz));

        current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br_if,1) );
        thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);
      end;


    procedure twasmwhilerepeatnode.pass_generate_code;
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

        oldclabel:=current_procinfo.CurrContinueLabel;
        oldblabel:=current_procinfo.CurrBreakLabel;

        include(flowcontrol,fc_inflowcontrol);
        exclude(flowcontrol,fc_unwind_loop);

        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_loop));

        if lnf_testatbegin in loopflags then
        begin
          hlcg.a_label(current_asmdata.CurrAsmList,lcont);
          pass_generate_code_condition;
        end;

        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        current_procinfo.CurrContinueLabel:=lcont;
        current_procinfo.CurrBreakLabel:=lbreak;

        secondpass(right);

        if (lnf_testatbegin in loopflags) then
          current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,1) ); // jump back to the external loop

        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if not (lnf_testatbegin in loopflags) then
          begin
            hlcg.a_label(current_asmdata.CurrAsmList,lcont);
            pass_generate_code_condition;
          end;
        current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,0) ); // jump back to loop

        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_loop));
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
        hlcg.a_label(current_asmdata.CurrAsmList,lbreak);

        current_procinfo.CurrContinueLabel:=oldclabel;
        current_procinfo.CurrBreakLabel:=oldblabel;

        { a break/continue in a while/repeat block can't be seen outside }
        flowcontrol:=oldflowcontrol+(flowcontrol-[fc_break,fc_continue,fc_inflowcontrol]);
      end;

{*****************************************************************************
                               twasmifnode
*****************************************************************************}

    procedure twasmifnode.pass_generate_code;
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

        secondpass(left); // condition exprssions
        thlcgwasm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);

        if is_64bit(left.resultdef) then
          begin
            thlcgwasm(hlcg).a_load_const_stack(current_asmdata.CurrAsmList,left.resultdef,0,R_INTREGISTER);
            current_asmdata.CurrAsmList.Concat(taicpu.op_none(a_i64_ne));
            thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);
          end;

        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_if));
        thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);

        if Assigned(right) then
          secondpass(right); // then branchs

        if Assigned(t1) then // else branch
          begin
            current_asmdata.CurrAsmList.concat(taicpu.op_none(a_else));
            secondpass(t1);
          end;

        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_if));

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
        result:=internalstatements(statements);

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
                  third:=cpointerconstnode.Create(0,voidpointertype);
              end
            else
              begin
                third:=cinlinenode.create(in_get_frame,false,nil);
                //current_addr:=clabelnode.create(cnothingnode.create,clabelsym.create('$raiseaddr'));
                //addstatement(statements,current_addr);
                //right:=caddrnode.create(cloadnode.create(current_addr.labsym,current_addr.labsym.owner));
                right:=cnilnode.create;

                { raise address off by one so we are for sure inside the action area for the raise }
                if tf_use_psabieh in target_info.flags then
                  right:=caddnode.create_internal(addn,right,cordconstnode.create(1,sizesinttype,false));
              end;

            raisenode:=ccallnode.createintern('fpc_raiseexception',
              ccallparanode.create(third,
              ccallparanode.create(right,
              ccallparanode.create(left,nil)))
              );
            include(raisenode.callnodeflags,cnf_call_never_returns);
            addstatement(statements,raisenode);
          end
        else
          begin
            addstatement(statements,ccallnode.createintern('fpc_popaddrstack',nil));
            raisenode:=ccallnode.createintern('fpc_reraise',nil);
            include(raisenode.callnodeflags,cnf_call_never_returns);
            addstatement(statements,raisenode);
          end;
        left:=nil;
        right:=nil;
        third:=nil;
      end;


    function twasmraisenode.pass_1_native_exceptions : tnode;
      var
        statements : tstatementnode;
        //current_addr : tlabelnode;
        raisenode : tcallnode;
      begin
        result:=internalstatements(statements);

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
                  third:=cpointerconstnode.Create(0,voidpointertype);
              end
            else
              begin
                third:=cinlinenode.create(in_get_frame,false,nil);
                //current_addr:=clabelnode.create(cnothingnode.create,clabelsym.create('$raiseaddr'));
                //addstatement(statements,current_addr);
                //right:=caddrnode.create(cloadnode.create(current_addr.labsym,current_addr.labsym.owner));
                right:=cnilnode.create;

                { raise address off by one so we are for sure inside the action area for the raise }
                if tf_use_psabieh in target_info.flags then
                  right:=caddnode.create_internal(addn,right,cordconstnode.create(1,sizesinttype,false));
              end;

            raisenode:=ccallnode.createintern('fpc_raiseexception',
              ccallparanode.create(third,
              ccallparanode.create(right,
              ccallparanode.create(left,nil)))
              );
            include(raisenode.callnodeflags,cnf_call_never_returns);
            addstatement(statements,raisenode);
          end
        else
          begin
            //addstatement(statements,ccallnode.createintern('fpc_popaddrstack',nil));
            raisenode:=ccallnode.createintern('fpc_reraise2',nil);
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
        result:=internalstatements(statements);

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
                  third:=cpointerconstnode.Create(0,voidpointertype);
              end
            else
              begin
                third:=cinlinenode.create(in_get_frame,false,nil);
                //current_addr:=clabelnode.create(cnothingnode.create,clabelsym.create('$raiseaddr'));
                //addstatement(statements,current_addr);
                //right:=caddrnode.create(cloadnode.create(current_addr.labsym,current_addr.labsym.owner));
                right:=cnilnode.create;

                { raise address off by one so we are for sure inside the action area for the raise }
                if tf_use_psabieh in target_info.flags then
                  right:=caddnode.create_internal(addn,right,cordconstnode.create(1,sizesinttype,false));
              end;

            raisenode:=ccallnode.createintern('fpc_raiseexception',
              ccallparanode.create(third,
              ccallparanode.create(right,
              ccallparanode.create(left,nil)))
              );
            include(raisenode.callnodeflags,cnf_call_never_returns);
            addstatement(statements,raisenode);
          end
        else
          begin
            //addstatement(statements,ccallnode.createintern('fpc_popaddrstack',nil));
            raisenode:=ccallnode.createintern('fpc_reraise2',nil);
            include(raisenode.callnodeflags,cnf_call_never_returns);
            addstatement(statements,raisenode);
          end;
        left:=nil;
        right:=nil;
        third:=nil;
      end;


    function twasmraisenode.pass_1 : tnode;
      begin
        if ts_wasm_no_exceptions in current_settings.targetswitches then
          result:=pass_1_no_exceptions
        else if ts_wasm_native_exceptions in current_settings.targetswitches then
          result:=pass_1_native_exceptions
        else if ts_wasm_bf_exceptions in current_settings.targetswitches then
          result:=pass_1_bf_exceptions
        else
          result:=inherited;
      end;

{*****************************************************************************
                             twasmtryexceptnode
*****************************************************************************}

    procedure twasmtryexceptnode.pass_generate_code_no_exceptions;
      begin
        location_reset(location,LOC_VOID,OS_NO);
        secondpass(left);
      end;

    procedure twasmtryexceptnode.pass_generate_code_js_exceptions;
      begin
        internalerror(2021091706);
      end;

    procedure twasmtryexceptnode.pass_generate_code_native_exceptions;
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

        in_loop:=assigned(current_procinfo.CurrBreakLabel);

        cexceptionstatehandler.new_exception(current_asmdata.CurrAsmList,excepttemps,tek_except,trystate);

        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_try));

        { try block }
        secondpass(left);
        if codegenerror then
          goto errorexit;

        cexceptionstatehandler.end_try_block(current_asmdata.CurrAsmList,tek_except,excepttemps,trystate,nil);

        current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_catch,current_asmdata.WeakRefAsmSymbol(FPC_EXCEPTION_TAG_SYM,AT_WASM_EXCEPTION_TAG)));

        flowcontrol:=[fc_inflowcontrol]+trystate.oldflowcontrol*[fc_catching_exceptions];
        { on statements }
        if assigned(right) then
          secondpass(right);

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
                cexceptionstatehandler.new_exception(current_asmdata.CurrAsmList,destroytemps,tek_except,doobjectdestroyandreraisestate);
                { the flowcontrol from the default except-block must be merged
                  with the flowcontrol flags potentially set by the
                  on-statements handled above (secondpass(right)), as they are
                  at the same program level }
                flowcontrol:=
                  flowcontrol+
                  afteronflowcontrol;

                current_asmdata.CurrAsmList.concat(taicpu.op_none(a_try));

                { the 'exit' block }
                current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

                oldCurrExitLabel:=current_procinfo.CurrExitLabel;
                current_asmdata.getjumplabel(NewCurrExitLabel);
                current_procinfo.CurrExitLabel:=NewCurrExitLabel;

                { the 'break' block }
                current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

                if in_loop then
                  begin
                    oldBreakLabel:=current_procinfo.CurrBreakLabel;
                    current_asmdata.getjumplabel(NewBreakLabel);
                    current_procinfo.CurrBreakLabel:=NewBreakLabel;
                  end;

                { the 'continue' block }
                current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

                if in_loop then
                  begin
                    oldContinueLabel:=current_procinfo.CurrContinueLabel;
                    current_asmdata.getjumplabel(NewContinueLabel);
                    current_procinfo.CurrContinueLabel:=NewContinueLabel;
                  end;

                secondpass(t1);

                cexceptionstatehandler.end_try_block(current_asmdata.CurrAsmList,tek_except,destroytemps,doobjectdestroyandreraisestate,nil);

                hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
                current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,4));

                { exit the 'continue' block }
                current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
                if in_loop then
                  hlcg.a_label(current_asmdata.CurrAsmList,NewContinueLabel);
                if fc_continue in doobjectdestroyandreraisestate.newflowcontrol then
                  begin
                    hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
                    current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_br,oldContinueLabel));
                  end;

                { exit the 'break' block }
                current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));  // break
                if in_loop then
                  hlcg.a_label(current_asmdata.CurrAsmList,NewBreakLabel);
                if fc_break in doobjectdestroyandreraisestate.newflowcontrol then
                  begin
                    hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
                    current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_br,oldBreakLabel));
                  end;

                { exit the 'exit' block }
                current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));  // exit
                hlcg.a_label(current_asmdata.CurrAsmList,NewCurrExitLabel);
                if fc_exit in doobjectdestroyandreraisestate.newflowcontrol then
                  begin
                    hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
                    current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_br,oldCurrExitLabel));
                  end;

                current_procinfo.CurrExitLabel:=oldCurrExitLabel;
                if in_loop then
                  begin
                    current_procinfo.CurrContinueLabel:=oldContinueLabel;
                    current_procinfo.CurrBreakLabel:=oldBreakLabel;
                  end;

                current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_catch,current_asmdata.WeakRefAsmSymbol(FPC_EXCEPTION_TAG_SYM,AT_WASM_EXCEPTION_TAG)));

                hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_raise_nested',[],nil).resetiftemp;

                current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_try));
              end
            else
              begin
                doobjectdestroyandreraisestate.newflowcontrol:=afteronflowcontrol;
                hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
              end;
          end
        else
          begin
            current_asmdata.CurrAsmList.concat(taicpu.op_const(a_rethrow,0));
            doobjectdestroyandreraisestate.newflowcontrol:=afteronflowcontrol;
          end;

        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_try));

      errorexit:
        { return all used control flow statements }
        flowcontrol:=trystate.oldflowcontrol+(doobjectdestroyandreraisestate.newflowcontrol +
          trystate.newflowcontrol - [fc_inflowcontrol,fc_catching_exceptions]);
      end;

    procedure twasmtryexceptnode.pass_generate_code_bf_exceptions;
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

        in_loop:=assigned(current_procinfo.CurrBreakLabel);

        cexceptionstatehandler.new_exception(current_asmdata.CurrAsmList,excepttemps,tek_except,trystate);

        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));
        oldCurrRaiseLabel:=tcpuprocinfo(current_procinfo).CurrRaiseLabel;
        current_asmdata.getjumplabel(NewCurrRaiseLabel);
        tcpuprocinfo(current_procinfo).CurrRaiseLabel:=NewCurrRaiseLabel;

        { try block }
        secondpass(left);
        if codegenerror then
          goto errorexit;

        cexceptionstatehandler.end_try_block(current_asmdata.CurrAsmList,tek_except,excepttemps,trystate,nil);

        current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,1));
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
        hlcg.a_label(current_asmdata.CurrAsmList,NewCurrRaiseLabel);

        hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_clear_exception_flag',[],nil).resetiftemp;
        tcpuprocinfo(current_procinfo).CurrRaiseLabel:=OldCurrRaiseLabel;

        flowcontrol:=[fc_inflowcontrol]+trystate.oldflowcontrol*[fc_catching_exceptions];
        { on statements }
        if assigned(right) then
          secondpass(right);

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
                cexceptionstatehandler.new_exception(current_asmdata.CurrAsmList,destroytemps,tek_except,doobjectdestroyandreraisestate);
                { the flowcontrol from the default except-block must be merged
                  with the flowcontrol flags potentially set by the
                  on-statements handled above (secondpass(right)), as they are
                  at the same program level }
                flowcontrol:=
                  flowcontrol+
                  afteronflowcontrol;

                current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

                current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));
                oldCurrRaiseLabel:=tcpuprocinfo(current_procinfo).CurrRaiseLabel;
                current_asmdata.getjumplabel(NewCurrRaiseLabel);
                tcpuprocinfo(current_procinfo).CurrRaiseLabel:=NewCurrRaiseLabel;

                { the 'exit' block }
                current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

                oldCurrExitLabel:=current_procinfo.CurrExitLabel;
                current_asmdata.getjumplabel(NewCurrExitLabel);
                current_procinfo.CurrExitLabel:=NewCurrExitLabel;

                { the 'break' block }
                current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

                if in_loop then
                  begin
                    oldBreakLabel:=current_procinfo.CurrBreakLabel;
                    current_asmdata.getjumplabel(NewBreakLabel);
                    current_procinfo.CurrBreakLabel:=NewBreakLabel;
                  end;

                { the 'continue' block }
                current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

                if in_loop then
                  begin
                    oldContinueLabel:=current_procinfo.CurrContinueLabel;
                    current_asmdata.getjumplabel(NewContinueLabel);
                    current_procinfo.CurrContinueLabel:=NewContinueLabel;
                  end;

                secondpass(t1);

                cexceptionstatehandler.end_try_block(current_asmdata.CurrAsmList,tek_except,destroytemps,doobjectdestroyandreraisestate,nil);

                hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
                current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,4));

                { exit the 'continue' block }
                current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
                if in_loop then
                  hlcg.a_label(current_asmdata.CurrAsmList,NewContinueLabel);
                if fc_continue in doobjectdestroyandreraisestate.newflowcontrol then
                  begin
                    hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
                    current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_br,OldContinueLabel));
                  end;

                { exit the 'break' block }
                current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));  // break
                if in_loop then
                  hlcg.a_label(current_asmdata.CurrAsmList,NewBreakLabel);
                if fc_break in doobjectdestroyandreraisestate.newflowcontrol then
                  begin
                    hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
                    current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_br,OldBreakLabel));
                  end;

                { exit the 'exit' block }
                current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));  // exit
                hlcg.a_label(current_asmdata.CurrAsmList,NewCurrExitLabel);
                if fc_exit in doobjectdestroyandreraisestate.newflowcontrol then
                  begin
                    hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
                    current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_br,oldCurrExitLabel));
                  end;

                current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
                hlcg.a_label(current_asmdata.CurrAsmList,NewCurrRaiseLabel);

                current_procinfo.CurrExitLabel:=oldCurrExitLabel;
                if in_loop then
                  begin
                    current_procinfo.CurrContinueLabel:=oldContinueLabel;
                    current_procinfo.CurrBreakLabel:=oldBreakLabel;
                  end;
                tcpuprocinfo(current_procinfo).CurrRaiseLabel:=OldCurrRaiseLabel;

                hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_clear_exception_flag',[],nil).resetiftemp;
                hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_raise_nested',[],nil).resetiftemp;
                hlcg.g_maybe_checkforexceptions(current_asmdata.CurrAsmList);

                current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
              end
            else
              begin
                doobjectdestroyandreraisestate.newflowcontrol:=afteronflowcontrol;
                hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
              end;
          end
        else
          begin
            hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_reraise',[],nil).resetiftemp;
            hlcg.g_maybe_checkforexceptions(current_asmdata.CurrAsmList);
            doobjectdestroyandreraisestate.newflowcontrol:=afteronflowcontrol;
          end;

        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));

      errorexit:
        { return all used control flow statements }
        flowcontrol:=trystate.oldflowcontrol+(doobjectdestroyandreraisestate.newflowcontrol +
          trystate.newflowcontrol - [fc_inflowcontrol,fc_catching_exceptions]);
        tcpuprocinfo(current_procinfo).CurrRaiseLabel:=OldCurrRaiseLabel;
      end;

    procedure twasmtryexceptnode.pass_generate_code;
      begin
        if ts_wasm_no_exceptions in current_settings.targetswitches then
          pass_generate_code_no_exceptions
        else if ts_wasm_js_exceptions in current_settings.targetswitches then
          pass_generate_code_js_exceptions
        else if ts_wasm_native_exceptions in current_settings.targetswitches then
          pass_generate_code_native_exceptions
        else if ts_wasm_bf_exceptions in current_settings.targetswitches then
          pass_generate_code_bf_exceptions
        else
          internalerror(2021091705);
      end;

{*****************************************************************************
                             twasmtryfinallynode
*****************************************************************************}

    procedure twasmtryfinallynode.pass_generate_code_no_exceptions;
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
            reasonreg:=hlcg.getintregister(current_asmdata.CurrAsmList,exceptionreasontype);
            hlcg.g_exception_reason_load(current_asmdata.CurrAsmList,exceptionreasontype,exceptionreasontype,excepttemps.reasonbuf,reasonreg);
            thlcgwasm(hlcg).a_cmp_const_reg_stack(current_asmdata.CurrAsmList,exceptionreasontype,OC_EQ,reason,reasonreg);
            current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br_if,br));
            thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);
          end;

        procedure generate_exceptreason_check_br(reason: tcgint; l: TAsmLabel);
          var
            reasonreg : tregister;
          begin
            reasonreg:=hlcg.getintregister(current_asmdata.CurrAsmList,exceptionreasontype);
            hlcg.g_exception_reason_load(current_asmdata.CurrAsmList,exceptionreasontype,exceptionreasontype,excepttemps.reasonbuf,reasonreg);
            thlcgwasm(hlcg).a_cmp_const_reg_stack(current_asmdata.CurrAsmList,exceptionreasontype,OC_EQ,reason,reasonreg);
            current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_br_if,l));
            thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);
          end;

      begin
        location_reset(location,LOC_VOID,OS_NO);
        oldBreakLabel:=nil;
        oldContinueLabel:=nil;
        continuefinallylabel:=nil;
        breakfinallylabel:=nil;

        in_loop:=assigned(current_procinfo.CurrBreakLabel);

        if not implicitframe then
          exceptframekind:=tek_normalfinally
        else
          exceptframekind:=tek_implicitfinally;

        { in 'no exceptions' mode, we still want to handle properly exit,
          continue and break (they still need to execute the 'finally'
          statements), so for this we need excepttemps.reasonbuf, and for this
          reason, we need to allocate excepttemps }
        cexceptionstatehandler.get_exception_temps(current_asmdata.CurrAsmList,excepttemps);
        cexceptionstatehandler.new_exception(current_asmdata.CurrAsmList,excepttemps,exceptframekind,finallyexceptionstate);

        { the finally block must catch break, continue and exit }
        { statements                                            }

        { the outer 'try..finally' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        { the 'exit' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        oldCurrExitLabel:=current_procinfo.CurrExitLabel;
        exitfinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
        current_procinfo.CurrExitLabel:=exitfinallylabel;

        { the 'break' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldBreakLabel:=current_procinfo.CurrBreakLabel;
            breakfinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
            current_procinfo.CurrBreakLabel:=breakfinallylabel;
          end;

        { the 'continue' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldContinueLabel:=current_procinfo.CurrContinueLabel;
            continuefinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
            current_procinfo.CurrContinueLabel:=continuefinallylabel;
          end;

        { try code }
        if assigned(left) then
          begin
            secondpass(left);
            if codegenerror then
              exit;
          end;

        { don't generate line info for internal cleanup }
        current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));

        cexceptionstatehandler.end_try_block(current_asmdata.CurrAsmList,exceptframekind,excepttemps,finallyexceptionstate,nil);

        { we've reached the end of the 'try' block, with no exceptions/exit/break/continue, so set exceptionreason:=0 }
        hlcg.g_exception_reason_save_const(current_asmdata.CurrAsmList,exceptionreasontype,0,excepttemps.reasonbuf);
        current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,3)); // jump to the 'finally' section

        { exit the 'continue' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if in_loop then
          hlcg.a_label(current_asmdata.CurrAsmList,continuefinallylabel);
        { exceptionreason:=4 (continue) }
        hlcg.g_exception_reason_save_const(current_asmdata.CurrAsmList,exceptionreasontype,4,excepttemps.reasonbuf);
        current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,2)); // jump to the 'finally' section

        { exit the 'break' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if in_loop then
          hlcg.a_label(current_asmdata.CurrAsmList,breakfinallylabel);
        { exceptionreason:=3 (break) }
        hlcg.g_exception_reason_save_const(current_asmdata.CurrAsmList,exceptionreasontype,3,excepttemps.reasonbuf);
        current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,1)); // jump to the 'finally' section

        { exit the 'exit' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
        hlcg.a_label(current_asmdata.CurrAsmList,exitfinallylabel);
        { exceptionreason:=2 (exit) }
        hlcg.g_exception_reason_save_const(current_asmdata.CurrAsmList,exceptionreasontype,2,excepttemps.reasonbuf);
        { proceed to the 'finally' section, which follow immediately, no need for jumps }

        { exit the outer 'try..finally' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));

        { end cleanup }
        current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

        { finally code (don't unconditionally set fc_inflowcontrol, since the
          finally code is unconditionally executed; we do have to filter out
          flags regarding break/contrinue/etc. because we have to give an
          error in case one of those is used in the finally-code }
        flowcontrol:=finallyexceptionstate.oldflowcontrol*[fc_inflowcontrol,fc_catching_exceptions];
        secondpass(right);
        { goto is allowed if it stays inside the finally block,
          this is checked using the exception block number }
        if (flowcontrol-[fc_gotolabel])<>(finallyexceptionstate.oldflowcontrol*[fc_inflowcontrol,fc_catching_exceptions]) then
          CGMessage(cg_e_control_flow_outside_finally);
        if codegenerror then
          exit;

        { don't generate line info for internal cleanup }
        current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));

        if fc_exit in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(2,oldCurrExitLabel);
        if fc_break in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(3,oldBreakLabel);
        if fc_continue in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(4,oldContinueLabel);

        cexceptionstatehandler.unget_exception_temps(current_asmdata.CurrAsmList,excepttemps);

        { end cleanup }
        current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

        current_procinfo.CurrExitLabel:=oldCurrExitLabel;
        if assigned(current_procinfo.CurrBreakLabel) then
         begin
           current_procinfo.CurrContinueLabel:=oldContinueLabel;
           current_procinfo.CurrBreakLabel:=oldBreakLabel;
         end;
        flowcontrol:=finallyexceptionstate.oldflowcontrol+(finallyexceptionstate.newflowcontrol-[fc_inflowcontrol,fc_catching_exceptions]);
      end;

    procedure twasmtryfinallynode.pass_generate_code_js_exceptions;
      begin
        internalerror(2021091702);
      end;

    procedure twasmtryfinallynode.pass_generate_code_native_exceptions;
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
          reasonreg:=hlcg.getintregister(current_asmdata.CurrAsmList,exceptionreasontype);
          hlcg.g_exception_reason_load(current_asmdata.CurrAsmList,exceptionreasontype,exceptionreasontype,excepttemps.reasonbuf,reasonreg);
          thlcgwasm(hlcg).a_cmp_const_reg_stack(current_asmdata.CurrAsmList,exceptionreasontype,OC_EQ,reason,reasonreg);
          current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br_if,br));
          thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);
        end;

      procedure generate_exceptreason_check_br(reason: tcgint; l: tasmlabel);
        var
          reasonreg : tregister;
        begin
          reasonreg:=hlcg.getintregister(current_asmdata.CurrAsmList,exceptionreasontype);
          hlcg.g_exception_reason_load(current_asmdata.CurrAsmList,exceptionreasontype,exceptionreasontype,excepttemps.reasonbuf,reasonreg);
          thlcgwasm(hlcg).a_cmp_const_reg_stack(current_asmdata.CurrAsmList,exceptionreasontype,OC_EQ,reason,reasonreg);
          current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_br_if,l));
          thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);
        end;

      procedure generate_exceptreason_throw(reason: tcgint);
        var
          reasonreg : tregister;
        begin
          reasonreg:=hlcg.getintregister(current_asmdata.CurrAsmList,exceptionreasontype);
          hlcg.g_exception_reason_load(current_asmdata.CurrAsmList,exceptionreasontype,exceptionreasontype,excepttemps.reasonbuf,reasonreg);
          thlcgwasm(hlcg).a_cmp_const_reg_stack(current_asmdata.CurrAsmList,exceptionreasontype,OC_EQ,reason,reasonreg);
          current_asmdata.CurrAsmList.concat(taicpu.op_none(a_if));
          thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);
          current_asmdata.CurrAsmList.Concat(taicpu.op_sym(a_throw,current_asmdata.WeakRefAsmSymbol(FPC_EXCEPTION_TAG_SYM,AT_WASM_EXCEPTION_TAG)));
          current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_if));
        end;

      begin
        location_reset(location,LOC_VOID,OS_NO);
        oldBreakLabel:=nil;
        oldContinueLabel:=nil;
        continuefinallylabel:=nil;
        breakfinallylabel:=nil;

        in_loop:=assigned(current_procinfo.CurrBreakLabel);

        if not implicitframe then
          exceptframekind:=tek_normalfinally
        else
          exceptframekind:=tek_implicitfinally;

        { in 'no exceptions' mode, we still want to handle properly exit,
          continue and break (they still need to execute the 'finally'
          statements), so for this we need excepttemps.reasonbuf, and for this
          reason, we need to allocate excepttemps }
        cexceptionstatehandler.get_exception_temps(current_asmdata.CurrAsmList,excepttemps);
        cexceptionstatehandler.new_exception(current_asmdata.CurrAsmList,excepttemps,exceptframekind,finallyexceptionstate);

        { the finally block must catch break, continue and exit }
        { statements                                            }

        { the outer 'try..finally' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        { the 'exit' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        oldCurrExitLabel:=current_procinfo.CurrExitLabel;
        exitfinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
        current_procinfo.CurrExitLabel:=exitfinallylabel;

        { the 'break' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldBreakLabel:=current_procinfo.CurrBreakLabel;
            breakfinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
            current_procinfo.CurrBreakLabel:=breakfinallylabel;
          end;

        { the 'continue' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldContinueLabel:=current_procinfo.CurrContinueLabel;
            continuefinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
            current_procinfo.CurrContinueLabel:=continuefinallylabel;
          end;

        { the inner 'try..end_try' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_try));

        { try code }
        if assigned(left) then
          begin
            secondpass(left);
            if codegenerror then
              exit;
          end;

        { don't generate line info for internal cleanup }
        current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));

        cexceptionstatehandler.end_try_block(current_asmdata.CurrAsmList,exceptframekind,excepttemps,finallyexceptionstate,nil);

        { we've reached the end of the 'try' block, with no exceptions/exit/break/continue, so set exceptionreason:=0 }
        hlcg.g_exception_reason_save_const(current_asmdata.CurrAsmList,exceptionreasontype,0,excepttemps.reasonbuf);
        current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,4)); // jump to the 'finally' section

        current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_catch,current_asmdata.WeakRefAsmSymbol(FPC_EXCEPTION_TAG_SYM,AT_WASM_EXCEPTION_TAG)));
        { exceptionreason:=1 (exception) }
        hlcg.g_exception_reason_save_const(current_asmdata.CurrAsmList,exceptionreasontype,1,excepttemps.reasonbuf);
        current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,4)); // jump to the 'finally' section

        { exit the inner 'try..end_try' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_try));

        { exit the 'continue' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if in_loop then
          hlcg.a_label(current_asmdata.CurrAsmList,continuefinallylabel);
        { exceptionreason:=4 (continue) }
        hlcg.g_exception_reason_save_const(current_asmdata.CurrAsmList,exceptionreasontype,4,excepttemps.reasonbuf);
        current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,2)); // jump to the 'finally' section

        { exit the 'break' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if in_loop then
          hlcg.a_label(current_asmdata.CurrAsmList,breakfinallylabel);
        { exceptionreason:=3 (break) }
        hlcg.g_exception_reason_save_const(current_asmdata.CurrAsmList,exceptionreasontype,3,excepttemps.reasonbuf);
        current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,1)); // jump to the 'finally' section

        { exit the 'exit' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
        hlcg.a_label(current_asmdata.CurrAsmList,exitfinallylabel);
        { exceptionreason:=2 (exit) }
        hlcg.g_exception_reason_save_const(current_asmdata.CurrAsmList,exceptionreasontype,2,excepttemps.reasonbuf);
        { proceed to the 'finally' section, which follow immediately, no need for jumps }

        { exit the outer 'try..finally' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));

        { end cleanup }
        current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

        { finally code (don't unconditionally set fc_inflowcontrol, since the
          finally code is unconditionally executed; we do have to filter out
          flags regarding break/contrinue/etc. because we have to give an
          error in case one of those is used in the finally-code }
        flowcontrol:=finallyexceptionstate.oldflowcontrol*[fc_inflowcontrol,fc_catching_exceptions];
        secondpass(right);
        { goto is allowed if it stays inside the finally block,
          this is checked using the exception block number }
        if (flowcontrol-[fc_gotolabel])<>(finallyexceptionstate.oldflowcontrol*[fc_inflowcontrol,fc_catching_exceptions]) then
          CGMessage(cg_e_control_flow_outside_finally);
        if codegenerror then
          exit;

        { don't generate line info for internal cleanup }
        current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));

        if fc_exit in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(2,oldCurrExitLabel);
        if fc_break in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(3,oldBreakLabel);
        if fc_continue in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(4,oldContinueLabel);
        generate_exceptreason_throw(1);

        cexceptionstatehandler.unget_exception_temps(current_asmdata.CurrAsmList,excepttemps);

        { end cleanup }
        current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

        current_procinfo.CurrExitLabel:=oldCurrExitLabel;
        if assigned(current_procinfo.CurrBreakLabel) then
         begin
           current_procinfo.CurrContinueLabel:=oldContinueLabel;
           current_procinfo.CurrBreakLabel:=oldBreakLabel;
         end;
        flowcontrol:=finallyexceptionstate.oldflowcontrol+(finallyexceptionstate.newflowcontrol-[fc_inflowcontrol,fc_catching_exceptions]);
      end;

    procedure twasmtryfinallynode.pass_generate_code_bf_exceptions;
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
          reasonreg:=hlcg.getintregister(current_asmdata.CurrAsmList,exceptionreasontype);
          hlcg.g_exception_reason_load(current_asmdata.CurrAsmList,exceptionreasontype,exceptionreasontype,excepttemps.reasonbuf,reasonreg);
          thlcgwasm(hlcg).a_cmp_const_reg_stack(current_asmdata.CurrAsmList,exceptionreasontype,OC_EQ,reason,reasonreg);
          current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br_if,br));
          thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);
        end;

      procedure generate_exceptreason_check_br(reason: tcgint; l: tasmsymbol);
        var
          reasonreg : tregister;
        begin
          reasonreg:=hlcg.getintregister(current_asmdata.CurrAsmList,exceptionreasontype);
          hlcg.g_exception_reason_load(current_asmdata.CurrAsmList,exceptionreasontype,exceptionreasontype,excepttemps.reasonbuf,reasonreg);
          thlcgwasm(hlcg).a_cmp_const_reg_stack(current_asmdata.CurrAsmList,exceptionreasontype,OC_EQ,reason,reasonreg);
          current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_br_if,l));
          thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);
        end;

      procedure generate_exceptreason_reraise(reason: tcgint);
        var
          reasonreg : tregister;
        begin
          reasonreg:=hlcg.getintregister(current_asmdata.CurrAsmList,exceptionreasontype);
          hlcg.g_exception_reason_load(current_asmdata.CurrAsmList,exceptionreasontype,exceptionreasontype,excepttemps.reasonbuf,reasonreg);
          thlcgwasm(hlcg).a_cmp_const_reg_stack(current_asmdata.CurrAsmList,exceptionreasontype,OC_EQ,reason,reasonreg);
          current_asmdata.CurrAsmList.concat(taicpu.op_none(a_if));
          thlcgwasm(hlcg).decstack(current_asmdata.CurrAsmList,1);
          hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_reraise',[],nil).resetiftemp;
          hlcg.g_maybe_checkforexceptions(current_asmdata.CurrAsmList);
          current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_if));
        end;

      begin
        location_reset(location,LOC_VOID,OS_NO);
        oldCurrRaiseLabel:=nil;
        oldBreakLabel:=nil;
        oldContinueLabel:=nil;
        continuefinallylabel:=nil;
        breakfinallylabel:=nil;

        in_loop:=assigned(current_procinfo.CurrBreakLabel);

        if not implicitframe then
          exceptframekind:=tek_normalfinally
        else
          exceptframekind:=tek_implicitfinally;

        { in 'no exceptions' mode, we still want to handle properly exit,
          continue and break (they still need to execute the 'finally'
          statements), so for this we need excepttemps.reasonbuf, and for this
          reason, we need to allocate excepttemps }
        cexceptionstatehandler.get_exception_temps(current_asmdata.CurrAsmList,excepttemps);
        cexceptionstatehandler.new_exception(current_asmdata.CurrAsmList,excepttemps,exceptframekind,finallyexceptionstate);

        { the finally block must catch break, continue and exit }
        { statements                                            }

        { the outer 'try..finally' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        { the 'exit' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        oldCurrExitLabel:=current_procinfo.CurrExitLabel;
        exitfinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
        current_procinfo.CurrExitLabel:=exitfinallylabel;

        { the 'break' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldBreakLabel:=current_procinfo.CurrBreakLabel;
            breakfinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
            current_procinfo.CurrBreakLabel:=breakfinallylabel;
          end;

        { the 'continue' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldContinueLabel:=current_procinfo.CurrContinueLabel;
            continuefinallylabel:=get_jump_out_of_try_finally_frame_label(finallyexceptionstate);
            current_procinfo.CurrContinueLabel:=continuefinallylabel;
          end;

        { the inner 'try..end_try' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));
        oldCurrRaiseLabel:=tcpuprocinfo(current_procinfo).CurrRaiseLabel;
        current_asmdata.getjumplabel(raisefinallylabel);
        tcpuprocinfo(current_procinfo).CurrRaiseLabel:=raisefinallylabel;

        { try code }
        if assigned(left) then
          begin
            secondpass(left);
            if codegenerror then
              exit;
          end;

        { don't generate line info for internal cleanup }
        current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));

        cexceptionstatehandler.end_try_block(current_asmdata.CurrAsmList,exceptframekind,excepttemps,finallyexceptionstate,nil);

        { we've reached the end of the 'try' block, with no exceptions/exit/break/continue, so set exceptionreason:=0 }
        hlcg.g_exception_reason_save_const(current_asmdata.CurrAsmList,exceptionreasontype,0,excepttemps.reasonbuf);
        current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,4)); // jump to the 'finally' section

        { exit the inner 'try..end_try' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
        hlcg.a_label(current_asmdata.CurrAsmList,raisefinallylabel);

        { exceptionreason:=1 (exception) }
        hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_clear_exception_flag',[],nil).resetiftemp;
        hlcg.g_exception_reason_save_const(current_asmdata.CurrAsmList,exceptionreasontype,1,excepttemps.reasonbuf);
        current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,3)); // jump to the 'finally' section

        { exit the 'continue' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if in_loop then
          hlcg.a_label(current_asmdata.CurrAsmList,continuefinallylabel);
        { exceptionreason:=4 (continue) }
        hlcg.g_exception_reason_save_const(current_asmdata.CurrAsmList,exceptionreasontype,4,excepttemps.reasonbuf);
        current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,2)); // jump to the 'finally' section

        { exit the 'break' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if in_loop then
          hlcg.a_label(current_asmdata.CurrAsmList,breakfinallylabel);
        { exceptionreason:=3 (break) }
        hlcg.g_exception_reason_save_const(current_asmdata.CurrAsmList,exceptionreasontype,3,excepttemps.reasonbuf);
        current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,1)); // jump to the 'finally' section

        { exit the 'exit' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
        hlcg.a_label(current_asmdata.CurrAsmList,exitfinallylabel);
        { exceptionreason:=2 (exit) }
        hlcg.g_exception_reason_save_const(current_asmdata.CurrAsmList,exceptionreasontype,2,excepttemps.reasonbuf);
        { proceed to the 'finally' section, which follow immediately, no need for jumps }

        { exit the outer 'try..finally' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));

        { end cleanup }
        current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

        tcpuprocinfo(current_procinfo).CurrRaiseLabel:=oldCurrRaiseLabel;

        { finally code (don't unconditionally set fc_inflowcontrol, since the
          finally code is unconditionally executed; we do have to filter out
          flags regarding break/contrinue/etc. because we have to give an
          error in case one of those is used in the finally-code }
        flowcontrol:=finallyexceptionstate.oldflowcontrol*[fc_inflowcontrol,fc_catching_exceptions];
        secondpass(right);
        { goto is allowed if it stays inside the finally block,
          this is checked using the exception block number }
        if (flowcontrol-[fc_gotolabel])<>(finallyexceptionstate.oldflowcontrol*[fc_inflowcontrol,fc_catching_exceptions]) then
          CGMessage(cg_e_control_flow_outside_finally);
        if codegenerror then
          exit;

        { don't generate line info for internal cleanup }
        current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoStart));

        if fc_exit in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(2,oldCurrExitLabel);
        if fc_break in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(3,oldBreakLabel);
        if fc_continue in finallyexceptionstate.newflowcontrol then
          generate_exceptreason_check_br(4,oldContinueLabel);
        generate_exceptreason_reraise(1);

        cexceptionstatehandler.unget_exception_temps(current_asmdata.CurrAsmList,excepttemps);

        { end cleanup }
        current_asmdata.CurrAsmList.concat(tai_marker.create(mark_NoLineInfoEnd));

        current_procinfo.CurrExitLabel:=oldCurrExitLabel;
        if assigned(current_procinfo.CurrBreakLabel) then
         begin
           current_procinfo.CurrContinueLabel:=oldContinueLabel;
           current_procinfo.CurrBreakLabel:=oldBreakLabel;
         end;
        flowcontrol:=finallyexceptionstate.oldflowcontrol+(finallyexceptionstate.newflowcontrol-[fc_inflowcontrol,fc_catching_exceptions]);
      end;

    procedure twasmtryfinallynode.pass_generate_code;
      begin
        if ts_wasm_no_exceptions in current_settings.targetswitches then
          pass_generate_code_no_exceptions
        else if ts_wasm_js_exceptions in current_settings.targetswitches then
          pass_generate_code_js_exceptions
        else if ts_wasm_native_exceptions in current_settings.targetswitches then
          pass_generate_code_native_exceptions
        else if ts_wasm_bf_exceptions in current_settings.targetswitches then
          pass_generate_code_bf_exceptions
        else
          internalerror(2021091704);
      end;

{*****************************************************************************
                                  twasmonnode
*****************************************************************************}

    procedure twasmonnode.pass_generate_code_no_exceptions;
      begin
        { should not be called }
        internalerror(2021092803);
      end;

    procedure twasmonnode.pass_generate_code_js_exceptions;
      begin
        { not yet implemented }
        internalerror(2021092804);
      end;

    procedure twasmonnode.pass_generate_code_native_exceptions;
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

        in_loop:=assigned(current_procinfo.CurrBreakLabel);

        cexceptionstatehandler.begin_catch(current_asmdata.CurrAsmList,excepttype,nil,exceptlocdef,exceptlocreg);

        { Retrieve exception variable }
        if assigned(excepTSymtable) then
          exceptvarsym:=tlocalvarsym(excepTSymtable.SymList[0])
        else
          internalerror(2011020401);

        if assigned(exceptvarsym) then
          begin
            location_reset_ref(exceptvarsym.localloc, LOC_REFERENCE, def_cgsize(voidpointertype), voidpointertype.alignment, []);
            tg.GetLocal(current_asmdata.CurrAsmList, exceptvarsym.vardef.size, exceptvarsym.vardef, exceptvarsym.localloc.reference);
            hlcg.a_load_reg_ref(current_asmdata.CurrAsmList, exceptlocdef, exceptvarsym.vardef, exceptlocreg, exceptvarsym.localloc.reference);
          end;

        cexceptionstatehandler.new_exception(current_asmdata.CurrAsmList,excepttemps,tek_except,doobjectdestroyandreraisestate);

        { in the case that another exception is risen
          we've to destroy the old one, so create a new
          exception frame for the catch-handler }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_try));

        { the 'exit' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        oldCurrExitLabel:=current_procinfo.CurrExitLabel;
        current_asmdata.getjumplabel(NewCurrExitLabel);
        current_procinfo.CurrExitLabel:=NewCurrExitLabel;

        { the 'break' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldBreakLabel:=current_procinfo.CurrBreakLabel;
            current_asmdata.getjumplabel(NewBreakLabel);
            current_procinfo.CurrBreakLabel:=NewBreakLabel;
          end;

        { the 'continue' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldContinueLabel:=current_procinfo.CurrContinueLabel;
            current_asmdata.getjumplabel(NewContinueLabel);
            current_procinfo.CurrContinueLabel:=NewContinueLabel;
          end;

        if assigned(right) then
          secondpass(right);

        cexceptionstatehandler.end_try_block(current_asmdata.CurrAsmList,tek_except,excepttemps,doobjectdestroyandreraisestate,nil);

        hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
        current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,5));

        { exit the 'continue' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if in_loop then
          hlcg.a_label(current_asmdata.CurrAsmList,NewContinueLabel);
        if fc_continue in doobjectdestroyandreraisestate.newflowcontrol then
          begin
            hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
            current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_br,oldContinueLabel));
          end;

        { exit the 'break' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));  // break
        if in_loop then
          hlcg.a_label(current_asmdata.CurrAsmList,NewBreakLabel);
        if fc_break in doobjectdestroyandreraisestate.newflowcontrol then
          begin
            hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
            current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_br,oldBreakLabel));
          end;

        { exit the 'exit' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));  // exit
        hlcg.a_label(current_asmdata.CurrAsmList,NewCurrExitLabel);
        if fc_exit in doobjectdestroyandreraisestate.newflowcontrol then
          begin
            hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
            current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_br,oldCurrExitLabel));
          end;

        current_procinfo.CurrExitLabel:=oldCurrExitLabel;
        if in_loop then
          begin
            current_procinfo.CurrContinueLabel:=oldContinueLabel;
            current_procinfo.CurrBreakLabel:=oldBreakLabel;
          end;

        current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_catch,current_asmdata.WeakRefAsmSymbol(FPC_EXCEPTION_TAG_SYM,AT_WASM_EXCEPTION_TAG)));

        hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_raise_nested',[],nil).resetiftemp;

        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_try));

        { clear some stuff }
        if assigned(exceptvarsym) then
          begin
            tg.UngetLocal(current_asmdata.CurrAsmList,exceptvarsym.localloc.reference);
            exceptvarsym.localloc.loc:=LOC_INVALID;
          end;
        cexceptionstatehandler.end_catch(current_asmdata.CurrAsmList);

        { propagate exit/break/continue }
        flowcontrol:=doobjectdestroyandreraisestate.oldflowcontrol+(doobjectdestroyandreraisestate.newflowcontrol-[fc_inflowcontrol,fc_catching_exceptions]);

        { next on node }
        if assigned(left) then
          secondpass(left);
      end;

    procedure twasmonnode.pass_generate_code_bf_exceptions;
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

        in_loop:=assigned(current_procinfo.CurrBreakLabel);

        cexceptionstatehandler.begin_catch(current_asmdata.CurrAsmList,excepttype,nil,exceptlocdef,exceptlocreg);

        { Retrieve exception variable }
        if assigned(excepTSymtable) then
          exceptvarsym:=tlocalvarsym(excepTSymtable.SymList[0])
        else
          internalerror(2011020401);

        if assigned(exceptvarsym) then
          begin
            location_reset_ref(exceptvarsym.localloc, LOC_REFERENCE, def_cgsize(voidpointertype), voidpointertype.alignment, []);
            tg.GetLocal(current_asmdata.CurrAsmList, exceptvarsym.vardef.size, exceptvarsym.vardef, exceptvarsym.localloc.reference);
            hlcg.a_load_reg_ref(current_asmdata.CurrAsmList, exceptlocdef, exceptvarsym.vardef, exceptlocreg, exceptvarsym.localloc.reference);
          end;

        cexceptionstatehandler.new_exception(current_asmdata.CurrAsmList,excepttemps,tek_except,doobjectdestroyandreraisestate);

        { in the case that another exception is risen
          we've to destroy the old one, so create a new
          exception frame for the catch-handler }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));
        oldCurrRaiseLabel:=tcpuprocinfo(current_procinfo).CurrRaiseLabel;
        current_asmdata.getjumplabel(NewCurrRaiseLabel);
        tcpuprocinfo(current_procinfo).CurrRaiseLabel:=NewCurrRaiseLabel;

        { the 'exit' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        oldCurrExitLabel:=current_procinfo.CurrExitLabel;
        current_asmdata.getjumplabel(NewCurrExitLabel);
        current_procinfo.CurrExitLabel:=NewCurrExitLabel;

        { the 'break' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldBreakLabel:=current_procinfo.CurrBreakLabel;
            current_asmdata.getjumplabel(NewBreakLabel);
            current_procinfo.CurrBreakLabel:=NewBreakLabel;
          end;

        { the 'continue' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));

        if in_loop then
          begin
            oldContinueLabel:=current_procinfo.CurrContinueLabel;
            current_asmdata.getjumplabel(NewContinueLabel);
            current_procinfo.CurrContinueLabel:=NewContinueLabel;
          end;

        if assigned(right) then
          secondpass(right);

        cexceptionstatehandler.end_try_block(current_asmdata.CurrAsmList,tek_except,excepttemps,doobjectdestroyandreraisestate,nil);

        hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
        current_asmdata.CurrAsmList.concat(taicpu.op_const(a_br,6));

        { exit the 'continue' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
        if in_loop then
          hlcg.a_label(current_asmdata.CurrAsmList,NewContinueLabel);
        if fc_continue in doobjectdestroyandreraisestate.newflowcontrol then
          begin
            hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
            current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_br,oldContinueLabel));
          end;

        { exit the 'break' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));  // break
        if in_loop then
          hlcg.a_label(current_asmdata.CurrAsmList,NewBreakLabel);
        if fc_break in doobjectdestroyandreraisestate.newflowcontrol then
          begin
            hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
            current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_br,oldBreakLabel));
          end;

        { exit the 'exit' block }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));  // exit
        hlcg.a_label(current_asmdata.CurrAsmList,NewCurrExitLabel);
        if fc_exit in doobjectdestroyandreraisestate.newflowcontrol then
          begin
            hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_doneexception',[],nil).resetiftemp;
            current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_br,oldCurrExitLabel));
          end;

        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
        hlcg.a_label(current_asmdata.CurrAsmList,NewCurrRaiseLabel);

        current_procinfo.CurrExitLabel:=oldCurrExitLabel;
        if in_loop then
          begin
            current_procinfo.CurrContinueLabel:=oldContinueLabel;
            current_procinfo.CurrBreakLabel:=oldBreakLabel;
          end;
        tcpuprocinfo(current_procinfo).CurrRaiseLabel:=oldCurrRaiseLabel;

        hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_clear_exception_flag',[],nil).resetiftemp;
        hlcg.g_call_system_proc(current_asmdata.CurrAsmList,'fpc_raise_nested',[],nil).resetiftemp;
        hlcg.g_maybe_checkforexceptions(current_asmdata.CurrAsmList);

        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));

        { clear some stuff }
        if assigned(exceptvarsym) then
          begin
            tg.UngetLocal(current_asmdata.CurrAsmList,exceptvarsym.localloc.reference);
            exceptvarsym.localloc.loc:=LOC_INVALID;
          end;
        cexceptionstatehandler.end_catch(current_asmdata.CurrAsmList);

        { propagate exit/break/continue }
        flowcontrol:=doobjectdestroyandreraisestate.oldflowcontrol+(doobjectdestroyandreraisestate.newflowcontrol-[fc_inflowcontrol,fc_catching_exceptions]);

        { next on node }
        if assigned(left) then
          secondpass(left);
      end;

    procedure twasmonnode.pass_generate_code;
      begin
        if ts_wasm_no_exceptions in current_settings.targetswitches then
          pass_generate_code_no_exceptions
        else if ts_wasm_js_exceptions in current_settings.targetswitches then
          pass_generate_code_js_exceptions
        else if ts_wasm_native_exceptions in current_settings.targetswitches then
          pass_generate_code_native_exceptions
        else if ts_wasm_bf_exceptions in current_settings.targetswitches then
          pass_generate_code_bf_exceptions
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
