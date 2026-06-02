{
    Copyright (c) 2011-2020 by Free Pascal development team

    Generate Win64-specific exception handling code (based on x86_64 code)

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
unit ncpuflw;

{$i fpcdefs.inc}

interface

  uses
    node,nflw,ncgflw,psub,
    compilerbase;

  type
    taarch64raisenode=class(tcgraisenode)
      function pass_1 : tnode;override;
    end;

    taarch64onnode=class(tcgonnode)
      procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
    end;

    taarch64tryexceptnode=class(tcgtryexceptnode)
      procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
    end;

    taarch64tryfinallynode=class(tcgtryfinallynode)
      finalizepi: tcgprocinfo;
      constructor create(l,r:TNode;acompiler:TCompilerBase);override;
      constructor create_implicit(l,r:TNode;acompiler:TCompilerBase);override;
      function simplify(forinline: boolean): tnode;override;
      procedure pass_generate_code(ctx:tpassgeneratecodecontext);override;
      function dogetcopy:tnode;override;
    end;

implementation

  uses
    globtype,globals,verbose,systemstypes,systems,fmodule,
    nbas,ncal,nutils,
    symconst,symsym,symdef,
    cgbase,cgobj,cgutils,tgobj,
    cpubase,htypechk,
    pass_1,pass_2,pass_2_context,
    aasmbase,aasmtai,aasmdata,aasmcpu,
    procinfo,cpupi,procdefutil,
    compiler,nodehelper;

  var
    endexceptlabel: tasmlabel;


{ taarch64raisenode }

function taarch64raisenode.pass_1 : tnode;
  var
    statements : tstatementnode;
    raisenode : tcallnode;
  begin
    { difference from generic code is that address stack is not popped on reraise }
    if (compiler.target.info.system<>system_aarch64_win64) or assigned(left) then
      result:=inherited pass_1
    else
      begin
        result:=internalstatements(compiler,statements);
        raisenode:=compiler.ccallnode_intern('fpc_reraise',nil);
        include(raisenode.callnodeflags,cnf_call_never_returns);
        addstatement(statements,raisenode);
      end;
end;

{ taarch64onnode }

procedure taarch64onnode.pass_generate_code(ctx:tpassgeneratecodecontext);
  var
    exceptvarsym : tlocalvarsym;
  begin
    if (compiler.target.info.system<>system_aarch64_win64) then
      begin
        inherited;
        exit;
      end;

    location_reset(location,LOC_VOID,OS_NO);

    { RTL will put exceptobject into X0 when jumping here }
    ctx.cg.a_reg_alloc(ctx.CurrAsmList,NR_FUNCTION_RESULT_REG);

    { Retrieve exception variable }
    if assigned(excepTSymtable) then
      exceptvarsym:=tlocalvarsym(excepTSymtable.SymList[0])
    else
      exceptvarsym:=nil;

    if assigned(exceptvarsym) then
      begin
        exceptvarsym.localloc.loc:=LOC_REFERENCE;
        exceptvarsym.localloc.size:=OS_ADDR;
        ctx.tg.GetLocal(ctx.CurrAsmList,sizeof(pint),compiler.deftypes.voidpointertype,exceptvarsym.localloc.reference);
        ctx.cg.a_load_reg_ref(ctx.CurrAsmList,OS_ADDR,OS_ADDR,NR_FUNCTION_RESULT_REG,exceptvarsym.localloc.reference);
      end;
    ctx.cg.a_reg_dealloc(ctx.CurrAsmList,NR_FUNCTION_RESULT_REG);

    if assigned(right) then
      secondpass(right,ctx);

    { deallocate exception symbol }
    if assigned(exceptvarsym) then
      begin
        ctx.tg.UngetLocal(ctx.CurrAsmList,exceptvarsym.localloc.reference);
        exceptvarsym.localloc.loc:=LOC_INVALID;
      end;
    ctx.cg.g_call(ctx.CurrAsmList,'FPC_DONEEXCEPTION');
    ctx.cg.a_jmp_always(ctx.CurrAsmList,endexceptlabel);
  end;

{ taarch64tryfinallynode }

function reset_regvars(var n: tnode; arg: pointer): foreachnoderesult;
  begin
    case n.nodetype of
      temprefn:
        make_not_regable(n,[]);
      calln:
        include(tprocinfo(arg).flags,pi_do_call);
      else
        ;
    end;
    result:=fen_true;
  end;

function copy_parasize(var n: tnode; arg: pointer): foreachnoderesult;
  begin
    case n.nodetype of
      calln:
        tcgprocinfo(arg).allocate_push_parasize(tcallnode(n).pushed_parasize);
      else
        ;
    end;
    result:=fen_true;
  end;

constructor taarch64tryfinallynode.create(l, r: TNode;acompiler:TCompilerBase);
  begin
    inherited;
    if (compiler.target.info.system=system_aarch64_win64) and
      { Don't create child procedures for generic methods, their nested-like
        behavior causes compilation errors because real nested procedures
        aren't allowed for generics. Not creating them doesn't harm because
        generic node tree is discarded without generating code. }
       not (df_generic in compiler.current_procinfo.procdef.defoptions) then
      begin
        finalizepi:=tcgprocinfo(compiler.current_procinfo.create_for_outlining('$fin$',compiler.current_procinfo.procdef.struct,potype_exceptfilter,compiler.deftypes.voidtype,r));
        { the init/final code is messing with asm nodes, so inform the compiler about this }
        include(finalizepi.flags,pi_has_assembler_block);
        { Regvar optimization for symbols is suppressed when using exceptions, but
          temps may be still placed into registers. This must be fixed. }
        foreachnodestatic(r,@reset_regvars,finalizepi);
      end;
  end;

constructor taarch64tryfinallynode.create_implicit(l, r: TNode;acompiler:TCompilerBase);
  begin
    inherited;
    if (compiler.target.info.system=system_aarch64_win64) then
      begin
        if df_generic in compiler.current_procinfo.procdef.defoptions then
          InternalError(2020033101);

        finalizepi:=tcgprocinfo(compiler.current_procinfo.create_for_outlining('$fin$',compiler.current_procinfo.procdef.struct,potype_exceptfilter,compiler.deftypes.voidtype,r));
        include(finalizepi.flags,pi_do_call);
        { the init/final code is messing with asm nodes, so inform the compiler about this }
        include(finalizepi.flags,pi_has_assembler_block);
      end;
  end;

function taarch64tryfinallynode.simplify(forinline: boolean): tnode;
  begin
    result:=inherited simplify(forinline);
    if (compiler.target.info.system<>system_aarch64_win64) then
      exit;
    if (result=nil) then
      begin
        { actually, this is not really the right place to do a node transformation like this }
        if not(assigned(finalizepi.code)) then
          begin
            finalizepi.code:=right;
            foreachnodestatic(right,@copy_parasize,finalizepi);
            right:=compiler.ccallnode(nil,tprocsym(finalizepi.procdef.procsym),nil,nil,[],nil);
            firstpass(right);
            { For implicit frames, no actual code is available at this time,
              it is added later in assembler form. So store the nested procinfo
              for later use. }
            if implicitframe then
              begin
                compiler.current_procinfo.finalize_procinfo:=finalizepi;
              end;
          end;
      end;
  end;

procedure emit_nop(ctx:tpassgeneratecodecontext);
  var
    dummy: TAsmLabel;
  begin
    { To avoid optimizing away the whole thing, prepend a jumplabel with increased refcount }
    ctx.CurrAsmList.AsmData.getjumplabel(dummy);
    dummy.increfs;
    ctx.cg.a_label(ctx.CurrAsmList,dummy);
    ctx.CurrAsmList.concat(Taicpu.op_none(A_NOP));
  end;

procedure taarch64tryfinallynode.pass_generate_code(ctx:tpassgeneratecodecontext);
  var
    trylabel,
    endtrylabel,
    finallylabel,
    endfinallylabel,
    templabel,
    oldexitlabel: tasmlabel;
    oldflowcontrol: tflowcontrol;
    catch_frame: boolean;
  begin
    if (compiler.target.info.system<>system_aarch64_win64) then
      begin
        inherited;
        exit;
      end;

    location_reset(location,LOC_VOID,OS_NO);

    { Do not generate a frame that catches exceptions if the only action
      would be reraising it. Doing so is extremely inefficient with SEH
      (in contrast with setjmp/longjmp exception handling) }
    catch_frame:=implicitframe and
      (compiler.current_procinfo.procdef.proccalloption=pocall_safecall);

    oldflowcontrol:=flowcontrol;
    flowcontrol:=[fc_inflowcontrol];

    templabel:=nil;
    ctx.CurrAsmList.AsmData.getjumplabel(trylabel);
    ctx.CurrAsmList.AsmData.getjumplabel(endtrylabel);
    ctx.CurrAsmList.AsmData.getjumplabel(finallylabel);
    ctx.CurrAsmList.AsmData.getjumplabel(endfinallylabel);
    oldexitlabel:=compiler.current_procinfo.CurrExitLabel;
    if implicitframe then
      compiler.current_procinfo.CurrExitLabel:=finallylabel;

    { Start of scope }
    { Padding with NOP is necessary here because exceptions in called
      procedures are seen at the next instruction, while CPU/OS exceptions
      like AV are seen at the current instruction.

      So in the following code

      raise_some_exception;        //(a)
      try
        pchar(nil)^:='0';          //(b)
        ...

      without NOP, exceptions (a) and (b) will be seen at the same address
      and fall into the same scope. However they should be seen in different scopes.
    }

    emit_nop(ctx);
    ctx.cg.a_label(ctx.CurrAsmList,trylabel);

    { try code }
    if assigned(left) then
      begin
        { fc_unwind_xx tells exit/continue/break statements to emit special
          unwind code instead of just JMP }
        if not implicitframe then
          flowcontrol:=flowcontrol+[fc_catching_exceptions,fc_unwind_exit,fc_unwind_loop];
        secondpass(left,ctx);
        flowcontrol:=flowcontrol-[fc_catching_exceptions,fc_unwind_exit,fc_unwind_loop];
        if compiler.verbose.codegenerror then
          exit;
      end;

    { finallylabel is only used in implicit frames as an exit point from nested try..finally
      statements, if any. To prevent finalizer from being executed twice, it must come before
      endtrylabel (bug #34772) }
    if catch_frame then
      begin
        ctx.CurrAsmList.AsmData.getjumplabel(templabel);
        ctx.cg.a_label(ctx.CurrAsmList, finallylabel);
        { jump over exception handler }
        ctx.cg.a_jmp_always(ctx.CurrAsmList,templabel);
        { Handle the except block first, so endtrylabel serves both
          as end of scope and as unwind target. This way it is possible to
          encode everything into a single scope record. }
        ctx.cg.a_label(ctx.CurrAsmList,endtrylabel);
        if (compiler.current_procinfo.procdef.proccalloption=pocall_safecall) then
          begin
            handle_safecall_exception(ctx);
            ctx.cg.a_jmp_always(ctx.CurrAsmList,endfinallylabel);
          end
        else
          InternalError(2014031601);
        ctx.cg.a_label(ctx.CurrAsmList,templabel);
      end
    else
      begin
        { same as emit_nop but using finallylabel instead of dummy }
        ctx.cg.a_label(ctx.CurrAsmList,finallylabel);
        finallylabel.increfs;
        ctx.CurrAsmList.concat(Taicpu.op_none(A_NOP));
        ctx.cg.a_label(ctx.CurrAsmList,endtrylabel);
      end;

      { i32913 - if the try..finally block is also inside a try..finally or
        try..except block, make a note of any Exit calls so all necessary labels
        are generated. [Kit] }
      if ((flowcontrol*[fc_exit,fc_break,fc_continue])<>[]) and (fc_inflowcontrol in oldflowcontrol) then
        oldflowcontrol:=oldflowcontrol+(flowcontrol*[fc_exit,fc_break,fc_continue]);

    flowcontrol:=[fc_inflowcontrol];
    { store the tempflags so that we can generate a copy of the finally handler
      later on }
    if not implicitframe then
      finalizepi.store_tempflags;
    { generate the inline finalizer code }
    secondpass(right,ctx);

    if compiler.verbose.codegenerror then
      exit;

    { normal exit from safecall proc must zero the result register }
    if implicitframe and (compiler.current_procinfo.procdef.proccalloption=pocall_safecall) then
      ctx.cg.a_load_const_reg(ctx.CurrAsmList,OS_INT,0,NR_FUNCTION_RESULT_REG);

    ctx.cg.a_label(ctx.CurrAsmList,endfinallylabel);

    { generate the scope record in .xdata }
    tcpuprocinfo(compiler.current_procinfo).add_finally_scope(ctx,trylabel,endtrylabel,
      ctx.CurrAsmList.AsmData.RefAsmSymbol(finalizepi.procdef.mangledname,AT_FUNCTION),catch_frame);

    if implicitframe then
      compiler.current_procinfo.CurrExitLabel:=oldexitlabel;
    flowcontrol:=oldflowcontrol;
  end;

function taarch64tryfinallynode.dogetcopy: tnode;
  var
    n : taarch64tryfinallynode;
  begin
    n:=taarch64tryfinallynode(inherited dogetcopy);
    if (compiler.target.info.system=system_aarch64_win64) then
      begin
        n.finalizepi:=tcgprocinfo(cprocinfo.create(finalizepi.parent,compiler));
        n.finalizepi.force_nested;
        n.finalizepi.procdef:=compiler.procdefutil.create_outline_procdef('$fin$',compiler.current_procinfo.procdef.struct,potype_exceptfilter,compiler.deftypes.voidtype);
        n.finalizepi.entrypos:=finalizepi.entrypos;
        n.finalizepi.entryswitches:=finalizepi.entryswitches;
        n.finalizepi.exitpos:=finalizepi.exitpos;
        n.finalizepi.exitswitches:=finalizepi.exitswitches;
        n.finalizepi.flags:=finalizepi.flags;
        { node already transformed? }
        if assigned(finalizepi.code) then
          begin
            n.finalizepi.code:=finalizepi.code.getcopy;
            n.right:=compiler.ccallnode(nil,tprocsym(n.finalizepi.procdef.procsym),nil,nil,[],nil);
            firstpass(n.right);
          end;
      end;
    result:=n;
  end;


{ taarch64tryexceptnode }

procedure taarch64tryexceptnode.pass_generate_code(ctx:tpassgeneratecodecontext);
  var
    trylabel,
    exceptlabel,oldendexceptlabel,
    lastonlabel,
    exitexceptlabel,
    continueexceptlabel,
    breakexceptlabel,
    oldCurrExitLabel,
    oldContinueLabel,
    oldBreakLabel : tasmlabel;
    onlabel,
    filterlabel: tasmlabel;
    oldflowcontrol,tryflowcontrol,
    exceptflowcontrol : tflowcontrol;
    hnode : tnode;
    hlist : tasmlist;
    onnodecount : tai_const;
    sym : tasmsymbol;
  label
    errorexit;
  begin
    if (compiler.target.info.system<>system_aarch64_win64) then
      begin
        inherited;
        exit;
      end;
    location_reset(location,LOC_VOID,OS_NO);

    oldflowcontrol:=flowcontrol;
    exceptflowcontrol:=[];
    continueexceptlabel:=nil;
    breakexceptlabel:=nil;

    include(flowcontrol,fc_inflowcontrol);
    { this can be called recursively }
    oldBreakLabel:=nil;
    oldContinueLabel:=nil;
    oldendexceptlabel:=endexceptlabel;

    { save the old labels for control flow statements }
    oldCurrExitLabel:=compiler.current_procinfo.CurrExitLabel;
    ctx.CurrAsmList.AsmData.getjumplabel(exitexceptlabel);
    if assigned(compiler.current_procinfo.CurrBreakLabel) then
      begin
        oldContinueLabel:=compiler.current_procinfo.CurrContinueLabel;
        oldBreakLabel:=compiler.current_procinfo.CurrBreakLabel;
        ctx.CurrAsmList.AsmData.getjumplabel(breakexceptlabel);
        ctx.CurrAsmList.AsmData.getjumplabel(continueexceptlabel);
      end;

    ctx.CurrAsmList.AsmData.getjumplabel(exceptlabel);
    ctx.CurrAsmList.AsmData.getjumplabel(endexceptlabel);
    ctx.CurrAsmList.AsmData.getjumplabel(lastonlabel);
    filterlabel:=nil;

    { start of scope }
    ctx.CurrAsmList.AsmData.getjumplabel(trylabel);
    emit_nop(ctx);
    ctx.cg.a_label(ctx.CurrAsmList,trylabel);

    { control flow in try block needs no special handling,
      just make sure that target labels are outside the scope }
    secondpass(left,ctx);
    tryflowcontrol:=flowcontrol;
    if compiler.verbose.codegenerror then
      goto errorexit;

    { jump over except handlers }
    ctx.cg.a_jmp_always(ctx.CurrAsmList,endexceptlabel);

    { end of scope }
    ctx.cg.a_label(ctx.CurrAsmList,exceptlabel);

    { set control flow labels for the except block }
    { and the on statements                        }
    compiler.current_procinfo.CurrExitLabel:=exitexceptlabel;
    if assigned(oldBreakLabel) then
      begin
        compiler.current_procinfo.CurrContinueLabel:=continueexceptlabel;
        compiler.current_procinfo.CurrBreakLabel:=breakexceptlabel;
      end;

    flowcontrol:=[fc_inflowcontrol];
    { on statements }
    if assigned(right) then
      begin
        { emit filter table to a temporary asmlist }
        hlist:=TAsmList.Create(ctx.CurrAsmList.AsmData);
        ctx.CurrAsmList.AsmData.getaddrlabel(filterlabel);
        new_section(hlist,sec_rodata_norel,filterlabel.name,4);
        ctx.cg.a_label(hlist,filterlabel);
        onnodecount:=tai_const.create_32bit(0);
        hlist.concat(onnodecount);

        hnode:=right;
        while assigned(hnode) do
          begin
            if hnode.nodetype<>onn then
              InternalError(2011103101);
            ctx.CurrAsmList.AsmData.getjumplabel(onlabel);
            sym:=ctx.CurrAsmList.AsmData.RefAsmSymbol(tonnode(hnode).excepttype.vmt_mangledname,AT_DATA,true);
            hlist.concat(tai_const.create_rva_sym(sym));
            hlist.concat(tai_const.create_rva_sym(onlabel));
            compiler.current_module.add_extern_asmsym(sym);
            ctx.cg.a_label(ctx.CurrAsmList,onlabel);
            secondpass(hnode,ctx);
            inc(onnodecount.value);
            hnode:=tonnode(hnode).left;
          end;
        { add 'else' node to the filter list, too }
        if assigned(t1) then
          begin
            hlist.concat(tai_const.create_32bit(-1));
            hlist.concat(tai_const.create_rva_sym(lastonlabel));
            inc(onnodecount.value);
          end;
        { now move filter table to permanent list all at once }
        compiler.current_procinfo.aktlocaldata.concatlist(hlist);
        hlist.free;
      end;

    ctx.cg.a_label(ctx.CurrAsmList,lastonlabel);
    if assigned(t1) then
      begin
        { here we don't have to reset flowcontrol           }
        { the default and on flowcontrols are handled equal }
        secondpass(t1,ctx);
        ctx.cg.g_call(ctx.CurrAsmList,'FPC_DONEEXCEPTION');
        if (flowcontrol*[fc_exit,fc_break,fc_continue]<>[]) then
          ctx.cg.a_jmp_always(ctx.CurrAsmList,endexceptlabel);
      end;
    exceptflowcontrol:=flowcontrol;

    if fc_exit in exceptflowcontrol then
      begin
        { do some magic for exit in the try block }
        ctx.cg.a_label(ctx.CurrAsmList,exitexceptlabel);
        ctx.cg.g_call(ctx.CurrAsmList,'FPC_DONEEXCEPTION');
        if (fc_unwind_exit in oldflowcontrol) then
          ctx.cg.g_local_unwind(ctx.CurrAsmList,oldCurrExitLabel)
        else
          ctx.cg.a_jmp_always(ctx.CurrAsmList,oldCurrExitLabel);
      end;

    if fc_break in exceptflowcontrol then
      begin
        ctx.cg.a_label(ctx.CurrAsmList,breakexceptlabel);
        ctx.cg.g_call(ctx.CurrAsmList,'FPC_DONEEXCEPTION');
        if (fc_unwind_loop in oldflowcontrol) then
          ctx.cg.g_local_unwind(ctx.CurrAsmList,oldBreakLabel)
        else
          ctx.cg.a_jmp_always(ctx.CurrAsmList,oldBreakLabel);
      end;

    if fc_continue in exceptflowcontrol then
      begin
        ctx.cg.a_label(ctx.CurrAsmList,continueexceptlabel);
        ctx.cg.g_call(ctx.CurrAsmList,'FPC_DONEEXCEPTION');
        if (fc_unwind_loop in oldflowcontrol) then
          ctx.cg.g_local_unwind(ctx.CurrAsmList,oldContinueLabel)
        else
          ctx.cg.a_jmp_always(ctx.CurrAsmList,oldContinueLabel);
      end;

    emit_nop(ctx);
    ctx.cg.a_label(ctx.CurrAsmList,endexceptlabel);
    tcpuprocinfo(compiler.current_procinfo).add_except_scope(ctx,trylabel,exceptlabel,endexceptlabel,filterlabel);

errorexit:
    { restore all saved labels }
    endexceptlabel:=oldendexceptlabel;

    { restore the control flow labels }
    compiler.current_procinfo.CurrExitLabel:=oldCurrExitLabel;
    if assigned(oldBreakLabel) then
      begin
        compiler.current_procinfo.CurrContinueLabel:=oldContinueLabel;
        compiler.current_procinfo.CurrBreakLabel:=oldBreakLabel;
      end;

    { return all used control flow statements }
    flowcontrol:=oldflowcontrol+(exceptflowcontrol +
      tryflowcontrol - [fc_inflowcontrol]);
  end;

initialization
  craisenode:=taarch64raisenode;
  connode:=taarch64onnode;
  ctryexceptnode:=taarch64tryexceptnode;
  ctryfinallynode:=taarch64tryfinallynode;
end.

