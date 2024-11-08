{
    Copyright (c) 1998-2002 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit implements the ARM64 optimizer object

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

Unit aoptcpu;

{$i fpcdefs.inc}

{$ifdef EXTDEBUG}
{$define DEBUG_AOPTCPU}
{$endif EXTDEBUG}

Interface

    uses
      globtype, globals,
      cutils,
      cgbase, cpubase, aasmtai, aasmcpu,
      aopt, aoptcpub, aoptarm, aoptobj;

    Type
      TCpuAsmOptimizer = class(TARMAsmOptimizer)
        { uses the same constructor as TAopObj }
        function PrePeepHoleOptsCpu(var p: tai): boolean; override;
        function PeepHoleOptPass1Cpu(var p: tai): boolean; override;
        function PeepHoleOptPass2Cpu(var p: tai): boolean; override;
        function PostPeepHoleOptsCpu(var p: tai): boolean; override;
        function RegLoadedWithNewValue(reg: tregister; hp: tai): boolean;override;
        function InstructionLoadsFromReg(const reg: TRegister; const hp: tai): boolean;override;
        function LookForPostindexedPattern(var p : tai) : boolean;
      public
        { With these routines, there's optimisation code that's general for all ARM platforms }
        function OptPass1LDR(var p: tai): Boolean; override;
        function OptPass1STR(var p: tai): Boolean; override;
      private
        function RemoveSuperfluousFMov(const p: tai; movp: tai; const optimizer: string): boolean;
        function OptPass1Shift(var p: tai): boolean;
        function OptPass1Data(var p: tai): boolean;
        function OptPass1FData(var p: tai): Boolean;
        function OptPass1STP(var p: tai): boolean;
        function OptPass1Mov(var p: tai): boolean;
        function OptPass1MOVZ(var p: tai): boolean;
        function OptPass1FMov(var p: tai): Boolean;
        function OptPass1B(var p: tai): boolean;
        function OptPass1SXTW(var p: tai): Boolean;

        function OptPass2CSEL(var p: tai): Boolean;
        function OptPass2B(var p: tai): Boolean;
        function OptPass2LDRSTR(var p: tai): boolean;
        function OptPass2MOV(var p: tai): Boolean;

        function PostPeepholeOptAND(var p: tai): Boolean;
        function PostPeepholeOptCMP(var p: tai): boolean;
        function PostPeepholeOptTST(var p: tai): Boolean;
      protected
        { Like UpdateUsedRegs, but ignores deallocations }
        class procedure UpdateIntRegsNoDealloc(var AUsedRegs: TAllUsedRegs; p: Tai); static;

        { Attempts to allocate a volatile integer register for use between p and hp,
          using AUsedRegs for the current register usage information.  Returns NR_NO
          if no free register could be found }
        function GetIntRegisterBetween(RegSize: TSubRegister; var AUsedRegs: TAllUsedRegs; p, hp: tai; DontAlloc: Boolean = False): TRegister;
      End;

Implementation

  uses
    aasmbase,
    aoptbase,
    aoptutils,
    cgutils,
    procinfo,
    paramgr,
    verbose;

{$ifdef DEBUG_AOPTCPU}
    const
      SPeepholeOptimization: shortstring = 'Peephole Optimization: ';
{$else DEBUG_AOPTCPU}
    { Empty strings help the optimizer to remove string concatenations that won't
      ever appear to the user on release builds. [Kit] }
    const
      SPeepholeOptimization = '';
{$endif DEBUG_AOPTCPU}

      MAX_CSEL_INSTRUCTIONS = 8;
      MAX_CSEL_REGISTERS = 30;

    type
      TCSELTrackingState = (tsInvalid, tsSimple, tsDetour, tsBranching,
        tsDouble, tsDoubleBranchSame, tsDoubleBranchDifferent, tsDoubleSecondBranching,
        tsProcessed);

      { For OptPass2Jcc }
      TCSELTracking = object
      private
        CSELScore, ConstCount: LongInt;

        RegWrites: array[0..MAX_CSEL_INSTRUCTIONS*2 - 1] of TRegister;

        ConstRegs: array[0..MAX_CSEL_REGISTERS - 1] of TRegister;
        ConstVals: array[0..MAX_CSEL_REGISTERS - 1] of TCGInt;
        ConstSizes: array[0..MAX_CSEL_REGISTERS - 1] of TSubRegister; { May not match ConstRegs if one is shared over multiple CSELs. }
        ConstMovs: array[0..MAX_CSEL_REGISTERS - 1] of tai; { Location of initialisation instruction }

        ConstWriteSizes: array[0..first_int_imreg - 1] of TSubRegister; { Largest size of register written. }

        fOptimizer: TCpuAsmOptimizer;

        fLabel: TAsmSymbol;

        fInsertionPoint,
        fCondition,
        fInitialJump,
        fFirstMovBlock,
        fFirstMovBlockStop,
        fSecondJump,
        fThirdJump,
        fSecondMovBlock,
        fSecondMovBlockStop,
        fMidLabel,
        fEndLabel,
        fAllocationRange: tai;

        fState: TCSELTrackingState;

        function TryCSELConst(p, start, stop: tai; var Count: LongInt): Boolean;
        function InitialiseBlock(BlockStart, OneBeforeBlock: tai; out BlockStop: tai; out EndJump: tai): Boolean;
        function AnalyseMOVBlock(BlockStart, BlockStop, SearchStart: tai): LongInt;
      public
        RegisterTracking: TAllUsedRegs;
        constructor Init(Optimizer: TCpuAsmOptimizer; var p_initialjump, p_initialmov: tai; var AFirstLabel: TAsmLabel);
        destructor Done;
        procedure Process(out new_p: tai);
        property State: TCSELTrackingState read fState;
      end;

      PCSELTracking = ^TCSELTracking;

  function CanBeCond(p : tai) : boolean;
    begin
      result:=(p.typ=ait_instruction) and (taicpu(p).condition=C_None);
    end;


  function TCpuAsmOptimizer.RegLoadedWithNewValue(reg: tregister; hp: tai): boolean;
    var
      p: taicpu;
    begin
      Result := false;
      if not ((assigned(hp)) and (hp.typ = ait_instruction)) then
        exit;

      p := taicpu(hp);
      case p.opcode of
        { These operations do not write into a register at all

          LDR/STR with post/pre-indexed operations do not need special treatment
          because post-/preindexed does not mean that a register
          is loaded with a new value, it is only modified }
        A_STR, A_CMP, A_CMN, A_TST, A_B, A_BL, A_MSR, A_FCMP:
          exit;
        else
          ;
      end;

      if p.ops=0 then
        exit;

      case p.oper[0]^.typ of
        top_reg:
          Result := SuperRegistersEqual(p.oper[0]^.reg,reg);
        top_ref:
          Result :=
            (taicpu(p).oper[0]^.ref^.addressmode in [AM_PREINDEXED,AM_POSTINDEXED]) and
            (taicpu(p).oper[0]^.ref^.base = reg);
        else
          ;
      end;
    end;


  function TCpuAsmOptimizer.InstructionLoadsFromReg(const reg: TRegister; const hp: tai): boolean;
    var
      p: taicpu;
      i: longint;
    begin
      instructionLoadsFromReg := false;
      if not (assigned(hp) and (hp.typ = ait_instruction)) then
        exit;
      p:=taicpu(hp);

      i:=1;

      { Start on oper[0]? }
      if taicpu(hp).spilling_get_operation_type(0) in [operand_read, operand_readwrite] then
        i:=0;

      while(i<p.ops) do
        begin
          case p.oper[I]^.typ of
            top_reg:
              Result := (p.oper[I]^.reg = reg);
            top_ref:
              Result :=
                (p.oper[I]^.ref^.base = reg) or
                (p.oper[I]^.ref^.index = reg);
            else
              ;
          end;
          { Bailout if we found something }
          if Result then
            exit;
          Inc(I);
        end;
    end;

  {
    optimize
      ldr/str regX,[reg1]
      ...
      add/sub reg1,reg1,regY/const

      into

      ldr/str regX,[reg1], regY/const
  }
  function TCpuAsmOptimizer.LookForPostindexedPattern(var p: tai) : boolean;
    var
      hp1 : tai;
    begin
      Result:=false;
      if (taicpu(p).oper[1]^.typ = top_ref) and
        (taicpu(p).oper[1]^.ref^.addressmode=AM_OFFSET) and
        (taicpu(p).oper[1]^.ref^.index=NR_NO) and
        (taicpu(p).oper[1]^.ref^.offset=0) and
        GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[1]^.ref^.base) and
        { we cannot check NR_DEFAULTFLAGS for modification yet so don't allow a condition }
        MatchInstruction(hp1, [A_ADD, A_SUB], [PF_None]) and
        (taicpu(hp1).oper[0]^.reg=taicpu(p).oper[1]^.ref^.base) and
        (taicpu(hp1).oper[1]^.reg=taicpu(p).oper[1]^.ref^.base) and
        (
         { valid offset? }
         (taicpu(hp1).oper[2]^.typ=top_const) and
         (taicpu(hp1).oper[2]^.val>=-256) and
         (abs(taicpu(hp1).oper[2]^.val)<256)
        ) and
        { don't apply the optimization if the base register is loaded }
        (getsupreg(taicpu(p).oper[0]^.reg)<>getsupreg(taicpu(p).oper[1]^.ref^.base)) and
        not(RegModifiedBetween(taicpu(hp1).oper[0]^.reg,p,hp1)) and
        not(RegModifiedBetween(taicpu(hp1).oper[2]^.reg,p,hp1)) then
        begin
          if taicpu(p).opcode = A_LDR then
            DebugMsg(SPeepholeOptimization + 'LdrAdd/Sub2Ldr Postindex done', p)
          else
            DebugMsg(SPeepholeOptimization + 'StrAdd/Sub2Str Postindex done', p);

          taicpu(p).oper[1]^.ref^.addressmode:=AM_POSTINDEXED;
          if taicpu(hp1).opcode=A_ADD then
            taicpu(p).oper[1]^.ref^.offset:=taicpu(hp1).oper[2]^.val
          else
            taicpu(p).oper[1]^.ref^.offset:=-taicpu(hp1).oper[2]^.val;
          asml.Remove(hp1);
          hp1.Free;
          Result:=true;
        end;
    end;


  function TCpuAsmOptimizer.RemoveSuperfluousFMov(const p: tai; movp: tai; const optimizer: string):boolean;
    var
      alloc,
      dealloc : tai_regalloc;
      hp1 : tai;
    begin
      Result:=false;
      if ((MatchInstruction(movp, A_FMOV, [taicpu(p).condition], [taicpu(p).oppostfix]) and
           ((getregtype(taicpu(movp).oper[0]^.reg)=R_MMREGISTER) { or (taicpu(p).opcode in [A_LDUR])})
          ) { or
          (((taicpu(p).oppostfix in [PF_F64F32,PF_F64S16,PF_F64S32,PF_F64U16,PF_F64U32]) or (getsubreg(taicpu(p).oper[0]^.reg)=R_SUBFD)) and MatchInstruction(movp, A_VMOV, [taicpu(p).condition], [PF_F64])) or
          (((taicpu(p).oppostfix in [PF_F32F64,PF_F32S16,PF_F32S32,PF_F32U16,PF_F32U32]) or (getsubreg(taicpu(p).oper[0]^.reg)=R_SUBFS)) and MatchInstruction(movp, A_VMOV, [taicpu(p).condition], [PF_F32])) }
         ) and
         (taicpu(movp).ops=2) and
         MatchOperand(taicpu(movp).oper[1]^, taicpu(p).oper[0]^.reg) and
         { the destination register of the mov might not be used beween p and movp }
         not(RegUsedBetween(taicpu(movp).oper[0]^.reg,p,movp)) and
         { Take care to only do this for instructions which REALLY load to the first register.
           Otherwise
             str reg0, [reg1]
             fmov reg2, reg0
           will be optimized to
             str reg2, [reg1]
         }
         RegLoadedWithNewValue(taicpu(p).oper[0]^.reg, p) then
        begin
          dealloc:=FindRegDeAlloc(taicpu(p).oper[0]^.reg,tai(movp.Next));
          if assigned(dealloc) then
            begin
              DebugMsg(SPeepholeOptimization + optimizer+' removed superfluous vmov', movp);
              result:=true;

              { taicpu(p).oper[0]^.reg is not used anymore, try to find its allocation
                and remove it if possible }
              asml.Remove(dealloc);
              alloc:=FindRegAllocBackward(taicpu(p).oper[0]^.reg,tai(p.previous));
              if assigned(alloc) then
                begin
                  asml.Remove(alloc);
                  alloc.free;
                  dealloc.free;
                end
              else
                asml.InsertAfter(dealloc,p);

              { try to move the allocation of the target register }
              GetLastInstruction(movp,hp1);
              alloc:=FindRegAlloc(taicpu(movp).oper[0]^.reg,tai(hp1.Next));
              if assigned(alloc) then
                begin
                  asml.Remove(alloc);
                  asml.InsertBefore(alloc,p);
                  { adjust used regs }
                  IncludeRegInUsedRegs(taicpu(movp).oper[0]^.reg,UsedRegs);
                end;

              { change
                  vldr reg0,[reg1]
                  vmov reg2,reg0
                into
                  ldr reg2,[reg1]

                if reg2 is an int register
              if (taicpu(p).opcode=A_VLDR) and (getregtype(taicpu(movp).oper[0]^.reg)=R_INTREGISTER) then
                taicpu(p).opcode:=A_LDR;
              }

              { finally get rid of the mov }
              taicpu(p).loadreg(0,taicpu(movp).oper[0]^.reg);
              asml.remove(movp);
              movp.free;
            end;
        end;
    end;


  function TCpuAsmOptimizer.OptPass1LDR(var p: tai): Boolean;
    var
      hp1: tai;
    begin
      Result := False;
      if inherited OptPass1LDR(p) or
        LookForPostindexedPattern(p) then
        Exit(True)
      else if (taicpu(p).oppostfix in [PF_B,PF_SB,PF_H,PF_SH,PF_None]) and
        GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) and
        RemoveSuperfluousMove(p, hp1, 'Ldr<Postfix>Mov2Ldr<Postfix>') then
        Exit(true);
    end;


  function TCpuAsmOptimizer.OptPass1STR(var p: tai): Boolean;
    begin
      Result := False;
      if inherited OptPass1STR(p) or
        LookForPostindexedPattern(p) then
        Exit(True);

      if getsupreg(taicpu(p).oper[0]^.reg) = RS_WZR then
        Result := TryConstMerge(p, nil);
    end;


  function TCpuAsmOptimizer.OptPass1Shift(var p : tai): boolean;
    var
      hp1,hp2: tai;
      I2, I: Integer;
      shifterop: tshifterop;
    begin
      Result:=false;
      { This folds shifterops into following instructions
        <shiftop> r0, r1, #imm
        <op> r2, r3, r0

        to

        <op> r2, r3, r1, <shiftop> #imm
      }
      { do not handle ROR yet, only part of the instructions below support ROR as shifter operand }
      if MatchInstruction(p,[A_LSL, A_LSR, A_ASR{, A_ROR}],[PF_None]) and
         MatchOpType(taicpu(p),top_reg,top_reg,top_const) and
         GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) and
         MatchInstruction(hp1, [A_ADD, A_AND, A_BIC, A_CMP, A_CMN,
                                A_EON, A_EOR, A_NEG, A_ORN, A_ORR,
                                A_SUB, A_TST], [PF_None]) and
         RegEndOfLife(taicpu(p).oper[0]^.reg, taicpu(hp1)) and
         (taicpu(hp1).ops >= 2) and
         { Currently we can't fold into another shifterop }
         (taicpu(hp1).oper[taicpu(hp1).ops-1]^.typ = top_reg) and
         { SP does not work completely with shifted registers, as I didn't find the exact rules,
           we do not operate on SP }
         (taicpu(hp1).oper[0]^.reg<>NR_SP) and
         (taicpu(hp1).oper[1]^.reg<>NR_SP) and
         (taicpu(hp1).oper[taicpu(hp1).ops-1]^.reg<>NR_SP) and
         { reg1 might not be modified inbetween }
         not(RegModifiedBetween(taicpu(p).oper[1]^.reg,p,hp1)) and
         (
           { Only ONE of the two src operands is allowed to match }
           MatchOperand(taicpu(p).oper[0]^, taicpu(hp1).oper[taicpu(hp1).ops-2]^) xor
           MatchOperand(taicpu(p).oper[0]^, taicpu(hp1).oper[taicpu(hp1).ops-1]^)
         ) and
         { for SUB, the last operand must match, there is no RSB on AArch64 }
         ((taicpu(hp1).opcode<>A_SUB) or
          MatchOperand(taicpu(p).oper[0]^, taicpu(hp1).oper[taicpu(hp1).ops-1]^)) then
        begin
          { for the two operand instructions, start also at the second operand as they are not always commutative
            (depends on the flags tested laster on) and thus the operands cannot swapped }
          I2:=1;
          for I:=I2 to taicpu(hp1).ops-1 do
            if MatchOperand(taicpu(p).oper[0]^, taicpu(hp1).oper[I]^.reg) then
              begin
                { If the parameter matched on the second op from the RIGHT
                  we have to switch the parameters, this will not happen for CMP
                  were we're only evaluating the most right parameter
                }
                shifterop_reset(shifterop);
                case taicpu(p).opcode of
                  A_LSL:
                    shifterop.shiftmode:=SM_LSL;
                  A_ROR:
                    shifterop.shiftmode:=SM_ROR;
                  A_LSR:
                    shifterop.shiftmode:=SM_LSR;
                  A_ASR:
                    shifterop.shiftmode:=SM_ASR;
                  else
                    InternalError(2019090401);
                end;
                shifterop.shiftimm:=taicpu(p).oper[2]^.val;

                if I <> taicpu(hp1).ops-1 then
                  begin
                    if taicpu(hp1).ops = 3 then
                      hp2:=taicpu.op_reg_reg_reg_shifterop(taicpu(hp1).opcode,
                           taicpu(hp1).oper[0]^.reg, taicpu(hp1).oper[2]^.reg,
                           taicpu(p).oper[1]^.reg, shifterop)
                    else
                      hp2:=taicpu.op_reg_reg_shifterop(taicpu(hp1).opcode,
                           taicpu(hp1).oper[0]^.reg, taicpu(p).oper[1]^.reg,
                           shifterop);
                  end
                else
                  if taicpu(hp1).ops = 3 then
                    hp2:=taicpu.op_reg_reg_reg_shifterop(taicpu(hp1).opcode,
                         taicpu(hp1).oper[0]^.reg, taicpu(hp1).oper[1]^.reg,
                         taicpu(p).oper[1]^.reg,shifterop)
                  else
                    hp2:=taicpu.op_reg_reg_shifterop(taicpu(hp1).opcode,
                         taicpu(hp1).oper[0]^.reg, taicpu(p).oper[1]^.reg,
                         shifterop);

                { Make sure the register used in the shifting is tracked all
                  the way through, otherwise it may become deallocated while
                  it's still live and cause incorrect optimisations later }
                if (taicpu(hp1).oper[0]^.reg <> taicpu(p).oper[1]^.reg) then
                  begin
                    TransferUsedRegs(TmpUsedRegs);
                    UpdateUsedRegs(TmpUsedRegs, tai(p.Next));
                    ALlocRegBetween(taicpu(p).oper[1]^.reg, p, hp1, TmpUsedRegs);
                  end;

                taicpu(hp2).fileinfo:=taicpu(hp1).fileinfo;
                asml.insertbefore(hp2, hp1);

                RemoveInstruction(hp1);
                RemoveCurrentp(p);

                DebugMsg(SPeepholeOptimization + 'FoldShiftProcess done', hp2);
                Result:=true;
                break;
              end;
        end
      else if MatchInstruction(p,[A_LSL, A_LSR, A_ASR,A_ROR],[PF_None]) and
        GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) and
        RemoveSuperfluousMove(p, hp1, 'ShiftMov2Shift') then
        Result:=true;
    end;


  function TCpuAsmOptimizer.OptPass1Data(var p : tai): boolean;
    var
      hp1: tai;
    begin
      Result := GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) and
        RemoveSuperfluousMove(p, hp1, 'DataMov2Data');
    end;


  function TCpuAsmOptimizer.OptPass1FData(var p: tai): Boolean;
    var
      hp1: tai;
    begin
      Result := GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) and
        RemoveSuperfluousFMov(p, hp1, 'FOpFMov2FOp');
    end;


  function TCpuAsmOptimizer.OptPass1STP(var p : tai): boolean;
    var
      hp1, hp2, hp3, hp4, tmp1 : tai;
    begin
      Result:=false;
      {
        change

	stp	x29,x30,[sp, #-16]!
	mov	x29,sp
	bl	abc
	ldp	x29,x30,[sp], #16
	ret

        into

        b         abc
      }
      if MatchInstruction(p, A_STP, [C_None], [PF_None]) and
        MatchOpType(taicpu(p),top_reg,top_reg,top_ref) and
        (taicpu(p).oper[0]^.reg = NR_X29) and
        (taicpu(p).oper[1]^.reg = NR_X30) and
        (taicpu(p).oper[2]^.ref^.base=NR_STACK_POINTER_REG) and
        (taicpu(p).oper[2]^.ref^.index=NR_NO) and
        (taicpu(p).oper[2]^.ref^.offset=-16) and
        (taicpu(p).oper[2]^.ref^.addressmode=AM_PREINDEXED) and

        GetNextInstruction(p, hp1) and
        MatchInstruction(hp1, A_MOV, [C_None], [PF_NONE]) and
        MatchOperand(taicpu(hp1).oper[0]^,taicpu(p).oper[0]^) and
        (taicpu(hp1).oper[1]^.typ = top_reg) and
        (taicpu(hp1).oper[1]^.reg = NR_STACK_POINTER_REG) and

        GetNextInstruction(hp1, hp2) and
        SkipEntryExitMarker(hp2, hp2) and
        MatchInstruction(hp2, A_BL, [C_None], [PF_NONE]) and
        (taicpu(hp2).oper[0]^.typ = top_ref) and

        GetNextInstruction(hp2, hp3) and
        SkipEntryExitMarker(hp3, hp3) and
        MatchInstruction(hp3, A_LDP, [C_None], [PF_NONE]) and
        MatchOpType(taicpu(hp3),top_reg,top_reg,top_ref) and
        (taicpu(hp3).oper[0]^.reg = NR_X29) and
        (taicpu(hp3).oper[1]^.reg = NR_X30) and
        (taicpu(hp3).oper[2]^.ref^.base=NR_STACK_POINTER_REG) and
        (taicpu(hp3).oper[2]^.ref^.index=NR_NO) and
        (taicpu(hp3).oper[2]^.ref^.offset=16) and
        (taicpu(hp3).oper[2]^.ref^.addressmode=AM_POSTINDEXED) and

        GetNextInstruction(hp3, hp4) and
        MatchInstruction(hp4, A_RET, [C_None], [PF_None]) and
        (taicpu(hp4).ops = 0) then
        begin
          { remove the SEH instruction for the STP FP,LR }
          if GetNextInstruction(p,tmp1,[ait_seh_directive]) and
              (tmp1.typ=ait_seh_directive) and
              (tai_seh_directive(tmp1).kind=ash_savefplr_x) then
            begin
              asml.Remove(tmp1);
              tmp1.free;
            end;
          { remove the SEH instruction for the MOV FP,SP }
          if GetNextInstruction(hp1,tmp1,[ait_seh_directive]) and
              (tmp1.typ=ait_seh_directive) and
              (tai_seh_directive(tmp1).kind=ash_setfp) then
            begin
              asml.Remove(tmp1);
              tmp1.free;
            end;
          asml.Remove(p);
          asml.Remove(hp1);
          asml.Remove(hp3);
          asml.Remove(hp4);
          taicpu(hp2).opcode:=A_B;
          p.free;
          hp1.free;
          hp3.free;
          hp4.free;
          p:=hp2;
          DebugMsg(SPeepholeOptimization + 'Bl2B done', p);
          Result:=true;
        end;
    end;


  function TCpuAsmOptimizer.OptPass1Mov(var p : tai): boolean;
    var
      hp1: tai;
      so: tshifterop;
    begin
     Result:=false;
     if MatchOperand(taicpu(p).oper[0]^,taicpu(p).oper[1]^) and
       (taicpu(p).oppostfix=PF_None) then
       begin
         RemoveCurrentP(p);
         DebugMsg(SPeepholeOptimization + 'Mov2None done', p);
         Result:=true;
       end


     else if (taicpu(p).ops=2) and
       (getsubreg(taicpu(p).oper[0]^.reg)=R_SUBD) and
       GetNextInstruction(p, hp1) and
       { Faster to get it out of the way than go through MatchInstruction }
       (hp1.typ=ait_instruction) and
       (taicpu(hp1).ops=3) and
       MatchInstruction(hp1,[A_ADD,A_SUB],[taicpu(p).condition], [PF_None,PF_S]) and
       (getsubreg(taicpu(hp1).oper[2]^.reg)=R_SUBQ) and
       (getsupreg(taicpu(p).oper[0]^.reg)=getsupreg(taicpu(hp1).oper[2]^.reg)) and
       RegEndOfLife(taicpu(hp1).oper[2]^.reg,taicpu(hp1)) then
       begin
         DebugMsg(SPeepholeOptimization + 'MovOp2AddUtxw 1 done', p);
         shifterop_reset(so);
         so.shiftmode:=SM_UXTW;
         taicpu(hp1).ops:=4;
         taicpu(hp1).loadreg(2,taicpu(p).oper[1]^.reg);
         taicpu(hp1).loadshifterop(3,so);
         RemoveCurrentP(p);
         Result:=true;
         exit;
       end
     {
       optimize
       mov rX, yyyy
       ....
     }
     else if GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) then
       begin
         if RemoveSuperfluousMove(p, hp1, 'MovMov2Mov') then
           Result:=true
         else if (taicpu(p).ops = 2) and
           (tai(hp1).typ = ait_instruction) and
           RedundantMovProcess(p,hp1) then
           Result:=true
       end;
    end;


  function TCpuAsmOptimizer.OptPass1MOVZ(var p: tai): boolean;
    var
      hp1: tai;
      TargetReg: TRegister;
    begin
      Result := False;
      hp1 := nil;

      TargetReg := taicpu(p).oper[0]^.reg;
      if (taicpu(p).oppostfix = PF_None) and (taicpu(p).condition = C_None) then
        begin
          if
            { Check next instruction first so hp1 gets set to something, then
              if it remains nil, we know for sure that there's no valid next
              instruction. }
            not GetNextInstruction(p, hp1) or
            { MOVZ and MOVK/MOVN instructions undergo macro-fusion. }
            not MatchInstruction(hp1, [A_MOVK, A_MOVN], [C_None], [PF_None]) or
            (taicpu(hp1).oper[0]^.reg <> TargetReg) then
            begin
              if (taicpu(p).oper[1]^.val = 0) then
                begin
                  { Change;
                      movz reg,#0
                      (no movk or movn)
                    To:
                      mov  reg,xzr (or wzr)

                    Easier to perform other optimisations with registers
                  }
                  DebugMsg(SPeepholeOptimization + 'Movz0ToMovZeroReg', p);

                  { Convert TargetReg to the correctly-sized zero register }
                  setsupreg(TargetReg, RS_XZR);

                  taicpu(p).opcode := A_MOV;
                  taicpu(p).loadreg(1, TargetReg);
                  Result := True;
                  Exit;
                end;
            end;
          {
             remove the second Movz from

             movz reg,...
             movz reg,...
          }
          if GetNextInstructionUsingReg(p,hp1,taicpu(p).oper[0]^.reg) and
            MatchInstruction(hp1,A_MOVZ,[C_None],[PF_none]) and
            MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[0]^) then
            begin
              DebugMsg(SPeepholeOptimization + 'MovzMovz2Movz', p);
              RemoveCurrentP(p);
              Result:=true;
              exit;
            end;
        end;

      if (getsupreg(TargetReg) <= RS_X30) and { Mostly to play safe }
        GetNextInstructionUsingReg(p, hp1, TargetReg) and
        (hp1.typ = ait_instruction) then
        begin
          case taicpu(hp1).opcode of
{$ifdef AARCH64}
            A_MOVK:
              { Try to avoid too much unnecessary processing by checking to see
                if the register is 32-bit }
              if (getsubreg(TargetReg) = R_SUBD) and
                (taicpu(hp1).oper[0]^.reg = TargetReg) and
                TryConstMerge(p, hp1) then
                begin
                  Result := True;
                  Exit;
                end;
{$endif AARCH64}
            A_STR:
              {
                With sequences such as:
                  movz  w0,x
                  strb  w0,[sp, #ofs]
                  movz  w0,y
                  strb  w0,[sp, #ofs+1]

                Merge the constants to:
                  movz  w0,x + (y shl 8)
                  strw  w0,[sp, #ofs]

                Only use the stack pointer or frame pointer and an even offset though
                to guarantee alignment
              }
              if TryConstMerge(p, hp1) then
                begin
                  Result := True;
                  Exit;
                end;
            else
              ;
          end;
        end;
    end;


  function TCpuAsmOptimizer.OptPass1FMov(var p: tai): Boolean;
    var
      hp1: tai;
      alloc, dealloc: tai_regalloc;
    begin
      {
        change
        fmov reg0,reg1
        fmov reg1,reg0
        into
        fmov reg0,reg1
      }
      Result := False;
      while GetNextInstruction(p, hp1) and
        MatchInstruction(hp1, A_FMOV, [taicpu(p).condition], [taicpu(p).oppostfix]) and
        MatchOperand(taicpu(p).oper[0]^, taicpu(hp1).oper[1]^) and
        MatchOperand(taicpu(p).oper[1]^, taicpu(hp1).oper[0]^) do
        begin
          asml.Remove(hp1);
          hp1.free;
          DebugMsg(SPeepholeOptimization + 'FMovFMov2FMov 1 done', p);
          Result:=true;
        end;

      { change
          fmov reg0,const
          fmov reg1,reg0
          dealloc reg0
          into
          fmov reg1,const
      }
      if MatchOpType(taicpu(p),top_reg,top_realconst) and
        GetNextInstructionUsingReg(p,hp1,taicpu(p).oper[0]^.reg) and
        (not RegModifiedBetween(taicpu(p).oper[1]^.reg, p, hp1)) and
        MatchInstruction(hp1,A_FMOV,[taicpu(p).condition],[taicpu(p).oppostfix]) and
        MatchOpType(taicpu(hp1),top_reg,top_reg) and
        MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[1]^.reg) and
        (not RegModifiedByInstruction(taicpu(p).oper[0]^.reg, hp1)) and
        assigned(FindRegDeAlloc(taicpu(p).oper[0]^.reg,tai(hp1.Next)))
        then
        begin
          DebugMsg('Peephole FMovFMov2FMov 2 done', p);

          taicpu(hp1).loadrealconst(1,taicpu(p).oper[1]^.val_real);

          alloc:=FindRegAllocBackward(taicpu(p).oper[0]^.reg,tai(p.Previous));
          dealloc:=FindRegDeAlloc(taicpu(p).oper[0]^.reg,tai(hp1.Next));

          if assigned(alloc) and assigned(dealloc) then
            begin
              asml.Remove(alloc);
              alloc.Free;
              asml.Remove(dealloc);
              dealloc.Free;
            end;

          { p will be removed, update used register as we continue
            with the next instruction after p }

          result:=RemoveCurrentP(p);
        end;

      { not enabled as apparently not happening
      if MatchOpType(taicpu(p),top_reg,top_reg) and
        GetNextInstructionUsingReg(p,hp1,taicpu(p).oper[0]^.reg) and
        MatchInstruction(hp1, [A_FSUB,A_FADD,A_FNEG,A_FMUL,A_FSQRT,A_FDIV,A_FABS], [PF_None]) and
        (MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[1]^) or
         ((taicpu(hp1).ops=3) and MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[2]^))
        ) and
        RegEndofLife(taicpu(p).oper[0]^.reg,taicpu(hp1)) and
        not(RegUsedBetween(taicpu(p).oper[0]^.reg,p,hp1)) then
        begin
          DebugMsg(SPeepholeOptimization + 'FMovFOp2FOp done', hp1);
          AllocRegBetween(taicpu(hp1).oper[1]^.reg,p,hp1,UsedRegs);
          if MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[1]^) then
            taicpu(hp1).oper[1]^.reg:=taicpu(p).oper[1]^.reg;
          if (taicpu(hp1).ops=3) and MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[2]^) then
            taicpu(hp1).oper[2]^.reg:=taicpu(p).oper[1]^.reg;
          RemoveCurrentP(p);
          Result:=true;
          exit;
        end;
      }
    end;


  function TCpuAsmOptimizer.OptPass1SXTW(var p : tai) : Boolean;
    var
      hp1: tai;
      GetNextInstructionUsingReg_hp1: Boolean;
    begin
      Result:=false;
      if GetNextInstructionUsingReg(p,hp1,taicpu(p).oper[0]^.reg) then
        begin
          {
            change
            sxtw reg2,reg1
            str reg2,[...]
            dealloc reg2
            to
            str reg1,[...]
          }
          if MatchInstruction(p, taicpu(p).opcode, [C_None], [PF_None]) and
            (taicpu(p).ops=2) and
            MatchInstruction(hp1, A_STR, [C_None], [PF_None]) and
            (getsubreg(taicpu(hp1).oper[0]^.reg)=R_SUBD) and
            RegEndofLife(taicpu(p).oper[0]^.reg,taicpu(hp1)) and
            { the reference in strb might not use reg2 }
            not(RegInRef(taicpu(p).oper[0]^.reg,taicpu(hp1).oper[1]^.ref^)) and
            { reg1 might not be modified inbetween }
            not(RegModifiedBetween(taicpu(p).oper[1]^.reg,p,hp1)) then
            begin
              DebugMsg('Peephole SXTHStr2Str done', p);
              taicpu(hp1).loadReg(0,taicpu(p).oper[1]^.reg);
              result:=RemoveCurrentP(p);
            end
          {
            change
            sxtw reg2,reg1
            sxtw reg3,reg2
            dealloc reg2
            to
            sxtw reg3,reg1
          }
          else if MatchInstruction(p, A_SXTW, [C_None], [PF_None]) and
            (taicpu(p).ops=2) and
            MatchInstruction(hp1, A_SXTW, [C_None], [PF_None]) and
            (taicpu(hp1).ops=2) and
            MatchOperand(taicpu(hp1).oper[1]^, taicpu(p).oper[0]^.reg) and
            RegEndofLife(taicpu(p).oper[0]^.reg,taicpu(hp1)) and
            { reg1 might not be modified inbetween }
            not(RegModifiedBetween(taicpu(p).oper[1]^.reg,p,hp1)) then
            begin
              DebugMsg('Peephole SxtwSxtw2Sxtw done', p);
              AllocRegBetween(taicpu(p).oper[1]^.reg,p,hp1,UsedRegs);
              taicpu(hp1).opcode:=A_SXTW;
              taicpu(hp1).loadReg(1,taicpu(p).oper[1]^.reg);
              result:=RemoveCurrentP(p);
            end
          else if USxtOp2Op(p,hp1,SM_SXTW) then
            Result:=true
          else if RemoveSuperfluousMove(p, hp1, 'SxtwMov2Data') then
            Result:=true;
        end;
    end;


  function TCpuAsmOptimizer.OptPass1B(var p: tai): boolean;
    var
      hp1, hp2, hp3, hp4, hp5: tai;
      Invert: Boolean;
    begin
      Result := False;
      {
          convert
          b<c>  .L1
          movz  reg,#1`
          b     .L2
        .L1
          movz  reg,#0 (or mov reg,xzr)
        .L2

        into
          cset  reg,<not(c)>

        Also do the same if the constants are reversed, instead converting it to:
          cset  reg,<c>
      }

      if (taicpu(p).condition <> C_None) and
        (taicpu(p).oper[0]^.typ = top_ref) and
        GetNextInstruction(p, hp1) and
        { Check individually instead of using MatchInstruction in order to save time }
        (hp1.typ = ait_instruction) and
        (taicpu(hp1).condition = C_None) and
        (taicpu(hp1).oppostfix = PF_None) and
        (taicpu(hp1).ops = 2) and
        (
          (
            (taicpu(hp1).opcode = A_MOVZ) and
            (taicpu(hp1).oper[1]^.val in [0, 1])
          ) or
          (
            (taicpu(hp1).opcode = A_MOV) and
            (getsupreg(taicpu(hp1).oper[1]^.reg) = RS_XZR)
          )
        ) and
        GetNextInstruction(hp1, hp2) and
        MatchInstruction(hp2, A_B, [PF_None]) and
        (taicpu(hp2).condition = C_None) and
        (taicpu(hp2).oper[0]^.typ = top_ref) and
        GetNextInstruction(hp2, hp3) and
        (hp3.typ = ait_label) and
        (tasmlabel(taicpu(p).oper[0]^.ref^.symbol) = tai_label(hp3).labsym) and
        GetNextInstruction(hp3, hp4) and
        { As before, check individually instead of using MatchInstruction in order to save time }
        (hp4.typ = ait_instruction) and
        (taicpu(hp4).condition = C_None) and
        (taicpu(hp4).oppostfix = PF_None) and
        (taicpu(hp4).ops = 2) and
        (taicpu(hp4).oper[0]^.reg = taicpu(hp1).oper[0]^.reg) and
        (
          (
            (taicpu(hp4).opcode = A_MOVZ) and
            (
              (
                { Check to confirm the following:
                  - First mov is either "movz reg,#0" or "mov reg,xzr"
                  - Second mov is "movz reg,#1"
                }
                (
                  (taicpu(hp1).oper[1]^.typ = top_reg) { Will be the zero register } or
                  (taicpu(hp1).oper[1]^.val = 0)
                ) and
                (taicpu(hp4).oper[1]^.val = 1)
              ) or
              (
                { Check to confirm the following:
                  - First mov is "movz reg,#1"
                  - Second mov is "movz reg,#0"
                }
                MatchOperand(taicpu(hp1).oper[1]^, 1) and
                (taicpu(hp4).oper[1]^.val = 0)
              )
            )
          ) or
          (
            { Check to confirm the following:
              - First mov is "movz reg,#1"
              - Second mov is "mov reg,xzr"
            }
            (taicpu(hp4).opcode = A_MOV) and
            (getsupreg(taicpu(hp4).oper[1]^.reg) = RS_XZR) and
            MatchOperand(taicpu(hp1).oper[1]^, 1)
          )
        ) and
        GetNextInstruction(hp4, hp5) and
        (hp5.typ = ait_label) and
        (tasmlabel(taicpu(hp2).oper[0]^.ref^.symbol) = tai_label(hp5).labsym) then
        begin
          Invert := MatchOperand(taicpu(hp1).oper[1]^, 1); { if true, hp4 will be mov reg,0 in some form }
          if Invert then
            taicpu(p).condition := inverse_cond(taicpu(p).condition);

          tai_label(hp3).labsym.DecRefs;

          { If this isn't the only reference to the middle label, we can
            still make a saving - only that the first jump and everything
            that follows will remain. }
          if (tai_label(hp3).labsym.getrefs = 0) then
            begin
              if Invert then
                DebugMsg(SPeepholeOptimization + 'B(c)Movz1BMovz0 -> Cset(~c)',p)
              else
                DebugMsg(SPeepholeOptimization + 'B(c)Movz0bMovZ1 -> Cset(c)',p);

              { remove jump, first label and second MOV (also catching any aligns) }
              repeat
                if not GetNextInstruction(hp2, hp3) then
                  InternalError(2022070801);

                RemoveInstruction(hp2);

                hp2 := hp3;
              until hp2 = hp5;

              { Don't decrement reference count before the removal loop
                above, otherwise GetNextInstruction won't stop on the
                the label }
              tai_label(hp5).labsym.DecRefs;
            end
          else
            begin
              if Invert then
                DebugMsg(SPeepholeOptimization + 'B(c)Movz1BMovz0 -> Cset(~c) (partial)',p)
              else
                DebugMsg(SPeepholeOptimization + 'B(c)Movz0BMovz1 -> Cset(c) (partial)',p);
            end;

          taicpu(hp1).opcode := A_CSET;
          taicpu(hp1).loadconditioncode(1, taicpu(p).condition);

          RemoveCurrentP(p, hp1);

          Result:=true;
          exit;
        end;
    end;


  function TCpuAsmOptimizer.OptPass2B(var p: tai): Boolean;
    var
      hp1: tai;
      LabelSym: TAsmLabel;
      CSELTracking: PCSELTracking;
    begin
      Result := False;
      if (taicpu(p).condition = C_None) and
        IsJumpToLabel(taicpu(p)) then
        begin
          { Check for:
                B   @lbl
                ...
              @Lbl:
                RET

            Change to:
                RET (and reduce reference count on label)
          }

          LabelSym := TAsmLabel(JumpTargetOp(taicpu(p))^.ref^.symbol);
          hp1 := GetLabelWithSym(LabelSym);
          if Assigned(hp1) and
            GetNextInstruction(hp1, hp1) and
            (hp1.typ = ait_instruction) and
            (taicpu(hp1).opcode = A_RET) then
            begin
              DebugMsg(SPeepholeOptimization + 'B -> RET since a RET immediately follows the destination label (B2Ret)', p);
              taicpu(p).ops := 0;
              taicpu(p).clearop(0);
              taicpu(p).is_jmp := false;
              taicpu(p).opcode := A_RET;

              { Make sure the label is dereferenced now }
              LabelSym.decrefs;

              Result := True;
              Exit;
            end;
        end;


      if (taicpu(p).condition <> C_None) and
        IsJumpToLabel(taicpu(p)) and
        GetNextInstruction(p, hp1) and
        (hp1.typ = ait_instruction) and
        (taicpu(hp1).opcode = A_MOV) then
        begin
          { check for
                jCC   xxx
                <several movs>
             xxx:

           Also spot:
                Jcc   xxx
                <several movs>
                jmp   xxx

           Change to:
                <several csets with inverted condition>
                jmp   xxx  (only for the 2nd case)
          }
          CSELTracking := New(PCSELTracking, Init(Self, p, hp1, TAsmLabel(JumpTargetOp(taicpu(p))^.ref^.symbol)));

          if CSELTracking^.State <> tsInvalid then
            begin
              CSELTracking^.Process(p);
              Result := True;
            end;

          CSELTracking^.Done;
        end;
    end;


  function TCpuAsmOptimizer.OptPass2CSEL(var p: tai): Boolean;
    begin
      Result := False;

      { Csel r0,r1,r1,cond -> mov r0,r1 }
      if (taicpu(p).oper[1]^.reg = taicpu(p).oper[2]^.reg) then
        begin
          DebugMsg(SPeepholeOptimization + 'CSel2Mov (identical true/false registers)', p);
          taicpu(p).opcode := A_MOV;
          taicpu(p).ops := 2;
          Result := True;
          Exit;
        end;
    end;


  function TCpuAsmOptimizer.OptPass2LDRSTR(var p: tai): boolean;
    var
      hp1, hp1_last: tai;
      ThisRegister: TRegister;
      OffsetVal, ValidOffset, MinOffset, MaxOffset: asizeint;
      TargetOpcode: TAsmOp;
    begin
      Result := False;
      ThisRegister := taicpu(p).oper[0]^.reg;

      case taicpu(p).opcode of
        A_LDR:
          TargetOpcode := A_LDP;
        A_STR:
          TargetOpcode := A_STP;
        else
          InternalError(2020081501);
      end;

      { reg appearing in ref invalidates these optimisations }
      if (TargetOpcode = A_STP) or not RegInRef(ThisRegister, taicpu(p).oper[1]^.ref^) then
        begin
          { LDP/STP has a smaller permitted offset range than LDR/STR.

            TODO: For a group of out-of-range LDR/STR instructions, can
            we declare a temporary register equal to the offset base
            address, modify the STR instructions to use that register
            and then convert them to STP instructions?  Note that STR
            generally takes 2 cycles (on top of the memory latency),
            while LDP/STP takes 3.
          }

          if (getsubreg(ThisRegister) = R_SUBQ) then
            begin
              ValidOffset := 8;
              MinOffset := -512;
              MaxOffset := 504;
            end
          else
            begin
              ValidOffset := 4;
              MinOffset := -256;
              MaxOffset := 252;
            end;

          hp1_last := p;

          { Look for nearby LDR/STR instructions }
          if (taicpu(p).oppostfix = PF_NONE) and
            (taicpu(p).oper[1]^.ref^.addressmode = AM_OFFSET) then
            { If SkipGetNext is True, GextNextInstruction isn't called }
            while GetNextInstruction(hp1_last, hp1) do
              begin
                if (hp1.typ <> ait_instruction) then
                  Break;

                if (taicpu(hp1).opcode = taicpu(p).opcode) then
                  begin
                    if (taicpu(hp1).oppostfix = PF_NONE) and
                      { Registers need to be the same size }
                      (getsubreg(ThisRegister) = getsubreg(taicpu(hp1).oper[0]^.reg)) and
                      (
                        (TargetOpcode = A_STP) or
                        { LDP x0, x0, [sp, #imm] is undefined behaviour, even
                          though such an LDR pair should have been optimised
                          out by now. STP is okay }
                        (ThisRegister <> taicpu(hp1).oper[0]^.reg)
                      ) and
                      (taicpu(hp1).oper[1]^.ref^.addressmode = AM_OFFSET) and
                      (taicpu(p).oper[1]^.ref^.base = taicpu(hp1).oper[1]^.ref^.base) and
                      (taicpu(p).oper[1]^.ref^.index = taicpu(hp1).oper[1]^.ref^.index) and
                      { Make sure the address registers haven't changed }
                      not RegModifiedBetween(taicpu(hp1).oper[1]^.ref^.base, p, hp1) and
                      (
                        (taicpu(hp1).oper[1]^.ref^.index = NR_NO) or
                        not RegModifiedBetween(taicpu(hp1).oper[1]^.ref^.index, p, hp1)
                      ) and
                      { Don't need to check "RegInRef" because the base registers are identical,
                        and the first one was checked already. [Kit] }
                      (((TargetOpcode=A_LDP) and not RegUsedBetween(taicpu(hp1).oper[0]^.reg, p, hp1)) or
                       ((TargetOpcode=A_STP) and not RegModifiedBetween(taicpu(hp1).oper[0]^.reg, p, hp1))) then
                      begin
                        { Can we convert these two LDR/STR instructions into a
                          single LDR/STP? }

                        OffsetVal := taicpu(hp1).oper[1]^.ref^.offset - taicpu(p).oper[1]^.ref^.offset;
                        if (OffsetVal = ValidOffset) then
                          begin
                            if  (taicpu(p).oper[1]^.ref^.offset >= MinOffset) and (taicpu(hp1).oper[1]^.ref^.offset <= MaxOffset) then
                              begin
                                { Convert:
                                    LDR/STR reg0, [reg2, #ofs]
                                    ...
                                    LDR/STR reg1. [reg2, #ofs + 8] // 4 if registers are 32-bit
                                  To:
                                    LDP/STP reg0, reg1, [reg2, #ofs]
                                }
                                taicpu(p).opcode := TargetOpcode;
                                if TargetOpcode = A_STP then
                                  DebugMsg(SPeepholeOptimization + 'StrStr2Stp', p)
                                else
                                  DebugMsg(SPeepholeOptimization + 'LdrLdr2Ldp', p);
                                taicpu(p).ops := 3;
                                taicpu(p).loadref(2, taicpu(p).oper[1]^.ref^);
                                taicpu(p).loadreg(1, taicpu(hp1).oper[0]^.reg);

                                asml.Remove(hp1);
                                hp1.Free;
                                Result := True;
                                Exit;
                              end;
                          end
                        else if (OffsetVal = -ValidOffset) then
                          begin
                            if (taicpu(hp1).oper[1]^.ref^.offset >= MinOffset) and (taicpu(p).oper[1]^.ref^.offset <= MaxOffset) then
                              begin
                                { Convert:
                                    LDR/STR reg0, [reg2, #ofs + 8] // 4 if registers are 32-bit
                                    ...
                                    LDR/STR reg1. [reg2, #ofs]
                                  To:
                                    LDP/STP reg1, reg0, [reg2, #ofs]
                                }
                                taicpu(p).opcode := TargetOpcode;
                                if TargetOpcode = A_STP then
                                  DebugMsg(SPeepholeOptimization + 'StrStr2Stp (reverse)', p)
                                else
                                  DebugMsg(SPeepholeOptimization + 'LdrLdr2Ldp (reverse)', p);
                                taicpu(p).ops := 3;
                                taicpu(p).loadref(2, taicpu(hp1).oper[1]^.ref^);
                                taicpu(p).loadreg(1, taicpu(p).oper[0]^.reg);
                                taicpu(p).loadreg(0, taicpu(hp1).oper[0]^.reg);

                                asml.Remove(hp1);
                                hp1.Free;
                                Result := True;
                                Exit;
                              end;
                          end;
                      end;
                  end
                else
                  Break;

                { Don't continue looking for LDR/STR pairs if the address register
                  gets modified }
                if RegModifiedByInstruction(taicpu(p).oper[1]^.ref^.base, hp1) then
                  Break;

                hp1_last := hp1;
              end;
        end;
    end;


  function TCpuAsmOptimizer.OptPass2MOV(var p: tai): Boolean;
    var
      hp1: tai;
      X: Integer;
    begin
      Result := False;

      { Merge MOV and CSEL instructions left behind by OptPass2B - that is,
        change:

          mov  r0,r1
          csel r0,r2,r0,cond

        To:
          csel r0,r2,r1,cond

        (Also if r0 is the second operand)
      }
      if (taicpu(p).oper[1]^.typ = top_reg) and
        GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) and
        (hp1.typ = ait_instruction) and
        (taicpu(hp1).opcode = A_CSEL) and
        (taicpu(hp1).oper[0]^.reg = taicpu(p).oper[0]^.reg) and
        not RegModifiedBetween(taicpu(p).oper[1]^.reg, p, hp1) then
        begin
          { Use "Result" to note if a change was made so we only have to do
            expensive register allocation once }
          for X := 1 to 2 do
            if (taicpu(hp1).oper[X]^.reg = taicpu(p).oper[0]^.reg) then
              begin
                taicpu(hp1).oper[X]^.reg := taicpu(p).oper[1]^.reg;
                Result := True;
              end;

          if Result then
            begin
              DebugMSg(SPeepholeOptimization + 'MovCSel2CSel', p);
              { Don't need to allocate the zero register - so save time by
                skipping it in this case }
              if getsupreg(taicpu(p).oper[1]^.reg) <> RS_XZR then
                AllocRegBetween(taicpu(p).oper[1]^.reg, p, hp1, UsedRegs);
              RemoveCurrentP(p);
              Exit;
            end;
        end;
    end;


  function TCpuAsmOptimizer.PostPeepholeOptAND(var p: tai): Boolean;
    var
      hp1, hp2: tai;
      hp3: taicpu;
      bitval : cardinal;
    begin
      Result:=false;
      {
        and reg1,reg0,<const=power of 2>
        cmp reg1,#0
        <reg1 end of life>
        b.e/b.ne label

        into

        tb(n)z reg0,<power of 2>,label
      }
      if MatchOpType(taicpu(p),top_reg,top_reg,top_const) and
        (PopCnt(QWord(taicpu(p).oper[2]^.val))=1) and
        GetNextInstruction(p,hp1) and
        MatchInstruction(hp1,A_CMP,[PF_None]) and
        MatchOpType(taicpu(hp1),top_reg,top_const) and
        (taicpu(hp1).oper[1]^.val=0) and
        MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[0]^) and
        RegEndOfLife(taicpu(p).oper[0]^.reg, taicpu(hp1)) and
        GetNextInstruction(hp1,hp2) and
        MatchInstruction(hp2,A_B,[PF_None]) and
        (taicpu(hp2).condition in [C_EQ,C_NE]) then
        begin
           bitval:=BsfQWord(qword(taicpu(p).oper[2]^.val));
           case taicpu(hp2).condition of
            C_NE:
              hp3:=taicpu.op_reg_const_ref(A_TBNZ,taicpu(p).oper[1]^.reg,bitval,taicpu(hp2).oper[0]^.ref^);
            C_EQ:
              hp3:=taicpu.op_reg_const_ref(A_TBZ,taicpu(p).oper[1]^.reg,bitval,taicpu(hp2).oper[0]^.ref^);
            else
              Internalerror(2021100201);
          end;
          taicpu(hp3).fileinfo:=taicpu(hp1).fileinfo;
          asml.insertbefore(hp3, hp1);

          RemoveInstruction(hp1);
          RemoveInstruction(hp2);
          RemoveCurrentP(p);
          DebugMsg(SPeepholeOptimization + 'AndCmpB.E/NE2Tbnz/Tbz done', p);
          Result:=true;
        end;
    end;


  function TCpuAsmOptimizer.PostPeepholeOptCMP(var p : tai): boolean;
    var
     hp1,hp2: tai;
    begin
      Result:=false;
      {
         cmp reg0,#0
         b.e/b.ne label

         into

         cb(n)z reg0,label
      }
      if MatchOpType(taicpu(p),top_reg,top_const) and
        (taicpu(p).oper[0]^.reg<>NR_SP) and
        (taicpu(p).oper[1]^.val=0) and
        GetNextInstruction(p,hp1) and
        MatchInstruction(hp1,A_B,[PF_None]) and
        (taicpu(hp1).condition in [C_EQ,C_NE]) then
        begin
          case taicpu(hp1).condition of
            C_NE:
              hp2:=taicpu.op_reg_sym_ofs(A_CBNZ,taicpu(p).oper[0]^.reg,taicpu(hp1).oper[0]^.ref^.symbol,taicpu(hp1).oper[0]^.ref^.offset);
            C_EQ:
              hp2:=taicpu.op_reg_sym_ofs(A_CBZ,taicpu(p).oper[0]^.reg,taicpu(hp1).oper[0]^.ref^.symbol,taicpu(hp1).oper[0]^.ref^.offset);
            else
              Internalerror(2019090801);
          end;
          taicpu(hp2).fileinfo:=taicpu(hp1).fileinfo;
          asml.insertbefore(hp2, hp1);

          asml.remove(p);
          asml.remove(hp1);
          p.free;
          hp1.free;
          p:=hp2;
          DebugMsg(SPeepholeOptimization + 'CMPB.E/NE2CBNZ/CBZ done', p);
          Result:=true;
        end;
    end;


  function TCpuAsmOptimizer.PostPeepholeOptTST(var p : tai): boolean;
    var
      hp1: tai;
      hp3: taicpu;
      bitval : cardinal;
    begin
      Result:=false;
      {
        tst reg1,<const=power of 2>
        b.e/b.ne label

        into

        tb(n)z reg0,<power of 2>,label
      }
      if MatchOpType(taicpu(p),top_reg,top_const) and
        (PopCnt(QWord(taicpu(p).oper[1]^.val))=1) and
        GetNextInstruction(p,hp1) and
        MatchInstruction(hp1,A_B,[C_EQ,C_NE],[PF_None]) then
        begin
           bitval:=BsfQWord(qword(taicpu(p).oper[1]^.val));
           case taicpu(hp1).condition of
            C_NE:
              hp3:=taicpu.op_reg_const_ref(A_TBNZ,taicpu(p).oper[0]^.reg,bitval,taicpu(hp1).oper[0]^.ref^);
            C_EQ:
              hp3:=taicpu.op_reg_const_ref(A_TBZ,taicpu(p).oper[0]^.reg,bitval,taicpu(hp1).oper[0]^.ref^);
            else
              Internalerror(2021100210);
          end;
          taicpu(hp3).fileinfo:=taicpu(p).fileinfo;
          asml.insertafter(hp3, p);

          RemoveInstruction(hp1);
          RemoveCurrentP(p, hp3);
          DebugMsg(SPeepholeOptimization + 'TST; B(E/NE) -> TB(Z/NZ) done', p);
          Result:=true;
        end;
    end;


  function TCpuAsmOptimizer.PrePeepHoleOptsCpu(var p: tai): boolean;
    begin
      result := false;
      if p.typ=ait_instruction then
        begin
          case taicpu(p).opcode of
            A_SBFX,
            A_UBFX:
              Result:=OptPreSBFXUBFX(p);
            else
              ;
          end;
        end;
    end;


  function TCpuAsmOptimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;
    begin
      result := false;
      if p.typ=ait_instruction then
        begin
          case taicpu(p).opcode of
            A_B:
              Result:=OptPass1B(p);
            A_LDR:
              Result:=OptPass1LDR(p);
            A_STR:
              Result:=OptPass1STR(p);
            A_MOV:
              Result:=OptPass1Mov(p);
            A_MOVZ:
              Result:=OptPass1MOVZ(p);
            A_STP:
              Result:=OptPass1STP(p);
            A_LSR,
            A_ROR,
            A_ASR,
            A_LSL:
              Result:=OptPass1Shift(p);
            A_AND:
              Result:=OptPass1And(p);
            A_NEG,
            A_CSEL,
            A_ADD,
            A_ADC,
            A_SUB,
            A_SBC,
            A_BIC,
            A_EOR,
            A_ORR,
            A_MUL:
              Result:=OptPass1Data(p);
            A_UXTB:
              Result:=OptPass1UXTB(p);
            A_UXTH:
              Result:=OptPass1UXTH(p);
            A_SXTB:
              Result:=OptPass1SXTB(p);
            A_SXTH:
              Result:=OptPass1SXTH(p);
            A_SXTW:
              Result:=OptPass1SXTW(p);
//            A_VLDR,
            A_FRINTA,
            A_FRINTI,
            A_FRINTM,
            A_FRINTN,
            A_FRINTP,
            A_FRINTX,
            A_FRINTZ,
            A_FCSEL,
            A_FMADD,
            A_FMSUB,
            A_FNMADD,
            A_FNMSUB,
            A_FNMUL,
            A_FADD,
            A_FMUL,
            A_FDIV,
            A_FSUB,
            A_FSQRT,
            A_FNEG,
            A_FCVT,
            A_FABS:
              Result:=OptPass1FData(p);
            A_FMOV:
              Result:=OptPass1FMov(p);
            else
              ;
          end;
        end;
    end;


  function TCpuAsmOptimizer.PeepHoleOptPass2Cpu(var p: tai): boolean;
    begin
      result := false;
      if p.typ=ait_instruction then
        begin
          case taicpu(p).opcode of
            A_AND,
            A_BIC:
              Result := OptPass2Bitwise(p);
            A_B:
              Result := OptPass2B(p);
            A_CSEL:
              Result := OptPass2CSEL(p);
            A_MOV:
              Result := OptPass2MOV(p);
            A_LDR,
            A_STR:
              Result := OptPass2LDRSTR(p);
            A_TST:
              Result := OptPass2TST(p);
            else
              ;
          end;
        end;
    end;


  function TCpuAsmOptimizer.PostPeepHoleOptsCpu(var p: tai): boolean;
    begin
      result := false;
      if p.typ=ait_instruction then
        begin
          case taicpu(p).opcode of
            A_CMP:
              Result:=PostPeepholeOptCMP(p);
            A_AND:
              Result:=PostPeepholeOptAND(p);
            A_TST:
              Result:=PostPeepholeOptTST(p);
            else
              ;
          end;
        end;
    end;


  class procedure TCpuAsmOptimizer.UpdateIntRegsNoDealloc(var AUsedRegs: TAllUsedRegs; p: Tai);
    begin
      { Update integer registers, ignoring deallocations }
      repeat
        while assigned(p) and
              ((p.typ in (SkipInstr - [ait_RegAlloc])) or
               (p.typ = ait_label) or
               ((p.typ = ait_marker) and
                (tai_Marker(p).Kind in [mark_AsmBlockEnd,mark_NoLineInfoStart,mark_NoLineInfoEnd]))) do
             p := tai(p.next);
        while assigned(p) and
              (p.typ=ait_RegAlloc) Do
          begin
            if (getregtype(tai_regalloc(p).reg) = R_INTREGISTER) then
              begin
                case tai_regalloc(p).ratype of
                  ra_alloc :
                    IncludeRegInUsedRegs(tai_regalloc(p).reg, AUsedRegs);
                  else
                    ;
                end;
              end;
            p := tai(p.next);
          end;
      until not(assigned(p)) or
            (not(p.typ in SkipInstr) and
             not((p.typ = ait_label) and
                 labelCanBeSkipped(tai_label(p))));
    end;

  { Attempts to allocate a volatile integer register for use between p and hp,
    using AUsedRegs for the current register usage information.  Returns NR_NO
    if no free register could be found }
  function TCpuAsmOptimizer.GetIntRegisterBetween(RegSize: TSubRegister; var AUsedRegs: TAllUsedRegs; p, hp: tai; DontAlloc: Boolean = False): TRegister;
    var
      RegSet: TCPURegisterSet;
      CurrentSuperReg: Integer;
      CurrentReg: TRegister;
      Currentp: tai;
      Breakout: Boolean;
    begin
      Result := NR_NO;
      RegSet :=
        paramanager.get_volatile_registers_int(current_procinfo.procdef.proccalloption) +
        current_procinfo.saved_regs_int;
(*
      { Don't use the frame register unless explicitly allowed (fixes i40111) }
      if ([cs_useebp, cs_userbp] * current_settings.optimizerswitches) = [] then
        Exclude(RegSet, RS_FRAME_POINTER_REG);
*)
      for CurrentSuperReg in RegSet do
        begin
          CurrentReg := newreg(R_INTREGISTER, TSuperRegister(CurrentSuperReg), RegSize);
          if not AUsedRegs[R_INTREGISTER].IsUsed(CurrentReg)
            then
            begin
              Currentp := p;
              Breakout := False;
              while not Breakout and GetNextInstruction(Currentp, Currentp) and (Currentp <> hp) do
                begin
                  case Currentp.typ of
                    ait_instruction:
                      begin
                        if RegInInstruction(CurrentReg, Currentp) then
                          begin
                            Breakout := True;
                            Break;

                          end;
                        { Cannot allocate across an unconditional jump }
                        if is_calljmpmaybeuncondret(taicpu(Currentp).opcode) and (taicpu(Currentp).condition = C_None) then
                          Exit;
                      end;
                    ait_marker:
                      { Don't try anything more if a marker is hit }
                      Exit;
                    ait_regalloc:
                      if (tai_regalloc(Currentp).ratype <> ra_dealloc) and SuperRegistersEqual(CurrentReg, tai_regalloc(Currentp).reg) then
                        begin
                          Breakout := True;
                          Break;
                        end;
                    else
                      ;
                  end;
                end;

              if Breakout then
                { Try the next register }
                Continue;

              { We have a free register available }
              Result := CurrentReg;
              if not DontAlloc then
                AllocRegBetween(CurrentReg, p, hp, AUsedRegs);
              Exit;
            end;
        end;
    end;


  function TCSELTracking.InitialiseBlock(BlockStart, OneBeforeBlock: tai; out BlockStop: tai; out EndJump: tai): Boolean;
    begin
      Result := False;
      EndJump := nil;
      BlockStop := nil;

      while (BlockStart <> fOptimizer.BlockEnd) and
        { stop on labels }
        (BlockStart.typ <> ait_label) do
        begin
          { Keep track of all integer registers that are used }
          fOptimizer.UpdateIntRegsNoDealloc(RegisterTracking, tai(OneBeforeBlock.Next));
          if BlockStart.typ = ait_instruction then
            begin
              if MatchInstruction(BlockStart, A_B, [C_None], []) then
                begin
                  if not IsJumpToLabel(taicpu(BlockStart)) or
                    (JumpTargetOp(taicpu(BlockStart))^.ref^.index <> NR_NO) then
                    Exit;

                  EndJump := BlockStart;
                  Break;
                end
              { Check to see if we have a valid MOV instruction instead }
              else if (taicpu(BlockStart).opcode <> A_MOV) or
                { Can't include the stack pointer in CSEL }
                fOptimizer.RegInInstruction(NR_SP, BlockStart) then
                begin
                  Exit;
                end
              else
                { This will be a valid MOV }
                fAllocationRange := BlockStart;
            end;

          OneBeforeBlock := BlockStart;
          fOptimizer.GetNextInstruction(BlockStart, BlockStart);
        end;

      if (BlockStart = fOptimizer.BlockEnd) then
        Exit;

      BlockStop := BlockStart;
      Result := True;
    end;


  function TCSELTracking.AnalyseMOVBlock(BlockStart, BlockStop, SearchStart: tai): LongInt;
    var
      hp1: tai;
      RefModified: Boolean;
    begin
      Result := 0;
      hp1 := BlockStart;
      RefModified := False; { As long as the condition is inverted, this can be reset }

      while assigned(hp1) and
        (hp1 <> BlockStop) do
        begin
          case hp1.typ of
            ait_instruction:
              if MatchInstruction(hp1, A_MOV, []) then
                begin
                  Inc(Result);
                  if taicpu(hp1).oper[1]^.typ = top_reg then
                    begin
                      Inc(Result);
                    end
                  else if not (cs_opt_size in current_settings.optimizerswitches) and
                    { CSEL with constants grows the code size }
                    TryCSELConst(hp1, SearchStart, BlockStop, Result) then
                    begin
                      { Register was reserved by TryCSELConst and
                        stored on ConstRegs }
                    end
                  else
                    begin
                      Result := -1;
                      Exit;
                    end;
                end
              else
                begin
                  Result := -1;
                  Exit;
                end;
            else
              { Most likely an align };
          end;
          fOptimizer.GetNextInstruction(hp1, hp1);
        end;
    end;


  constructor TCSELTracking.Init(Optimizer: TCpuAsmOptimizer; var p_initialjump, p_initialmov: tai; var AFirstLabel: TAsmLabel);

    { For the tsBranching type, increase the weighting score to account for the new conditional jump
      (this is done as a separate stage because the double types are extensions of the branching type,
      but we can't discount the conditional jump until the last step) }
    procedure EvaluateBranchingType;
      begin
        Inc(CSELScore);
        if (CSELScore > MAX_CSEL_INSTRUCTIONS) then
          { Too many instructions to be worthwhile }
          fState := tsInvalid;
      end;

    var
      hp1: tai;
      Count: Integer;
    begin
      { Table of valid CSEL block types

        Block type                  2nd Jump    Mid-label   2nd MOVs    3rd Jump    End-label
        ----------                  ---------   ---------   ---------   ---------   ---------
        tsSimple                        X          Yes          X           X           X
        tsDetour                      = 1st         X           X           X           X
        tsBranching                  <> Mid        Yes          X           X           X
        tsDouble                    End-label      Yes *       Yes          X          Yes
        tsDoubleBranchSame           <> Mid        Yes *       Yes        = 2nd         X
        tsDoubleBranchDifferent      <> Mid        Yes *       Yes       <> 2nd         X
        tsDoubleSecondBranching     End-label      Yes *       Yes       <> 2nd        Yes

        * Only one reference allowed
      }

      hp1 := nil; { To prevent compiler warnings }

      Optimizer.CopyUsedRegs(RegisterTracking);
      fOptimizer := Optimizer;
      fLabel := AFirstLabel;

      CSELScore := 0;
      ConstCount := 0;

      { Initialise RegWrites, ConstRegs, ConstVals, ConstSizes, ConstWriteSizes and ConstMovs }
      FillChar(RegWrites[0], MAX_CSEL_INSTRUCTIONS * 2 * SizeOf(TRegister), 0);
      FillChar(ConstRegs[0], MAX_CSEL_REGISTERS * SizeOf(TRegister), 0);
      FillChar(ConstVals[0], MAX_CSEL_REGISTERS * SizeOf(TCGInt), 0);
      FillChar(ConstSizes[0], MAX_CSEL_REGISTERS * SizeOf(TSubRegister), 0);
      FillChar(ConstWriteSizes[0], first_int_imreg * SizeOf(TOpSize), 0);
      FillChar(ConstMovs[0], MAX_CSEL_REGISTERS * SizeOf(taicpu), 0);

      fInsertionPoint := p_initialjump;
      fCondition := nil;
      fInitialJump := p_initialjump;
      fFirstMovBlock := p_initialmov;
      fFirstMovBlockStop := nil;

      fSecondJump := nil;
      fSecondMovBlock := nil;
      fSecondMovBlockStop := nil;

      fMidLabel := nil;

      fSecondJump := nil;
      fSecondMovBlock := nil;

      fEndLabel := nil;

      fAllocationRange := nil;

      { Assume it all goes horribly wrong! }
      fState := tsInvalid;

      { Look backwards at the comparisons to get an accurate picture of register usage and a better position for any MOV const,reg insertions }
      if Optimizer.GetLastInstruction(p_initialjump, fCondition) and
        (
          MatchInstruction(fCondition, [A_CMP, A_CMN, A_TST], []) or
          (
            (fCondition.typ = ait_instruction) and
            (taicpu(fCondition).opcode = A_AND) and
            (taicpu(fCondition).oppostfix = PF_S)
          )
        ) then
        begin
          { Mark all the registers in the comparison as 'in use', even if they've just been deallocated }
          for Count := 0 to taicpu(fCondition).ops - 1 do
            with taicpu(fCondition).oper[Count]^ do
              case typ of
                top_reg:
                  if getregtype(reg) = R_INTREGISTER then
                    Optimizer.IncludeRegInUsedRegs(reg, RegisterTracking);
                top_ref:
                  begin
                    if
                      (ref^.base <> NR_NO) then
                      Optimizer.IncludeRegInUsedRegs(ref^.base, RegisterTracking);

                    if (ref^.index <> NR_NO) then
                      Optimizer.IncludeRegInUsedRegs(ref^.index, RegisterTracking);
                  end
                else
                  ;
              end;

          { When inserting instructions before hp_prev, try to insert them
            before the allocation of the FLAGS register }
          if not SetAndTest(Optimizer.FindRegAllocBackward(NR_DEFAULTFLAGS, tai(fCondition.Previous)), fInsertionPoint) or
            (tai_regalloc(fInsertionPoint).ratype = ra_dealloc) then
            { If not found, set it equal to the condition so it's something sensible }
            fInsertionPoint := fCondition;

        end
      else
        fCondition := nil;

      { When inserting instructions, try to insert them before the allocation of the FLAGS register }
      if SetAndTest(Optimizer.FindRegAllocBackward(NR_DEFAULTFLAGS, tai(p_initialjump.Previous)), hp1) and
        (tai_regalloc(hp1).ratype <> ra_dealloc) then
        { If not found, set it equal to p so it's something sensible }
        fInsertionPoint := hp1;

      hp1 := p_initialmov;

      if not InitialiseBlock(p_initialmov, p_initialjump, fFirstMovBlockStop, fSecondJump) then
        Exit;

      hp1 := fFirstMovBlockStop; { Will either be on a label or a jump }

      if (hp1.typ <> ait_label) then { should be on a jump }
        begin
          if not Optimizer.GetNextInstruction(hp1, fMidLabel) or (fMidLabel.typ <> ait_label) then
            { Need a label afterwards }
            Exit;
        end
      else
        fMidLabel := hp1;

      if tai_label(fMidLabel).labsym <> AFirstLabel then
        { Not the correct label }
        fMidLabel := nil;

      if not Assigned(fSecondJump) and not Assigned(fMidLabel) then
        { If there's neither a 2nd jump nor correct label, then it's invalid
          (see above table) }
        Exit;

      { Analyse the first block of MOVs more closely }
      CSELScore := AnalyseMOVBlock(fFirstMovBlock, fFirstMovBlockStop, fInsertionPoint);

      if Assigned(fSecondJump) then
        begin
          if (JumpTargetOp(taicpu(fSecondJump))^.ref^.symbol = AFirstLabel) then
            begin
              fState := tsDetour
            end
          else
            begin
              { Need the correct mid-label for this one }
              if not Assigned(fMidLabel) then
                Exit;

              fState := tsBranching;
            end;
        end
      else
        { No jump. but mid-label is present }
        fState := tsSimple;

      if (CSELScore > MAX_CSEL_INSTRUCTIONS) or (CSELScore <= 0) then
        begin
          { Invalid or too many instructions to be worthwhile }
          fState := tsInvalid;
          Exit;
        end;


      { check further for
             b     xxx
             <several movs 1>
             bl    yyy
         xxx:
             <several movs 2>
         yyy:

        etc.
      }
      if (fState = tsBranching) and
        { Estimate for required savings for extra jump }
        (CSELScore <= MAX_CSEL_INSTRUCTIONS - 1) and
        { Only one reference is allowed for double blocks }
        (AFirstLabel.getrefs = 1) then
        begin
          Optimizer.GetNextInstruction(fMidLabel, hp1);
          fSecondMovBlock := hp1;

          if not InitialiseBlock(fSecondMovBlock, fMidLabel, fSecondMovBlockStop, fThirdJump) then
            begin
              EvaluateBranchingType;
              Exit;
            end;

          hp1 := fSecondMovBlockStop; { Will either be on a label or a jump }

          if (hp1.typ <> ait_label) then { should be on a jump }
            begin
              if not Optimizer.GetNextInstruction(hp1, fEndLabel) or (fEndLabel.typ <> ait_label) then
                begin
                  { Need a label afterwards }
                  EvaluateBranchingType;
                  Exit;
                end;
            end
          else
            fEndLabel := hp1;

          if tai_label(fEndLabel).labsym <> JumpTargetOp(taicpu(fSecondJump))^.ref^.symbol then
            { Second jump doesn't go to the end }
            fEndLabel := nil;

          if not Assigned(fThirdJump) and not Assigned(fEndLabel) then
            begin
            { If there's neither a 3rd jump nor correct end label, then it's
              not a invalid double block, but is a valid single branching
              block (see above table) }
              EvaluateBranchingType;
              Exit;
            end;

          Count := AnalyseMOVBlock(fSecondMovBlock, fSecondMovBlockStop, fMidLabel);

          if (Count > MAX_CSEL_INSTRUCTIONS) or (Count <= 0) then
            { Invalid or too many instructions to be worthwhile }
            Exit;

          Inc(CSELScore, Count);

          if Assigned(fThirdJump) then
            begin
              if not Assigned(fSecondJump) then
                fState := tsDoubleSecondBranching
              else if (JumpTargetOp(taicpu(fSecondJump))^.ref^.symbol = JumpTargetOp(taicpu(fThirdJump))^.ref^.symbol) then
                fState := tsDoubleBranchSame
              else
                fState := tsDoubleBranchDifferent;
            end
          else
            fState := tsDouble;
        end;

      if fState = tsBranching then
        EvaluateBranchingType;
    end;

  { Tries to convert a mov const,%reg instruction into a CSEL by reserving a
    new register to store the constant }
  function TCSELTracking.TryCSELConst(p, start, stop: tai; var Count: LongInt): Boolean;
    var
      RegSize: TSubRegister;
      CurrentVal: TCGInt;
      ANewReg: TRegister;
      X: ShortInt;
    begin
      Result := False;

      if not MatchOpType(taicpu(p), top_reg, top_const) then
        Exit;

      if ConstCount >= MAX_CSEL_REGISTERS then
        { Arrays are full }
        Exit;

      { See if the value has already been reserved for another CSEL instruction }
      CurrentVal := taicpu(p).oper[1]^.val;
      RegSize := getsubreg(taicpu(p).oper[0]^.reg);
      for X := 0 to ConstCount - 1 do
        if ConstVals[X] = CurrentVal then
          begin
            ConstRegs[ConstCount] := ConstRegs[X];
            ConstSizes[ConstCount] := RegSize;
            ConstVals[ConstCount] := CurrentVal;

            Inc(ConstCount);
            Inc(Count);

            Result := True;
            Exit;
          end;

      ANewReg := fOptimizer.GetIntRegisterBetween(R_SUBWHOLE, RegisterTracking, start, stop, True);
      if ANewReg = NR_NO then
        { No free registers }
        Exit;

      { Reserve the register so subsequent TryCSELConst calls don't all end
        up vying for the same register }
      fOptimizer.IncludeRegInUsedRegs(ANewReg, RegisterTracking);

      ConstRegs[ConstCount] := ANewReg;
      ConstSizes[ConstCount] := RegSize;
      ConstVals[ConstCount] := CurrentVal;

      Inc(ConstCount);
      Inc(Count);

      Result := True;
    end;

  destructor TCSELTracking.Done;
    begin
      TAOptObj.ReleaseUsedRegs(RegisterTracking);
    end;

  procedure TCSELTracking.Process(out new_p: tai);
    var
      Count, Writes: LongInt;
      RegMatch: Boolean;
      hp1, hp_new: tai;
      inverted_condition, condition: TAsmCond;
    begin
      if (fState in [tsInvalid, tsProcessed]) then
        InternalError(2023110702);

      { Repurpose RegisterTracking to mark registers that we've defined }
      RegisterTracking[R_INTREGISTER].Clear;

      Count := 0;
      Writes := 0;
      condition := taicpu(fInitialJump).condition;
      inverted_condition := inverse_cond(condition);

      { Exclude tsDoubleBranchDifferent from this check, as the second block
        doesn't get CSELs in this case }
      if (fState in [tsDouble, tsDoubleBranchSame, tsDoubleSecondBranching]) then
        begin
          { Include the jump in the flag tracking }
          if Assigned(fThirdJump) then
            begin
              if (fState = tsDoubleBranchSame) then
                begin
                  { Will be an unconditional jump, so track to the instruction before it }
                  if not fOptimizer.GetLastInstruction(fThirdJump, hp1) then
                    InternalError(2023110712);
                end
              else
                hp1 := fThirdJump;
            end
          else
            hp1 := fSecondMovBlockStop;
        end
      else
        begin
          { Include a conditional jump in the flag tracking }
          if Assigned(fSecondJump) then
            begin
              if (fState = tsDetour) then
                begin
                  { Will be an unconditional jump, so track to the instruction before it }
                  if not fOptimizer.GetLastInstruction(fSecondJump, hp1) then
                    InternalError(2023110713);
                end
              else
                hp1 := fSecondJump;
            end
          else
            hp1 := fFirstMovBlockStop;
        end;

      fOptimizer.AllocRegBetween(NR_DEFAULTFLAGS, fInitialJump, hp1, fOptimizer.UsedRegs);

      { Process the second set of MOVs first, because if a destination
        register is shared between the first and second MOV sets, it is more
        efficient to turn the first one into a MOV instruction and place it
        before the CMP if possible, but we won't know which registers are
        shared until we've processed at least one list, so we might as well
        make it the second one since that won't be modified again. }

      if (fState in [tsDouble, tsDoubleBranchSame, tsDoubleBranchDifferent, tsDoubleSecondBranching]) then
        begin
          hp1 := fSecondMovBlock;
          repeat
            if not Assigned(hp1) then
              InternalError(2018062902);

            if (hp1.typ = ait_instruction) then
              begin
                { Extra safeguard }
                if (taicpu(hp1).opcode <> A_MOV) then
                  InternalError(2018062903);

                { Note: tsDoubleBranchDifferent is essentially identical to
                  tsBranching and the 2nd block is best left largely
                  untouched, but we need to evaluate which registers the MOVs
                  write to in order to track what would be complementary CSEL
                  pairs that can be further optimised. [Kit] }
                if fState <> tsDoubleBranchDifferent then
                  begin
                    if taicpu(hp1).oper[1]^.typ = top_const then
                      begin
                        RegMatch := False;

                        for Count := 0 to ConstCount - 1 do
                          if (ConstVals[Count] = taicpu(hp1).oper[1]^.val) and
                            (getsubreg(taicpu(hp1).oper[0]^.reg) = ConstSizes[Count]) then
                            begin
                              RegMatch := True;

                              { If it's in RegisterTracking, then this register
                                is being used more than once and hence has
                                already had its value defined (it gets added to
                                UsedRegs through AllocRegBetween below) }
                              if not RegisterTracking[R_INTREGISTER].IsUsed(ConstRegs[Count]) then
                                begin
                                  hp_new := tai(hp1.getcopy);
                                  taicpu(hp_new).oper[0]^.reg := ConstRegs[Count];
                                  taicpu(hp_new).fileinfo := taicpu(fInitialJump).fileinfo;

                                  fOptimizer.asml.InsertBefore(hp_new, fInsertionPoint);
                                  fOptimizer.IncludeRegInUsedRegs(ConstRegs[Count], RegisterTracking);

                                  ConstMovs[Count] := hp_new;
                                end
                              else
                                { We just need an instruction between hp_prev and hp1
                                  where we know the register is marked as in use }
                                hp_new := fSecondMovBlock;

                              { Keep track of largest write for this register so it can be optimised later }
                              if (getsubreg(taicpu(hp1).oper[0]^.reg) > ConstWriteSizes[getsupreg(ConstRegs[Count])]) then
                                ConstWriteSizes[getsupreg(ConstRegs[Count])] := getsubreg(taicpu(hp1).oper[0]^.reg);

                              fOptimizer.AllocRegBetween(ConstRegs[Count], hp_new, hp1, fOptimizer.UsedRegs);
                              taicpu(hp1).loadreg(1, newreg(R_INTREGISTER, getsupreg(ConstRegs[Count]), ConstSizes[Count]));
                              Break;
                            end;

                        if not RegMatch then
                          InternalError(2021100413);
                      end;

                    taicpu(hp1).opcode := A_CSEL;
                    taicpu(hp1).ops := 4;
                    taicpu(hp1).loadreg(2, taicpu(hp1).oper[0]^.reg);
                    taicpu(hp1).loadconditioncode(3, condition);
                  end;

                { Store these writes to search for duplicates later on }
                RegWrites[Writes] := taicpu(hp1).oper[0]^.reg;
                Inc(Writes);
              end;

            fOptimizer.GetNextInstruction(hp1, hp1);
          until (hp1 = fSecondMovBlockStop);
        end;

      { Now do the first set of MOVs }
      hp1 := fFirstMovBlock;
      repeat
        if not Assigned(hp1) then
          InternalError(2018062904);

        if (hp1.typ = ait_instruction) then
          begin
            RegMatch := False;

            { Extra safeguard }
            if (taicpu(hp1).opcode <> A_MOV) then
              InternalError(2018062905);

            { Search through the RegWrites list to see if there are any
              opposing CSEL pairs that write to the same register }
            for Count := 0 to Writes - 1 do
              if (RegWrites[Count] = taicpu(hp1).oper[0]^.reg) then
                begin
                  { We have a match.  Keep this as a MOV }

                  { Move ahead in preparation }
                  fOptimizer.GetNextInstruction(hp1, hp1);

                  RegMatch := True;
                  Break;
                end;

            if RegMatch then
              Continue;

            if taicpu(hp1).oper[1]^.typ = top_const then
              begin
                for Count := 0 to ConstCount - 1 do
                  if (ConstVals[Count] = taicpu(hp1).oper[1]^.val) and
                    (getsubreg(taicpu(hp1).oper[0]^.reg) = ConstSizes[Count]) then
                    begin
                      RegMatch := True;

                      { If it's in RegisterTracking, then this register is
                        being used more than once and hence has already had
                        its value defined (it gets added to UsedRegs through
                        AllocRegBetween below) }
                      if not RegisterTracking[R_INTREGISTER].IsUsed(ConstRegs[Count]) then
                        begin
                          hp_new := tai(hp1.getcopy);
                          taicpu(hp_new).oper[0]^.reg := ConstRegs[Count];
                          taicpu(hp_new).fileinfo := taicpu(fInitialJump).fileinfo;

                          fOptimizer.asml.InsertBefore(hp_new, fInsertionPoint);
                          fOptimizer.IncludeRegInUsedRegs(ConstRegs[Count], RegisterTracking);

                          ConstMovs[Count] := hp_new;
                        end
                      else
                        { We just need an instruction between hp_prev and hp1
                          where we know the register is marked as in use }
                          hp_new := fFirstMovBlock;

                      { Keep track of largest write for this register so it can be optimised later }
                      if (getsubreg(taicpu(hp1).oper[0]^.reg) > ConstWriteSizes[getsupreg(ConstRegs[Count])]) then
                        ConstWriteSizes[getsupreg(ConstRegs[Count])] := getsubreg(taicpu(hp1).oper[0]^.reg);

                      fOptimizer.AllocRegBetween(ConstRegs[Count], hp_new, hp1, fOptimizer.UsedRegs);
                      taicpu(hp1).loadreg(1, newreg(R_INTREGISTER, getsupreg(ConstRegs[Count]), ConstSizes[Count]));
                      Break;
                    end;

                if not RegMatch then
                  InternalError(2021100412);
              end;

              taicpu(hp1).opcode := A_CSEL;
              taicpu(hp1).ops := 4;
              taicpu(hp1).loadreg(2, taicpu(hp1).oper[0]^.reg);
              taicpu(hp1).loadconditioncode(3, inverted_condition);

            if (fState = tsDoubleBranchDifferent) then
              begin
                { Store these writes to search for duplicates later on }
                RegWrites[Writes] := taicpu(hp1).oper[0]^.reg;
                Inc(Writes);
              end;
          end;

        fOptimizer.GetNextInstruction(hp1, hp1);
      until (hp1 = fFirstMovBlockStop);

      { Update initialisation MOVs to the smallest possible size }
      for Count := 0 to ConstCount - 1 do
        if Assigned(ConstMovs[Count]) then
          setsubreg(taicpu(ConstMovs[Count]).oper[0]^.reg, ConstWriteSizes[Word(ConstRegs[Count])]);

      case fState of
        tsSimple:
          begin
            fOptimizer.DebugMsg(SPeepholeOptimization + 'CSEL Block (Simple type)', fInitialJump);
            { No branch to delete }
          end;
        tsDetour:
          begin
            fOptimizer.DebugMsg(SPeepholeOptimization + 'CSEL Block (Detour type)', fInitialJump);
            { Preserve jump }
          end;
        tsBranching, tsDoubleBranchDifferent:
          begin
            if (fState = tsBranching) then
              fOptimizer.DebugMsg(SPeepholeOptimization + 'CSEL Block (Branching type)', fInitialJump)
            else
              fOptimizer.DebugMsg(SPeepholeOptimization + 'CSEL Block (Double branching (different) type)', fInitialJump);
            taicpu(fSecondJump).condition := inverted_condition;
          end;
        tsDouble, tsDoubleBranchSame:
          begin
            if (fState = tsDouble) then
              fOptimizer.DebugMsg(SPeepholeOptimization + 'CSEL Block (Double type)', fInitialJump)
            else
              fOptimizer.DebugMsg(SPeepholeOptimization + 'CSEL Block (Double branching (same) type)', fInitialJump);
            { Delete second jump }
            JumpTargetOp(taicpu(fSecondJump))^.ref^.symbol.decrefs;
            fOptimizer.RemoveInstruction(fSecondJump);
          end;
        tsDoubleSecondBranching:
          begin
            fOptimizer.DebugMsg(SPeepholeOptimization + 'CSEL Block (Double, second branching type)', fInitialJump);
            { Delete second jump, preserve third jump as conditional }
            JumpTargetOp(taicpu(fSecondJump))^.ref^.symbol.decrefs;
            fOptimizer.RemoveInstruction(fSecondJump);
            taicpu(fThirdJump).condition := condition;
          end;
        else
          InternalError(2023110721);
      end;

      { Now we can safely decrement the reference count }
      tasmlabel(fLabel).decrefs;

      fOptimizer.UpdateUsedRegs(tai(fInitialJump.next));

      { Remove the original jump }
      fOptimizer.RemoveInstruction(fInitialJump); { Note, the choice to not use RemoveCurrentp is deliberate }

      new_p := fFirstMovBlock; { Appears immediately after the initial jump }

      fState := tsProcessed;
    end;

begin
  casmoptimizer:=TCpuAsmOptimizer;
End.

