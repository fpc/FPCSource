{
    Copyright (c) 1998-2002 by Florian Klaempfl and Jonas Maebe

    This unit contains the peephole optimizer.

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
unit aoptx86;

{$i fpcdefs.inc}

{ $define DEBUG_AOPTCPU}

{$ifdef EXTDEBUG}
{$define DEBUG_AOPTCPU}
{$endif EXTDEBUG}

  interface

    uses
      globtype,cclasses,
      cpubase,
      aasmtai,aasmcpu,
      cgbase,cgutils,
      aopt,aoptobj;

    type
      TOptsToCheck = (
        aoc_MovAnd2Mov_3,
        aoc_ForceNewIteration
      );

      TX86AsmOptimizer = class(TAsmOptimizer)
        { some optimizations are very expensive to check, so the
          pre opt pass can be used to set some flags, depending on the found
          instructions if it is worth to check a certain optimization }
        OptsToCheck : set of TOptsToCheck;
        function RegLoadedWithNewValue(reg : tregister; hp : tai) : boolean; override;
        function InstructionLoadsFromReg(const reg : TRegister; const hp : tai) : boolean; override;
        class function RegReadByInstruction(reg : TRegister; hp : tai) : boolean; static;
        function RegInInstruction(Reg: TRegister; p1: tai): Boolean;override;
        function GetNextInstructionUsingReg(Current: tai; out Next: tai; reg: TRegister): Boolean;

        { Identical to GetNextInstructionUsingReg, but returns a value indicating
          how many instructions away that Next is from Current is.

          0 = failure, equivalent to False in GetNextInstructionUsingReg }
        function GetNextInstructionUsingRegCount(Current: tai; out Next: tai; reg: TRegister): Cardinal;

        { This version of GetNextInstructionUsingReg will look across conditional jumps,
          potentially allowing further optimisation (although it might need to know if
          it crossed a conditional jump. }
        function GetNextInstructionUsingRegCond(Current: tai; out Next: tai; reg: TRegister; var JumpTracking: TLinkedList; var CrossJump: Boolean): Boolean;

        {
          In comparison with GetNextInstructionUsingReg, GetNextInstructionUsingRegTrackingUse tracks
          the use of a register by allocs/dealloc, so it can ignore calls.

          In the following example, GetNextInstructionUsingReg will return the second movq,
          GetNextInstructionUsingRegTrackingUse won't.

          movq	%rdi,%rax
          # Register rdi released

          # Register rdi allocated
          movq	%rax,%rdi

          While in this example:

          movq	%rdi,%rax
          call  proc
          movq	%rdi,%rax

          GetNextInstructionUsingRegTrackingUse will return the second instruction while GetNextInstructionUsingReg
          won't.
        }
        function GetNextInstructionUsingRegTrackingUse(Current: tai; out Next: tai; reg: TRegister): Boolean;
        function RegModifiedByInstruction(Reg: TRegister; p1: tai): boolean; override;
      private
        function SkipSimpleInstructions(var hp1: tai): Boolean;

      protected
        class function IsMOVZXAcceptable: Boolean; static; inline;

        function CheckMovMov2MovMov2(const p, hp1: tai): Boolean;

        { Attempts to allocate a volatile integer register for use between p and hp,
          using AUsedRegs for the current register usage information.  Returns NR_NO
          if no free register could be found }
        function GetIntRegisterBetween(RegSize: TSubRegister; var AUsedRegs: TAllUsedRegs; p, hp: tai; DontAlloc: Boolean = False): TRegister;
        { Attempts to allocate a volatile MM register for use between p and hp,
          using AUsedRegs for the current register usage information.  Returns NR_NO
          if no free register could be found }
        function GetMMRegisterBetween(RegSize: TSubRegister; var AUsedRegs: TAllUsedRegs; p, hp: tai; DontAlloc: Boolean = False): TRegister;

        { checks whether loading a new value in reg1 overwrites the entirety of reg2 }
        class function Reg1WriteOverwritesReg2Entirely(reg1, reg2: tregister): boolean; static;
        { checks whether reading the value in reg1 depends on the value of reg2. This
          is very similar to SuperRegisterEquals, except it takes into account that
          R_SUBH and R_SUBL are independendent (e.g. reading from AL does not
          depend on the value in AH). }
        class function Reg1ReadDependsOnReg2(reg1, reg2: tregister): boolean; static;

        { Replaces all references to AOldReg in a memory reference to ANewReg }
        class function ReplaceRegisterInRef(var ref: TReference; const AOldReg, ANewReg: TRegister): Boolean; static;

        { Replaces all references to AOldReg in an operand to ANewReg }
        class function ReplaceRegisterInOper(const p: taicpu; const OperIdx: Integer; const AOldReg, ANewReg: TRegister): Boolean; static;

        { Replaces all references to AOldReg in an instruction to ANewReg,
          except where the register is being written }
        class function ReplaceRegisterInInstruction(const p: taicpu; const AOldReg, ANewReg: TRegister): Boolean; static;

        { Returns true if the reference only refers to ESP or EBP (or their 64-bit equivalents),
          or writes to a global symbol }
        class function IsRefSafe(const ref: PReference): Boolean; static;


        { Returns true if the given MOV instruction can be safely converted to CMOV }
        class function CanBeCMOV(p, cond_p: tai; var RefModified: Boolean) : boolean; static;

        { Like UpdateUsedRegs, but ignores deallocations }
        class procedure UpdateIntRegsNoDealloc(var AUsedRegs: TAllUsedRegs; p: Tai); static;

        { Returns true if the given logic instruction can be converted into a BTx instruction (BT not included) }
        class function IsBTXAcceptable(p : tai) : boolean; static;


        { Converts the LEA instruction to ADD/INC/SUB/DEC. Returns True if the
          conversion was successful }
        function ConvertLEA(const p : taicpu): Boolean;

        function DeepMOVOpt(const p_mov: taicpu; const hp: taicpu): Boolean;

        function FuncMov2Func(var p: tai; const hp1: tai): Boolean;

        procedure DebugMsg(const s : string; p : tai);inline;

        class function IsExitCode(p : tai) : boolean; static;
        class function isFoldableArithOp(hp1 : taicpu; reg : tregister) : boolean; static;
        class function IsShrMovZFoldable(shr_size, movz_size: topsize; Shift: TCGInt): Boolean; static;
        procedure RemoveLastDeallocForFuncRes(p : tai);

        function DoArithCombineOpt(var p : tai) : Boolean;
        function DoMovCmpMemOpt(var p : tai; const hp1: tai) : Boolean;
        function DoSETccLblRETOpt(var p: tai; const hp_label: tai_label) : Boolean;

        function PrePeepholeOptSxx(var p : tai) : boolean;
        function PrePeepholeOptIMUL(var p : tai) : boolean;
        function PrePeepholeOptAND(var p : tai) : boolean;

        function OptPass1Test(var p: tai): boolean;
        function OptPass1Add(var p: tai): boolean;
        function OptPass1AND(var p : tai) : boolean;
        function OptPass1_V_MOVAP(var p : tai) : boolean;
        function OptPass1VOP(var p : tai) : boolean;
        function OptPass1MOV(var p : tai) : boolean;
        function OptPass1Movx(var p : tai) : boolean;
        function OptPass1MOVXX(var p : tai) : boolean;
        function OptPass1OP(var p : tai) : boolean;
        function OptPass1LEA(var p : tai) : boolean;
        function OptPass1Sub(var p : tai) : boolean;
        function OptPass1SHLSAL(var p : tai) : boolean;
        function OptPass1SHR(var p : tai) : boolean;
        function OptPass1FSTP(var p : tai) : boolean;
        function OptPass1FLD(var p : tai) : boolean;
        function OptPass1Cmp(var p : tai) : boolean;
        function OptPass1PXor(var p : tai) : boolean;
        function OptPass1VPXor(var p: tai): boolean;
        function OptPass1Imul(var p : tai) : boolean;
        function OptPass1Jcc(var p : tai) : boolean;
        function OptPass1SHXX(var p: tai): boolean;
        function OptPass1VMOVDQ(var p: tai): Boolean;
        function OptPass1_V_Cvtss2sd(var p: tai): boolean;

        function OptPass2Movx(var p : tai): Boolean;
        function OptPass2MOV(var p : tai) : boolean;
        function OptPass2Imul(var p : tai) : boolean;
        function OptPass2Jmp(var p : tai) : boolean;
        function OptPass2Jcc(var p : tai) : boolean;
        function OptPass2Lea(var p: tai): Boolean;
        function OptPass2SUB(var p: tai): Boolean;
        function OptPass2ADD(var p : tai): Boolean;
        function OptPass2SETcc(var p : tai) : boolean;

        function CheckMemoryWrite(var first_mov, second_mov: taicpu): Boolean;

        function PostPeepholeOptMov(var p : tai) : Boolean;
        function PostPeepholeOptMovzx(var p : tai) : Boolean;
        function PostPeepholeOptXor(var p : tai) : Boolean;
        function PostPeepholeOptAnd(var p : tai) : boolean;
        function PostPeepholeOptMOVSX(var p : tai) : boolean;
        function PostPeepholeOptCmp(var p : tai) : Boolean;
        function PostPeepholeOptTestOr(var p : tai) : Boolean;
        function PostPeepholeOptCall(var p : tai) : Boolean;
        function PostPeepholeOptLea(var p : tai) : Boolean;
        function PostPeepholeOptPush(var p: tai): Boolean;
        function PostPeepholeOptShr(var p : tai) : boolean;
        function PostPeepholeOptADDSUB(var p : tai) : Boolean;
        function PostPeepholeOptVPXOR(var p: tai): Boolean;

        procedure ConvertJumpToRET(const p: tai; const ret_p: tai);

        function CheckJumpMovTransferOpt(var p: tai; hp1: tai; LoopCount: Integer; out Count: Integer): Boolean;
        function TrySwapMovOp(var p, hp1: tai): Boolean;
        function TrySwapMovCmp(var p, hp1: tai): Boolean;

        { Processor-dependent reference optimisation }
        class procedure OptimizeRefs(var p: taicpu); static;
      end;

    function MatchInstruction(const instr: tai; const op: TAsmOp; const opsize: topsizes): boolean;
    function MatchInstruction(const instr: tai; const op1,op2: TAsmOp; const opsize: topsizes): boolean;
    function MatchInstruction(const instr: tai; const op1,op2,op3: TAsmOp; const opsize: topsizes): boolean;
    function MatchInstruction(const instr: tai; const ops: array of TAsmOp; const opsize: topsizes): boolean;

    function MatchOperand(const oper: TOper; const reg: TRegister): boolean; inline;
    function MatchOperand(const oper: TOper; const a: tcgint): boolean; inline;
    function MatchOperand(const oper1: TOper; const oper2: TOper): boolean;
{$if max_operands>2}
    function MatchOperand(const oper1: TOper; const oper2: TOper; const oper3: TOper): boolean;
{$endif max_operands>2}

    function RefsEqual(const r1, r2: treference): boolean;

    { Note that Result is set to True if the references COULD overlap but the
      compiler cannot be sure (e.g. "(%reg1)" and "4(%reg2)" with a range of 4
      might still overlap because %reg2 could be equal to %reg1-4 }
    function RefsMightOverlap(const r1, r2: treference; const Range: asizeint): boolean;

    function MatchReference(const ref : treference;base,index : TRegister) : Boolean;

    { returns true, if ref is a reference using only the registers passed as base and index
      and having an offset }
    function MatchReferenceWithOffset(const ref : treference;base,index : TRegister) : Boolean;

  implementation

    uses
      cutils,verbose,
      systems,
      globals,
      cpuinfo,
      procinfo,
      paramgr,
      aasmbase,
      aoptbase,aoptutils,
      symconst,symsym,
      cgx86,
      itcpugas;

{$ifdef DEBUG_AOPTCPU}
    const
      SPeepholeOptimization: shortstring = 'Peephole Optimization: ';
{$else DEBUG_AOPTCPU}
    { Empty strings help the optimizer to remove string concatenations that won't
      ever appear to the user on release builds. [Kit] }
    const
      SPeepholeOptimization = '';
{$endif DEBUG_AOPTCPU}
      LIST_STEP_SIZE = 4;
{$ifndef 8086}
      MAX_CMOV_INSTRUCTIONS = 4;
      MAX_CMOV_REGISTERS = 8;
{$endif 8086}

    type
      TJumpTrackingItem = class(TLinkedListItem)
      private
        FSymbol: TAsmSymbol;
        FRefs: LongInt;
      public
        constructor Create(ASymbol: TAsmSymbol);
        procedure IncRefs; {$ifdef USEINLINE}inline;{$endif USEINLINE}
        property Symbol: TAsmSymbol read FSymbol;
        property Refs: LongInt read FRefs;
      end;


    constructor TJumpTrackingItem.Create(ASymbol: TAsmSymbol);
      begin
        inherited Create;
        FSymbol := ASymbol;
        FRefs := 0;
      end;


    procedure TJumpTrackingItem.IncRefs; {$ifdef USEINLINE}inline;{$endif USEINLINE}
      begin
        Inc(FRefs);
      end;


    function MatchInstruction(const instr: tai; const op: TAsmOp; const opsize: topsizes): boolean;
      begin
        result :=
          (instr.typ = ait_instruction) and
          (taicpu(instr).opcode = op) and
          ((opsize = []) or (taicpu(instr).opsize in opsize));
      end;


    function MatchInstruction(const instr: tai; const op1,op2: TAsmOp; const opsize: topsizes): boolean;
      begin
        result :=
          (instr.typ = ait_instruction) and
          ((taicpu(instr).opcode = op1) or
           (taicpu(instr).opcode = op2)
          ) and
          ((opsize = []) or (taicpu(instr).opsize in opsize));
      end;


    function MatchInstruction(const instr: tai; const op1,op2,op3: TAsmOp; const opsize: topsizes): boolean;
      begin
        result :=
          (instr.typ = ait_instruction) and
          ((taicpu(instr).opcode = op1) or
           (taicpu(instr).opcode = op2) or
           (taicpu(instr).opcode = op3)
          ) and
          ((opsize = []) or (taicpu(instr).opsize in opsize));
      end;


    function MatchInstruction(const instr : tai;const ops : array of TAsmOp;
     const opsize : topsizes) : boolean;
      var
        op : TAsmOp;
      begin
        result:=false;
        if (instr.typ <> ait_instruction) or
          ((opsize <> []) and not(taicpu(instr).opsize in opsize)) then
          exit;
        for op in ops do
          begin
            if taicpu(instr).opcode = op then
              begin
                result:=true;
                exit;
              end;
          end;
      end;


    function MatchOperand(const oper: TOper; const reg: TRegister): boolean; inline;
      begin
        result := (oper.typ = top_reg) and (oper.reg = reg);
      end;


    function MatchOperand(const oper: TOper; const a: tcgint): boolean; inline;
      begin
        result := (oper.typ = top_const) and (oper.val = a);
      end;


    function MatchOperand(const oper1: TOper; const oper2: TOper): boolean;
      begin
        result := oper1.typ = oper2.typ;

        if result then
          case oper1.typ of
            top_const:
              Result:=oper1.val = oper2.val;
            top_reg:
              Result:=oper1.reg = oper2.reg;
            top_ref:
              Result:=RefsEqual(oper1.ref^, oper2.ref^);
            else
              internalerror(2013102801);
          end
      end;


    function MatchOperand(const oper1: TOper; const oper2: TOper; const oper3: TOper): boolean;
      begin
        result := (oper1.typ = oper2.typ) and (oper1.typ = oper3.typ);

        if result then
          case oper1.typ of
            top_const:
              Result:=(oper1.val = oper2.val) and (oper1.val = oper3.val);
            top_reg:
              Result:=(oper1.reg = oper2.reg) and (oper1.reg = oper3.reg);
            top_ref:
              Result:=RefsEqual(oper1.ref^, oper2.ref^) and RefsEqual(oper1.ref^, oper3.ref^);
            else
              internalerror(2020052401);
          end
      end;


    function RefsEqual(const r1, r2: treference): boolean;
      begin
        RefsEqual :=
          (r1.symbol=r2.symbol) and (r1.refaddr = r2.refaddr) and
          (r1.relsymbol = r2.relsymbol) and
          (r1.segment = r2.segment) and (r1.base = r2.base) and
          (r1.index = r2.index) and (r1.scalefactor = r2.scalefactor) and
          (r1.offset = r2.offset) and
          (r1.volatility + r2.volatility = []);
      end;


    function RefsMightOverlap(const r1, r2: treference; const Range: asizeint): boolean;
      begin
        if (r1.symbol<>r2.symbol) then
          { If the index registers are different, there's a chance one could
            be set so it equals the other symbol }
          Exit((r1.index<>r2.index) or (r1.scalefactor<>r2.scalefactor));

        if (r1.symbol=r2.symbol) and (r1.refaddr = r2.refaddr) and
          (r1.relsymbol = r2.relsymbol) and
          (r1.segment = r2.segment) and (r1.base = r2.base) and
          (r1.index = r2.index) and (r1.scalefactor = r2.scalefactor) and
          (r1.volatility + r2.volatility = []) then
          { In this case, it all depends on the offsets }
          Exit(abs(r1.offset - r2.offset) < Range);

        { There's a chance things MIGHT overlap, so take no chances }
        Result := True;
      end;


    function MatchReference(const ref : treference;base,index : TRegister) : Boolean;
      begin
       Result:=(ref.offset=0) and
         (ref.scalefactor in [0,1]) and
         (ref.segment=NR_NO) and
         (ref.symbol=nil) and
         (ref.relsymbol=nil) and
         ((base=NR_INVALID) or
          (ref.base=base)) and
         ((index=NR_INVALID) or
          (ref.index=index)) and
         (ref.volatility=[]);
      end;


    function MatchReferenceWithOffset(const ref : treference;base,index : TRegister) : Boolean;
      begin
       Result:=(ref.scalefactor in [0,1]) and
         (ref.segment=NR_NO) and
         (ref.symbol=nil) and
         (ref.relsymbol=nil) and
         ((base=NR_INVALID) or
          (ref.base=base)) and
         ((index=NR_INVALID) or
          (ref.index=index)) and
         (ref.volatility=[]);
      end;


    function InstrReadsFlags(p: tai): boolean;
      begin
        InstrReadsFlags := true;
        case p.typ of
          ait_instruction:
            if InsProp[taicpu(p).opcode].Ch*
               [Ch_RCarryFlag,Ch_RParityFlag,Ch_RAuxiliaryFlag,Ch_RZeroFlag,Ch_RSignFlag,Ch_ROverflowFlag,
                Ch_RWCarryFlag,Ch_RWParityFlag,Ch_RWAuxiliaryFlag,Ch_RWZeroFlag,Ch_RWSignFlag,Ch_RWOverflowFlag,
                Ch_RFlags,Ch_RWFlags,Ch_RFLAGScc,Ch_All]<>[] then
              exit;
          ait_label:
            exit;
          else
            ;
        end;
        InstrReadsFlags := false;
      end;


  function TX86AsmOptimizer.GetNextInstructionUsingReg(Current: tai; out Next: tai; reg: TRegister): Boolean;
    begin
      Next:=Current;
      repeat
        Result:=GetNextInstruction(Next,Next);
      until not (Result) or
            not(cs_opt_level3 in current_settings.optimizerswitches) or
            (Next.typ<>ait_instruction) or
            RegInInstruction(reg,Next) or
            is_calljmp(taicpu(Next).opcode);
    end;


  function TX86AsmOptimizer.GetNextInstructionUsingRegCount(Current: tai; out Next: tai; reg: TRegister): Cardinal;
    var
      GetNextResult: Boolean;
    begin
      Result:=0;
      Next:=Current;
      repeat
        GetNextResult := GetNextInstruction(Next,Next);
        if GetNextResult then
          Inc(Result)
        else
          { Must return zero upon hitting the end of the linked list without a match }
          Result := 0;
      until not (GetNextResult) or
            not(cs_opt_level3 in current_settings.optimizerswitches) or
            (Next.typ<>ait_instruction) or
            RegInInstruction(reg,Next) or
            is_calljmp(taicpu(Next).opcode);
    end;


  function TX86AsmOptimizer.GetNextInstructionUsingRegCond(Current: tai; out Next: tai; reg: TRegister; var JumpTracking: TLinkedList; var CrossJump: Boolean): Boolean;

    procedure TrackJump(Symbol: TAsmSymbol);
      var
        Search: TJumpTrackingItem;
      begin
        { See if an entry already exists in our jump tracking list
          (faster to search backwards due to the higher chance of
          matching destinations) }
        Search := TJumpTrackingItem(JumpTracking.Last);

        while Assigned(Search) do
          begin
            if Search.Symbol = Symbol then
              begin
                { Found it - remove it so it can be pushed to the front }
                JumpTracking.Remove(Search);
                Break;
              end;

            Search := TJumpTrackingItem(Search.Previous);
          end;

        if not Assigned(Search) then
          Search := TJumpTrackingItem.Create(JumpTargetOp(taicpu(Next))^.ref^.symbol);

        JumpTracking.Concat(Search);
        Search.IncRefs;
      end;

    function LabelAccountedFor(Symbol: TAsmSymbol): Boolean;
      var
        Search: TJumpTrackingItem;
      begin
        Result := False;

        { See if this label appears in the tracking list }
        Search := TJumpTrackingItem(JumpTracking.Last);
        while Assigned(Search) do
          begin
            if Search.Symbol = Symbol then
              begin
                { Found it - let's see what we can discover }
                if Search.Symbol.getrefs = Search.Refs then
                  begin
                    { Success - all the references are accounted for }
                    JumpTracking.Remove(Search);
                    Search.Free;

                    { It is logically impossible for CrossJump to be false here
                      because we must have run into a conditional jump for
                      this label at some point }
                    if not CrossJump then
                      InternalError(2022041710);

                    if JumpTracking.First = nil then
                      { Tracking list is now empty - no more cross jumps }
                      CrossJump := False;

                    Result := True;
                    Exit;
                  end;

                { If the references don't match, it's possible to enter
                  this label through other means, so drop out }
                Exit;
              end;

            Search := TJumpTrackingItem(Search.Previous);
          end;
      end;

    var
      Next_Label: tai;
    begin
      { Note, CrossJump keeps its input value if a conditional jump is not found - it doesn't get set to False }
      Next := Current;
      repeat
        Result := GetNextInstruction(Next,Next);
        if not Result then
          Break;

        if Next.typ = ait_align then
          Result := SkipAligns(Next, Next);

        if (Next.typ=ait_instruction) and is_calljmp(taicpu(Next).opcode) then
          if is_calljmpuncondret(taicpu(Next).opcode) then
            begin
              if (taicpu(Next).opcode = A_JMP) and
                { Remove dead code now to save time }
                RemoveDeadCodeAfterJump(taicpu(Next)) then
                { A jump was removed, but not the current instruction, and
                  Result doesn't necessarily translate into an optimisation
                  routine's Result, so use the "Force New Iteration" flag so
                  mark a new pass }
                Include(OptsToCheck, aoc_ForceNewIteration);

              if not Assigned(JumpTracking) then
                begin
                  { Cross-label optimisations often causes other optimisations
                    to perform worse because they're not given the chance to
                    optimise locally.  In this case, don't do the cross-label
                    optimisations yet, but flag them as a potential possibility
                    for the next iteration of Pass 1 }
                  if not NotFirstIteration then
                    Include(OptsToCheck, aoc_ForceNewIteration);
                end
              else if IsJumpToLabel(taicpu(Next)) and
                GetNextInstruction(Next, Next_Label) and
                SkipAligns(Next_Label, Next_Label) then
                begin

                  { If we have JMP .lbl, and the label after it has all of its
                    references tracked, then this is probably an if-else style of
                    block and we can keep tracking.  If the label for this jump
                    then appears later and is fully tracked, then it's the end
                    of the if-else blocks and the code paths converge (thus
                    marking the end of the cross-jump) }

                  if (Next_Label.typ = ait_label) then
                    begin
                      if LabelAccountedFor(tai_label(Next_Label).labsym) then
                        begin
                          TrackJump(JumpTargetOp(taicpu(Next))^.ref^.symbol);
                          Next := Next_Label;

                          { CrossJump gets set to false by LabelAccountedFor if the
                            list is completely emptied (as it indicates that all
                            code paths have converged).  We could avoid this nuance
                            by moving the TrackJump call to before the
                            LabelAccountedFor call, but this is slower in situations
                            where LabelAccountedFor would return False due to the
                            creation of a new object that is not used and destroyed
                            soon after. }
                          CrossJump := True;
                          Continue;
                        end;
                    end
                  else if (Next_Label.typ <> ait_marker) then
                    { We just did a RemoveDeadCodeAfterJump, so either we find
                      a label, the end of the procedure or some kind of marker}
                    InternalError(2022041720);
                end;

              Result := False;
              Exit;
            end
          else
            begin
              if not Assigned(JumpTracking) then
                begin
                  { Cross-label optimisations often causes other optimisations
                    to perform worse because they're not given the chance to
                    optimise locally.  In this case, don't do the cross-label
                    optimisations yet, but flag them as a potential possibility
                    for the next iteration of Pass 1 }
                  if not NotFirstIteration then
                    Include(OptsToCheck, aoc_ForceNewIteration);
                end
              else if IsJumpToLabel(taicpu(Next)) then
                TrackJump(JumpTargetOp(taicpu(Next))^.ref^.symbol)
              else
                { Conditional jumps should always be a jump to label }
                InternalError(2022041701);

              CrossJump := True;
              Continue;
            end;

        if Next.typ = ait_label then
          begin
            if not Assigned(JumpTracking) then
              begin
                { Cross-label optimisations often causes other optimisations
                  to perform worse because they're not given the chance to
                  optimise locally.  In this case, don't do the cross-label
                  optimisations yet, but flag them as a potential possibility
                  for the next iteration of Pass 1 }
                if not NotFirstIteration then
                  Include(OptsToCheck, aoc_ForceNewIteration);

              end
            else if LabelAccountedFor(tai_label(Next).labsym) then
              Continue;

            { If we reach here, we're at a label that hasn't been seen before
              (or JumpTracking was nil) }
            Break;
          end;
      until not Result or
            not (cs_opt_level3 in current_settings.optimizerswitches) or
            not (Next.typ in [ait_label, ait_instruction]) or
            RegInInstruction(reg,Next);
    end;


  function TX86AsmOptimizer.GetNextInstructionUsingRegTrackingUse(Current: tai; out Next: tai; reg: TRegister): Boolean;
    begin
      if not(cs_opt_level3 in current_settings.optimizerswitches) then
        begin
          Result:=GetNextInstruction(Current,Next);
          exit;
        end;
      Next:=tai(Current.Next);
      Result:=false;
      while assigned(Next) do
        begin
          if ((Next.typ=ait_instruction) and is_calljmp(taicpu(Next).opcode) and not(taicpu(Next).opcode=A_CALL)) or
            ((Next.typ=ait_regalloc) and (getsupreg(tai_regalloc(Next).reg)=getsupreg(reg))) or
            ((Next.typ=ait_label) and not(labelCanBeSkipped(Tai_Label(Next)))) then
            exit
          else if (Next.typ=ait_instruction) and RegInInstruction(reg,Next) and not(taicpu(Next).opcode=A_CALL) then
            begin
              Result:=true;
              exit;
            end;
          Next:=tai(Next.Next);
        end;
    end;


  function TX86AsmOptimizer.InstructionLoadsFromReg(const reg: TRegister;const hp: tai): boolean;
    begin
      Result:=RegReadByInstruction(reg,hp);
    end;


  class function TX86AsmOptimizer.RegReadByInstruction(reg: TRegister; hp: tai): boolean;
    var
      p: taicpu;
      opcount: longint;
    begin
      RegReadByInstruction := false;
      if hp.typ <> ait_instruction then
        exit;
      p := taicpu(hp);
      case p.opcode of
        A_CALL:
          regreadbyinstruction := true;
        A_IMUL:
          case p.ops of
            1:
              regReadByInstruction := RegInOp(reg,p.oper[0]^) or
                 (
                  ((getregtype(reg)=R_INTREGISTER) and (getsupreg(reg)=RS_EAX)) and
                  ((getsubreg(reg)<>R_SUBH) or (p.opsize<>S_B))
                 );
            2,3:
              regReadByInstruction :=
                reginop(reg,p.oper[0]^) or
                reginop(reg,p.oper[1]^);
            else
              InternalError(2019112801);
          end;
        A_MUL:
          begin
            regReadByInstruction := RegInOp(reg,p.oper[0]^) or
               (
                ((getregtype(reg)=R_INTREGISTER) and (getsupreg(reg)=RS_EAX)) and
                ((getsubreg(reg)<>R_SUBH) or (p.opsize<>S_B))
               );
          end;
        A_IDIV,A_DIV:
          begin
            regReadByInstruction := RegInOp(reg,p.oper[0]^) or
               (
                 (getregtype(reg)=R_INTREGISTER) and
                 (
                   (getsupreg(reg)=RS_EAX) or ((getsupreg(reg)=RS_EDX) and (p.opsize<>S_B))
                 )
               );
          end;
        else
          begin
            if (p.opcode=A_LEA) and is_segment_reg(reg) then
              begin
                RegReadByInstruction := false;
                exit;
              end;
            for opcount := 0 to p.ops-1 do
              if (p.oper[opCount]^.typ = top_ref) and
                 RegInRef(reg,p.oper[opcount]^.ref^) then
                begin
                  RegReadByInstruction := true;
                  exit
                end;
            { special handling for SSE MOVSD }
            if (p.opcode=A_MOVSD) and (p.ops>0) then
              begin
                if p.ops<>2 then
                  internalerror(2017042702);
                regReadByInstruction := reginop(reg,p.oper[0]^) or
                  (
                   (p.oper[1]^.typ=top_reg) and (p.oper[0]^.typ=top_reg) and reginop(reg, p.oper[1]^)
                  );
                exit;
              end;
            with insprop[p.opcode] do
              begin
                case getregtype(reg) of
                  R_INTREGISTER:
                    begin
                      case getsupreg(reg) of
                        RS_EAX:
                          if [Ch_REAX,Ch_RWEAX,Ch_MEAX]*Ch<>[] then
                            begin
                              RegReadByInstruction := true;
                              exit
                            end;
                        RS_ECX:
                          if [Ch_RECX,Ch_RWECX,Ch_MECX]*Ch<>[] then
                            begin
                              RegReadByInstruction := true;
                              exit
                            end;
                        RS_EDX:
                          if [Ch_REDX,Ch_RWEDX,Ch_MEDX]*Ch<>[] then
                            begin
                              RegReadByInstruction := true;
                              exit
                            end;
                        RS_EBX:
                          if [Ch_REBX,Ch_RWEBX,Ch_MEBX]*Ch<>[] then
                            begin
                              RegReadByInstruction := true;
                              exit
                            end;
                        RS_ESP:
                          if [Ch_RESP,Ch_RWESP,Ch_MESP]*Ch<>[] then
                            begin
                              RegReadByInstruction := true;
                              exit
                            end;
                        RS_EBP:
                          if [Ch_REBP,Ch_RWEBP,Ch_MEBP]*Ch<>[] then
                            begin
                              RegReadByInstruction := true;
                              exit
                            end;
                        RS_ESI:
                          if [Ch_RESI,Ch_RWESI,Ch_MESI]*Ch<>[] then
                            begin
                              RegReadByInstruction := true;
                              exit
                            end;
                        RS_EDI:
                          if [Ch_REDI,Ch_RWEDI,Ch_MEDI]*Ch<>[] then
                            begin
                              RegReadByInstruction := true;
                              exit
                            end;
                        end;
                    end;
                  R_MMREGISTER:
                    begin
                      case getsupreg(reg) of
                        RS_XMM0:
                          if [Ch_RXMM0,Ch_RWXMM0,Ch_MXMM0]*Ch<>[] then
                            begin
                              RegReadByInstruction := true;
                              exit
                            end;
                        end;
                    end;
                  else
                    ;
                end;
                if SuperRegistersEqual(reg,NR_DEFAULTFLAGS) then
                  begin
                    if (Ch_RFLAGScc in Ch) and not(getsubreg(reg) in [R_SUBW,R_SUBD,R_SUBQ]) then
                      begin
                        case p.condition of
                          C_A,C_NBE,       { CF=0 and ZF=0  }
                          C_BE,C_NA:       { CF=1 or ZF=1   }
                            RegReadByInstruction:=getsubreg(reg) in [R_SUBFLAGCARRY,R_SUBFLAGZERO];
                          C_AE,C_NB,C_NC,  { CF=0           }
                          C_B,C_NAE,C_C:   { CF=1           }
                            RegReadByInstruction:=getsubreg(reg) in [R_SUBFLAGCARRY];
                          C_NE,C_NZ,       { ZF=0           }
                          C_E,C_Z:         { ZF=1           }
                            RegReadByInstruction:=getsubreg(reg) in [R_SUBFLAGZERO];
                          C_G,C_NLE,       { ZF=0 and SF=OF }
                          C_LE,C_NG:       { ZF=1 or SF<>OF }
                            RegReadByInstruction:=getsubreg(reg) in [R_SUBFLAGZERO,R_SUBFLAGSIGN,R_SUBFLAGOVERFLOW];
                          C_GE,C_NL,       { SF=OF          }
                          C_L,C_NGE:       { SF<>OF         }
                            RegReadByInstruction:=getsubreg(reg) in [R_SUBFLAGSIGN,R_SUBFLAGOVERFLOW];
                          C_NO,            { OF=0           }
                          C_O:             { OF=1           }
                            RegReadByInstruction:=getsubreg(reg) in [R_SUBFLAGOVERFLOW];
                          C_NP,C_PO,       { PF=0           }
                          C_P,C_PE:        { PF=1           }
                            RegReadByInstruction:=getsubreg(reg) in [R_SUBFLAGPARITY];
                          C_NS,            { SF=0           }
                          C_S:             { SF=1           }
                            RegReadByInstruction:=getsubreg(reg) in [R_SUBFLAGSIGN];
                          else
                            internalerror(2017042701);
                        end;
                        if RegReadByInstruction then
                          exit;
                      end;
                    case getsubreg(reg) of
                      R_SUBW,R_SUBD,R_SUBQ:
                        RegReadByInstruction :=
                          [Ch_RCarryFlag,Ch_RParityFlag,Ch_RAuxiliaryFlag,Ch_RZeroFlag,Ch_RSignFlag,Ch_ROverflowFlag,
                           Ch_RWCarryFlag,Ch_RWParityFlag,Ch_RWAuxiliaryFlag,Ch_RWZeroFlag,Ch_RWSignFlag,Ch_RWOverflowFlag,
                           Ch_RDirFlag,Ch_RFlags,Ch_RWFlags,Ch_RFLAGScc]*Ch<>[];
                      R_SUBFLAGCARRY:
                        RegReadByInstruction:=[Ch_RCarryFlag,Ch_RWCarryFlag,Ch_RFlags,Ch_RWFlags]*Ch<>[];
                      R_SUBFLAGPARITY:
                        RegReadByInstruction:=[Ch_RParityFlag,Ch_RWParityFlag,Ch_RFlags,Ch_RWFlags]*Ch<>[];
                      R_SUBFLAGAUXILIARY:
                        RegReadByInstruction:=[Ch_RAuxiliaryFlag,Ch_RWAuxiliaryFlag,Ch_RFlags,Ch_RWFlags]*Ch<>[];
                      R_SUBFLAGZERO:
                        RegReadByInstruction:=[Ch_RZeroFlag,Ch_RWZeroFlag,Ch_RFlags,Ch_RWFlags]*Ch<>[];
                      R_SUBFLAGSIGN:
                        RegReadByInstruction:=[Ch_RSignFlag,Ch_RWSignFlag,Ch_RFlags,Ch_RWFlags]*Ch<>[];
                      R_SUBFLAGOVERFLOW:
                        RegReadByInstruction:=[Ch_ROverflowFlag,Ch_RWOverflowFlag,Ch_RFlags,Ch_RWFlags]*Ch<>[];
                      R_SUBFLAGINTERRUPT:
                        RegReadByInstruction:=[Ch_RFlags,Ch_RWFlags]*Ch<>[];
                      R_SUBFLAGDIRECTION:
                        RegReadByInstruction:=[Ch_RDirFlag,Ch_RFlags,Ch_RWFlags]*Ch<>[];
                      else
                        internalerror(2017042601);
                    end;
                    exit;
                  end;
                if (Ch_NoReadIfEqualRegs in Ch) and (p.ops=2) and
                   (p.oper[0]^.typ=top_reg) and (p.oper[1]^.typ=top_reg) and
                   (p.oper[0]^.reg=p.oper[1]^.reg) then
                  exit;
                if ([CH_RWOP1,CH_ROP1,CH_MOP1]*Ch<>[]) and reginop(reg,p.oper[0]^) then
                  begin
                    RegReadByInstruction := true;
                    exit
                  end;
                if ([Ch_RWOP2,Ch_ROP2,Ch_MOP2]*Ch<>[]) and reginop(reg,p.oper[1]^) then
                  begin
                    RegReadByInstruction := true;
                    exit
                  end;
                if ([Ch_RWOP3,Ch_ROP3,Ch_MOP3]*Ch<>[]) and reginop(reg,p.oper[2]^) then
                  begin
                    RegReadByInstruction := true;
                    exit
                  end;
                if ([Ch_RWOP4,Ch_ROP4,Ch_MOP4]*Ch<>[]) and reginop(reg,p.oper[3]^) then
                  begin
                    RegReadByInstruction := true;
                    exit
                  end;
              end;
          end;
      end;
    end;


  function TX86AsmOptimizer.RegInInstruction(Reg: TRegister; p1: tai): Boolean;
    begin
      result:=false;
      if p1.typ<>ait_instruction then
        exit;

      if (Ch_All in insprop[taicpu(p1).opcode].Ch) then
        exit(true);

      if (getregtype(reg)=R_INTREGISTER) and
        { change information for xmm movsd are not correct }
        ((taicpu(p1).opcode<>A_MOVSD) or (taicpu(p1).ops=0)) then
        begin
          { Handle instructions that behave differently depending on the size and operand count }
          case taicpu(p1).opcode of
            A_MUL, A_DIV, A_IDIV:
              if taicpu(p1).opsize = S_B then
                Result := (getsupreg(Reg) = RS_EAX)
              else
                Result := (getsupreg(Reg) in [RS_EAX, RS_EDX]);

            A_IMUL:
              if taicpu(p1).ops = 1 then
                begin
                  if taicpu(p1).opsize = S_B then
                    Result := (getsupreg(Reg) = RS_EAX)
                  else
                    Result := (getsupreg(Reg) in [RS_EAX, RS_EDX]);
                end;
              { If ops are greater than 1, call inherited method }

            else
              case getsupreg(reg) of
                { RS_EAX = RS_RAX on x86-64 }
                RS_EAX:
                  result:=([Ch_REAX,Ch_RRAX,Ch_WEAX,Ch_WRAX,Ch_RWEAX,Ch_RWRAX,Ch_MEAX,Ch_MRAX]*insprop[taicpu(p1).opcode].Ch)<>[];
                RS_ECX:
                  result:=([Ch_RECX,Ch_RRCX,Ch_WECX,Ch_WRCX,Ch_RWECX,Ch_RWRCX,Ch_MECX,Ch_MRCX]*insprop[taicpu(p1).opcode].Ch)<>[];
                RS_EDX:
                  result:=([Ch_REDX,Ch_RRDX,Ch_WEDX,Ch_WRDX,Ch_RWEDX,Ch_RWRDX,Ch_MEDX,Ch_MRDX]*insprop[taicpu(p1).opcode].Ch)<>[];
                RS_EBX:
                  result:=([Ch_REBX,Ch_RRBX,Ch_WEBX,Ch_WRBX,Ch_RWEBX,Ch_RWRBX,Ch_MEBX,Ch_MRBX]*insprop[taicpu(p1).opcode].Ch)<>[];
                RS_ESP:
                  result:=([Ch_RESP,Ch_RRSP,Ch_WESP,Ch_WRSP,Ch_RWESP,Ch_RWRSP,Ch_MESP,Ch_MRSP]*insprop[taicpu(p1).opcode].Ch)<>[];
                RS_EBP:
                  result:=([Ch_REBP,Ch_RRBP,Ch_WEBP,Ch_WRBP,Ch_RWEBP,Ch_RWRBP,Ch_MEBP,Ch_MRBP]*insprop[taicpu(p1).opcode].Ch)<>[];
                RS_ESI:
                  result:=([Ch_RESI,Ch_RRSI,Ch_WESI,Ch_WRSI,Ch_RWESI,Ch_RWRSI,Ch_MESI,Ch_MRSI,Ch_RMemEDI]*insprop[taicpu(p1).opcode].Ch)<>[];
                RS_EDI:
                  result:=([Ch_REDI,Ch_RRDI,Ch_WEDI,Ch_WRDI,Ch_RWEDI,Ch_RWRDI,Ch_MEDI,Ch_MRDI,Ch_WMemEDI]*insprop[taicpu(p1).opcode].Ch)<>[];
                else
                  ;
              end;
          end;

          if result then
            exit;
        end
      else if getregtype(reg)=R_MMREGISTER then
        begin
          case getsupreg(reg) of
            RS_XMM0:
              result:=([Ch_RXMM0,Ch_WXMM0,Ch_RWXMM0,Ch_MXMM0]*insprop[taicpu(p1).opcode].Ch)<>[];
            else
              ;
          end;
          if result then
            exit;
        end
      else if SuperRegistersEqual(reg,NR_DEFAULTFLAGS) then
        begin
          if ([Ch_RFlags,Ch_WFlags,Ch_RWFlags,Ch_RFLAGScc]*insprop[taicpu(p1).opcode].Ch)<>[] then
            exit(true);
          case getsubreg(reg) of
            R_SUBFLAGCARRY:
              Result:=([Ch_RCarryFlag,Ch_RWCarryFlag,Ch_W0CarryFlag,Ch_W1CarryFlag,Ch_WCarryFlag,Ch_WUCarryFlag]*insprop[taicpu(p1).opcode].Ch)<>[];
            R_SUBFLAGPARITY:
              Result:=([Ch_RParityFlag,Ch_RWParityFlag,Ch_W0ParityFlag,Ch_W1ParityFlag,Ch_WParityFlag,Ch_WUParityFlag]*insprop[taicpu(p1).opcode].Ch)<>[];
            R_SUBFLAGAUXILIARY:
              Result:=([Ch_RAuxiliaryFlag,Ch_RWAuxiliaryFlag,Ch_W0AuxiliaryFlag,Ch_W1AuxiliaryFlag,Ch_WAuxiliaryFlag,Ch_WUAuxiliaryFlag]*insprop[taicpu(p1).opcode].Ch)<>[];
            R_SUBFLAGZERO:
              Result:=([Ch_RZeroFlag,Ch_RWZeroFlag,Ch_W0ZeroFlag,Ch_W1ZeroFlag,Ch_WZeroFlag,Ch_WUZeroFlag]*insprop[taicpu(p1).opcode].Ch)<>[];
            R_SUBFLAGSIGN:
              Result:=([Ch_RSignFlag,Ch_RWSignFlag,Ch_W0SignFlag,Ch_W1SignFlag,Ch_WSignFlag,Ch_WUSignFlag]*insprop[taicpu(p1).opcode].Ch)<>[];
            R_SUBFLAGOVERFLOW:
              Result:=([Ch_ROverflowFlag,Ch_RWOverflowFlag,Ch_W0OverflowFlag,Ch_W1OverflowFlag,Ch_WOverflowFlag,Ch_WUOverflowFlag]*insprop[taicpu(p1).opcode].Ch)<>[];
            R_SUBFLAGINTERRUPT:
              Result:=([Ch_W0IntFlag,Ch_W1IntFlag,Ch_WFlags]*insprop[taicpu(p1).opcode].Ch)<>[];
            R_SUBFLAGDIRECTION:
              Result:=([Ch_RDirFlag,Ch_W0DirFlag,Ch_W1DirFlag,Ch_WFlags]*insprop[taicpu(p1).opcode].Ch)<>[];
            R_SUBW,R_SUBD,R_SUBQ:
              { Everything except the direction bits }
              Result:=
                ([Ch_RCarryFlag,Ch_RParityFlag,Ch_RAuxiliaryFlag,Ch_RZeroFlag,Ch_RSignFlag,Ch_ROverflowFlag,
                Ch_WCarryFlag,Ch_WParityFlag,Ch_WAuxiliaryFlag,Ch_WZeroFlag,Ch_WSignFlag,Ch_WOverflowFlag,
                Ch_W0CarryFlag,Ch_W0ParityFlag,Ch_W0AuxiliaryFlag,Ch_W0ZeroFlag,Ch_W0SignFlag,Ch_W0OverflowFlag,
                Ch_W1CarryFlag,Ch_W1ParityFlag,Ch_W1AuxiliaryFlag,Ch_W1ZeroFlag,Ch_W1SignFlag,Ch_W1OverflowFlag,
                Ch_WUCarryFlag,Ch_WUParityFlag,Ch_WUAuxiliaryFlag,Ch_WUZeroFlag,Ch_WUSignFlag,Ch_WUOverflowFlag,
                Ch_RWCarryFlag,Ch_RWParityFlag,Ch_RWAuxiliaryFlag,Ch_RWZeroFlag,Ch_RWSignFlag,Ch_RWOverflowFlag
                ]*insprop[taicpu(p1).opcode].Ch)<>[];
            else
              ;
          end;
          if result then
            exit;
        end
      else if (getregtype(reg)=R_FPUREGISTER) and (Ch_FPU in insprop[taicpu(p1).opcode].Ch) then
        exit(true);
      Result:=inherited RegInInstruction(Reg, p1);
    end;


    function TX86AsmOptimizer.RegModifiedByInstruction(Reg: TRegister; p1: tai): boolean;
      const
        WriteOps: array[0..3] of set of TInsChange =
          ([CH_RWOP1,CH_WOP1,CH_MOP1],
           [Ch_RWOP2,Ch_WOP2,Ch_MOP2],
           [Ch_RWOP3,Ch_WOP3,Ch_MOP3],
           [Ch_RWOP4,Ch_WOP4,Ch_MOP4]);
      var
        OperIdx: Integer;
      begin
        Result := False;
        if p1.typ <> ait_instruction then
          exit;

        with insprop[taicpu(p1).opcode] do
          if SuperRegistersEqual(reg,NR_DEFAULTFLAGS) then
            begin
              case getsubreg(reg) of
                R_SUBW,R_SUBD,R_SUBQ:
                  Result :=
                    [Ch_WCarryFlag,Ch_WParityFlag,Ch_WAuxiliaryFlag,Ch_WZeroFlag,Ch_WSignFlag,Ch_WOverflowFlag,
                     Ch_W0CarryFlag,Ch_W0ParityFlag,Ch_W0AuxiliaryFlag,Ch_W0ZeroFlag,Ch_W0SignFlag,Ch_W0OverflowFlag,
                     Ch_W1CarryFlag,Ch_W1ParityFlag,Ch_W1AuxiliaryFlag,Ch_W1ZeroFlag,Ch_W1SignFlag,Ch_W1OverflowFlag,
                     Ch_WUCarryFlag,Ch_WUParityFlag,Ch_WUAuxiliaryFlag,Ch_WUZeroFlag,Ch_WUSignFlag,Ch_WUOverflowFlag,
                     Ch_RWCarryFlag,Ch_RWParityFlag,Ch_RWAuxiliaryFlag,Ch_RWZeroFlag,Ch_RWSignFlag,Ch_RWOverflowFlag,
                     Ch_W0DirFlag,Ch_W1DirFlag,Ch_W0IntFlag,Ch_W1IntFlag,Ch_WFlags,Ch_RWFlags]*Ch<>[];
                R_SUBFLAGCARRY:
                  Result:=[Ch_WCarryFlag,Ch_W0CarryFlag,Ch_W1CarryFlag,Ch_WUCarryFlag,Ch_RWCarryFlag,Ch_WFlags,Ch_RWFlags]*Ch<>[];
                R_SUBFLAGPARITY:
                  Result:=[Ch_WParityFlag,Ch_W0ParityFlag,Ch_W1ParityFlag,Ch_WUParityFlag,Ch_RWParityFlag,Ch_WFlags,Ch_RWFlags]*Ch<>[];
                R_SUBFLAGAUXILIARY:
                  Result:=[Ch_WAuxiliaryFlag,Ch_W0AuxiliaryFlag,Ch_W1AuxiliaryFlag,Ch_WUAuxiliaryFlag,Ch_RWAuxiliaryFlag,Ch_WFlags,Ch_RWFlags]*Ch<>[];
                R_SUBFLAGZERO:
                  Result:=[Ch_WZeroFlag,Ch_W0ZeroFlag,Ch_W1ZeroFlag,Ch_WUZeroFlag,Ch_RWZeroFlag,Ch_WFlags,Ch_RWFlags]*Ch<>[];
                R_SUBFLAGSIGN:
                  Result:=[Ch_WSignFlag,Ch_W0SignFlag,Ch_W1SignFlag,Ch_WUSignFlag,Ch_RWSignFlag,Ch_WFlags,Ch_RWFlags]*Ch<>[];
                R_SUBFLAGOVERFLOW:
                  Result:=[Ch_WOverflowFlag,Ch_W0OverflowFlag,Ch_W1OverflowFlag,Ch_WUOverflowFlag,Ch_RWOverflowFlag,Ch_WFlags,Ch_RWFlags]*Ch<>[];
                R_SUBFLAGINTERRUPT:
                  Result:=[Ch_W0IntFlag,Ch_W1IntFlag,Ch_WFlags,Ch_RWFlags]*Ch<>[];
                R_SUBFLAGDIRECTION:
                  Result:=[Ch_W0DirFlag,Ch_W1DirFlag,Ch_WFlags,Ch_RWFlags]*Ch<>[];
                else
                  internalerror(2017042602);
              end;
              exit;
            end;

        case taicpu(p1).opcode of
          A_CALL:
            { We could potentially set Result to False if the register in
              question is non-volatile for the subroutine's calling convention,
              but this would require detecting the calling convention in use and
              also assuming that the routine doesn't contain malformed assembly
              language, for example... so it could only be done under -O4 as it
              would be considered a side-effect. [Kit] }
            Result := True;
          A_MOVSD:
            { special handling for SSE MOVSD }
            if (taicpu(p1).ops>0) then
              begin
                if taicpu(p1).ops<>2 then
                  internalerror(2017042703);
                Result := (taicpu(p1).oper[1]^.typ=top_reg) and RegInOp(reg,taicpu(p1).oper[1]^);
              end;
          {  VMOVSS and VMOVSD has two and three operand flavours, this cannot modelled by x86ins.dat
             so fix it here (FK)
          }
          A_VMOVSS,
          A_VMOVSD:
            begin
              Result := (taicpu(p1).ops=3) and (taicpu(p1).oper[2]^.typ=top_reg) and RegInOp(reg,taicpu(p1).oper[2]^);
              exit;
            end;
          A_MUL, A_DIV, A_IDIV:
            begin
              if taicpu(p1).opsize = S_B then
                Result := (getsupreg(Reg) = RS_EAX)
              else
                Result := (getsupreg(Reg) in [RS_EAX, RS_EDX]);
            end;
          A_IMUL:
            begin
              if taicpu(p1).ops = 1 then
                begin
                  Result := (getsupreg(Reg) in [RS_EAX, RS_EDX]);
                end
              else
                Result := (taicpu(p1).oper[taicpu(p1).ops-1]^.typ=top_reg) and RegInOp(reg,taicpu(p1).oper[taicpu(p1).ops-1]^);

              Exit;
            end;
          else
            ;
        end;
        if Result then
          exit;
        with insprop[taicpu(p1).opcode] do
          begin
            if getregtype(reg)=R_INTREGISTER then
              begin
                case getsupreg(reg) of
                  RS_EAX:
                    if [Ch_WEAX,Ch_RWEAX,Ch_MEAX,Ch_WRAX,Ch_RWRAX,Ch_MRAX]*Ch<>[] then
                      begin
                        Result := True;
                        exit
                      end;
                  RS_ECX:
                    if [Ch_WECX,Ch_RWECX,Ch_MECX,Ch_WRCX,Ch_RWRCX,Ch_MRCX]*Ch<>[] then
                      begin
                        Result := True;
                        exit
                      end;
                  RS_EDX:
                    if [Ch_WEDX,Ch_RWEDX,Ch_MEDX,Ch_WRDX,Ch_RWRDX,Ch_MRDX]*Ch<>[] then
                      begin
                        Result := True;
                        exit
                      end;
                  RS_EBX:
                    if [Ch_WEBX,Ch_RWEBX,Ch_MEBX,Ch_WRBX,Ch_RWRBX,Ch_MRBX]*Ch<>[] then
                      begin
                        Result := True;
                        exit
                      end;
                  RS_ESP:
                    if [Ch_WESP,Ch_RWESP,Ch_MESP,Ch_WRSP,Ch_RWRSP,Ch_MRSP]*Ch<>[] then
                      begin
                        Result := True;
                        exit
                      end;
                  RS_EBP:
                    if [Ch_WEBP,Ch_RWEBP,Ch_MEBP,Ch_WRBP,Ch_RWRBP,Ch_MRBP]*Ch<>[] then
                      begin
                        Result := True;
                        exit
                      end;
                  RS_ESI:
                    if [Ch_WESI,Ch_RWESI,Ch_MESI,Ch_WRSI,Ch_RWRSI,Ch_MRSI]*Ch<>[] then
                      begin
                        Result := True;
                        exit
                      end;
                  RS_EDI:
                    if [Ch_WEDI,Ch_RWEDI,Ch_MEDI,Ch_WRDI,Ch_RWRDI,Ch_MRDI]*Ch<>[] then
                      begin
                        Result := True;
                        exit
                      end;
                end;
              end;

            for OperIdx := 0 to taicpu(p1).ops - 1 do
              if (WriteOps[OperIdx]*Ch<>[]) and
                { The register doesn't get modified inside a reference }
                (taicpu(p1).oper[OperIdx]^.typ = top_reg) and
                SuperRegistersEqual(reg,taicpu(p1).oper[OperIdx]^.reg) then
                begin
                  Result := true;
                  exit
                end;
          end;
      end;


{$ifdef DEBUG_AOPTCPU}
    procedure TX86AsmOptimizer.DebugMsg(const s: string;p : tai);
      begin
        asml.insertbefore(tai_comment.Create(strpnew(s)), p);
      end;

    function debug_tostr(i: tcgint): string; inline;
      begin
        Result := tostr(i);
      end;

    function debug_hexstr(i: tcgint): string;
      begin
        Result := '0x';
        case i of
          0..$FF:
            Result := Result + hexstr(i, 2);
          $100..$FFFF:
            Result := Result + hexstr(i, 4);
          $10000..$FFFFFF:
            Result := Result + hexstr(i, 6);
          $1000000..$FFFFFFFF:
            Result := Result + hexstr(i, 8);
          else
            Result := Result + hexstr(i, 16);
        end;
      end;

    function debug_regname(r: TRegister): string; inline;
      begin
        Result := '%' + std_regname(r);
      end;

    { Debug output function - creates a string representation of an operator }
    function debug_operstr(oper: TOper): string;
      begin
        case oper.typ of
          top_const:
            Result := '$' + debug_tostr(oper.val);
          top_reg:
            Result := debug_regname(oper.reg);
          top_ref:
            begin
              if oper.ref^.offset <> 0 then
                Result := debug_tostr(oper.ref^.offset) + '('
              else
                Result := '(';

              if (oper.ref^.base <> NR_INVALID) and (oper.ref^.base <> NR_NO) then
                begin
                  Result := Result + debug_regname(oper.ref^.base);
                  if (oper.ref^.index <> NR_INVALID) and (oper.ref^.index <> NR_NO) then
                    Result := Result + ',' + debug_regname(oper.ref^.index);
                end
              else
                if (oper.ref^.index <> NR_INVALID) and (oper.ref^.index <> NR_NO) then
                  Result := Result + debug_regname(oper.ref^.index);

              if (oper.ref^.scalefactor > 1) then
                Result := Result + ',' + debug_tostr(oper.ref^.scalefactor) + ')'
              else
                Result := Result + ')';
            end;
          else
            Result := '[UNKNOWN]';
        end;
      end;

    function debug_op2str(opcode: tasmop): string; inline;
      begin
        Result := std_op2str[opcode];
      end;

    function debug_opsize2str(opsize: topsize): string; inline;
      begin
        Result := gas_opsize2str[opsize];
      end;

{$else DEBUG_AOPTCPU}
    procedure TX86AsmOptimizer.DebugMsg(const s: string;p : tai);inline;
      begin
      end;

    function debug_tostr(i: tcgint): string; inline;
      begin
        Result := '';
      end;

    function debug_hexstr(i: tcgint): string; inline;
      begin
        Result := '';
      end;

    function debug_regname(r: TRegister): string; inline;
      begin
        Result := '';
      end;

    function debug_operstr(oper: TOper): string; inline;
      begin
        Result := '';
      end;

    function debug_op2str(opcode: tasmop): string; inline;
      begin
        Result := '';
      end;

    function debug_opsize2str(opsize: topsize): string; inline;
      begin
        Result := '';
      end;
{$endif DEBUG_AOPTCPU}

    class function TX86AsmOptimizer.IsMOVZXAcceptable: Boolean; inline;
      begin
{$ifdef x86_64}
        { Always fine on x86-64 }
        Result := True;
{$else x86_64}
        Result :=
{$ifdef i8086}
          (current_settings.cputype >= cpu_386) and
{$endif i8086}
          (
            { Always accept if optimising for size }
            (cs_opt_size in current_settings.optimizerswitches) or
            { From the Pentium II onwards, MOVZX only takes 1 cycle. [Kit] }
            (current_settings.optimizecputype >= cpu_Pentium2)
          );
{$endif x86_64}
      end;


    { Attempts to allocate a volatile integer register for use between p and hp,
      using AUsedRegs for the current register usage information.  Returns NR_NO
      if no free register could be found }
    function TX86AsmOptimizer.GetIntRegisterBetween(RegSize: TSubRegister; var AUsedRegs: TAllUsedRegs; p, hp: tai; DontAlloc: Boolean = False): TRegister;
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
{$if defined(i386) or defined(i8086)}
              { If the target size is 8-bit, make sure we can actually encode it }
              and (
                (RegSize >= R_SUBW) or { Not R_SUBL or R_SUBH }
                (GetSupReg(CurrentReg) in [RS_EAX,RS_EBX,RS_ECX,RS_EDX])
              )
{$endif i386 or i8086}
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
                          if is_calljmpuncondret(taicpu(Currentp).opcode) then
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


    { Attempts to allocate a volatile MM register for use between p and hp,
      using AUsedRegs for the current register usage information.  Returns NR_NO
      if no free register could be found }
    function TX86AsmOptimizer.GetMMRegisterBetween(RegSize: TSubRegister; var AUsedRegs: TAllUsedRegs; p, hp: tai; DontAlloc: Boolean = False): TRegister;
      var
        RegSet: TCPURegisterSet;
        CurrentSuperReg: Integer;
        CurrentReg: TRegister;
        Currentp: tai;
        Breakout: Boolean;
      begin
        Result := NR_NO;
        RegSet :=
          paramanager.get_volatile_registers_mm(current_procinfo.procdef.proccalloption) +
          current_procinfo.saved_regs_mm;

        for CurrentSuperReg in RegSet do
          begin
            CurrentReg := newreg(R_MMREGISTER, TSuperRegister(CurrentSuperReg), RegSize);
            if not AUsedRegs[R_MMREGISTER].IsUsed(CurrentReg) then
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
                          if is_calljmpuncondret(taicpu(Currentp).opcode) then
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


    class function TX86AsmOptimizer.Reg1WriteOverwritesReg2Entirely(reg1, reg2: tregister): boolean;
      begin
        if not SuperRegistersEqual(reg1,reg2) then
          exit(false);
        if getregtype(reg1)<>R_INTREGISTER then
          exit(true);  {because SuperRegisterEqual is true}
        case getsubreg(reg1) of
          { A write to R_SUBL doesn't change R_SUBH and if reg2 is R_SUBW or
            higher, it preserves the high bits, so the new value depends on
            reg2's previous value. In other words, it is equivalent to doing:

            reg2 := (reg2 and $ffffff00) or byte(reg1); }
          R_SUBL:
            exit(getsubreg(reg2)=R_SUBL);
          { A write to R_SUBH doesn't change R_SUBL and if reg2 is R_SUBW or
            higher, it actually does a:

            reg2 := (reg2 and $ffff00ff) or (reg1 and $ff00); }
          R_SUBH:
            exit(getsubreg(reg2)=R_SUBH);
          { If reg2 is R_SUBD or larger, a write to R_SUBW preserves the high 16
            bits of reg2:

            reg2 := (reg2 and $ffff0000) or word(reg1); }
          R_SUBW:
            exit(getsubreg(reg2) in [R_SUBL,R_SUBH,R_SUBW]);
          { a write to R_SUBD always overwrites every other subregister,
            because it clears the high 32 bits of R_SUBQ on x86_64 }
          R_SUBD,
          R_SUBQ:
            exit(true);
          else
            internalerror(2017042801);
        end;
      end;


    class function TX86AsmOptimizer.Reg1ReadDependsOnReg2(reg1, reg2: tregister): boolean;
      begin
        if not SuperRegistersEqual(reg1,reg2) then
          exit(false);
        if getregtype(reg1)<>R_INTREGISTER then
          exit(true);  {because SuperRegisterEqual is true}
        case getsubreg(reg1) of
          R_SUBL:
            exit(getsubreg(reg2)<>R_SUBH);
          R_SUBH:
            exit(getsubreg(reg2)<>R_SUBL);
          R_SUBW,
          R_SUBD,
          R_SUBQ:
            exit(true);
          else
            internalerror(2017042802);
        end;
      end;


    function TX86AsmOptimizer.PrePeepholeOptSxx(var p : tai) : boolean;
      var
        hp1 : tai;
        l : TCGInt;
      begin
        result:=false;
        if not(GetNextInstruction(p, hp1)) then
          exit;
        { changes the code sequence
          shr/sar const1, x
          shl     const2, x

          to

          either "sar/and", "shl/and" or just "and" depending on const1 and const2 }
        if (taicpu(p).oper[0]^.typ = top_const) and
          MatchInstruction(hp1,A_SHL,[]) and
          (taicpu(hp1).oper[0]^.typ = top_const) and
          (taicpu(hp1).opsize = taicpu(p).opsize) and
          (taicpu(hp1).oper[1]^.typ = taicpu(p).oper[1]^.typ) and
          OpsEqual(taicpu(hp1).oper[1]^, taicpu(p).oper[1]^) then
          begin
            if (taicpu(p).oper[0]^.val > taicpu(hp1).oper[0]^.val) and
              not(cs_opt_size in current_settings.optimizerswitches) then
              begin
                { shr/sar const1, %reg
                  shl     const2, %reg
                  with const1 > const2 }
                DebugMsg(SPeepholeOptimization + 'SxrShl2SxrAnd 1 done',p);
                taicpu(p).loadConst(0,taicpu(p).oper[0]^.val-taicpu(hp1).oper[0]^.val);
                taicpu(hp1).opcode := A_AND;
                l := (1 shl (taicpu(hp1).oper[0]^.val)) - 1;
                case taicpu(p).opsize Of
                  S_B: taicpu(hp1).loadConst(0,l Xor $ff);
                  S_W: taicpu(hp1).loadConst(0,l Xor $ffff);
                  S_L: taicpu(hp1).loadConst(0,l Xor tcgint($ffffffff));
                  S_Q: taicpu(hp1).loadConst(0,l Xor tcgint($ffffffffffffffff));
                  else
                    Internalerror(2017050703)
                end;
              end
            else if (taicpu(p).oper[0]^.val<taicpu(hp1).oper[0]^.val) and
              not(cs_opt_size in current_settings.optimizerswitches) then
              begin
                { shr/sar const1, %reg
                  shl     const2, %reg
                  with const1 < const2 }
                DebugMsg(SPeepholeOptimization + 'SxrShl2SxrAnd 2 done',p);
                taicpu(hp1).loadConst(0,taicpu(hp1).oper[0]^.val-taicpu(p).oper[0]^.val);
                taicpu(p).opcode := A_AND;
                l := (1 shl (taicpu(p).oper[0]^.val))-1;
                case taicpu(p).opsize Of
                  S_B: taicpu(p).loadConst(0,l Xor $ff);
                  S_W: taicpu(p).loadConst(0,l Xor $ffff);
                  S_L: taicpu(p).loadConst(0,l Xor tcgint($ffffffff));
                  S_Q: taicpu(p).loadConst(0,l Xor tcgint($ffffffffffffffff));
                  else
                    Internalerror(2017050702)
                end;
              end
            else if (taicpu(p).oper[0]^.val = taicpu(hp1).oper[0]^.val) then
              begin
                { shr/sar const1, %reg
                  shl     const2, %reg
                  with const1 = const2 }
                DebugMsg(SPeepholeOptimization + 'SxrShl2And done',p);
                taicpu(p).opcode := A_AND;
                l := (1 shl (taicpu(p).oper[0]^.val))-1;
                case taicpu(p).opsize Of
                  S_B: taicpu(p).loadConst(0,l Xor $ff);
                  S_W: taicpu(p).loadConst(0,l Xor $ffff);
                  S_L: taicpu(p).loadConst(0,l Xor tcgint($ffffffff));
                  S_Q: taicpu(p).loadConst(0,l Xor tcgint($ffffffffffffffff));
                  else
                    Internalerror(2017050701)
                end;
                RemoveInstruction(hp1);
              end;
          end;
      end;


    function TX86AsmOptimizer.PrePeepholeOptIMUL(var p : tai) : boolean;
      var
        opsize : topsize;
        hp1, hp2 : tai;
        tmpref : treference;
        ShiftValue : Cardinal;
        BaseValue : TCGInt;
      begin
        result:=false;
        opsize:=taicpu(p).opsize;
        { changes certain "imul const, %reg"'s to lea sequences }
        if (MatchOpType(taicpu(p),top_const,top_reg) or
            MatchOpType(taicpu(p),top_const,top_reg,top_reg)) and
           (opsize in [S_L{$ifdef x86_64},S_Q{$endif x86_64}]) then
          if (taicpu(p).oper[0]^.val = 1) then
            if (taicpu(p).ops = 2) then
             { remove "imul $1, reg" }
              begin
                DebugMsg(SPeepholeOptimization + 'Imul2Nop done',p);
                Result := RemoveCurrentP(p);
              end
            else
             { change "imul $1, reg1, reg2" to "mov reg1, reg2" }
              begin
                hp1 := taicpu.Op_Reg_Reg(A_MOV, opsize, taicpu(p).oper[1]^.reg,taicpu(p).oper[2]^.reg);
                taicpu(hp1).fileinfo := taicpu(p).fileinfo;
                asml.InsertAfter(hp1, p);
                DebugMsg(SPeepholeOptimization + 'Imul2Mov done',p);
                RemoveCurrentP(p, hp1);
                Result := True;
              end
          else if ((taicpu(p).ops <= 2) or
              (taicpu(p).oper[2]^.typ = Top_Reg)) and
           not(cs_opt_size in current_settings.optimizerswitches) and
           (not(GetNextInstruction(p, hp1)) or
             not((tai(hp1).typ = ait_instruction) and
                 ((taicpu(hp1).opcode=A_Jcc) and
                  (taicpu(hp1).condition in [C_O,C_NO])))) then
            begin
              {
                imul X, reg1, reg2 to
                  lea (reg1,reg1,Y), reg2
                  shl ZZ,reg2
                imul XX, reg1 to
                  lea (reg1,reg1,YY), reg1
                  shl ZZ,reg2

                This optimziation makes sense for pretty much every x86, except the VIA Nano3000: it has IMUL latency 2, lea/shl pair as well,
                it does not exist as a separate optimization target in FPC though.

                This optimziation can be applied as long as only two bits are set in the constant and those two bits are separated by
                at most two zeros
              }
              reference_reset(tmpref,1,[]);
              if (PopCnt(QWord(taicpu(p).oper[0]^.val))=2) and (BsrQWord(taicpu(p).oper[0]^.val)-BsfQWord(taicpu(p).oper[0]^.val)<=3) then
                begin
                  ShiftValue:=BsfQWord(taicpu(p).oper[0]^.val);
                  BaseValue:=taicpu(p).oper[0]^.val shr ShiftValue;
                  TmpRef.base := taicpu(p).oper[1]^.reg;
                  TmpRef.index := taicpu(p).oper[1]^.reg;
                  if not(BaseValue in [3,5,9]) then
                    Internalerror(2018110101);
                  TmpRef.ScaleFactor := BaseValue-1;
                  if (taicpu(p).ops = 2) then
                    hp1 := taicpu.op_ref_reg(A_LEA, opsize, TmpRef, taicpu(p).oper[1]^.reg)
                  else
                    hp1 := taicpu.op_ref_reg(A_LEA, opsize, TmpRef, taicpu(p).oper[2]^.reg);
                  AsmL.InsertAfter(hp1,p);
                  DebugMsg(SPeepholeOptimization + 'Imul2LeaShl done',p);
                  taicpu(hp1).fileinfo:=taicpu(p).fileinfo;
                  RemoveCurrentP(p, hp1);
                  if ShiftValue>0 then
                    begin
                      hp2 := taicpu.op_const_reg(A_SHL, opsize, ShiftValue, taicpu(hp1).oper[1]^.reg);
                      AsmL.InsertAfter(hp2,hp1);
                      taicpu(hp2).fileinfo:=taicpu(hp1).fileinfo;
                    end;
                  Result := True;
                end;
            end;
      end;


    function TX86AsmOptimizer.PrePeepholeOptAND(var p : tai) : boolean;
      begin
        Result := False;
        if MatchOperand(taicpu(p).oper[0]^, 0) and
          not RegInUsedRegs(NR_DEFAULTFLAGS, UsedRegs) then
          begin
            DebugMsg(SPeepholeOptimization + 'AND 0 -> MOV 0', p);
            taicpu(p).opcode := A_MOV;
            Result := True;
          end;
      end;


    function TX86AsmOptimizer.RegLoadedWithNewValue(reg: tregister; hp: tai): boolean;
      var
        p: taicpu absolute hp; { Implicit typecast }
        i: Integer;
      begin
        Result := False;
        if not assigned(hp) or
           (hp.typ <> ait_instruction) then
         Exit;

        Prefetch(insprop[p.opcode]);
        if SuperRegistersEqual(reg,NR_DEFAULTFLAGS) then
          with insprop[p.opcode] do
            begin
              case getsubreg(reg) of
                R_SUBW,R_SUBD,R_SUBQ:
                  Result:=
                    { ZF, CF, OF, SF, PF and AF must all be set in some way (ordered so the most
                      uncommon flags are checked first }
                    ([Ch_W0AuxiliaryFlag,Ch_W1AuxiliaryFlag,Ch_WAuxiliaryFlag,Ch_WUAuxiliaryFlag,Ch_WFlags] * Ch <> []) and
                    ([Ch_W0ParityFlag,Ch_W1ParityFlag,Ch_WParityFlag,Ch_WUParityFlag,Ch_WFlags]*Ch <> []) and
                    ([Ch_W0SignFlag,Ch_W1SignFlag,Ch_WSignFlag,Ch_WUSignFlag,Ch_WFlags]*Ch <> []) and
                    ([Ch_W0OverflowFlag,Ch_W1OverflowFlag,Ch_WOverflowFlag,Ch_WUOverflowFlag,Ch_WFlags]*Ch <> []) and
                    ([Ch_W0CarryFlag,Ch_W1CarryFlag,Ch_WCarryFlag,Ch_WUCarryFlag,Ch_WFlags]*Ch <> []) and
                    ([Ch_W0ZeroFlag,Ch_W1ZeroFlag,Ch_WZeroFlag,Ch_WUZeroFlag,Ch_WFlags]*Ch <> []);
                R_SUBFLAGCARRY:
                  Result:=[Ch_W0CarryFlag,Ch_W1CarryFlag,Ch_WCarryFlag,Ch_WUCarryFlag,Ch_WFlags]*Ch<>[];
                R_SUBFLAGPARITY:
                  Result:=[Ch_W0ParityFlag,Ch_W1ParityFlag,Ch_WParityFlag,Ch_WUParityFlag,Ch_WFlags]*Ch<>[];
                R_SUBFLAGAUXILIARY:
                  Result:=[Ch_W0AuxiliaryFlag,Ch_W1AuxiliaryFlag,Ch_WAuxiliaryFlag,Ch_WUAuxiliaryFlag,Ch_WFlags]*Ch<>[];
                R_SUBFLAGZERO:
                  Result:=[Ch_W0ZeroFlag,Ch_W1ZeroFlag,Ch_WZeroFlag,Ch_WUZeroFlag,Ch_WFlags]*Ch<>[];
                R_SUBFLAGSIGN:
                  Result:=[Ch_W0SignFlag,Ch_W1SignFlag,Ch_WSignFlag,Ch_WUSignFlag,Ch_WFlags]*Ch<>[];
                R_SUBFLAGOVERFLOW:
                  Result:=[Ch_W0OverflowFlag,Ch_W1OverflowFlag,Ch_WOverflowFlag,Ch_WUOverflowFlag,Ch_WFlags]*Ch<>[];
                R_SUBFLAGINTERRUPT:
                  Result:=[Ch_W0IntFlag,Ch_W1IntFlag,Ch_WFlags]*Ch<>[];
                R_SUBFLAGDIRECTION:
                  Result:=[Ch_W0DirFlag,Ch_W1DirFlag,Ch_WFlags]*Ch<>[];
                else
                  internalerror(2017050501);
              end;
              exit;
            end;

        { Handle special cases first }
        case p.opcode of
          A_MOV, A_MOVZX, A_MOVSX, A_LEA, A_VMOVSS, A_VMOVSD, A_VMOVAPD,
          A_VMOVAPS, A_VMOVQ, A_MOVSS, A_MOVSD, A_MOVQ, A_MOVAPD, A_MOVAPS:
            begin
              Result :=
              (p.ops=2) and { A_MOVSD can have zero operands, so this check is needed }
              (p.oper[1]^.typ = top_reg) and
               (Reg1WriteOverwritesReg2Entirely(p.oper[1]^.reg,reg)) and
               (
                 (p.oper[0]^.typ = top_const) or
                 (
                   (p.oper[0]^.typ = top_reg) and
                   not(Reg1ReadDependsOnReg2(p.oper[0]^.reg,reg))
                 ) or (
                   (p.oper[0]^.typ = top_ref) and
                   not RegInRef(reg,p.oper[0]^.ref^)
                 )
               );
            end;

          A_MUL, A_IMUL:
            Result :=
              (
                (p.ops=3) and { IMUL only }
                (Reg1WriteOverwritesReg2Entirely(p.oper[2]^.reg,reg)) and
                (
                  (
                    (p.oper[1]^.typ=top_reg) and
                    not Reg1ReadDependsOnReg2(p.oper[1]^.reg,reg)
                  ) or (
                    (p.oper[1]^.typ=top_ref) and
                    not RegInRef(reg,p.oper[1]^.ref^)
                  )
                )
              ) or (
                (
                  (p.ops=1) and
                  (
                    (
                      (
                        (p.oper[0]^.typ=top_reg) and
                        not Reg1ReadDependsOnReg2(p.oper[0]^.reg,reg)
                      )
                    ) or (
                      (p.oper[0]^.typ=top_ref) and
                      not RegInRef(reg,p.oper[0]^.ref^)
                    )
                  ) and (
                    (
                      (p.opsize=S_B) and
                      Reg1WriteOverwritesReg2Entirely(NR_AX,reg) and
                      not Reg1ReadDependsOnReg2(NR_AL,reg)
                    ) or (
                      (p.opsize=S_W) and
                      Reg1WriteOverwritesReg2Entirely(NR_DX,reg)
                    ) or (
                      (p.opsize=S_L) and
                      Reg1WriteOverwritesReg2Entirely(NR_EDX,reg)
{$ifdef x86_64}
                    ) or (
                      (p.opsize=S_Q) and
                      Reg1WriteOverwritesReg2Entirely(NR_RDX,reg)
{$endif x86_64}
                    )
                  )
                )
              );

          A_CBW:
            Result := Reg1WriteOverwritesReg2Entirely(NR_AX,reg) and not(Reg1ReadDependsOnReg2(NR_AL,reg));
{$ifndef x86_64}
          A_LDS:
            Result := (reg=NR_DS) and not(RegInRef(reg,p.oper[0]^.ref^));

          A_LES:
            Result := (reg=NR_ES) and not(RegInRef(reg,p.oper[0]^.ref^));
{$endif not x86_64}
          A_LFS:
            Result := (reg=NR_FS) and not(RegInRef(reg,p.oper[0]^.ref^));

          A_LGS:
            Result := (reg=NR_GS) and not(RegInRef(reg,p.oper[0]^.ref^));

          A_LSS:
            Result := (reg=NR_SS) and not(RegInRef(reg,p.oper[0]^.ref^));

          A_LAHF{$ifndef x86_64}, A_AAM{$endif not x86_64}:
            Result := Reg1WriteOverwritesReg2Entirely(NR_AH,reg);

          A_LODSB:
            Result := Reg1WriteOverwritesReg2Entirely(NR_AL,reg);

          A_LODSW:
            Result := Reg1WriteOverwritesReg2Entirely(NR_AX,reg);
{$ifdef x86_64}
          A_LODSQ:
            Result := Reg1WriteOverwritesReg2Entirely(NR_RAX,reg);
{$endif x86_64}
          A_LODSD:
            Result := Reg1WriteOverwritesReg2Entirely(NR_EAX,reg);

          A_FSTSW, A_FNSTSW:
            Result := (p.oper[0]^.typ=top_reg) and Reg1WriteOverwritesReg2Entirely(p.oper[0]^.reg,reg);

          else
            begin
              with insprop[p.opcode] do
                begin
                  if (
                    { xor %reg,%reg etc. is classed as a new value }
                    (([Ch_NoReadIfEqualRegs]*Ch)<>[]) and
                    MatchOpType(p, top_reg, top_reg) and
                    (p.oper[0]^.reg = p.oper[1]^.reg) and
                    Reg1WriteOverwritesReg2Entirely(p.oper[1]^.reg,reg)
                  ) then
                    begin
                      Result := True;
                      Exit;
                    end;

                  { Make sure the entire register is overwritten }
                  if (getregtype(reg) = R_INTREGISTER) then
                    begin

                      if (p.ops > 0) then
                        begin
                          if RegInOp(reg, p.oper[0]^) then
                            begin
                              if (p.oper[0]^.typ = top_ref) then
                                begin
                                  if RegInRef(reg, p.oper[0]^.ref^) then
                                    begin
                                      Result := False;
                                      Exit;
                                    end;
                                 end
                              else if (p.oper[0]^.typ = top_reg) then
                                begin

                                  if ([Ch_ROp1, Ch_RWOp1, Ch_MOp1]*Ch<>[]) then
                                    begin
                                      Result := False;
                                      Exit;
                                    end
                                  else if ([Ch_WOp1]*Ch<>[]) then
                                    begin
                                      if Reg1WriteOverwritesReg2Entirely(p.oper[0]^.reg, reg) then
                                        Result := True
                                      else
                                        begin
                                          Result := False;
                                          Exit;
                                        end;
                                    end;
                                end;
                            end;

                          if (p.ops > 1) then
                            begin
                              if RegInOp(reg, p.oper[1]^) then
                                begin
                                  if (p.oper[1]^.typ = top_ref) then
                                    begin
                                      if RegInRef(reg, p.oper[1]^.ref^) then
                                        begin
                                          Result := False;
                                          Exit;
                                        end;
                                     end
                                  else if (p.oper[1]^.typ = top_reg) then
                                    begin

                                      if ([Ch_ROp2, Ch_RWOp2, Ch_MOp2]*Ch<>[]) then
                                        begin
                                          Result := False;
                                          Exit;
                                        end
                                      else if ([Ch_WOp2]*Ch<>[]) then
                                        begin
                                          if Reg1WriteOverwritesReg2Entirely(p.oper[1]^.reg, reg) then
                                            Result := True
                                          else
                                            begin
                                              Result := False;
                                              Exit;
                                            end;
                                        end;
                                    end;
                                end;

                              if (p.ops > 2) then
                                begin
                                  if RegInOp(reg, p.oper[2]^) then
                                    begin
                                      if (p.oper[2]^.typ = top_ref) then
                                        begin
                                          if RegInRef(reg, p.oper[2]^.ref^) then
                                            begin
                                              Result := False;
                                              Exit;
                                            end;
                                         end
                                      else if (p.oper[2]^.typ = top_reg) then
                                        begin

                                          if ([Ch_ROp3, Ch_RWOp3, Ch_MOp3]*Ch<>[]) then
                                            begin
                                              Result := False;
                                              Exit;
                                            end
                                          else if ([Ch_WOp3]*Ch<>[]) then
                                            begin
                                              if Reg1WriteOverwritesReg2Entirely(p.oper[2]^.reg, reg) then
                                                Result := True
                                              else
                                                begin
                                                  Result := False;
                                                  Exit;
                                                end;
                                            end;
                                        end;
                                    end;

                                  if (p.ops > 3) and RegInOp(reg, p.oper[3]^) then
                                    begin
                                      if (p.oper[3]^.typ = top_ref) then
                                        begin
                                          if RegInRef(reg, p.oper[3]^.ref^) then
                                            begin
                                              Result := False;
                                              Exit;
                                            end;
                                         end
                                      else if (p.oper[3]^.typ = top_reg) then
                                        begin

                                          if ([Ch_ROp4, Ch_RWOp4, Ch_MOp4]*Ch<>[]) then
                                            begin
                                              Result := False;
                                              Exit;
                                            end
                                          else if ([Ch_WOp4]*Ch<>[]) then
                                            begin
                                              if Reg1WriteOverwritesReg2Entirely(p.oper[3]^.reg, reg) then
                                                Result := True
                                              else
                                                begin
                                                  Result := False;
                                                  Exit;
                                                end;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end;

                      { Don't do these ones first in case an input operand is equal to an explicit output register }
                      case getsupreg(reg) of
                        RS_EAX:
                          if ([Ch_WEAX{$ifdef x86_64},Ch_WRAX{$endif x86_64}]*Ch<>[]) and Reg1WriteOverwritesReg2Entirely(NR_EAX, reg) then
                            begin
                              Result := True;
                              Exit;
                            end;
                        RS_ECX:
                          if ([Ch_WECX{$ifdef x86_64},Ch_WRCX{$endif x86_64}]*Ch<>[]) and Reg1WriteOverwritesReg2Entirely(NR_ECX, reg) then
                            begin
                              Result := True;
                              Exit;
                            end;
                        RS_EDX:
                          if ([Ch_REDX{$ifdef x86_64},Ch_WRDX{$endif x86_64}]*Ch<>[]) and Reg1WriteOverwritesReg2Entirely(NR_EDX, reg) then
                            begin
                              Result := True;
                              Exit;
                            end;
                        RS_EBX:
                          if ([Ch_WEBX{$ifdef x86_64},Ch_WRBX{$endif x86_64}]*Ch<>[]) and Reg1WriteOverwritesReg2Entirely(NR_EBX, reg) then
                            begin
                              Result := True;
                              Exit;
                            end;
                        RS_ESP:
                          if ([Ch_WESP{$ifdef x86_64},Ch_WRSP{$endif x86_64}]*Ch<>[]) and Reg1WriteOverwritesReg2Entirely(NR_ESP, reg) then
                            begin
                              Result := True;
                              Exit;
                            end;
                        RS_EBP:
                          if ([Ch_WEBP{$ifdef x86_64},Ch_WRBP{$endif x86_64}]*Ch<>[]) and Reg1WriteOverwritesReg2Entirely(NR_EBP, reg) then
                            begin
                              Result := True;
                              Exit;
                            end;
                        RS_ESI:
                          if ([Ch_WESI{$ifdef x86_64},Ch_WRSI{$endif x86_64}]*Ch<>[]) and Reg1WriteOverwritesReg2Entirely(NR_ESI, reg) then
                            begin
                              Result := True;
                              Exit;
                            end;
                        RS_EDI:
                          if ([Ch_WEDI{$ifdef x86_64},Ch_WRDI{$endif x86_64}]*Ch<>[]) and Reg1WriteOverwritesReg2Entirely(NR_EDI, reg) then
                            begin
                              Result := True;
                              Exit;
                            end;
                        else
                          ;
                      end;
                    end;
               end;
            end;
          end;
      end;


    class function TX86AsmOptimizer.IsExitCode(p : tai) : boolean;
      var
        hp2,hp3 : tai;
      begin
        { some x86-64 issue a NOP before the real exit code }
        if MatchInstruction(p,A_NOP,[]) then
          GetNextInstruction(p,p);
        result:=assigned(p) and (p.typ=ait_instruction) and
        ((taicpu(p).opcode = A_RET) or
         ((taicpu(p).opcode=A_LEAVE) and
          GetNextInstruction(p,hp2) and
          MatchInstruction(hp2,A_RET,[S_NO])
         ) or
         (((taicpu(p).opcode=A_LEA) and
           MatchOpType(taicpu(p),top_ref,top_reg) and
           (taicpu(p).oper[0]^.ref^.base=NR_STACK_POINTER_REG) and
           (taicpu(p).oper[1]^.reg=NR_STACK_POINTER_REG)
           ) and
          GetNextInstruction(p,hp2) and
          MatchInstruction(hp2,A_RET,[S_NO])
         ) or
         ((((taicpu(p).opcode=A_MOV) and
           MatchOpType(taicpu(p),top_reg,top_reg) and
           (taicpu(p).oper[0]^.reg=current_procinfo.framepointer) and
           (taicpu(p).oper[1]^.reg=NR_STACK_POINTER_REG)) or
           ((taicpu(p).opcode=A_LEA) and
           MatchOpType(taicpu(p),top_ref,top_reg) and
           (taicpu(p).oper[0]^.ref^.base=current_procinfo.framepointer) and
           (taicpu(p).oper[1]^.reg=NR_STACK_POINTER_REG)
           )
          ) and
          GetNextInstruction(p,hp2) and
          MatchInstruction(hp2,A_POP,[reg2opsize(current_procinfo.framepointer)]) and
          MatchOpType(taicpu(hp2),top_reg) and
          (taicpu(hp2).oper[0]^.reg=current_procinfo.framepointer) and
          GetNextInstruction(hp2,hp3) and
          MatchInstruction(hp3,A_RET,[S_NO])
         )
        );
      end;


    class function TX86AsmOptimizer.isFoldableArithOp(hp1: taicpu; reg: tregister): boolean;
      begin
        isFoldableArithOp := False;
        case hp1.opcode of
          A_ADD,A_SUB,A_OR,A_XOR,A_AND,A_SHL,A_SHR,A_SAR:
            isFoldableArithOp :=
              ((taicpu(hp1).oper[0]^.typ = top_const) or
               ((taicpu(hp1).oper[0]^.typ = top_reg) and
                (taicpu(hp1).oper[0]^.reg <> reg))) and
              (taicpu(hp1).oper[1]^.typ = top_reg) and
              (taicpu(hp1).oper[1]^.reg = reg);
          A_INC,A_DEC,A_NEG,A_NOT:
            isFoldableArithOp :=
              (taicpu(hp1).oper[0]^.typ = top_reg) and
              (taicpu(hp1).oper[0]^.reg = reg);
          else
            ;
        end;
      end;


    procedure TX86AsmOptimizer.RemoveLastDeallocForFuncRes(p: tai);

      procedure DoRemoveLastDeallocForFuncRes( supreg: tsuperregister);
        var
          hp2: tai;
        begin
          hp2 := p;
          repeat
            hp2 := tai(hp2.previous);
            if assigned(hp2) and
               (hp2.typ = ait_regalloc) and
               (tai_regalloc(hp2).ratype=ra_dealloc) and
               (getregtype(tai_regalloc(hp2).reg) = R_INTREGISTER) and
               (getsupreg(tai_regalloc(hp2).reg) = supreg) then
              begin
                RemoveInstruction(hp2);
                break;
              end;
          until not(assigned(hp2)) or regInInstruction(newreg(R_INTREGISTER,supreg,R_SUBWHOLE),hp2);
        end;

      begin
          case current_procinfo.procdef.returndef.typ of
            arraydef,recorddef,pointerdef,
               stringdef,enumdef,procdef,objectdef,errordef,
               filedef,setdef,procvardef,
               classrefdef,forwarddef:
              DoRemoveLastDeallocForFuncRes(RS_EAX);
            orddef:
              if current_procinfo.procdef.returndef.size <> 0 then
                begin
                  DoRemoveLastDeallocForFuncRes(RS_EAX);
                  { for int64/qword }
                  if current_procinfo.procdef.returndef.size = 8 then
                    DoRemoveLastDeallocForFuncRes(RS_EDX);
                end;
            else
              ;
          end;
      end;


    function TX86AsmOptimizer.OptPass1_V_MOVAP(var p : tai) : boolean;
      var
        hp1,hp2 : tai;
      begin
        result:=false;
        if MatchOpType(taicpu(p),top_reg,top_reg) then
          begin
            { vmova* reg1,reg1
              =>
              <nop> }
            if taicpu(p).oper[0]^.reg = taicpu(p).oper[1]^.reg then
              begin
                RemoveCurrentP(p);
                result:=true;
                exit;
              end;

            if GetNextInstructionUsingReg(p,hp1,taicpu(p).oper[1]^.reg) and
              (hp1.typ = ait_instruction) and
              (
                { Under -O2 and below, the instructions are always adjacent }
                not (cs_opt_level3 in current_settings.optimizerswitches) or
                (taicpu(hp1).ops <= 1) or
                not RegInOp(taicpu(p).oper[0]^.reg, taicpu(hp1).oper[1]^) or
                { If reg1 = reg3, reg1 must not be modified in between }
                not RegModifiedBetween(taicpu(p).oper[0]^.reg, p, hp1)
              ) then
              begin
                if MatchInstruction(hp1,[taicpu(p).opcode],[S_NO]) and
                  MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[0]^) then
                  begin
                    { vmova* reg1,reg2
                      ...
                      vmova* reg2,reg3
                      dealloc reg2
                      =>
                      vmova* reg1,reg3 }
                    TransferUsedRegs(TmpUsedRegs);
                    UpdateUsedRegs(TmpUsedRegs, tai(p.next));
                    if MatchOpType(taicpu(hp1),top_reg,top_reg) and
                      not RegUsedBetween(taicpu(hp1).oper[1]^.reg, p, hp1) and
                      not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp1,TmpUsedRegs)) then
                      begin
                        DebugMsg(SPeepholeOptimization + '(V)MOVA*(V)MOVA*2(V)MOVA* 1',p);
                        taicpu(p).loadoper(1,taicpu(hp1).oper[1]^);

                        TransferUsedRegs(TmpUsedRegs);
                        AllocRegBetween(taicpu(hp1).oper[1]^.reg, p, hp1, TmpUsedRegs);

                        RemoveInstruction(hp1);
                        result:=true;
                        exit;
                      end;
                    { special case:
                      vmova* reg1,<op>
                      ...
                      vmova* <op>,reg1
                      =>
                      vmova* reg1,<op> }
                    if MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[1]^) and
                      ((taicpu(p).oper[0]^.typ<>top_ref) or
                       (not(vol_read in taicpu(p).oper[0]^.ref^.volatility))
                      ) then
                      begin
                        DebugMsg(SPeepholeOptimization + '(V)MOVA*(V)MOVA*2(V)MOVA* 2',p);
                        RemoveInstruction(hp1);
                        result:=true;
                        exit;
                      end
                  end
                else if ((MatchInstruction(p,[A_MOVAPS,A_VMOVAPS],[S_NO]) and
                  MatchInstruction(hp1,[A_MOVSS,A_VMOVSS],[S_NO])) or
                  ((MatchInstruction(p,[A_MOVAPD,A_VMOVAPD],[S_NO]) and
                    MatchInstruction(hp1,[A_MOVSD,A_VMOVSD],[S_NO])))
                  ) and
                  MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[0]^) then
                  begin
                    { vmova* reg1,reg2
                      ...
                      vmovs* reg2,<op>
                      dealloc reg2
                      =>
                      vmovs* reg1,<op> }
                    TransferUsedRegs(TmpUsedRegs);
                    UpdateUsedRegsBetween(TmpUsedRegs, p, hp1);
                    if not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp1,TmpUsedRegs)) then
                      begin
                        DebugMsg(SPeepholeOptimization + '(V)MOVA*(V)MOVS*2(V)MOVS* 1',p);
                        taicpu(p).opcode:=taicpu(hp1).opcode;
                        taicpu(p).loadoper(1,taicpu(hp1).oper[1]^);

                        TransferUsedRegs(TmpUsedRegs);
                        AllocRegBetween(taicpu(p).oper[0]^.reg, p, hp1, TmpUsedRegs);

                        RemoveInstruction(hp1);
                        result:=true;
                        exit;
                      end
                  end;

              if MatchInstruction(hp1,[A_VFMADDPD,
                                              A_VFMADD132PD,
                                              A_VFMADD132PS,
                                              A_VFMADD132SD,
                                              A_VFMADD132SS,
                                              A_VFMADD213PD,
                                              A_VFMADD213PS,
                                              A_VFMADD213SD,
                                              A_VFMADD213SS,
                                              A_VFMADD231PD,
                                              A_VFMADD231PS,
                                              A_VFMADD231SD,
                                              A_VFMADD231SS,
                                              A_VFMADDSUB132PD,
                                              A_VFMADDSUB132PS,
                                              A_VFMADDSUB213PD,
                                              A_VFMADDSUB213PS,
                                              A_VFMADDSUB231PD,
                                              A_VFMADDSUB231PS,
                                              A_VFMSUB132PD,
                                              A_VFMSUB132PS,
                                              A_VFMSUB132SD,
                                              A_VFMSUB132SS,
                                              A_VFMSUB213PD,
                                              A_VFMSUB213PS,
                                              A_VFMSUB213SD,
                                              A_VFMSUB213SS,
                                              A_VFMSUB231PD,
                                              A_VFMSUB231PS,
                                              A_VFMSUB231SD,
                                              A_VFMSUB231SS,
                                              A_VFMSUBADD132PD,
                                              A_VFMSUBADD132PS,
                                              A_VFMSUBADD213PD,
                                              A_VFMSUBADD213PS,
                                              A_VFMSUBADD231PD,
                                              A_VFMSUBADD231PS,
                                              A_VFNMADD132PD,
                                              A_VFNMADD132PS,
                                              A_VFNMADD132SD,
                                              A_VFNMADD132SS,
                                              A_VFNMADD213PD,
                                              A_VFNMADD213PS,
                                              A_VFNMADD213SD,
                                              A_VFNMADD213SS,
                                              A_VFNMADD231PD,
                                              A_VFNMADD231PS,
                                              A_VFNMADD231SD,
                                              A_VFNMADD231SS,
                                              A_VFNMSUB132PD,
                                              A_VFNMSUB132PS,
                                              A_VFNMSUB132SD,
                                              A_VFNMSUB132SS,
                                              A_VFNMSUB213PD,
                                              A_VFNMSUB213PS,
                                              A_VFNMSUB213SD,
                                              A_VFNMSUB213SS,
                                              A_VFNMSUB231PD,
                                              A_VFNMSUB231PS,
                                              A_VFNMSUB231SD,
                                              A_VFNMSUB231SS],[S_NO]) and
                  { we mix single and double opperations here because we assume that the compiler
                    generates vmovapd only after double operations and vmovaps only after single operations }
                  MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[2]^.reg) and
                  GetNextInstructionUsingReg(hp1, hp2, taicpu(hp1).oper[2]^.reg) and
                  MatchInstruction(hp2,[A_VMOVAPD,A_VMOVAPS,A_MOVAPD,A_MOVAPS],[S_NO]) and
                  MatchOperand(taicpu(p).oper[0]^,taicpu(hp2).oper[1]^) then
                  begin
                    TransferUsedRegs(TmpUsedRegs);
                    UpdateUsedRegsBetween(TmpUsedRegs, p, hp2);
                    if not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp2,TmpUsedRegs)) then
                      begin
                        taicpu(hp1).loadoper(2,taicpu(p).oper[0]^);
                        if (cs_opt_level3 in current_settings.optimizerswitches) then
                          RemoveCurrentP(p)
                        else
                          RemoveCurrentP(p, hp1); // hp1 is guaranteed to be the immediate next instruction in this case.
                        RemoveInstruction(hp2);
                      end;
                  end
                else if (hp1.typ = ait_instruction) and
                  (((taicpu(p).opcode=A_MOVAPS) and
                    ((taicpu(hp1).opcode=A_ADDSS) or (taicpu(hp1).opcode=A_SUBSS) or
                     (taicpu(hp1).opcode=A_MULSS) or (taicpu(hp1).opcode=A_DIVSS))) or
                   ((taicpu(p).opcode=A_MOVAPD) and
                    ((taicpu(hp1).opcode=A_ADDSD) or (taicpu(hp1).opcode=A_SUBSD) or
                     (taicpu(hp1).opcode=A_MULSD) or (taicpu(hp1).opcode=A_DIVSD)))
                  ) and
                  GetNextInstructionUsingReg(hp1, hp2, taicpu(hp1).oper[1]^.reg) and
                  MatchInstruction(hp2,taicpu(p).opcode,[]) and
                  OpsEqual(taicpu(hp2).oper[1]^, taicpu(p).oper[0]^) and
                  MatchOpType(taicpu(hp2),top_reg,top_reg) and
                  MatchOperand(taicpu(hp2).oper[0]^,taicpu(p).oper[1]^) then
                  { change
                             movapX    reg,reg2
                             addsX/subsX/... reg3, reg2
                             movapX    reg2,reg
                    to
                             addsX/subsX/... reg3,reg
                  }
                  begin
                    TransferUsedRegs(TmpUsedRegs);
                    UpdateUsedRegsBetween(TmpUsedRegs, p, hp2);
                    If not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp2,TmpUsedRegs)) then
                      begin
                        DebugMsg(SPeepholeOptimization + 'MovapXOpMovapX2Op ('+
                              debug_op2str(taicpu(p).opcode)+' '+
                              debug_op2str(taicpu(hp1).opcode)+' '+
                              debug_op2str(taicpu(hp2).opcode)+') done',p);
                        { we cannot eliminate the first move if
                          the operations uses the same register for source and dest }
                        if not(OpsEqual(taicpu(hp1).oper[1]^,taicpu(hp1).oper[0]^)) then
                          { Remember that hp1 is not necessarily the immediate
                            next instruction }
                          RemoveCurrentP(p);

                        taicpu(hp1).loadoper(1, taicpu(hp2).oper[1]^);
                        RemoveInstruction(hp2);
                        result:=true;
                      end;
                  end
                else if (hp1.typ = ait_instruction) and
                  (((taicpu(p).opcode=A_VMOVAPD) and
                    (taicpu(hp1).opcode=A_VCOMISD)) or
                   ((taicpu(p).opcode=A_VMOVAPS) and
                    ((taicpu(hp1).opcode=A_VCOMISS))
                   )
                  ) and not(OpsEqual(taicpu(hp1).oper[1]^,taicpu(hp1).oper[0]^)) then
                  { change
                             movapX    reg,reg1
                             vcomisX   reg1,reg1
                    to
                             vcomisX   reg,reg
                  }
                  begin
                    TransferUsedRegs(TmpUsedRegs);
                    UpdateUsedRegs(TmpUsedRegs, tai(p.next));
                    If not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp1,TmpUsedRegs)) then
                      begin
                        DebugMsg(SPeepholeOptimization + 'MovapXComisX2ComisX2 ('+
                              debug_op2str(taicpu(p).opcode)+' '+
                              debug_op2str(taicpu(hp1).opcode)+') done',p);
                        if OpsEqual(taicpu(p).oper[1]^,taicpu(hp1).oper[0]^) then
                          taicpu(hp1).loadoper(0, taicpu(p).oper[0]^);
                        if OpsEqual(taicpu(p).oper[1]^,taicpu(hp1).oper[1]^) then
                          taicpu(hp1).loadoper(1, taicpu(p).oper[0]^);
                        RemoveCurrentP(p);
                        result:=true;
                        exit;
                      end;
                  end
              end;
          end;
      end;


    function TX86AsmOptimizer.OptPass1VOP(var p : tai) : boolean;
      var
        hp1 : tai;
      begin
        result:=false;
        { replace
            V<Op>X   %mreg1,%mreg2,%mreg3
            VMovX    %mreg3,%mreg4
            dealloc  %mreg3

            by
            V<Op>X   %mreg1,%mreg2,%mreg4
          ?
        }
        if GetNextInstruction(p,hp1) and
          { we mix single and double operations here because we assume that the compiler
            generates vmovapd only after double operations and vmovaps only after single operations }
          MatchInstruction(hp1,A_VMOVAPD,A_VMOVAPS,[S_NO]) and
          MatchOperand(taicpu(p).oper[2]^,taicpu(hp1).oper[0]^) and
          (taicpu(hp1).oper[1]^.typ=top_reg) then
          begin
            TransferUsedRegs(TmpUsedRegs);
            UpdateUsedRegs(TmpUsedRegs, tai(p.next));
            if not(RegUsedAfterInstruction(taicpu(hp1).oper[0]^.reg,hp1,TmpUsedRegs)) then
              begin
                taicpu(p).loadoper(2,taicpu(hp1).oper[1]^);
                DebugMsg(SPeepholeOptimization + 'VOpVmov2VOp done',p);
                RemoveInstruction(hp1);
                result:=true;
              end;
          end;
      end;


    { Replaces all references to AOldReg in a memory reference to ANewReg }
    class function TX86AsmOptimizer.ReplaceRegisterInRef(var ref: TReference; const AOldReg, ANewReg: TRegister): Boolean;
      begin
        Result := False;
        { For safety reasons, only check for exact register matches }

        { Check base register }
        if (ref.base = AOldReg) then
          begin
            ref.base := ANewReg;
            Result := True;
          end;

        { Check index register }
        if (ref.index = AOldReg) and (getsupreg(ANewReg)<>RS_ESP) then
          begin
            ref.index := ANewReg;
            Result := True;
          end;
      end;


    { Replaces all references to AOldReg in an operand to ANewReg }
    class function TX86AsmOptimizer.ReplaceRegisterInOper(const p: taicpu; const OperIdx: Integer; const AOldReg, ANewReg: TRegister): Boolean;
      var
        OldSupReg, NewSupReg: TSuperRegister;
        OldSubReg, NewSubReg: TSubRegister;
        OldRegType: TRegisterType;
        ThisOper: POper;
      begin
        ThisOper := p.oper[OperIdx]; { Faster to access overall }
        Result := False;

        if (AOldReg = NR_NO) or (ANewReg = NR_NO) then
          InternalError(2020011801);

        OldSupReg := getsupreg(AOldReg);
        OldSubReg := getsubreg(AOldReg);
        OldRegType := getregtype(AOldReg);
        NewSupReg := getsupreg(ANewReg);
        NewSubReg := getsubreg(ANewReg);

        if OldRegType <> getregtype(ANewReg) then
          InternalError(2020011802);

        if OldSubReg <> NewSubReg then
          InternalError(2020011803);

        case ThisOper^.typ of
          top_reg:
            if (
              (ThisOper^.reg = AOldReg) or
                (
                  (OldRegType = R_INTREGISTER) and
                  (getsupreg(ThisOper^.reg) = OldSupReg) and
                  (getregtype(ThisOper^.reg) = R_INTREGISTER) and
                  (
                    (getsubreg(ThisOper^.reg) <= OldSubReg)
{$ifndef x86_64}
                    and (
                    { Under i386 and i8086, ESI, EDI, EBP and ESP
                      don't have an 8-bit representation }
                      (getsubreg(ThisOper^.reg) >= R_SUBW) or
                      not (NewSupReg in [RS_ESI, RS_EDI, RS_EBP, RS_ESP])
                    )
{$endif x86_64}
                  )
                )
              ) then
              begin
                ThisOper^.reg := newreg(getregtype(ANewReg), NewSupReg, getsubreg(p.oper[OperIdx]^.reg));
                Result := True;
              end;
          top_ref:
            if ReplaceRegisterInRef(ThisOper^.ref^, AOldReg, ANewReg) then
              Result := True;

          else
            ;
        end;
      end;


    { Replaces all references to AOldReg in an instruction to ANewReg }
    class function TX86AsmOptimizer.ReplaceRegisterInInstruction(const p: taicpu; const AOldReg, ANewReg: TRegister): Boolean;
      const
        ReadFlag: array[0..3] of TInsChange = (Ch_Rop1, Ch_Rop2, Ch_Rop3, Ch_Rop4);
      var
        OperIdx: Integer;
      begin
        Result := False;

        for OperIdx := 0 to p.ops - 1 do
          if (ReadFlag[OperIdx] in InsProp[p.Opcode].Ch) then
            begin
            { The shift and rotate instructions can only use CL }
              if not (
                  (OperIdx = 0) and
                  { This second condition just helps to avoid unnecessarily
                    calling MatchInstruction for 10 different opcodes }
                  (p.oper[0]^.reg = NR_CL) and
                  MatchInstruction(p, [A_RCL, A_RCR, A_ROL, A_ROR, A_SAL, A_SAR, A_SHL, A_SHLD, A_SHR, A_SHRD], [])
                ) then
                  Result := ReplaceRegisterInOper(p, OperIdx, AOldReg, ANewReg) or Result;
            end
          else if p.oper[OperIdx]^.typ = top_ref then
            { It's okay to replace registers in references that get written to }
            Result := ReplaceRegisterInOper(p, OperIdx, AOldReg, ANewReg) or Result;
      end;


    class function TX86AsmOptimizer.IsRefSafe(const ref: PReference): Boolean;
      begin
        Result :=
          (ref^.index = NR_NO) and
          (
{$ifdef x86_64}
            (
              (ref^.base = NR_RIP) and
              (ref^.refaddr in [addr_pic, addr_pic_no_got])
            ) or
{$endif x86_64}
            (ref^.refaddr = addr_full) or
            (ref^.base = NR_STACK_POINTER_REG) or
            (ref^.base = current_procinfo.framepointer)
          );
      end;


    function TX86AsmOptimizer.ConvertLEA(const p: taicpu): Boolean;
      var
        l: asizeint;
      begin
        Result := False;

        { Should have been checked previously }
        if p.opcode <> A_LEA then
          InternalError(2020072501);

        { do not mess with the stack point as adjusting it by lea is recommend, except if we optimize for size }
         if (p.oper[1]^.reg=NR_STACK_POINTER_REG) and
           not(cs_opt_size in current_settings.optimizerswitches) then
           exit;

         with p.oper[0]^.ref^ do
          begin
            if (base <> p.oper[1]^.reg) or
               (index <> NR_NO) or
               assigned(symbol) then
              exit;

            l:=offset;
            if (l=1) and UseIncDec then
              begin
                p.opcode:=A_INC;
                p.loadreg(0,p.oper[1]^.reg);
                p.ops:=1;
                DebugMsg(SPeepholeOptimization + 'Lea2Inc done',p);
              end
            else if (l=-1) and UseIncDec then
              begin
                p.opcode:=A_DEC;
                p.loadreg(0,p.oper[1]^.reg);
                p.ops:=1;
                DebugMsg(SPeepholeOptimization + 'Lea2Dec done',p);
              end
            else
              begin
                if (l<0) and (l<>-2147483648) then
                  begin
                    p.opcode:=A_SUB;
                    p.loadConst(0,-l);
                    DebugMsg(SPeepholeOptimization + 'Lea2Sub done',p);
                  end
                else
                  begin
                    p.opcode:=A_ADD;
                    p.loadConst(0,l);
                    DebugMsg(SPeepholeOptimization + 'Lea2Add done',p);
                  end;
              end;
          end;

        Result := True;
      end;


    function TX86AsmOptimizer.DeepMOVOpt(const p_mov: taicpu; const hp: taicpu): Boolean;
      var
        CurrentReg, ReplaceReg: TRegister;
      begin
        Result := False;

        ReplaceReg := taicpu(p_mov).oper[0]^.reg;
        CurrentReg := taicpu(p_mov).oper[1]^.reg;

        case hp.opcode of
          A_FSTSW, A_FNSTSW,
          A_IN,   A_INS,  A_OUT,  A_OUTS,
          A_CMPS, A_LODS, A_MOVS, A_SCAS, A_STOS:
            { These routines have explicit operands, but they are restricted in
              what they can be (e.g. IN and OUT can only read from AL, AX or
              EAX. }
            Exit;

          A_IMUL:
            begin
                { The 1-operand version writes to implicit registers
                  The 2-operand version reads from the first operator, and reads
                  from and writes to the second (equivalent to Ch_ROp1, ChRWOp2).
                  the 3-operand version reads from a register that it doesn't write to
                }
              case hp.ops of
                1:
                  if (
                    (
                      (hp.opsize = S_B) and (getsupreg(CurrentReg) <> RS_EAX)
                    ) or
                      not (getsupreg(CurrentReg) in [RS_EAX, RS_EDX])
                  ) and ReplaceRegisterInOper(hp, 0, CurrentReg, ReplaceReg) then
                    begin
                      Result := True;
                      DebugMsg(SPeepholeOptimization + debug_regname(CurrentReg) + ' = ' + debug_regname(ReplaceReg) + '; changed to minimise pipeline stall (MovIMul2MovIMul 1)', hp);
                      AllocRegBetween(ReplaceReg, p_mov, hp, UsedRegs);
                    end;
                2:
                  { Only modify the first parameter }
                  if ReplaceRegisterInOper(hp, 0, CurrentReg, ReplaceReg) then
                    begin
                      Result := True;
                      DebugMsg(SPeepholeOptimization + debug_regname(CurrentReg) + ' = ' + debug_regname(ReplaceReg) + '; changed to minimise pipeline stall (MovIMul2MovIMul 2)', hp);
                      AllocRegBetween(ReplaceReg, p_mov, hp, UsedRegs);
                    end;
                3:
                  { Only modify the second parameter }
                  if ReplaceRegisterInOper(hp, 1, CurrentReg, ReplaceReg) then
                    begin
                      Result := True;
                      DebugMsg(SPeepholeOptimization + debug_regname(CurrentReg) + ' = ' + debug_regname(ReplaceReg) + '; changed to minimise pipeline stall (MovIMul2MovIMul 3)', hp);
                      AllocRegBetween(ReplaceReg, p_mov, hp, UsedRegs);
                    end;
                else
                  InternalError(2020012901);
              end;
            end;

          else
            if (hp.ops > 0) and
              ReplaceRegisterInInstruction(hp, CurrentReg, ReplaceReg) then
              begin
                Result := True;
                DebugMsg(SPeepholeOptimization + debug_regname(CurrentReg) + ' = ' + debug_regname(ReplaceReg) + '; changed to minimise pipeline stall (MovXXX2MovXXX)', hp);

                AllocRegBetween(ReplaceReg, p_mov, hp, UsedRegs);
              end;
          end;
      end;


    function TX86AsmOptimizer.FuncMov2Func(var p: tai; const hp1: tai): Boolean;
      var
        hp2: tai;
        p_SourceReg, p_TargetReg: TRegister;

      begin
        Result := False;
        { Backward optimisation.  If we have:
            func.  %reg1,%reg2
            mov    %reg2,%reg3
            (dealloc %reg2)

          Change to:
            func.  %reg1,%reg3 (see comment below for what a valid func. is)

          Perform similar optimisations with 1, 3 and 4-operand instructions
          that only have one output.
        }
        if MatchOpType(taicpu(p), top_reg, top_reg) then
          begin
            p_SourceReg := taicpu(p).oper[0]^.reg;
            p_TargetReg := taicpu(p).oper[1]^.reg;
            TransferUsedRegs(TmpUsedRegs);
            if not RegUsedAfterInstruction(p_SourceReg, p, TmpUsedRegs) and
              GetLastInstruction(p, hp2) and
              (hp2.typ = ait_instruction) and
              { Have to make sure it's an instruction that only reads from
                the first operands and only writes (not reads or modifies) to
                the last one; in essence, a pure function such as BSR, POPCNT
                or ANDN }
              (
                (
                  (taicpu(hp2).ops = 1) and
                  (insprop[taicpu(hp2).opcode].Ch * [Ch_Wop1] = [Ch_Wop1])
                ) or
                (
                  (taicpu(hp2).ops = 2) and
                  (insprop[taicpu(hp2).opcode].Ch * [Ch_Rop1, Ch_Wop2] = [Ch_Rop1, Ch_Wop2])
                ) or
                (
                  (taicpu(hp2).ops = 3) and
                  (insprop[taicpu(hp2).opcode].Ch * [Ch_Rop1, Ch_Rop2, Ch_Wop3] = [Ch_Rop1, Ch_Rop2, Ch_Wop3])
                ) or
                (
                  (taicpu(hp2).ops = 4) and
                  (insprop[taicpu(hp2).opcode].Ch * [Ch_Rop1, Ch_Rop2, Ch_Rop3, Ch_Wop4] = [Ch_Rop1, Ch_Rop2, Ch_Rop3, Ch_Wop4])
                )
              ) and
              (taicpu(hp2).oper[taicpu(hp2).ops-1]^.typ = top_reg) and
              (taicpu(hp2).oper[taicpu(hp2).ops-1]^.reg = p_SourceReg) then
              begin
                case taicpu(hp2).opcode of
                  A_FSTSW, A_FNSTSW,
                  A_IN,   A_INS,  A_OUT,  A_OUTS,
                  A_CMPS, A_LODS, A_MOVS, A_SCAS, A_STOS:
                    { These routines have explicit operands, but they are restricted in
                      what they can be (e.g. IN and OUT can only read from AL, AX or
                      EAX. }
                    ;
                  else
                    begin
                      DebugMsg(SPeepholeOptimization + 'Removed MOV and changed destination on previous instruction to optimise register usage (FuncMov2Func)', p);
                      taicpu(hp2).oper[taicpu(hp2).ops-1]^.reg := p_TargetReg;

                      if not RegInInstruction(p_TargetReg, hp2) then
                        begin
                          { Since we're allocating from an earlier point, we
                            need to remove the register from the tracking }
                          ExcludeRegFromUsedRegs(p_TargetReg, TmpUsedRegs);
                          AllocRegBetween(p_TargetReg, hp2, p, TmpUsedRegs);
                        end;
                      RemoveCurrentp(p, hp1);

                      { If the Func was another MOV instruction, we might get
                        "mov %reg,%reg" that doesn't get removed in Pass 2
                        otherwise, so deal with it here (also do something
                        similar with lea (%reg),%reg}
                      if (taicpu(hp2).opcode = A_MOV) and MatchOperand(taicpu(hp2).oper[0]^, taicpu(hp2).oper[1]^.reg) then
                        begin
                          DebugMsg(SPeepholeOptimization + 'Mov2Nop 1a done', hp2);

                          if p = hp2 then
                            RemoveCurrentp(p)
                          else
                            RemoveInstruction(hp2);
                        end;

                      Result := True;
                      Exit;
                    end;
                end;
              end;
          end;
      end;


    function TX86AsmOptimizer.CheckMovMov2MovMov2(const p, hp1: tai) : boolean;
      begin
        Result := False;
        if MatchOpType(taicpu(p),top_ref,top_reg) and
          MatchOpType(taicpu(hp1),top_ref,top_reg) and
          (taicpu(p).opsize = taicpu(hp1).opsize) and
          RefsEqual(taicpu(p).oper[0]^.ref^,taicpu(hp1).oper[0]^.ref^) and
          (taicpu(p).oper[0]^.ref^.volatility=[]) and
          (taicpu(hp1).oper[0]^.ref^.volatility=[]) and
          not(SuperRegistersEqual(taicpu(p).oper[1]^.reg,taicpu(hp1).oper[0]^.ref^.base)) and
          not(SuperRegistersEqual(taicpu(p).oper[1]^.reg,taicpu(hp1).oper[0]^.ref^.index)) then
          begin
            DebugMsg(SPeepholeOptimization + 'MovMov2MovMov 2',p);
            taicpu(hp1).loadReg(0,taicpu(p).oper[1]^.reg);
            Result := True;
          end;
      end;


    function TX86AsmOptimizer.OptPass1MOV(var p : tai) : boolean;
    var
      hp1, hp2, hp3, hp4: tai;
      DoOptimisation, TempBool: Boolean;
{$ifdef x86_64}
      NewConst: TCGInt;
{$endif x86_64}

      procedure convert_mov_value(signed_movop: tasmop; max_value: tcgint); inline;
        begin
          if taicpu(hp1).opcode = signed_movop then
            begin
              if taicpu(p).oper[0]^.val > max_value shr 1 then
                taicpu(p).oper[0]^.val:=taicpu(p).oper[0]^.val - max_value - 1 { Convert to signed }
            end
          else
            taicpu(p).oper[0]^.val:=taicpu(p).oper[0]^.val and max_value; { Trim to unsigned }
        end;

      function TryConstMerge(var p1, p2: tai): Boolean;
        var
          ThisRef: TReference;
        begin
          Result := False;
          ThisRef := taicpu(p2).oper[1]^.ref^;
          { Only permit writes to the stack, since we can guarantee alignment with that }
          if (ThisRef.index = NR_NO) and
            (
              (ThisRef.base = NR_STACK_POINTER_REG) or
              (ThisRef.base = current_procinfo.framepointer)
            ) then
            begin
              case taicpu(p).opsize of
                S_B:
                  begin
                    { Word writes must be on a 2-byte boundary }
                    if (taicpu(p1).oper[1]^.ref^.offset mod 2) = 0 then
                      begin
                        { Reduce offset of second reference to see if it is sequential with the first }
                        Dec(ThisRef.offset, 1);
                        if RefsEqual(taicpu(p1).oper[1]^.ref^, ThisRef) then
                          begin
                            { Make sure the constants aren't represented as a
                              negative number, as these won't merge properly }
                            taicpu(p1).opsize := S_W;
                            taicpu(p1).oper[0]^.val := (taicpu(p1).oper[0]^.val and $FF) or ((taicpu(p2).oper[0]^.val and $FF) shl 8);
                            DebugMsg(SPeepholeOptimization + 'Merged two byte-sized constant writes to stack (MovMov2Mov 2a)', p1);
                            RemoveInstruction(p2);
                            Result := True;
                          end;
                      end;
                  end;
                S_W:
                  begin
                    { Longword writes must be on a 4-byte boundary }
                    if (taicpu(p1).oper[1]^.ref^.offset mod 4) = 0 then
                      begin
                        { Reduce offset of second reference to see if it is sequential with the first }
                        Dec(ThisRef.offset, 2);
                        if RefsEqual(taicpu(p1).oper[1]^.ref^, ThisRef) then
                          begin
                            { Make sure the constants aren't represented as a
                              negative number, as these won't merge properly }
                            taicpu(p1).opsize := S_L;
                            taicpu(p1).oper[0]^.val := (taicpu(p1).oper[0]^.val and $FFFF) or ((taicpu(p2).oper[0]^.val and $FFFF) shl 16);
                            DebugMsg(SPeepholeOptimization + 'Merged two word-sized constant writes to stack (MovMov2Mov 2b)', p1);
                            RemoveInstruction(p2);
                            Result := True;
                          end;
                      end;
                  end;
{$ifdef x86_64}
                S_L:
                  begin
                    { Only sign-extended 32-bit constants can be written to 64-bit memory directly, so check to
                      see if the constants can be encoded this way. }
                    NewConst := (taicpu(p1).oper[0]^.val and $FFFFFFFF) or (taicpu(p2).oper[0]^.val shl 32);
                    if (NewConst >= -2147483648) and (NewConst <= 2147483647) and
                      { Quadword writes must be on an 8-byte boundary }
                      ((taicpu(p1).oper[1]^.ref^.offset mod 8) = 0) then
                      begin
                        { Reduce offset of second reference to see if it is sequential with the first }
                        Dec(ThisRef.offset, 4);
                        if RefsEqual(taicpu(p1).oper[1]^.ref^, ThisRef) then
                          begin
                            { Make sure the constants aren't represented as a
                              negative number, as these won't merge properly }
                            taicpu(p1).opsize := S_Q;
                            { Force a typecast into a 32-bit signed integer (that will then be sign-extended to 64-bit) }
                            taicpu(p1).oper[0]^.val := NewConst;
                            DebugMsg(SPeepholeOptimization + 'Merged two longword-sized constant writes to stack (MovMov2Mov 2c)', p1);
                            RemoveInstruction(p2);
                            Result := True;
                          end;
                      end;
                  end;
{$endif x86_64}
                else
                  ;
              end;
            end;
        end;

      var
        GetNextInstruction_p, TempRegUsed, CrossJump: Boolean;
        PreMessage, RegName1, RegName2, InputVal, MaskNum: string;
        NewSize: topsize; NewOffset: asizeint;
        p_SourceReg, p_TargetReg, NewMMReg: TRegister;
        SourceRef, TargetRef: TReference;
        MovAligned, MovUnaligned: TAsmOp;
        ThisRef: TReference;
        JumpTracking: TLinkedList;
      begin
        Result:=false;

        GetNextInstruction_p:=GetNextInstruction(p, hp1);

        {  remove mov reg1,reg1? }
        if MatchOperand(taicpu(p).oper[0]^,taicpu(p).oper[1]^)
        then
          begin
            DebugMsg(SPeepholeOptimization + 'Mov2Nop 1 done',p);
            { take care of the register (de)allocs following p }
            RemoveCurrentP(p, hp1);
            Result:=true;
            exit;
          end;

        { All the next optimisations require a next instruction }
        if not GetNextInstruction_p or (hp1.typ <> ait_instruction) then
          Exit;

        { Prevent compiler warnings }
        p_TargetReg := NR_NO;

        if taicpu(p).oper[1]^.typ = top_reg then
          begin
            { Saves on a large number of dereferences }
            p_TargetReg := taicpu(p).oper[1]^.reg;

            { Look for:
                mov %reg1,%reg2
                ??? %reg2,r/m
              Change to:
                mov %reg1,%reg2
                ??? %reg1,r/m
            }
            if taicpu(p).oper[0]^.typ = top_reg then
              begin
                if RegReadByInstruction(p_TargetReg, hp1) and
                  DeepMOVOpt(taicpu(p), taicpu(hp1)) then
                  begin
                    { A change has occurred, just not in p }
                    Result := True;

                    TransferUsedRegs(TmpUsedRegs);
                    UpdateUsedRegs(TmpUsedRegs, tai(p.Next));

                    if not RegUsedAfterInstruction(p_TargetReg, hp1, TmpUsedRegs) and
                      { Just in case something didn't get modified (e.g. an
                        implicit register) }
                      not RegReadByInstruction(p_TargetReg, hp1) then
                      begin
                        { We can remove the original MOV }
                        DebugMsg(SPeepholeOptimization + 'Mov2Nop 3 done',p);
                        RemoveCurrentp(p, hp1);

                        { UsedRegs got updated by RemoveCurrentp }
                        Result := True;
                        Exit;
                      end;

                    { If we know a MOV instruction has become a null operation, we might as well
                      get rid of it now to save time. }
                    if (taicpu(hp1).opcode = A_MOV) and
                      (taicpu(hp1).oper[1]^.typ = top_reg) and
                      SuperRegistersEqual(taicpu(hp1).oper[1]^.reg, taicpu(p).oper[0]^.reg) and
                      { Just being a register is enough to confirm it's a null operation }
                      (taicpu(hp1).oper[0]^.typ = top_reg) then
                      begin

                        Result := True;

                        { Speed-up to reduce a pipeline stall... if we had something like...

                            movl %eax,%edx
                            movw %dx,%ax

                          ... the second instruction would change to movw %ax,%ax, but
                          given that it is now %ax that's active rather than %eax,
                          penalties might occur due to a partial register write, so instead,
                          change it to a MOVZX instruction when optimising for speed.
                        }
                        if not (cs_opt_size in current_settings.optimizerswitches) and
                          IsMOVZXAcceptable and
                          (taicpu(hp1).opsize < taicpu(p).opsize)
{$ifdef x86_64}
                          { operations already implicitly set the upper 64 bits to zero }
                          and not ((taicpu(hp1).opsize = S_L) and (taicpu(p).opsize = S_Q))
{$endif x86_64}
                          then
                          begin
                            DebugMsg(SPeepholeOptimization + 'Zero-extension to minimise pipeline stall (Mov2Movz)',hp1);
                            case taicpu(p).opsize of
                              S_W:
                                if taicpu(hp1).opsize = S_B then
                                  taicpu(hp1).opsize := S_BL
                                else
                                  InternalError(2020012911);
                              S_L{$ifdef x86_64}, S_Q{$endif x86_64}:
                                case taicpu(hp1).opsize of
                                  S_B:
                                    taicpu(hp1).opsize := S_BL;
                                  S_W:
                                    taicpu(hp1).opsize := S_WL;
                                  else
                                    InternalError(2020012912);
                                end;
                              else
                                InternalError(2020012910);
                            end;

                            taicpu(hp1).opcode := A_MOVZX;
                            setsubreg(taicpu(hp1).oper[1]^.reg, R_SUBD);
                          end
                        else
                          begin
                            GetNextInstruction_p := GetNextInstruction(hp1, hp2);
                            DebugMsg(SPeepholeOptimization + 'Mov2Nop 4 done',hp1);
                            RemoveInstruction(hp1);

                            { The instruction after what was hp1 is now the immediate next instruction,
                              so we can continue to make optimisations if it's present }
                            if not GetNextInstruction_p or (hp2.typ <> ait_instruction) then
                              Exit;

                            hp1 := hp2;
                          end;
                      end;

                  end;
              end;
          end;

        { Depending on the DeepMOVOpt above, it may turn out that hp1 completely
          overwrites the original destination register.  e.g.

          movl   ###,%reg2d
          movslq ###,%reg2q (### doesn't have to be the same as the first one)

          In this case, we can remove the MOV (Go to "Mov2Nop 5" below)
        }
        if (taicpu(p).oper[1]^.typ = top_reg) and
          MatchInstruction(hp1, [A_LEA, A_MOV, A_MOVSX, A_MOVZX{$ifdef x86_64}, A_MOVSXD{$endif x86_64}], []) and
          (taicpu(hp1).oper[1]^.typ = top_reg) and
          Reg1WriteOverwritesReg2Entirely(taicpu(hp1).oper[1]^.reg, taicpu(p).oper[1]^.reg) then
            begin
              if RegInOp(taicpu(p).oper[1]^.reg, taicpu(hp1).oper[0]^) then
                begin
                  if (taicpu(hp1).oper[0]^.typ = top_reg) then
                    case taicpu(p).oper[0]^.typ of
                      top_const:
                        { We have something like:

                          movb   $x,   %regb
                          movzbl %regb,%regd

                          Change to:

                          movl   $x,   %regd
                        }
                        begin
                          case taicpu(hp1).opsize of
                            S_BW:
                              begin
                                convert_mov_value(A_MOVSX, $FF);
                                setsubreg(taicpu(p).oper[1]^.reg, R_SUBW);
                                taicpu(p).opsize := S_W;
                              end;
                            S_BL:
                              begin
                                convert_mov_value(A_MOVSX, $FF);
                                setsubreg(taicpu(p).oper[1]^.reg, R_SUBD);
                                taicpu(p).opsize := S_L;
                              end;
                            S_WL:
                              begin
                                convert_mov_value(A_MOVSX, $FFFF);
                                setsubreg(taicpu(p).oper[1]^.reg, R_SUBD);
                                taicpu(p).opsize := S_L;
                              end;
{$ifdef x86_64}
                            S_BQ:
                              begin
                                convert_mov_value(A_MOVSX, $FF);
                                setsubreg(taicpu(p).oper[1]^.reg, R_SUBQ);
                                taicpu(p).opsize := S_Q;
                              end;
                            S_WQ:
                              begin
                                convert_mov_value(A_MOVSX, $FFFF);
                                setsubreg(taicpu(p).oper[1]^.reg, R_SUBQ);
                                taicpu(p).opsize := S_Q;
                              end;
                            S_LQ:
                              begin
                                convert_mov_value(A_MOVSXD, $FFFFFFFF);  { Note it's MOVSXD, not MOVSX }
                                setsubreg(taicpu(p).oper[1]^.reg, R_SUBQ);
                                taicpu(p).opsize := S_Q;
                              end;
{$endif x86_64}
                            else
                              { If hp1 was a MOV instruction, it should have been
                                optimised already }
                              InternalError(2020021001);
                          end;
                          DebugMsg(SPeepholeOptimization + 'MovMovXX2MovXX 2 done',p);
                          RemoveInstruction(hp1);
                          Result := True;
                          Exit;
                        end;
                      top_ref:
                        begin
                          { We have something like:

                            movb   mem,  %regb
                            movzbl %regb,%regd

                            Change to:

                            movzbl mem,  %regd
                          }
                          ThisRef := taicpu(p).oper[0]^.ref^;
                          if (ThisRef.refaddr<>addr_full) and (IsMOVZXAcceptable or (taicpu(hp1).opcode<>A_MOVZX)) then
                            begin
                              DebugMsg(SPeepholeOptimization + 'MovMovXX2MovXX 1 done',p);
                              taicpu(hp1).loadref(0, ThisRef);

                              { Make sure any registers in the references are properly tracked }
                              if (ThisRef.base <> NR_NO){$ifdef x86_64} and (ThisRef.base <> NR_RIP){$endif x86_64} then
                                AllocRegBetween(ThisRef.base, p, hp1, UsedRegs);

                              if (ThisRef.index <> NR_NO) then
                                AllocRegBetween(ThisRef.index, p, hp1, UsedRegs);

                              RemoveCurrentP(p, hp1);
                              Result := True;
                              Exit;
                            end;
                        end;
                      else
                        if (taicpu(hp1).opcode <> A_MOV) and (taicpu(hp1).opcode <> A_LEA) then
                          { Just to make a saving, since there are no more optimisations with MOVZX and MOVSX/D }
                          Exit;
                    end;
                end
             { The RegInOp check makes sure that movl r/m,%reg1l; movzbl (%reg1l),%reg1l"
               and "movl r/m,%reg1; leal $1(%reg1,%reg2),%reg1" etc. are not incorrectly
               optimised }
              else
                begin
                  DebugMsg(SPeepholeOptimization + 'Mov2Nop 5 done',p);
                  RemoveCurrentP(p, hp1);
                  Result := True;
                  Exit;
                end;
            end;

        if (taicpu(hp1).opcode = A_AND) and
          (taicpu(p).oper[1]^.typ = top_reg) and
          MatchOpType(taicpu(hp1),top_const,top_reg) then
          begin
            if MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[1]^) then
              begin
                case taicpu(p).opsize of
                  S_L:
                    if (taicpu(hp1).oper[0]^.val = $ffffffff) then
                      begin
                        { Optimize out:
                            mov x, %reg
                            and ffffffffh, %reg
                        }
                        DebugMsg(SPeepholeOptimization + 'MovAnd2Mov 1 done',p);
                        RemoveInstruction(hp1);
                        Result:=true;
                        exit;
                      end;
                  S_Q: { TODO: Confirm if this is even possible }
                    if (taicpu(hp1).oper[0]^.val = $ffffffffffffffff) then
                      begin
                        { Optimize out:
                            mov x, %reg
                            and ffffffffffffffffh, %reg
                        }
                        DebugMsg(SPeepholeOptimization + 'MovAnd2Mov 2 done',p);
                        RemoveInstruction(hp1);
                        Result:=true;
                        exit;
                      end;
                  else
                    ;
                end;
                if (
                    (taicpu(p).oper[0]^.typ=top_reg) or
                    (
                      (taicpu(p).oper[0]^.typ=top_ref) and
                      (taicpu(p).oper[0]^.ref^.refaddr<>addr_full)
                    )
                  ) and
                  GetNextInstruction(hp1,hp2) and
                  MatchInstruction(hp2,A_TEST,[]) and
                  (
                    MatchOperand(taicpu(hp1).oper[1]^,taicpu(hp2).oper[1]^) or
                    (
                      { If the register being tested is smaller than the one
                        that received a bitwise AND, permit it if the constant
                        fits into the smaller size  }
                      (taicpu(hp1).oper[1]^.typ = top_reg) and (taicpu(hp2).oper[1]^.typ = top_reg) and
                      SuperRegistersEqual(taicpu(hp1).oper[1]^.reg,taicpu(hp2).oper[1]^.reg) and
                      (taicpu(hp1).oper[0]^.typ = top_const) and (taicpu(hp1).oper[0]^.val >= 0) and
                      (GetSubReg(taicpu(hp2).oper[1]^.reg) < GetSubReg(taicpu(hp1).oper[1]^.reg)) and
                      (
                        (
                          (GetSubReg(taicpu(hp2).oper[1]^.reg) = R_SUBL) and
                          (taicpu(hp1).oper[0]^.val <= $FF)
                        ) or
                        (
                          (GetSubReg(taicpu(hp2).oper[1]^.reg) = R_SUBW) and
                          (taicpu(hp1).oper[0]^.val <= $FFFF)
{$ifdef x86_64}
                        ) or
                        (
                          (GetSubReg(taicpu(hp2).oper[1]^.reg) = R_SUBD) and
                          (taicpu(hp1).oper[0]^.val <= $FFFFFFFF)
{$endif x86_64}
                        )
                      )
                    )
                  ) and
                  (
                    MatchOperand(taicpu(hp2).oper[0]^,taicpu(hp2).oper[1]^) or
                    MatchOperand(taicpu(hp2).oper[0]^,-1)
                  ) and
                  GetNextInstruction(hp2,hp3) and
                  MatchInstruction(hp3,A_Jcc,A_Setcc,[]) and
                  (taicpu(hp3).condition in [C_E,C_NE]) then
                  begin
                    TransferUsedRegs(TmpUsedRegs);
                    UpdateUsedRegs(TmpUsedRegs, tai(p.Next));
                    UpdateUsedRegs(TmpUsedRegs, tai(hp1.Next));
                    if not(RegUsedAfterInstruction(taicpu(hp2).oper[1]^.reg, hp2, TmpUsedRegs)) then
                      begin
                        DebugMsg(SPeepholeOptimization + 'MovAndTest2Test done',p);
                        taicpu(hp1).loadoper(1,taicpu(p).oper[0]^);
                        taicpu(hp1).opcode:=A_TEST;

                        { Shrink the TEST instruction down to the smallest possible size }
                        case taicpu(hp1).oper[0]^.val of
                          0..255:
                            if (taicpu(hp1).opsize <> S_B)
{$ifndef x86_64}
                              and (
                                (taicpu(hp1).oper[1]^.typ <> top_reg) or
                                { Cannot encode byte-sized ESI, EDI, EBP or ESP under i386 }
                                (GetSupReg(taicpu(hp1).oper[1]^.reg) in [RS_EAX, RS_EBX, RS_ECX, RS_EDX])
                              )
{$endif x86_64}
                              then
                              begin
                                if taicpu(hp1).opsize <> taicpu(hp2).opsize then
                                  { Only print debug message if the TEST instruction
                                    is a different size before and after }
                                  DebugMsg(SPeepholeOptimization + 'test' + debug_opsize2str(taicpu(hp1).opsize) + ' -> testb to reduce instruction size (Test2Test 1a)' , p);

                                taicpu(hp1).opsize := S_B;
                                if (taicpu(hp1).oper[1]^.typ = top_reg) then
                                  setsubreg(taicpu(hp1).oper[1]^.reg, R_SUBL);
                              end;
                          256..65535:
                            if (taicpu(hp1).opsize <> S_W) then
                              begin
                                if taicpu(hp1).opsize <> taicpu(hp2).opsize then
                                  { Only print debug message if the TEST instruction
                                    is a different size before and after }
                                  DebugMsg(SPeepholeOptimization + 'test' + debug_opsize2str(taicpu(hp1).opsize) + ' -> testw to reduce instruction size (Test2Test 1b)' , p);

                                taicpu(hp1).opsize := S_W;
                                if (taicpu(hp1).oper[1]^.typ = top_reg) then
                                  setsubreg(taicpu(hp1).oper[1]^.reg, R_SUBW);
                              end;
{$ifdef x86_64}
                          65536..$7FFFFFFF:
                            if (taicpu(hp1).opsize <> S_L) then
                              begin
                                if taicpu(hp1).opsize <> taicpu(hp2).opsize then
                                  { Only print debug message if the TEST instruction
                                    is a different size before and after }
                                  DebugMsg(SPeepholeOptimization + 'test' + debug_opsize2str(taicpu(hp1).opsize) + ' -> testl to reduce instruction size (Test2Test 1c)' , p);

                                taicpu(hp1).opsize := S_L;
                                if (taicpu(hp1).oper[1]^.typ = top_reg) then
                                  setsubreg(taicpu(hp1).oper[1]^.reg, R_SUBD);
                              end;
{$endif x86_64}
                          else
                            ;
                        end;

                        RemoveInstruction(hp2);
                        RemoveCurrentP(p, hp1);
                        Result:=true;
                        exit;
                      end;
                  end;
              end
            else if IsMOVZXAcceptable and
              (taicpu(p).oper[1]^.typ = top_reg) and (taicpu(hp1).oper[1]^.typ = top_reg) and
              (taicpu(p).oper[0]^.typ <> top_const) and { MOVZX only supports registers and memory, not immediates (use MOV for that!) }
              (getsupreg(taicpu(p).oper[1]^.reg) = getsupreg(taicpu(hp1).oper[1]^.reg))
              then
              begin
                InputVal := debug_operstr(taicpu(p).oper[0]^);
                MaskNum := debug_tostr(taicpu(hp1).oper[0]^.val);

                case taicpu(p).opsize of
                  S_B:
                    if (taicpu(hp1).oper[0]^.val = $ff) then
                      begin
                        { Convert:
                            movb x, %regl        movb x, %regl
                            andw ffh, %regw      andl ffh, %regd
                          To:
                            movzbw x, %regd      movzbl x, %regd

                          (Identical registers, just different sizes)
                        }
                        RegName1 := debug_regname(taicpu(p).oper[1]^.reg); { 8-bit register name }
                        RegName2 := debug_regname(taicpu(hp1).oper[1]^.reg); { 16/32-bit register name }

                        case taicpu(hp1).opsize of
                          S_W: NewSize := S_BW;
                          S_L: NewSize := S_BL;
{$ifdef x86_64}
                          S_Q: NewSize := S_BQ;
{$endif x86_64}
                          else
                            InternalError(2018011510);
                        end;
                      end
                    else
                      NewSize := S_NO;
                  S_W:
                    if (taicpu(hp1).oper[0]^.val = $ffff) then
                      begin
                        { Convert:
                            movw x, %regw
                            andl ffffh, %regd
                          To:
                            movzwl x, %regd

                          (Identical registers, just different sizes)
                        }
                        RegName1 := debug_regname(taicpu(p).oper[1]^.reg); { 16-bit register name }
                        RegName2 := debug_regname(taicpu(hp1).oper[1]^.reg); { 32-bit register name }

                        case taicpu(hp1).opsize of
                          S_L: NewSize := S_WL;
{$ifdef x86_64}
                          S_Q: NewSize := S_WQ;
{$endif x86_64}
                          else
                            InternalError(2018011511);
                        end;
                      end
                    else
                      NewSize := S_NO;
                  else
                    NewSize := S_NO;
                end;

                if NewSize <> S_NO then
                  begin
                    PreMessage := 'mov' + debug_opsize2str(taicpu(p).opsize) + ' ' + InputVal + ',' + RegName1;

                    { The actual optimization }
                    taicpu(p).opcode := A_MOVZX;
                    taicpu(p).changeopsize(NewSize);
                    taicpu(p).oper[1]^ := taicpu(hp1).oper[1]^;

                    { Safeguard if "and" is followed by a conditional command }
                    TransferUsedRegs(TmpUsedRegs);
                    UpdateUsedRegs(TmpUsedRegs,tai(p.next));

                    if (RegUsedAfterInstruction(NR_DEFAULTFLAGS, hp1, TmpUsedRegs)) then
                      begin
                        { At this point, the "and" command is effectively equivalent to
                          "test %reg,%reg". This will be handled separately by the
                          Peephole Optimizer. [Kit] }

                        DebugMsg(SPeepholeOptimization + PreMessage +
                          ' -> movz' + debug_opsize2str(NewSize) + ' ' + InputVal + ',' + RegName2, p);
                      end
                    else
                      begin
                        DebugMsg(SPeepholeOptimization + PreMessage + '; and' + debug_opsize2str(taicpu(hp1).opsize) + ' $' + MaskNum + ',' + RegName2 +
                          ' -> movz' + debug_opsize2str(NewSize) + ' ' + InputVal + ',' + RegName2, p);

                        RemoveInstruction(hp1);
                      end;

                    Result := True;
                    Exit;

                  end;
              end;
          end;

        if (taicpu(hp1).opcode = A_OR) and
          (taicpu(p).oper[1]^.typ = top_reg) and
          MatchOperand(taicpu(p).oper[0]^, 0) and
          MatchOperand(taicpu(hp1).oper[1]^, taicpu(p).oper[1]^.reg) then
          begin
            {   mov 0,  %reg
                or  ###,%reg
              Change to (only if the flags are not used):
                mov ###,%reg
            }
            TransferUsedRegs(TmpUsedRegs);
            UpdateUsedRegs(TmpUsedRegs, tai(p.Next));
            DoOptimisation := True;

            { Even if the flags are used, we might be able to do the optimisation
              if the conditions are predictable }
            if RegInUsedRegs(NR_DEFAULTFLAGS, TmpUsedRegs) then
              begin
                { Only perform if ### = %reg (the same register) or equal to 0,
                  so %reg is guaranteed to still have a value of zero }
                if MatchOperand(taicpu(hp1).oper[0]^, 0) or
                  MatchOperand(taicpu(hp1).oper[0]^, taicpu(hp1).oper[1]^.reg) then
                  begin
                    hp2 := hp1;
                    UpdateUsedRegs(TmpUsedRegs, tai(hp1.Next));
                    while RegInUsedRegs(NR_DEFAULTFLAGS, TmpUsedRegs) and
                      GetNextInstruction(hp2, hp3) do
                      begin
                        { Don't continue modifying if the flags state is getting changed }
                        if RegModifiedByInstruction(NR_DEFAULTFLAGS, hp3) then
                          Break;

                        UpdateUsedRegs(TmpUsedRegs, tai(hp3.Next));
                        if MatchInstruction(hp3, A_Jcc, A_SETcc, A_CMOVcc, []) then
                          begin

                            if condition_in(C_E, taicpu(hp3).condition) or (taicpu(hp3).condition in [C_NC, C_NS, C_NO]) then
                              begin
                                { Condition is always true }
                                case taicpu(hp3).opcode of
                                  A_Jcc:
                                    begin
                                      DebugMsg(SPeepholeOptimization + 'Condition is always true (jump made unconditional)', hp3);
                                      { Check for jump shortcuts before we destroy the condition }
                                      DoJumpOptimizations(hp3, TempBool);
                                      MakeUnconditional(taicpu(hp3));
                                      Result := True;
                                    end;
                                  A_CMOVcc:
                                    begin
                                      DebugMsg(SPeepholeOptimization + 'Condition is always true (CMOVcc -> MOV)', hp3);
                                      taicpu(hp3).opcode := A_MOV;
                                      taicpu(hp3).condition := C_None;
                                      Result := True;
                                    end;
                                  A_SETcc:
                                    begin
                                      DebugMsg(SPeepholeOptimization + 'Condition is always true (changed to MOV 1)', hp3);
                                      { Convert "set(c) %reg" instruction to "movb 1,%reg" }
                                      taicpu(hp3).opcode := A_MOV;
                                      taicpu(hp3).ops := 2;
                                      taicpu(hp3).condition := C_None;
                                      taicpu(hp3).opsize := S_B;
                                      taicpu(hp3).loadreg(1,taicpu(hp3).oper[0]^.reg);
                                      taicpu(hp3).loadconst(0, 1);
                                      Result := True;
                                    end;
                                  else
                                    InternalError(2021090701);
                                end;
                              end
                            else if (taicpu(hp3).condition in [C_A, C_B, C_C, C_G, C_L, C_NE, C_NZ, C_O, C_S]) then
                              begin
                                { Condition is always false }
                                case taicpu(hp3).opcode of
                                  A_Jcc:
                                    begin
                                      DebugMsg(SPeepholeOptimization + 'Condition is always false (jump removed)', hp3);
                                      TAsmLabel(taicpu(hp3).oper[0]^.ref^.symbol).decrefs;
                                      RemoveInstruction(hp3);
                                      Result := True;
                                      { Since hp3 was deleted, hp2 must not be updated }
                                      Continue;
                                    end;
                                  A_CMOVcc:
                                    begin
                                      DebugMsg(SPeepholeOptimization + 'Condition is always false (conditional load removed)', hp3);
                                      RemoveInstruction(hp3);
                                      Result := True;
                                      { Since hp3 was deleted, hp2 must not be updated }
                                      Continue;
                                    end;
                                  A_SETcc:
                                    begin
                                      DebugMsg(SPeepholeOptimization + 'Condition is always false (changed to MOV 0)', hp3);
                                      { Convert "set(c) %reg" instruction to "movb 0,%reg" }
                                      taicpu(hp3).opcode := A_MOV;
                                      taicpu(hp3).ops := 2;
                                      taicpu(hp3).condition := C_None;
                                      taicpu(hp3).opsize := S_B;
                                      taicpu(hp3).loadreg(1,taicpu(hp3).oper[0]^.reg);
                                      taicpu(hp3).loadconst(0, 0);
                                      Result := True;
                                    end;
                                  else
                                    InternalError(2021090702);
                                end;
                              end
                            else
                              { Uncertain what to do - don't optimise (although optimise other conditional statements if present) }
                              DoOptimisation := False;
                          end;

                        hp2 := hp3;
                      end;

                    { Flags are still in use - don't optimise }
                    if DoOptimisation and RegInUsedRegs(NR_DEFAULTFLAGS, TmpUsedRegs) then
                      DoOptimisation := False;

                  end
                else
                  DoOptimisation := False;
              end;

            if DoOptimisation then
              begin
{$ifdef x86_64}
                { OR only supports 32-bit sign-extended constants for 64-bit
                  instructions, so compensate for this if the constant is
                  encoded as a value greater than or equal to 2^31 }
                if (taicpu(hp1).opsize = S_Q) and
                  (taicpu(hp1).oper[0]^.typ = top_const) and
                  (taicpu(hp1).oper[0]^.val >= $80000000) then
                  taicpu(hp1).oper[0]^.val := taicpu(hp1).oper[0]^.val or $FFFFFFFF00000000;
{$endif x86_64}

                DebugMsg(SPeepholeOptimization + 'MOV 0 / OR -> MOV', p);
                taicpu(hp1).opcode := A_MOV;
                RemoveCurrentP(p, hp1);
                Result := True;
                Exit;
              end;
          end;

        { Next instruction is also a MOV ? }
        if MatchInstruction(hp1,A_MOV,[taicpu(p).opsize]) then
          begin
            if MatchOpType(taicpu(p), top_const, top_ref) and
              MatchOpType(taicpu(hp1), top_const, top_ref) and
              TryConstMerge(p, hp1) then
              begin
                Result := True;

                { In case we have four byte writes in a row, check for 2 more
                  right now so we don't have to wait for another iteration of
                  pass 1
                }

                { If two byte-writes were merged, the opsize is now S_W, not S_B }
                case taicpu(p).opsize of
                  S_W:
                    begin
                      if GetNextInstruction(p, hp1) and
                        MatchInstruction(hp1, A_MOV, [S_B]) and
                        MatchOpType(taicpu(hp1), top_const, top_ref) and
                        GetNextInstruction(hp1, hp2) and
                        MatchInstruction(hp2, A_MOV, [S_B]) and
                        MatchOpType(taicpu(hp2), top_const, top_ref) and
                        { Try to merge the two bytes }
                        TryConstMerge(hp1, hp2) then
                          { Now try to merge the two words (hp2 will get deleted) }
                          TryConstMerge(p, hp1);
                    end;
                  S_L:
                    begin
                      { Though this only really benefits x86_64 and not i386, it
                        gets a potential optimisation done faster and hence
                        reduces the number of times OptPass1MOV is entered }
                      if GetNextInstruction(p, hp1) and
                        MatchInstruction(hp1, A_MOV, [S_W]) and
                        MatchOpType(taicpu(hp1), top_const, top_ref) and
                        GetNextInstruction(hp1, hp2) and
                        MatchInstruction(hp2, A_MOV, [S_W]) and
                        MatchOpType(taicpu(hp2), top_const, top_ref) and
                        { Try to merge the two words }
                        TryConstMerge(hp1, hp2) then
                          { This will always fail on i386, so don't bother
                            calling it unless we're doing x86_64 }
{$ifdef x86_64}
                          { Now try to merge the two longwords (hp2 will get deleted) }
                          TryConstMerge(p, hp1)
{$endif x86_64}
                          ;
                    end;
                  else
                    ;
                end;

                Exit;
              end;

            if (taicpu(p).oper[1]^.typ = top_reg) and
              MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[0]^) then
              begin
                { Remember that p_TargetReg contains taicpu(p).oper[1]^.reg }
                TransferUsedRegs(TmpUsedRegs);
                UpdateUsedRegs(TmpUsedRegs, tai(p.Next));
                { we have
                    mov x, %treg
                    mov %treg, y
                }
                if not(RegInOp(p_TargetReg, taicpu(hp1).oper[1]^)) then
                  if not(RegUsedAfterInstruction(p_TargetReg, hp1, TmpUsedRegs)) then
                    { we've got

                      mov x, %treg
                      mov %treg, y

                      with %treg is not used after }
                    case taicpu(p).oper[0]^.typ Of
                      { top_reg is covered by DeepMOVOpt }
                      top_const:
                        begin
                          { change
                              mov const, %treg
                              mov %treg, y

                              to

                              mov const, y
                          }
                          if (taicpu(hp1).oper[1]^.typ=top_reg) or
                            ((taicpu(p).oper[0]^.val>=low(longint)) and (taicpu(p).oper[0]^.val<=high(longint))) then
                            begin
                              if taicpu(hp1).oper[1]^.typ=top_reg then
                                AllocRegBetween(taicpu(hp1).oper[1]^.reg,p,hp1,usedregs);
                              taicpu(p).loadOper(1,taicpu(hp1).oper[1]^);
                              DebugMsg(SPeepholeOptimization + 'MovMov2Mov 5 done',p);
                              RemoveInstruction(hp1);
                              Result:=true;
                              Exit;
                            end;
                        end;
                      top_ref:
                        case taicpu(hp1).oper[1]^.typ of
                          top_reg:
                            begin
                              { change
                                   mov mem, %treg
                                   mov %treg, %reg

                                   to

                                   mov mem, %reg"
                              }
                              AllocRegBetween(taicpu(hp1).oper[1]^.reg,p,hp1,usedregs);
                              taicpu(p).loadreg(1, taicpu(hp1).oper[1]^.reg);
                              DebugMsg(SPeepholeOptimization + 'MovMov2Mov 3 done',p);
                              RemoveInstruction(hp1);
                              Result:=true;
                              Exit;
                            end;
                          top_ref:
                            begin
{$ifdef x86_64}
                              { Look for the following to simplify:

                                  mov x(mem1), %reg
                                  mov %reg, y(mem2)
                                  mov x+8(mem1), %reg
                                  mov %reg, y+8(mem2)

                                Change to:
                                  movdqu x(mem1), %xmmreg
                                  movdqu %xmmreg, y(mem2)

                                ...but only as long as the memory blocks don't overlap
                              }
                              SourceRef := taicpu(p).oper[0]^.ref^;
                              TargetRef := taicpu(hp1).oper[1]^.ref^;
                              if (taicpu(p).opsize = S_Q) and
                                GetNextInstruction(hp1, hp2) and
                                MatchInstruction(hp2, A_MOV, [taicpu(p).opsize]) and
                                MatchOpType(taicpu(hp2), top_ref, top_reg) then
                                begin
                                  { Delay calling GetNextInstruction(hp2, hp3) for as long as possible }

                                  UpdateUsedRegs(TmpUsedRegs, tai(hp1.Next));

                                  Inc(SourceRef.offset, 8);

                                  if UseAVX then
                                    begin
                                      MovAligned :=  A_VMOVDQA;
                                      MovUnaligned := A_VMOVDQU;
                                    end
                                  else
                                    begin
                                      MovAligned := A_MOVDQA;
                                      MovUnaligned := A_MOVDQU;
                                    end;

                                  if RefsEqual(SourceRef, taicpu(hp2).oper[0]^.ref^) and
                                    not RefsMightOverlap(taicpu(p).oper[0]^.ref^, TargetRef, 16) then
                                    begin
                                      UpdateUsedRegs(TmpUsedRegs, tai(hp2.Next));
                                      Inc(TargetRef.offset, 8);
                                      if GetNextInstruction(hp2, hp3) and
                                        MatchInstruction(hp3, A_MOV, [taicpu(p).opsize]) and
                                        MatchOpType(taicpu(hp3), top_reg, top_ref) and
                                        (taicpu(hp2).oper[1]^.reg = taicpu(hp3).oper[0]^.reg) and
                                        RefsEqual(TargetRef, taicpu(hp3).oper[1]^.ref^) and
                                        not RegUsedAfterInstruction(taicpu(hp2).oper[1]^.reg, hp3, TmpUsedRegs) then
                                        begin
                                          NewMMReg := GetMMRegisterBetween(R_SUBMMX, UsedRegs, p, hp3);
                                          if NewMMReg <> NR_NO then
                                            begin
                                              { Remember that the offsets are 8 ahead }
                                              if ((SourceRef.offset mod 16) = 8) and
                                                (
                                                  { Base pointer is always aligned (stack pointer won't be if there's no stack frame) }
                                                  (SourceRef.base = current_procinfo.framepointer) or
                                                  ((SourceRef.alignment >= 16) and ((SourceRef.alignment mod 16) = 0))
                                                ) then
                                                taicpu(p).opcode := MovAligned
                                              else
                                                taicpu(p).opcode := MovUnaligned;

                                              taicpu(p).opsize := S_XMM;
                                              taicpu(p).oper[1]^.reg := NewMMReg;

                                              if ((TargetRef.offset mod 16) = 8) and
                                                (
                                                  { Base pointer is always aligned (stack pointer won't be if there's no stack frame) }
                                                  (TargetRef.base = current_procinfo.framepointer) or
                                                  ((TargetRef.alignment >= 16) and ((TargetRef.alignment mod 16) = 0))
                                                ) then
                                                taicpu(hp1).opcode := MovAligned
                                              else
                                                taicpu(hp1).opcode := MovUnaligned;

                                              taicpu(hp1).opsize := S_XMM;
                                              taicpu(hp1).oper[0]^.reg := NewMMReg;

                                              DebugMsg(SPeepholeOptimization + 'Used ' + debug_regname(NewMMReg) + ' to merge a pair of memory moves (MovMovMovMov2MovdqMovdq 1)', p);

                                              RemoveInstruction(hp2);
                                              RemoveInstruction(hp3);
                                              Result := True;
                                              Exit;
                                            end;
                                        end;
                                    end
                                  else
                                    begin
                                      { See if the next references are 8 less rather than 8 greater }

                                      Dec(SourceRef.offset, 16); { -8 the other way }
                                      if RefsEqual(SourceRef, taicpu(hp2).oper[0]^.ref^) then
                                        begin
                                          UpdateUsedRegs(TmpUsedRegs, tai(hp2.Next));
                                          Dec(TargetRef.offset, 8); { Only 8, not 16, as it wasn't incremented unlike SourceRef }
                                          if not RefsMightOverlap(SourceRef, TargetRef, 16) and
                                            GetNextInstruction(hp2, hp3) and
                                            MatchInstruction(hp3, A_MOV, [taicpu(p).opsize]) and
                                            MatchOpType(taicpu(hp3), top_reg, top_ref) and
                                            (taicpu(hp2).oper[1]^.reg = taicpu(hp3).oper[0]^.reg) and
                                            RefsEqual(TargetRef, taicpu(hp3).oper[1]^.ref^) and
                                            not RegUsedAfterInstruction(taicpu(hp2).oper[1]^.reg, hp3, TmpUsedRegs) then
                                            begin
                                              NewMMReg := GetMMRegisterBetween(R_SUBMMX, UsedRegs, p, hp3);
                                              if NewMMReg <> NR_NO then
                                                begin
                                                  { hp2 and hp3 are the starting offsets, so mod = 0 this time }
                                                  if ((SourceRef.offset mod 16) = 0) and
                                                    (
                                                      { Base pointer is always aligned (stack pointer won't be if there's no stack frame) }
                                                      (SourceRef.base = current_procinfo.framepointer) or
                                                      ((SourceRef.alignment >= 16) and ((SourceRef.alignment mod 16) = 0))
                                                    ) then
                                                    taicpu(hp2).opcode := MovAligned
                                                  else
                                                    taicpu(hp2).opcode := MovUnaligned;

                                                  taicpu(hp2).opsize := S_XMM;
                                                  taicpu(hp2).oper[1]^.reg := NewMMReg;

                                                  if ((TargetRef.offset mod 16) = 0) and
                                                    (
                                                      { Base pointer is always aligned (stack pointer won't be if there's no stack frame) }
                                                      (TargetRef.base = current_procinfo.framepointer) or
                                                      ((TargetRef.alignment >= 16) and ((TargetRef.alignment mod 16) = 0))
                                                    ) then
                                                    taicpu(hp3).opcode := MovAligned
                                                  else
                                                    taicpu(hp3).opcode := MovUnaligned;

                                                  taicpu(hp3).opsize := S_XMM;
                                                  taicpu(hp3).oper[0]^.reg := NewMMReg;

                                                  DebugMsg(SPeepholeOptimization + 'Used ' + debug_regname(NewMMReg) + ' to merge a pair of memory moves (MovMovMovMov2MovdqMovdq 2)', p);

                                                  RemoveInstruction(hp1);
                                                  RemoveCurrentP(p, hp2);
                                                  Result := True;
                                                  Exit;
                                                end;
                                            end;
                                        end;
                                    end;
                                end;
{$endif x86_64}
                            end;
                          else
                            { The write target should be a reg or a ref }
                            InternalError(2021091601);
                        end;
                      else
                        ;
                    end
                  else
                    { %treg is used afterwards, but all eventualities
                      other than the first MOV instruction being a constant
                      are covered by DeepMOVOpt, so only check for that }
                    if (taicpu(p).oper[0]^.typ = top_const) and
                      (
                        { For MOV operations, a size saving is only made if the register/const is byte-sized }
                        not (cs_opt_size in current_settings.optimizerswitches) or
                        (taicpu(hp1).opsize = S_B)
                      ) and
                      (
                        (taicpu(hp1).oper[1]^.typ = top_reg) or
                        ((taicpu(p).oper[0]^.val >= low(longint)) and (taicpu(p).oper[0]^.val <= high(longint)))
                      ) then
                      begin
                        DebugMsg(SPeepholeOptimization + debug_operstr(taicpu(hp1).oper[0]^) + ' = $' + debug_tostr(taicpu(p).oper[0]^.val) + '; changed to minimise pipeline stall (MovMov2Mov 6b)',hp1);
                        taicpu(hp1).loadconst(0, taicpu(p).oper[0]^.val);
                      end;
              end;
            if (taicpu(hp1).oper[0]^.typ = taicpu(p).oper[1]^.typ) and
               (taicpu(hp1).oper[1]^.typ = taicpu(p).oper[0]^.typ) then
                {  mov reg1, mem1     or     mov mem1, reg1
                   mov mem2, reg2            mov reg2, mem2}
              begin
                if OpsEqual(taicpu(hp1).oper[1]^,taicpu(p).oper[0]^) then
                  { mov reg1, mem1     or     mov mem1, reg1
                    mov mem2, reg1            mov reg2, mem1}
                  begin
                    if OpsEqual(taicpu(hp1).oper[0]^,taicpu(p).oper[1]^) then
                      { Removes the second statement from
                        mov reg1, mem1/reg2
                        mov mem1/reg2, reg1 }
                      begin
                        if taicpu(p).oper[0]^.typ=top_reg then
                          AllocRegBetween(taicpu(p).oper[0]^.reg,p,hp1,usedregs);
                        DebugMsg(SPeepholeOptimization + 'MovMov2Mov 1',p);
                        RemoveInstruction(hp1);
                        Result:=true;
                        exit;
                      end
                    else
                      begin
                        TransferUsedRegs(TmpUsedRegs);
                        UpdateUsedRegs(TmpUsedRegs, tai(hp1.next));
                        if (taicpu(p).oper[1]^.typ = top_ref) and
                          { mov reg1, mem1
                            mov mem2, reg1 }
                           (taicpu(hp1).oper[0]^.ref^.refaddr = addr_no) and
                           GetNextInstruction(hp1, hp2) and
                           MatchInstruction(hp2,A_CMP,[taicpu(p).opsize]) and
                           OpsEqual(taicpu(p).oper[1]^,taicpu(hp2).oper[0]^) and
                           OpsEqual(taicpu(p).oper[0]^,taicpu(hp2).oper[1]^) and
                           not(RegUsedAfterInstruction(taicpu(p).oper[0]^.reg, hp2, TmpUsedRegs)) then
                           { change                   to
                             mov reg1, mem1           mov reg1, mem1
                             mov mem2, reg1           cmp reg1, mem2
                             cmp mem1, reg1
                           }
                          begin
                            RemoveInstruction(hp2);
                            taicpu(hp1).opcode := A_CMP;
                            taicpu(hp1).loadref(1,taicpu(hp1).oper[0]^.ref^);
                            taicpu(hp1).loadreg(0,taicpu(p).oper[0]^.reg);
                            AllocRegBetween(taicpu(p).oper[0]^.reg,p,hp1,UsedRegs);
                            DebugMsg(SPeepholeOptimization + 'MovMovCmp2MovCmp done',hp1);
                          end;
                      end;
                  end
                else if (taicpu(p).oper[1]^.typ=top_ref) and
                  OpsEqual(taicpu(hp1).oper[0]^,taicpu(p).oper[1]^) then
                  begin
                    AllocRegBetween(taicpu(p).oper[0]^.reg,p,hp1,UsedRegs);
                    taicpu(hp1).loadreg(0,taicpu(p).oper[0]^.reg);
                    DebugMsg(SPeepholeOptimization + 'MovMov2MovMov1 done',p);
                  end
                else
                  begin
                    TransferUsedRegs(TmpUsedRegs);
                    if GetNextInstruction(hp1, hp2) and
                      MatchOpType(taicpu(p),top_ref,top_reg) and
                      MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[0]^) and
                      (taicpu(hp1).oper[1]^.typ = top_ref) and
                      MatchInstruction(hp2,A_MOV,[taicpu(p).opsize]) and
                      MatchOpType(taicpu(hp2),top_ref,top_reg) and
                      RefsEqual(taicpu(hp2).oper[0]^.ref^, taicpu(hp1).oper[1]^.ref^)  then
                      if not RegInRef(taicpu(hp2).oper[1]^.reg,taicpu(hp2).oper[0]^.ref^) and
                         not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp1,tmpUsedRegs)) then
                        {   mov mem1, %reg1
                            mov %reg1, mem2
                            mov mem2, reg2
                         to:
                            mov mem1, reg2
                            mov reg2, mem2}
                        begin
                          AllocRegBetween(taicpu(hp2).oper[1]^.reg,p,hp2,usedregs);
                          DebugMsg(SPeepholeOptimization + 'MovMovMov2MovMov 1 done',p);
                          taicpu(p).loadoper(1,taicpu(hp2).oper[1]^);
                          taicpu(hp1).loadoper(0,taicpu(hp2).oper[1]^);
                          RemoveInstruction(hp2);
                          Result := True;
                        end
{$ifdef i386}
                      { this is enabled for i386 only, as the rules to create the reg sets below
                        are too complicated for x86-64, so this makes this code too error prone
                        on x86-64
                      }
                      else if (taicpu(p).oper[1]^.reg <> taicpu(hp2).oper[1]^.reg) and
                        not(RegInRef(taicpu(p).oper[1]^.reg,taicpu(p).oper[0]^.ref^)) and
                        not(RegInRef(taicpu(hp2).oper[1]^.reg,taicpu(hp2).oper[0]^.ref^)) then
                        {   mov mem1, reg1         mov mem1, reg1
                            mov reg1, mem2         mov reg1, mem2
                            mov mem2, reg2         mov mem2, reg1
                         to:                    to:
                            mov mem1, reg1         mov mem1, reg1
                            mov mem1, reg2         mov reg1, mem2
                            mov reg1, mem2

                         or (if mem1 depends on reg1
                      and/or if mem2 depends on reg2)
                         to:
                             mov mem1, reg1
                             mov reg1, mem2
                             mov reg1, reg2
                        }
                        begin
                          taicpu(hp1).loadRef(0,taicpu(p).oper[0]^.ref^);
                          taicpu(hp1).loadReg(1,taicpu(hp2).oper[1]^.reg);
                          taicpu(hp2).loadRef(1,taicpu(hp2).oper[0]^.ref^);
                          taicpu(hp2).loadReg(0,taicpu(p).oper[1]^.reg);
                          AllocRegBetween(taicpu(p).oper[1]^.reg,p,hp2,usedregs);
                          if (taicpu(p).oper[0]^.ref^.base <> NR_NO) and
                             (getsupreg(taicpu(p).oper[0]^.ref^.base) in [RS_EAX,RS_EBX,RS_ECX,RS_EDX,RS_ESI,RS_EDI]) then
                            AllocRegBetween(taicpu(p).oper[0]^.ref^.base,p,hp2,usedregs);
                          if (taicpu(p).oper[0]^.ref^.index <> NR_NO) and
                             (getsupreg(taicpu(p).oper[0]^.ref^.index) in [RS_EAX,RS_EBX,RS_ECX,RS_EDX,RS_ESI,RS_EDI]) then
                            AllocRegBetween(taicpu(p).oper[0]^.ref^.index,p,hp2,usedregs);
                        end
                      else if (taicpu(hp1).Oper[0]^.reg <> taicpu(hp2).Oper[1]^.reg) then
                        begin
                          taicpu(hp2).loadReg(0,taicpu(hp1).Oper[0]^.reg);
                          AllocRegBetween(taicpu(p).oper[1]^.reg,p,hp2,usedregs);
                        end
                      else
                        begin
                          RemoveInstruction(hp2);
                        end
{$endif i386}
                        ;
                  end;
              end
            { movl [mem1],reg1
              movl [mem1],reg2

              to

              movl [mem1],reg1
              movl reg1,reg2
             }
             else if not CheckMovMov2MovMov2(p, hp1) and

            {   movl const1,[mem1]
                movl [mem1],reg1

                to

                movl const1,reg1
                movl reg1,[mem1]
            }
                 MatchOpType(Taicpu(p),top_const,top_ref) and
                 MatchOpType(Taicpu(hp1),top_ref,top_reg) and
                 (taicpu(p).opsize = taicpu(hp1).opsize) and
                 RefsEqual(taicpu(hp1).oper[0]^.ref^,taicpu(p).oper[1]^.ref^) and
                 not(RegInRef(taicpu(hp1).oper[1]^.reg,taicpu(hp1).oper[0]^.ref^)) then
              begin
                AllocRegBetween(taicpu(hp1).oper[1]^.reg,p,hp1,usedregs);
                taicpu(hp1).loadReg(0,taicpu(hp1).oper[1]^.reg);
                taicpu(hp1).loadRef(1,taicpu(p).oper[1]^.ref^);
                taicpu(p).loadReg(1,taicpu(hp1).oper[0]^.reg);
                taicpu(hp1).fileinfo := taicpu(p).fileinfo;
                DebugMsg(SPeepholeOptimization + 'MovMov2MovMov 1',p);
                Result:=true;
                exit;
              end;

              { mov x,reg1; mov y,reg1 -> mov y,reg1 is handled by the Mov2Nop 5 optimisation }

            { Change:
                movl %reg1,%reg2
                movl x(%reg1),%reg1  (If something other than %reg1 is written to, DeepMOVOpt would have caught it)
                movl x(%reg2),%regX  (%regX can be %reg2 or something else)
              To:
                movl %reg1,%reg2 (if %regX = %reg2, then remove this instruction)
                movl x(%reg1),%reg1
                movl %reg1,%regX
            }
            if MatchOpType(taicpu(p), top_reg, top_reg) then
              begin
                p_SourceReg := taicpu(p).oper[0]^.reg;
                { Remember that p_TargetReg contains taicpu(p).oper[1]^.reg }

                if (taicpu(hp1).oper[0]^.typ = top_ref) { The other operand will be a register } and
                  (taicpu(hp1).oper[1]^.reg = p_SourceReg) and
                  RegInRef(p_SourceReg, taicpu(hp1).oper[0]^.ref^) and
                  GetNextInstruction(hp1, hp2) and
                  MatchInstruction(hp2, A_MOV, [taicpu(p).opsize]) and
                  (taicpu(hp2).oper[0]^.typ = top_ref) { The other operand will be a register } then
                  begin
                    SourceRef := taicpu(hp2).oper[0]^.ref^;
                    if RegInRef(p_TargetReg, SourceRef) and
                      { If %reg1 also appears in the second reference, then it will
                        not refer to the same memory block as the first reference }
                      not RegInRef(p_SourceReg, SourceRef) then
                      begin
                        { Check to see if the references match if %reg2 is changed to %reg1 }
                        if SourceRef.base = p_TargetReg then
                          SourceRef.base := p_SourceReg;

                        if SourceRef.index = p_TargetReg then
                          SourceRef.index := p_SourceReg;

                        { RefsEqual also checks to ensure both references are non-volatile }
                        if RefsEqual(taicpu(hp1).oper[0]^.ref^, SourceRef) then
                          begin
                            taicpu(hp2).loadreg(0, p_SourceReg);

                            DebugMsg(SPeepholeOptimization + 'Optimised register duplication and memory read (MovMovMov2MovMovMov)', p);
                            Result := True;
                            if taicpu(hp2).oper[1]^.reg = p_TargetReg then
                              begin
                                DebugMsg(SPeepholeOptimization + 'Mov2Nop 5a done', p);
                                RemoveCurrentP(p, hp1);
                                Exit;
                              end
                            else
                              begin
                                { Check to see if %reg2 is no longer in use }
                                TransferUsedRegs(TmpUsedRegs);
                                UpdateUsedRegs(TmpUsedRegs, tai(p.Next));
                                UpdateUsedRegs(TmpUsedRegs, tai(hp1.Next));

                                if not RegUsedAfterInstruction(p_TargetReg, hp2, TmpUsedRegs) then
                                  begin
                                    DebugMsg(SPeepholeOptimization + 'Mov2Nop 5b done', p);
                                    RemoveCurrentP(p, hp1);
                                    Exit;
                                  end;
                              end;
                            { If we reach this point, p and hp1 weren't actually modified,
                              so we can do a bit more work on this pass }
                          end;
                      end;
                  end;
              end;
          end;

{$ifdef x86_64}
        { Change:
            movl %reg1l,%reg2l
            movq %reg2q,%reg3q  (%reg1 <> %reg3)

          To:
            movl %reg1l,%reg2l
            movl %reg1l,%reg3l  (Upper 32 bits of %reg3q will be zero)

          If %reg1 = %reg3, convert to:
            movl %reg1l,%reg2l
            andl %reg1l,%reg1l
        }
        if (taicpu(p).opsize = S_L) and MatchInstruction(hp1,A_MOV,[S_Q]) and
          MatchOpType(taicpu(p), top_reg, top_reg) and
          MatchOpType(taicpu(hp1), top_reg, top_reg) and
          SuperRegistersEqual(taicpu(p).oper[1]^.reg, taicpu(hp1).oper[0]^.reg) then
          begin
            TransferUsedRegs(TmpUsedRegs);
            UpdateUsedRegs(TmpUsedRegs, tai(p.Next));

            taicpu(hp1).opsize := S_L;
            taicpu(hp1).loadreg(0, taicpu(p).oper[0]^.reg);
            setsubreg(taicpu(hp1).oper[1]^.reg, R_SUBD);

            AllocRegBetween(taicpu(p).oper[0]^.reg, p, hp1, UsedRegs);

            if (taicpu(p).oper[0]^.reg = taicpu(hp1).oper[1]^.reg) then
              begin
                { %reg1 = %reg3 }
                DebugMsg(SPeepholeOptimization + 'Made 32-to-64-bit zero extension more efficient (MovlMovq2MovlAndl 1)', hp1);
                taicpu(hp1).opcode := A_AND;
              end
            else
              begin
                { %reg1 <> %reg3 }
                DebugMsg(SPeepholeOptimization + 'Made 32-to-64-bit zero extension more efficient (MovlMovq2MovlMovl 1)', hp1);
              end;

            if not RegUsedAfterInstruction(taicpu(p).oper[1]^.reg, hp1, TmpUsedRegs) then
              begin
                DebugMsg(SPeepholeOptimization + 'Mov2Nop 8 done', p);
                RemoveCurrentP(p, hp1);
                Result := True;
                Exit;
              end
            else
              begin
                { Initial instruction wasn't actually changed }
                Include(OptsToCheck, aoc_ForceNewIteration);

                { if %reg1 = %reg3, don't do the long-distance lookahead that
                  appears below since %reg1 has technically changed }
                if taicpu(hp1).opcode = A_AND then
                  Exit;
              end;
          end;
{$endif x86_64}

        { search further than the next instruction for a mov (as long as it's not a jump) }
        if not is_calljmpuncondret(taicpu(hp1).opcode) and
          { check as much as possible before the expensive GetNextInstructionUsingRegCond call }
          (taicpu(p).oper[1]^.typ = top_reg) and
          (taicpu(p).oper[0]^.typ in [top_reg,top_const]) and
          not RegModifiedByInstruction(taicpu(p).oper[1]^.reg, hp1) then
          begin
            { we work with hp2 here, so hp1 can be still used later on when
              checking for GetNextInstruction_p }
            hp3 := hp1;

            { Initialise CrossJump (if it becomes True at any point, it will remain True) }
            CrossJump := (taicpu(hp1).opcode = A_Jcc);

            { Remember that p_TargetReg contains taicpu(p).oper[1]^.reg }

            TransferUsedRegs(TmpUsedRegs);
            UpdateUsedRegs(TmpUsedRegs, tai(p.Next));

            if NotFirstIteration then
              JumpTracking := TLinkedList.Create
            else
              JumpTracking := nil;

            while GetNextInstructionUsingRegCond(hp3,hp2,p_TargetReg,JumpTracking,CrossJump) and
              { GetNextInstructionUsingRegCond only searches one instruction ahead unless -O3 is specified }
              (hp2.typ=ait_instruction) do
              begin
                case taicpu(hp2).opcode of
                  A_POP:
                    if MatchOperand(taicpu(hp2).oper[0]^,p_TargetReg) then
                      begin
                        if not CrossJump and
                          not RegUsedBetween(p_TargetReg, p, hp2) then
                          begin
                            { We can remove the original MOV since the register
                              wasn't used between it and its popping from the stack }
                            DebugMsg(SPeepholeOptimization + 'Mov2Nop 3c done',p);
                            RemoveCurrentp(p, hp1);
                            Result := True;
                            JumpTracking.Free;
                            Exit;
                          end;
                        { Can't go any further }
                        Break;
                      end;
                  A_MOV:
                    if MatchOperand(taicpu(hp2).oper[0]^,p_TargetReg) and
                      ((taicpu(p).oper[0]^.typ=top_const) or
                       ((taicpu(p).oper[0]^.typ=top_reg) and
                        not(RegModifiedBetween(taicpu(p).oper[0]^.reg, p, hp2))
                       )
                      ) then
                      begin
                        { we have
                            mov x, %treg
                            mov %treg, y
                        }

                        { We don't need to call UpdateUsedRegs for every instruction between
                          p and hp2 because the register we're concerned about will not
                          become deallocated (otherwise GetNextInstructionUsingReg would
                          have stopped at an earlier instruction). [Kit] }

                        TempRegUsed :=
                          CrossJump { Assume the register is in use if it crossed a conditional jump } or
                          RegReadByInstruction(p_TargetReg, hp3) or
                          RegUsedAfterInstruction(p_TargetReg, hp2, TmpUsedRegs);

                        case taicpu(p).oper[0]^.typ Of
                          top_reg:
                            begin
                              { change
                                  mov %reg, %treg
                                  mov %treg, y

                                  to

                                  mov %reg, y
                              }
                              p_SourceReg := taicpu(p).oper[0]^.reg; { Saves on a handful of pointer dereferences }
                              RegName1 := debug_regname(taicpu(hp2).oper[0]^.reg);
                              if MatchOperand(taicpu(hp2).oper[1]^, p_SourceReg) then
                                begin
                                  { %reg = y - remove hp2 completely (doing it here instead of relying on
                                    the "mov %reg,%reg" optimisation might cut down on a pass iteration) }

                                  if TempRegUsed then
                                    begin
                                      DebugMsg(SPeepholeOptimization + debug_regname(p_SourceReg) + ' = ' + RegName1 + '; removed unnecessary instruction (MovMov2MovNop 6b}',hp2);
                                      AllocRegBetween(p_SourceReg, p, hp2, UsedRegs);
                                      { Set the start of the next GetNextInstructionUsingRegCond search
                                        to start at the entry right before hp2 (which is about to be removed) }
                                      hp3 := tai(hp2.Previous);
                                      RemoveInstruction(hp2);

                                      Include(OptsToCheck, aoc_ForceNewIteration);

                                      { See if there's more we can optimise }
                                      Continue;
                                    end
                                  else
                                    begin
                                      RemoveInstruction(hp2);

                                      { We can remove the original MOV too }
                                      DebugMsg(SPeepholeOptimization + 'MovMov2NopNop 6b done',p);
                                      RemoveCurrentP(p, hp1);
                                      Result:=true;
                                      JumpTracking.Free;
                                      Exit;
                                    end;
                                end
                              else
                                begin
                                  AllocRegBetween(p_SourceReg, p, hp2, UsedRegs);
                                  taicpu(hp2).loadReg(0, p_SourceReg);

                                  DebugMsg(SPeepholeOptimization + RegName1 + ' = ' + debug_regname(p_SourceReg) + '; changed to minimise pipeline stall (MovMov2Mov 6a}',hp2);

                                  { Check to see if the register also appears in the reference }
                                  if (taicpu(hp2).oper[1]^.typ = top_ref) then
                                    ReplaceRegisterInRef(taicpu(hp2).oper[1]^.ref^, p_TargetReg, p_SourceReg);

                                  { ReplaceRegisterInRef won't actually replace the register if it's a different size }
                                  if not RegInOp(p_TargetReg, taicpu(hp2).oper[1]^) then
                                    begin
                                      { Don't remove the first instruction if the temporary register is in use }
                                      if not TempRegUsed then
                                        begin
                                          DebugMsg(SPeepholeOptimization + 'MovMov2Mov 6 done',p);
                                          RemoveCurrentP(p, hp1);
                                          Result:=true;
                                          JumpTracking.Free;
                                          Exit;
                                        end;

                                      { No need to set Result to True here. If there's another instruction later
                                        on that can be optimised, it will be detected when the main Pass 1 loop
                                        reaches what is now hp2 and passes it through OptPass1MOV. [Kit] }
                                      hp3 := hp2;
                                      Continue;
                                    end;
                                end;
                            end;
                          top_const:
                            if not (cs_opt_size in current_settings.optimizerswitches) or (taicpu(hp2).opsize = S_B) then
                              begin
                                { change
                                    mov const, %treg
                                    mov %treg, y

                                    to

                                    mov const, y
                                }
                                if (taicpu(hp2).oper[1]^.typ=top_reg) or
                                  ((taicpu(p).oper[0]^.val>=low(longint)) and (taicpu(p).oper[0]^.val<=high(longint))) then
                                  begin
                                    RegName1 := debug_regname(taicpu(hp2).oper[0]^.reg);
                                    taicpu(hp2).loadOper(0,taicpu(p).oper[0]^);

                                    if TempRegUsed then
                                      begin
                                        { Don't remove the first instruction if the temporary register is in use }
                                        DebugMsg(SPeepholeOptimization + RegName1 + ' = ' + debug_tostr(taicpu(p).oper[0]^.val) + '; changed to minimise pipeline stall (MovMov2Mov 7a)',hp2);

                                        { No need to set Result to True. If there's another instruction later on
                                          that can be optimised, it will be detected when the main Pass 1 loop
                                          reaches what is now hp2 and passes it through OptPass1MOV. [Kit] };
                                      end
                                    else
                                      begin
                                        DebugMsg(SPeepholeOptimization + 'MovMov2Mov 7 done',p);
                                        RemoveCurrentP(p, hp1);
                                        Result:=true;
                                        Exit;
                                      end;
                                  end;
                              end;
                            else
                              Internalerror(2019103001);
                          end;
                      end
                    else if MatchOperand(taicpu(hp2).oper[1]^, p_TargetReg) then
                      begin
                        if not CrossJump and
                          not RegUsedBetween(p_TargetReg, p, hp2) and
                          not RegReadByInstruction(p_TargetReg, hp2) then
                          begin
                            { Register is not used before it is overwritten }
                            DebugMsg(SPeepholeOptimization + 'Mov2Nop 3a done',p);
                            RemoveCurrentp(p, hp1);
                            Result := True;
                            Exit;
                          end;

                        if (taicpu(p).oper[0]^.typ = top_const) and
                          (taicpu(hp2).oper[0]^.typ = top_const) then
                          begin
                            if taicpu(p).oper[0]^.val = taicpu(hp2).oper[0]^.val then
                              begin
                                { Same value - register hasn't changed }
                                DebugMsg(SPeepholeOptimization + 'Mov2Nop 2 done', hp2);
                                RemoveInstruction(hp2);

                                Include(OptsToCheck, aoc_ForceNewIteration);

                                { See if there's more we can optimise }
                                Continue;
                              end;
                          end;
{$ifdef x86_64}
                      end
                    { Change:
                        movl %reg1l,%reg2l
                        ...
                        movq %reg2q,%reg3q  (%reg1 <> %reg3)

                      To:
                        movl %reg1l,%reg2l
                        ...
                        movl %reg1l,%reg3l  (Upper 32 bits of %reg3q will be zero)

                      If %reg1 = %reg3, convert to:
                        movl %reg1l,%reg2l
                        ...
                        andl %reg1l,%reg1l
                    }
                    else if (taicpu(p).opsize = S_L) and MatchInstruction(hp2,A_MOV,[S_Q]) and
                      (taicpu(p).oper[0]^.typ = top_reg) and
                      MatchOpType(taicpu(hp2), top_reg, top_reg) and
                      SuperRegistersEqual(p_TargetReg, taicpu(hp2).oper[0]^.reg) and
                      not RegModifiedBetween(p_TargetReg, p, hp2) then
                      begin
                        TempRegUsed :=
                          CrossJump { Assume the register is in use if it crossed a conditional jump } or
                          RegReadByInstruction(p_TargetReg, hp3) or
                          RegUsedAfterInstruction(p_TargetReg, hp2, TmpUsedRegs);

                        taicpu(hp2).opsize := S_L;
                        taicpu(hp2).loadreg(0, taicpu(p).oper[0]^.reg);
                        setsubreg(taicpu(hp2).oper[1]^.reg, R_SUBD);

                        AllocRegBetween(taicpu(p).oper[0]^.reg, p, hp2, UsedRegs);

                        if (taicpu(p).oper[0]^.reg = taicpu(hp2).oper[1]^.reg) then
                          begin
                            { %reg1 = %reg3 }
                            DebugMsg(SPeepholeOptimization + 'Made 32-to-64-bit zero extension more efficient (MovlMovq2MovlAndl 2)', hp2);
                            taicpu(hp2).opcode := A_AND;
                          end
                        else
                          begin
                            { %reg1 <> %reg3 }
                            DebugMsg(SPeepholeOptimization + 'Made 32-to-64-bit zero extension more efficient (MovlMovq2MovlMovl 2)', hp2);
                          end;

                        if not TempRegUsed then
                          begin
                            DebugMsg(SPeepholeOptimization + 'Mov2Nop 8a done', p);
                            RemoveCurrentP(p, hp1);
                            Result := True;
                            Exit;
                          end
                        else
                          begin
                            { Initial instruction wasn't actually changed }
                            Include(OptsToCheck, aoc_ForceNewIteration);

                            { if %reg1 = %reg3, don't do the long-distance lookahead that
                              appears below since %reg1 has technically changed }
                            if taicpu(hp2).opcode = A_AND then
                              Break;
                          end;
{$endif x86_64}
                      end
                    else if (taicpu(hp2).oper[0]^.typ = top_ref) and
                      GetNextInstruction(hp2, hp4) and
                      (hp4.typ = ait_instruction) and (taicpu(hp4).opcode = A_MOV) then
                      { Optimise the following first:
                          movl [mem1],reg1
                          movl [mem1],reg2

                          to

                          movl [mem1],reg1
                          movl reg1,reg2

                        If [mem1] contains the target register and reg1 is the
                        the source register, this optimisation will get missed
                        and produce less efficient code later on.
                      }
                      if CheckMovMov2MovMov2(hp2, hp4) then
                        { Initial instruction wasn't actually changed }
                        Include(OptsToCheck, aoc_ForceNewIteration);

                  A_MOVZX, A_MOVSX{$ifdef x86_64}, A_MOVSXD{$endif x86_64}:
                    if MatchOpType(taicpu(hp2), top_reg, top_reg) and
                      MatchOperand(taicpu(hp2).oper[0]^, p_TargetReg) and
                      SuperRegistersEqual(taicpu(hp2).oper[1]^.reg, p_TargetReg) then
                      begin
                        {
                          Change from:
                            mov    ###, %reg
                            ...
                            movs/z %reg,%reg  (Same register, just different sizes)

                          To:
                            movs/z ###, %reg  (Longer version)
                            ...
                            (remove)
                        }
                        DebugMsg(SPeepholeOptimization + 'MovMovs/z2Mov/s/z done', p);
                        taicpu(p).oper[1]^.reg := taicpu(hp2).oper[1]^.reg;

                        { Keep the first instruction as mov if ### is a constant }
                        if taicpu(p).oper[0]^.typ = top_const then
                          taicpu(p).opsize := reg2opsize(taicpu(hp2).oper[1]^.reg)
                        else
                          begin
                            taicpu(p).opcode := taicpu(hp2).opcode;
                            taicpu(p).opsize := taicpu(hp2).opsize;
                          end;

                        DebugMsg(SPeepholeOptimization + 'Removed movs/z instruction and extended earlier write (MovMovs/z2Mov/s/z)', hp2);
                        AllocRegBetween(taicpu(hp2).oper[1]^.reg, p, hp2, UsedRegs);
                        RemoveInstruction(hp2);

                        Result := True;
                        JumpTracking.Free;
                        Exit;
                      end;
                  else
                    { Move down to the if-block below };
                end;

                { Also catches MOV/S/Z instructions that aren't modified }
                if taicpu(p).oper[0]^.typ = top_reg then
                  begin
                    p_SourceReg := taicpu(p).oper[0]^.reg;
                    if
                      not RegModifiedByInstruction(p_SourceReg, hp3) and
                      not RegModifiedBetween(p_SourceReg, hp3, hp2) and
                      DeepMOVOpt(taicpu(p), taicpu(hp2)) then
                      begin
                        Result := True;

                        { Just in case something didn't get modified (e.g. an
                          implicit register).  Also, if it does read from this
                          register, then there's no longer an advantage to
                          changing the register on subsequent instructions.}
                        if not RegReadByInstruction(p_TargetReg, hp2) then
                          begin
                            { If a conditional jump was crossed, do not delete
                              the original MOV no matter what }
                            if not CrossJump and
                              { RegEndOfLife returns True if the register is
                                deallocated before the next instruction or has
                                been loaded with a new value }
                              RegEndOfLife(p_TargetReg, taicpu(hp2)) then
                              begin
                                { We can remove the original MOV }
                                DebugMsg(SPeepholeOptimization + 'Mov2Nop 3b done',p);
                                RemoveCurrentp(p, hp1);
                                JumpTracking.Free;
                                Result := True;
                                Exit;
                              end;

                            if not RegModifiedByInstruction(p_TargetReg, hp2) then
                              begin
                                { See if there's more we can optimise }
                                hp3 := hp2;
                                Continue;
                              end;
                          end;
                      end;
                  end;

                { Break out of the while loop under normal circumstances }
                Break;
              end;

            JumpTracking.Free;

          end;

        if (aoc_MovAnd2Mov_3 in OptsToCheck) and
          (taicpu(p).oper[1]^.typ = top_reg) and
          (taicpu(p).opsize = S_L) and
          GetNextInstructionUsingRegTrackingUse(p,hp2,taicpu(p).oper[1]^.reg) and
          (hp2.typ = ait_instruction) and
          (taicpu(hp2).opcode = A_AND) and
          (MatchOpType(taicpu(hp2),top_const,top_reg) or
           (MatchOpType(taicpu(hp2),top_reg,top_reg) and
            MatchOperand(taicpu(hp2).oper[0]^,taicpu(hp2).oper[1]^))
           ) then
          begin
            if SuperRegistersEqual(taicpu(p).oper[1]^.reg,taicpu(hp2).oper[1]^.reg) then
              begin
                if ((taicpu(hp2).oper[0]^.typ=top_const) and (taicpu(hp2).oper[0]^.val = $ffffffff)) or
                  ((taicpu(hp2).oper[0]^.typ=top_reg) and (taicpu(hp2).opsize=S_L)) then
                  begin
                    { Optimize out:
                        mov x, %reg
                        and ffffffffh, %reg
                    }
                    DebugMsg(SPeepholeOptimization + 'MovAnd2Mov 3 done',p);
                    RemoveInstruction(hp2);
                    Result:=true;
                    exit;
                  end;
              end;
          end;

        { leave out the mov from "mov reg, x(%frame_pointer); leave/ret" (with
          x >= RetOffset) as it doesn't do anything (it writes either to a
          parameter or to the temporary storage room for the function
          result)
        }
        if IsExitCode(hp1) and
          (taicpu(p).oper[1]^.typ = top_ref) and
          (taicpu(p).oper[1]^.ref^.index = NR_NO) and
          (
            (
              (taicpu(p).oper[1]^.ref^.base = current_procinfo.FramePointer) and
              not (
                assigned(current_procinfo.procdef.funcretsym) and
                (taicpu(p).oper[1]^.ref^.offset <= tabstractnormalvarsym(current_procinfo.procdef.funcretsym).localloc.reference.offset)
              )
            ) or
            { Also discard writes to the stack that are below the base pointer,
              as this is temporary storage rather than a function result on the
              stack, say. }
            (
              (taicpu(p).oper[1]^.ref^.base = NR_STACK_POINTER_REG) and
              (taicpu(p).oper[1]^.ref^.offset < current_procinfo.final_localsize)
            )
          ) then
          begin
            RemoveCurrentp(p, hp1);
            DebugMsg(SPeepholeOptimization + 'removed deadstore before leave/ret',p);
            RemoveLastDeallocForFuncRes(p);
            Result:=true;
            exit;
          end;
        if MatchInstruction(hp1,A_CMP,A_TEST,[taicpu(p).opsize]) then
          begin
            if MatchOpType(taicpu(p),top_reg,top_ref) and
              (taicpu(hp1).oper[1]^.typ = top_ref) and
              RefsEqual(taicpu(p).oper[1]^.ref^, taicpu(hp1).oper[1]^.ref^) then
              begin
                { change
                    mov reg1, mem1
                    test/cmp x, mem1

                    to

                    mov reg1, mem1
                    test/cmp x, reg1
                }
                taicpu(hp1).loadreg(1,taicpu(p).oper[0]^.reg);
                DebugMsg(SPeepholeOptimization + 'MovTestCmp2MovTestCmp 1',hp1);
                AllocRegBetween(taicpu(p).oper[0]^.reg,p,hp1,usedregs);
                Result := True;
                Exit;
              end;

            if DoMovCmpMemOpt(p, hp1) then
              begin
                Result := True;
                Exit;
              end;
          end;

        if MatchInstruction(hp1,A_LEA,[S_L{$ifdef x86_64},S_Q{$endif x86_64}]) and
          { If the flags register is in use, don't change the instruction to an
            ADD otherwise this will scramble the flags. [Kit] }
          not RegInUsedRegs(NR_DEFAULTFLAGS, UsedRegs) then
          begin
            if MatchOpType(Taicpu(p),top_ref,top_reg) and
               ((MatchReference(Taicpu(hp1).oper[0]^.ref^,Taicpu(hp1).oper[1]^.reg,Taicpu(p).oper[1]^.reg) and
                 (Taicpu(hp1).oper[0]^.ref^.base<>Taicpu(p).oper[1]^.reg)
                ) or
                (MatchReference(Taicpu(hp1).oper[0]^.ref^,Taicpu(p).oper[1]^.reg,Taicpu(hp1).oper[1]^.reg) and
                 (Taicpu(hp1).oper[0]^.ref^.index<>Taicpu(p).oper[1]^.reg)
                )
               ) then
               { mov reg1,ref
                 lea reg2,[reg1,reg2]

                 to

                 add reg2,ref}
              begin
                TransferUsedRegs(TmpUsedRegs);
                { reg1 may not be used afterwards }
                if not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg, hp1, TmpUsedRegs)) then
                  begin
                    Taicpu(hp1).opcode:=A_ADD;
                    Taicpu(hp1).oper[0]^.ref^:=Taicpu(p).oper[0]^.ref^;
                    DebugMsg(SPeepholeOptimization + 'MovLea2Add done',hp1);
                    RemoveCurrentp(p, hp1);
                    result:=true;
                    exit;
                  end;
              end;

            { If the LEA instruction can be converted into an arithmetic instruction,
              it may be possible to then fold it in the next optimisation, otherwise
              there's nothing more that can be optimised here. }
            if not ConvertLEA(taicpu(hp1)) then
              Exit;

          end;

        if (taicpu(p).oper[1]^.typ = top_reg) and
          (hp1.typ = ait_instruction) and
          GetNextInstruction(hp1, hp2) and
          MatchInstruction(hp2,A_MOV,[]) and
          (SuperRegistersEqual(taicpu(hp2).oper[0]^.reg,taicpu(p).oper[1]^.reg)) and
          (topsize2memsize[taicpu(hp1).opsize]>=topsize2memsize[taicpu(hp2).opsize]) and
          (
            IsFoldableArithOp(taicpu(hp1), taicpu(p).oper[1]^.reg)
{$ifdef x86_64}
            or
            (
              (taicpu(p).opsize=S_L) and (taicpu(hp1).opsize=S_Q) and (taicpu(hp2).opsize=S_L) and
              IsFoldableArithOp(taicpu(hp1), newreg(R_INTREGISTER,getsupreg(taicpu(p).oper[1]^.reg),R_SUBQ))
            )
{$endif x86_64}
          ) then
          begin
            if OpsEqual(taicpu(hp2).oper[1]^, taicpu(p).oper[0]^) and
              (taicpu(hp2).oper[0]^.typ=top_reg) then
              { change   movsX/movzX    reg/ref, reg2
                         add/sub/or/... reg3/$const, reg2
                         mov            reg2 reg/ref
                         dealloc        reg2
                to
                         add/sub/or/... reg3/$const, reg/ref      }
              begin
                TransferUsedRegs(TmpUsedRegs);
                UpdateUsedRegs(TmpUsedRegs, tai(p.next));
                UpdateUsedRegs(TmpUsedRegs, tai(hp1.next));
                If not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp2,TmpUsedRegs)) then
                  begin
                    { by example:
                        movswl  %si,%eax        movswl  %si,%eax      p
                        decl    %eax            addl    %edx,%eax     hp1
                        movw    %ax,%si         movw    %ax,%si       hp2
                      ->
                        movswl  %si,%eax        movswl  %si,%eax      p
                        decw    %eax            addw    %edx,%eax     hp1
                        movw    %ax,%si         movw    %ax,%si       hp2
                    }
                    DebugMsg(SPeepholeOptimization + 'MovOpMov2Op ('+
                          debug_op2str(taicpu(p).opcode)+debug_opsize2str(taicpu(p).opsize)+' '+
                          debug_op2str(taicpu(hp1).opcode)+debug_opsize2str(taicpu(hp1).opsize)+' '+
                          debug_op2str(taicpu(hp2).opcode)+debug_opsize2str(taicpu(hp2).opsize)+')',p);
                    taicpu(hp1).changeopsize(taicpu(hp2).opsize);
                    {
                      ->
                        movswl  %si,%eax        movswl  %si,%eax      p
                        decw    %si             addw    %dx,%si       hp1
                        movw    %ax,%si         movw    %ax,%si       hp2
                    }
                    case taicpu(hp1).ops of
                      1:
                        begin
                          taicpu(hp1).loadoper(0, taicpu(hp2).oper[1]^);
                          if taicpu(hp1).oper[0]^.typ=top_reg then
                            setsubreg(taicpu(hp1).oper[0]^.reg,getsubreg(taicpu(hp2).oper[0]^.reg));
                        end;
                      2:
                        begin
                          taicpu(hp1).loadoper(1, taicpu(hp2).oper[1]^);
                          if (taicpu(hp1).oper[0]^.typ=top_reg) and
                            (taicpu(hp1).opcode<>A_SHL) and
                            (taicpu(hp1).opcode<>A_SHR) and
                            (taicpu(hp1).opcode<>A_SAR) then
                            setsubreg(taicpu(hp1).oper[0]^.reg,getsubreg(taicpu(hp2).oper[0]^.reg));
                        end;
                      else
                        internalerror(2008042701);
                    end;
                    {
                      ->
                        decw    %si             addw    %dx,%si       p
                    }
                    RemoveInstruction(hp2);
                    RemoveCurrentP(p, hp1);
                    Result:=True;
                    Exit;
                  end;
              end;
            if MatchOpType(taicpu(hp2),top_reg,top_reg) and
              not(SuperRegistersEqual(taicpu(hp1).oper[0]^.reg,taicpu(hp2).oper[1]^.reg)) and
              ((topsize2memsize[taicpu(hp1).opsize]<= topsize2memsize[taicpu(hp2).opsize]) or
               { opsize matters for these opcodes, we could probably work around this, but it is not worth the effort }
               ((taicpu(hp1).opcode<>A_SHL) and (taicpu(hp1).opcode<>A_SHR) and (taicpu(hp1).opcode<>A_SAR))
              )
{$ifdef i386}
              { byte registers of esi, edi, ebp, esp are not available on i386 }
              and ((taicpu(hp2).opsize<>S_B) or not(getsupreg(taicpu(hp1).oper[0]^.reg) in [RS_ESI,RS_EDI,RS_EBP,RS_ESP]))
              and ((taicpu(hp2).opsize<>S_B) or not(getsupreg(taicpu(p).oper[0]^.reg) in [RS_ESI,RS_EDI,RS_EBP,RS_ESP]))
{$endif i386}
              then
              { change   movsX/movzX    reg/ref, reg2
                         add/sub/or/... regX/$const, reg2
                         mov            reg2, reg3
                         dealloc        reg2
                to
                         movsX/movzX    reg/ref, reg3
                         add/sub/or/... reg3/$const, reg3
              }
              begin
                TransferUsedRegs(TmpUsedRegs);
                UpdateUsedRegs(TmpUsedRegs, tai(p.next));
                UpdateUsedRegs(TmpUsedRegs, tai(hp1.next));
                If not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp2,TmpUsedRegs)) then
                  begin
                    { by example:
                        movswl  %si,%eax        movswl  %si,%eax      p
                        decl    %eax            addl    %edx,%eax     hp1
                        movw    %ax,%si         movw    %ax,%si       hp2
                      ->
                        movswl  %si,%eax        movswl  %si,%eax      p
                        decw    %eax            addw    %edx,%eax     hp1
                        movw    %ax,%si         movw    %ax,%si       hp2
                    }
                    DebugMsg(SPeepholeOptimization + 'MovOpMov2MovOp ('+
                          debug_op2str(taicpu(p).opcode)+debug_opsize2str(taicpu(p).opsize)+' '+
                          debug_op2str(taicpu(hp1).opcode)+debug_opsize2str(taicpu(hp1).opsize)+' '+
                          debug_op2str(taicpu(hp2).opcode)+debug_opsize2str(taicpu(hp2).opsize)+')',p);
                    { limit size of constants as well to avoid assembler errors, but
                      check opsize to avoid overflow when left shifting the 1 }
                    if (taicpu(p).oper[0]^.typ=top_const) and (topsize2memsize[taicpu(hp2).opsize]<=63) then
                      taicpu(p).oper[0]^.val:=taicpu(p).oper[0]^.val and ((qword(1) shl topsize2memsize[taicpu(hp2).opsize])-1);

{$ifdef x86_64}
                    { Be careful of, for example:
                        movl %reg1,%reg2
                        addl %reg3,%reg2
                        movq %reg2,%reg4

                      This will cause problems if the upper 32-bits of %reg3 or %reg4 are non-zero
                    }
                    if (taicpu(hp1).opsize = S_L) and (taicpu(hp2).opsize = S_Q) then
                      begin
                        taicpu(hp2).changeopsize(S_L);
                        setsubreg(taicpu(hp2).oper[0]^.reg, R_SUBD);
                        setsubreg(taicpu(hp2).oper[1]^.reg, R_SUBD);
                      end;
{$endif x86_64}

                    taicpu(hp1).changeopsize(taicpu(hp2).opsize);
                    taicpu(p).changeopsize(taicpu(hp2).opsize);
                    if taicpu(p).oper[0]^.typ=top_reg then
                      setsubreg(taicpu(p).oper[0]^.reg,getsubreg(taicpu(hp2).oper[0]^.reg));
                    taicpu(p).loadoper(1, taicpu(hp2).oper[1]^);
                    AllocRegBetween(taicpu(p).oper[1]^.reg,p,hp1,usedregs);
                    {
                      ->
                        movswl  %si,%eax        movswl  %si,%eax      p
                        decw    %si             addw    %dx,%si       hp1
                        movw    %ax,%si         movw    %ax,%si       hp2
                    }
                    case taicpu(hp1).ops of
                      1:
                        begin
                          taicpu(hp1).loadoper(0, taicpu(hp2).oper[1]^);
                          if taicpu(hp1).oper[0]^.typ=top_reg then
                            setsubreg(taicpu(hp1).oper[0]^.reg,getsubreg(taicpu(hp2).oper[0]^.reg));
                        end;
                      2:
                        begin
                          taicpu(hp1).loadoper(1, taicpu(hp2).oper[1]^);
                          if (taicpu(hp1).oper[0]^.typ=top_reg) and
                            (taicpu(hp1).opcode<>A_SHL) and
                            (taicpu(hp1).opcode<>A_SHR) and
                            (taicpu(hp1).opcode<>A_SAR) then
                            setsubreg(taicpu(hp1).oper[0]^.reg,getsubreg(taicpu(hp2).oper[0]^.reg));
                        end;
                      else
                        internalerror(2018111801);
                    end;
                    {
                      ->
                        decw    %si             addw    %dx,%si       p
                    }
                    RemoveInstruction(hp2);
                  end;
              end;

          end;
        if MatchInstruction(hp1,A_BTS,A_BTR,[Taicpu(p).opsize]) and
          GetNextInstruction(hp1, hp2) and
          MatchInstruction(hp2,A_OR,[Taicpu(p).opsize]) and
          MatchOperand(Taicpu(p).oper[0]^,0) and
          (Taicpu(p).oper[1]^.typ = top_reg) and
          MatchOperand(Taicpu(p).oper[1]^,Taicpu(hp1).oper[1]^) and
          MatchOperand(Taicpu(p).oper[1]^,Taicpu(hp2).oper[1]^) then
          { mov reg1,0
            bts reg1,operand1             -->      mov reg1,operand2
            or  reg1,operand2                      bts reg1,operand1}
          begin
            Taicpu(hp2).opcode:=A_MOV;
            DebugMsg(SPeepholeOptimization + 'MovBtsOr2MovBts done',hp1);
            asml.remove(hp1);
            insertllitem(hp2,hp2.next,hp1);
            RemoveCurrentp(p, hp1);
            Result:=true;
            exit;
          end;

        if MatchInstruction(hp1,A_SUB,[Taicpu(p).opsize]) and
          MatchOperand(Taicpu(p).oper[1]^,Taicpu(hp1).oper[1]^) and
          GetNextInstruction(hp1, hp2) and
          MatchInstruction(hp2,A_CMP,[Taicpu(p).opsize]) and
          MatchOperand(Taicpu(p).oper[0]^,Taicpu(hp2).oper[1]^) and
          MatchOperand(Taicpu(hp1).oper[0]^,Taicpu(hp2).oper[0]^) then
          { change

            mov reg1,reg2
            sub reg3,reg2
            cmp reg3,reg1

            into

            mov reg1,reg2
            sub reg3,reg2
          }
          begin
            DebugMsg(SPeepholeOptimization + 'MovSubCmp2MovSub done',p);
            RemoveInstruction(hp2);
            Result:=true;
            exit;
          end;

        {
          mov ref,reg0
          <op> reg0,reg1
          dealloc reg0

          to

          <op> ref,reg1
        }
        if MatchOpType(taicpu(p),top_ref,top_reg) and
          MatchOpType(taicpu(hp1),top_reg,top_reg) and
          MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[0]^) and
          MatchInstruction(hp1,[A_AND,A_OR,A_XOR,A_ADD,A_SUB,A_CMP],[Taicpu(p).opsize]) and
          not(MatchOperand(taicpu(hp1).oper[0]^,taicpu(hp1).oper[1]^)) and
          RegEndOfLife(taicpu(p).oper[1]^.reg,taicpu(hp1)) then
          begin
            taicpu(hp1).loadoper(0,taicpu(p).oper[0]^);
            DebugMsg(SPeepholeOptimization + 'MovOp2Op done',hp1);
            RemoveCurrentp(p, hp1);
            Result:=true;
            exit;
          end;

        if (taicpu(p).oper[0]^.typ = top_ref) and { Second operand will be a register }
          MatchInstruction(hp1, A_SHR, A_SAR, [taicpu(p).opsize]) and
          MatchOpType(taicpu(hp1), top_const, top_reg) and
          (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
          begin
            RegName1 := debug_regname(taicpu(hp1).oper[1]^.reg);
{$ifdef x86_64}
            { Convert:
                movq x(ref),%reg64
                shrq y,%reg64
              To:
                movl x+4(ref),%reg32
                shrl y-32,%reg32 (Remove if y = 32)
            }
            if (taicpu(p).opsize = S_Q) and
              (taicpu(hp1).opcode = A_SHR) and
              (taicpu(hp1).oper[0]^.val >= 32) then
              begin
                PreMessage := 'movq ' + debug_operstr(taicpu(p).oper[0]^) + ',' + RegName1 + '; ' +
                  'shrq $' + debug_tostr(taicpu(hp1).oper[0]^.val) + ',' + RegName1 + ' -> movl ';

                { Convert to 32-bit }
                setsubreg(taicpu(p).oper[1]^.reg, R_SUBD);
                taicpu(p).opsize := S_L;

                Inc(taicpu(p).oper[0]^.ref^.offset, 4);

                PreMessage := PreMessage + debug_operstr(taicpu(p).oper[0]^) + ',' + debug_regname(taicpu(p).oper[1]^.reg);
                if (taicpu(hp1).oper[0]^.val = 32) then
                  begin
                    DebugMsg(SPeepholeOptimization + PreMessage + ' (MovShr2Mov)', p);
                    RemoveInstruction(hp1);
                  end
                else
                  begin
                    { This will potentially open up more arithmetic operations since
                      the peephole optimizer now has a big hint that only the lower
                      32 bits are currently in use (and opcodes are smaller in size) }
                    setsubreg(taicpu(hp1).oper[1]^.reg, R_SUBD);
                    taicpu(hp1).opsize := S_L;

                    Dec(taicpu(hp1).oper[0]^.val, 32);
                    DebugMsg(SPeepholeOptimization + PreMessage +
                      '; shrl $' + debug_tostr(taicpu(hp1).oper[0]^.val) + ',' + debug_regname(taicpu(hp1).oper[1]^.reg) + ' (MovShr2MovShr)', p);
                  end;
                Result := True;
                Exit;
              end;
{$endif x86_64}
            { Convert:
                movl   x(ref),%reg
                shrl   $24,%reg
              To:
                movzbl x+3(ref),%reg

              Do similar things for movl; shrl $16 -> movzwl and movw; shrw $8 -> movzbw

              Also accept sar instead of shr, but convert to movsx instead of movzx
            }
            if taicpu(hp1).opcode = A_SHR then
              MovUnaligned := A_MOVZX
            else
              MovUnaligned := A_MOVSX;

            NewSize := S_NO;
            NewOffset := 0;
            case taicpu(p).opsize of
              S_B:
                { No valid combinations };
              S_W:
                if (taicpu(hp1).oper[0]^.val = 8) then
                  begin
                    NewSize := S_BW;
                    NewOffset := 1;
                  end;
              S_L:
                case taicpu(hp1).oper[0]^.val of
                  16:
                    begin
                      NewSize := S_WL;
                      NewOffset := 2;
                    end;
                  24:
                    begin
                      NewSize := S_BL;
                      NewOffset := 3;
                    end;
                  else
                    ;
                end;
{$ifdef x86_64}
              S_Q:
                case taicpu(hp1).oper[0]^.val of
                  32:
                    begin
                      if taicpu(hp1).opcode = A_SAR then
                        begin
                          { 32-bit to 64-bit is a distinct instruction }
                          MovUnaligned := A_MOVSXD;
                          NewSize := S_LQ;
                          NewOffset := 4;
                        end
                      else
                        { Should have been handled by MovShr2Mov above }
                        InternalError(2022081811);
                    end;
                  48:
                    begin
                      NewSize := S_WQ;
                      NewOffset := 6;
                    end;
                  56:
                    begin
                      NewSize := S_BQ;
                      NewOffset := 7;
                    end;
                  else
                    ;
                end;
{$endif x86_64}
              else
                InternalError(2022081810);
            end;

            if (NewSize <> S_NO) and
              (taicpu(p).oper[0]^.ref^.offset <= $7FFFFFFF - NewOffset) then
              begin
                PreMessage := 'mov' + debug_opsize2str(taicpu(p).opsize) + ' ' + debug_operstr(taicpu(p).oper[0]^) + ',' + RegName1 + '; ' +
                  'shr' + debug_opsize2str(taicpu(p).opsize) + ' $' + debug_tostr(taicpu(hp1).oper[0]^.val) + ',' + RegName1 + ' -> ' +
                  debug_op2str(MovUnaligned);

{$ifdef x86_64}
                if MovUnaligned <> A_MOVSXD then
                  { Don't add size suffix for MOVSXD }
{$endif x86_64}
                  PreMessage := PreMessage + debug_opsize2str(NewSize);

                Inc(taicpu(p).oper[0]^.ref^.offset, NewOffset);
                taicpu(p).opcode := MovUnaligned;
                taicpu(p).opsize := NewSize;

                DebugMsg(SPeepholeOptimization + PreMessage + ' ' +
                  debug_operstr(taicpu(p).oper[0]^) + ',' + debug_regname(taicpu(hp1).oper[1]^.reg) + ' (MovShr/Sar2Movx)', p);

                RemoveInstruction(hp1);
                Result := True;
                Exit;
              end;
          end;

        { Backward optimisation shared with OptPass2MOV }
        if FuncMov2Func(p, hp1) then
          begin
            Result := True;
            Exit;
          end;
      end;


   function TX86AsmOptimizer.OptPass1MOVXX(var p : tai) : boolean;
      var
        hp1 : tai;
      begin
        Result:=false;
        if taicpu(p).ops <> 2 then
          exit;
        if (MatchOpType(taicpu(p),top_reg,top_reg) and GetNextInstructionUsingReg(p,hp1,taicpu(p).oper[1]^.reg)) or
          GetNextInstruction(p,hp1) then
          begin
            if MatchInstruction(hp1,taicpu(p).opcode,[taicpu(p).opsize]) and
            (taicpu(hp1).ops = 2) then
            begin
              if (taicpu(hp1).oper[0]^.typ = taicpu(p).oper[1]^.typ) and
                 (taicpu(hp1).oper[1]^.typ = taicpu(p).oper[0]^.typ) then
                  {  movXX reg1, mem1     or     movXX mem1, reg1
                     movXX mem2, reg2            movXX reg2, mem2}
                begin
                  if OpsEqual(taicpu(hp1).oper[1]^,taicpu(p).oper[0]^) then
                    { movXX reg1, mem1     or     movXX mem1, reg1
                      movXX mem2, reg1            movXX reg2, mem1}
                    begin
                      if OpsEqual(taicpu(hp1).oper[0]^,taicpu(p).oper[1]^) then
                        begin
                          { Removes the second statement from
                            movXX reg1, mem1/reg2
                            movXX mem1/reg2, reg1
                          }
                          if taicpu(p).oper[0]^.typ=top_reg then
                            AllocRegBetween(taicpu(p).oper[0]^.reg,p,hp1,usedregs);
                          { Removes the second statement from
                            movXX mem1/reg1, reg2
                            movXX reg2, mem1/reg1
                          }
                          if (taicpu(p).oper[1]^.typ=top_reg) and
                            not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp1,UsedRegs)) then
                            begin
                              DebugMsg(SPeepholeOptimization + 'MovXXMovXX2Nop 1 done',p);
                              RemoveInstruction(hp1);
                              RemoveCurrentp(p); { p will now be equal to the instruction that follows what was hp1 }
                              Result:=true;
                              exit;
                            end
                          else if (taicpu(hp1).oper[1]^.typ<>top_ref) or (not(vol_write in taicpu(hp1).oper[1]^.ref^.volatility)) and
                            (taicpu(hp1).oper[0]^.typ<>top_ref) or (not(vol_read in taicpu(hp1).oper[0]^.ref^.volatility)) then
                            begin
                              DebugMsg(SPeepholeOptimization + 'MovXXMovXX2MoVXX 1 done',p);
                              RemoveInstruction(hp1);
                              Result:=true;
                              exit;
                            end;
                        end
                  end;
              end;
            end;
          end;
      end;


    function TX86AsmOptimizer.OptPass1OP(var p : tai) : boolean;
      var
        hp1 : tai;
      begin
        result:=false;
        { replace
            <Op>X    %mreg1,%mreg2  // Op in [ADD,MUL]
            MovX     %mreg2,%mreg1
            dealloc  %mreg2

            by
            <Op>X    %mreg2,%mreg1
          ?
        }
        if GetNextInstruction(p,hp1) and
          { we mix single and double opperations here because we assume that the compiler
            generates vmovapd only after double operations and vmovaps only after single operations }
          MatchInstruction(hp1,A_MOVAPD,A_MOVAPS,[S_NO]) and
          MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[0]^) and
          MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[1]^) and
          (taicpu(p).oper[0]^.typ=top_reg) then
          begin
            TransferUsedRegs(TmpUsedRegs);
            UpdateUsedRegs(TmpUsedRegs, tai(p.next));
            if not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp1,TmpUsedRegs)) then
              begin
                taicpu(p).loadoper(0,taicpu(hp1).oper[0]^);
                taicpu(p).loadoper(1,taicpu(hp1).oper[1]^);
                DebugMsg(SPeepholeOptimization + 'OpMov2Op done',p);
                RemoveInstruction(hp1);
                result:=true;
              end;
          end;
      end;


    function TX86AsmOptimizer.OptPass1Test(var p: tai) : boolean;
      var
        hp1, p_label, p_dist, hp1_dist, hp1_last: tai;
        JumpLabel, JumpLabel_dist: TAsmLabel;
        FirstValue, SecondValue: TCGInt;

        function OptimizeJump(var InputP: tai): Boolean;
          var
            TempBool: Boolean;
          begin
            Result := False;
            TempBool := True;
            if DoJumpOptimizations(InputP, TempBool) or
              not TempBool then
              begin
                Result := True;

                if Assigned(InputP) then
                  begin
                    { CollapseZeroDistJump will be set to the label or an align
                      before it after the jump if it optimises, whether or not
                      the label is live or dead }
                    if (InputP.typ = ait_align) or
                      (
                        (InputP.typ = ait_label) and
                        not (tai_label(InputP).labsym.is_used)
                      ) then
                      GetNextInstruction(InputP, InputP);
                  end;

                Exit;
              end;
          end;

      begin
        Result := False;
        if (taicpu(p).oper[0]^.typ = top_const) and
          (taicpu(p).oper[0]^.val <> -1) then
          begin
            { Convert unsigned maximum constants to -1 to aid optimisation }
            case taicpu(p).opsize of
              S_B:
                if (taicpu(p).oper[0]^.val and $FF) = $FF then
                  begin
                    taicpu(p).oper[0]^.val := -1;
                    Result := True;
                    Exit;
                  end;
              S_W:
                if (taicpu(p).oper[0]^.val and $FFFF) = $FFFF then
                  begin
                    taicpu(p).oper[0]^.val := -1;
                    Result := True;
                    Exit;
                  end;
              S_L:
                if (taicpu(p).oper[0]^.val and $FFFFFFFF) = $FFFFFFFF then
                  begin
                    taicpu(p).oper[0]^.val := -1;
                    Result := True;
                    Exit;
                  end;
{$ifdef x86_64}
              S_Q:
                { Storing anything greater than $7FFFFFFF is not possible so do
                  nothing };
{$endif x86_64}
              else
                InternalError(2021121001);
            end;
          end;

        if GetNextInstruction(p, hp1) and
          TrySwapMovCmp(p, hp1) then
          begin
            Result := True;
            Exit;
          end;

        p_label := nil;
        JumpLabel := nil;

        if MatchInstruction(hp1, A_Jcc, []) then
          begin
            if OptimizeJump(hp1) then
              begin
                Result := True;

                if Assigned(hp1) then
                  begin
                    { CollapseZeroDistJump will be set to the label or an align
                      before it after the jump if it optimises, whether or not
                      the label is live or dead }
                    if (hp1.typ = ait_align) or
                      (
                        (hp1.typ = ait_label) and
                        not (tai_label(hp1).labsym.is_used)
                      ) then
                      GetNextInstruction(hp1, hp1);
                  end;

                TransferUsedRegs(TmpUsedRegs);
                UpdateUsedRegs(TmpUsedRegs, tai(p.Next));

                if not Assigned(hp1) or
                  (
                    not MatchInstruction(hp1, A_Jcc, A_SETcc, A_CMOVcc, []) and
                    not RegUsedAfterInstruction(NR_DEFAULTFLAGS, hp1, TmpUsedRegs)
                  ) then
                  begin
                    { No more conditional jumps; conditional statement is no longer required }
                    DebugMsg(SPeepholeOptimization + 'Removed unnecessary condition (Test2Nop)', p);
                    RemoveCurrentP(p);
                  end;

                Exit;
              end;

            if IsJumpToLabel(taicpu(hp1)) then
              begin
                JumpLabel := TAsmLabel(taicpu(hp1).oper[0]^.ref^.symbol);
                if Assigned(JumpLabel) then
                  p_label := getlabelwithsym(JumpLabel);
              end;
          end;

        { Search for:
            test  $x,(reg/ref)
            jne   @lbl1
            test  $y,(reg/ref) (same register or reference)
            jne   @lbl1

          Change to:
            test  $(x or y),(reg/ref)
            jne   @lbl1

          (Note, this doesn't work with je instead of jne)

          Also catch cases where "cmp $0,(reg/ref)" and "test %reg,%reg" are used.

          Also search for:
            test  $x,(reg/ref)
            je    @lbl1
            ...
            test  $y,(reg/ref)
            je/jne @lbl2

            If (x or y) = x, then the second jump is deterministic
        }
        if (
            (
              (taicpu(p).oper[0]^.typ = top_const) or
              (
                { test %reg,%reg can be considered equivalent to test, -1,%reg }
                (taicpu(p).oper[0]^.typ = top_reg) and
                MatchOperand(taicpu(p).oper[1]^, taicpu(p).oper[0]^.reg)
              )
            ) and
            MatchInstruction(hp1, A_JCC, [])
          ) then
          begin
            if (taicpu(p).oper[0]^.typ = top_reg) and
              MatchOperand(taicpu(p).oper[1]^, taicpu(p).oper[0]^.reg) then
              FirstValue := -1
            else
              FirstValue := taicpu(p).oper[0]^.val;

            { If we have several test/jne's in a row, it might be the case that
              the second label doesn't go to the same location, but the one
              after it might (e.g. test; jne @lbl1; test; jne @lbl2; test @lbl1),
              so accommodate for this with a while loop.
            }
            hp1_last := hp1;

            while (
                (
                  (taicpu(p).oper[1]^.typ = top_reg) and
                  GetNextInstructionUsingReg(hp1_last, p_dist, taicpu(p).oper[1]^.reg)
                ) or GetNextInstruction(hp1_last, p_dist)

              ) and (p_dist.typ = ait_instruction) do
              begin

                if (
                    (
                      (taicpu(p_dist).opcode = A_TEST) and
                      (
                        (taicpu(p_dist).oper[0]^.typ = top_const) or
                        { test %reg,%reg can be considered equivalent to test, -1,%reg }
                        MatchOperand(taicpu(p_dist).oper[1]^, taicpu(p_dist).oper[0]^)
                      )
                    ) or
                    (
                      { cmp 0,%reg = test %reg,%reg }
                      (taicpu(p_dist).opcode = A_CMP) and
                      MatchOperand(taicpu(p_dist).oper[0]^, 0)
                    )
                  ) and
                  { Make sure the destination operands are actually the same }
                  MatchOperand(taicpu(p_dist).oper[1]^, taicpu(p).oper[1]^) and
                  GetNextInstruction(p_dist, hp1_dist) and
                  MatchInstruction(hp1_dist, A_JCC, []) then
                  begin
                    if OptimizeJump(hp1_dist) then
                      begin
                        Result := True;
                        Exit;
                      end;

                    if
                      (taicpu(p_dist).opcode = A_CMP) { constant will be zero } or
                      (
                        (taicpu(p_dist).oper[0]^.typ = top_reg) and
                        MatchOperand(taicpu(p_dist).oper[1]^, taicpu(p_dist).oper[0]^.reg)
                      ) then
                      SecondValue := -1
                    else
                      SecondValue := taicpu(p_dist).oper[0]^.val;

                    { If both of the TEST constants are identical, delete the
                      second TEST that is unnecessary (be careful though, just
                      in case the flags are modified in between) }
                    if (FirstValue = SecondValue) then
                      begin
                        if condition_in(taicpu(hp1_dist).condition, taicpu(hp1).condition) then
                          begin
                            { Since the second jump's condition is a subset of the first, we
                              know it will never branch because the first jump dominates it.
                              Get it out of the way now rather than wait for the jump
                              optimisations for a speed boost. }
                            if IsJumpToLabel(taicpu(hp1_dist)) then
                              TAsmLabel(taicpu(hp1_dist).oper[0]^.ref^.symbol).DecRefs;

                            DebugMsg(SPeepholeOptimization + 'Removed dominated jump (via TEST/Jcc/TEST)', hp1_dist);
                            RemoveInstruction(hp1_dist);
                            Result := True;
                          end
                        else if condition_in(inverse_cond(taicpu(hp1).condition), taicpu(hp1_dist).condition) then
                          begin
                            { If the inverse of the first condition is a subset of the second,
                              the second one will definitely branch if the first one doesn't }
                            DebugMsg(SPeepholeOptimization + 'Conditional jump will always branch (via TEST/Jcc/TEST)', hp1_dist);

                            { We can remove the TEST instruction too }
                            DebugMsg(SPeepholeOptimization + 'TEST/Jcc/TEST; removed superfluous TEST', p_dist);
                            RemoveInstruction(p_dist);

                            MakeUnconditional(taicpu(hp1_dist));
                            RemoveDeadCodeAfterJump(hp1_dist);

                            { Since the jump is now unconditional, we can't
                              continue any further with this particular
                              optimisation.  The original TEST is still intact
                              though, so there might be something else we can
                              do }
                            Include(OptsToCheck, aoc_ForceNewIteration);
                            Break;
                          end;

                        if Result or
                          { If a jump wasn't removed or made unconditional, only
                            remove the identical TEST instruction if the flags
                            weren't modified }
                          not RegModifiedBetween(NR_DEFAULTFLAGS, hp1, p_dist) then
                          begin
                            DebugMsg(SPeepholeOptimization + 'TEST/Jcc/TEST; removed superfluous TEST', p_dist);
                            RemoveInstruction(p_dist);

                            { If the jump was removed or made unconditional, we
                              don't need to allocate NR_DEFAULTFLAGS over the
                              entire range }
                            if not Result then
                              begin
                                { Mark the flags as 'in use' over the entire range }
                                AllocRegBetween(NR_DEFAULTFLAGS, hp1, hp1_dist, UsedRegs);

                                { Speed gain - continue search from the Jcc instruction }
                                hp1_last := hp1_dist;

                                { Only the TEST instruction was removed, and the
                                  original was unchanged, so we can safely do
                                  another iteration of the while loop }
                                Include(OptsToCheck, aoc_ForceNewIteration);
                                Continue;
                              end;

                            Exit;
                          end;
                      end;

                    hp1_last := nil;
                    if (taicpu(hp1).condition in [C_NE, C_NZ]) and
                      (
                        { In this situation, the TEST/JNE pairs must be adjacent (fixes #40366) }

                        { Always adjacent under -O2 and under }
                        not(cs_opt_level3 in current_settings.optimizerswitches) or
                        (
                          GetNextInstruction(hp1, hp1_last) and
                          (hp1_last = p_dist)
                        )
                      ) and
                      (
                        (
                          { Test the following variant:
                              test  $x,(reg/ref)
                              jne   @lbl1
                              test  $y,(reg/ref)
                              je    @lbl2
                            @lbl1:

                            Becomes:
                              test  $(x or y),(reg/ref)
                              je    @lbl2
                            @lbl1: (may become a dead label)
                          }
                          (taicpu(hp1_dist).condition in [C_E, C_Z]) and
                          GetNextInstruction(hp1_dist, hp1_last) and
                          (hp1_last = p_label)
                        ) or
                        (
                          (taicpu(hp1_dist).condition in [C_NE, C_NZ]) and
                          { If the first instruction is test %reg,%reg or test $-1,%reg,
                            then the second jump will never branch, so it can also be
                            removed regardless of where it goes }
                          (
                            (FirstValue = -1) or
                            (SecondValue = -1) or
                            MatchOperand(taicpu(hp1_dist).oper[0]^, taicpu(hp1).oper[0]^)
                          )
                        )
                      ) then
                      begin
                        { Same jump location... can be a register since nothing's changed }

                        { If any of the entries are equivalent to test %reg,%reg, then the
                          merged $(x or y) is also test %reg,%reg / test $-1,%reg }
                        taicpu(p).loadconst(0, FirstValue or SecondValue);

                        if (hp1_last = p_label) then
                          begin
                            { Variant }
                            DebugMsg(SPeepholeOptimization + 'TEST/JNE/TEST/JE/@Lbl merged', p);
                            RemoveInstruction(p_dist);

                            if Assigned(JumpLabel) then
                              JumpLabel.decrefs;

                            RemoveInstruction(hp1);
                          end
                        else
                          begin
                            { Only remove the second test if no jumps or other conditional instructions follow }
                            TransferUsedRegs(TmpUsedRegs);
                            UpdateUsedRegs(TmpUsedRegs, tai(p.Next));
                            UpdateUsedRegs(TmpUsedRegs, tai(hp1.Next));
                            UpdateUsedRegs(TmpUsedRegs, tai(p_dist.Next));
                            if not RegUsedAfterInstruction(NR_DEFAULTFLAGS, hp1_dist, TmpUsedRegs) then
                              begin
                                DebugMsg(SPeepholeOptimization + 'TEST/JNE/TEST/JNE merged', p);
                                RemoveInstruction(p_dist);

                                { Remove the first jump, not the second, to keep
                                  any register deallocations between the second
                                  TEST/JNE pair in the same place.  Aids future
                                  optimisation. }
                                if Assigned(JumpLabel) then
                                  JumpLabel.decrefs;

                                RemoveInstruction(hp1);
                              end
                            else
                              begin
                                DebugMsg(SPeepholeOptimization + 'TEST/JNE/TEST/JNE merged (second TEST preserved)', p);

                                if IsJumpToLabel(taicpu(hp1_dist)) then
                                  TAsmLabel(taicpu(hp1_dist).oper[0]^.ref^.symbol).DecRefs;

                                { Remove second jump in this instance }
                                RemoveInstruction(hp1_dist);
                              end;
                          end;

                        Result := True;
                        Exit;
                      end;
                  end;

                if { If -O2 and under, it may stop on any old instruction }
                  (cs_opt_level3 in current_settings.optimizerswitches) and
                  (taicpu(p).oper[1]^.typ = top_reg) and
                  not RegModifiedByInstruction(taicpu(p).oper[1]^.reg, p_dist) then
                  begin
                    hp1_last := p_dist;
                    Continue;
                  end;

                Break;

              end;
          end;

        { Search for:
            test  %reg,%reg
            j(c1) @lbl1
            ...
          @lbl:
            test %reg,%reg (same register)
            j(c2) @lbl2

          If c2 is a subset of c1, change to:
            test  %reg,%reg
            j(c1) @lbl2
            (@lbl1 may become a dead label as a result)
        }

        if (taicpu(p).oper[1]^.typ = top_reg) and
          (taicpu(p).oper[0]^.typ = top_reg) and
          (taicpu(p).oper[0]^.reg = taicpu(p).oper[1]^.reg) and
          { p_label <> nil is a marker that hp1 is a Jcc to a label }
          Assigned(p_label) and
          GetNextInstruction(p_label, p_dist) and
          MatchInstruction(p_dist, A_TEST, []) and
          { It's fine if the second test uses smaller sub-registers }
          (taicpu(p_dist).opsize <= taicpu(p).opsize) and
          MatchOpType(taicpu(p_dist), top_reg, top_reg) and
          SuperRegistersEqual(taicpu(p_dist).oper[0]^.reg, taicpu(p).oper[0]^.reg) and
          SuperRegistersEqual(taicpu(p_dist).oper[1]^.reg, taicpu(p).oper[1]^.reg) and
          GetNextInstruction(p_dist, hp1_dist) and
          MatchInstruction(hp1_dist, A_JCC, []) then { This doesn't have to be an explicit label }
          begin
            JumpLabel_dist := TAsmLabel(taicpu(hp1_dist).oper[0]^.ref^.symbol);

            if JumpLabel = JumpLabel_dist then
              { This is an infinite loop }
              Exit;

            { Best optimisation when the first condition is a subset (or equal) of the second }
            if condition_in(taicpu(hp1).condition, taicpu(hp1_dist).condition) then
              begin
                { Any registers used here will already be allocated }
                if Assigned(JumpLabel) then
                  JumpLabel.DecRefs;

                DebugMsg(SPeepholeOptimization + 'TEST/Jcc/@Lbl/TEST/Jcc -> TEST/Jcc, redirecting first jump', hp1);
                taicpu(hp1).loadref(0, taicpu(hp1_dist).oper[0]^.ref^); { This also increases the reference count }
                Result := True;
                Exit;
              end;
          end;
      end;


    function TX86AsmOptimizer.OptPass1Add(var p : tai) : boolean;
      var
        hp1, hp2: tai;
        ActiveReg: TRegister;
        OldOffset: asizeint;
        ThisConst: TCGInt;

      function RegDeallocated: Boolean;
        begin
          TransferUsedRegs(TmpUsedRegs);
          UpdateUsedRegs(TmpUsedRegs, tai(p.next));
          Result := not(RegUsedAfterInstruction(ActiveReg,hp1,TmpUsedRegs))
        end;

      begin
        result:=false;
        hp1 := nil;
        { replace
            addX     const,%reg1
            leaX     (%reg1,%reg1,Y),%reg2   // Base or index might not be equal to reg1
            dealloc  %reg1

            by

            leaX     const+const*Y(%reg1,%reg1,Y),%reg2
        }

        if MatchOpType(taicpu(p),top_const,top_reg) then
          begin
            ActiveReg := taicpu(p).oper[1]^.reg;
            { Ensures the entire register was updated }
            if (taicpu(p).opsize >= S_L) and
              GetNextInstructionUsingReg(p,hp1, ActiveReg) and
              MatchInstruction(hp1,A_LEA,[]) and
              (SuperRegistersEqual(ActiveReg, taicpu(hp1).oper[0]^.ref^.base) or
               SuperRegistersEqual(ActiveReg, taicpu(hp1).oper[0]^.ref^.index)) and
              (
                { Cover the case where the register in the reference is also the destination register }
                Reg1WriteOverwritesReg2Entirely(taicpu(hp1).oper[1]^.reg, ActiveReg) or
                (
                  { Try to avoid the expensive check of RegUsedAfterInstruction if we know it will return False }
                  not SuperRegistersEqual(taicpu(hp1).oper[1]^.reg, ActiveReg) and
                  RegDeallocated
                )
              ) then
              begin
                OldOffset := taicpu(hp1).oper[0]^.ref^.offset;
{$push}
{$R-}{$Q-}
                { Explicitly disable overflow checking for these offset calculation
                  as those do not matter for the final result }
                if ActiveReg=taicpu(hp1).oper[0]^.ref^.base then
                  inc(taicpu(hp1).oper[0]^.ref^.offset,taicpu(p).oper[0]^.val);
                if ActiveReg=taicpu(hp1).oper[0]^.ref^.index then
                  inc(taicpu(hp1).oper[0]^.ref^.offset,taicpu(p).oper[0]^.val*max(taicpu(hp1).oper[0]^.ref^.scalefactor,1));
{$pop}
{$ifdef x86_64}
                if (taicpu(hp1).oper[0]^.ref^.offset > $7FFFFFFF) or (taicpu(hp1).oper[0]^.ref^.offset < -2147483648) then
                  begin
                    { Overflow; abort }
                    taicpu(hp1).oper[0]^.ref^.offset := OldOffset;
                  end
                else
{$endif x86_64}
                  begin
                    DebugMsg(SPeepholeOptimization + 'AddLea2Lea done',p);
                    if not (cs_opt_level3 in current_settings.optimizerswitches) then
                      { hp1 is the immediate next instruction for sure - good for a quick speed boost }
                      RemoveCurrentP(p, hp1)
                    else
                      RemoveCurrentP(p);

                    result:=true;
                    Exit;
                  end;
              end;

            if (
                { Save calling GetNextInstructionUsingReg again }
                Assigned(hp1) or
                GetNextInstructionUsingReg(p,hp1, ActiveReg)
              ) and
              MatchInstruction(hp1,A_ADD,A_SUB,[taicpu(p).opsize]) and
              (taicpu(hp1).oper[1]^.reg = ActiveReg) then
              begin
                if taicpu(hp1).oper[0]^.typ = top_const then
                  begin
                    { Merge add const1,%reg; add/sub const2,%reg to add const1+/-const2,%reg }
                    if taicpu(hp1).opcode = A_ADD then
                      ThisConst := taicpu(p).oper[0]^.val + taicpu(hp1).oper[0]^.val
                    else
                      ThisConst := taicpu(p).oper[0]^.val - taicpu(hp1).oper[0]^.val;

                    Result := True;

                    { Handle any overflows }
                    case taicpu(p).opsize of
                      S_B:
                        taicpu(p).oper[0]^.val := ThisConst and $FF;
                      S_W:
                        taicpu(p).oper[0]^.val := ThisConst and $FFFF;
                      S_L:
                        taicpu(p).oper[0]^.val := ThisConst and $FFFFFFFF;
{$ifdef x86_64}
                      S_Q:
                        if (ThisConst > $7FFFFFFF) or (ThisConst < -2147483648) then
                          { Overflow; abort }
                          Result := False
                        else
                          taicpu(p).oper[0]^.val := ThisConst;
{$endif x86_64}
                      else
                        InternalError(2021102610);
                    end;

                    { Result may get set to False again if the combined immediate overflows for S_Q sizes }
                    if Result then
                      begin
                        if (taicpu(p).oper[0]^.val < 0) and
                          (
                            ((taicpu(p).opsize = S_B) and (taicpu(p).oper[0]^.val <> -128)) or
                            ((taicpu(p).opsize = S_W) and (taicpu(p).oper[0]^.val <> -32768)) or
                            ((taicpu(p).opsize in [S_L{$ifdef x86_64}, S_Q{$endif x86_64}]) and (taicpu(p).oper[0]^.val <> -2147483648))
                          ) then
                          begin
                            DebugMsg(SPeepholeOptimization + 'ADD; ADD/SUB -> SUB',p);
                            taicpu(p).opcode := A_SUB;
                            taicpu(p).oper[0]^.val := -taicpu(p).oper[0]^.val;
                          end
                        else
                          DebugMsg(SPeepholeOptimization + 'ADD; ADD/SUB -> ADD',p);
                        RemoveInstruction(hp1);
                      end;
                  end
                else
                  begin
                    { Make doubly sure the flags aren't in use because the order of additions may affect them }
                    TransferUsedRegs(TmpUsedRegs);
                    UpdateUsedRegs(TmpUsedRegs, tai(p.next));
                    hp2 := p;

                    while not (cs_opt_level3 in current_settings.optimizerswitches) and
                      GetNextInstruction(hp2, hp2) and (hp2 <> hp1) do
                      UpdateUsedRegs(TmpUsedRegs, tai(hp2.next));

                    if not RegInUsedRegs(NR_DEFAULTFLAGS, TmpUsedRegs) then
                      begin
                        { Move the constant addition to after the reg/ref addition to improve optimisation }
                        DebugMsg(SPeepholeOptimization + 'Add/sub swap 1a done',p);
                        Asml.Remove(p);
                        Asml.InsertAfter(p, hp1);
                        p := hp1;
                        Result := True;
                        Exit;
                      end;
                  end;
              end;

            if DoArithCombineOpt(p) then
              Result:=true;
          end;
      end;


    function TX86AsmOptimizer.OptPass1LEA(var p : tai) : boolean;
      var
        hp1, hp2: tai;
        ref: Integer;
        saveref: treference;
        offsetcalc: Int64;
        TempReg: TRegister;
        Multiple: TCGInt;
        Adjacent, IntermediateRegDiscarded: Boolean;
      begin
        Result:=false;

        { play save and throw an error if LEA uses a seg register prefix,
          this is most likely an error somewhere else }
        if taicpu(p).oper[0]^.ref^.Segment<>NR_NO then
          internalerror(2022022001);

        { changes "lea (%reg1), %reg2" into "mov %reg1, %reg2" }
        if (taicpu(p).oper[0]^.ref^.base <> NR_NO) and
           (taicpu(p).oper[0]^.ref^.index = NR_NO) and
           (
             { do not mess with leas accessing the stack pointer
               unless it's a null operation }
             (taicpu(p).oper[1]^.reg <> NR_STACK_POINTER_REG) or
             (
               (taicpu(p).oper[0]^.ref^.base = NR_STACK_POINTER_REG) and
               (taicpu(p).oper[0]^.ref^.offset = 0)
             )
           ) and
           (not(Assigned(taicpu(p).oper[0]^.ref^.Symbol))) then
          begin
            if (taicpu(p).oper[0]^.ref^.offset = 0) then
              begin
                if (taicpu(p).oper[0]^.ref^.base <> taicpu(p).oper[1]^.reg) then
                  begin
                    taicpu(p).opcode := A_MOV;
                    taicpu(p).loadreg(0, taicpu(p).oper[0]^.ref^.base);
                    DebugMsg(SPeepholeOptimization + 'Lea2Mov done',p);
                  end
                else
                  begin
                    DebugMsg(SPeepholeOptimization + 'Lea2Nop done',p);
                    RemoveCurrentP(p);
                  end;
                Result:=true;
                exit;
              end
            else if (
              { continue to use lea to adjust the stack pointer,
                it is the recommended way, but only if not optimizing for size }
                (taicpu(p).oper[1]^.reg<>NR_STACK_POINTER_REG) or
                (cs_opt_size in current_settings.optimizerswitches)
              ) and
              { If the flags register is in use, don't change the instruction
                to an ADD otherwise this will scramble the flags. [Kit] }
              not RegInUsedRegs(NR_DEFAULTFLAGS, UsedRegs) and
              ConvertLEA(taicpu(p)) then
              begin
                Result:=true;
                exit;
              end;
          end;

        { Don't optimise if the stack or frame pointer is the destination register }
        if (taicpu(p).oper[1]^.reg=NR_STACK_POINTER_REG) or (taicpu(p).oper[1]^.reg=current_procinfo.framepointer) then
          Exit;
        if GetNextInstruction(p,hp1) and
          (hp1.typ=ait_instruction) then
          begin
            if MatchInstruction(hp1,A_MOV,[taicpu(p).opsize]) and
              MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[0]^) and
              MatchOpType(Taicpu(hp1),top_reg,top_reg) then
              begin
                TransferUsedRegs(TmpUsedRegs);
                UpdateUsedRegs(TmpUsedRegs, tai(p.next));
                if not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp1,TmpUsedRegs)) then
                  begin
                    taicpu(p).loadoper(1,taicpu(hp1).oper[1]^);
                    DebugMsg(SPeepholeOptimization + 'LeaMov2Lea done',p);
                    RemoveInstruction(hp1);
                    result:=true;
                    exit;
                  end;
              end;

            { changes
                lea <ref1>, reg1
                <op> ...,<ref. with reg1>,...
                to
                <op> ...,<ref1>,... }

            { find a reference which uses reg1 }
            if (taicpu(hp1).ops>=1) and (taicpu(hp1).oper[0]^.typ=top_ref) and RegInOp(taicpu(p).oper[1]^.reg,taicpu(hp1).oper[0]^) then
              ref:=0
            else if (taicpu(hp1).ops>=2) and (taicpu(hp1).oper[1]^.typ=top_ref) and RegInOp(taicpu(p).oper[1]^.reg,taicpu(hp1).oper[1]^) then
              ref:=1
            else
              ref:=-1;
            if (ref<>-1) and
              { reg1 must be either the base or the index }
              ((taicpu(hp1).oper[ref]^.ref^.base=taicpu(p).oper[1]^.reg) xor (taicpu(hp1).oper[ref]^.ref^.index=taicpu(p).oper[1]^.reg)) then
              begin
                { reg1 can be removed from the reference }
                saveref:=taicpu(hp1).oper[ref]^.ref^;
                if taicpu(hp1).oper[ref]^.ref^.base=taicpu(p).oper[1]^.reg then
                  taicpu(hp1).oper[ref]^.ref^.base:=NR_NO
                else if taicpu(hp1).oper[ref]^.ref^.index=taicpu(p).oper[1]^.reg then
                  taicpu(hp1).oper[ref]^.ref^.index:=NR_NO
                else
                  Internalerror(2019111201);
                { check if the can insert all data of the lea into the second instruction }
                if ((taicpu(hp1).oper[ref]^.ref^.base=taicpu(p).oper[1]^.reg) or (taicpu(hp1).oper[ref]^.ref^.scalefactor <= 1)) and
                  ((taicpu(p).oper[0]^.ref^.base=NR_NO) or (taicpu(hp1).oper[ref]^.ref^.base=NR_NO)) and
                  ((taicpu(p).oper[0]^.ref^.index=NR_NO) or (taicpu(hp1).oper[ref]^.ref^.index=NR_NO)) and
                  ((taicpu(p).oper[0]^.ref^.symbol=nil) or (taicpu(hp1).oper[ref]^.ref^.symbol=nil)) and
                  ((taicpu(p).oper[0]^.ref^.relsymbol=nil) or (taicpu(hp1).oper[ref]^.ref^.relsymbol=nil)) and
                  ((taicpu(p).oper[0]^.ref^.scalefactor <= 1) or (taicpu(hp1).oper[ref]^.ref^.scalefactor <= 1)) and
                  (taicpu(p).oper[0]^.ref^.segment=NR_NO) and (taicpu(hp1).oper[ref]^.ref^.segment=NR_NO)
{$ifdef x86_64}
                  and (abs(taicpu(hp1).oper[ref]^.ref^.offset+taicpu(p).oper[0]^.ref^.offset)<=$7fffffff)
                  and (((taicpu(p).oper[0]^.ref^.base<>NR_RIP) and (taicpu(p).oper[0]^.ref^.index<>NR_RIP)) or
                       ((taicpu(hp1).oper[ref]^.ref^.base=NR_NO) and (taicpu(hp1).oper[ref]^.ref^.index=NR_NO))
                      )
{$endif x86_64}
                  then
                  begin
                    { reg1 might not used by the second instruction after it is remove from the reference }
                    if not(RegInInstruction(taicpu(p).oper[1]^.reg,taicpu(hp1))) then
                      begin
                        TransferUsedRegs(TmpUsedRegs);
                        UpdateUsedRegs(TmpUsedRegs, tai(p.next));
                        { reg1 is not updated so it might not be used afterwards }
                        if not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp1,TmpUsedRegs)) then
                          begin
                            DebugMsg(SPeepholeOptimization + 'LeaOp2Op done',p);
                            if taicpu(p).oper[0]^.ref^.base<>NR_NO then
                              taicpu(hp1).oper[ref]^.ref^.base:=taicpu(p).oper[0]^.ref^.base;
                            if taicpu(p).oper[0]^.ref^.index<>NR_NO then
                              taicpu(hp1).oper[ref]^.ref^.index:=taicpu(p).oper[0]^.ref^.index;
                            if taicpu(p).oper[0]^.ref^.symbol<>nil then
                              taicpu(hp1).oper[ref]^.ref^.symbol:=taicpu(p).oper[0]^.ref^.symbol;
                            if taicpu(p).oper[0]^.ref^.relsymbol<>nil then
                              taicpu(hp1).oper[ref]^.ref^.relsymbol:=taicpu(p).oper[0]^.ref^.relsymbol;
                            if taicpu(p).oper[0]^.ref^.scalefactor > 1 then
                              taicpu(hp1).oper[ref]^.ref^.scalefactor:=taicpu(p).oper[0]^.ref^.scalefactor;
                            inc(taicpu(hp1).oper[ref]^.ref^.offset,taicpu(p).oper[0]^.ref^.offset);
                            RemoveCurrentP(p, hp1);
                            result:=true;
                            exit;
                          end
                      end;
                  end;
                { recover }
                taicpu(hp1).oper[ref]^.ref^:=saveref;
              end;

            Adjacent := RegInInstruction(taicpu(p).oper[1]^.reg, hp1);
            if Adjacent or
              { Check further ahead (up to 2 instructions ahead for -O2) }
              GetNextInstructionUsingReg(hp1,hp1,taicpu(p).oper[1]^.reg) then
              begin
                { Check common LEA/LEA conditions }
                if MatchInstruction(hp1,A_LEA,[taicpu(p).opsize]) and
                  (taicpu(p).oper[0]^.ref^.relsymbol = nil) and
                  (taicpu(p).oper[0]^.ref^.segment = NR_NO) and
                  (taicpu(p).oper[0]^.ref^.symbol = nil) and
                  (taicpu(hp1).oper[0]^.ref^.relsymbol = nil) and
                  (taicpu(hp1).oper[0]^.ref^.segment = NR_NO) and
                  (taicpu(hp1).oper[0]^.ref^.symbol = nil) and
                  (
                    { If p and hp1 are adjacent, RegModifiedBetween always returns False, so avoid
                      calling it (since it calls GetNextInstruction) }
                    Adjacent or
                    (
                      (
                        (taicpu(p).oper[0]^.ref^.base = NR_NO) or { Don't call RegModifiedBetween unnecessarily }
                        not(RegModifiedBetween(taicpu(p).oper[0]^.ref^.base,p,hp1))
                      ) and (
                        (taicpu(p).oper[0]^.ref^.index = taicpu(p).oper[0]^.ref^.base) or { Don't call RegModifiedBetween unnecessarily }
                        (taicpu(p).oper[0]^.ref^.index = NR_NO) or
                        not(RegModifiedBetween(taicpu(p).oper[0]^.ref^.index,p,hp1))
                      )
                    )
                  ) then
                  begin
                    TransferUsedRegs(TmpUsedRegs);
                    hp2 := p;
                    repeat
                      UpdateUsedRegs(TmpUsedRegs, tai(hp2.Next));
                    until not GetNextInstruction(hp2, hp2) or (hp2 = hp1);

                    IntermediateRegDiscarded :=
                      (taicpu(p).oper[1]^.reg = taicpu(hp1).oper[1]^.reg) or
                      not RegUsedAfterInstruction(taicpu(p).oper[1]^.reg, hp1, TmpUsedRegs);

                    { changes
                        lea offset1(regX,scale), reg1
                        lea offset2(reg1,reg1), reg2
                        to
                        lea (offset1*scale*2)+offset2(regX,scale*2), reg2

                      and
                        lea offset1(regX,scale1), reg1
                        lea offset2(reg1,scale2), reg2
                        to
                        lea (offset1*scale1*2)+offset2(regX,scale1*scale2), reg2

                      and
                        lea offset1(regX,scale1), reg1
                        lea offset2(reg3,reg1,scale2), reg2
                        to
                        lea (offset1*scale*2)+offset2(reg3,regX,scale1*scale2), reg2

                      ... so long as the final scale does not exceed 8

                      (Similarly, allow the first instruction to be "lea (regX,regX),reg1")
                      }
                    if (taicpu(p).oper[0]^.ref^.base<>NR_STACK_POINTER_REG) and { lea (%rsp,scale),reg is not a valid encoding }
                      (
                        { Don't optimise if size is a concern and the intermediate register remains in use }
                        IntermediateRegDiscarded or
                        not (cs_opt_size in current_settings.optimizerswitches)
                      ) and
                      (taicpu(hp1).oper[0]^.ref^.index = taicpu(p).oper[1]^.reg) and
                      (
                        (taicpu(p).oper[0]^.ref^.base <> taicpu(p).oper[0]^.ref^.index) or
                        (taicpu(p).oper[0]^.ref^.scalefactor <= 1)
                      ) and (
                        (
                          { lea (reg1,scale2), reg2 variant }
                          (taicpu(hp1).oper[0]^.ref^.base <> taicpu(p).oper[1]^.reg) and
                          (
                            Adjacent or
                            not RegModifiedBetween(taicpu(hp1).oper[0]^.ref^.base, p, hp1)
                          ) and
                          (
                            (
                              (taicpu(p).oper[0]^.ref^.base = NR_NO) and
                              (taicpu(hp1).oper[0]^.ref^.scalefactor * taicpu(p).oper[0]^.ref^.scalefactor <= 8)
                            ) or (
                              { lea (regX,regX), reg1 variant }
                              (taicpu(p).oper[0]^.ref^.base = taicpu(p).oper[0]^.ref^.index) and
                              (taicpu(hp1).oper[0]^.ref^.scalefactor <= 4)
                            )
                          )
                        ) or (
                          { lea (reg1,reg1), reg1 variant }
                          (taicpu(hp1).oper[0]^.ref^.base = taicpu(p).oper[1]^.reg) and
                          (taicpu(hp1).oper[0]^.ref^.scalefactor <= 1)
                        )
                      ) then
                      begin
                        { Make everything homogeneous to make calculations easier }
                        if (taicpu(p).oper[0]^.ref^.base <> NR_NO) then
                          begin
                            if taicpu(p).oper[0]^.ref^.index <> NR_NO then
                              { Convert lea (regX,regX),reg1 to lea (regX,2),reg1 }
                              taicpu(p).oper[0]^.ref^.scalefactor := 2
                            else
                              taicpu(p).oper[0]^.ref^.index := taicpu(p).oper[0]^.ref^.base;

                            taicpu(p).oper[0]^.ref^.base := NR_NO;
                          end;

                        { Make sure the offset doesn't go out of range (use 64-bit arithmetic)}
                        offsetcalc := taicpu(hp1).oper[0]^.ref^.offset;
                        Inc(offsetcalc, Int64(taicpu(p).oper[0]^.ref^.offset) * max(taicpu(hp1).oper[0]^.ref^.scalefactor, 1));

                        if (offsetcalc <= $7FFFFFFF) and (offsetcalc >= -2147483648) then
                          begin
                            if (taicpu(hp1).oper[0]^.ref^.base = taicpu(p).oper[1]^.reg) and
                              (taicpu(hp1).oper[0]^.ref^.index <> taicpu(p).oper[1]^.reg) then
                              begin
                                { Put the register to change in the index register }
                                TempReg := taicpu(hp1).oper[0]^.ref^.index;
                                taicpu(hp1).oper[0]^.ref^.index := taicpu(hp1).oper[0]^.ref^.base;
                                taicpu(hp1).oper[0]^.ref^.base := TempReg;
                              end;

                            { Change lea (reg,reg) to lea(,reg,2) }
                            if (taicpu(hp1).oper[0]^.ref^.base = taicpu(p).oper[1]^.reg) then
                              begin
                                taicpu(hp1).oper[0]^.ref^.base := NR_NO;
                                taicpu(hp1).oper[0]^.ref^.scalefactor := 2;
                              end;

                            if (taicpu(p).oper[0]^.ref^.offset <> 0) then
                              Inc(taicpu(hp1).oper[0]^.ref^.offset, taicpu(p).oper[0]^.ref^.offset * max(taicpu(hp1).oper[0]^.ref^.scalefactor, 1));
                            taicpu(hp1).oper[0]^.ref^.index := taicpu(p).oper[0]^.ref^.index;

                            { Just to prevent miscalculations }
                            if (taicpu(hp1).oper[0]^.ref^.scalefactor = 0) then
                              taicpu(hp1).oper[0]^.ref^.scalefactor := taicpu(p).oper[0]^.ref^.scalefactor
                            else
                              taicpu(hp1).oper[0]^.ref^.scalefactor := taicpu(hp1).oper[0]^.ref^.scalefactor * max(taicpu(p).oper[0]^.ref^.scalefactor, 1);

                            { Only remove the first LEA if we don't need the intermediate register's value as is }
                            if IntermediateRegDiscarded then
                              begin
                                DebugMsg(SPeepholeOptimization + 'LeaLea2Lea 2 done',p);
                                RemoveCurrentP(p);
                              end
                            else
                              DebugMsg(SPeepholeOptimization + 'LeaLea2LeaLea 2 done (intermediate register still in use)',p);

                            result:=true;
                            exit;
                          end;
                      end;

                    { changes
                        lea offset1(regX), reg1
                        lea offset2(reg1), reg2
                        to
                        lea offset1+offset2(regX), reg2 }
                    if (
                        { Don't optimise if size is a concern and the intermediate register remains in use }
                        IntermediateRegDiscarded or
                        not (cs_opt_size in current_settings.optimizerswitches)
                      ) and
                      (
                        (
                          (taicpu(hp1).oper[0]^.ref^.index = taicpu(p).oper[1]^.reg) and
                          (getsupreg(taicpu(p).oper[0]^.ref^.base)<>RS_ESP) and
                          (taicpu(p).oper[0]^.ref^.index = NR_NO)
                        ) or (
                          (taicpu(hp1).oper[0]^.ref^.base = taicpu(p).oper[1]^.reg) and
                          (taicpu(hp1).oper[0]^.ref^.scalefactor <= 1) and
                          (
                            (
                              (taicpu(p).oper[0]^.ref^.index = NR_NO) or
                              (taicpu(p).oper[0]^.ref^.base = NR_NO)
                            ) or (
                              (taicpu(p).oper[0]^.ref^.scalefactor <= 1) and
                              (
                                (taicpu(p).oper[0]^.ref^.index = NR_NO) or
                                (
                                  (taicpu(p).oper[0]^.ref^.index = taicpu(p).oper[0]^.ref^.base) and
                                  (
                                    (taicpu(hp1).oper[0]^.ref^.index = NR_NO) or
                                    (taicpu(hp1).oper[0]^.ref^.base = NR_NO)
                                  )
                                )
                              )
                            )
                          )
                        )
                      ) then
                      begin
                        { Make sure the offset doesn't go out of range (use 64-bit arithmetic)}
                        offsetcalc := taicpu(hp1).oper[0]^.ref^.offset;
                        Inc(offsetcalc, Int64(taicpu(p).oper[0]^.ref^.offset) * max(taicpu(hp1).oper[0]^.ref^.scalefactor, 1));

                        if (offsetcalc <= $7FFFFFFF) and (offsetcalc >= -2147483648) then
                          begin
                            if taicpu(hp1).oper[0]^.ref^.index=taicpu(p).oper[1]^.reg then
                              begin
                                taicpu(hp1).oper[0]^.ref^.index:=taicpu(p).oper[0]^.ref^.base;
                                inc(taicpu(hp1).oper[0]^.ref^.offset,taicpu(p).oper[0]^.ref^.offset*max(taicpu(hp1).oper[0]^.ref^.scalefactor,1));
                                { if the register is used as index and base, we have to increase for base as well
                                  and adapt base }
                                if taicpu(hp1).oper[0]^.ref^.base=taicpu(p).oper[1]^.reg then
                                  begin
                                    taicpu(hp1).oper[0]^.ref^.base:=taicpu(p).oper[0]^.ref^.base;
                                    inc(taicpu(hp1).oper[0]^.ref^.offset,taicpu(p).oper[0]^.ref^.offset);
                                  end;
                              end
                            else
                              begin
                                inc(taicpu(hp1).oper[0]^.ref^.offset,taicpu(p).oper[0]^.ref^.offset);
                                taicpu(hp1).oper[0]^.ref^.base:=taicpu(p).oper[0]^.ref^.base;
                              end;
                            if taicpu(p).oper[0]^.ref^.index<>NR_NO then
                              begin
                                taicpu(hp1).oper[0]^.ref^.base:=taicpu(hp1).oper[0]^.ref^.index;
                                taicpu(hp1).oper[0]^.ref^.index:=taicpu(p).oper[0]^.ref^.index;
                                taicpu(hp1).oper[0]^.ref^.scalefactor:=taicpu(p).oper[0]^.ref^.scalefactor;
                              end;

                            { Only remove the first LEA if we don't need the intermediate register's value as is }
                            if IntermediateRegDiscarded then
                              begin
                                DebugMsg(SPeepholeOptimization + 'LeaLea2Lea 1 done',p);
                                RemoveCurrentP(p);
                              end
                            else
                              DebugMsg(SPeepholeOptimization + 'LeaLea2LeaLea 1 done (intermediate register still in use)',p);


                            result:=true;
                            exit;
                          end;
                      end;
                  end;

                { Change:
                    leal/q $x(%reg1),%reg2
                    ...
                    shll/q $y,%reg2
                  To:
                    leal/q $(x+2^y)(%reg1,2^y),%reg2 (if y <= 3)
                }
                if (taicpu(p).oper[0]^.ref^.base<>NR_STACK_POINTER_REG) and { lea (%rsp,scale),reg is not a valid encoding }
                  MatchInstruction(hp1, A_SHL, [taicpu(p).opsize]) and
                  MatchOpType(taicpu(hp1), top_const, top_reg) and
                  (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) and
                  (taicpu(hp1).oper[0]^.val <= 3) then
                  begin
                    Multiple := 1 shl taicpu(hp1).oper[0]^.val;
                    TransferUsedRegs(TmpUsedRegs);
                    UpdateUsedRegs(TmpUsedRegs, tai(hp1.Next));

                    if
                      { This allows the optimisation in some circumstances even if the lea instruction already has a scale factor
                        (this works even if scalefactor is zero) }
                      ((Multiple * taicpu(p).oper[0]^.ref^.scalefactor) <= 8) and

                      { Ensure offset doesn't go out of bounds }
                      (abs(taicpu(p).oper[0]^.ref^.offset * Multiple) <= $7FFFFFFF) and

                      not (RegInUsedRegs(NR_DEFAULTFLAGS,TmpUsedRegs)) and
                      (
                        (
                          not SuperRegistersEqual(taicpu(p).oper[0]^.ref^.base, taicpu(p).oper[1]^.reg) and
                          (
                            (taicpu(p).oper[0]^.ref^.index = NR_NO) or
                            (taicpu(p).oper[0]^.ref^.index = NR_INVALID) or
                            (
                              { Check for lea $x(%reg1,%reg1),%reg2 and treat as it it were lea $x(%reg1,2),%reg2 }
                              (taicpu(p).oper[0]^.ref^.index = taicpu(p).oper[0]^.ref^.base) and
                              (taicpu(p).oper[0]^.ref^.scalefactor <= 1)
                            )
                          )
                        ) or (
                          (
                            (taicpu(p).oper[0]^.ref^.base = NR_NO) or
                            (taicpu(p).oper[0]^.ref^.base = NR_INVALID)
                          ) and
                          not SuperRegistersEqual(taicpu(p).oper[0]^.ref^.index, taicpu(p).oper[1]^.reg)
                        )
                      ) then
                      begin
                        repeat
                          with taicpu(p).oper[0]^.ref^ do
                            begin
                              { Convert lea $x(%reg1,%reg1),%reg2 to lea $x(%reg1,2),%reg2 }
                              if index = base then
                                begin
                                  if Multiple > 4 then
                                    { Optimisation will no longer work because resultant
                                      scale factor will exceed 8 }
                                    Break;

                                  base := NR_NO;
                                  scalefactor := 2;
                                  DebugMsg(SPeepholeOptimization + 'lea $x(%reg1,%reg1),%reg2 -> lea $x(%reg1,2),%reg2 for following optimisation', p);
                                end
                              else if (base <> NR_NO) and (base <> NR_INVALID) then
                                begin
                                  { Scale factor only works on the index register }
                                  index := base;
                                  base := NR_NO;
                                end;

                              { For safety }
                              if scalefactor <= 1 then
                                begin
                                  DebugMsg(SPeepholeOptimization + 'LeaShl2Lea 1', p);
                                  scalefactor := Multiple;
                                end
                              else
                                begin
                                  DebugMsg(SPeepholeOptimization + 'LeaShl2Lea 2', p);
                                  scalefactor := scalefactor * Multiple;
                                end;

                              offset := offset * Multiple;
                            end;
                          RemoveInstruction(hp1);
                          Result := True;
                          Exit;
                        { This repeat..until loop exists for the benefit of Break }
                        until True;
                      end;
                  end;
              end;
          end;
      end;


    function TX86AsmOptimizer.DoArithCombineOpt(var p: tai): Boolean;
      var
        hp1 : tai;
        SubInstr: Boolean;
        ThisConst: TCGInt;

      const
        OverflowMin: array[S_B..S_Q] of TCGInt = (-128, -32768, -2147483648, -2147483648);
         { Note: 64-bit-sized arithmetic instructions can only take signed 32-bit immediates }
        OverflowMax: array[S_B..S_Q] of TCGInt = ( 255,  65535,   $FFFFFFFF,  2147483647);
      begin
        Result := False;

        if taicpu(p).oper[0]^.typ <> top_const then
          { Should have been confirmed before calling }
          InternalError(2021102601);

        SubInstr := (taicpu(p).opcode = A_SUB);

        if GetLastInstruction(p, hp1) and
           (hp1.typ = ait_instruction) and
           (taicpu(hp1).opsize = taicpu(p).opsize) then
          begin
            if not (taicpu(p).opsize in [S_B, S_W, S_L{$ifdef x86_64}, S_Q{$endif x86_64}]) then
              { Bad size }
              InternalError(2022042001);

            case taicpu(hp1).opcode Of
              A_INC:
                if MatchOperand(taicpu(hp1).oper[0]^,taicpu(p).oper[1]^) then
                  begin
                    if SubInstr then
                      ThisConst := taicpu(p).oper[0]^.val - 1
                    else
                      ThisConst := taicpu(p).oper[0]^.val + 1;
                  end
                else
                  Exit;
              A_DEC:
                if MatchOperand(taicpu(hp1).oper[0]^,taicpu(p).oper[1]^) then
                  begin
                    if SubInstr then
                      ThisConst := taicpu(p).oper[0]^.val + 1
                    else
                      ThisConst := taicpu(p).oper[0]^.val - 1;
                  end
                else
                  Exit;
              A_SUB:
                 if (taicpu(hp1).oper[0]^.typ = top_const) and
                   MatchOperand(taicpu(hp1).oper[1]^,taicpu(p).oper[1]^) then
                   begin
                     if SubInstr then
                       ThisConst := taicpu(p).oper[0]^.val + taicpu(hp1).oper[0]^.val
                     else
                       ThisConst := taicpu(p).oper[0]^.val - taicpu(hp1).oper[0]^.val;
                   end
                 else
                   Exit;
              A_ADD:
                 if (taicpu(hp1).oper[0]^.typ = top_const) and
                   MatchOperand(taicpu(hp1).oper[1]^,taicpu(p).oper[1]^) then
                   begin
                     if SubInstr then
                       ThisConst := taicpu(p).oper[0]^.val - taicpu(hp1).oper[0]^.val
                     else
                       ThisConst := taicpu(p).oper[0]^.val + taicpu(hp1).oper[0]^.val;
                   end
                 else
                   Exit;
              else
                Exit;
            end;

            { Check that the values are in range }
            if (ThisConst < OverflowMin[taicpu(p).opsize]) or (ThisConst > OverflowMax[taicpu(p).opsize]) then
              { Overflow; abort }
              Exit;

            if (ThisConst = 0) then
              begin
                DebugMsg(SPeepholeOptimization + 'Arithmetic combine: ' +
                  debug_op2str(taicpu(hp1).opcode) + ' $' + debug_tostr(taicpu(hp1).oper[0]^.val) + ',' + debug_operstr(taicpu(hp1).oper[1]^) + '; ' +
                  debug_op2str(taicpu(p).opcode) + ' $' + debug_tostr(taicpu(p).oper[0]^.val) + ',' + debug_operstr(taicpu(p).oper[1]^) + ' cancel out (NOP)', p);

                RemoveInstruction(hp1);
                hp1 := tai(p.next);
                RemoveInstruction(p); { Note, the choice to not use RemoveCurrentp is deliberate }
                if not GetLastInstruction(hp1, p) then
                  p := hp1;
              end
            else
              begin
                if taicpu(hp1).opercnt=1 then
                  DebugMsg(SPeepholeOptimization + 'Arithmetic combine: ' +
                    debug_op2str(taicpu(hp1).opcode) + ' $' + debug_tostr(taicpu(hp1).oper[0]^.val) + '; ' +
                    debug_op2str(taicpu(p).opcode) + ' $' + debug_tostr(taicpu(p).oper[0]^.val) + ',' + debug_operstr(taicpu(p).oper[1]^) + ' -> ' +
                    debug_op2str(taicpu(p).opcode) + ' $' + debug_tostr(ThisConst) + ' ' + debug_operstr(taicpu(p).oper[1]^), p)
                else
                  DebugMsg(SPeepholeOptimization + 'Arithmetic combine: ' +
                    debug_op2str(taicpu(hp1).opcode) + ' $' + debug_tostr(taicpu(hp1).oper[0]^.val) + ',' + debug_operstr(taicpu(hp1).oper[1]^) + '; ' +
                    debug_op2str(taicpu(p).opcode) + ' $' + debug_tostr(taicpu(p).oper[0]^.val) + ',' + debug_operstr(taicpu(p).oper[1]^) + ' -> ' +
                    debug_op2str(taicpu(p).opcode) + ' $' + debug_tostr(ThisConst) + ' ' + debug_operstr(taicpu(p).oper[1]^), p);

                RemoveInstruction(hp1);
                taicpu(p).loadconst(0, ThisConst);
              end;

            Result := True;
          end;
      end;


    function TX86AsmOptimizer.DoMovCmpMemOpt(var p : tai; const hp1: tai) : Boolean;
      begin
        Result := False;
        if MatchOpType(taicpu(p),top_ref,top_reg) and
          { The x86 assemblers have difficulty comparing values against absolute addresses }
          (taicpu(p).oper[0]^.ref^.refaddr <> addr_full) and
          (taicpu(hp1).oper[0]^.typ <> top_ref) and
          MatchOperand(taicpu(hp1).oper[1]^, taicpu(p).oper[1]^.reg) and
          (
            (
              (taicpu(hp1).opcode = A_TEST)
            ) or (
              (taicpu(hp1).opcode = A_CMP) and
              { A sanity check more than anything }
              not MatchOperand(taicpu(hp1).oper[0]^, taicpu(p).oper[1]^.reg)
            )
          ) then
          begin
            { change
                mov      mem, %reg
                ...
                cmp/test x,   %reg / test %reg,%reg
                (reg deallocated)

                to

                cmp/test x,   mem  / cmp  0,   mem
            }
            TransferUsedRegs(TmpUsedRegs);
            UpdateUsedRegs(TmpUsedRegs, tai(p.Next));
            if not RegUsedAfterInstruction(taicpu(p).oper[1]^.reg, hp1, TmpUsedRegs) then
              begin
                { Convert test %reg,%reg or test $-1,%reg to cmp $0,mem }
                if (taicpu(hp1).opcode = A_TEST) and
                  (
                    MatchOperand(taicpu(hp1).oper[0]^, taicpu(p).oper[1]^.reg) or
                    MatchOperand(taicpu(hp1).oper[0]^, -1)
                  ) then
                  begin
                    taicpu(hp1).opcode := A_CMP;
                    taicpu(hp1).loadconst(0, 0);
                  end;
                taicpu(hp1).loadref(1, taicpu(p).oper[0]^.ref^);
                DebugMsg(SPeepholeOptimization + 'MOV/CMP -> CMP (memory check)', p);

                RemoveCurrentP(p);

                if (p <> hp1) then
                  { Correctly update TmpUsedRegs if p and hp1 aren't adjacent }
                  UpdateUsedRegsBetween(TmpUsedRegs, p, hp1);

                { Make sure the flags are allocated across the CMP instruction }
                if not RegInUsedRegs(NR_DEFAULTFLAGS, TmpUsedRegs) then
                  AllocRegBetween(NR_DEFAULTFLAGS, hp1, hp1, TmpUsedRegs);

                Result := True;
                Exit;
              end;
          end;
      end;


    function TX86AsmOptimizer.DoSETccLblRETOpt(var p: tai; const hp_label: tai_label) : Boolean;
      var
        hp_allocstart, hp_pos, hp2, hp3, hp4, hp5, hp6: tai;
        ThisReg, SecondReg: TRegister;
        JumpLoc: TAsmLabel;
        NewSize: TOpSize;
      begin
        Result := False;
        {
          Convert:
            j<c>  .L1
          .L2:
            mov   1,reg
            jmp   .L3    (or ret, although it might not be a RET yet)
          .L1:
            mov   0,reg
            jmp   .L3    (or ret)

            ( As long as .L3 <> .L1 or .L2)

          To:
            mov   0,reg
            set<not(c)> reg
            jmp   .L3    (or ret)
          .L2:
            mov   1,reg
            jmp   .L3    (or ret)
          .L1:
            mov   0,reg
            jmp   .L3    (or ret)
        }

        if JumpTargetOp(taicpu(p))^.ref^.refaddr<>addr_full then
          Exit;

        JumpLoc := TAsmLabel(JumpTargetOp(taicpu(p))^.ref^.symbol);

        if GetNextInstruction(hp_label, hp2) and
          MatchInstruction(hp2,A_MOV,[]) and
          (taicpu(hp2).oper[0]^.typ = top_const) and
          (
            (
              (taicpu(hp2).oper[1]^.typ = top_reg)
 {$ifdef i386}
              { Under i386, ESI, EDI, EBP and ESP
                don't have an 8-bit representation }
               and not (getsupreg(taicpu(hp2).oper[1]^.reg) in [RS_ESI, RS_EDI, RS_EBP, RS_ESP])
 {$endif i386}
            ) or (
 {$ifdef i386}
              (taicpu(hp2).oper[1]^.typ <> top_reg) and
 {$endif i386}
              (taicpu(hp2).opsize = S_B)
            )
          ) and
          GetNextInstruction(hp2, hp3) and
          MatchInstruction(hp3, A_JMP, A_RET, []) and
          (
            (taicpu(hp3).opcode=A_RET) or
            (
              (taicpu(hp3).oper[0]^.ref^.refaddr=addr_full) and
              (tasmlabel(taicpu(hp3).oper[0]^.ref^.symbol)<>tai_label(hp_label).labsym)
            )
          ) and
          GetNextInstruction(hp3, hp4) and
          SkipAligns(hp4, hp4) and
          (hp4.typ=ait_label) and
          (tai_label(hp4).labsym=JumpLoc) and
          (
            not (cs_opt_size in current_settings.optimizerswitches) or
            { If the initial jump is the label's only reference, then it will
              become a dead label if the other conditions are met and hence
              remove at least 2 instructions, including a jump }
            (JumpLoc.getrefs = 1)
          ) and
          { Don't check if hp3 jumps to hp4 because this is a zero-distance jump
            that will be optimised out }
          GetNextInstruction(hp4, hp5) and
          MatchInstruction(hp5,A_MOV,[taicpu(hp2).opsize]) and
          (taicpu(hp5).oper[0]^.typ = top_const) and
          (
            ((taicpu(hp2).oper[0]^.val = 0) and (taicpu(hp5).oper[0]^.val = 1)) or
            ((taicpu(hp2).oper[0]^.val = 1) and (taicpu(hp5).oper[0]^.val = 0))
          ) and
          MatchOperand(taicpu(hp2).oper[1]^,taicpu(hp5).oper[1]^) and
          GetNextInstruction(hp5,hp6) and
          (
            (hp6.typ<>ait_label) or
            SkipLabels(hp6, hp6)
          ) and
          (hp6.typ=ait_instruction) then
          begin
            { First, let's look at the two jumps that are hp3 and hp6 }
            if not
              (
                (taicpu(hp6).opcode=taicpu(hp3).opcode) and { Both RET or both JMP to the same label }
                (
                  (taicpu(hp6).opcode=A_RET) or
                  MatchOperand(taicpu(hp6).oper[0]^, taicpu(hp3).oper[0]^)
                )
              ) then
              { If condition is False, then the JMP/RET instructions matched conventionally }
              begin
                { See if one of the jumps can be instantly converted into a RET }
                if (taicpu(hp3).opcode=A_JMP) then
                  begin
                    { Reuse hp5 }
                    hp5 := getlabelwithsym(TAsmLabel(JumpTargetOp(taicpu(hp3))^.ref^.symbol));

                    { Make sure hp5 doesn't jump back to .L2 (infinite loop) }
                    if not Assigned(hp5) or (hp5=hp4) or not GetNextInstruction(hp5, hp5) then
                      Exit;

                    if MatchInstruction(hp5, A_RET, []) then
                      begin
                        DebugMsg(SPeepholeOptimization + 'Converted JMP to RET as part of SETcc optimisation (1st jump)', hp3);
                        ConvertJumpToRET(hp3, hp5);
                        Result := True;
                      end
                    else
                      Exit;
                  end;

                if (taicpu(hp6).opcode=A_JMP) then
                  begin
                    { Reuse hp5 }
                    hp5 := getlabelwithsym(TAsmLabel(JumpTargetOp(taicpu(hp6))^.ref^.symbol));
                    if not Assigned(hp5) or not GetNextInstruction(hp5, hp5) then
                      Exit;

                    if MatchInstruction(hp5, A_RET, []) then
                      begin
                        DebugMsg(SPeepholeOptimization + 'Converted JMP to RET as part of SETcc optimisation (2nd jump)', hp6);
                        ConvertJumpToRET(hp6, hp5);
                        Result := True;
                      end
                    else
                      Exit;
                  end;

                if not
                  (
                    (taicpu(hp6).opcode=taicpu(hp3).opcode) and { Both RET or both JMP to the same label }
                    (
                      (taicpu(hp6).opcode=A_RET) or
                      MatchOperand(taicpu(hp6).oper[0]^, taicpu(hp3).oper[0]^)
                    )
                  ) then
                  { Still doesn't match }
                  Exit;
              end;


            if (taicpu(hp2).oper[0]^.val = 1) then
              begin
                taicpu(p).condition := inverse_cond(taicpu(p).condition);
                DebugMsg(SPeepholeOptimization + 'J(c)Mov1Jmp/RetMov0Jmp/Ret -> Set(~c)Jmp/Ret',p)
              end
            else
              DebugMsg(SPeepholeOptimization + 'J(c)Mov0Jmp/RetMov1Jmp/Ret -> Set(c)Jmp/Ret',p);

            if taicpu(hp2).opsize=S_B then
              begin
                if taicpu(hp2).oper[1]^.typ = top_reg then
                  begin
                    SecondReg := taicpu(hp2).oper[1]^.reg;
                    hp4:=taicpu.op_reg(A_SETcc, S_B, SecondReg);
                  end
                else
                  begin
                    hp4:=taicpu.op_ref(A_SETcc, S_B, taicpu(hp2).oper[1]^.ref^);
                    SecondReg := NR_NO;
                  end;

                hp_pos := p;
                hp_allocstart := hp4;
              end
            else
              begin
                { Will be a register because the size can't be S_B otherwise }
                SecondReg:=taicpu(hp2).oper[1]^.reg;
                ThisReg:=newreg(R_INTREGISTER,getsupreg(SecondReg), R_SUBL);
                hp4:=taicpu.op_reg(A_SETcc, S_B, ThisReg);

                if (cs_opt_size in current_settings.optimizerswitches) then
                  begin
                    { Favour using MOVZX when optimising for size }
                    case taicpu(hp2).opsize of
                      S_W:
                        NewSize := S_BW;
                      S_L:
                        NewSize := S_BL;
{$ifdef x86_64}
                      S_Q:
                        begin
                          NewSize := S_BL;
                          { Will implicitly zero-extend to 64-bit }
                          setsubreg(SecondReg, R_SUBD);
                        end;
{$endif x86_64}
                      else
                        InternalError(2022101301);
                    end;

                    hp5:=taicpu.op_reg_reg(A_MOVZX, NewSize, ThisReg, SecondReg);
                    { Inserting it right before p will guarantee that the flags are also tracked }
                    Asml.InsertBefore(hp5, p);

                    { Make sure the SET instruction gets inserted before the MOVZX instruction }
                    hp_pos := hp5;
                    hp_allocstart := hp4;
                  end
                else
                  begin
                    hp5:=taicpu.op_const_reg(A_MOV, taicpu(hp2).opsize, 0, SecondReg);
                    { Inserting it right before p will guarantee that the flags are also tracked }
                    Asml.InsertBefore(hp5, p);

                    hp_pos := p;
                    hp_allocstart := hp5;
                  end;

                taicpu(hp5).fileinfo:=taicpu(p).fileinfo;

              end;

            taicpu(hp4).fileinfo := taicpu(p).fileinfo;
            taicpu(hp4).condition := taicpu(p).condition;
            asml.InsertBefore(hp4, hp_pos);

            if taicpu(hp3).is_jmp then
              begin
                JumpLoc.decrefs;
                MakeUnconditional(taicpu(p));
                taicpu(p).loadref(0, JumpTargetOp(taicpu(hp3))^.ref^);
                TAsmLabel(JumpTargetOp(taicpu(hp3))^.ref^.symbol).increfs;
              end
            else
              ConvertJumpToRET(p, hp3);

            if SecondReg <> NR_NO then
              { Ensure the destination register is allocated over this region }
              AllocRegBetween(SecondReg, hp_allocstart, p, UsedRegs);

            if (JumpLoc.getrefs = 0) then
              RemoveDeadCodeAfterJump(hp3);

            Result:=true;
            exit;
          end;
      end;


    function TX86AsmOptimizer.OptPass1Sub(var p : tai) : boolean;
      var
        hp1, hp2: tai;
        ActiveReg: TRegister;
        OldOffset: asizeint;
        ThisConst: TCGInt;

      function RegDeallocated: Boolean;
        begin
          TransferUsedRegs(TmpUsedRegs);
          UpdateUsedRegs(TmpUsedRegs, tai(p.next));
          Result := not(RegUsedAfterInstruction(ActiveReg,hp1,TmpUsedRegs))
        end;

      begin
        Result:=false;
        hp1 := nil;
        { replace
            subX     const,%reg1
            leaX     (%reg1,%reg1,Y),%reg2   // Base or index might not be equal to reg1
            dealloc  %reg1

            by

            leaX     -const-const*Y(%reg1,%reg1,Y),%reg2
        }
        if MatchOpType(taicpu(p),top_const,top_reg) then
          begin
            ActiveReg := taicpu(p).oper[1]^.reg;
            { Ensures the entire register was updated }
            if (taicpu(p).opsize >= S_L) and
              GetNextInstructionUsingReg(p,hp1, ActiveReg) and
              MatchInstruction(hp1,A_LEA,[]) and
              (SuperRegistersEqual(ActiveReg, taicpu(hp1).oper[0]^.ref^.base) or
               SuperRegistersEqual(ActiveReg, taicpu(hp1).oper[0]^.ref^.index)) and
              (
                { Cover the case where the register in the reference is also the destination register }
                Reg1WriteOverwritesReg2Entirely(taicpu(hp1).oper[1]^.reg, ActiveReg) or
                (
                  { Try to avoid the expensive check of RegUsedAfterInstruction if we know it will return False }
                  not SuperRegistersEqual(taicpu(hp1).oper[1]^.reg, ActiveReg) and
                  RegDeallocated
                )
              ) then
              begin
                OldOffset := taicpu(hp1).oper[0]^.ref^.offset;

                if ActiveReg=taicpu(hp1).oper[0]^.ref^.base then
                  Dec(taicpu(hp1).oper[0]^.ref^.offset,taicpu(p).oper[0]^.val);
                if ActiveReg=taicpu(hp1).oper[0]^.ref^.index then
                  Dec(taicpu(hp1).oper[0]^.ref^.offset,taicpu(p).oper[0]^.val*max(taicpu(hp1).oper[0]^.ref^.scalefactor,1));

{$ifdef x86_64}
                if (taicpu(hp1).oper[0]^.ref^.offset > $7FFFFFFF) or (taicpu(hp1).oper[0]^.ref^.offset < -2147483648) then
                  begin
                    { Overflow; abort }
                    taicpu(hp1).oper[0]^.ref^.offset := OldOffset;
                  end
                else
{$endif x86_64}
                  begin
                    DebugMsg(SPeepholeOptimization + 'SubLea2Lea done',p);

                    if not (cs_opt_level3 in current_settings.optimizerswitches) then
                      { hp1 is the immediate next instruction for sure - good for a quick speed boost }
                      RemoveCurrentP(p, hp1)
                    else
                      RemoveCurrentP(p);

                    result:=true;
                    Exit;
                  end;
              end;

            if (
                { Save calling GetNextInstructionUsingReg again }
                Assigned(hp1) or
                GetNextInstructionUsingReg(p,hp1, ActiveReg)
              ) and
              MatchInstruction(hp1,A_SUB,[taicpu(p).opsize]) and
              (taicpu(hp1).oper[1]^.reg = ActiveReg) then
              begin
                if taicpu(hp1).oper[0]^.typ = top_const then
                  begin
                    { Merge add const1,%reg; add const2,%reg to add const1+const2,%reg }
                    ThisConst := taicpu(p).oper[0]^.val + taicpu(hp1).oper[0]^.val;
                    Result := True;

                    { Handle any overflows }
                    case taicpu(p).opsize of
                      S_B:
                        taicpu(p).oper[0]^.val := ThisConst and $FF;
                      S_W:
                        taicpu(p).oper[0]^.val := ThisConst and $FFFF;
                      S_L:
                        taicpu(p).oper[0]^.val := ThisConst and $FFFFFFFF;
{$ifdef x86_64}
                      S_Q:
                        if (ThisConst > $7FFFFFFF) or (ThisConst < -2147483648) then
                          { Overflow; abort }
                          Result := False
                        else
                          taicpu(p).oper[0]^.val := ThisConst;
{$endif x86_64}
                      else
                        InternalError(2021102611);
                    end;

                    { Result may get set to False again if the combined immediate overflows for S_Q sizes }
                    if Result then
                      begin
                        if (taicpu(p).oper[0]^.val < 0) and
                          (
                            ((taicpu(p).opsize = S_B) and (taicpu(p).oper[0]^.val <> -128)) or
                            ((taicpu(p).opsize = S_W) and (taicpu(p).oper[0]^.val <> -32768)) or
                            ((taicpu(p).opsize in [S_L{$ifdef x86_64}, S_Q{$endif x86_64}]) and (taicpu(p).oper[0]^.val <> -2147483648))
                          ) then
                          begin
                            DebugMsg(SPeepholeOptimization + 'SUB; ADD/SUB -> ADD',p);
                            taicpu(p).opcode := A_SUB;
                            taicpu(p).oper[0]^.val := -taicpu(p).oper[0]^.val;
                          end
                        else
                          DebugMsg(SPeepholeOptimization + 'SUB; ADD/SUB -> SUB',p);
                        RemoveInstruction(hp1);
                      end;
                  end
                else
                  begin
                    { Make doubly sure the flags aren't in use because the order of subtractions may affect them }
                    TransferUsedRegs(TmpUsedRegs);
                    UpdateUsedRegs(TmpUsedRegs, tai(p.next));
                    hp2 := p;

                    while not (cs_opt_level3 in current_settings.optimizerswitches) and
                      GetNextInstruction(hp2, hp2) and (hp2 <> hp1) do
                      UpdateUsedRegs(TmpUsedRegs, tai(hp2.next));

                    if not RegInUsedRegs(NR_DEFAULTFLAGS, TmpUsedRegs) then
                      begin
                        { Move the constant subtraction to after the reg/ref addition to improve optimisation }
                        DebugMsg(SPeepholeOptimization + 'Add/sub swap 1b done',p);
                        Asml.Remove(p);
                        Asml.InsertAfter(p, hp1);
                        p := hp1;
                        Result := True;
                        Exit;
                      end;
                  end;
              end;

        { * change "subl $2, %esp; pushw x" to "pushl x"}
        { * change "sub/add const1, reg" or "dec reg" followed by
            "sub const2, reg" to one "sub ..., reg" }
{$ifdef i386}
            if (taicpu(p).oper[0]^.val = 2) and
               (ActiveReg = NR_ESP) and
               { Don't do the sub/push optimization if the sub }
               { comes from setting up the stack frame (JM)    }
               (not(GetLastInstruction(p,hp1)) or
               not(MatchInstruction(hp1,A_MOV,[S_L]) and
                 MatchOperand(taicpu(hp1).oper[0]^,NR_ESP) and
                 MatchOperand(taicpu(hp1).oper[0]^,NR_EBP))) then
              begin
                hp1 := tai(p.next);
                while Assigned(hp1) and
                      (tai(hp1).typ in [ait_instruction]+SkipInstr) and
                      not RegReadByInstruction(NR_ESP,hp1) and
                      not RegModifiedByInstruction(NR_ESP,hp1) do
                  hp1 := tai(hp1.next);
                if Assigned(hp1) and
                  MatchInstruction(hp1,A_PUSH,[S_W]) then
                  begin
                    taicpu(hp1).changeopsize(S_L);
                    if taicpu(hp1).oper[0]^.typ=top_reg then
                      setsubreg(taicpu(hp1).oper[0]^.reg,R_SUBWHOLE);
                    hp1 := tai(p.next);
                    RemoveCurrentp(p, hp1);
                    Result:=true;
                    exit;
                  end;
              end;
{$endif i386}
            if DoArithCombineOpt(p) then
              Result:=true;
          end;
      end;


    function TX86AsmOptimizer.OptPass1SHLSAL(var p : tai) : boolean;
      var
        TmpBool1,TmpBool2 : Boolean;
        tmpref : treference;
        hp1,hp2: tai;
        mask, shiftval:    tcgint;
      begin
        Result:=false;

        { All these optimisations work on "shl/sal const,%reg" }
        if not MatchOpType(taicpu(p),top_const,top_reg) then
          Exit;

        if (taicpu(p).opsize in [S_L{$ifdef x86_64},S_Q{$endif x86_64}]) and
           (taicpu(p).oper[0]^.val <= 3) then
          { Changes "shl const, %reg32; add const/reg, %reg32" to one lea statement }
          begin
            { should we check the next instruction? }
            TmpBool1 := True;
            { have we found an add/sub which could be
              integrated in the lea? }
            TmpBool2 := False;
            reference_reset(tmpref,2,[]);
            TmpRef.index := taicpu(p).oper[1]^.reg;
            TmpRef.scalefactor := 1 shl taicpu(p).oper[0]^.val;
            while TmpBool1 and
                  GetNextInstruction(p, hp1) and
                  (tai(hp1).typ = ait_instruction) and
                  ((((taicpu(hp1).opcode = A_ADD) or
                     (taicpu(hp1).opcode = A_SUB)) and
                    (taicpu(hp1).oper[1]^.typ = Top_Reg) and
                    (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg)) or
                   (((taicpu(hp1).opcode = A_INC) or
                     (taicpu(hp1).opcode = A_DEC)) and
                    (taicpu(hp1).oper[0]^.typ = Top_Reg) and
                    (taicpu(hp1).oper[0]^.reg = taicpu(p).oper[1]^.reg)) or
                    ((taicpu(hp1).opcode = A_LEA) and
                    (taicpu(hp1).oper[0]^.ref^.index = taicpu(p).oper[1]^.reg) and
                    (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg))) and
                  (not GetNextInstruction(hp1,hp2) or
                   not instrReadsFlags(hp2)) Do
              begin
                TmpBool1 := False;
                if taicpu(hp1).opcode=A_LEA then
                  begin
                    if (TmpRef.base = NR_NO) and
                       (taicpu(hp1).oper[0]^.ref^.symbol=nil) and
                       (taicpu(hp1).oper[0]^.ref^.relsymbol=nil) and
                       { Segment register isn't a concern here }
                       ((taicpu(hp1).oper[0]^.ref^.scalefactor=0) or
                       (taicpu(hp1).oper[0]^.ref^.scalefactor*tmpref.scalefactor<=8)) then
                      begin
                        TmpBool1 := True;
                        TmpBool2 := True;
                        inc(TmpRef.offset, taicpu(hp1).oper[0]^.ref^.offset);
                        if taicpu(hp1).oper[0]^.ref^.scalefactor<>0 then
                          tmpref.scalefactor:=tmpref.scalefactor*taicpu(hp1).oper[0]^.ref^.scalefactor;
                        TmpRef.base := taicpu(hp1).oper[0]^.ref^.base;
                        RemoveInstruction(hp1);
                      end
                  end
                else if (taicpu(hp1).oper[0]^.typ = Top_Const) then
                  begin
                    TmpBool1 := True;
                    TmpBool2 := True;
                    case taicpu(hp1).opcode of
                      A_ADD:
                        inc(TmpRef.offset, longint(taicpu(hp1).oper[0]^.val));
                      A_SUB:
                        dec(TmpRef.offset, longint(taicpu(hp1).oper[0]^.val));
                      else
                        internalerror(2019050536);
                    end;
                    RemoveInstruction(hp1);
                  end
                else
                  if (taicpu(hp1).oper[0]^.typ = Top_Reg) and
                     (((taicpu(hp1).opcode = A_ADD) and
                       (TmpRef.base = NR_NO)) or
                      (taicpu(hp1).opcode = A_INC) or
                      (taicpu(hp1).opcode = A_DEC)) then
                    begin
                      TmpBool1 := True;
                      TmpBool2 := True;
                      case taicpu(hp1).opcode of
                        A_ADD:
                          TmpRef.base := taicpu(hp1).oper[0]^.reg;
                        A_INC:
                          inc(TmpRef.offset);
                        A_DEC:
                          dec(TmpRef.offset);
                        else
                          internalerror(2019050535);
                      end;
                      RemoveInstruction(hp1);
                    end;
              end;
            if TmpBool2
{$ifndef x86_64}
               or
               ((current_settings.optimizecputype < cpu_Pentium2) and
               (taicpu(p).oper[0]^.val <= 3) and
               not(cs_opt_size in current_settings.optimizerswitches))
{$endif x86_64}
              then
              begin
                if not(TmpBool2) and
                    (taicpu(p).oper[0]^.val=1) then
                  begin
                    taicpu(p).opcode := A_ADD;
                    taicpu(p).loadreg(0, taicpu(p).oper[1]^.reg);
                  end
                else
                  begin
                    taicpu(p).opcode := A_LEA;
                    taicpu(p).loadref(0, TmpRef);
                  end;
                DebugMsg(SPeepholeOptimization + 'ShlAddLeaSubIncDec2Lea',p);
                Result := True;
              end;
          end
{$ifndef x86_64}
        else if (current_settings.optimizecputype < cpu_Pentium2) then
          begin
            { changes "shl $1, %reg" to "add %reg, %reg", which is the same on a 386,
              but faster on a 486, and Tairable in both U and V pipes on the Pentium
              (unlike shl, which is only Tairable in the U pipe) }
            if taicpu(p).oper[0]^.val=1 then
                begin
                  taicpu(p).opcode := A_ADD;
                  taicpu(p).loadreg(0, taicpu(p).oper[1]^.reg);
                  Result := True;
                end
           { changes "shl $2, %reg" to "lea (,%reg,4), %reg"
             "shl $3, %reg" to "lea (,%reg,8), %reg }
           else if (taicpu(p).opsize = S_L) and
                   (taicpu(p).oper[0]^.val<= 3) then
             begin
               reference_reset(tmpref,2,[]);
               TmpRef.index := taicpu(p).oper[1]^.reg;
               TmpRef.scalefactor := 1 shl taicpu(p).oper[0]^.val;
               taicpu(p).opcode := A_LEA;
               taicpu(p).loadref(0, TmpRef);
               Result := True;
             end;
          end
{$endif x86_64}
        else if
          GetNextInstruction(p, hp1) and (hp1.typ = ait_instruction) and MatchOpType(taicpu(hp1), top_const, top_reg) and
          (
            (
              MatchInstruction(hp1, A_AND, [taicpu(p).opsize]) and
              SetAndTest(hp1, hp2)
{$ifdef x86_64}
            ) or
            (
              MatchInstruction(hp1, A_MOV, [taicpu(p).opsize]) and
              GetNextInstruction(hp1, hp2) and
              MatchInstruction(hp2, A_AND, [taicpu(p).opsize]) and
              MatchOpType(taicpu(hp2), top_reg, top_reg) and
              (taicpu(hp1).oper[1]^.reg = taicpu(hp2).oper[0]^.reg)
{$endif x86_64}
            )
          ) and
          (taicpu(p).oper[1]^.reg = taicpu(hp2).oper[1]^.reg) then
          begin
            { Change:
                shl x, %reg1
                mov -(1<<x), %reg2
                and %reg2, %reg1

              Or:
                shl x, %reg1
                and -(1<<x), %reg1

              To just:
                shl x, %reg1

              Since the and operation only zeroes bits that are already zero from the shl operation
            }
            case taicpu(p).oper[0]^.val of
               8:
                 mask:=$FFFFFFFFFFFFFF00;
               16:
                 mask:=$FFFFFFFFFFFF0000;
               32:
                 mask:=$FFFFFFFF00000000;
               63:
                 { Constant pre-calculated to prevent overflow errors with Int64 }
                 mask:=$8000000000000000;
               else
                 begin
                   if taicpu(p).oper[0]^.val >= 64 then
                     { Shouldn't happen realistically, since the register
                       is guaranteed to be set to zero at this point }
                     mask := 0
                   else
                     mask := -(Int64(1 shl taicpu(p).oper[0]^.val));
                 end;
            end;

            if taicpu(hp1).oper[0]^.val = mask then
              begin
                { Everything checks out, perform the optimisation, as long as
                  the FLAGS register isn't being used}
                TransferUsedRegs(TmpUsedRegs);
                UpdateUsedRegs(TmpUsedRegs, tai(p.next));

{$ifdef x86_64}
                if (hp1 <> hp2) then
                  begin
                    { "shl/mov/and" version }
                    UpdateUsedRegs(TmpUsedRegs, tai(hp1.next));

                    { Don't do the optimisation if the FLAGS register is in use }
                    if not(RegUsedAfterInstruction(NR_DEFAULTFLAGS, hp2, TmpUsedRegs)) then
                      begin
                        DebugMsg(SPeepholeOptimization + 'ShlMovAnd2Shl', p);
                        { Don't remove the 'mov' instruction if its register is used elsewhere }
                        if not(RegUsedAfterInstruction(taicpu(hp1).oper[1]^.reg, hp2, TmpUsedRegs)) then
                          begin
                            RemoveInstruction(hp1);
                            Result := True;
                          end;

                        { Only set Result to True if the 'mov' instruction was removed }
                        RemoveInstruction(hp2);
                      end;
                  end
                else
{$endif x86_64}
                  begin
                    { "shl/and" version }

                    { Don't do the optimisation if the FLAGS register is in use }
                    if not(RegUsedAfterInstruction(NR_DEFAULTFLAGS, hp1, TmpUsedRegs)) then
                      begin
                        DebugMsg(SPeepholeOptimization + 'ShlAnd2Shl', p);
                        RemoveInstruction(hp1);
                        Result := True;
                      end;
                  end;

                Exit;
              end
            else {$ifdef x86_64}if (hp1 = hp2) then{$endif x86_64}
              begin
                { Even if the mask doesn't allow for its removal, we might be
                  able to optimise the mask for the "shl/and" version, which
                  may permit other peephole optimisations }
{$ifdef DEBUG_AOPTCPU}
                mask := taicpu(hp1).oper[0]^.val and mask;
                if taicpu(hp1).oper[0]^.val <> mask then
                  begin
                    DebugMsg(
                      SPeepholeOptimization +
                      'Changed mask from $' + debug_tostr(taicpu(hp1).oper[0]^.val) +
                      ' to $' + debug_tostr(mask) +
                      'based on previous instruction (ShlAnd2ShlAnd)', hp1);
                    taicpu(hp1).oper[0]^.val := mask;
                  end;
{$else DEBUG_AOPTCPU}
                { If debugging is off, just set the operand even if it's the same }
                taicpu(hp1).oper[0]^.val := taicpu(hp1).oper[0]^.val and mask;
{$endif DEBUG_AOPTCPU}
              end;
          end;
        {
           change
           shl/sal const,reg
           <op> ...(...,reg,1),...

           into

           <op> ...(...,reg,1 shl const),...

           if const in 1..3
        }

        if MatchOpType(taicpu(p), top_const, top_reg) and
          (taicpu(p).oper[0]^.val in [1..3]) and
          GetNextInstructionUsingReg(p,hp1,taicpu(p).oper[1]^.reg) and
          ((MatchInstruction(hp1,A_MOV,A_LEA,[]) and
           MatchOpType(taicpu(hp1),top_ref,top_reg)) or
           (MatchInstruction(hp1,A_FST,A_FSTP,A_FLD,[]) and
           MatchOpType(taicpu(hp1),top_ref))
          ) and
          (taicpu(p).oper[1]^.reg=taicpu(hp1).oper[0]^.ref^.index) and
          (taicpu(p).oper[1]^.reg<>taicpu(hp1).oper[0]^.ref^.base) and
          (taicpu(hp1).oper[0]^.ref^.scalefactor in [0,1]) then
          begin
            TransferUsedRegs(TmpUsedRegs);
            UpdateUsedRegs(TmpUsedRegs, tai(p.next));
            if not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg, hp1, TmpUsedRegs)) then
              begin
                taicpu(hp1).oper[0]^.ref^.scalefactor:=1 shl taicpu(p).oper[0]^.val;
                DebugMsg(SPeepholeOptimization + 'ShlOp2Op', p);
                RemoveCurrentP(p);
                Result:=true;
                exit;
              end;
          end;
        if MatchOpType(taicpu(p), top_const, top_reg) and
          GetNextInstructionUsingReg(p,hp1,taicpu(p).oper[1]^.reg) and
          MatchInstruction(hp1,A_SHL,[taicpu(p).opsize]) and
          MatchOpType(taicpu(hp1),top_const,top_reg) and
          (taicpu(p).oper[1]^.reg=taicpu(hp1).oper[1]^.reg) then
          begin
            shiftval:=taicpu(p).oper[0]^.val+taicpu(hp1).oper[0]^.val;
            if ((taicpu(p).opsize=S_B) and (shiftval>7)) or
              ((taicpu(p).opsize=S_W) and (shiftval>15)) or
{$ifdef x86_64}
              ((taicpu(p).opsize=S_Q) and (shiftval>63)) or
{$endif x86_64}
              ((taicpu(p).opsize=S_L) and (shiftval>31)) then
              begin
                DebugMsg(SPeepholeOptimization + 'ShlShl2Mov', p);
                taicpu(hp1).opcode:=A_MOV;
                taicpu(hp1).oper[0]^.val:=0;
              end
            else
              begin
                DebugMsg(SPeepholeOptimization + 'ShlShl2Shl', p);
                taicpu(hp1).oper[0]^.val:=shiftval;
              end;
            RemoveCurrentP(p);
            Result:=true;
            exit;
          end;
      end;


    class function TX86AsmOptimizer.IsShrMovZFoldable(shr_size, movz_size: topsize; Shift: TCGInt): Boolean;
      begin
        case shr_size of
          S_B:
            { No valid combinations }
            Result := False;

          S_W:
            Result := (Shift >= 8) and (movz_size = S_BW);

          S_L:
            Result :=
              (Shift >= 24) { Any opsize is valid for this shift } or
              ((Shift >= 16) and (movz_size = S_WL));
{$ifdef x86_64}
          S_Q:
            Result :=
              (Shift >= 56) { Any opsize is valid for this shift } or
              ((Shift >= 48) and (movz_size = S_WL));
{$endif x86_64}
          else
            InternalError(2022081510);
        end;
      end;

    function TX86AsmOptimizer.OptPass1SHR(var p : tai) : boolean;
      var
        hp1, hp2: tai;
        Shift: TCGInt;
        LimitSize: Topsize;
        DoNotMerge: Boolean;
      begin
        Result := False;

        { All these optimisations work on "shr const,%reg" }
        if not MatchOpType(taicpu(p), top_const, top_reg) then
          Exit;

        DoNotMerge := False;
        Shift := taicpu(p).oper[0]^.val;
        LimitSize := taicpu(p).opsize;

        hp1 := p;
        repeat
          if not GetNextInstructionUsingReg(hp1, hp1, taicpu(p).oper[1]^.reg) or (hp1.typ <> ait_instruction) then
            Exit;

          case taicpu(hp1).opcode of
            A_TEST, A_CMP, A_Jcc:
              { Skip over conditional jumps and relevant comparisons }
              Continue;

            A_MOVZX:
              if MatchOpType(taicpu(hp1), top_reg, top_reg) and
                SuperRegistersEqual(taicpu(hp1).oper[0]^.reg, taicpu(p).oper[1]^.reg) then
                begin
                  { Since the original register is being read as is, subsequent
                    SHRs must not be merged at this point }
                  DoNotMerge := True;

                  if IsShrMovZFoldable(taicpu(p).opsize, taicpu(hp1).opsize, Shift) then
                    begin
                      if not SuperRegistersEqual(taicpu(hp1).oper[0]^.reg, taicpu(hp1).oper[1]^.reg) then { Different register target }
                        begin
                          DebugMsg(SPeepholeOptimization + 'Converted MOVZX instruction to MOV since previous SHR makes zero-extension unnecessary (ShrMovz2ShrMov 1)', hp1);
                          taicpu(hp1).opcode := A_MOV;
                          setsubreg(taicpu(hp1).oper[0]^.reg, getsubreg(taicpu(hp1).oper[1]^.reg));
                          case taicpu(hp1).opsize of
                            S_BW:
                              taicpu(hp1).opsize := S_W;
                            S_BL, S_WL:
                              taicpu(hp1).opsize := S_L;
                            else
                              InternalError(2022081503);
                          end;

                          { p itself hasn't changed, so no need to set Result to True }
                          Include(OptsToCheck, aoc_ForceNewIteration);

                          { See if there's anything afterwards that can be
                            optimised, since the input register hasn't changed }
                          Continue;
                        end;

                      { NOTE: If the MOVZX instruction reads and writes the same
                        register, defer this to the post-peephole optimisation stage }
                      Exit;
                    end;
                end;
            A_SHL, A_SAL, A_SHR:
              if (taicpu(hp1).opsize <= LimitSize) and
                MatchOpType(taicpu(hp1), top_const, top_reg) and
                SuperRegistersEqual(taicpu(hp1).oper[1]^.reg, taicpu(p).oper[1]^.reg) then
                begin
                  { Make sure the sizes don't exceed the register size limit
                    (measured by the shift value falling below the limit) }

                  if taicpu(hp1).opsize < LimitSize then
                    LimitSize := taicpu(hp1).opsize;

                  if taicpu(hp1).opcode = A_SHR then
                    Inc(Shift, taicpu(hp1).oper[0]^.val)
                  else
                    begin
                      Dec(Shift, taicpu(hp1).oper[0]^.val);
                      DoNotMerge := True;
                    end;

                  if Shift < topsize2memsize[taicpu(p).opsize] - topsize2memsize[LimitSize] then
                    Exit;

                  { Since we've established that the combined shift is within
                    limits, we can actually combine the adjacent SHR
                    instructions even if they're different sizes }
                  if not DoNotMerge and (taicpu(hp1).opcode = A_SHR) then
                    begin
                      hp2 := tai(hp1.Previous);
                      DebugMsg(SPeepholeOptimization + 'ShrShr2Shr 1', p);
                      Inc(taicpu(p).oper[0]^.val, taicpu(hp1).oper[0]^.val);
                      RemoveInstruction(hp1);
                      hp1 := hp2;

                      { Though p has changed, only the constant has, and its
                        effects can still be detected on the next iteration of
                        the repeat..until loop }
                      Include(OptsToCheck, aoc_ForceNewIteration);
                    end;

                  { Move onto the next instruction }
                  Continue;
                end;
            else
              ;
          end;

          Break;
        until False;
      end;


    function TX86AsmOptimizer.CheckMemoryWrite(var first_mov, second_mov: taicpu): Boolean;
      var
        CurrentRef: TReference;
        FullReg: TRegister;
        hp1, hp2: tai;
      begin
        Result := False;
        if (first_mov.opsize <> S_B) or (second_mov.opsize <> S_B) then
          Exit;

        { We assume you've checked if the operand is actually a reference by
          this point. If it isn't, you'll most likely get an access violation }
        CurrentRef := first_mov.oper[1]^.ref^;

        { Memory must be aligned }
        if (CurrentRef.offset mod 4) <> 0 then
          Exit;

        Inc(CurrentRef.offset);
        CurrentRef.alignment := 1; { Otherwise references_equal will return False }

        if MatchOperand(second_mov.oper[0]^, 0) and
          references_equal(second_mov.oper[1]^.ref^, CurrentRef) and
          GetNextInstruction(second_mov, hp1) and
          (hp1.typ = ait_instruction) and
          (taicpu(hp1).opcode = A_MOV) and
          MatchOpType(taicpu(hp1), top_const, top_ref) and
          (taicpu(hp1).oper[0]^.val = 0) then
          begin
            Inc(CurrentRef.offset);
            CurrentRef.alignment := taicpu(hp1).oper[1]^.ref^.alignment; { Otherwise references_equal might return False }

            FullReg := newreg(R_INTREGISTER,getsupreg(first_mov.oper[0]^.reg), R_SUBD);

            if references_equal(taicpu(hp1).oper[1]^.ref^, CurrentRef) then
              begin
                case taicpu(hp1).opsize of
                  S_B:
                    if GetNextInstruction(hp1, hp2) and
                      MatchInstruction(taicpu(hp2), A_MOV, [S_B]) and
                      MatchOpType(taicpu(hp2), top_const, top_ref) and
                      (taicpu(hp2).oper[0]^.val = 0) then
                      begin
                        Inc(CurrentRef.offset);
                        CurrentRef.alignment := 1; { Otherwise references_equal will return False }

                        if references_equal(taicpu(hp2).oper[1]^.ref^, CurrentRef) and
                          (taicpu(hp2).opsize = S_B) then
                          begin
                            RemoveInstruction(hp1);
                            RemoveInstruction(hp2);

                            first_mov.opsize := S_L;

                            if first_mov.oper[0]^.typ = top_reg then
                              begin
                                DebugMsg(SPeepholeOptimization + 'MOVb/MOVb/MOVb/MOVb -> MOVZX/MOVl', first_mov);

                                { Reuse second_mov as a MOVZX instruction }
                                second_mov.opcode := A_MOVZX;
                                second_mov.opsize := S_BL;
                                second_mov.loadreg(0, first_mov.oper[0]^.reg);
                                second_mov.loadreg(1, FullReg);

                                first_mov.oper[0]^.reg := FullReg;

                                asml.Remove(second_mov);
                                asml.InsertBefore(second_mov, first_mov);
                              end
                            else
                              { It's a value }
                              begin
                                DebugMsg(SPeepholeOptimization + 'MOVb/MOVb/MOVb/MOVb -> MOVl', first_mov);
                                RemoveInstruction(second_mov);
                              end;

                            Result := True;
                            Exit;
                          end;
                      end;
                  S_W:
                    begin
                      RemoveInstruction(hp1);

                      first_mov.opsize := S_L;

                      if first_mov.oper[0]^.typ = top_reg then
                        begin
                          DebugMsg(SPeepholeOptimization + 'MOVb/MOVb/MOVw -> MOVZX/MOVl', first_mov);

                          { Reuse second_mov as a MOVZX instruction }
                          second_mov.opcode := A_MOVZX;
                          second_mov.opsize := S_BL;
                          second_mov.loadreg(0, first_mov.oper[0]^.reg);
                          second_mov.loadreg(1, FullReg);

                          first_mov.oper[0]^.reg := FullReg;

                          asml.Remove(second_mov);
                          asml.InsertBefore(second_mov, first_mov);
                        end
                      else
                        { It's a value }
                        begin
                          DebugMsg(SPeepholeOptimization + 'MOVb/MOVb/MOVw -> MOVl', first_mov);
                          RemoveInstruction(second_mov);
                        end;

                      Result := True;
                      Exit;
                    end;
                  else
                    ;
                end;
              end;
          end;
      end;


    function TX86AsmOptimizer.OptPass1FSTP(var p: tai): boolean;
      { returns true if a "continue" should be done after this optimization }
      var
        hp1, hp2, hp3: tai;
      begin
        Result := false;
        hp3 := nil;
        if MatchOpType(taicpu(p),top_ref) and
           GetNextInstruction(p, hp1) and
           (hp1.typ = ait_instruction) and
           (((taicpu(hp1).opcode = A_FLD) and
             (taicpu(p).opcode = A_FSTP)) or
            ((taicpu(p).opcode = A_FISTP) and
             (taicpu(hp1).opcode = A_FILD))) and
           MatchOpType(taicpu(hp1),top_ref) and
           (taicpu(hp1).opsize = taicpu(p).opsize) and
           RefsEqual(taicpu(p).oper[0]^.ref^, taicpu(hp1).oper[0]^.ref^) then
          begin
            { replacing fstp f;fld f by fst f is only valid for extended because of rounding or if fastmath is on }
            if ((taicpu(p).opsize=S_FX) or (cs_opt_fastmath in current_settings.optimizerswitches)) and
               GetNextInstruction(hp1, hp2) and
               (((hp2.typ = ait_instruction) and
               IsExitCode(hp2) and
               (taicpu(p).oper[0]^.ref^.base = current_procinfo.FramePointer) and
               not(assigned(current_procinfo.procdef.funcretsym) and
                   (taicpu(p).oper[0]^.ref^.offset < tabstractnormalvarsym(current_procinfo.procdef.funcretsym).localloc.reference.offset)) and
               (taicpu(p).oper[0]^.ref^.index = NR_NO)) or
               { fstp <temp>
                 fld  <temp>
                 <dealloc> <temp>
               }
               ((taicpu(p).oper[0]^.ref^.base = current_procinfo.FramePointer) and
                (taicpu(p).oper[0]^.ref^.index = NR_NO) and
                SetAndTest(FindTempDeAlloc(taicpu(p).oper[0]^.ref^.offset,tai(hp1.next)),hp2) and
                (tai_tempalloc(hp2).temppos=taicpu(p).oper[0]^.ref^.offset) and
                (((taicpu(p).opsize=S_FX) and (tai_tempalloc(hp2).tempsize=16)) or
                 ((taicpu(p).opsize in [S_IQ,S_FL]) and (tai_tempalloc(hp2).tempsize=8)) or
                 ((taicpu(p).opsize=S_FS) and (tai_tempalloc(hp2).tempsize=4))
                )
               )
               ) then
              begin
                DebugMsg(SPeepholeOptimization + 'FstpFld2<Nop>',p);
                RemoveInstruction(hp1);
                RemoveCurrentP(p, hp2);
                { first case: exit code }
                if hp2.typ = ait_instruction then
                  RemoveLastDeallocForFuncRes(p);
                Result := true;
              end
            else
              { we can do this only in fast math mode as fstp is rounding ...
                ... still disabled as it breaks the compiler and/or rtl }
              if { (cs_opt_fastmath in current_settings.optimizerswitches) or }
                { ... or if another fstp equal to the first one follows }
                GetNextInstruction(hp1,hp2) and
                (hp2.typ = ait_instruction) and
                (taicpu(p).opcode=taicpu(hp2).opcode) and
                (taicpu(p).opsize=taicpu(hp2).opsize) then
                begin
                  if (taicpu(p).oper[0]^.ref^.base = current_procinfo.FramePointer) and
                    (taicpu(p).oper[0]^.ref^.index = NR_NO) and
                    SetAndTest(FindTempDeAlloc(taicpu(p).oper[0]^.ref^.offset,tai(hp2.next)),hp3) and
                    MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[0]^) and
                    (tai_tempalloc(hp3).temppos=taicpu(p).oper[0]^.ref^.offset) and
                    (((taicpu(p).opsize=S_FX) and (tai_tempalloc(hp3).tempsize=16)) or
                     ((taicpu(p).opsize in [S_IQ,S_FL]) and (tai_tempalloc(hp3).tempsize=8)) or
                     ((taicpu(p).opsize=S_FS) and (tai_tempalloc(hp3).tempsize=4))
                    ) then
                    begin
                      DebugMsg(SPeepholeOptimization + 'FstpFldFstp2Fstp',p);
                      RemoveCurrentP(p,hp2);
                      RemoveInstruction(hp1);
                      Result := true;
                    end
                  else if { fst can't store an extended/comp value }
                    (taicpu(p).opsize <> S_FX) and
                    (taicpu(p).opsize <> S_IQ) then
                    begin
                      if (taicpu(p).opcode = A_FSTP) then
                        taicpu(p).opcode := A_FST
                      else
                        taicpu(p).opcode := A_FIST;
                      DebugMsg(SPeepholeOptimization + 'FstpFld2Fst',p);
                      RemoveInstruction(hp1);
                      Result := true;
                    end;
                end;
          end;
      end;


     function TX86AsmOptimizer.OptPass1FLD(var p : tai) : boolean;
      var
       hp1, hp2, hp3: tai;
      begin
        result:=false;
        if MatchOpType(taicpu(p),top_reg) and
           GetNextInstruction(p, hp1) and
           (hp1.typ = Ait_Instruction) and
           MatchOpType(taicpu(hp1),top_reg,top_reg) and
           (taicpu(hp1).oper[0]^.reg = NR_ST) and
           (taicpu(hp1).oper[1]^.reg = NR_ST1) then
           { change                        to
               fld      reg               fxxx reg,st
               fxxxp    st, st1 (hp1)
             Remark: non commutative operations must be reversed!
           }
          begin
              case taicpu(hp1).opcode Of
                A_FMULP,A_FADDP,
                A_FSUBP,A_FDIVP,A_FSUBRP,A_FDIVRP:
                  begin
                    case taicpu(hp1).opcode Of
                      A_FADDP: taicpu(hp1).opcode := A_FADD;
                      A_FMULP: taicpu(hp1).opcode := A_FMUL;
                      A_FSUBP: taicpu(hp1).opcode := A_FSUBR;
                      A_FSUBRP: taicpu(hp1).opcode := A_FSUB;
                      A_FDIVP: taicpu(hp1).opcode := A_FDIVR;
                      A_FDIVRP: taicpu(hp1).opcode := A_FDIV;
                      else
                        internalerror(2019050534);
                    end;
                    taicpu(hp1).oper[0]^.reg := taicpu(p).oper[0]^.reg;
                    taicpu(hp1).oper[1]^.reg := NR_ST;
                    DebugMsg(SPeepholeOptimization + 'FldF*p2F*',hp1);
                    RemoveCurrentP(p, hp1);
                    Result:=true;
                    exit;
                  end;
                else
                  ;
              end;
          end
        else
          if MatchOpType(taicpu(p),top_ref) and
             GetNextInstruction(p, hp2) and
             (hp2.typ = Ait_Instruction) and
             MatchOpType(taicpu(hp2),top_reg,top_reg) and
             (taicpu(p).opsize in [S_FS, S_FL]) and
             (taicpu(hp2).oper[0]^.reg = NR_ST) and
             (taicpu(hp2).oper[1]^.reg = NR_ST1) then
            if GetLastInstruction(p, hp1) and
              MatchInstruction(hp1,A_FLD,A_FST,[taicpu(p).opsize]) and
              MatchOpType(taicpu(hp1),top_ref) and
              RefsEqual(taicpu(p).oper[0]^.ref^, taicpu(hp1).oper[0]^.ref^) then
              if ((taicpu(hp2).opcode = A_FMULP) or
                  (taicpu(hp2).opcode = A_FADDP)) then
              { change                      to
                  fld/fst   mem1  (hp1)       fld/fst   mem1
                  fld       mem1  (p)         fadd/
                  faddp/                       fmul     st, st
                  fmulp  st, st1 (hp2) }
                begin
                  DebugMsg(SPeepholeOptimization + 'Fld/FstFldFaddp/Fmulp2Fld/FstFadd/Fmul',hp1);
                  RemoveCurrentP(p, hp1);
                  if (taicpu(hp2).opcode = A_FADDP) then
                    taicpu(hp2).opcode := A_FADD
                  else
                    taicpu(hp2).opcode := A_FMUL;
                  taicpu(hp2).oper[1]^.reg := NR_ST;
                end
              else
                { change              to
                    fld/fst mem1 (hp1)   fld/fst mem1
                    fld     mem1 (p)     fld      st
                }
                begin
                  DebugMsg(SPeepholeOptimization + 'Fld/Fst<mem>Fld<mem>2Fld/Fst<mem>Fld<reg>',hp1);
                  taicpu(p).changeopsize(S_FL);
                  taicpu(p).loadreg(0,NR_ST);
                end
            else
              begin
                case taicpu(hp2).opcode Of
                  A_FMULP,A_FADDP,A_FSUBP,A_FDIVP,A_FSUBRP,A_FDIVRP:
                  { change                        to
                      fld/fst  mem1    (hp1)      fld/fst    mem1
                      fld      mem2    (p)        fxxx       mem2
                      fxxxp    st, st1 (hp2)                      }
                    begin
                      case taicpu(hp2).opcode Of
                        A_FADDP: taicpu(p).opcode := A_FADD;
                        A_FMULP: taicpu(p).opcode := A_FMUL;
                        A_FSUBP: taicpu(p).opcode := A_FSUBR;
                        A_FSUBRP: taicpu(p).opcode := A_FSUB;
                        A_FDIVP: taicpu(p).opcode := A_FDIVR;
                        A_FDIVRP: taicpu(p).opcode := A_FDIV;
                        else
                          internalerror(2019050533);
                      end;
                      DebugMsg(SPeepholeOptimization + 'Fld/FstFldF*2Fld/FstF*',p);
                      RemoveInstruction(hp2);
                    end
                  else
                    ;
                end
              end
      end;


     function IsCmpSubset(cond1, cond2: TAsmCond): Boolean; inline;
       begin
         Result := condition_in(cond1, cond2) or
           { Not strictly subsets due to the actual flags checked, but because we're
             comparing integers, E is a subset of AE and GE and their aliases }
           ((cond1 in [C_E, C_Z]) and (cond2 in [C_AE, C_NB, C_NC, C_GE, C_NL]));
       end;


     function TX86AsmOptimizer.OptPass1Cmp(var p: tai): boolean;
       var
         v: TCGInt;
         hp1, hp2, p_dist, p_jump, hp1_dist, p_label, hp1_label: tai;
         FirstMatch, TempBool: Boolean;
         NewReg: TRegister;
         JumpLabel, JumpLabel_dist, JumpLabel_far: TAsmLabel;
       begin
         Result:=false;

         { All these optimisations need a next instruction }
         if not GetNextInstruction(p, hp1) then
           Exit;

         { Search for:
             cmp   ###,###
             j(c1) @lbl1
             ...
           @lbl:
             cmp   ###,### (same comparison as above)
             j(c2) @lbl2

           If c1 is a subset of c2, change to:
             cmp   ###,###
             j(c1) @lbl2
             (@lbl1 may become a dead label as a result)
         }

         { Also handle cases where there are multiple jumps in a row }
         p_jump := hp1;
         while Assigned(p_jump) and MatchInstruction(p_jump, A_JCC, []) do
           begin
             if IsJumpToLabel(taicpu(p_jump)) then
               begin
                 { Do jump optimisations first in case the condition becomes
                   unnecessary }

                 TempBool := True;
                 if DoJumpOptimizations(p_jump, TempBool) or
                   not TempBool then
                   begin

                     if Assigned(p_jump) then
                       begin
                         hp1 := p_jump;
                         if (p_jump.typ in [ait_align]) then
                           SkipAligns(p_jump, p_jump);

                         { CollapseZeroDistJump will be set to the label after the
                           jump if it optimises, whether or not it's live or dead }
                         if (p_jump.typ in [ait_label]) and
                           not (tai_label(p_jump).labsym.is_used) then
                           GetNextInstruction(p_jump, p_jump);
                       end;

                     TransferUsedRegs(TmpUsedRegs);
                     UpdateUsedRegs(TmpUsedRegs, tai(p.Next));

                     if not Assigned(p_jump) or
                       (
                         not MatchInstruction(p_jump, A_Jcc, A_SETcc, A_CMOVcc, []) and
                         not RegUsedAfterInstruction(NR_DEFAULTFLAGS, p_jump, TmpUsedRegs)
                       ) then
                       begin
                         { No more conditional jumps; conditional statement is no longer required }
                         DebugMsg(SPeepholeOptimization + 'Removed unnecessary condition (Cmp2Nop)', p);
                         RemoveCurrentP(p);
                         Result := True;
                         Exit;
                       end;

                     hp1 := p_jump;
                     Include(OptsToCheck, aoc_ForceNewIteration);
                     Continue;
                   end;

                 JumpLabel := TAsmLabel(taicpu(p_jump).oper[0]^.ref^.symbol);
                 if GetNextInstruction(p_jump, hp2) and
                   (
                     OptimizeConditionalJump(JumpLabel, p_jump, hp2, TempBool) or
                     not TempBool
                   ) then
                   begin
                     hp1 := p_jump;
                     Include(OptsToCheck, aoc_ForceNewIteration);
                     Continue;
                   end;

                 p_label := nil;
                 if Assigned(JumpLabel) then
                   p_label := getlabelwithsym(JumpLabel);

                 if Assigned(p_label) and
                   GetNextInstruction(p_label, p_dist) and
                   MatchInstruction(p_dist, A_CMP, []) and
                   MatchOperand(taicpu(p_dist).oper[0]^, taicpu(p).oper[0]^) and
                   MatchOperand(taicpu(p_dist).oper[1]^, taicpu(p).oper[1]^) and
                   GetNextInstruction(p_dist, hp1_dist) and
                   MatchInstruction(hp1_dist, A_JCC, []) then { This doesn't have to be an explicit label }
                   begin
                     JumpLabel_dist := TAsmLabel(taicpu(hp1_dist).oper[0]^.ref^.symbol);

                     if JumpLabel = JumpLabel_dist then
                       { This is an infinite loop }
                       Exit;

                     { Best optimisation when the first condition is a subset (or equal) of the second }
                     if IsCmpSubset(taicpu(p_jump).condition, taicpu(hp1_dist).condition) then
                       begin
                         { Any registers used here will already be allocated }
                         if Assigned(JumpLabel) then
                           JumpLabel.DecRefs;

                         DebugMsg(SPeepholeOptimization + 'CMP/Jcc/@Lbl/CMP/Jcc -> CMP/Jcc, redirecting first jump', p_jump);
                         taicpu(p_jump).loadref(0, taicpu(hp1_dist).oper[0]^.ref^); { This also increases the reference count }
                         Result := True;
                         { Don't exit yet.  Since p and p_jump haven't actually been
                           removed, we can check for more on this iteration }
                       end
                     else if IsCmpSubset(taicpu(hp1_dist).condition, inverse_cond(taicpu(p_jump).condition)) and
                       GetNextInstruction(hp1_dist, hp1_label) and
                       SkipAligns(hp1_label, hp1_label) and
                       (hp1_label.typ = ait_label) then
                       begin
                         JumpLabel_far := tai_label(hp1_label).labsym;

                         if (JumpLabel_far = JumpLabel_dist) or (JumpLabel_far = JumpLabel) then
                           { This is an infinite loop }
                           Exit;

                         if Assigned(JumpLabel_far) then
                           begin
                             { In this situation, if the first jump branches, the second one will never,
                               branch so change the destination label to after the second jump }

                             DebugMsg(SPeepholeOptimization + 'CMP/Jcc/@Lbl/CMP/Jcc/@Lbl -> CMP/Jcc, redirecting first jump to 2nd label', p_jump);

                             if Assigned(JumpLabel) then
                               JumpLabel.DecRefs;

                             JumpLabel_far.IncRefs;

                             taicpu(p_jump).oper[0]^.ref^.symbol := JumpLabel_far;

                             Result := True;
                             { Don't exit yet.  Since p and p_jump haven't actually been
                               removed, we can check for more on this iteration }
                             Continue;
                           end;
                       end;

                   end;
               end;

             { Search for:
                 cmp   ###,###
                 j(c1) @lbl1
                 cmp   ###,### (same as first)

               Remove second cmp
             }
             if GetNextInstruction(p_jump, hp2) and
               (
                 (
                   MatchInstruction(hp2, A_CMP, [taicpu(p).opsize]) and
                   (
                     (
                       MatchOpType(taicpu(p), top_const, top_reg) and
                       MatchOpType(taicpu(hp2), top_const, top_reg) and
                       (taicpu(hp2).oper[0]^.val = taicpu(p).oper[0]^.val) and
                       Reg1WriteOverwritesReg2Entirely(taicpu(hp2).oper[1]^.reg, taicpu(p).oper[1]^.reg)
                     ) or (
                       MatchOperand(taicpu(hp2).oper[0]^, taicpu(p).oper[0]^) and
                       MatchOperand(taicpu(hp2).oper[1]^, taicpu(p).oper[1]^)
                     )
                   )
                 ) or (
                   { Also match cmp $0,%reg; jcc @lbl; test %reg,%reg }
                   MatchOperand(taicpu(p).oper[0]^, 0) and
                   (taicpu(p).oper[1]^.typ = top_reg) and
                   MatchInstruction(hp2, A_TEST, []) and
                   MatchOpType(taicpu(hp2), top_reg, top_reg) and
                   (taicpu(hp2).oper[0]^.reg = taicpu(hp2).oper[1]^.reg) and
                   Reg1WriteOverwritesReg2Entirely(taicpu(hp2).oper[1]^.reg, taicpu(p).oper[1]^.reg)
                 )
               ) then
               begin
                 DebugMsg(SPeepholeOptimization + 'CMP/Jcc/CMP; removed superfluous CMP', hp2);
                 RemoveInstruction(hp2);
                 Result := True;
                 { Continue the while loop in case "Jcc/CMP" follows the second CMP that was just removed }
               end;

             GetNextInstruction(p_jump, p_jump);
           end;

         if (
             { Don't call GetNextInstruction again if we already have it }
             (hp1 = p_jump) or
             GetNextInstruction(p, hp1)
           ) and
           MatchInstruction(hp1, A_Jcc, []) and
           IsJumpToLabel(taicpu(hp1)) and
           (taicpu(hp1).condition in [C_E, C_Z, C_NE, C_NZ]) and
           GetNextInstruction(hp1, hp2) then
           begin
             {
                 cmp       x, y    (or "cmp y, x")
                 je        @lbl
                 mov       x, y
               @lbl:
                 (x and y can be constants, registers or references)

               Change to:
                 mov       x, y    (x and y will always be equal in the end)
               @lbl:               (may beceome a dead label)


               Also:
                 cmp       x, y    (or "cmp y, x")
                 jne       @lbl
                 mov       x, y
               @lbl:
                 (x and y can be constants, registers or references)

               Change to:
                 Absolutely nothing! (Except @lbl if it's still live)
             }
             if MatchInstruction(hp2, A_MOV, [taicpu(p).opsize]) and
               (
                 (
                   MatchOperand(taicpu(p).oper[0]^, taicpu(hp2).oper[0]^) and
                   MatchOperand(taicpu(p).oper[1]^, taicpu(hp2).oper[1]^)
                 ) or (
                   MatchOperand(taicpu(p).oper[0]^, taicpu(hp2).oper[1]^) and
                   MatchOperand(taicpu(p).oper[1]^, taicpu(hp2).oper[0]^)
                 )
               ) and
               GetNextInstruction(hp2, hp1_label) and
               SkipAligns(hp1_label, hp1_label) and
               (hp1_label.typ = ait_label) and
               (tai_label(hp1_label).labsym = taicpu(hp1).oper[0]^.ref^.symbol) then
               begin
                 tai_label(hp1_label).labsym.DecRefs;
                 if (taicpu(hp1).condition in [C_NE, C_NZ]) then
                   begin
                     DebugMsg(SPeepholeOptimization + 'CMP/JNE/MOV/@Lbl -> NOP, since the MOV is only executed if the operands are equal (CmpJneMov2Nop)', p);
                     RemoveInstruction(hp2);
                     hp2 := hp1_label; { So RemoveCurrentp below can be set to something valid }
                   end
                 else
                   DebugMsg(SPeepholeOptimization + 'CMP/JE/MOV/@Lbl -> MOV, since the MOV is only executed if the operands aren''t equal (CmpJeMov2Mov)', p);

                 RemoveInstruction(hp1);
                 RemoveCurrentp(p, hp2);
                 Result := True;
                 Exit;
               end;

             {
               Try to optimise the following:
                 cmp       $x,###  ($x and $y can be registers or constants)
                 je        @lbl1   (only reference)
                 cmp       $y,###  (### are identical)
               @Lbl:
                 sete      %reg1

               Change to:
                 cmp       $x,###
                 sete      %reg2   (allocate new %reg2)
                 cmp       $y,###
                 sete      %reg1
                 orb       %reg2,%reg1
                 (dealloc %reg2)

               This adds an instruction (so don't perform under -Os), but it removes
               a conditional branch.
             }
             if not (cs_opt_size in current_settings.optimizerswitches) and
               MatchInstruction(hp2, A_CMP, A_TEST, [taicpu(p).opsize]) and
               MatchOperand(taicpu(p).oper[1]^, taicpu(hp2).oper[1]^) and
               { The first operand of CMP instructions can only be a register or
                 immediate anyway, so no need to check }
               GetNextInstruction(hp2, p_label) and
               (p_label.typ = ait_label) and
               (tai_label(p_label).labsym.getrefs = 1) and
               (JumpTargetOp(taicpu(hp1))^.ref^.symbol = tai_label(p_label).labsym) and
               GetNextInstruction(p_label, p_dist) and
               MatchInstruction(p_dist, A_SETcc, []) and
               (taicpu(p_dist).condition in [C_E, C_Z]) and
               (taicpu(p_dist).oper[0]^.typ = top_reg) then
               begin
                 TransferUsedRegs(TmpUsedRegs);
                 UpdateUsedRegs(TmpUsedRegs, tai(hp1.Next));
                 UpdateUsedRegs(TmpUsedRegs, tai(hp2.Next));
                 UpdateUsedRegs(TmpUsedRegs, tai(p_label.Next));
                 UpdateUsedRegs(TmpUsedRegs, tai(p_dist.Next));

                 if not RegInUsedRegs(NR_DEFAULTFLAGS, TmpUsedRegs) and
                   { Get the instruction after the SETcc instruction so we can
                     allocate a new register over the entire range }
                   GetNextInstruction(p_dist, hp1_dist) then
                   begin
                     { Register can appear in p if it's not used afterwards, so only
                       allocate between hp1 and hp1_dist }
                     NewReg := GetIntRegisterBetween(R_SUBL, TmpUsedRegs, hp1, hp1_dist);
                     if NewReg <> NR_NO then
                       begin
                         DebugMsg(SPeepholeOptimization + 'CMP/JE/CMP/@Lbl/SETE -> CMP/SETE/CMP/SETE/OR, removing conditional branch', p);

                         { Change the jump instruction into a SETcc instruction }
                         taicpu(hp1).opcode := A_SETcc;
                         taicpu(hp1).opsize := S_B;
                         taicpu(hp1).loadreg(0, NewReg);

                         { This is now a dead label }
                         tai_label(p_label).labsym.decrefs;

                         { Prefer adding before the next instruction so the FLAGS
                           register is deallicated first  }
                         AsmL.InsertBefore(
                           taicpu.op_reg_reg(A_OR, S_B, NewReg, taicpu(p_dist).oper[0]^.reg),
                           hp1_dist
                         );

                         Result := True;
                         { Don't exit yet, as p wasn't changed and hp1, while
                           modified, is still intact and might be optimised by the
                           SETcc optimisation below }
                       end;
                   end;
               end;
           end;

         if taicpu(p).oper[0]^.typ = top_const then
           begin

             if (taicpu(p).oper[0]^.val = 0) and
               (taicpu(p).oper[1]^.typ = top_reg) and
               MatchInstruction(hp1,A_Jcc,A_SETcc,[]) then
               begin
                 hp2 := p;
                 FirstMatch := True;
                 { When dealing with "cmp $0,%reg", only ZF and SF contain
                   anything meaningful once it's converted to "test %reg,%reg";
                   additionally, some jumps will always (or never) branch, so
                   evaluate every jump immediately following the
                   comparison, optimising the conditions if possible.
                   Similarly with SETcc... those that are always set to 0 or 1
                   are changed to MOV instructions }
                 while FirstMatch or { Saves calling GetNextInstruction unnecessarily }
                   (
                     GetNextInstruction(hp2, hp1) and
                     MatchInstruction(hp1,A_Jcc,A_SETcc,[])
                   ) do
                   begin
                     FirstMatch := False;
                     case taicpu(hp1).condition of
                       C_B, C_C, C_NAE, C_O:
                         { For B/NAE:
                             Will never branch since an unsigned integer can never be below zero
                           For C/O:
                             Result cannot overflow because 0 is being subtracted
                         }
                         begin
                           if taicpu(hp1).opcode = A_Jcc then
                             begin
                               DebugMsg(SPeepholeOptimization + 'Cmpcc2Testcc - condition B/C/NAE/O --> Never (jump removed)', hp1);
                               TAsmLabel(taicpu(hp1).oper[0]^.ref^.symbol).decrefs;
                               RemoveInstruction(hp1);
                               { Since hp1 was deleted, hp2 must not be updated }
                               Continue;
                             end
                           else
                             begin
                               DebugMsg(SPeepholeOptimization + 'Cmpcc2Testcc - condition B/C/NAE/O --> Never (set -> mov 0)', hp1);
                               { Convert "set(c) %reg" instruction to "movb 0,%reg" }
                               taicpu(hp1).opcode := A_MOV;
                               taicpu(hp1).ops := 2;
                               taicpu(hp1).condition := C_None;
                               taicpu(hp1).opsize := S_B;
                               taicpu(hp1).loadreg(1,taicpu(hp1).oper[0]^.reg);
                               taicpu(hp1).loadconst(0, 0);
                             end;
                         end;
                       C_BE, C_NA:
                         begin
                           { Will only branch if equal to zero }
                           DebugMsg(SPeepholeOptimization + 'Cmpcc2Testcc - condition BE/NA --> E', hp1);
                           taicpu(hp1).condition := C_E;
                         end;
                       C_A, C_NBE:
                         begin
                           { Will only branch if not equal to zero }
                           DebugMsg(SPeepholeOptimization + 'Cmpcc2Testcc - condition A/NBE --> NE', hp1);
                           taicpu(hp1).condition := C_NE;
                         end;
                       C_AE, C_NB, C_NC, C_NO:
                         begin
                           { Will always branch }
                           DebugMsg(SPeepholeOptimization + 'Cmpcc2Testcc - condition AE/NB/NC/NO --> Always', hp1);
                           if taicpu(hp1).opcode = A_Jcc then
                             begin
                               MakeUnconditional(taicpu(hp1));
                               { Any jumps/set that follow will now be dead code }
                               RemoveDeadCodeAfterJump(taicpu(hp1));
                               Break;
                             end
                           else
                             begin
                               { Convert "set(c) %reg" instruction to "movb 1,%reg" }
                               taicpu(hp1).opcode := A_MOV;
                               taicpu(hp1).ops := 2;
                               taicpu(hp1).condition := C_None;
                               taicpu(hp1).opsize := S_B;
                               taicpu(hp1).loadreg(1,taicpu(hp1).oper[0]^.reg);
                               taicpu(hp1).loadconst(0, 1);
                             end;
                         end;
                       C_None:
                         InternalError(2020012201);
                       C_P, C_PE, C_NP, C_PO:
                         { We can't handle parity checks and they should never be generated
                           after a general-purpose CMP (it's used in some floating-point
                           comparisons that don't use CMP) }
                         InternalError(2020012202);
                       else
                         { Zero/Equality, Sign, their complements and all of the
                           signed comparisons do not need to be converted };
                     end;
                     hp2 := hp1;
                   end;

                 { Convert the instruction to a TEST }

                 taicpu(p).opcode := A_TEST;
                 taicpu(p).loadreg(0,taicpu(p).oper[1]^.reg);
                 Result := True;
                 Exit;
               end
             else if (taicpu(p).oper[0]^.val = 1) and
               MatchInstruction(hp1,A_Jcc,A_SETcc,[]) and
               (taicpu(hp1).condition in [C_L, C_NL, C_NGE, C_GE]) then
               begin
                 { Convert;       To:
                     cmp $1,r/m     cmp $0,r/m
                     jl  @lbl       jle @lbl
                     (Also do inverted conditions)
                 }
                 DebugMsg(SPeepholeOptimization + 'Cmp1Jl2Cmp0Jle', p);
                 taicpu(p).oper[0]^.val := 0;
                 if taicpu(hp1).condition in [C_L, C_NGE] then
                   taicpu(hp1).condition := C_LE
                 else
                   taicpu(hp1).condition := C_NLE;

                 { If the instruction is now "cmp $0,%reg", convert it to a
                   TEST (and effectively do the work of the "cmp $0,%reg" in
                   the block above)
                 }
                 if (taicpu(p).oper[1]^.typ = top_reg) then
                   begin
                     taicpu(p).opcode := A_TEST;
                     taicpu(p).loadreg(0,taicpu(p).oper[1]^.reg);
                   end;

                 Result := True;
                 Exit;
               end
             else if (taicpu(p).oper[1]^.typ = top_reg)
{$ifdef x86_64}
               and (taicpu(p).opsize <> S_Q) { S_Q will never happen: cmp with 64 bit constants is not possible }
{$endif x86_64}
               then
               begin
                 { cmp register,$8000                neg register
                   je target                 -->     jo target

                   .... only if register is deallocated before jump.}
                 case Taicpu(p).opsize of
                   S_B: v:=$80;
                   S_W: v:=$8000;
                   S_L: v:=qword($80000000);
                   else
                     internalerror(2013112905);
                 end;

                 if (taicpu(p).oper[0]^.val=v) and
                    MatchInstruction(hp1,A_Jcc,A_SETcc,[]) and
                    (Taicpu(hp1).condition in [C_E,C_NE]) then
                   begin
                     TransferUsedRegs(TmpUsedRegs);
                     UpdateUsedRegs(TmpUsedRegs,tai(p.next));
                     if not(RegInUsedRegs(Taicpu(p).oper[1]^.reg, TmpUsedRegs)) then
                       begin
                         DebugMsg(SPeepholeOptimization + 'CmpJe2NegJo done',p);
                         Taicpu(p).opcode:=A_NEG;
                         Taicpu(p).loadoper(0,Taicpu(p).oper[1]^);
                         Taicpu(p).clearop(1);
                         Taicpu(p).ops:=1;
                         if Taicpu(hp1).condition=C_E then
                           Taicpu(hp1).condition:=C_O
                         else
                           Taicpu(hp1).condition:=C_NO;
                         Result:=true;
                         exit;
                       end;
                   end;
               end;
           end;

         if TrySwapMovCmp(p, hp1) then
           begin
             Result := True;
             Exit;
           end;
     end;


   function TX86AsmOptimizer.OptPass1PXor(var p: tai): boolean;
     var
       hp1: tai;
     begin
       {
         remove the second (v)pxor from

           pxor reg,reg
           ...
           pxor reg,reg
       }
       Result:=false;
       if MatchOperand(taicpu(p).oper[0]^,taicpu(p).oper[1]^) and
         MatchOpType(taicpu(p),top_reg,top_reg) and
         GetNextInstructionUsingReg(p,hp1,taicpu(p).oper[0]^.reg) and
         MatchInstruction(hp1,taicpu(p).opcode,[taicpu(p).opsize]) and
         MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[0]^) and
         MatchOperand(taicpu(hp1).oper[0]^,taicpu(hp1).oper[1]^) then
         begin
           DebugMsg(SPeepholeOptimization + 'PXorPXor2PXor done',hp1);
           RemoveInstruction(hp1);
           Result:=true;
           Exit;
         end
        {
           replace
             pxor reg1,reg1
             movapd/s reg1,reg2
             dealloc reg1

             by

             pxor reg2,reg2
        }
        else if GetNextInstruction(p,hp1) and
          { we mix single and double opperations here because we assume that the compiler
            generates vmovapd only after double operations and vmovaps only after single operations }
          MatchInstruction(hp1,A_MOVAPD,A_MOVAPS,[S_NO]) and
          MatchOperand(taicpu(p).oper[0]^,taicpu(p).oper[1]^) and
          MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[0]^) and
          (taicpu(p).oper[0]^.typ=top_reg) then
          begin
            TransferUsedRegs(TmpUsedRegs);
            UpdateUsedRegs(TmpUsedRegs, tai(p.next));
            if not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp1,TmpUsedRegs)) then
              begin
                taicpu(p).loadoper(0,taicpu(hp1).oper[1]^);
                taicpu(p).loadoper(1,taicpu(hp1).oper[1]^);
                DebugMsg(SPeepholeOptimization + 'PXorMovapd2PXor done',p);
                RemoveInstruction(hp1);
                result:=true;
              end;
          end;

     end;


   function TX86AsmOptimizer.OptPass1VPXor(var p: tai): boolean;
     var
       hp1: tai;
     begin
       {
         remove the second (v)pxor from

           (v)pxor reg,reg
           ...
           (v)pxor reg,reg
       }
       Result:=false;
       if MatchOperand(taicpu(p).oper[0]^,taicpu(p).oper[1]^,taicpu(p).oper[2]^) and
         MatchOpType(taicpu(p),top_reg,top_reg,top_reg) then
         begin
           if GetNextInstructionUsingReg(p,hp1,taicpu(p).oper[0]^.reg) and
             MatchInstruction(hp1,taicpu(p).opcode,[taicpu(p).opsize]) and
             MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[0]^) and
             MatchOperand(taicpu(hp1).oper[0]^,taicpu(hp1).oper[1]^,taicpu(hp1).oper[2]^) then
             begin
               DebugMsg(SPeepholeOptimization + 'VPXorVPXor2VPXor done',hp1);
               RemoveInstruction(hp1);
               Result:=true;
               Exit;
             end;
{$ifdef x86_64}
           {
              replace
                 vpxor reg1,reg1,reg1
                 vmov reg,mem

                 by

                 movq $0,mem
           }
           if GetNextInstruction(p,hp1) and
             MatchInstruction(hp1,A_VMOVSD,[]) and
             MatchOperand(taicpu(p).oper[2]^,taicpu(hp1).oper[0]^) and
             MatchOpType(taicpu(hp1),top_reg,top_ref) then
             begin
               TransferUsedRegs(TmpUsedRegs);
               UpdateUsedRegs(TmpUsedRegs, tai(p.next));
               if not(RegUsedAfterInstruction(taicpu(hp1).oper[0]^.reg,hp1,TmpUsedRegs)) then
                 begin
                   taicpu(hp1).loadconst(0,0);
                   taicpu(hp1).opcode:=A_MOV;
                   taicpu(hp1).opsize:=S_Q;
                   DebugMsg(SPeepholeOptimization + 'VPXorVMov2Mov done',p);
                   RemoveCurrentP(p);
                   result:=true;
                   Exit;
                 end;
             end;
{$endif x86_64}
         end
       {
          replace
             vpxor reg1,reg1,reg2

             by

             vpxor reg2,reg2,reg2

             to avoid unncessary data dependencies
       }
       else if MatchOperand(taicpu(p).oper[0]^,taicpu(p).oper[1]^) and
         MatchOpType(taicpu(p),top_reg,top_reg,top_reg) then
         begin
           DebugMsg(SPeepholeOptimization + 'VPXor2VPXor done',p);
           { avoid unncessary data dependency }
           taicpu(p).loadreg(0,taicpu(p).oper[2]^.reg);
           taicpu(p).loadreg(1,taicpu(p).oper[2]^.reg);
           result:=true;
           exit;
         end;
       Result:=OptPass1VOP(p);
     end;


   function TX86AsmOptimizer.OptPass1Imul(var p: tai): boolean;
     var
       hp1 : tai;
     begin
       result:=false;
       { replace
           IMul   const,%mreg1,%mreg2
           Mov    %reg2,%mreg3
           dealloc  %mreg3

           by
           Imul   const,%mreg1,%mreg23
       }
       if (taicpu(p).ops=3) and
         GetNextInstruction(p,hp1) and
         MatchInstruction(hp1,A_MOV,[taicpu(p).opsize]) and
         MatchOperand(taicpu(p).oper[2]^,taicpu(hp1).oper[0]^) and
         (taicpu(hp1).oper[1]^.typ=top_reg) then
         begin
           TransferUsedRegs(TmpUsedRegs);
           UpdateUsedRegs(TmpUsedRegs, tai(p.next));
           if not(RegUsedAfterInstruction(taicpu(hp1).oper[0]^.reg,hp1,TmpUsedRegs)) then
             begin
               taicpu(p).loadoper(2,taicpu(hp1).oper[1]^);
               DebugMsg(SPeepholeOptimization + 'ImulMov2Imul done',p);
               RemoveInstruction(hp1);
               result:=true;
             end;
         end;
     end;


   function TX86AsmOptimizer.OptPass1SHXX(var p: tai): boolean;
     var
       hp1 : tai;
     begin
       result:=false;
       { replace
           IMul   %reg0,%reg1,%reg2
           Mov    %reg2,%reg3
           dealloc  %reg2

           by
           Imul   %reg0,%reg1,%reg3
       }
       if GetNextInstruction(p,hp1) and
         MatchInstruction(hp1,A_MOV,[taicpu(p).opsize]) and
         MatchOperand(taicpu(p).oper[2]^,taicpu(hp1).oper[0]^) and
         (taicpu(hp1).oper[1]^.typ=top_reg) then
         begin
           TransferUsedRegs(TmpUsedRegs);
           UpdateUsedRegs(TmpUsedRegs, tai(p.next));
           if not(RegUsedAfterInstruction(taicpu(hp1).oper[0]^.reg,hp1,TmpUsedRegs)) then
             begin
               taicpu(p).loadoper(2,taicpu(hp1).oper[1]^);
               DebugMsg(SPeepholeOptimization + 'SHXXMov2SHXX done',p);
               RemoveInstruction(hp1);
               result:=true;
             end;
         end;
     end;


   function TX86AsmOptimizer.OptPass1_V_Cvtss2sd(var p: tai): boolean;
     var
       hp1: tai;
     begin
       Result:=false;
       { get rid of

         (v)cvtss2sd reg0,<reg1,>reg2
         (v)cvtss2sd reg2,<reg2,>reg0
       }
       if GetNextInstruction(p,hp1) and
         (((taicpu(p).opcode=A_CVTSS2SD) and MatchInstruction(hp1,A_CVTSD2SS,[taicpu(p).opsize]) and
           MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[1]^) and MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[0]^)) or
          ((taicpu(p).opcode=A_VCVTSS2SD) and MatchInstruction(hp1,A_VCVTSD2SS,[taicpu(p).opsize]) and
           MatchOpType(taicpu(p),top_reg,top_reg,top_reg) and
           MatchOpType(taicpu(hp1),top_reg,top_reg,top_reg) and
           (getsupreg(taicpu(p).oper[0]^.reg)=getsupreg(taicpu(p).oper[1]^.reg)) and
           (getsupreg(taicpu(hp1).oper[0]^.reg)=getsupreg(taicpu(hp1).oper[1]^.reg)) and
           (getsupreg(taicpu(p).oper[2]^.reg)=getsupreg(taicpu(hp1).oper[0]^.reg))
          )
         ) then
         begin
           if ((taicpu(p).opcode=A_CVTSS2SD) and (getsupreg(taicpu(p).oper[0]^.reg)=getsupreg(taicpu(hp1).oper[1]^.reg))) or
             ((taicpu(p).opcode=A_VCVTSS2SD) and (getsupreg(taicpu(p).oper[0]^.reg)=getsupreg(taicpu(hp1).oper[2]^.reg))) then
             begin
               DebugMsg(SPeepholeOptimization + '(V)Cvtss2CvtSd(V)Cvtsd2ss2Nop done',p);
               RemoveCurrentP(p);
               RemoveInstruction(hp1);
             end
           else
             begin
               DebugMsg(SPeepholeOptimization + '(V)Cvtss2CvtSd(V)Cvtsd2ss2Vmovaps done',p);
               if taicpu(hp1).opcode=A_CVTSD2SS then
                 begin
                   taicpu(p).loadreg(1,taicpu(hp1).oper[1]^.reg);
                   taicpu(p).opcode:=A_MOVAPS;
                 end
               else
                 begin
                   taicpu(p).loadreg(1,taicpu(hp1).oper[2]^.reg);
                   taicpu(p).opcode:=A_VMOVAPS;
                 end;
               taicpu(p).ops:=2;
               RemoveInstruction(hp1);
             end;
           Result:=true;
           Exit;
         end;
     end;


   function TX86AsmOptimizer.OptPass1Jcc(var p : tai) : boolean;
     var
       hp1, hp2, hp3, hp4, hp5, hp6: tai;
       ThisReg: TRegister;
     begin
       Result := False;
       if not GetNextInstruction(p,hp1) then
         Exit;

       {
           convert
           j<c>  .L1
           mov   1,reg
           jmp   .L2
         .L1
           mov   0,reg
         .L2

         into
           mov   0,reg
           set<not(c)> reg

         take care of alignment and that the mov 0,reg is not converted into a xor as this
         would destroy the flag contents

         Use MOVZX if size is preferred, since while mov 0,reg is bigger, it can be
         executed at the same time as a previous comparison.
           set<not(c)> reg
           movzx       reg, reg
       }

       if MatchInstruction(hp1,A_MOV,[]) and
         (taicpu(hp1).oper[0]^.typ = top_const) and
         (
           (
             (taicpu(hp1).oper[1]^.typ = top_reg)
{$ifdef i386}
             { Under i386, ESI, EDI, EBP and ESP
               don't have an 8-bit representation }
              and not (getsupreg(taicpu(hp1).oper[1]^.reg) in [RS_ESI, RS_EDI, RS_EBP, RS_ESP])

{$endif i386}
           ) or (
{$ifdef i386}
             (taicpu(hp1).oper[1]^.typ <> top_reg) and
{$endif i386}
             (taicpu(hp1).opsize = S_B)
           )
         ) and
         GetNextInstruction(hp1,hp2) and
         MatchInstruction(hp2,A_JMP,[]) and (taicpu(hp2).oper[0]^.ref^.refaddr=addr_full) and
         GetNextInstruction(hp2,hp3) and
         SkipAligns(hp3, hp3) and
         (hp3.typ=ait_label) and
         (tasmlabel(taicpu(p).oper[0]^.ref^.symbol)=tai_label(hp3).labsym) and
         GetNextInstruction(hp3,hp4) and
         MatchInstruction(hp4,A_MOV,[taicpu(hp1).opsize]) and
         (taicpu(hp4).oper[0]^.typ = top_const) and
         (
           ((taicpu(hp1).oper[0]^.val = 0) and (taicpu(hp4).oper[0]^.val = 1)) or
           ((taicpu(hp1).oper[0]^.val = 1) and (taicpu(hp4).oper[0]^.val = 0))
         ) and
         MatchOperand(taicpu(hp1).oper[1]^,taicpu(hp4).oper[1]^) and
         GetNextInstruction(hp4,hp5) and
         SkipAligns(hp5, hp5) and
         (hp5.typ=ait_label) and
         (tasmlabel(taicpu(hp2).oper[0]^.ref^.symbol)=tai_label(hp5).labsym) then
         begin
           if (taicpu(hp1).oper[0]^.val = 1) and (taicpu(hp4).oper[0]^.val = 0) then
             taicpu(p).condition := inverse_cond(taicpu(p).condition);

           tai_label(hp3).labsym.DecRefs;

           { If this isn't the only reference to the middle label, we can
             still make a saving - only that the first jump and everything
             that follows will remain. }
           if (tai_label(hp3).labsym.getrefs = 0) then
             begin
               if (taicpu(hp1).oper[0]^.val = 1) and (taicpu(hp4).oper[0]^.val = 0) then
                 DebugMsg(SPeepholeOptimization + 'J(c)Mov1JmpMov0 -> Set(~c)',p)
               else
                 DebugMsg(SPeepholeOptimization + 'J(c)Mov0JmpMov1 -> Set(c)',p);

               { remove jump, first label and second MOV (also catching any aligns) }
               repeat
                 if not GetNextInstruction(hp2, hp3) then
                   InternalError(2021040810);

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
               if (taicpu(hp1).oper[0]^.val = 1) and (taicpu(hp4).oper[0]^.val = 0) then
                 DebugMsg(SPeepholeOptimization + 'J(c)Mov1JmpMov0 -> Set(~c) (partial)',p)
               else
                 DebugMsg(SPeepholeOptimization + 'J(c)Mov0JmpMov1 -> Set(c) (partial)',p);
             end;

           taicpu(p).opcode:=A_SETcc;
           taicpu(p).opsize:=S_B;
           taicpu(p).is_jmp:=False;

           if taicpu(hp1).opsize=S_B then
             begin
               taicpu(p).loadoper(0, taicpu(hp1).oper[1]^);
               if taicpu(hp1).oper[1]^.typ = top_reg then
                 AllocRegBetween(taicpu(hp1).oper[1]^.reg, p, hp2, UsedRegs);

               RemoveInstruction(hp1);
             end
           else
             begin
               { Will be a register because the size can't be S_B otherwise }
               ThisReg := newreg(R_INTREGISTER,getsupreg(taicpu(hp1).oper[1]^.reg), R_SUBL);
               taicpu(p).loadreg(0, ThisReg);
               AllocRegBetween(ThisReg, p, hp2, UsedRegs);

               if (cs_opt_size in current_settings.optimizerswitches) and IsMOVZXAcceptable then
                 begin
                   case taicpu(hp1).opsize of
                     S_W:
                       taicpu(hp1).opsize := S_BW;
                     S_L:
                       taicpu(hp1).opsize := S_BL;
{$ifdef x86_64}
                     S_Q:
                       begin
                         taicpu(hp1).opsize := S_BL;
                         { Change the destination register to 32-bit }
                         taicpu(hp1).loadreg(1, newreg(R_INTREGISTER,getsupreg(ThisReg), R_SUBD));
                       end;
{$endif x86_64}
                     else
                       InternalError(2021040820);
                   end;

                   taicpu(hp1).opcode := A_MOVZX;
                   taicpu(hp1).loadreg(0, ThisReg);
                 end
               else
                 begin
                   AllocRegBetween(NR_FLAGS,p,hp1,UsedRegs);

                   { hp1 is already a MOV instruction with the correct register }
                   taicpu(hp1).loadconst(0, 0);

                   { Inserting it right before p will guarantee that the flags are also tracked }
                   asml.Remove(hp1);
                   asml.InsertBefore(hp1, p);
                 end;
             end;

           Result:=true;
           exit;
         end
       else if (hp1.typ = ait_label) then
         Result := DoSETccLblRETOpt(p, tai_label(hp1));
     end;


  function TX86AsmOptimizer.OptPass1VMOVDQ(var p: tai): Boolean;
    var
      hp1, hp2, hp3: tai;
      SourceRef, TargetRef: TReference;
      CurrentReg: TRegister;
    begin
      { VMOVDQU/CMOVDQA shouldn't have even been generated }
      if not UseAVX then
        InternalError(2021100501);

      Result := False;

      { Look for the following to simplify:

          vmovdqa/u x(mem1), %xmmreg
          vmovdqa/u %xmmreg, y(mem2)
          vmovdqa/u x+16(mem1), %xmmreg
          vmovdqa/u %xmmreg, y+16(mem2)

        Change to:
          vmovdqa/u x(mem1), %ymmreg
          vmovdqa/u %ymmreg, y(mem2)
          vpxor     %ymmreg, %ymmreg, %ymmreg

          ( The VPXOR instruction is to zero the upper half, thus removing the
            need to call the potentially expensive VZEROUPPER instruction. Other
            peephole optimisations can remove VPXOR if it's unnecessary )
      }
      TransferUsedRegs(TmpUsedRegs);
      UpdateUsedRegs(TmpUsedRegs, tai(p.Next));

      { NOTE: In the optimisations below, if the references dictate that an
        aligned move is possible (i.e. VMOVDQA), the existing instructions
        should already be VMOVDQA because if (x mod 32) = 0, then (x mod 16) = 0 }
      if (taicpu(p).opsize = S_XMM) and
        MatchOpType(taicpu(p), top_ref, top_reg) and
        GetNextInstruction(p, hp1) and
        MatchInstruction(hp1, A_VMOVDQA, A_VMOVDQU, [S_XMM]) and
        MatchOpType(taicpu(hp1), top_reg, top_ref) and
        not RegUsedAfterInstruction(taicpu(p).oper[1]^.reg, hp1, TmpUsedRegs) then
        begin
          SourceRef := taicpu(p).oper[0]^.ref^;
          TargetRef := taicpu(hp1).oper[1]^.ref^;
          if GetNextInstruction(hp1, hp2) and
            MatchInstruction(hp2, A_VMOVDQA, A_VMOVDQU, [S_XMM]) and
            MatchOpType(taicpu(hp2), top_ref, top_reg) then
            begin
              { Delay calling GetNextInstruction(hp2, hp3) for as long as possible }
              UpdateUsedRegs(TmpUsedRegs, tai(hp1.Next));

              Inc(SourceRef.offset, 16);

              { Reuse the register in the first block move }
              CurrentReg := newreg(R_MMREGISTER, getsupreg(taicpu(p).oper[1]^.reg), R_SUBMMY);

              if RefsEqual(SourceRef, taicpu(hp2).oper[0]^.ref^) and
                not RefsMightOverlap(taicpu(p).oper[0]^.ref^, TargetRef, 32) then
                begin
                  UpdateUsedRegs(TmpUsedRegs, tai(hp2.Next));
                  Inc(TargetRef.offset, 16);
                  if GetNextInstruction(hp2, hp3) and
                    MatchInstruction(hp3, A_VMOVDQA, A_VMOVDQU, [S_XMM]) and
                    MatchOpType(taicpu(hp3), top_reg, top_ref) and
                    (taicpu(hp2).oper[1]^.reg = taicpu(hp3).oper[0]^.reg) and
                    RefsEqual(TargetRef, taicpu(hp3).oper[1]^.ref^) and
                    not RegUsedAfterInstruction(taicpu(hp2).oper[1]^.reg, hp3, TmpUsedRegs) then
                    begin
                      { Update the register tracking to the new size }
                      AllocRegBetween(CurrentReg, p, hp2, UsedRegs);

                      { Remember that the offsets are 16 ahead }

                      { Switch to unaligned if the memory isn't on a 32-byte boundary }
                      if not (
                          ((SourceRef.offset mod 32) = 16) and
                          (SourceRef.alignment >= 32) and ((SourceRef.alignment mod 32) = 0)
                        ) then
                        taicpu(p).opcode := A_VMOVDQU;

                      taicpu(p).opsize := S_YMM;
                      taicpu(p).oper[1]^.reg := CurrentReg;

                      if not (
                          ((TargetRef.offset mod 32) = 16) and
                          (TargetRef.alignment >= 32) and ((TargetRef.alignment mod 32) = 0)
                        ) then
                        taicpu(hp1).opcode := A_VMOVDQU;

                      taicpu(hp1).opsize := S_YMM;
                      taicpu(hp1).oper[0]^.reg := CurrentReg;

                      DebugMsg(SPeepholeOptimization + 'Used ' + debug_regname(CurrentReg) + ' to merge a pair of memory moves (VmovdqxVmovdqxVmovdqxVmovdqx2VmovdqyVmovdqy 1)', p);

                      { If pi_uses_ymm is set, VZEROUPPER is present to do this for us }
                      if (pi_uses_ymm in current_procinfo.flags) then
                        RemoveInstruction(hp2)
                      else
                        begin
                          taicpu(hp2).opcode := A_VPXOR;
                          taicpu(hp2).opsize := S_YMM;
                          taicpu(hp2).loadreg(0, CurrentReg);
                          taicpu(hp2).loadreg(1, CurrentReg);
                          taicpu(hp2).loadreg(2, CurrentReg);
                          taicpu(hp2).ops := 3;
                        end;

                      RemoveInstruction(hp3);
                      Result := True;
                      Exit;
                    end;
                end
              else
                begin
                  { See if the next references are 16 less rather than 16 greater }

                  Dec(SourceRef.offset, 32); { -16 the other way }
                  if RefsEqual(SourceRef, taicpu(hp2).oper[0]^.ref^) then
                    begin
                      UpdateUsedRegs(TmpUsedRegs, tai(hp2.Next));
                      Dec(TargetRef.offset, 16); { Only 16, not 32, as it wasn't incremented unlike SourceRef }
                      if not RefsMightOverlap(SourceRef, TargetRef, 32) and
                        GetNextInstruction(hp2, hp3) and
                        MatchInstruction(hp3, A_MOV, [taicpu(p).opsize]) and
                        MatchOpType(taicpu(hp3), top_reg, top_ref) and
                        (taicpu(hp2).oper[1]^.reg = taicpu(hp3).oper[0]^.reg) and
                        RefsEqual(TargetRef, taicpu(hp3).oper[1]^.ref^) and
                        not RegUsedAfterInstruction(taicpu(hp2).oper[1]^.reg, hp3, TmpUsedRegs) then
                        begin
                          { Update the register tracking to the new size }
                          AllocRegBetween(CurrentReg, hp2, hp3, UsedRegs);

                          { hp2 and hp3 are the starting offsets, so mod = 0 this time }

                          { Switch to unaligned if the memory isn't on a 32-byte boundary }
                          if not(
                              ((SourceRef.offset mod 32) = 0) and
                              (SourceRef.alignment >= 32) and ((SourceRef.alignment mod 32) = 0)
                            ) then
                            taicpu(hp2).opcode := A_VMOVDQU;

                          taicpu(hp2).opsize := S_YMM;
                          taicpu(hp2).oper[1]^.reg := CurrentReg;

                          if not (
                              ((TargetRef.offset mod 32) = 0) and
                              (TargetRef.alignment >= 32) and ((TargetRef.alignment mod 32) = 0)
                            ) then
                            taicpu(hp3).opcode := A_VMOVDQU;

                          taicpu(hp3).opsize := S_YMM;
                          taicpu(hp3).oper[0]^.reg := CurrentReg;

                          DebugMsg(SPeepholeOptimization + 'Used ' + debug_regname(CurrentReg) + ' to merge a pair of memory moves (VmovdqxVmovdqxVmovdqxVmovdqx2VmovdqyVmovdqy 2)', p);

                          { If pi_uses_ymm is set, VZEROUPPER is present to do this for us }
                          if (pi_uses_ymm in current_procinfo.flags) then
                            RemoveInstruction(hp1)
                          else
                            begin
                              taicpu(hp1).opcode := A_VPXOR;
                              taicpu(hp1).opsize := S_YMM;
                              taicpu(hp1).loadreg(0, CurrentReg);
                              taicpu(hp1).loadreg(1, CurrentReg);
                              taicpu(hp1).loadreg(2, CurrentReg);
                              taicpu(hp1).ops := 3;

                              Asml.Remove(hp1);
                              Asml.InsertAfter(hp1, hp3); { Register deallocations will be after hp3 }
                            end;

                          RemoveCurrentP(p, hp2);
                          Result := True;
                          Exit;
                        end;
                    end;
                end;
            end;
        end;
    end;

  function TX86AsmOptimizer.CheckJumpMovTransferOpt(var p: tai; hp1: tai; LoopCount: Integer; out Count: Integer): Boolean;
    var
      hp2, hp3, first_assignment: tai;
      IncCount, OperIdx: Integer;
      OrigLabel: TAsmLabel;
    begin
      Count := 0;
      Result := False;
      first_assignment := nil;
      if (LoopCount >= 20) then
        begin
          { Guard against infinite loops }
          Exit;
        end;
      if (taicpu(p).oper[0]^.typ <> top_ref) or
        (taicpu(p).oper[0]^.ref^.refaddr <> addr_full) or
        (taicpu(p).oper[0]^.ref^.base <> NR_NO) or
        (taicpu(p).oper[0]^.ref^.index <> NR_NO) or
        not (taicpu(p).oper[0]^.ref^.symbol is TAsmLabel) then
        Exit;

      OrigLabel := TAsmLabel(taicpu(p).oper[0]^.ref^.symbol);

      {
        change
               jmp .L1
               ...
           .L1:
               mov ##, ## ( multiple movs possible )
               jmp/ret
        into
               mov ##, ##
               jmp/ret
      }

      if not Assigned(hp1) then
        begin
          hp1 := GetLabelWithSym(OrigLabel);
          if not Assigned(hp1) or not SkipLabels(hp1, hp1) then
            Exit;

        end;

      hp2 := hp1;

      while Assigned(hp2) do
        begin
          if Assigned(hp2) and (hp2.typ in [ait_label, ait_align]) then
            SkipLabels(hp2,hp2);

          if not Assigned(hp2) or (hp2.typ <> ait_instruction) then
            Break;

          case taicpu(hp2).opcode of
            A_MOVSD:
              begin
                if taicpu(hp2).ops = 0 then
                  { Wrong MOVSD }
                  Break;
                Inc(Count);
                if Count >= 5 then
                  { Too many to be worthwhile }
                  Break;
                GetNextInstruction(hp2, hp2);
                Continue;
              end;
            A_MOV,
            A_MOVD,
            A_MOVQ,
            A_MOVSX,
{$ifdef x86_64}
            A_MOVSXD,
{$endif x86_64}
            A_MOVZX,
            A_MOVAPS,
            A_MOVUPS,
            A_MOVSS,
            A_MOVAPD,
            A_MOVUPD,
            A_MOVDQA,
            A_MOVDQU,
            A_VMOVSS,
            A_VMOVAPS,
            A_VMOVUPS,
            A_VMOVSD,
            A_VMOVAPD,
            A_VMOVUPD,
            A_VMOVDQA,
            A_VMOVDQU:
              begin
                Inc(Count);
                if Count >= 5 then
                  { Too many to be worthwhile }
                  Break;
                GetNextInstruction(hp2, hp2);
                Continue;
              end;
            A_JMP:
              begin
                { Guard against infinite loops }
                if taicpu(hp2).oper[0]^.ref^.symbol = OrigLabel then
                  Exit;

                { Analyse this jump first in case it also duplicates assignments }
                if CheckJumpMovTransferOpt(hp2, nil, LoopCount + 1, IncCount) then
                  begin
                    { Something did change! }
                    Result := True;

                    Inc(Count, IncCount);
                    if Count >= 5 then
                      begin
                        { Too many to be worthwhile }
                        Exit;
                      end;

                    if MatchInstruction(hp2, [A_JMP, A_RET], []) then
                      Break;
                  end;

                Result := True;
                Break;
              end;
            A_RET:
              begin
                Result := True;
                Break;
              end;
            else
              Break;
          end;
        end;

      if Result then
        begin
          { A count of zero can happen when CheckJumpMovTransferOpt is called recursively }
          if Count = 0 then
            begin
              Result := False;
              Exit;
            end;

          hp3 := p;
          DebugMsg(SPeepholeOptimization + 'Duplicated ' + debug_tostr(Count) + ' assignment(s) and redirected jump', p);
          while True do
            begin
              if Assigned(hp1) and (hp1.typ in [ait_label, ait_align]) then
                SkipLabels(hp1,hp1);

              if (hp1.typ <> ait_instruction) then
                InternalError(2021040720);

              case taicpu(hp1).opcode of
                A_JMP:
                  begin
                    { Change the original jump to the new destination }
                    OrigLabel.decrefs;
                    taicpu(hp1).oper[0]^.ref^.symbol.increfs;
                    taicpu(p).loadref(0, taicpu(hp1).oper[0]^.ref^);

                    { Set p to the first duplicated assignment so it can get optimised if needs be }
                    if not Assigned(first_assignment) then
                      InternalError(2021040810)
                    else
                      p := first_assignment;

                    Exit;
                  end;
                A_RET:
                  begin
                    { Now change the jump into a RET instruction }
                    ConvertJumpToRET(p, hp1);

                    { Set p to the first duplicated assignment so it can get optimised if needs be }
                    if not Assigned(first_assignment) then
                      InternalError(2021040811)
                    else
                      p := first_assignment;

                    Exit;
                  end;
                else
                  begin
                    { Duplicate the MOV instruction }
                    hp3:=tai(hp1.getcopy);
                    if first_assignment = nil then
                      first_assignment := hp3;

                    asml.InsertBefore(hp3, p);

                    { Make sure the compiler knows about any final registers written here }
                    for OperIdx := 0 to taicpu(hp3).ops - 1 do
                      with taicpu(hp3).oper[OperIdx]^ do
                        begin
                          case typ of
                            top_ref:
                              begin
                                if (ref^.base <> NR_NO) and
                                  (getsupreg(ref^.base) <> RS_ESP) and
                                  (getsupreg(ref^.base) <> RS_EBP)
                                  {$ifdef x86_64} and (ref^.base <> NR_RIP) {$endif x86_64}
                                  then
                                  AllocRegBetween(ref^.base, hp3, tai(p.Next), UsedRegs);
                                if (ref^.index <> NR_NO) and
                                  (getsupreg(ref^.index) <> RS_ESP) and
                                  (getsupreg(ref^.index) <> RS_EBP)
                                  {$ifdef x86_64} and (ref^.index <> NR_RIP) {$endif x86_64} and
                                  (ref^.index <> ref^.base) then
                                  AllocRegBetween(ref^.index, hp3, tai(p.Next), UsedRegs);
                              end;
                            top_reg:
                              AllocRegBetween(reg, hp3, tai(p.Next), UsedRegs);
                            else
                              ;
                          end;
                        end;
                  end;
              end;

              if not GetNextInstruction(hp1, hp1) then
                { Should have dropped out earlier }
                InternalError(2021040710);
            end;
        end;
    end;


  const
    WriteOp: array[0..3] of set of TInsChange = (
      [Ch_Wop1, Ch_RWop1, Ch_Mop1],
      [Ch_Wop2, Ch_RWop2, Ch_Mop2],
      [Ch_Wop3, Ch_RWop3, Ch_Mop3],
      [Ch_Wop4, Ch_RWop4, Ch_Mop4]);

    RegWriteFlags: array[0..7] of set of TInsChange = (
      { The order is important: EAX, ECX, EDX, EBX, ESI, EDI, EBP, ESP }
      [Ch_WEAX, Ch_RWEAX, Ch_MEAX{$ifdef x86_64}, Ch_WRAX, Ch_RWRAX, Ch_MRAX{$endif x86_64}],
      [Ch_WECX, Ch_RWECX, Ch_MECX{$ifdef x86_64}, Ch_WRCX, Ch_RWRCX, Ch_MRCX{$endif x86_64}],
      [Ch_WEDX, Ch_RWEDX, Ch_MEDX{$ifdef x86_64}, Ch_WRDX, Ch_RWRDX, Ch_MRDX{$endif x86_64}],
      [Ch_WEBX, Ch_RWEBX, Ch_MEBX{$ifdef x86_64}, Ch_WRBX, Ch_RWRBX, Ch_MRBX{$endif x86_64}],
      [Ch_WESI, Ch_RWESI, Ch_MESI{$ifdef x86_64}, Ch_WRSI, Ch_RWRSI, Ch_MRSI{$endif x86_64}],
      [Ch_WEDI, Ch_RWEDI, Ch_MEDI{$ifdef x86_64}, Ch_WRDI, Ch_RWRDI, Ch_MRDI{$endif x86_64}],
      [Ch_WEBP, Ch_RWEBP, Ch_MEBP{$ifdef x86_64}, Ch_WRBP, Ch_RWRBP, Ch_MRBP{$endif x86_64}],
      [Ch_WESP, Ch_RWESP, Ch_MESP{$ifdef x86_64}, Ch_WRSP, Ch_RWRSP, Ch_MRSP{$endif x86_64}]);


  function TX86AsmOptimizer.TrySwapMovOp(var p, hp1: tai): Boolean;
    var
      hp2: tai;
      X: Integer;
    begin
      { If we have something like:
          op  ###,###
          mov ###,###

        Try to move the MOV instruction to before OP as long as OP and MOV don't
        interfere in regards to what they write to.

        NOTE: p must be a 2-operand instruction
      }

      Result := False;
      if (hp1.typ <> ait_instruction) or
        taicpu(hp1).is_jmp or
        RegInInstruction(NR_DEFAULTFLAGS, hp1) then
        Exit;

      { NOP is a pipeline fence, likely marking the beginning of the function
        epilogue, so drop out.  Similarly, drop out if POP or RET are
        encountered }
      if MatchInstruction(hp1, A_NOP, A_POP, A_RET, []) then
        Exit;

      if (taicpu(hp1).opcode = A_MOVSD) and
        (taicpu(hp1).ops = 0) then
        { Wrong MOVSD }
        Exit;

      { Check for writes to specific registers first }
      { EAX, ECX, EDX, EBX, ESI, EDI, EBP, ESP in that order }
      for X := 0 to 7 do
        if (RegWriteFlags[X] * InsProp[taicpu(hp1).opcode].Ch <> [])
          and RegInInstruction(newreg(R_INTREGISTER, TSuperRegister(X), R_SUBWHOLE), p) then
          Exit;

      for X := 0 to taicpu(hp1).ops - 1 do
        begin
          { Check to see if this operand writes to something }
          if ((WriteOp[X] * InsProp[taicpu(hp1).opcode].Ch) <> []) and
            { And matches something in the CMP/TEST instruction }
            (
              MatchOperand(taicpu(hp1).oper[X]^, taicpu(p).oper[0]^) or
              MatchOperand(taicpu(hp1).oper[X]^, taicpu(p).oper[1]^) or
              (
                { If it's a register, make sure the register written to doesn't
                  appear in the cmp instruction as part of a reference }
                (taicpu(hp1).oper[X]^.typ = top_reg) and
                RegInInstruction(taicpu(hp1).oper[X]^.reg, p)
              )
            ) then
            Exit;
        end;

      { Check p to make sure it doesn't write to something that affects hp1 }

      { Check for writes to specific registers first }
      { EAX, ECX, EDX, EBX, ESI, EDI, EBP, ESP in that order }
      for X := 0 to 7 do
        if (RegWriteFlags[X] * InsProp[taicpu(p).opcode].Ch <> [])
          and RegInInstruction(newreg(R_INTREGISTER, TSuperRegister(X), R_SUBWHOLE), hp1) then
          Exit;

      for X := 0 to taicpu(p).ops - 1 do
        begin
          { Check to see if this operand writes to something }
          if ((WriteOp[X] * InsProp[taicpu(p).opcode].Ch) <> []) and
            { And matches something in hp1 }
            (taicpu(p).oper[X]^.typ = top_reg) and
            RegInInstruction(taicpu(p).oper[X]^.reg, hp1) then
            Exit;
        end;

      { The instruction can be safely moved }
      asml.Remove(hp1);

      { Try to insert after the last instructions where the FLAGS register is not
        yet in use, so "mov $0,%reg" can be optimised into "xor %reg,%reg" later }
      if SetAndTest(FindRegAllocBackward(NR_DEFAULTFLAGS, tai(p.Previous)), hp2) then
        asml.InsertBefore(hp1, hp2)

        { Failing that, try to insert after the last instructions where the
          FLAGS register is not yet in use }
      else if GetLastInstruction(p, hp2) and
        (
          (hp2.typ <> ait_instruction) or
          { Don't insert after an instruction that uses the flags when p doesn't use them }
          RegInInstruction(NR_DEFAULTFLAGS, p) or
          not RegInInstruction(NR_DEFAULTFLAGS, hp2)
        ) then
        asml.InsertAfter(hp1, hp2)
      else
        { Note, if p.Previous is nil (even if it should logically never be the
        case), FindRegAllocBackward immediately exits with False and so we
        safely land here (we can't just pass p because FindRegAllocBackward
        immediately exits on an instruction). [Kit] }
        asml.InsertBefore(hp1, p);

      DebugMsg(SPeepholeOptimization + 'Swapped ' + debug_op2str(taicpu(p).opcode) + ' and ' + debug_op2str(taicpu(hp1).opcode) + ' instructions to improve optimisation potential', hp1);

      { We can't trust UsedRegs because we're looking backwards, although we
        know the registers are allocated after p at the very least, so manually
        create tai_regalloc objects if needed }
      for X := 0 to taicpu(hp1).ops - 1 do
        case taicpu(hp1).oper[X]^.typ of
          top_reg:
            begin
              asml.InsertBefore(tai_regalloc.alloc(taicpu(hp1).oper[X]^.reg, nil), hp1);
              IncludeRegInUsedRegs(taicpu(hp1).oper[X]^.reg, UsedRegs);
              AllocRegBetween(taicpu(hp1).oper[X]^.reg, hp1, p, UsedRegs);
            end;
          top_ref:
            begin
              if taicpu(hp1).oper[X]^.ref^.base <> NR_NO then
                begin
                  asml.InsertBefore(tai_regalloc.alloc(taicpu(hp1).oper[X]^.ref^.base, nil), hp1);
                  IncludeRegInUsedRegs(taicpu(hp1).oper[X]^.ref^.base, UsedRegs);
                  AllocRegBetween(taicpu(hp1).oper[X]^.ref^.base, hp1, p, UsedRegs);
                end;
              if taicpu(hp1).oper[X]^.ref^.index <> NR_NO then
                begin
                  asml.InsertBefore(tai_regalloc.alloc(taicpu(hp1).oper[X]^.ref^.index, nil), hp1);
                  IncludeRegInUsedRegs(taicpu(hp1).oper[X]^.ref^.index, UsedRegs);
                  AllocRegBetween(taicpu(hp1).oper[X]^.ref^.index, hp1, p, UsedRegs);
                end;
            end;
          else
            ;
        end;

      Result := True;
    end;


  function TX86AsmOptimizer.TrySwapMovCmp(var p, hp1: tai): Boolean;
    var
      hp2: tai;
      X: Integer;
    begin
      { If we have something like:
          cmp ###,%reg1
          mov 0,%reg2

        And no modified registers are shared, move the instruction to before
        the comparison as this means it can be optimised without worrying
        about the FLAGS register. (CMP/MOV is generated by
        "J(c)Mov1JmpMov0 -> Set(~c)", among other things).

        As long as the second instruction doesn't use the flags or one of the
        registers used by CMP or TEST (also check any references that use the
        registers), then it can be moved prior to the comparison.
      }

      Result := False;
      if not TrySwapMovOp(p, hp1) then
        Exit;

      if taicpu(hp1).opcode = A_LEA then
        { The flags will be overwritten by the CMP/TEST instruction }
        ConvertLEA(taicpu(hp1));

      Result := True;

      { Can we move it one further back? }
      if GetLastInstruction(hp1, hp2) and (hp2.typ = ait_instruction) and
        { Check to see if CMP/TEST is a comparison against zero }
        (
          (
            (taicpu(p).opcode = A_CMP) and
            MatchOperand(taicpu(p).oper[0]^, 0)
          ) or
          (
            (taicpu(p).opcode = A_TEST) and
            (
              OpsEqual(taicpu(p).oper[0]^, taicpu(p).oper[1]^) or
              MatchOperand(taicpu(p).oper[0]^, -1)
            )
          )
        ) and
        { These instructions set the zero flag if the result is zero }
        MatchInstruction(hp2, [A_ADD, A_SUB, A_OR, A_XOR, A_AND, A_POPCNT, A_LZCNT], []) and
        OpsEqual(taicpu(hp2).oper[1]^, taicpu(p).oper[1]^) then
          { Looks like we can - if successful, this benefits PostPeepholeOptTestOr }
          TrySwapMovOp(hp2, hp1);
    end;


  function TX86AsmOptimizer.OptPass2MOV(var p : tai) : boolean;

     function IsXCHGAcceptable: Boolean; inline;
       begin
         { Always accept if optimising for size }
         Result := (cs_opt_size in current_settings.optimizerswitches) or
           { From the Pentium M onwards, XCHG only has a latency of 2 rather
             than 3, so it becomes a saving compared to three MOVs with two of
             them able to execute simultaneously. [Kit] }
           (CPUX86_HINT_FAST_XCHG in cpu_optimization_hints[current_settings.optimizecputype]);
       end;

      var
        NewRef: TReference;
        hp1, hp2, hp3, hp4: Tai;
{$ifndef x86_64}
        OperIdx: Integer;
{$endif x86_64}
        NewInstr : Taicpu;
        NewAligh : Tai_align;
        DestLabel: TAsmLabel;
        TempTracking: TAllUsedRegs;

        function TryMovArith2Lea(InputInstr: tai): Boolean;
          var
            NextInstr: tai;
          begin
            Result := False;
            UpdateUsedRegs(TmpUsedRegs, tai(InputInstr.Next));

            if not GetNextInstruction(InputInstr, NextInstr) or
              (
              { The FLAGS register isn't always tracked properly, so do not
                perform this optimisation if a conditional statement follows }
                not RegReadByInstruction(NR_DEFAULTFLAGS, NextInstr) and
                not RegUsedAfterInstruction(NR_DEFAULTFLAGS, NextInstr, TmpUsedRegs)
              ) then
              begin
                reference_reset(NewRef, 1, []);
                NewRef.base := taicpu(p).oper[0]^.reg;
                NewRef.scalefactor := 1;

                if taicpu(InputInstr).opcode = A_ADD then
                  begin
                    DebugMsg(SPeepholeOptimization + 'MovAdd2Lea', p);
                    NewRef.offset := taicpu(InputInstr).oper[0]^.val;
                  end
                else
                  begin
                    DebugMsg(SPeepholeOptimization + 'MovSub2Lea', p);
                    NewRef.offset := -taicpu(InputInstr).oper[0]^.val;
                  end;

                taicpu(p).opcode := A_LEA;
                taicpu(p).loadref(0, NewRef);

                RemoveInstruction(InputInstr);

                Result := True;
              end;
          end;

     begin
        Result:=false;

        { This optimisation adds an instruction, so only do it for speed }
        if not (cs_opt_size in current_settings.optimizerswitches) and
          MatchOpType(taicpu(p), top_const, top_reg) and
          (taicpu(p).oper[0]^.val = 0) then
          begin

            { To avoid compiler warning }
            DestLabel := nil;

            if (p.typ <> ait_instruction) or (taicpu(p).oper[1]^.typ <> top_reg) then
              InternalError(2021040750);

            if not GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[1]^.reg) then
              Exit;

            case hp1.typ of
              ait_align,
              ait_label:
                begin
                  { Change:
                      mov  $0,%reg                    mov  $0,%reg
                    @Lbl1:                          @Lbl1:
                      test %reg,%reg / cmp $0,%reg    test %reg,%reg / mov $0,%reg
                      je   @Lbl2                      jne  @Lbl2

                    To:                             To:
                      mov  $0,%reg                    mov  $0,%reg
                      jmp  @Lbl2                      jmp  @Lbl3
                      (align)                         (align)
                    @Lbl1:                          @Lbl1:
                      test %reg,%reg / cmp $0,%reg    test %reg,%reg / cmp $0,%reg
                      je   @Lbl2                      je   @Lbl2
                                                    @Lbl3:   <-- Only if label exists

                    (Not if it's optimised for size)
                  }
                  if not SkipAligns(hp1, hp1) or not GetNextInstruction(hp1, hp2) then
                    Exit;

                  if (hp2.typ = ait_instruction) and
                    (
                      { Register sizes must exactly match }
                      (
                        (taicpu(hp2).opcode = A_CMP) and
                        MatchOperand(taicpu(hp2).oper[0]^, 0) and
                        MatchOperand(taicpu(hp2).oper[1]^, taicpu(p).oper[1]^.reg)
                      ) or (
                        (taicpu(hp2).opcode = A_TEST) and
                        MatchOperand(taicpu(hp2).oper[0]^, taicpu(p).oper[1]^.reg) and
                        MatchOperand(taicpu(hp2).oper[1]^, taicpu(p).oper[1]^.reg)
                      )
                    ) and GetNextInstruction(hp2, hp3) and
                    (hp3.typ = ait_instruction) and
                    (taicpu(hp3).opcode = A_JCC) and
                    (taicpu(hp3).oper[0]^.typ=top_ref) and (taicpu(hp3).oper[0]^.ref^.refaddr=addr_full) and (taicpu(hp3).oper[0]^.ref^.base=NR_NO) and
                    (taicpu(hp3).oper[0]^.ref^.index=NR_NO) and (taicpu(hp3).oper[0]^.ref^.symbol is tasmlabel) then
                    begin
                      { Check condition of jump }

                      { Always true? }
                      if condition_in(C_E, taicpu(hp3).condition) then
                        begin
                          { Copy label symbol and obtain matching label entry for the
                            conditional jump, as this will be our destination}
                          DestLabel := tasmlabel(taicpu(hp3).oper[0]^.ref^.symbol);
                          DebugMsg(SPeepholeOptimization + 'Mov0LblCmp0Je -> Mov0JmpLblCmp0Je', p);
                          Result := True;
                        end

                      { Always false? }
                      else if condition_in(C_NE, taicpu(hp3).condition) and GetNextInstruction(hp3, hp2) then
                        begin
                          { This is only worth it if there's a jump to take }

                          case hp2.typ of
                            ait_instruction:
                              begin
                                if taicpu(hp2).opcode = A_JMP then
                                  begin
                                    DestLabel := tasmlabel(taicpu(hp2).oper[0]^.ref^.symbol);
                                    { An unconditional jump follows the conditional jump which will always be false,
                                      so use this jump's destination for the new jump }
                                    DebugMsg(SPeepholeOptimization + 'Mov0LblCmp0Jne -> Mov0JmpLblCmp0Jne (with JMP)', p);
                                    Result := True;
                                  end
                                else if taicpu(hp2).opcode = A_JCC then
                                  begin
                                    DestLabel := tasmlabel(taicpu(hp2).oper[0]^.ref^.symbol);
                                    if condition_in(C_E, taicpu(hp2).condition) then
                                      begin
                                        { A second conditional jump follows the conditional jump which will always be false,
                                          while the second jump is always True, so use this jump's destination for the new jump }
                                        DebugMsg(SPeepholeOptimization + 'Mov0LblCmp0Jne -> Mov0JmpLblCmp0Jne (with second Jcc)', p);
                                        Result := True;
                                      end;

                                    { Don't risk it if the jump isn't always true (Result remains False) }
                                  end;
                              end;
                            else
                              { If anything else don't optimise };
                          end;
                        end;

                      if Result then
                        begin
                          { Just so we have something to insert as a paremeter}
                          reference_reset(NewRef, 1, []);
                          NewInstr := taicpu.op_ref(A_JMP, S_NO, NewRef);

                          { Now actually load the correct parameter (this also
                            increases the reference count) }
                          NewInstr.loadsymbol(0, DestLabel, 0);

                          if (cs_opt_level3 in current_settings.optimizerswitches) then
                            begin
                              { Get instruction before original label (may not be p under -O3) }
                              if not GetLastInstruction(hp1, hp2) then
                                { Shouldn't fail here }
                                InternalError(2021040701);

                              { Before the aligns too }
                              while (hp2.typ = ait_align) do
                                if not GetLastInstruction(hp2, hp2) then
                                  { Shouldn't fail here }
                                  InternalError(2021040702);
                            end
                          else
                            hp2 := p;

                          taicpu(NewInstr).fileinfo := taicpu(hp2).fileinfo;
                          AsmL.InsertAfter(NewInstr, hp2);
                          { Add new alignment field }
      (*                    AsmL.InsertAfter(
                            cai_align.create_max(
                              current_settings.alignment.jumpalign,
                              current_settings.alignment.jumpalignskipmax
                            ),
                            NewInstr
                          ); *)
                        end;

                      Exit;
                    end;
                end;
              else
                ;
            end;

          end;

        if not GetNextInstruction(p, hp1) then
          Exit;

        if MatchInstruction(hp1, A_CMP, A_TEST, [taicpu(p).opsize])
          and DoMovCmpMemOpt(p, hp1) then
          begin
            Result := True;
            Exit;
          end
        else if MatchInstruction(hp1, A_JMP, [S_NO]) then
          begin
            { Sometimes the MOVs that OptPass2JMP produces can be improved
              further, but we can't just put this jump optimisation in pass 1
              because it tends to perform worse when conditional jumps are
              nearby (e.g. when converting CMOV instructions). [Kit] }
            CopyUsedRegs(TempTracking);
            UpdateUsedRegs(tai(p.Next));

            if OptPass2JMP(hp1) then
              { call OptPass1MOV once to potentially merge any MOVs that were created }
              Result := OptPass1MOV(p);
              { OptPass2MOV will now exit but will be called again if OptPass1MOV
                returned True and the instruction is still a MOV, thus checking
                the optimisations below }

            { If OptPass2JMP returned False, no optimisations were done to
              the jump and there are no further optimisations that can be done
              to the MOV instruction on this pass }

            { Restore register state }
            RestoreUsedRegs(TempTracking);
            ReleaseUsedRegs(TempTracking);
          end
        else if MatchOpType(taicpu(p),top_reg,top_reg) and
          (taicpu(p).opsize in [S_L{$ifdef x86_64}, S_Q{$endif x86_64}]) and
          MatchInstruction(hp1,A_ADD,A_SUB,[taicpu(p).opsize]) and
          (taicpu(hp1).oper[1]^.typ = top_reg) and
          (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
          begin
            { Change:
                movl/q %reg1,%reg2      movl/q %reg1,%reg2
                addl/q $x,%reg2         subl/q $x,%reg2
              To:
                leal/q x(%reg1),%reg2   leal/q -x(%reg1),%reg2
            }
            if (taicpu(hp1).oper[0]^.typ = top_const) and
              { be lazy, checking separately for sub would be slightly better }
              (abs(taicpu(hp1).oper[0]^.val)<=$7fffffff) then
              begin
                TransferUsedRegs(TmpUsedRegs);
                UpdateUsedRegs(TmpUsedRegs, tai(p.Next));
                if TryMovArith2Lea(hp1) then
                  begin
                    Result := True;
                    Exit;
                  end
              end
            else if not RegInOp(taicpu(p).oper[1]^.reg, taicpu(hp1).oper[0]^) and
              GetNextInstructionUsingReg(hp1, hp2, taicpu(p).oper[1]^.reg) and
              { Same as above, but also adds or subtracts to %reg2 in between.
                It's still valid as long as the flags aren't in use }
              MatchInstruction(hp2,A_ADD,A_SUB,[taicpu(p).opsize]) and
              MatchOpType(taicpu(hp2), top_const, top_reg) and
              (taicpu(hp2).oper[1]^.reg = taicpu(p).oper[1]^.reg) and
              { be lazy, checking separately for sub would be slightly better }
              (abs(taicpu(hp2).oper[0]^.val)<=$7fffffff) then
              begin
                TransferUsedRegs(TmpUsedRegs);
                UpdateUsedRegs(TmpUsedRegs, tai(p.Next));
                UpdateUsedRegs(TmpUsedRegs, tai(hp1.Next));
                if TryMovArith2Lea(hp2) then
                  begin
                    Result := True;
                    Exit;
                  end;
              end;
          end
        else if MatchOpType(taicpu(p),top_reg,top_reg) and
{$ifdef x86_64}
          MatchInstruction(hp1,A_MOVZX,A_MOVSX,A_MOVSXD,[]) and
{$else x86_64}
          MatchInstruction(hp1,A_MOVZX,A_MOVSX,[]) and
{$endif x86_64}
          MatchOpType(taicpu(hp1),top_reg,top_reg) and
          (taicpu(hp1).oper[0]^.reg = taicpu(p).oper[1]^.reg) then
          { mov reg1, reg2                mov reg1, reg2
            movzx/sx reg2, reg3      to   movzx/sx reg1, reg3}
          begin
            taicpu(hp1).oper[0]^.reg := taicpu(p).oper[0]^.reg;
            DebugMsg(SPeepholeOptimization + 'mov %reg1,%reg2; movzx/sx %reg2,%reg3 -> mov %reg1,%reg2;movzx/sx %reg1,%reg3',p);

            { Don't remove the MOV command without first checking that reg2 isn't used afterwards,
              or unless supreg(reg3) = supreg(reg2)). [Kit] }

            TransferUsedRegs(TmpUsedRegs);
            UpdateUsedRegs(TmpUsedRegs, tai(p.next));

            if (getsupreg(taicpu(p).oper[1]^.reg) = getsupreg(taicpu(hp1).oper[1]^.reg)) or
              not RegUsedAfterInstruction(taicpu(p).oper[1]^.reg, hp1, TmpUsedRegs)
            then
              begin
                RemoveCurrentP(p, hp1);
                Result:=true;
              end;

            exit;
          end
        else if MatchOpType(taicpu(p),top_reg,top_reg) and
          IsXCHGAcceptable and
          { XCHG doesn't support 8-byte registers }
          (taicpu(p).opsize <> S_B) and
          MatchInstruction(hp1, A_MOV, []) and
          MatchOpType(taicpu(hp1),top_reg,top_reg) and
          (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[0]^.reg) and
          GetNextInstruction(hp1, hp2) and
          MatchInstruction(hp2, A_MOV, []) and
          { Don't need to call MatchOpType for hp2 because the operand matches below cover for it }
          MatchOperand(taicpu(hp2).oper[0]^, taicpu(p).oper[1]^.reg) and
          MatchOperand(taicpu(hp2).oper[1]^, taicpu(hp1).oper[0]^.reg) then
          begin
            { mov %reg1,%reg2
              mov %reg3,%reg1        ->  xchg %reg3,%reg1
              mov %reg2,%reg3
              (%reg2 not used afterwards)

              Note that xchg takes 3 cycles to execute, and generally mov's take
              only one cycle apiece, but the first two mov's can be executed in
              parallel, only taking 2 cycles overall.  Older processors should
              therefore only optimise for size. [Kit]
            }
            TransferUsedRegs(TmpUsedRegs);
            UpdateUsedRegs(TmpUsedRegs, tai(p.Next));
            UpdateUsedRegs(TmpUsedRegs, tai(hp1.Next));

            if not RegUsedAfterInstruction(taicpu(p).oper[1]^.reg, hp2, TmpUsedRegs) then
              begin
                DebugMsg(SPeepholeOptimization + 'MovMovMov2XChg', p);
                AllocRegBetween(taicpu(hp2).oper[1]^.reg, p, hp1, UsedRegs);
                taicpu(hp1).opcode := A_XCHG;

                RemoveCurrentP(p, hp1);
                RemoveInstruction(hp2);

                Result := True;
                Exit;
              end;
          end
        else if MatchOpType(taicpu(p),top_reg,top_reg) and
          MatchInstruction(hp1, A_SAR, []) then
          begin
            if MatchOperand(taicpu(hp1).oper[0]^, 31) then
              begin
                { the use of %edx also covers the opsize being S_L }
                if MatchOperand(taicpu(hp1).oper[1]^, NR_EDX) then
                  begin
                    { Note it has to be specifically "movl %eax,%edx", and those specific sub-registers }
                    if (taicpu(p).oper[0]^.reg = NR_EAX) and
                      (taicpu(p).oper[1]^.reg = NR_EDX) then
                      begin
                        { Change:
                            movl %eax,%edx
                            sarl $31,%edx
                          To:
                            cltd
                        }
                        DebugMsg(SPeepholeOptimization + 'MovSar2Cltd', p);
                        RemoveInstruction(hp1);
                        taicpu(p).opcode := A_CDQ;
                        taicpu(p).opsize := S_NO;
                        taicpu(p).clearop(1);
                        taicpu(p).clearop(0);
                        taicpu(p).ops:=0;
                        Result := True;
                      end
                    else if (cs_opt_size in current_settings.optimizerswitches) and
                      (taicpu(p).oper[0]^.reg = NR_EDX) and
                      (taicpu(p).oper[1]^.reg = NR_EAX) then
                      begin
                        { Change:
                            movl %edx,%eax
                            sarl $31,%edx
                          To:
                            movl %edx,%eax
                            cltd

                          Note that this creates a dependency between the two instructions,
                            so only perform if optimising for size.
                        }
                        DebugMsg(SPeepholeOptimization + 'MovSar2MovCltd', p);
                        taicpu(hp1).opcode := A_CDQ;
                        taicpu(hp1).opsize := S_NO;
                        taicpu(hp1).clearop(1);
                        taicpu(hp1).clearop(0);
                        taicpu(hp1).ops:=0;
                      end;
{$ifndef x86_64}
                  end
                { Don't bother if CMOV is supported, because a more optimal
                  sequence would have been generated for the Abs() intrinsic }
                else if not(CPUX86_HAS_CMOV in cpu_capabilities[current_settings.cputype]) and
                  { the use of %eax also covers the opsize being S_L }
                  MatchOperand(taicpu(hp1).oper[1]^, NR_EAX) and
                  (taicpu(p).oper[0]^.reg = NR_EAX) and
                  (taicpu(p).oper[1]^.reg = NR_EDX) and
                  GetNextInstruction(hp1, hp2) and
                  MatchInstruction(hp2, A_XOR, [S_L]) and
                  MatchOperand(taicpu(hp2).oper[0]^, NR_EAX) and
                  MatchOperand(taicpu(hp2).oper[1]^, NR_EDX) and

                  GetNextInstruction(hp2, hp3) and
                  MatchInstruction(hp3, A_SUB, [S_L]) and
                  MatchOperand(taicpu(hp3).oper[0]^, NR_EAX) and
                  MatchOperand(taicpu(hp3).oper[1]^, NR_EDX) then
                  begin
                    { Change:
                        movl %eax,%edx
                        sarl $31,%eax
                        xorl %eax,%edx
                        subl %eax,%edx
                        (Instruction that uses %edx)
                        (%eax deallocated)
                        (%edx deallocated)
                      To:
                        cltd
                        xorl %edx,%eax  <-- Note the registers have swapped
                        subl %edx,%eax
                        (Instruction that uses %eax) <-- %eax rather than %edx
                    }

                    TransferUsedRegs(TmpUsedRegs);
                    UpdateUsedRegs(TmpUsedRegs, tai(p.Next));
                    UpdateUsedRegs(TmpUsedRegs, tai(hp1.Next));
                    UpdateUsedRegs(TmpUsedRegs, tai(hp2.Next));

                    if not RegUsedAfterInstruction(NR_EAX, hp3, TmpUsedRegs) then
                      begin
                        if GetNextInstruction(hp3, hp4) and
                          not RegModifiedByInstruction(NR_EDX, hp4) and
                          not RegUsedAfterInstruction(NR_EDX, hp4, TmpUsedRegs) then
                          begin
                            DebugMsg(SPeepholeOptimization + 'abs() intrinsic optimisation', p);

                            taicpu(p).opcode := A_CDQ;
                            taicpu(p).clearop(1);
                            taicpu(p).clearop(0);
                            taicpu(p).ops:=0;

                            RemoveInstruction(hp1);

                            taicpu(hp2).loadreg(0, NR_EDX);
                            taicpu(hp2).loadreg(1, NR_EAX);

                            taicpu(hp3).loadreg(0, NR_EDX);
                            taicpu(hp3).loadreg(1, NR_EAX);

                            AllocRegBetween(NR_EAX, hp3, hp4, TmpUsedRegs);
                            { Convert references in the following instruction (hp4) from %edx to %eax }
                            for OperIdx := 0 to taicpu(hp4).ops - 1 do
                              with taicpu(hp4).oper[OperIdx]^ do
                                case typ of
                                  top_reg:
                                    if getsupreg(reg) = RS_EDX then
                                      reg := newreg(R_INTREGISTER,RS_EAX,getsubreg(reg));
                                  top_ref:
                                    begin
                                      if getsupreg(reg) = RS_EDX then
                                        ref^.base := newreg(R_INTREGISTER,RS_EAX,getsubreg(reg));
                                      if getsupreg(reg) = RS_EDX then
                                        ref^.index := newreg(R_INTREGISTER,RS_EAX,getsubreg(reg));
                                    end;
                                  else
                                    ;
                                end;
                          end;
                      end;
{$else x86_64}
                  end;
              end
            else if MatchOperand(taicpu(hp1).oper[0]^, 63) and
              { the use of %rdx also covers the opsize being S_Q }
              MatchOperand(taicpu(hp1).oper[1]^, NR_RDX) then
              begin
                { Note it has to be specifically "movq %rax,%rdx", and those specific sub-registers }
                if (taicpu(p).oper[0]^.reg = NR_RAX) and
                  (taicpu(p).oper[1]^.reg = NR_RDX) then
                  begin
                    { Change:
                        movq %rax,%rdx
                        sarq $63,%rdx
                      To:
                        cqto
                    }
                    DebugMsg(SPeepholeOptimization + 'MovSar2Cqto', p);
                    RemoveInstruction(hp1);
                    taicpu(p).opcode := A_CQO;
                    taicpu(p).opsize := S_NO;
                    taicpu(p).clearop(1);
                    taicpu(p).clearop(0);
                    taicpu(p).ops:=0;
                    Result := True;
                  end
                else if (cs_opt_size in current_settings.optimizerswitches) and
                  (taicpu(p).oper[0]^.reg = NR_RDX) and
                  (taicpu(p).oper[1]^.reg = NR_RAX) then
                  begin
                    { Change:
                        movq %rdx,%rax
                        sarq $63,%rdx
                      To:
                        movq %rdx,%rax
                        cqto

                      Note that this creates a dependency between the two instructions,
                        so only perform if optimising for size.
                    }
                    DebugMsg(SPeepholeOptimization + 'MovSar2MovCqto', p);
                    taicpu(hp1).opcode := A_CQO;
                    taicpu(hp1).opsize := S_NO;
                    taicpu(hp1).clearop(1);
                    taicpu(hp1).clearop(0);
                    taicpu(hp1).ops:=0;
{$endif x86_64}
                  end;
              end;
          end
        else if MatchInstruction(hp1, A_MOV, []) and
          (taicpu(hp1).oper[1]^.typ = top_reg) then
          { Though "GetNextInstruction" could be factored out, along with
            the instructions that depend on hp2, it is an expensive call that
            should be delayed for as long as possible, hence we do cheaper
            checks first that are likely to be False. [Kit] }
          begin

            if (
                (
                  MatchOperand(taicpu(p).oper[1]^, NR_EDX) and
                  (taicpu(hp1).oper[1]^.reg = NR_EAX) and
                  (
                    MatchOperand(taicpu(hp1).oper[0]^, taicpu(p).oper[0]^) or
                    MatchOperand(taicpu(hp1).oper[0]^, NR_EDX)
                  )
                ) or
                (
                  MatchOperand(taicpu(p).oper[1]^, NR_EAX) and
                  (taicpu(hp1).oper[1]^.reg = NR_EDX) and
                  (
                    MatchOperand(taicpu(hp1).oper[0]^, taicpu(p).oper[0]^) or
                    MatchOperand(taicpu(hp1).oper[0]^, NR_EAX)
                  )
                )
              ) and
              GetNextInstruction(hp1, hp2) and
              MatchInstruction(hp2, A_SAR, []) and
              MatchOperand(taicpu(hp2).oper[0]^, 31) then
              begin
                if MatchOperand(taicpu(hp2).oper[1]^, NR_EDX) then
                  begin
                    { Change:
                        movl r/m,%edx         movl r/m,%eax         movl r/m,%edx         movl r/m,%eax
                        movl %edx,%eax   or   movl %eax,%edx   or   movl r/m,%eax    or   movl r/m,%edx
                        sarl $31,%edx         sarl $31,%edx         sarl $31,%edx         sarl $31,%edx
                      To:
                        movl r/m,%eax    <- Note the change in register
                        cltd
                    }
                    DebugMsg(SPeepholeOptimization + 'MovMovSar2MovCltd', p);

                    AllocRegBetween(NR_EAX, p, hp1, UsedRegs);
                    taicpu(p).loadreg(1, NR_EAX);

                    taicpu(hp1).opcode := A_CDQ;
                    taicpu(hp1).clearop(1);
                    taicpu(hp1).clearop(0);
                    taicpu(hp1).ops:=0;

                    RemoveInstruction(hp2);
(*
{$ifdef x86_64}
                  end
                else if MatchOperand(taicpu(hp2).oper[1]^, NR_RDX) and
                  { This code sequence does not get generated - however it might become useful
                    if and when 128-bit signed integer types make an appearance, so the code
                    is kept here for when it is eventually needed. [Kit] }
                  (
                    (
                      (taicpu(hp1).oper[1]^.reg = NR_RAX) and
                      (
                        MatchOperand(taicpu(hp1).oper[0]^, taicpu(p).oper[0]^) or
                        MatchOperand(taicpu(hp1).oper[0]^, NR_RDX)
                      )
                    ) or
                    (
                      (taicpu(hp1).oper[1]^.reg = NR_RDX) and
                      (
                        MatchOperand(taicpu(hp1).oper[0]^, taicpu(p).oper[0]^) or
                        MatchOperand(taicpu(hp1).oper[0]^, NR_RAX)
                      )
                    )
                  ) and
                  GetNextInstruction(hp1, hp2) and
                  MatchInstruction(hp2, A_SAR, [S_Q]) and
                  MatchOperand(taicpu(hp2).oper[0]^, 63) and
                  MatchOperand(taicpu(hp2).oper[1]^, NR_RDX) then
                  begin
                    { Change:
                        movq r/m,%rdx         movq r/m,%rax         movq r/m,%rdx         movq r/m,%rax
                        movq %rdx,%rax   or   movq %rax,%rdx   or   movq r/m,%rax    or   movq r/m,%rdx
                        sarq $63,%rdx         sarq $63,%rdx         sarq $63,%rdx         sarq $63,%rdx
                      To:
                        movq r/m,%rax    <- Note the change in register
                        cqto
                    }
                    DebugMsg(SPeepholeOptimization + 'MovMovSar2MovCqto', p);

                    AllocRegBetween(NR_RAX, p, hp1, UsedRegs);
                    taicpu(p).loadreg(1, NR_RAX);

                    taicpu(hp1).opcode := A_CQO;
                    taicpu(hp1).clearop(1);
                    taicpu(hp1).clearop(0);
                    taicpu(hp1).ops:=0;

                    RemoveInstruction(hp2);
{$endif x86_64}
*)
                  end;
              end;
{$ifdef x86_64}
          end
        else if (taicpu(p).opsize = S_L) and
          (taicpu(p).oper[1]^.typ = top_reg) and
          (
            MatchInstruction(hp1, A_MOV,[]) and
            (taicpu(hp1).opsize = S_L) and
            (taicpu(hp1).oper[1]^.typ = top_reg)
          ) and (
            GetNextInstruction(hp1, hp2) and
            (tai(hp2).typ=ait_instruction) and
            (taicpu(hp2).opsize = S_Q) and
            (
              (
                MatchInstruction(hp2, A_ADD,[]) and
                (taicpu(hp2).opsize = S_Q) and
                (taicpu(hp2).oper[0]^.typ = top_reg) and (taicpu(hp2).oper[1]^.typ = top_reg) and
                (
                  (
                    (getsupreg(taicpu(hp2).oper[0]^.reg) = getsupreg(taicpu(p).oper[1]^.reg)) and
                    (getsupreg(taicpu(hp2).oper[1]^.reg) = getsupreg(taicpu(hp1).oper[1]^.reg))
                  ) or (
                    (getsupreg(taicpu(hp2).oper[0]^.reg) = getsupreg(taicpu(hp1).oper[1]^.reg)) and
                    (getsupreg(taicpu(hp2).oper[1]^.reg) = getsupreg(taicpu(p).oper[1]^.reg))
                  )
                )
              ) or (
                MatchInstruction(hp2, A_LEA,[]) and
                (taicpu(hp2).oper[0]^.ref^.offset = 0) and
                (taicpu(hp2).oper[0]^.ref^.scalefactor <= 1) and
                (
                  (
                    (getsupreg(taicpu(hp2).oper[0]^.ref^.base) = getsupreg(taicpu(p).oper[1]^.reg)) and
                    (getsupreg(taicpu(hp2).oper[0]^.ref^.index) = getsupreg(taicpu(hp1).oper[1]^.reg))
                  ) or (
                    (getsupreg(taicpu(hp2).oper[0]^.ref^.base) = getsupreg(taicpu(hp1).oper[1]^.reg)) and
                    (getsupreg(taicpu(hp2).oper[0]^.ref^.index) = getsupreg(taicpu(p).oper[1]^.reg))
                  )
                ) and (
                  (
                    (getsupreg(taicpu(hp2).oper[1]^.reg) = getsupreg(taicpu(hp1).oper[1]^.reg))
                  ) or (
                    (getsupreg(taicpu(hp2).oper[1]^.reg) = getsupreg(taicpu(p).oper[1]^.reg))
                  )
                )
              )
            )
          ) and (
            GetNextInstruction(hp2, hp3) and
            MatchInstruction(hp3, A_SHR,[]) and
            (taicpu(hp3).opsize = S_Q) and
            (taicpu(hp3).oper[0]^.typ = top_const) and (taicpu(hp2).oper[1]^.typ = top_reg) and
            (taicpu(hp3).oper[0]^.val = 1) and
            (taicpu(hp3).oper[1]^.reg = taicpu(hp2).oper[1]^.reg)
          ) then
          begin
            { Change   movl    x,    reg1d         movl    x,    reg1d
                       movl    y,    reg2d         movl    y,    reg2d
                       addq    reg2q,reg1q   or    leaq    (reg1q,reg2q),reg1q
                       shrq    $1,   reg1q         shrq    $1,   reg1q

            ( reg1d and reg2d can be switched around in the first two instructions )

              To       movl    x,    reg1d
                       addl    y,    reg1d
                       rcrl    $1,   reg1d

              This corresponds to the common expression (x + y) shr 1, where
              x and y are Cardinals (replacing "shr 1" with "div 2" produces
              smaller code, but won't account for x + y causing an overflow). [Kit]
            }

            DebugMsg(SPeepholeOptimization + 'MovMov*Shr2MovMov*Rcr', p);

            if (getsupreg(taicpu(hp2).oper[1]^.reg) = getsupreg(taicpu(hp1).oper[1]^.reg)) then
              { Change first MOV command to have the same register as the final output }
              taicpu(p).oper[1]^.reg := taicpu(hp1).oper[1]^.reg
            else
              taicpu(hp1).oper[1]^.reg := taicpu(p).oper[1]^.reg;

            { Change second MOV command to an ADD command. This is easier than
              converting the existing command because it means we don't have to
              touch 'y', which might be a complicated reference, and also the
              fact that the third command might either be ADD or LEA. [Kit] }
            taicpu(hp1).opcode := A_ADD;

            { Delete old ADD/LEA instruction }
            RemoveInstruction(hp2);

            { Convert "shrq $1, reg1q" to "rcr $1, reg1d" }
            taicpu(hp3).opcode := A_RCR;
            taicpu(hp3).changeopsize(S_L);
            setsubreg(taicpu(hp3).oper[1]^.reg, R_SUBD);
{$endif x86_64}
          end;

        if FuncMov2Func(p, hp1) then
          begin
            Result := True;
            Exit;
          end;
      end;


{$push}
{$q-}{$r-}
    function TX86AsmOptimizer.OptPass2Movx(var p : tai) : boolean;
      var
        ThisReg: TRegister;
        MinSize, MaxSize, TryShiftDown, TargetSize: TOpSize;
        TargetSubReg: TSubRegister;
        hp1, hp2: tai;
        RegInUse, RegChanged, p_removed, hp1_removed: Boolean;

        { Store list of found instructions so we don't have to call
          GetNextInstructionUsingReg multiple times }
        InstrList: array of taicpu;
        InstrMax, Index: Integer;
        UpperLimit, SignedUpperLimit, SignedUpperLimitBottom,
        LowerLimit, SignedLowerLimit, SignedLowerLimitBottom,
        TryShiftDownLimit, TryShiftDownSignedLimit, TryShiftDownSignedLimitLower,
        WorkingValue: TCgInt;

        PreMessage: string;

        { Data flow analysis }
        TestValMin, TestValMax, TestValSignedMax: TCgInt;
        BitwiseOnly, OrXorUsed,
        ShiftDownOverflow, UpperSignedOverflow, UpperUnsignedOverflow, LowerSignedOverflow, LowerUnsignedOverflow: Boolean;

        function CheckOverflowConditions: Boolean;
          begin
            Result := True;
            if (TestValSignedMax > SignedUpperLimit) then
              UpperSignedOverflow := True;

            if (TestValSignedMax > SignedLowerLimit) or (TestValSignedMax < SignedLowerLimitBottom) then
              LowerSignedOverflow := True;

            if (TestValMin > LowerLimit) or (TestValMax > LowerLimit) then
              LowerUnsignedOverflow := True;

            if (TestValMin > UpperLimit) or (TestValMax > UpperLimit) or (TestValSignedMax > UpperLimit) or
              (TestValMin < SignedUpperLimitBottom) or (TestValMax < SignedUpperLimitBottom) or (TestValSignedMax < SignedUpperLimitBottom) then
              begin
                { Absolute overflow }
                Result := False;
                Exit;
              end;

            if not ShiftDownOverflow and (TryShiftDown <> S_NO) and
              ((TestValMin > TryShiftDownLimit) or (TestValMax > TryShiftDownLimit)) then
              ShiftDownOverflow := True;

            if (TestValMin < 0) or (TestValMax < 0) then
              begin
                LowerUnsignedOverflow := True;
                UpperUnsignedOverflow := True;
              end;
          end;

        function AdjustInitialLoadAndSize: Boolean;
          begin
            Result := False;

            if not p_removed then
              begin
                if TargetSize = MinSize then
                  begin
                    { Convert the input MOVZX to a MOV }
                    if (taicpu(p).oper[0]^.typ = top_reg) and
                      SuperRegistersEqual(taicpu(p).oper[0]^.reg, ThisReg) then
                      begin
                        { Or remove it completely! }
                        DebugMsg(SPeepholeOptimization + 'Movzx2Nop 1', p);
                        RemoveCurrentP(p);
                        p_removed := True;
                      end
                    else
                      begin
                        DebugMsg(SPeepholeOptimization + 'Movzx2Mov 1', p);
                        taicpu(p).opcode := A_MOV;
                        taicpu(p).oper[1]^.reg := ThisReg;
                        taicpu(p).opsize := TargetSize;
                      end;

                    Result := True;
                  end
                else if TargetSize <> MaxSize then
                  begin

                    case MaxSize of
                      S_L:
                        if TargetSize = S_W then
                          begin
                            DebugMsg(SPeepholeOptimization + 'movzbl2movzbw', p);
                            taicpu(p).opsize := S_BW;
                            taicpu(p).oper[1]^.reg := ThisReg;
                            Result := True;
                          end
                        else
                          InternalError(2020112341);

                      S_W:
                        if TargetSize = S_L then
                          begin
                            DebugMsg(SPeepholeOptimization + 'movzbw2movzbl', p);
                            taicpu(p).opsize := S_BL;
                            taicpu(p).oper[1]^.reg := ThisReg;
                            Result := True;
                          end
                        else
                          InternalError(2020112342);
                      else
                        ;
                    end;
                  end
                else if not hp1_removed and not RegInUse then
                  begin
                    { If we have something like:
                        movzbl (oper),%regd
                        add    x,     %regd
                        movzbl %regb, %regd

                      We can reduce the register size to the input of the final
                      movzbl instruction.  Overflows won't have any effect.
                    }
                    if (taicpu(p).opsize in [S_BW, S_BL]) and
                      (taicpu(hp1).opsize in [S_BW, S_BL{$ifdef x86_64}, S_BQ{$endif x86_64}]) then
                      begin
                        TargetSize := S_B;
                        setsubreg(ThisReg, R_SUBL);
                        Result := True;
                      end
                    else if (taicpu(p).opsize = S_WL) and
                      (taicpu(hp1).opsize in [S_WL{$ifdef x86_64}, S_BQ{$endif x86_64}]) then
                      begin
                        TargetSize := S_W;
                        setsubreg(ThisReg, R_SUBW);
                        Result := True;
                      end;

                    if Result then
                      begin
                        { Convert the input MOVZX to a MOV }
                        if (taicpu(p).oper[0]^.typ = top_reg) and
                          SuperRegistersEqual(taicpu(p).oper[0]^.reg, ThisReg) then
                          begin
                            { Or remove it completely! }
                            DebugMsg(SPeepholeOptimization + 'Movzx2Nop 1a', p);
                            RemoveCurrentP(p);
                            p_removed := True;
                          end
                        else
                          begin
                            DebugMsg(SPeepholeOptimization + 'Movzx2Mov 1a', p);
                            taicpu(p).opcode := A_MOV;
                            taicpu(p).oper[1]^.reg := ThisReg;
                            taicpu(p).opsize := TargetSize;
                          end;
                      end;
                  end;
              end;
          end;

        procedure AdjustFinalLoad;
          begin
            if not LowerUnsignedOverflow then
              begin
                if ((TargetSize = S_L) and (taicpu(hp1).opsize in [S_L, S_BL, S_WL])) or
                  ((TargetSize = S_W) and (taicpu(hp1).opsize in [S_W, S_BW])) then
                  begin
                    { Convert the output MOVZX to a MOV }
                    if SuperRegistersEqual(taicpu(hp1).oper[1]^.reg, ThisReg) then
                      begin
                        { Make sure the zero-expansion covers at least the minimum size (fixes i40003) }
                        if (MinSize = S_B) or
                          (not ShiftDownOverflow and (TryShiftDown = S_B)) or
                          ((MinSize = S_W) and (taicpu(hp1).opsize = S_WL)) then
                          begin
                            { Remove it completely! }
                            DebugMsg(SPeepholeOptimization + 'Movzx2Nop 2', hp1);

                            { Be careful; if p = hp1 and p was also removed, p
                              will become a dangling pointer }
                            if p = hp1 then
                              begin
                                RemoveCurrentp(p); { p = hp1 and will then become the next instruction }
                                p_removed := True;
                              end
                            else
                              RemoveInstruction(hp1);

                            hp1_removed := True;
                          end;
                      end
                    else
                      begin
                        DebugMsg(SPeepholeOptimization + 'Movzx2Mov 2', hp1);
                        taicpu(hp1).opcode := A_MOV;
                        taicpu(hp1).oper[0]^.reg := ThisReg;
                        taicpu(hp1).opsize := TargetSize;
                      end;
                  end
                else if (TargetSize = S_B) and (MaxSize = S_W) and (taicpu(hp1).opsize = S_WL) then
                  begin
                    { Need to change the size of the output }
                    DebugMsg(SPeepholeOptimization + 'movzwl2movzbl 2', hp1);
                    taicpu(hp1).oper[0]^.reg := ThisReg;
                    taicpu(hp1).opsize := S_BL;
                  end;
              end;
          end;

        function CompressInstructions: Boolean;
          var
            LocalIndex: Integer;
          begin
            Result := False;

            { The objective here is to try to find a combination that
              removes one of the MOV/Z instructions. }

            if (
                (taicpu(p).oper[0]^.typ <> top_reg) or
                not SuperRegistersEqual(taicpu(p).oper[0]^.reg, ThisReg)
              ) and
              (taicpu(hp1).oper[1]^.typ = top_reg) and
              SuperRegistersEqual(taicpu(hp1).oper[1]^.reg, ThisReg) then
              begin
                { Make a preference to remove the second MOVZX instruction }
                case taicpu(hp1).opsize of
                  S_BL, S_WL:
                    begin
                      TargetSize := S_L;
                      TargetSubReg := R_SUBD;
                    end;

                  S_BW:
                    begin
                      TargetSize := S_W;
                      TargetSubReg := R_SUBW;
                    end;

                  else
                    InternalError(2020112302);
                end;

              end
            else
              begin

                if LowerUnsignedOverflow and not UpperUnsignedOverflow then
                  begin
                    { Exceeded lower bound but not upper bound }
                    TargetSize := MaxSize;
                  end
                else if not LowerUnsignedOverflow then
                  begin
                    { Size didn't exceed lower bound }
                    TargetSize := MinSize;
                  end
                else
                  Exit;

              end;

            case TargetSize of
              S_B:
                TargetSubReg := R_SUBL;
              S_W:
                TargetSubReg := R_SUBW;
              S_L:
                TargetSubReg := R_SUBD;
              else
                InternalError(2020112350);
            end;

            { Update the register to its new size }
            setsubreg(ThisReg, TargetSubReg);
            RegInUse := False;

            if not SuperRegistersEqual(taicpu(hp1).oper[1]^.reg, ThisReg) then
              begin

                { Check to see if the active register is used afterwards;
                  if not, we can change it and make a saving. }

                TransferUsedRegs(TmpUsedRegs);

                { The target register may be marked as in use to cross
                  a jump to a distant label, so exclude it }
                ExcludeRegFromUsedRegs(taicpu(hp1).oper[1]^.reg, TmpUsedRegs);

                hp2 := p;
                repeat

                  { Explicitly check for the excluded register (don't include the first
                    instruction as it may be reading from here }
                  if ((p <> hp2) and (RegInInstruction(taicpu(hp1).oper[1]^.reg, hp2))) or
                    RegInUsedRegs(taicpu(hp1).oper[1]^.reg, TmpUsedRegs) then
                    begin
                      RegInUse := True;
                      Break;
                    end;

                  UpdateUsedRegs(TmpUsedRegs, tai(hp2.next));

                  if not GetNextInstruction(hp2, hp2) then
                    InternalError(2020112340);

                until (hp2 = hp1);

                if not RegInUse and RegUsedAfterInstruction(ThisReg, hp1, TmpUsedRegs) then
                  { We might still be able to get away with this }
                  RegInUse := not
                    (
                      GetNextInstructionUsingReg(hp1, hp2, ThisReg) and
                      (hp2.typ = ait_instruction) and
                      (
                        { Under -O1 and -O2, GetNextInstructionUsingReg may return an
                          instruction that doesn't actually contain ThisReg }
                        (cs_opt_level3 in current_settings.optimizerswitches) or
                        RegInInstruction(ThisReg, hp2)
                      ) and
                      RegLoadedWithNewValue(ThisReg, hp2)
                    );

                if not RegInUse then
                  begin
                    { Force the register size to the same as this instruction so it can be removed}
                    if (taicpu(hp1).opsize in [S_L, S_BL, S_WL]) then
                      begin
                        TargetSize := S_L;
                        TargetSubReg := R_SUBD;
                      end
                    else if (taicpu(hp1).opsize in [S_W, S_BW]) then
                      begin
                        TargetSize := S_W;
                        TargetSubReg := R_SUBW;
                      end;

                    ThisReg := taicpu(hp1).oper[1]^.reg;
                    setsubreg(ThisReg, TargetSubReg);
                    RegChanged := True;

                    DebugMsg(SPeepholeOptimization + 'Simplified register usage so ' + debug_regname(ThisReg) + ' = ' + debug_regname(taicpu(p).oper[1]^.reg), p);

                    TransferUsedRegs(TmpUsedRegs);
                    AllocRegBetween(ThisReg, p, hp1, TmpUsedRegs);

                    DebugMsg(SPeepholeOptimization + 'Movzx2Nop 3', hp1);
                    if p = hp1 then
                      begin
                        RemoveCurrentp(p); { p = hp1 and will then become the next instruction }
                        p_removed := True;
                      end
                    else
                      RemoveInstruction(hp1);

                    hp1_removed := True;

                    { Instruction will become "mov %reg,%reg" }
                    if not p_removed and (taicpu(p).opcode = A_MOV) and
                      MatchOperand(taicpu(p).oper[0]^, ThisReg) then
                      begin
                        DebugMsg(SPeepholeOptimization + 'Movzx2Nop 6', p);
                        RemoveCurrentP(p);
                        p_removed := True;
                      end
                    else
                      taicpu(p).oper[1]^.reg := ThisReg;

                    Result := True;
                  end
                else
                  begin
                    if TargetSize <> MaxSize then
                      begin
                        { Since the register is in use, we have to force it to
                          MaxSize otherwise part of it may become undefined later on }
                        TargetSize := MaxSize;

                        case TargetSize of
                          S_B:
                            TargetSubReg := R_SUBL;
                          S_W:
                            TargetSubReg := R_SUBW;
                          S_L:
                            TargetSubReg := R_SUBD;
                          else
                            InternalError(2020112351);
                        end;

                        setsubreg(ThisReg, TargetSubReg);
                      end;

                    AdjustFinalLoad;
                  end;
              end
            else
              AdjustFinalLoad;

            Result := AdjustInitialLoadAndSize or Result;

            { Now go through every instruction we found and change the
              size. If TargetSize = MaxSize, then almost no changes are
              needed and Result can remain False if it hasn't been set
              yet.

              If RegChanged is True, then the register requires changing
              and so the point about TargetSize = MaxSize doesn't apply. }

            if ((TargetSize <> MaxSize) or RegChanged) and (InstrMax >= 0) then
              begin
                for LocalIndex := 0 to InstrMax do
                  begin

                    { If p_removed is true, then the original MOV/Z was removed
                      and removing the AND instruction may not be safe if it
                      appears first }
                    if (InstrList[LocalIndex].oper[InstrList[LocalIndex].ops - 1]^.typ <> top_reg) then
                      InternalError(2020112310);

                    if InstrList[LocalIndex].oper[0]^.typ = top_reg then
                      InstrList[LocalIndex].oper[0]^.reg := ThisReg;

                    InstrList[LocalIndex].oper[InstrList[LocalIndex].ops - 1]^.reg := ThisReg;
                    InstrList[LocalIndex].opsize := TargetSize;
                  end;

                Result := True;
              end;
          end;

      begin
        Result := False;
        p_removed := False;
        hp1_removed := False;
        ThisReg := taicpu(p).oper[1]^.reg;

        { Check for:
            movs/z   ###,%ecx (or %cx or %rcx)
            ...
            shl/shr/sar/rcl/rcr/ror/rol  %cl,###
            (dealloc %ecx)

          Change to:
            mov      ###,%cl (if ### = %cl, then remove completely)
            ...
            shl/shr/sar/rcl/rcr/ror/rol  %cl,###
        }

        if (getsupreg(ThisReg) = RS_ECX) and
          GetNextInstructionUsingReg(p, hp1, NR_ECX) and
          (hp1.typ = ait_instruction) and
          (
            { Under -O1 and -O2, GetNextInstructionUsingReg may return an
              instruction that doesn't actually contain ECX }
            (cs_opt_level3 in current_settings.optimizerswitches) or
            RegInInstruction(NR_ECX, hp1) or
            (
              { It's common for the shift/rotate's read/write register to be
                initialised in between, so under -O2 and under, search ahead
                one more instruction
              }
              GetNextInstruction(hp1, hp1) and
              (hp1.typ = ait_instruction) and
              RegInInstruction(NR_ECX, hp1)
            )
          ) and
          MatchInstruction(hp1, [A_SHL, A_SHR, A_SAR, A_ROR, A_ROL, A_RCR, A_RCL], []) and
          (taicpu(hp1).oper[0]^.typ = top_reg) { This is enough to determine that it's %cl } then
          begin
            TransferUsedRegs(TmpUsedRegs);
            hp2 := p;
            repeat
              UpdateUsedRegs(TmpUsedRegs, tai(hp2.Next));
            until not GetNextInstruction(hp2, hp2) or (hp2 = hp1);

            if not RegUsedAfterInstruction(NR_CL, hp1, TmpUsedRegs) then
              begin

                case taicpu(p).opsize of
                  S_BW, S_BL{$ifdef x86_64}, S_BQ{$endif x86_64}:
                    if MatchOperand(taicpu(p).oper[0]^, NR_CL) then
                      begin
                        DebugMsg(SPeepholeOptimization + 'MovxOp2Op 3a', p);
                        RemoveCurrentP(p);
                      end
                    else
                      begin
                        taicpu(p).opcode := A_MOV;
                        taicpu(p).opsize := S_B;
                        taicpu(p).oper[1]^.reg := NR_CL;
                        DebugMsg(SPeepholeOptimization + 'MovxOp2MovOp 1', p);
                      end;
                  S_WL{$ifdef x86_64}, S_WQ{$endif x86_64}:
                    if MatchOperand(taicpu(p).oper[0]^, NR_CX) then
                      begin
                        DebugMsg(SPeepholeOptimization + 'MovxOp2Op 3b', p);
                        RemoveCurrentP(p);
                      end
                    else
                      begin
                        taicpu(p).opcode := A_MOV;
                        taicpu(p).opsize := S_W;
                        taicpu(p).oper[1]^.reg := NR_CX;
                        DebugMsg(SPeepholeOptimization + 'MovxOp2MovOp 2', p);
                      end;
{$ifdef x86_64}
                  S_LQ:
                    if MatchOperand(taicpu(p).oper[0]^, NR_ECX) then
                      begin
                        DebugMsg(SPeepholeOptimization + 'MovxOp2Op 3c', p);
                        RemoveCurrentP(p);
                      end
                    else
                      begin
                        taicpu(p).opcode := A_MOV;
                        taicpu(p).opsize := S_L;
                        taicpu(p).oper[1]^.reg := NR_ECX;
                        DebugMsg(SPeepholeOptimization + 'MovxOp2MovOp 3', p);
                      end;
{$endif x86_64}
                  else
                    InternalError(2021120401);
                end;

                Result := True;
                Exit;
              end;
          end;

        { This is anything but quick! }
        if not(cs_opt_level2 in current_settings.optimizerswitches) then
          Exit;

        SetLength(InstrList, 0);
        InstrMax := -1;

        case taicpu(p).opsize of
          S_BW, S_BL{$ifdef x86_64}, S_BQ{$endif x86_64}:
            begin
{$if defined(i386) or defined(i8086)}
              { If the target size is 8-bit, make sure we can actually encode it }
              if not (GetSupReg(ThisReg) in [RS_EAX,RS_EBX,RS_ECX,RS_EDX]) then
                Exit;
{$endif i386 or i8086}

              LowerLimit := $FF;
              SignedLowerLimit := $7F;
              SignedLowerLimitBottom := -128;
              MinSize := S_B;
              if taicpu(p).opsize = S_BW then
                begin
                  MaxSize := S_W;
                  UpperLimit := $FFFF;
                  SignedUpperLimit := $7FFF;
                  SignedUpperLimitBottom := -32768;
                end
              else
                begin
                  { Keep at a 32-bit limit for BQ as well since one can't really optimise otherwise }
                  MaxSize := S_L;
                  UpperLimit := $FFFFFFFF;
                  SignedUpperLimit := $7FFFFFFF;
                  SignedUpperLimitBottom := -2147483648;
                end;
            end;
          S_WL{$ifdef x86_64}, S_WQ{$endif x86_64}:
            begin
              { Keep at a 32-bit limit for WQ as well since one can't really optimise otherwise }
              LowerLimit := $FFFF;
              SignedLowerLimit := $7FFF;
              SignedLowerLimitBottom := -32768;
              UpperLimit := $FFFFFFFF;
              SignedUpperLimit := $7FFFFFFF;
              SignedUpperLimitBottom := -2147483648;
              MinSize := S_W;
              MaxSize := S_L;
            end;
{$ifdef x86_64}
          S_LQ:
            begin
              { Both the lower and upper limits are set to 32-bit.  If a limit
                is breached, then optimisation is impossible }
              LowerLimit := $FFFFFFFF;
              SignedLowerLimit := $7FFFFFFF;
              SignedLowerLimitBottom := -2147483648;
              UpperLimit := $FFFFFFFF;
              SignedUpperLimit := $7FFFFFFF;
              SignedUpperLimitBottom := -2147483648;
              MinSize := S_L;
              MaxSize := S_L;
            end;
{$endif x86_64}
          else
            InternalError(2020112301);
        end;

        TestValMin := 0;
        TestValMax := LowerLimit;
        TestValSignedMax := SignedLowerLimit;
        TryShiftDownLimit := LowerLimit;
        TryShiftDown := S_NO;
        ShiftDownOverflow := False;
        RegChanged := False;
        BitwiseOnly := True;
        OrXorUsed := False;
        UpperSignedOverflow := False;
        LowerSignedOverflow := False;
        UpperUnsignedOverflow := False;
        LowerUnsignedOverflow := False;

        hp1 := p;

        while GetNextInstructionUsingReg(hp1, hp1, ThisReg) and
          (hp1.typ = ait_instruction) and
          (
            { Under -O1 and -O2, GetNextInstructionUsingReg may return an
              instruction that doesn't actually contain ThisReg }
            (cs_opt_level3 in current_settings.optimizerswitches) or
            { This allows this Movx optimisation to work through the SETcc instructions
              inserted by the 'CMP/JE/CMP/@Lbl/SETE -> CMP/SETE/CMP/SETE/OR'
              optimisation on -O1 and -O2 (on -O3, GetNextInstructionUsingReg will
              skip over these SETcc instructions). }
            (taicpu(hp1).opcode = A_SETcc) or
            RegInInstruction(ThisReg, hp1)
          ) do
          begin

            case taicpu(hp1).opcode of
              A_INC,A_DEC:
                begin
                  { Has to be an exact match on the register }
                  if not MatchOperand(taicpu(hp1).oper[0]^, ThisReg) then
                    Break;

                  if taicpu(hp1).opcode = A_INC then
                    begin
                      Inc(TestValMin);
                      Inc(TestValMax);
                      Inc(TestValSignedMax);
                    end
                  else
                    begin
                      Dec(TestValMin);
                      Dec(TestValMax);
                      Dec(TestValSignedMax);
                    end;
                end;

              A_TEST, A_CMP:
                begin
                  if (
                      { Too high a risk of non-linear behaviour that breaks DFA
                        here, unless it's cmp $0,%reg, which is equivalent to
                        test %reg,%reg }
                      OrXorUsed and
                      (taicpu(hp1).opcode = A_CMP) and
                      not Matchoperand(taicpu(hp1).oper[0]^, 0)
                    ) or
                    (taicpu(hp1).oper[1]^.typ <> top_reg) or
                    { Has to be an exact match on the register }
                    (taicpu(hp1).oper[1]^.reg <> ThisReg) or
                    (
                      { Permit "test %reg,%reg" }
                      (taicpu(hp1).opcode = A_TEST) and
                      (taicpu(hp1).oper[0]^.typ = top_reg) and
                      (taicpu(hp1).oper[0]^.reg <> ThisReg)
                    ) or
                    (taicpu(hp1).oper[0]^.typ <> top_const) or
                    { Make sure the comparison value is not smaller than the
                      smallest allowed signed value for the minimum size (e.g.
                      -128 for 8-bit) }
                    not (
                      ((taicpu(hp1).oper[0]^.val and LowerLimit) = taicpu(hp1).oper[0]^.val) or
                      { Is it in the negative range? }
                      (
                        (taicpu(hp1).oper[0]^.val < 0) and
                        (taicpu(hp1).oper[0]^.val >= SignedLowerLimitBottom)
                      )
                    ) then
                    Break;

                  { Check to see if the active register is used afterwards }
                  TransferUsedRegs(TmpUsedRegs);
                  IncludeRegInUsedRegs(ThisReg, TmpUsedRegs);
                  if not RegUsedAfterInstruction(ThisReg, hp1, TmpUsedRegs) then
                    begin
                      { Make sure the comparison or any previous instructions
                        hasn't pushed the test values outside of the range of
                        MinSize }
                      if LowerUnsignedOverflow and not UpperUnsignedOverflow then
                        begin
                          { Exceeded lower bound but not upper bound }
                          Exit;
                        end
                      else if not LowerSignedOverflow or not LowerUnsignedOverflow then
                        begin
                          { Size didn't exceed lower bound }
                          TargetSize := MinSize;
                        end
                      else
                        Break;

                      case TargetSize of
                        S_B:
                          TargetSubReg := R_SUBL;
                        S_W:
                          TargetSubReg := R_SUBW;
                        S_L:
                          TargetSubReg := R_SUBD;
                        else
                          InternalError(2021051002);
                      end;

		      if TargetSize <> MaxSize then
		        begin
                          { Update the register to its new size }
                          setsubreg(ThisReg, TargetSubReg);

                          DebugMsg(SPeepholeOptimization + 'CMP instruction resized thanks to register size optimisation (see MOV/Z assignment above)', hp1);
                          taicpu(hp1).oper[1]^.reg := ThisReg;
                          taicpu(hp1).opsize := TargetSize;

                          { Convert the input MOVZX to a MOV if necessary }
                          AdjustInitialLoadAndSize;

                          if (InstrMax >= 0) then
                            begin
                              for Index := 0 to InstrMax do
                                 begin

                                  { If p_removed is true, then the original MOV/Z was removed
                                    and removing the AND instruction may not be safe if it
                                    appears first }
                                  if (InstrList[Index].oper[InstrList[Index].ops - 1]^.typ <> top_reg) then
                                    InternalError(2020112311);

                                  if InstrList[Index].oper[0]^.typ = top_reg then
                                    InstrList[Index].oper[0]^.reg := ThisReg;

                                  InstrList[Index].oper[InstrList[Index].ops - 1]^.reg := ThisReg;
                                  InstrList[Index].opsize := MinSize;
                                end;
                            end;

                          Result := True;
                        end;
                      Exit;
                    end;
                end;
              A_SETcc:
                begin
                  { This allows this Movx optimisation to work through the SETcc instructions
                    inserted by the 'CMP/JE/CMP/@Lbl/SETE -> CMP/SETE/CMP/SETE/OR'
                    optimisation on -O1 and -O2 (on -O3, GetNextInstructionUsingReg will
                    skip over these SETcc instructions). }
                  if (cs_opt_level3 in current_settings.optimizerswitches) or
                    { Of course, break out if the current register is used }
                    RegInOp(ThisReg, taicpu(hp1).oper[0]^) then
                    Break
                  else
                    { We must use Continue so the instruction doesn't get added
                      to InstrList }
                    Continue;
                end;

              A_ADD,A_SUB,A_AND,A_OR,A_XOR,A_SHL,A_SHR,A_SAR:
                begin
                  if
                    (taicpu(hp1).oper[1]^.typ <> top_reg) or
                    { Has to be an exact match on the register }
                    (taicpu(hp1).oper[1]^.reg <> ThisReg) or not
                    (
                      (
                        (taicpu(hp1).oper[0]^.typ = top_const) and
                        (
                          (
                            (taicpu(hp1).opcode = A_SHL) and
                            (
                              ((MinSize = S_B) and (taicpu(hp1).oper[0]^.val < 8)) or
                              ((MinSize = S_W) and (taicpu(hp1).oper[0]^.val < 16)) or
                              ((MinSize = S_L) and (taicpu(hp1).oper[0]^.val < 32))
                            )
                          ) or (
                            (taicpu(hp1).opcode <> A_SHL) and
                            (
                              ((taicpu(hp1).oper[0]^.val and UpperLimit) = taicpu(hp1).oper[0]^.val) or
                              { Is it in the negative range? }
                              (((not taicpu(hp1).oper[0]^.val) and (UpperLimit shr 1)) = (not taicpu(hp1).oper[0]^.val))
                            )
                          )
                        )
                      ) or (
                        MatchOperand(taicpu(hp1).oper[0]^, taicpu(hp1).oper[1]^.reg) and
                        ((taicpu(hp1).opcode = A_ADD) or (taicpu(hp1).opcode = A_AND) or (taicpu(hp1).opcode = A_SUB))
                      )
                    ) then
                    Break;

                  { Only process OR and XOR if there are only bitwise operations,
                    since otherwise they can too easily fool the data flow
                    analysis (they can cause non-linear behaviour) }

                  case taicpu(hp1).opcode of
                    A_ADD:
                      begin
                        if OrXorUsed then
                          { Too high a risk of non-linear behaviour that breaks DFA here }
                          Break
                        else
                          BitwiseOnly := False;

                        if (taicpu(hp1).oper[0]^.typ = top_reg) then
                          begin
                            TestValMin := TestValMin * 2;
                            TestValMax := TestValMax * 2;
                            TestValSignedMax := TestValSignedMax * 2;
                          end
                        else
                          begin
                            WorkingValue := taicpu(hp1).oper[0]^.val;
                            TestValMin := TestValMin + WorkingValue;
                            TestValMax := TestValMax + WorkingValue;
                            TestValSignedMax := TestValSignedMax + WorkingValue;
                          end;
                      end;
                    A_SUB:
                      begin
                        if (taicpu(hp1).oper[0]^.typ = top_reg) then
                          begin
                            TestValMin := 0;
                            TestValMax := 0;
                            TestValSignedMax := 0;
                          end
                        else
                          begin
                            if OrXorUsed then
                              { Too high a risk of non-linear behaviour that breaks DFA here }
                              Break
                            else
                              BitwiseOnly := False;

                            WorkingValue := taicpu(hp1).oper[0]^.val;
                            TestValMin := TestValMin - WorkingValue;
                            TestValMax := TestValMax - WorkingValue;
                            TestValSignedMax := TestValSignedMax - WorkingValue;
                          end;
                      end;
                    A_AND:
                      if (taicpu(hp1).oper[0]^.typ = top_const) then
                        begin
                          { we might be able to go smaller if AND appears first }
                          if InstrMax = -1 then
                            case MinSize of
                              S_B:
                                ;
                              S_W:
                                if ((taicpu(hp1).oper[0]^.val and $FF) = taicpu(hp1).oper[0]^.val) or
                                  ((not(taicpu(hp1).oper[0]^.val) and $7F) = (not taicpu(hp1).oper[0]^.val)) then
                                  begin
                                    TryShiftDown := S_B;
                                    TryShiftDownLimit := $FF;
                                  end;
                              S_L:
                                if ((taicpu(hp1).oper[0]^.val and $FF) = taicpu(hp1).oper[0]^.val) or
                                  ((not(taicpu(hp1).oper[0]^.val) and $7F) = (not taicpu(hp1).oper[0]^.val)) then
                                  begin
                                    TryShiftDown := S_B;
                                    TryShiftDownLimit := $FF;
                                  end
                                else if ((taicpu(hp1).oper[0]^.val and $FFFF) = taicpu(hp1).oper[0]^.val) or
                                  ((not(taicpu(hp1).oper[0]^.val) and $7FFF) = (not taicpu(hp1).oper[0]^.val)) then
                                  begin
                                    TryShiftDown := S_W;
                                    TryShiftDownLimit := $FFFF;
                                  end;
                              else
                                InternalError(2020112320);
                            end;

                          WorkingValue := taicpu(hp1).oper[0]^.val;
                          TestValMin := TestValMin and WorkingValue;
                          TestValMax := TestValMax and WorkingValue;
                          TestValSignedMax := TestValSignedMax and WorkingValue;
                        end;
                    A_OR:
                      begin
                        if not BitwiseOnly then
                          Break;

                        OrXorUsed := True;

                        WorkingValue := taicpu(hp1).oper[0]^.val;
                        TestValMin := TestValMin or WorkingValue;
                        TestValMax := TestValMax or WorkingValue;
                        TestValSignedMax := TestValSignedMax or WorkingValue;
                      end;
                    A_XOR:
                      begin
                        if (taicpu(hp1).oper[0]^.typ = top_reg) then
                          begin
                            TestValMin := 0;
                            TestValMax := 0;
                            TestValSignedMax := 0;
                          end
                        else
                          begin
                            if not BitwiseOnly then
                              Break;

                            OrXorUsed := True;

                            WorkingValue := taicpu(hp1).oper[0]^.val;
                            TestValMin := TestValMin xor WorkingValue;
                            TestValMax := TestValMax xor WorkingValue;
                            TestValSignedMax := TestValSignedMax xor WorkingValue;
                          end;
                      end;
                    A_SHL:
                      begin
                        BitwiseOnly := False;

                        WorkingValue := taicpu(hp1).oper[0]^.val;
                        TestValMin := TestValMin shl WorkingValue;
                        TestValMax := TestValMax shl WorkingValue;
                        TestValSignedMax := TestValSignedMax shl WorkingValue;
                      end;
                    A_SHR,
                    { The first instruction was MOVZX, so the value won't be negative }
                    A_SAR:
                      begin
                        if InstrMax <> -1 then
                          BitwiseOnly := False
                        else
                          { we might be able to go smaller if SHR appears first }
                          case MinSize of
                            S_B:
                              ;
                            S_W:
                              if (taicpu(hp1).oper[0]^.val >= 8) then
                                begin
                                  TryShiftDown := S_B;
                                  TryShiftDownLimit := $FF;
                                  TryShiftDownSignedLimit := $7F;
                                  TryShiftDownSignedLimitLower := -128;
                                end;
                            S_L:
                              if (taicpu(hp1).oper[0]^.val >= 24) then
                                begin
                                  TryShiftDown := S_B;
                                  TryShiftDownLimit := $FF;
                                  TryShiftDownSignedLimit := $7F;
                                  TryShiftDownSignedLimitLower := -128;
                                end
                              else if (taicpu(hp1).oper[0]^.val >= 16) then
                                begin
                                  TryShiftDown := S_W;
                                  TryShiftDownLimit := $FFFF;
                                  TryShiftDownSignedLimit := $7FFF;
                                  TryShiftDownSignedLimitLower := -32768;
                                end;
                            else
                              InternalError(2020112321);
                          end;

                        WorkingValue := taicpu(hp1).oper[0]^.val;
                        if taicpu(hp1).opcode = A_SAR then
                          begin
                            TestValMin := SarInt64(TestValMin, WorkingValue);
                            TestValMax := SarInt64(TestValMax, WorkingValue);
                            TestValSignedMax := SarInt64(TestValSignedMax, WorkingValue);
                          end
                        else
                          begin
                            TestValMin := TestValMin shr WorkingValue;
                            TestValMax := TestValMax shr WorkingValue;
                            TestValSignedMax := TestValSignedMax shr WorkingValue;
                          end;
                      end;
                    else
                      InternalError(2020112303);
                  end;
                end;
(*
              A_IMUL:
                case taicpu(hp1).ops of
                  2:
                    begin
                      if not MatchOpType(hp1, top_reg, top_reg) or
                        { Has to be an exact match on the register }
                        (taicpu(hp1).oper[0]^.reg <> ThisReg) or
                        (taicpu(hp1).oper[1]^.reg <> ThisReg) then
                        Break;

                      TestValMin := TestValMin * TestValMin;
                      TestValMax := TestValMax * TestValMax;
                      TestValSignedMax := TestValSignedMax * TestValMax;
                    end;
                  3:
                    begin
                      if not MatchOpType(hp1, top_const, top_reg, top_reg) or
                        { Has to be an exact match on the register }
                        (taicpu(hp1).oper[1]^.reg <> ThisReg) or
                        (taicpu(hp1).oper[2]^.reg <> ThisReg) or
                        ((taicpu(hp1).oper[0]^.val and UpperLimit) = taicpu(hp1).oper[0]^.val) or
                        { Is it in the negative range? }
                        (((not taicpu(hp1).oper[0]^.val) and (UpperLimit shr 1)) = (not taicpu(hp1).oper[0]^.val)) then
                        Break;

                      TestValMin := TestValMin * taicpu(hp1).oper[0]^.val;
                      TestValMax := TestValMax * taicpu(hp1).oper[0]^.val;
                      TestValSignedMax := TestValSignedMax * taicpu(hp1).oper[0]^.val;
                    end;
                  else
                    Break;
                end;

              A_IDIV:
                case taicpu(hp1).ops of
                  3:
                    begin
                      if not MatchOpType(hp1, top_const, top_reg, top_reg) or
                        { Has to be an exact match on the register }
                        (taicpu(hp1).oper[1]^.reg <> ThisReg) or
                        (taicpu(hp1).oper[2]^.reg <> ThisReg) or
                        ((taicpu(hp1).oper[0]^.val and UpperLimit) = taicpu(hp1).oper[0]^.val) or
                        { Is it in the negative range? }
                        (((not taicpu(hp1).oper[0]^.val) and (UpperLimit shr 1)) = (not taicpu(hp1).oper[0]^.val)) then
                        Break;

                      TestValMin := TestValMin div taicpu(hp1).oper[0]^.val;
                      TestValMax := TestValMax div taicpu(hp1).oper[0]^.val;
                      TestValSignedMax := TestValSignedMax div taicpu(hp1).oper[0]^.val;
                    end;
                  else
                    Break;
                end;
*)
              A_MOVSX{$ifdef x86_64}, A_MOVSXD{$endif x86_64}:
                begin
                  { If there are no instructions in between, then we might be able to make a saving }
                  if UpperSignedOverflow or (taicpu(hp1).oper[0]^.typ <> top_reg) or (taicpu(hp1).oper[0]^.reg <> ThisReg) then
                    Break;

                  { We have something like:
          	      movzbw %dl,%dx
                      ...
          	      movswl %dx,%edx

                    Change the latter to a zero-extension then enter the
                    A_MOVZX case branch.
                  }

{$ifdef x86_64}
                  if (taicpu(hp1).opsize = S_LQ) and SuperRegistersEqual(taicpu(hp1).oper[1]^.reg, ThisReg) then
                    begin
                      { this becomes a zero extension from 32-bit to 64-bit, but
                        the upper 32 bits are already zero, so just delete the
                        instruction }

                      DebugMsg(SPeepholeOptimization + 'MovzMovsxd2MovzNop', hp1);
                      RemoveInstruction(hp1);

                      Result := True;
                      Exit;
                    end
                  else
{$endif x86_64}
                    begin
                      DebugMsg(SPeepholeOptimization + 'MovzMovs2MovzMovz', hp1);
                      taicpu(hp1).opcode := A_MOVZX;
{$ifdef x86_64}
                      case taicpu(hp1).opsize of
                        S_BQ:
                          begin
                            taicpu(hp1).opsize := S_BL;
                            setsubreg(taicpu(hp1).oper[1]^.reg, R_SUBD);
                          end;
                        S_WQ:
                          begin
                            taicpu(hp1).opsize := S_WL;
                            setsubreg(taicpu(hp1).oper[1]^.reg, R_SUBD);
                          end;
                        S_LQ:
                          begin
                            taicpu(hp1).opcode := A_MOV;
                            taicpu(hp1).opsize := S_L;
                            setsubreg(taicpu(hp1).oper[1]^.reg, R_SUBD);

                            { In this instance, we need to break out because the
                              instruction is no longer MOVZX or MOVSXD }
                            Result := True;
                            Exit;
                          end;
                        else
                          ;
                      end;
{$endif x86_64}
                      Result := CompressInstructions;
                      Exit;
                    end;
                end;

              A_MOVZX:
                begin
                  if UpperUnsignedOverflow or (taicpu(hp1).oper[0]^.typ <> top_reg) then
                    Break;

                  if (InstrMax = -1) then
                    begin
                      if SuperRegistersEqual(taicpu(hp1).oper[0]^.reg, ThisReg) then
                        begin
                          { Optimise around i40003 }
                          if SuperRegistersEqual(taicpu(hp1).oper[1]^.reg, ThisReg) and
                            (taicpu(p).opsize = S_WL) and (taicpu(hp1).opsize = S_BL)
{$ifndef x86_64}
                            and (
                              (taicpu(p).oper[0]^.typ <> top_reg) or
                              { Cannot encode byte-sized ESI, EDI, EBP or ESP under i386 }
                              (GetSupReg(taicpu(p).oper[0]^.reg) in [RS_EAX, RS_EBX, RS_ECX, RS_EDX])
                            )
{$endif not x86_64}
                            then
                              begin
                                if (taicpu(p).oper[0]^.typ = top_reg) then
                                  setsubreg(taicpu(p).oper[0]^.reg, R_SUBL);

                                DebugMsg(SPeepholeOptimization + 'movzwl2movzbl 1', p);
                                taicpu(p).opsize := S_BL;

                                DebugMsg(SPeepholeOptimization + 'Movzx2Nop 2a', hp1);
                                RemoveInstruction(hp1);
                                Result := True;
                                Exit;
                              end;
                        end
                      else
                        begin
                          { Will return false if the second parameter isn't ThisReg
                            (can happen on -O2 and under) }
                          if Reg1WriteOverwritesReg2Entirely(taicpu(hp1).oper[1]^.reg, ThisReg) then
                            begin
                              { The two MOVZX instructions are adjacent, so remove the first one }
                              DebugMsg(SPeepholeOptimization + 'Movzx2Nop 5', p);
                              RemoveCurrentP(p);
                              Result := True;
                              Exit;
                            end;

                          Break;
                        end;
                    end;

                  Result := CompressInstructions;
                  Exit;
                end;

              else
                { This includes ADC, SBB and IDIV }
                Break;
            end;

            if not CheckOverflowConditions then
              Break;

            { Contains highest index (so instruction count - 1) }
            Inc(InstrMax);
            if InstrMax > High(InstrList) then
              SetLength(InstrList, InstrMax + LIST_STEP_SIZE);

            InstrList[InstrMax] := taicpu(hp1);
          end;
      end;
{$pop}

    function TX86AsmOptimizer.OptPass2Imul(var p : tai) : boolean;
      var
        hp1 : tai;
      begin
        Result:=false;
        if (taicpu(p).ops >= 2) and
           ((taicpu(p).oper[0]^.typ = top_const) or
            ((taicpu(p).oper[0]^.typ = top_ref) and (taicpu(p).oper[0]^.ref^.refaddr=addr_full))) and
           (taicpu(p).oper[1]^.typ = top_reg) and
           ((taicpu(p).ops = 2) or
            ((taicpu(p).oper[2]^.typ = top_reg) and
             (taicpu(p).oper[2]^.reg = taicpu(p).oper[1]^.reg))) and
           GetLastInstruction(p,hp1) and
           MatchInstruction(hp1,A_MOV,[]) and
           MatchOpType(taicpu(hp1),top_reg,top_reg) and
           (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
          begin
            TransferUsedRegs(TmpUsedRegs);
            if not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,p,TmpUsedRegs)) or
              ((taicpu(p).ops = 3) and (taicpu(p).oper[1]^.reg=taicpu(p).oper[2]^.reg)) then
              { change
                  mov reg1,reg2
                  imul y,reg2 to imul y,reg1,reg2 }
              begin
                taicpu(p).ops := 3;
                taicpu(p).loadreg(2,taicpu(p).oper[1]^.reg);
                taicpu(p).loadreg(1,taicpu(hp1).oper[0]^.reg);
                DebugMsg(SPeepholeOptimization + 'MovImul2Imul done',p);
                RemoveInstruction(hp1);
                result:=true;
              end;
          end;
      end;


    procedure TX86AsmOptimizer.ConvertJumpToRET(const p: tai; const ret_p: tai);
      var
        ThisLabel: TAsmLabel;
      begin
        ThisLabel := tasmlabel(taicpu(p).oper[0]^.ref^.symbol);
        ThisLabel.decrefs;
        taicpu(p).condition := C_None;
        taicpu(p).opcode := A_RET;
        taicpu(p).is_jmp := false;
        taicpu(p).ops := taicpu(ret_p).ops;
        case taicpu(ret_p).ops of
          0:
            taicpu(p).clearop(0);
          1:
            taicpu(p).loadconst(0,taicpu(ret_p).oper[0]^.val);
          else
            internalerror(2016041301);
        end;

        { If the original label is now dead, it might turn out that the label
          immediately follows p.  As a result, everything beyond it, which will
          be just some final register configuration and a RET instruction, is
          now dead code. [Kit] }

        { NOTE: This is much faster than introducing a OptPass2RET routine and
          running RemoveDeadCodeAfterJump for each RET instruction, because
          this optimisation rarely happens and most RETs appear at the end of
          routines where there is nothing that can be stripped. [Kit] }
        if not ThisLabel.is_used then
          RemoveDeadCodeAfterJump(p);
      end;


    function TX86AsmOptimizer.OptPass2SETcc(var p: tai): boolean;
      var
        hp1,hp2,next: tai; SetC, JumpC: TAsmCond;
        Unconditional, PotentialModified: Boolean;
        OperPtr: POper;

        NewRef: TReference;

        InstrList: array of taicpu;
        InstrMax, Index: Integer;
      const
{$ifdef DEBUG_AOPTCPU}
        SNoFlags: shortstring = ' so the flags aren''t modified';
{$else DEBUG_AOPTCPU}
        SNoFlags = '';
{$endif DEBUG_AOPTCPU}
      begin
        Result:=false;

        if MatchOpType(taicpu(p),top_reg) and GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) then
          begin
            if MatchInstruction(hp1, A_TEST, [S_B]) and
              MatchOpType(taicpu(hp1),top_reg,top_reg) and
              (taicpu(hp1).oper[0]^.reg = taicpu(hp1).oper[1]^.reg) and
              (taicpu(p).oper[0]^.reg = taicpu(hp1).oper[1]^.reg) and
              GetNextInstruction(hp1, hp2) and
              MatchInstruction(hp2, A_Jcc, A_SETcc, []) then
              { Change from:             To:

                set(C) %reg              j(~C) label
                test   %reg,%reg/cmp $0,%reg
                je     label


                set(C) %reg              j(C)  label
                test   %reg,%reg/cmp $0,%reg
                jne    label

                (Also do something similar with sete/setne instead of je/jne)
              }
              begin
                { Before we do anything else, we need to check the instructions
                  in between SETcc and TEST to make sure they don't modify the
                  FLAGS register - if -O2 or under, there won't be any
                  instructions between SET and TEST }

                TransferUsedRegs(TmpUsedRegs);
                UpdateUsedRegs(TmpUsedRegs, tai(p.next));

                if (cs_opt_level3 in current_settings.optimizerswitches) then
                  begin
                    next := p;
                    SetLength(InstrList, 0);
                    InstrMax := -1;
                    PotentialModified := False;

                    { Make a note of every instruction that modifies the FLAGS
                      register }
                    while GetNextInstruction(next, next) and (next <> hp1) do
                      begin
                        if next.typ <> ait_instruction then
                          { GetNextInstructionUsingReg should have returned False }
                          InternalError(2021051701);

                        if RegModifiedByInstruction(NR_DEFAULTFLAGS, next) then
                          begin
                            case taicpu(next).opcode of
                              A_SETcc,
                              A_CMOVcc,
                              A_Jcc:
                                begin
                                  if PotentialModified then
                                    { Not safe because the flags were modified earlier }
                                    Exit
                                  else
                                    { Condition is the same as the initial SETcc, so this is safe
                                      (don't add to instruction list though) }
                                    Continue;
                                end;
                              A_ADD:
                                begin
                                  if (taicpu(next).opsize = S_B) or
                                    { LEA doesn't support 8-bit operands }
                                    (taicpu(next).oper[1]^.typ <> top_reg) or
                                    { Must write to a register }
                                    (taicpu(next).oper[0]^.typ = top_ref) then
                                    { Require a constant or a register }
                                    Exit;

                                  PotentialModified := True;
                                end;
                              A_SUB:
                                begin
                                  if (taicpu(next).opsize = S_B) or
                                    { LEA doesn't support 8-bit operands }
                                    (taicpu(next).oper[1]^.typ <> top_reg) or
                                    { Must write to a register }
                                    (taicpu(next).oper[0]^.typ <> top_const) or
                                    (taicpu(next).oper[0]^.val = $80000000) then
                                    { Can't subtract a register with LEA - also
                                      check that the value isn't -2^31, as this
                                      can't be negated }
                                    Exit;

                                  PotentialModified := True;
                                end;
                              A_SAL,
                              A_SHL:
                                begin
                                  if (taicpu(next).opsize = S_B) or
                                    { LEA doesn't support 8-bit operands }
                                    (taicpu(next).oper[1]^.typ <> top_reg) or
                                    { Must write to a register }
                                    (taicpu(next).oper[0]^.typ <> top_const) or
                                    (taicpu(next).oper[0]^.val < 0) or
                                    (taicpu(next).oper[0]^.val > 3) then
                                      Exit;

                                  PotentialModified := True;
                                end;
                              A_IMUL:
                                begin
                                  if (taicpu(next).ops <> 3) or
                                    (taicpu(next).oper[1]^.typ <> top_reg) or
                                    { Must write to a register }
                                    (taicpu(next).oper[2]^.val in [2,3,4,5,8,9]) then
                                    { We can convert "imul x,%reg1,%reg2" (where x = 2, 4 or 8)
                                      to "lea (%reg1,x),%reg2".  If x = 3, 5  or 9, we can
                                      change this to "lea (%reg1,%reg1,(x-1)),%reg2" }
                                    Exit
                                  else
                                    PotentialModified := True;
                                end;
                              else
                                { Don't know how to change this, so abort }
                                Exit;
                            end;

                            { Contains highest index (so instruction count - 1) }
                            Inc(InstrMax);
                            if InstrMax > High(InstrList) then
                              SetLength(InstrList, InstrMax + LIST_STEP_SIZE);

                            InstrList[InstrMax] := taicpu(next);
                          end;
                        UpdateUsedRegs(TmpUsedRegs, tai(next.next));
                      end;

                    if not Assigned(next) or (next <> hp1) then
                      { It should be equal to hp1 }
                      InternalError(2021051702);

                    { Cycle through each instruction and check to see if we can
                      change them to versions that don't modify the flags }
                    if (InstrMax >= 0) then
                      begin
                        for Index := 0 to InstrMax do
                          case InstrList[Index].opcode of
                            A_ADD:
                              begin
                                DebugMsg(SPeepholeOptimization + 'ADD -> LEA' + SNoFlags, InstrList[Index]);
                                InstrList[Index].opcode := A_LEA;
                                reference_reset(NewRef, 1, []);
                                NewRef.base := InstrList[Index].oper[1]^.reg;

                                if InstrList[Index].oper[0]^.typ = top_reg then
                                  begin
                                    NewRef.index := InstrList[Index].oper[0]^.reg;
                                    NewRef.scalefactor := 1;
                                  end
                                else
                                  NewRef.offset := InstrList[Index].oper[0]^.val;

                                InstrList[Index].loadref(0, NewRef);
                              end;
                            A_SUB:
                              begin
                                DebugMsg(SPeepholeOptimization + 'SUB -> LEA' + SNoFlags, InstrList[Index]);
                                InstrList[Index].opcode := A_LEA;
                                reference_reset(NewRef, 1, []);
                                NewRef.base := InstrList[Index].oper[1]^.reg;
                                NewRef.offset := -InstrList[Index].oper[0]^.val;

                                InstrList[Index].loadref(0, NewRef);
                              end;
                            A_SHL,
                            A_SAL:
                              begin
                                DebugMsg(SPeepholeOptimization + 'SHL -> LEA' + SNoFlags, InstrList[Index]);
                                InstrList[Index].opcode := A_LEA;
                                reference_reset(NewRef, 1, []);
                                NewRef.index := InstrList[Index].oper[1]^.reg;
                                NewRef.scalefactor := 1 shl (InstrList[Index].oper[0]^.val);

                                InstrList[Index].loadref(0, NewRef);
                              end;
                            A_IMUL:
                              begin
                                DebugMsg(SPeepholeOptimization + 'IMUL -> LEA' + SNoFlags, InstrList[Index]);
                                InstrList[Index].opcode := A_LEA;
                                reference_reset(NewRef, 1, []);
                                NewRef.index := InstrList[Index].oper[1]^.reg;
                                case InstrList[Index].oper[0]^.val of
                                  2, 4, 8:
                                    NewRef.scalefactor := InstrList[Index].oper[0]^.val;
                                  else {3, 5 and 9}
                                    begin
                                      NewRef.scalefactor := InstrList[Index].oper[0]^.val - 1;
                                      NewRef.base := InstrList[Index].oper[1]^.reg;
                                    end;
                                end;

                                InstrList[Index].loadref(0, NewRef);
                              end;
                            else
                              InternalError(2021051710);
                          end;

                      end;

                    { Mark the FLAGS register as used across this whole block }
                    AllocRegBetween(NR_DEFAULTFLAGS, p, hp1, UsedRegs);
                  end;

                UpdateUsedRegs(TmpUsedRegs, tai(hp1.next));

                JumpC := taicpu(hp2).condition;
                Unconditional := False;

                if conditions_equal(JumpC, C_E) then
                  SetC := inverse_cond(taicpu(p).condition)
                else if conditions_equal(JumpC, C_NE) then
                  SetC := taicpu(p).condition
                else
                  { We've got something weird here (and inefficent) }
                  begin
                    DebugMsg('DEBUG: Inefficient jump - check code generation', p);
                    SetC := C_NONE;

                    { JAE/JNB will always branch (use 'condition_in', since C_AE <> C_NB normally) }
                    if condition_in(C_AE, JumpC) then
                      Unconditional := True
                    else
                      { Not sure what to do with this jump - drop out }
                      Exit;
                  end;

                RemoveInstruction(hp1);

                if Unconditional then
                  MakeUnconditional(taicpu(hp2))
                else
                  begin
                    if SetC = C_NONE then
                      InternalError(2018061402);

                    taicpu(hp2).SetCondition(SetC);
                  end;

                { as hp2 is a jump, we cannot use RegUsedAfterInstruction but we have to check if it is included in
                  TmpUsedRegs }
                if not TmpUsedRegs[getregtype(taicpu(p).oper[0]^.reg)].IsUsed(taicpu(p).oper[0]^.reg) then
                  begin
                    RemoveCurrentp(p, hp2);
                    if taicpu(hp2).opcode = A_SETcc then
                      DebugMsg(SPeepholeOptimization + 'SETcc/TEST/SETcc -> SETcc',p)
                    else
                      DebugMsg(SPeepholeOptimization + 'SETcc/TEST/Jcc -> Jcc',p);
                  end
                else
                  if taicpu(hp2).opcode = A_SETcc then
                    DebugMsg(SPeepholeOptimization + 'SETcc/TEST/SETcc -> SETcc/SETcc',p)
                  else
                    DebugMsg(SPeepholeOptimization + 'SETcc/TEST/Jcc -> SETcc/Jcc',p);

                Result := True;
              end
            else if
              { Make sure the instructions are adjacent }
              (
                not (cs_opt_level3 in current_settings.optimizerswitches) or
                GetNextInstruction(p, hp1)
              ) and
              MatchInstruction(hp1, A_MOV, [S_B]) and
              { Writing to memory is allowed }
              MatchOperand(taicpu(p).oper[0]^, taicpu(hp1).oper[0]^.reg) then
              begin
                {
                  Watch out for sequences such as:

                  set(c)b %regb
                  movb    %regb,(ref)
                  movb    $0,1(ref)
                  movb    $0,2(ref)
                  movb    $0,3(ref)

                  Much more efficient to turn it into:
                    movl    $0,%regl
                    set(c)b %regb
                    movl    %regl,(ref)

                  Or:
                    set(c)b %regb
                    movzbl  %regb,%regl
                    movl    %regl,(ref)
                }
                if (taicpu(hp1).oper[1]^.typ = top_ref) and
                  GetNextInstruction(hp1, hp2) and
                  MatchInstruction(hp2, A_MOV, [S_B]) and
                  (taicpu(hp2).oper[1]^.typ = top_ref) and
                  CheckMemoryWrite(taicpu(hp1), taicpu(hp2)) then
                  begin
                    { Don't do anything else except set Result to True }
                  end
                else
                  begin
                    if taicpu(p).oper[0]^.typ = top_reg then
                      begin
                        TransferUsedRegs(TmpUsedRegs);
                        UpdateUsedRegs(TmpUsedRegs, tai(p.Next));
                      end;

                    { If it's not a register, it's a memory address }
                    if (taicpu(p).oper[0]^.typ <> top_reg) or RegUsedAfterInstruction(taicpu(p).oper[0]^.reg, hp1, TmpUsedRegs) then
                      begin
                        { Even if the register is still in use, we can minimise the
                          pipeline stall by changing the MOV into another SETcc. }
                        taicpu(hp1).opcode := A_SETcc;
                        taicpu(hp1).condition := taicpu(p).condition;
                        if taicpu(hp1).oper[1]^.typ = top_ref then
                          begin
                            { Swapping the operand pointers like this is probably a
                              bit naughty, but it is far faster than using loadoper
                              to transfer the reference from oper[1] to oper[0] if
                              you take into account the extra procedure calls and
                              the memory allocation and deallocation required }
                            OperPtr := taicpu(hp1).oper[1];
                            taicpu(hp1).oper[1] := taicpu(hp1).oper[0];
                            taicpu(hp1).oper[0] := OperPtr;
                          end
                        else
                          taicpu(hp1).oper[0]^.reg := taicpu(hp1).oper[1]^.reg;

                        taicpu(hp1).clearop(1);
                        taicpu(hp1).ops := 1;
                        DebugMsg(SPeepholeOptimization + 'SETcc/Mov -> SETcc/SETcc',p);
                      end
                    else
                      begin
                        if taicpu(hp1).oper[1]^.typ = top_reg then
                          AllocRegBetween(taicpu(hp1).oper[1]^.reg,p,hp1,UsedRegs);

                        taicpu(p).loadoper(0, taicpu(hp1).oper[1]^);
                        RemoveInstruction(hp1);
                        DebugMsg(SPeepholeOptimization + 'SETcc/Mov -> SETcc',p);
                      end
                  end;
                Result := True;
              end;
          end;
      end;


    function TX86AsmOptimizer.OptPass2Jmp(var p : tai) : boolean;
      var
        hp1: tai;
        Count: Integer;
        OrigLabel: TAsmLabel;
      begin
        result := False;

        { Sometimes, the optimisations below can permit this }
        RemoveDeadCodeAfterJump(p);

        if (taicpu(p).oper[0]^.typ=top_ref) and (taicpu(p).oper[0]^.ref^.refaddr=addr_full) and (taicpu(p).oper[0]^.ref^.base=NR_NO) and
          (taicpu(p).oper[0]^.ref^.index=NR_NO) and (taicpu(p).oper[0]^.ref^.symbol is tasmlabel) then
          begin
            OrigLabel := TAsmLabel(taicpu(p).oper[0]^.ref^.symbol);

            { Also a side-effect of optimisations }
            if CollapseZeroDistJump(p, OrigLabel) then
              begin
                Result := True;
                Exit;
              end;

            hp1 := GetLabelWithSym(OrigLabel);
            if (taicpu(p).condition=C_None) and assigned(hp1) and SkipLabels(hp1,hp1) and (hp1.typ = ait_instruction) then
              begin
                if taicpu(hp1).opcode = A_RET then
                  begin
                    {
                      change
                             jmp .L1
                             ...
                         .L1:
                             ret
                      into
                             ret
                    }
                    begin
                      ConvertJumpToRET(p, hp1);
                      result:=true;
                    end;
                  end
                else if (cs_opt_level3 in current_settings.optimizerswitches) and
                  not (cs_opt_size in current_settings.optimizerswitches) and
                  CheckJumpMovTransferOpt(p, hp1, 0, Count) then
                  begin
                    Result := True;
                    Exit;
                  end;
              end;
          end;
      end;


    class function TX86AsmOptimizer.CanBeCMOV(p, cond_p: tai; var RefModified: Boolean) : boolean;
      begin
        Result := assigned(p) and
          MatchInstruction(p,A_MOV,[S_W,S_L,S_Q]) and
          (taicpu(p).oper[1]^.typ = top_reg) and
          (
            (taicpu(p).oper[0]^.typ = top_reg) or
            { allow references, but only pure symbols or got rel. addressing with RIP as based,
              it is not expected that this can cause a seg. violation }
            (
              (taicpu(p).oper[0]^.typ = top_ref) and
              { TODO: Can we detect which references become constants at this
                stage so we don't have to do a blanket ban? }
              (taicpu(p).oper[0]^.ref^.refaddr <> addr_full) and
              (
                IsRefSafe(taicpu(p).oper[0]^.ref) or
                (
                  { Don't use the reference in the condition if one of its registers got modified by a previous MOV }
                  not RefModified and
                  { If the reference also appears in the condition, then we know it's safe, otherwise
                    any kind of access violation would have occurred already }
                  Assigned(cond_p) and
                  { Make sure the sizes match too so we're reading and writing the same number of bytes }
                  (cond_p.typ = ait_instruction) and
                  (taicpu(cond_p).opsize = taicpu(p).opsize) and
                  { Just consider 2-operand comparison instructions for now to be safe }
                  (taicpu(cond_p).ops = 2) and
                  (
                    ((taicpu(cond_p).oper[1]^.typ = top_ref) and RefsEqual(taicpu(cond_p).oper[1]^.ref^, taicpu(p).oper[0]^.ref^)) or
                    (
                      (taicpu(cond_p).oper[0]^.typ = top_ref) and
                      { Don't risk identical registers but different offsets, as we may have constructs
                        such as buffer streams with things like length fields that indicate whether
                        any more data follows.  And there are probably some contrived examples where
                        writing to offsets behind the one being read also lead to access violations }
                      RefsEqual(taicpu(cond_p).oper[0]^.ref^, taicpu(p).oper[0]^.ref^) and
                      (
                        { Check that we're not modifying a register that appears in the reference }
                        (InsProp[taicpu(cond_p).opcode].Ch * [Ch_Mop2, Ch_RWop2, Ch_Wop2] = []) or
                        (taicpu(cond_p).oper[1]^.typ <> top_reg) or
                        not RegInRef(taicpu(cond_p).oper[1]^.reg, taicpu(cond_p).oper[0]^.ref^)
                      )
                    )
                  )
                )
              )
            )
          );
      end;


    class procedure TX86AsmOptimizer.UpdateIntRegsNoDealloc(var AUsedRegs: TAllUsedRegs; p: Tai);
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


    function TX86AsmOptimizer.OptPass2Jcc(var p : tai) : boolean;
      var
        hp1,hp2: tai;
        carryadd_opcode : TAsmOp;
        symbol: TAsmSymbol;
        increg, tmpreg: TRegister;
        RefModified: Boolean;
{$ifndef i8086}
        { Code and variables specific to CMOV optimisations }
        hp3,hp4,hp5,
        hp_stop, hp_lblxxx, hp_lblyyy, hpmov1,hpmov2, hp_prev, hp_flagalloc, hp_prev2, hp_new, hp_jump: tai;
        l, c, w, x : Longint;
        condition, second_condition : TAsmCond;
        FoundMatchingJump, RegMatch: Boolean;

        RegWrites: array[0..MAX_CMOV_INSTRUCTIONS*2 - 1] of TRegister;

        ConstRegs: array[0..MAX_CMOV_REGISTERS - 1] of TRegister;
        ConstVals: array[0..MAX_CMOV_REGISTERS - 1] of TCGInt;
        ConstSizes: array[0..MAX_CMOV_REGISTERS - 1] of TSubRegister; { May not match ConstRegs if one is shared over multiple CMOVs. }
        ConstMovs: array[0..MAX_CMOV_REGISTERS - 1] of tai; { Location of initialisation instruction }

        ConstWriteSizes: array[0..first_int_imreg - 1] of TSubRegister; { Largest size of register written. }

        { Tries to convert a mov const,%reg instruction into a CMOV by reserving a
          new register to store the constant }
        function TryCMOVConst(p, search_start_p, stop_search_p: tai; var StoredCount: LongInt; var CMOVCount: LongInt): Boolean;
          var
            RegSize: TSubRegister;
            CurrentVal: TCGInt;
            ANewReg: TRegister;
            X: ShortInt;
          begin
            Result := False;

            if not MatchOpType(taicpu(p), top_const, top_reg) then
              Exit;

            if StoredCount >= MAX_CMOV_REGISTERS then
              { Arrays are full }
              Exit;

            { Remember that CMOV can't encode 8-bit registers }
            case taicpu(p).opsize of
              S_W:
                RegSize := R_SUBW;
              S_L:
                RegSize := R_SUBD;
{$ifdef x86_64}
              S_Q:
                RegSize := R_SUBQ;
{$endif x86_64}
              else
                InternalError(2021100401);
            end;

            { See if the value has already been reserved for another CMOV instruction }
            CurrentVal := taicpu(p).oper[0]^.val;
            for X := 0 to StoredCount - 1 do
              if ConstVals[X] = CurrentVal then
                begin
                  ConstRegs[StoredCount] := ConstRegs[X];
                  ConstSizes[StoredCount] := RegSize;
                  ConstVals[StoredCount] := CurrentVal;
                  Result := True;

                  Inc(StoredCount);
                  { Don't increase CMOVCount this time, since we're re-using a register }
                  Exit;
                end;

            ANewReg := GetIntRegisterBetween(R_SUBWHOLE, TmpUsedRegs, search_start_p, stop_search_p, True);
            if ANewReg = NR_NO then
              { No free registers }
              Exit;

            { Reserve the register so subsequent TryCMOVConst calls don't all end
              up vying for the same register }
            IncludeRegInUsedRegs(ANewReg, TmpUsedRegs);

            ConstRegs[StoredCount] := ANewReg;
            ConstSizes[StoredCount] := RegSize;
            ConstVals[StoredCount] := CurrentVal;

            Inc(StoredCount);
            { Increment the CMOV count variable from OptPass2JCC, since the extra
              MOV required adds complexity and will cause diminishing returns
              sooner than normal.  This is more of an approximate weighting than
              anything else. }
            Inc(CMOVCount);
            Result := True;
          end;
{$endif i8086}

      begin
        result:=false;
        if GetNextInstruction(p,hp1) then
          begin
            if (hp1.typ=ait_label) then
              begin
                Result := DoSETccLblRETOpt(p, tai_label(hp1));
                Exit;
              end
            else if (hp1.typ<>ait_instruction) then
              Exit;

            symbol := TAsmLabel(taicpu(p).oper[0]^.ref^.symbol);
            if (
                (
                  ((Taicpu(hp1).opcode=A_ADD) or (Taicpu(hp1).opcode=A_SUB)) and
                  MatchOptype(Taicpu(hp1),top_const,top_reg) and
                  (Taicpu(hp1).oper[0]^.val=1)
                ) or
                ((Taicpu(hp1).opcode=A_INC) or (Taicpu(hp1).opcode=A_DEC))
              ) and
              GetNextInstruction(hp1,hp2) and
              SkipAligns(hp2, hp2) and
              (hp2.typ = ait_label) and
              (Tasmlabel(symbol) = Tai_label(hp2).labsym) then
             { jb @@1                            cmc
               inc/dec operand           -->     adc/sbb operand,0
               @@1:

               ... and ...

               jnb @@1
               inc/dec operand           -->     adc/sbb operand,0
               @@1: }
              begin
                if Taicpu(p).condition in [C_NAE,C_B,C_C] then
                  begin
                    case taicpu(hp1).opcode of
                      A_INC,
                      A_ADD:
                        carryadd_opcode:=A_ADC;
                      A_DEC,
                      A_SUB:
                        carryadd_opcode:=A_SBB;
                      else
                        InternalError(2021011001);
                    end;

                    Taicpu(p).clearop(0);
                    Taicpu(p).ops:=0;
                    Taicpu(p).is_jmp:=false;
                    Taicpu(p).opcode:=A_CMC;
                    Taicpu(p).condition:=C_NONE;
                    DebugMsg(SPeepholeOptimization+'JccAdd/Inc/Dec2CmcAdc/Sbb',p);
                    Taicpu(hp1).ops:=2;
                    if (Taicpu(hp1).opcode=A_ADD) or (Taicpu(hp1).opcode=A_SUB) then
                      Taicpu(hp1).loadoper(1,Taicpu(hp1).oper[1]^)
                    else
                      Taicpu(hp1).loadoper(1,Taicpu(hp1).oper[0]^);
                    Taicpu(hp1).loadconst(0,0);
                    Taicpu(hp1).opcode:=carryadd_opcode;
                    result:=true;
                    exit;
                  end
                else if Taicpu(p).condition in [C_AE,C_NB,C_NC] then
                  begin
                    case taicpu(hp1).opcode of
                      A_INC,
                      A_ADD:
                        carryadd_opcode:=A_ADC;
                      A_DEC,
                      A_SUB:
                        carryadd_opcode:=A_SBB;
                      else
                        InternalError(2021011002);
                    end;

                    Taicpu(hp1).ops:=2;
                    DebugMsg(SPeepholeOptimization+'JccAdd/Inc/Dec2Adc/Sbb',p);
                    if (Taicpu(hp1).opcode=A_ADD) or (Taicpu(hp1).opcode=A_SUB) then
                      Taicpu(hp1).loadoper(1,Taicpu(hp1).oper[1]^)
                    else
                      Taicpu(hp1).loadoper(1,Taicpu(hp1).oper[0]^);
                    Taicpu(hp1).loadconst(0,0);
                    Taicpu(hp1).opcode:=carryadd_opcode;
                    RemoveCurrentP(p, hp1);
                    result:=true;
                    exit;
                  end
                 {
                   jcc @@1                            setcc tmpreg
                   inc/dec/add/sub operand    ->      (movzx tmpreg)
                   @@1:                               add/sub tmpreg,operand

                   While this increases code size slightly, it makes the code much faster if the
                   jump is unpredictable
                 }
                 else if not(cs_opt_size in current_settings.optimizerswitches) then
                   begin
                     { search for an available register which is volatile }
                     increg := GetIntRegisterBetween(R_SUBL, UsedRegs, p, hp1);
                     if increg <> NR_NO then
                       begin
                         { We don't need to check if tmpreg is in hp1 or not, because
                           it will be marked as in use at p (if not, this is
                           indictive of a compiler bug). }
                         TAsmLabel(symbol).decrefs;
                         Taicpu(p).clearop(0);
                         Taicpu(p).ops:=1;
                         Taicpu(p).is_jmp:=false;
                         Taicpu(p).opcode:=A_SETcc;
                         DebugMsg(SPeepholeOptimization+'JccAdd2SetccAdd',p);
                         Taicpu(p).condition:=inverse_cond(Taicpu(p).condition);
                         Taicpu(p).loadreg(0,increg);

                         if getsubreg(Taicpu(hp1).oper[1]^.reg)<>R_SUBL then
                           begin
                             case getsubreg(Taicpu(hp1).oper[1]^.reg) of
                               R_SUBW:
                                 begin
                                   tmpreg := newreg(R_INTREGISTER,getsupreg(increg),R_SUBW);
                                   hp2:=Taicpu.op_reg_reg(A_MOVZX,S_BW,increg,tmpreg);
                                 end;
                               R_SUBD:
                                 begin
                                   tmpreg := newreg(R_INTREGISTER,getsupreg(increg),R_SUBD);
                                   hp2:=Taicpu.op_reg_reg(A_MOVZX,S_BL,increg,tmpreg);
                                 end;
{$ifdef x86_64}
                               R_SUBQ:
                                 begin
                                   { MOVZX doesn't have a 64-bit variant, because
                                     the 32-bit version implicitly zeroes the
                                     upper 32-bits of the destination register }
                                   tmpreg := newreg(R_INTREGISTER,getsupreg(increg),R_SUBD);
                                   hp2:=Taicpu.op_reg_reg(A_MOVZX,S_BL,increg,tmpreg);
                                   setsubreg(tmpreg, R_SUBQ);
                                 end;
{$endif x86_64}
                               else
                                 Internalerror(2020030601);
                             end;
                             taicpu(hp2).fileinfo:=taicpu(hp1).fileinfo;
                             asml.InsertAfter(hp2,p);
                           end
                         else
                           tmpreg := increg;

                         if (Taicpu(hp1).opcode=A_INC) or (Taicpu(hp1).opcode=A_DEC) then
                           begin
                             Taicpu(hp1).ops:=2;
                             Taicpu(hp1).loadoper(1,Taicpu(hp1).oper[0]^)
                           end;
                         Taicpu(hp1).loadreg(0,tmpreg);
                         AllocRegBetween(tmpreg,p,hp1,UsedRegs);

                         Result := True;

                         { p is no longer a Jcc instruction, so exit }
                         Exit;
                       end;
                   end;
               end;

              { Detect the following:
                  jmp<cond>     @Lbl1
                  jmp           @Lbl2
                  ...
                @Lbl1:
                  ret

                Change to:

                  jmp<inv_cond> @Lbl2
                  ret
              }
              if MatchInstruction(hp1,A_JMP,[]) and (taicpu(hp1).oper[0]^.ref^.refaddr=addr_full) then
                begin
                  hp2:=getlabelwithsym(TAsmLabel(symbol));
                  if Assigned(hp2) and SkipLabels(hp2,hp2) and
                    MatchInstruction(hp2,A_RET,[S_NO]) then
                    begin
                      taicpu(p).condition := inverse_cond(taicpu(p).condition);

                      { Change label address to that of the unconditional jump }
                      taicpu(p).loadoper(0, taicpu(hp1).oper[0]^);

                      TAsmLabel(symbol).DecRefs;
                      taicpu(hp1).opcode := A_RET;
                      taicpu(hp1).is_jmp := false;
                      taicpu(hp1).ops := taicpu(hp2).ops;
                      DebugMsg(SPeepholeOptimization+'JccJmpRet2J!ccRet',p);
                      case taicpu(hp2).ops of
                        0:
                          taicpu(hp1).clearop(0);
                        1:
                          taicpu(hp1).loadconst(0,taicpu(hp2).oper[0]^.val);
                        else
                          internalerror(2016041302);
                      end;
                    end;
{$ifndef i8086}
                end
              {
                  convert
                  j<c>  .L1
                  mov   1,reg
                  jmp   .L2
                .L1
                  mov   0,reg
                .L2

                into
                  mov   0,reg
                  set<not(c)> reg

                take care of alignment and that the mov 0,reg is not converted into a xor as this
                would destroy the flag contents
              }
              else if MatchInstruction(hp1,A_MOV,[]) and
                MatchOpType(taicpu(hp1),top_const,top_reg) and
{$ifdef i386}
                (
                { Under i386, ESI, EDI, EBP and ESP
                  don't have an 8-bit representation }
                  not (getsupreg(taicpu(hp1).oper[1]^.reg) in [RS_ESI, RS_EDI, RS_EBP, RS_ESP])
                ) and
{$endif i386}
                (taicpu(hp1).oper[0]^.val=1) and
                GetNextInstruction(hp1,hp2) and
                MatchInstruction(hp2,A_JMP,[]) and (taicpu(hp2).oper[0]^.ref^.refaddr=addr_full) and
                GetNextInstruction(hp2,hp3) and
                { skip align }
                ((hp3.typ<>ait_align) or GetNextInstruction(hp3,hp3)) and
                (hp3.typ=ait_label) and
                (tasmlabel(taicpu(p).oper[0]^.ref^.symbol)=tai_label(hp3).labsym) and
                (tai_label(hp3).labsym.getrefs=1) and
                GetNextInstruction(hp3,hp4) and
                MatchInstruction(hp4,A_MOV,[]) and
                MatchOpType(taicpu(hp4),top_const,top_reg) and
                (taicpu(hp4).oper[0]^.val=0) and
                MatchOperand(taicpu(hp1).oper[1]^,taicpu(hp4).oper[1]^) and
                GetNextInstruction(hp4,hp5) and
                (hp5.typ=ait_label) and
                (tasmlabel(taicpu(hp2).oper[0]^.ref^.symbol)=tai_label(hp5).labsym) and
                (tai_label(hp5).labsym.getrefs=1) then
                begin
                  AllocRegBetween(NR_FLAGS,p,hp4,UsedRegs);
                  DebugMsg(SPeepholeOptimization+'JccMovJmpMov2MovSetcc',p);
                  { remove last label }
                  RemoveInstruction(hp5);
                  { remove second label }
                  RemoveInstruction(hp3);
                  { if align is present remove it }
                  if GetNextInstruction(hp2,hp3) and (hp3.typ=ait_align) then
                    RemoveInstruction(hp3);
                  { remove jmp }
                  RemoveInstruction(hp2);
                  if taicpu(hp1).opsize=S_B then
                    RemoveInstruction(hp1)
                  else
                    taicpu(hp1).loadconst(0,0);
                  taicpu(hp4).opcode:=A_SETcc;
                  taicpu(hp4).opsize:=S_B;
                  taicpu(hp4).condition:=inverse_cond(taicpu(p).condition);
                  taicpu(hp4).loadreg(0,newreg(R_INTREGISTER,getsupreg(taicpu(hp4).oper[1]^.reg),R_SUBL));
                  taicpu(hp4).opercnt:=1;
                  taicpu(hp4).ops:=1;
                  taicpu(hp4).freeop(1);
                  RemoveCurrentP(p);
                  Result:=true;
                  exit;
                end
              else if (CPUX86_HAS_CMOV in cpu_capabilities[current_settings.cputype]) and
                MatchInstruction(hp1,A_MOV,[S_W,S_L{$ifdef x86_64},S_Q{$endif x86_64}]) then
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
                        <several cmovs with inverted condition>
                        jmp   xxx  (only for the 2nd case)
                 }
                 hp2 := p;
                 hp_lblxxx := hp1;
                 hp_flagalloc := nil;
                 hp_stop := nil;
                 FoundMatchingJump := False;

                 { Remember the first instruction in the first block of MOVs }
                 hpmov1 := hp1;

                 TransferUsedRegs(TmpUsedRegs);
                 while assigned(hp_lblxxx) and
                   { stop on labels }
                   (hp_lblxxx.typ <> ait_label) do
                   begin
                     { Keep track of all integer registers that are used }
                     UpdateIntRegsNoDealloc(TmpUsedRegs, tai(hp2.Next));
                     if hp_lblxxx.typ = ait_instruction then
                       begin
                         if (taicpu(hp_lblxxx).opcode = A_JMP) and
                           IsJumpToLabel(taicpu(hp_lblxxx)) then
                           begin
                             hp_stop := hp_lblxxx;
                             if (TAsmLabel(taicpu(hp_lblxxx).oper[0]^.ref^.symbol) = symbol) then
                               begin
                                 { We found Jcc xxx; <several movs>; Jmp xxx }
                                 FoundMatchingJump := True;
                                 Break;
                               end;
                             { If it's not the jump we're looking for, it's
                               possibly the "if..else" variant }
                           end
                           { Check to see if we have a valid MOV instruction instead }
                         else if (taicpu(hp_lblxxx).opcode <> A_MOV) or
                           not (taicpu(hp_lblxxx).opsize in [S_W, S_L{$ifdef x86_64}, S_Q{$endif x86_64}]) then
                           Break
                         else
                           { This will be a valid MOV }
                           hp_stop := hp_lblxxx;
                       end;

                     hp2 := hp_lblxxx;
                     GetNextInstruction(hp_lblxxx, hp_lblxxx);
                   end;

                 { Just make sure the last MOV is included if there's no jump }
                 if (hp_lblxxx.typ = ait_label) and MatchInstruction(hp_stop, A_MOV, []) then
                   hp_stop := hp_lblxxx;

                 { Note, the logic behind using hp_stop over hp_lblxxx in the
                   range for TryCMOVConst is so GetIntRegisterBetween doesn't
                   fail when it reaches a JMP instruction in the "jcc xxx; movs;
                   jmp yyy; xxx:; movs; yyy:" variation }

                 if assigned(hp_lblxxx) and
                   (
                     { If we found JMP xxx, we don't actually need a label
                       (hp_lblxxx is the JMP instruction instead) }
                     FoundMatchingJump or
                     { Make sure we actually have the right label }
                     FindLabel(TAsmLabel(symbol), hp_lblxxx)
                   ) then
                   begin
                     { Use TmpUsedRegs to track registers that we reserve }

                     { When allocating temporary registers, try to look one
                       instruction back, as defining them before a CMP or TEST
                       instruction will be faster, and also avoid picking a
                       register that was only just deallocated }
                     if GetLastInstruction(p, hp_prev) and
                       MatchInstruction(hp_prev, [A_CMP, A_TEST, A_BSR, A_BSF, A_COMISS, A_COMISD, A_UCOMISS, A_UCOMISD, A_VCOMISS, A_VCOMISD, A_VUCOMISS, A_VUCOMISD], []) then
                       begin
                         { Mark all the registers in the comparison as 'in use', even if they've just been deallocated }
                         for l := 0 to 1 do
                           with taicpu(hp_prev).oper[l]^ do
                             case typ of
                               top_reg:
                                 if getregtype(reg) = R_INTREGISTER then
                                   IncludeRegInUsedRegs(reg, TmpUsedRegs);
                               top_ref:
                                 begin
                                   if
{$ifdef x86_64}
                                     (ref^.base <> NR_RIP) and
{$endif x86_64}
                                     (ref^.base <> NR_NO) then
                                     IncludeRegInUsedRegs(ref^.base, TmpUsedRegs);

                                   if (ref^.index <> NR_NO) then
                                     IncludeRegInUsedRegs(ref^.index, TmpUsedRegs);
                                 end
                               else
                                 ;
                             end;

                         { When inserting instructions before hp_prev, try to insert
                           them before the allocation of the FLAGS register }
                         if not SetAndTest(FindRegAllocBackward(NR_DEFAULTFLAGS, tai(hp_prev.Previous)), hp_flagalloc) then
                           { If not found, set it equal to hp_prev so it's something sensible }
                           hp_flagalloc := hp_prev;

                         hp_prev2 := nil;
                         { When dealing with a comparison against zero, take
                           note of the instruction before it to see if we can
                           move instructions further back in order to benefit
                           PostPeepholeOptTestOr.
                         }
                         if (
                             (
                               (taicpu(hp_prev).opcode = A_CMP) and
                               MatchOperand(taicpu(hp_prev).oper[0]^, 0)
                             ) or
                             (
                               (taicpu(hp_prev).opcode = A_TEST) and
                               (
                                 OpsEqual(taicpu(hp_prev).oper[0]^, taicpu(hp_prev).oper[1]^) or
                                 MatchOperand(taicpu(hp_prev).oper[0]^, -1)
                               )
                             )
                           ) and
                           GetLastInstruction(hp_prev, hp_prev2) then
                             begin
                               if (hp_prev2.typ = ait_instruction) and
                                 { These instructions set the zero flag if the result is zero }
                                 MatchInstruction(hp_prev2, [A_ADD, A_SUB, A_OR, A_XOR, A_AND, A_POPCNT, A_LZCNT], []) then
                                 begin
                                  { Also mark all the registers in this previous instruction
                                    as 'in use', even if they've just been deallocated }
                                  for l := 0 to 1 do
                                    with taicpu(hp_prev2).oper[l]^ do
                                      case typ of
                                        top_reg:
                                          if getregtype(reg) = R_INTREGISTER then
                                            IncludeRegInUsedRegs(reg, TmpUsedRegs);
                                        top_ref:
                                          begin
                                            if
{$ifdef x86_64}
                                              (ref^.base <> NR_RIP) and
{$endif x86_64}
                                              (ref^.base <> NR_NO) then
                                              IncludeRegInUsedRegs(ref^.base, TmpUsedRegs);

                                            if (ref^.index <> NR_NO) then
                                              IncludeRegInUsedRegs(ref^.index, TmpUsedRegs);
                                          end
                                        else
                                          ;
                                      end;
                                 end
                               else
                                 { Unsuitable instruction }
                                 hp_prev2 := nil;
                           end;
                       end
                     else
                       begin
                         hp_prev := p;
                         { When inserting instructions before hp_prev, try to insert
                           them before the allocation of the FLAGS register }
                         if not SetAndTest(FindRegAllocBackward(NR_DEFAULTFLAGS, tai(p.Previous)), hp_flagalloc) then
                           { If not found, set it equal to p so it's something sensible }
                           hp_flagalloc := p;
                         hp_prev2 := nil;
                       end;

                     l := 0;
                     c := 0;

                     { Initialise RegWrites, ConstRegs, ConstVals, ConstSizes, ConstWriteSizes and ConstMovs }
                     FillChar(RegWrites[0], MAX_CMOV_INSTRUCTIONS * 2 * SizeOf(TRegister), 0);
                     FillChar(ConstRegs[0], MAX_CMOV_REGISTERS * SizeOf(TRegister), 0);
                     FillChar(ConstVals[0], MAX_CMOV_REGISTERS * SizeOf(TCGInt), 0);
                     FillChar(ConstSizes[0], MAX_CMOV_REGISTERS * SizeOf(TSubRegister), 0);
                     FillChar(ConstWriteSizes[0], first_int_imreg * SizeOf(TOpSize), 0);
                     FillChar(ConstMovs[0], MAX_CMOV_REGISTERS * SizeOf(taicpu), 0);

                     RefModified := False;
                     while assigned(hp1) and
                       { Stop on the label we found }
                       (hp1 <> hp_lblxxx) do
                       begin
                         case hp1.typ of
                           ait_instruction:
                             if MatchInstruction(hp1, A_MOV, [S_W, S_L{$ifdef x86_64}, S_Q{$endif x86_64}]) then
                               begin
                                 if CanBeCMOV(hp1, hp_prev, RefModified) then
                                   begin
                                     Inc(l);

                                     { MOV instruction will be writing to a register }
                                     if Assigned(hp_prev) and
                                       { Make sure the sizes match too so we're reading and writing the same number of bytes }
                                       (hp_prev.typ = ait_instruction) and
                                       (taicpu(hp_prev).ops = 2) and
                                       (
                                         (
                                           (taicpu(hp_prev).oper[0]^.typ = top_ref) and
                                           RegInRef(taicpu(hp1).oper[1]^.reg, taicpu(hp_prev).oper[0]^.ref^)
                                         ) or
                                         (
                                           (taicpu(hp_prev).oper[1]^.typ = top_ref) and
                                           RegInRef(taicpu(hp1).oper[1]^.reg, taicpu(hp_prev).oper[1]^.ref^)
                                         )
                                       ) then
                                       { It is no longer safe to use the reference in the condition.
                                         this prevents problems such as:
                                           mov (%reg),%reg
                                           mov (%reg),...

                                         When the comparison is cmp (%reg),0 and guarding against a null pointer deallocation
                                         (fixes #40165)

                                         Note: "mov (%reg1),%reg2; mov (%reg2),..." won't be optimised this way since
                                         at least one of (%reg1) and (%reg2) won't be in the condition and is hence unsafe.
                                       }
                                       RefModified := True;
                                   end
                                 else if not (cs_opt_size in current_settings.optimizerswitches) and
                                   { CMOV with constants grows the code size }
                                   TryCMOVConst(hp1, hp_prev, hp_stop, c, l) then
                                   begin
                                     { Register was reserved by TryCMOVConst and
                                       stored on ConstRegs[c] }
                                   end
                                 else
                                   Break;
                               end
                             else
                               Break;
                           else
                             ;
                         end;
                         GetNextInstruction(hp1,hp1);
                       end;

                      if (hp1 = hp_lblxxx) then
                        begin
                          if (l <= MAX_CMOV_INSTRUCTIONS) and (l > 0) then
                            begin
                              { Repurpose TmpUsedRegs to mark registers that we've defined }
                              TmpUsedRegs[R_INTREGISTER].Clear;

                              x := 0;
                              AllocRegBetween(NR_DEFAULTFLAGS, p, hp_lblxxx, UsedRegs);
                              condition := inverse_cond(taicpu(p).condition);
                              UpdateUsedRegs(tai(p.next));

                              hp1 := hpmov1;
                              repeat
                                if not Assigned(hp1) then
                                  InternalError(2018062900);

                                if (hp1.typ = ait_instruction) then
                                  begin
                                    { Extra safeguard }
                                    if (taicpu(hp1).opcode <> A_MOV) then
                                      InternalError(2018062901);

                                    if taicpu(hp1).oper[0]^.typ = top_const then
                                      begin
                                        if x >= MAX_CMOV_REGISTERS then
                                          InternalError(2021100410);

                                        { If it's in TmpUsedRegs, then this register
                                          is being used more than once and hence has
                                          already had its value defined (it gets
                                          added to UsedRegs through AllocRegBetween
                                          below) }
                                        if not TmpUsedRegs[R_INTREGISTER].IsUsed(ConstRegs[x]) then
                                          begin
                                            hp_new := taicpu.op_const_reg(A_MOV, subreg2opsize(R_SUBWHOLE), taicpu(hp1).oper[0]^.val, ConstRegs[X]);
                                            taicpu(hp_new).fileinfo := taicpu(hp_prev).fileinfo;

                                            asml.InsertBefore(hp_new, hp_flagalloc);
                                            if Assigned(hp_prev2) then
                                              TrySwapMovOp(hp_prev2, hp_new);

                                            IncludeRegInUsedRegs(ConstRegs[x], TmpUsedRegs);

                                            ConstMovs[X] := hp_new;
                                          end
                                        else
                                        { We just need an instruction between hp_prev and hp1
                                          where we know the register is marked as in use }
                                          hp_new := hpmov1;

                                        { Keep track of largest write for this register so it can be optimised later }
                                        if (getsubreg(taicpu(hp1).oper[1]^.reg) > ConstWriteSizes[getsupreg(ConstRegs[X])]) then
                                          ConstWriteSizes[getsupreg(ConstRegs[X])] := getsubreg(taicpu(hp1).oper[1]^.reg);

                                        AllocRegBetween(ConstRegs[x], hp_new, hp1, UsedRegs);
                                        taicpu(hp1).loadreg(0, newreg(R_INTREGISTER, getsupreg(ConstRegs[X]), ConstSizes[X]));
                                        Inc(x);
                                      end;

                                    taicpu(hp1).opcode := A_CMOVcc;
                                    taicpu(hp1).condition := condition;
                                  end;

                                UpdateUsedRegs(tai(hp1.next));
                                GetNextInstruction(hp1, hp1);
                              until (hp1 = hp_lblxxx);

                              { Update initialisation MOVs to the smallest possible size }
                              for c := 0 to x - 1 do
                                if Assigned(ConstMovs[c]) then
                                  begin
                                    taicpu(ConstMovs[c]).opsize := subreg2opsize(ConstWriteSizes[Word(ConstRegs[c])]);
                                    setsubreg(taicpu(ConstMovs[c]).oper[1]^.reg, ConstWriteSizes[Word(ConstRegs[c])]);
                                  end;

                              hp2 := hp_lblxxx;
                              repeat
                                if not Assigned(hp2) then
                                  InternalError(2018062910);

                                case hp2.typ of
                                  ait_label:
                                    { What we expected - break out of the loop (it won't be a dead label at the top of
                                      a cluster because that was optimised at an earlier stage) }
                                    Break;
                                  ait_align:
                                    { Go to the next entry until a label is found (may be multiple aligns before it) }
                                    begin
                                      hp2 := tai(hp2.Next);
                                      Continue;
                                    end;
                                  ait_instruction:
                                    begin
                                      if taicpu(hp2).opcode<>A_JMP then
                                        InternalError(2018062912);

                                      { This is the Jcc @Lbl; <several movs>; JMP @Lbl variant }
                                      Break;
                                    end
                                  else
                                    begin
                                      { Might be a comment or temporary allocation entry }
                                      if not (hp2.typ in SkipInstr) then
                                        InternalError(2018062911);

                                      hp2 := tai(hp2.Next);
                                      Continue;
                                    end;
                                end;

                              until False;

                              { Now we can safely decrement the reference count }
                              tasmlabel(symbol).decrefs;

                              DebugMsg(SPeepholeOptimization+'JccMov2CMov',p);

                              { Remove the original jump }
                              RemoveInstruction(p); { Note, the choice to not use RemoveCurrentp is deliberate }

                              if hp2.typ=ait_instruction then
                                begin
                                  p := hp2;
                                  Result := True;
                                end
                              else
                                begin
                                  UpdateUsedRegs(tai(hp2.next));
                                  Result := GetNextInstruction(hp2, p); { Instruction after the label }

                                  { Remove the label if this is its final reference }
                                  if (tasmlabel(symbol).getrefs=0) then
                                    begin
                                      { Make sure the aligns get stripped too }
                                      hp1 := tai(hp_lblxxx.Previous);
                                      while Assigned(hp1) and (hp1.typ = ait_align) do
                                        begin
                                          hp_lblxxx := hp1;
                                          hp1 := tai(hp_lblxxx.Previous);
                                        end;
                                      StripLabelFast(hp_lblxxx);
                                    end;
                                end;

                              Exit;
                            end;
                        end
                      else if assigned(hp_lblxxx) and
                         { check further for
                                jCC   xxx
                                <several movs 1>
                                jmp   yyy
                        xxx:
                                <several movs 2>
                        yyy:
                         }
                        (l <= MAX_CMOV_INSTRUCTIONS - 1) and
                        { hp1 should be pointing to jmp yyy }
                        MatchInstruction(hp1, A_JMP, []) and
                        { real label and jump, no further references to the
                          label are allowed }
                        (TAsmLabel(symbol).getrefs=1) and
                        FindLabel(TAsmLabel(symbol), hp_lblxxx) then
                        begin
                          hp_jump := hp1;

                          { Don't set c to zero }
                          l := 0;
                          w := 0;

                          GetNextInstruction(hp_lblxxx, hpmov2);

                          hp2 := hp_lblxxx;
                          hp_lblyyy := hpmov2;

                          while assigned(hp_lblyyy) and
                            { stop on labels }
                            (hp_lblyyy.typ <> ait_label) do
                            begin
                              { Keep track of all integer registers that are used }
                              UpdateIntRegsNoDealloc(TmpUsedRegs, tai(hp2.Next));
                              if not MatchInstruction(hp_lblyyy, A_MOV, [S_W, S_L{$ifdef x86_64}, S_Q{$endif x86_64}]) then
                                Break;

                              hp2 := hp_lblyyy;
                              GetNextInstruction(hp_lblyyy, hp_lblyyy);
                            end;

                          { Analyse the second batch of MOVs to see if the setup is valid }
                          RefModified := False;
                          hp1 := hpmov2;
                          while assigned(hp1) and
                            (hp1 <> hp_lblyyy) do
                            begin
                              case hp1.typ of
                                ait_instruction:
                                  if MatchInstruction(hp1, A_MOV, [S_W, S_L{$ifdef x86_64}, S_Q{$endif x86_64}]) then
                                    begin
                                      if CanBeCMOV(hp1, hp_prev, RefModified) then
                                        begin
                                          Inc(l);

                                          { MOV instruction will be writing to a register }
                                          if Assigned(hp_prev) and
                                            { Make sure the sizes match too so we're reading and writing the same number of bytes }
                                            (hp_prev.typ = ait_instruction) and
                                            (taicpu(hp_prev).ops = 2) and
                                            (
                                              (
                                                (taicpu(hp_prev).oper[0]^.typ = top_ref) and
                                                RegInRef(taicpu(hp1).oper[1]^.reg, taicpu(hp_prev).oper[0]^.ref^)
                                              ) or
                                              (
                                                (taicpu(hp_prev).oper[1]^.typ = top_ref) and
                                                RegInRef(taicpu(hp1).oper[1]^.reg, taicpu(hp_prev).oper[1]^.ref^)
                                              )
                                            ) then
                                              { It is no longer safe to use the reference in the condition.
                                                this prevents problems such as:
                                                  mov (%reg),%reg
                                                  mov (%reg),...

                                                When the comparison is cmp (%reg),0 and guarding against a null pointer deallocation
                                                (fixes #40165)

                                                Note: "mov (%reg1),%reg2; mov (%reg2),..." won't be optimised this way since
                                                at least one of (%reg1) and (%reg2) won't be in the condition and is hence unsafe.
                                              }
                                            RefModified := True;
                                        end
                                      else if not (cs_opt_size in current_settings.optimizerswitches)
                                        { CMOV with constants grows the code size }
                                        and TryCMOVConst(hp1, hpmov2, hp_lblyyy, c, l) then
                                        begin
                                          { Register was reserved by TryCMOVConst and
                                            stored on ConstRegs[c] }
                                        end
                                      else
                                        Break;
                                    end
                                  else
                                    Break;
                                else
                                  ;
                              end;
                              GetNextInstruction(hp1,hp1);
                            end;

                          { Repurpose TmpUsedRegs to mark registers that we've defined }
                          TmpUsedRegs[R_INTREGISTER].Clear;

                          if (l <= MAX_CMOV_INSTRUCTIONS - 1) and
                            (hp1 = hp_lblyyy) and
                            FindLabel(TAsmLabel(taicpu(hp_jump).oper[0]^.ref^.symbol), hp_lblyyy) then
                            begin
                              AllocRegBetween(NR_DEFAULTFLAGS, p, hp_lblyyy, UsedRegs);

                              second_condition := taicpu(p).condition;
                              condition := inverse_cond(taicpu(p).condition);
                              UpdateUsedRegs(tai(p.next));

                              { Scan through the first set of MOVs to update UsedRegs,
                                but don't process them yet }
                              hp1 := hpmov1;
                              repeat
                                if not Assigned(hp1) then
                                  InternalError(2018062901);

                                UpdateUsedRegs(tai(hp1.next));
                                GetNextInstruction(hp1, hp1);
                              until (hp1 = hp_lblxxx);

                              UpdateUsedRegs(tai(hp_lblxxx.next));

                              { Process the second set of MOVs first,
                                because if a destination register is
                                shared between the first and second MOV
                                sets, it is more efficient to turn the
                                first one into a MOV instruction and place
                                it before the CMP if possible, but we
                                won't know which registers are shared
                                until we've processed at least one list,
                                so we might as well make it the second
                                one since that won't be modified again. }

                              hp1 := hpmov2;
                              repeat
                                if not Assigned(hp1) then
                                  InternalError(2018062902);

                                if (hp1.typ = ait_instruction) then
                                  begin
                                    { Extra safeguard }
                                    if (taicpu(hp1).opcode <> A_MOV) then
                                      InternalError(2018062903);

                                    if taicpu(hp1).oper[0]^.typ = top_const then
                                      begin
                                        RegMatch := False;

                                        for x := 0 to c - 1 do
                                          if (ConstVals[x] = taicpu(hp1).oper[0]^.val) and
                                            (getsubreg(taicpu(hp1).oper[1]^.reg) = ConstSizes[X]) then
                                            begin
                                              RegMatch := True;

                                              { If it's in TmpUsedRegs, then this register
                                                is being used more than once and hence has
                                                already had its value defined (it gets
                                                added to UsedRegs through AllocRegBetween
                                                below) }
                                              if not TmpUsedRegs[R_INTREGISTER].IsUsed(ConstRegs[x]) then
                                                begin
                                                  hp_new := taicpu.op_const_reg(A_MOV, subreg2opsize(R_SUBWHOLE), taicpu(hp1).oper[0]^.val, ConstRegs[X]);
                                                  asml.InsertBefore(hp_new, hp_flagalloc);
                                                  if Assigned(hp_prev2) then
                                                    TrySwapMovOp(hp_prev2, hp_new);

                                                  IncludeRegInUsedRegs(ConstRegs[x], TmpUsedRegs);

                                                  ConstMovs[X] := hp_new;
                                                end
                                              else
                                                { We just need an instruction between hp_prev and hp1
                                                  where we know the register is marked as in use }
                                                hp_new := hpmov2;

                                              { Keep track of largest write for this register so it can be optimised later }
                                              if (getsubreg(taicpu(hp1).oper[1]^.reg) > ConstWriteSizes[getsupreg(ConstRegs[X])]) then
                                                ConstWriteSizes[getsupreg(ConstRegs[X])] := getsubreg(taicpu(hp1).oper[1]^.reg);

                                              AllocRegBetween(ConstRegs[x], hp_new, hp1, UsedRegs);
                                              taicpu(hp1).loadreg(0, newreg(R_INTREGISTER, getsupreg(ConstRegs[X]), ConstSizes[X]));
                                              Break;
                                            end;

                                        if not RegMatch then
                                          InternalError(2021100411);
                                      end;

                                    taicpu(hp1).opcode := A_CMOVcc;
                                    taicpu(hp1).condition := second_condition;

                                    { Store these writes to search for
                                      duplicates later on }
                                    RegWrites[w] := taicpu(hp1).oper[1]^.reg;
                                    Inc(w);
                                  end;

                                UpdateUsedRegs(tai(hp1.next));
                                GetNextInstruction(hp1, hp1);
                              until (hp1 = hp_lblyyy);

                              { Now do the first set of MOVs }
                              hp1 := hpmov1;
                              repeat
                                if not Assigned(hp1) then
                                  InternalError(2018062904);

                                if (hp1.typ = ait_instruction) then
                                  begin
                                    RegMatch := False;

                                    { Extra safeguard }
                                    if (taicpu(hp1).opcode <> A_MOV) then
                                      InternalError(2018062905);

                                    { Search through the RegWrites list to see
                                      if there are any opposing CMOV pairs that
                                      write to the same register }
                                    for x := 0 to w - 1 do
                                      if (RegWrites[x] = taicpu(hp1).oper[1]^.reg) then
                                        begin
                                          { We have a match.  Keep this as a MOV }

                                          { Move ahead in preparation }
                                          GetNextInstruction(hp1, hp1);

                                          RegMatch := True;
                                          Break;
                                        end;

                                    if RegMatch then
                                      Continue;

                                    if taicpu(hp1).oper[0]^.typ = top_const then
                                      begin
                                        RegMatch := False;

                                        for x := 0 to c - 1 do
                                          if (ConstVals[x] = taicpu(hp1).oper[0]^.val) and
                                            (getsubreg(taicpu(hp1).oper[1]^.reg) = ConstSizes[X]) then
                                            begin
                                              RegMatch := True;

                                              { If it's in TmpUsedRegs, then this register
                                                is being used more than once and hence has
                                                already had its value defined (it gets
                                                added to UsedRegs through AllocRegBetween
                                                below) }
                                              if not TmpUsedRegs[R_INTREGISTER].IsUsed(ConstRegs[x]) then
                                                begin
                                                  hp_new := taicpu.op_const_reg(A_MOV, subreg2opsize(R_SUBWHOLE), taicpu(hp1).oper[0]^.val, ConstRegs[X]);
                                                  asml.InsertBefore(hp_new, hp_flagalloc);
                                                  if Assigned(hp_prev2) then
                                                    TrySwapMovOp(hp_prev2, hp_new);

                                                  IncludeRegInUsedRegs(ConstRegs[x], TmpUsedRegs);

                                                  ConstMovs[X] := hp_new;
                                                end
                                              else
                                                { We just need an instruction between hp_prev and hp1
                                                  where we know the register is marked as in use }
                                                hp_new := hpmov1;

                                              { Keep track of largest write for this register so it can be optimised later }
                                              if (getsubreg(taicpu(hp1).oper[1]^.reg) > ConstWriteSizes[getsupreg(ConstRegs[X])]) then
                                                ConstWriteSizes[getsupreg(ConstRegs[X])] := getsubreg(taicpu(hp1).oper[1]^.reg);

                                              AllocRegBetween(ConstRegs[x], hp_new, hp1, UsedRegs);
                                              taicpu(hp1).loadreg(0, newreg(R_INTREGISTER, getsupreg(ConstRegs[X]), ConstSizes[X]));
                                              Break;
                                            end;

                                        if not RegMatch then
                                          InternalError(2021100412);
                                      end;

                                    taicpu(hp1).opcode := A_CMOVcc;
                                    taicpu(hp1).condition := condition;
                                  end;

                                GetNextInstruction(hp1, hp1);
                              until (hp1 = hp_jump); { Stop at the jump, not lbl xxx }

                              { Update initialisation MOVs to the smallest possible size }
                              for x := 0 to c - 1 do
                                if Assigned(ConstMovs[x]) then
                                  begin
                                    taicpu(ConstMovs[x]).opsize := subreg2opsize(ConstWriteSizes[Word(ConstRegs[x])]);
                                    setsubreg(taicpu(ConstMovs[x]).oper[1]^.reg, ConstWriteSizes[Word(ConstRegs[x])]);
                                  end;

                              UpdateUsedRegs(tai(hp_jump.next));
                              UpdateUsedRegs(tai(hp_lblyyy.next));

                              { Get first instruction after label }
                              hp1 := p;
                              GetNextInstruction(hp_lblyyy, p);

                              { Don't dereference yet, as doing so will cause
                                GetNextInstruction to skip the label and
                                optional align marker. [Kit] }

                              DebugMsg(SPeepholeOptimization+'JccMovJmpMov2CMovCMov',hp1);

                              { remove Jcc }
                              RemoveInstruction(hp1);

                              { Now we can safely decrement it }
                              tasmlabel(symbol).decrefs;

                              { Remove label xxx (it will have a ref of zero due to the initial check) }
                              { Make sure the aligns get stripped too }
                              hp1 := tai(hp_lblxxx.Previous);
                              while Assigned(hp1) and (hp1.typ = ait_align) do
                                begin
                                  hp_lblxxx := hp1;
                                  hp1 := tai(hp_lblxxx.Previous);
                                end;
                              StripLabelFast(hp_lblxxx);

                              { remove jmp }
                              symbol := taicpu(hp_jump).oper[0]^.ref^.symbol;

                              RemoveInstruction(hp_jump);

                              { As before, now we can safely decrement it }
                              TAsmLabel(symbol).decrefs;

                              { Remove label yyy (and the optional alignment) if its reference falls to zero }
                              if TAsmLabel(symbol).getrefs = 0 then
                                begin
                                  { Make sure the aligns get stripped too }
                                  hp1 := tai(hp_lblyyy.Previous);
                                  while Assigned(hp1) and (hp1.typ = ait_align) do
                                    begin
                                      hp_lblyyy := hp1;
                                      hp1 := tai(hp_lblyyy.Previous);
                                    end;
                                  StripLabelFast(hp_lblyyy);
                                end;

                              if Assigned(p) then
                                result := True;
                              exit;
                            end;
                        end;
                    end;
{$endif i8086}
              end;
          end;
      end;


    function TX86AsmOptimizer.OptPass1Movx(var p : tai) : boolean;
      var
        hp1,hp2,hp3: tai;
        reg_and_hp1_is_instr, RegUsed, AndTest: Boolean;
        NewSize: TOpSize;
        NewRegSize: TSubRegister;
        Limit: TCgInt;
        SwapOper: POper;
      begin
        result:=false;
        reg_and_hp1_is_instr:=(taicpu(p).oper[1]^.typ = top_reg) and
          GetNextInstruction(p,hp1) and
          (hp1.typ = ait_instruction);

        if reg_and_hp1_is_instr and
          (
            (taicpu(hp1).opcode <> A_LEA) or
            { If the LEA instruction can be converted into an arithmetic instruction,
              it may be possible to then fold it. }
            (
              { If the flags register is in use, don't change the instruction
                to an ADD otherwise this will scramble the flags. [Kit] }
              not RegInUsedRegs(NR_DEFAULTFLAGS, UsedRegs) and
              ConvertLEA(taicpu(hp1))
            )
          ) and
          IsFoldableArithOp(taicpu(hp1),taicpu(p).oper[1]^.reg) and
          GetNextInstruction(hp1,hp2) and
          MatchInstruction(hp2,A_MOV,[]) and
          (taicpu(hp2).oper[0]^.typ = top_reg) and
          OpsEqual(taicpu(hp2).oper[1]^,taicpu(p).oper[0]^) and
          ((taicpu(p).opsize in [S_BW,S_BL]) and (taicpu(hp2).opsize=S_B) or
           (taicpu(p).opsize in [S_WL]) and (taicpu(hp2).opsize=S_W)) and
{$ifdef i386}
           { not all registers have byte size sub registers on i386 }
           ((taicpu(hp2).opsize<>S_B) or (getsupreg(taicpu(hp1).oper[0]^.reg) in [RS_EAX, RS_EBX, RS_ECX, RS_EDX])) and
{$endif i386}
           (((taicpu(hp1).ops=2) and
             (getsupreg(taicpu(hp2).oper[0]^.reg)=getsupreg(taicpu(hp1).oper[1]^.reg))) or
             ((taicpu(hp1).ops=1) and
             (getsupreg(taicpu(hp2).oper[0]^.reg)=getsupreg(taicpu(hp1).oper[0]^.reg)))) and
           not(RegUsedAfterInstruction(taicpu(hp2).oper[0]^.reg,hp2,UsedRegs)) then
          begin
            { change   movsX/movzX    reg/ref, reg2
                       add/sub/or/... reg3/$const, reg2
                       mov            reg2 reg/ref
              to       add/sub/or/... reg3/$const, reg/ref      }

            { by example:
                movswl  %si,%eax        movswl  %si,%eax      p
                decl    %eax            addl    %edx,%eax     hp1
                movw    %ax,%si         movw    %ax,%si       hp2
              ->
                movswl  %si,%eax        movswl  %si,%eax      p
                decw    %eax            addw    %edx,%eax     hp1
                movw    %ax,%si         movw    %ax,%si       hp2
            }
            taicpu(hp1).changeopsize(taicpu(hp2).opsize);
            {
              ->
                movswl  %si,%eax        movswl  %si,%eax      p
                decw    %si             addw    %dx,%si       hp1
                movw    %ax,%si         movw    %ax,%si       hp2
            }
            case taicpu(hp1).ops of
              1:
               taicpu(hp1).loadoper(0,taicpu(hp2).oper[1]^);
              2:
                begin
                  taicpu(hp1).loadoper(1,taicpu(hp2).oper[1]^);
                  if (taicpu(hp1).oper[0]^.typ = top_reg) then
                    setsubreg(taicpu(hp1).oper[0]^.reg,getsubreg(taicpu(hp2).oper[0]^.reg));
                end;
              else
                internalerror(2008042702);
            end;
            {
              ->
                decw    %si             addw    %dx,%si       p
            }
            DebugMsg(SPeepholeOptimization + 'var3',p);
            RemoveCurrentP(p, hp1);
            RemoveInstruction(hp2);
            Result := True;
            Exit;
          end;

        if reg_and_hp1_is_instr and
          (taicpu(hp1).opcode = A_MOV) and
          MatchOpType(taicpu(hp1),top_reg,top_reg) and
          (MatchOperand(taicpu(p).oper[1]^,taicpu(hp1).oper[0]^)
{$ifdef x86_64}
           { check for implicit extension to 64 bit }
           or
           ((taicpu(p).opsize in [S_BL,S_WL]) and
            (taicpu(hp1).opsize=S_Q) and
            SuperRegistersEqual(taicpu(p).oper[1]^.reg,taicpu(hp1).oper[0]^.reg)
           )
{$endif x86_64}
          )
          then
          begin
            { change
              movx   %reg1,%reg2
              mov    %reg2,%reg3
              dealloc %reg2

              into

              movx   %reg,%reg3
            }
            TransferUsedRegs(TmpUsedRegs);
            UpdateUsedRegs(TmpUsedRegs, tai(p.next));
            if not(RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp1,TmpUsedRegs)) then
              begin
                DebugMsg(SPeepholeOptimization + 'MovxMov2Movx',p);
{$ifdef x86_64}
                if (taicpu(p).opsize in [S_BL,S_WL]) and
                  (taicpu(hp1).opsize=S_Q) then
                  taicpu(p).loadreg(1,newreg(R_INTREGISTER,getsupreg(taicpu(hp1).oper[1]^.reg),R_SUBD))
                else
{$endif x86_64}
                  taicpu(p).loadreg(1,taicpu(hp1).oper[1]^.reg);
                RemoveInstruction(hp1);
                Result := True;
                Exit;
              end;
          end;

        if reg_and_hp1_is_instr and
          ((taicpu(hp1).opcode=A_MOV) or
           (taicpu(hp1).opcode=A_ADD) or
           (taicpu(hp1).opcode=A_SUB) or
           (taicpu(hp1).opcode=A_CMP) or
           (taicpu(hp1).opcode=A_OR) or
           (taicpu(hp1).opcode=A_XOR) or
           (taicpu(hp1).opcode=A_AND)
          ) and
          (taicpu(hp1).oper[1]^.typ = top_reg) then
          begin
            AndTest := (taicpu(hp1).opcode=A_AND) and
              GetNextInstruction(hp1, hp2) and
              (hp2.typ = ait_instruction) and
              (
                (
                  (taicpu(hp2).opcode=A_TEST) and
                  (
                    MatchOperand(taicpu(hp2).oper[0]^, taicpu(hp1).oper[1]^.reg) or
                    MatchOperand(taicpu(hp2).oper[0]^, -1) or
                    (
                      { If the AND and TEST instructions share a constant, this is also valid }
                      (taicpu(hp1).oper[0]^.typ = top_const) and
                      MatchOperand(taicpu(hp2).oper[0]^, taicpu(hp1).oper[0]^.val)
                    )
                  ) and
                  MatchOperand(taicpu(hp2).oper[1]^, taicpu(hp1).oper[1]^.reg)
                ) or
                (
                  (taicpu(hp2).opcode=A_CMP) and
                  MatchOperand(taicpu(hp2).oper[0]^, 0) and
                  MatchOperand(taicpu(hp2).oper[1]^, taicpu(hp1).oper[1]^.reg)
                )
              );

            { change
              movx    (oper),%reg2
              and     $x,%reg2
              test    %reg2,%reg2
              dealloc %reg2

              into

              op     %reg1,%reg3

              if the second op accesses only the bits stored in reg1
            }
            if ((taicpu(p).oper[0]^.typ=top_reg) or
              ((taicpu(p).oper[0]^.typ=top_ref) and (taicpu(p).oper[0]^.ref^.refaddr<>addr_full))) and
              (taicpu(hp1).oper[0]^.typ = top_const) and
              (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) and
              AndTest then
              begin
                { Check if the AND constant is in range }
                case taicpu(p).opsize of
                  S_BW, S_BL{$ifdef x86_64}, S_BQ{$endif x86_64}:
                    begin
                      NewSize := S_B;
                      Limit := $FF;
                    end;
                  S_WL{$ifdef x86_64}, S_WQ{$endif x86_64}:
                    begin
                      NewSize := S_W;
                      Limit := $FFFF;
                    end;
{$ifdef x86_64}
                  S_LQ:
                    begin
                      NewSize := S_L;
                      Limit := $FFFFFFFF;
                    end;
{$endif x86_64}
                  else
                    InternalError(2021120303);
                end;

                if (
                    ((taicpu(hp1).oper[0]^.val and Limit) = taicpu(hp1).oper[0]^.val) or
                    { Check for negative operands }
                    (((not taicpu(hp1).oper[0]^.val) and Limit) = (not taicpu(hp1).oper[0]^.val))
                  ) and
                  GetNextInstruction(hp2,hp3) and
                  MatchInstruction(hp3,A_Jcc,A_Setcc,A_CMOVcc,[]) and
                  (taicpu(hp3).condition in [C_E,C_NE]) then
                  begin
                    TransferUsedRegs(TmpUsedRegs);
                    UpdateUsedRegs(TmpUsedRegs, tai(p.Next));
                    UpdateUsedRegs(TmpUsedRegs, tai(hp1.Next));
                    if not(RegUsedAfterInstruction(taicpu(hp2).oper[1]^.reg, hp2, TmpUsedRegs)) then
                      begin
                        DebugMsg(SPeepholeOptimization + 'MovxAndTest2Test done',p);
                        taicpu(hp1).loadoper(1, taicpu(p).oper[0]^);
                        taicpu(hp1).opcode := A_TEST;
                        taicpu(hp1).opsize := NewSize;
                        RemoveInstruction(hp2);
                        RemoveCurrentP(p, hp1);
                        Result:=true;
                        exit;
                      end;
                  end;
              end;


            if (taicpu(hp1).oper[0]^.typ = top_reg) and
              (((taicpu(p).opsize in [S_BW,S_BL,S_WL{$ifdef x86_64},S_BQ,S_WQ,S_LQ{$endif x86_64}]) and
               (taicpu(hp1).opsize=S_B)) or
               ((taicpu(p).opsize in [S_WL{$ifdef x86_64},S_WQ,S_LQ{$endif x86_64}]) and
               (taicpu(hp1).opsize=S_W))
{$ifdef x86_64}
               or ((taicpu(p).opsize=S_LQ) and
                (taicpu(hp1).opsize=S_L))
{$endif x86_64}
              ) and
              SuperRegistersEqual(taicpu(p).oper[1]^.reg,taicpu(hp1).oper[0]^.reg) then
              begin
                { change
                  movx   %reg1,%reg2
                  op     %reg2,%reg3
                  dealloc %reg2

                  into

                  op     %reg1,%reg3

                  if the second op accesses only the bits stored in reg1
                }
                TransferUsedRegs(TmpUsedRegs);
                UpdateUsedRegs(TmpUsedRegs, tai(p.next));
                if AndTest then
                  begin
                    UpdateUsedRegs(TmpUsedRegs, tai(hp1.next));
                    RegUsed := RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp2,TmpUsedRegs);
                  end
                else
                  RegUsed := RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp1,TmpUsedRegs);

                if not RegUsed then
                  begin
                    DebugMsg(SPeepholeOptimization + 'MovxOp2Op 1',p);
                    if taicpu(p).oper[0]^.typ=top_reg then
                      begin
                        case taicpu(hp1).opsize of
                          S_B:
                            taicpu(hp1).loadreg(0,newreg(R_INTREGISTER,getsupreg(taicpu(p).oper[0]^.reg),R_SUBL));
                          S_W:
                            taicpu(hp1).loadreg(0,newreg(R_INTREGISTER,getsupreg(taicpu(p).oper[0]^.reg),R_SUBW));
                          S_L:
                            taicpu(hp1).loadreg(0,newreg(R_INTREGISTER,getsupreg(taicpu(p).oper[0]^.reg),R_SUBD));
                          else
                            Internalerror(2020102301);
                        end;
                        AllocRegBetween(taicpu(hp1).oper[0]^.reg,p,hp1,UsedRegs);
                      end
                    else
                      taicpu(hp1).loadref(0,taicpu(p).oper[0]^.ref^);
                    RemoveCurrentP(p);
                    if AndTest then
                      RemoveInstruction(hp2);
                    result:=true;
                    exit;
                  end;
              end
            else if (taicpu(p).oper[1]^.reg = taicpu(hp1).oper[1]^.reg) and
              (
                { Bitwise operations only }
                (taicpu(hp1).opcode=A_AND) or
                (taicpu(hp1).opcode=A_TEST) or
                (
                  (taicpu(hp1).oper[0]^.typ = top_const) and
                  (
                    (taicpu(hp1).opcode=A_OR) or
                    (taicpu(hp1).opcode=A_XOR)
                  )
                )
              ) and
              (
                (taicpu(hp1).oper[0]^.typ = top_const) or
                MatchOperand(taicpu(hp1).oper[0]^, taicpu(p).oper[1]^.reg) or
                not RegInOp(taicpu(p).oper[1]^.reg, taicpu(hp1).oper[0]^)
              ) then
              begin
                { change
                  movx   %reg2,%reg2
                  op     const,%reg2

                  into
                  op     const,%reg2  (smaller version)
                  movx   %reg2,%reg2

                  also change
                  movx     %reg1,%reg2
                  and/test (oper),%reg2
                  dealloc %reg2

                  into

                  and/test (oper),%reg1
                }
                case taicpu(p).opsize of
                  S_BW, S_BL{$ifdef x86_64}, S_BQ{$endif x86_64}:
                    begin
                      NewSize := S_B;
                      NewRegSize := R_SUBL;
                      Limit := $FF;
                    end;
                  S_WL{$ifdef x86_64}, S_WQ{$endif x86_64}:
                    begin
                      NewSize := S_W;
                      NewRegSize := R_SUBW;
                      Limit := $FFFF;
                    end;
{$ifdef x86_64}
                  S_LQ:
                    begin
                      NewSize := S_L;
                      NewRegSize := R_SUBD;
                      Limit := $FFFFFFFF;
                    end;
{$endif x86_64}
                  else
                    Internalerror(2021120302);
                end;

                TransferUsedRegs(TmpUsedRegs);
                UpdateUsedRegs(TmpUsedRegs, tai(p.next));
                if AndTest then
                  begin
                    UpdateUsedRegs(TmpUsedRegs, tai(hp1.next));
                    RegUsed := RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp2,TmpUsedRegs);
                  end
                else
                  RegUsed := RegUsedAfterInstruction(taicpu(p).oper[1]^.reg,hp1,TmpUsedRegs);

                if
                  (
                    (taicpu(p).opcode = A_MOVZX) and
                    (
                      (taicpu(hp1).opcode=A_AND) or
                      (taicpu(hp1).opcode=A_TEST)
                    ) and
                    not (
                      { If both are references, then the final instruction will have
                        both operands as references, which is not allowed }
                      (taicpu(p).oper[0]^.typ = top_ref) and
                      (taicpu(hp1).oper[0]^.typ = top_ref)
                    ) and
                    not RegUsed
                  ) or
                  (
                    (
                      SuperRegistersEqual(taicpu(p).oper[0]^.reg, taicpu(p).oper[1]^.reg) or
                      not RegUsed
                    ) and
                    (taicpu(p).oper[0]^.typ = top_reg) and
                    SuperRegistersEqual(taicpu(p).oper[0]^.reg, taicpu(p).oper[1]^.reg) and
                    (taicpu(hp1).oper[0]^.typ = top_const) and
                    ((taicpu(hp1).oper[0]^.val and Limit) = taicpu(hp1).oper[0]^.val)
                  ) then
                  begin
{$if defined(i386) or defined(i8086)}
                    { If the target size is 8-bit, make sure we can actually encode it }
                    if (NewRegSize = R_SUBL) and (taicpu(hp1).oper[0]^.typ = top_reg) and not (GetSupReg(taicpu(hp1).oper[0]^.reg) in [RS_EAX,RS_EBX,RS_ECX,RS_EDX]) then
                      Exit;
{$endif i386 or i8086}

                    DebugMsg(SPeepholeOptimization + 'MovxOp2Op 2',p);

                    taicpu(hp1).opsize := NewSize;

                    taicpu(hp1).loadoper(1, taicpu(p).oper[0]^);
                    if AndTest then
                      begin
                        RemoveInstruction(hp2);
                        if not RegUsed then
                          begin
                            taicpu(hp1).opcode := A_TEST;
                            if (taicpu(hp1).oper[0]^.typ = top_ref) then
                              begin
                                { Make sure the reference is the second operand }
                                SwapOper := taicpu(hp1).oper[0];
                                taicpu(hp1).oper[0] := taicpu(hp1).oper[1];
                                taicpu(hp1).oper[1] := SwapOper;
                              end;
                          end;
                      end;

                    case taicpu(hp1).oper[0]^.typ of
                      top_reg:
                        setsubreg(taicpu(hp1).oper[0]^.reg, NewRegSize);
                      top_const:
                        { For the AND/TEST case }
                        taicpu(hp1).oper[0]^.val := taicpu(hp1).oper[0]^.val and Limit;
                      else
                        ;
                    end;

                    if RegUsed then
                      begin
                        AsmL.Remove(p);
                        AsmL.InsertAfter(p, hp1);
                        p := hp1;
                      end
                    else
                      RemoveCurrentP(p, hp1);

                    result:=true;
                    exit;
                  end;
              end;
          end;

        if reg_and_hp1_is_instr and
          (taicpu(p).oper[0]^.typ = top_reg) and
          (
            (taicpu(hp1).opcode = A_SHL) or (taicpu(hp1).opcode = A_SAL)
          ) and
          (taicpu(hp1).oper[0]^.typ = top_const) and
          SuperRegistersEqual(taicpu(p).oper[0]^.reg, taicpu(p).oper[1]^.reg) and
          MatchOperand(taicpu(hp1).oper[1]^, taicpu(p).oper[1]^.reg) and
          { Minimum shift value allowed is the bit difference between the sizes }
          (taicpu(hp1).oper[0]^.val >=
            { Multiply by 8 because tcgsize2size returns bytes, not bits }
            8 * (
              tcgsize2size[reg_cgsize(taicpu(p).oper[1]^.reg)] -
              tcgsize2size[reg_cgsize(taicpu(p).oper[0]^.reg)]
            )
          ) then
          begin
            { For:
                movsx/movzx %reg1,%reg1 (same register, just different sizes)
                shl/sal     ##,   %reg1

              Remove the movsx/movzx instruction if the shift overwrites the
              extended bits of the register (e.g. movslq %eax,%rax; shlq $32,%rax
            }
            DebugMsg(SPeepholeOptimization + 'MovxShl2Shl',p);
            RemoveCurrentP(p, hp1);
            Result := True;
            Exit;
          end
        else if reg_and_hp1_is_instr and
          (taicpu(p).oper[0]^.typ = top_reg) and
          (
            ((taicpu(hp1).opcode = A_SHR) and (taicpu(p).opcode = A_MOVZX)) or
            ((taicpu(hp1).opcode = A_SAR) and (taicpu(p).opcode <> A_MOVZX))
          ) and
          (taicpu(hp1).oper[0]^.typ = top_const) and
          SuperRegistersEqual(taicpu(p).oper[0]^.reg, taicpu(p).oper[1]^.reg) and
          MatchOperand(taicpu(hp1).oper[1]^, taicpu(p).oper[1]^.reg) and
          { Minimum shift value allowed is the bit size of the smallest register - 1 }
          (taicpu(hp1).oper[0]^.val <
            { Multiply by 8 because tcgsize2size returns bytes, not bits }
            8 * (
              tcgsize2size[reg_cgsize(taicpu(p).oper[0]^.reg)]
            )
          ) then
          begin
            { For:
                movsx   %reg1,%reg1     movzx   %reg1,%reg1   (same register, just different sizes)
                sar     ##,   %reg1     shr     ##,   %reg1

              Move the shift to before the movx instruction if the shift value
              is not too large.
            }
            asml.Remove(hp1);
            asml.InsertBefore(hp1, p);

            taicpu(hp1).oper[1]^.reg := taicpu(p).oper[0]^.reg;

            case taicpu(p).opsize of
              s_BW, S_BL{$ifdef x86_64}, S_BQ{$endif}:
                taicpu(hp1).opsize := S_B;
              S_WL{$ifdef x86_64}, S_WQ{$endif}:
                taicpu(hp1).opsize := S_W;
              {$ifdef x86_64}
              S_LQ:
                taicpu(hp1).opsize := S_L;
              {$endif}
              else
                InternalError(2020112401);
            end;

            if (taicpu(hp1).opcode = A_SHR) then
              DebugMsg(SPeepholeOptimization + 'MovzShr2ShrMovz', hp1)
            else
              DebugMsg(SPeepholeOptimization + 'MovsSar2SarMovs', hp1);

            Result := True;
          end;

        if reg_and_hp1_is_instr and
          (taicpu(p).oper[0]^.typ = top_reg) and
          SuperRegistersEqual(taicpu(p).oper[0]^.reg, taicpu(p).oper[1]^.reg) and
          (
            (taicpu(hp1).opcode = taicpu(p).opcode)
            or ((taicpu(p).opcode = A_MOVZX) and ((taicpu(hp1).opcode = A_MOVSX){$ifdef x86_64} or (taicpu(hp1).opcode = A_MOVSXD){$endif x86_64}))
{$ifdef x86_64}
            or ((taicpu(p).opcode = A_MOVSX) and (taicpu(hp1).opcode = A_MOVSXD))
{$endif x86_64}
          ) then
          begin
            if MatchOpType(taicpu(hp1), top_reg, top_reg) and
              (taicpu(p).oper[1]^.reg = taicpu(hp1).oper[0]^.reg) and
              SuperRegistersEqual(taicpu(hp1).oper[0]^.reg, taicpu(hp1).oper[1]^.reg) then
              begin
                {
                  For example:
   	            movzbw  %al,%ax
    	            movzwl  %ax,%eax

                  Compress into:
 	            movzbl  %al,%eax
                }
                RegUsed := False;
                case taicpu(p).opsize of
                  S_BW:
                    case taicpu(hp1).opsize of
                      S_WL:
                        begin
                          taicpu(p).opsize := S_BL;
                          RegUsed := True;
                        end;
{$ifdef x86_64}
                      S_WQ:
                        begin
                          if taicpu(p).opcode = A_MOVZX then
                            begin
                              taicpu(p).opsize := S_BL;
                              { 64-bit zero extension is implicit, so change to the 32-bit register }
                              setsubreg(taicpu(hp1).oper[1]^.reg, R_SUBD);
                            end
                          else
                            taicpu(p).opsize := S_BQ;
                          RegUsed := True;
                        end;
{$endif x86_64}
                      else
                        ;
                    end;
{$ifdef x86_64}
                  S_BL:
                    case taicpu(hp1).opsize of
                      S_LQ:
                        begin
                          if taicpu(p).opcode = A_MOVZX then
                            begin
                              taicpu(p).opsize := S_BL;
                              { 64-bit zero extension is implicit, so change to the 32-bit register }
                              setsubreg(taicpu(hp1).oper[1]^.reg, R_SUBD);
                            end
                          else
                            taicpu(p).opsize := S_BQ;
                          RegUsed := True;
                        end;
                      else
                        ;
                    end;
                  S_WL:
                    case taicpu(hp1).opsize of
                      S_LQ:
                        begin
                          if taicpu(p).opcode = A_MOVZX then
                            begin
                              taicpu(p).opsize := S_WL;
                              { 64-bit zero extension is implicit, so change to the 32-bit register }
                              setsubreg(taicpu(hp1).oper[1]^.reg, R_SUBD);
                            end
                          else
                            taicpu(p).opsize := S_WQ;
                          RegUsed := True;
                        end;
                      else
                        ;
                    end;
{$endif x86_64}
                  else
                    ;
                end;

                if RegUsed then
                  begin
                    DebugMsg(SPeepholeOptimization + 'MovxMovx2Movx', p);
                    taicpu(p).oper[1]^.reg := taicpu(hp1).oper[1]^.reg;
                    RemoveInstruction(hp1);
                    Result := True;
                    Exit;
                  end;
              end;

            if (taicpu(hp1).opsize = taicpu(p).opsize) and
              not RegInInstruction(taicpu(p).oper[1]^.reg, hp1) and
              GetNextInstruction(hp1, hp2) and
              MatchInstruction(hp2, [A_AND, A_OR, A_XOR, A_TEST], []) and
              (
                ((taicpu(hp2).opsize = S_W) and (taicpu(p).opsize = S_BW)) or
                ((taicpu(hp2).opsize = S_L) and (taicpu(p).opsize in [S_BL, S_WL]))
{$ifdef x86_64}
                or ((taicpu(hp2).opsize = S_Q) and (taicpu(p).opsize in [S_BL, S_BQ, S_WL, S_WQ, S_LQ]))
{$endif x86_64}
              ) and
              MatchOpType(taicpu(hp2), top_reg, top_reg) and
              (
                (
                  (taicpu(hp2).oper[0]^.reg = taicpu(hp1).oper[1]^.reg) and
                  (taicpu(hp2).oper[1]^.reg = taicpu(p).oper[1]^.reg)
                ) or
                (
                  { Only allow the operands in reverse order for TEST instructions }
                  (taicpu(hp2).opcode = A_TEST) and
                  (taicpu(hp2).oper[0]^.reg = taicpu(p).oper[1]^.reg) and
                  (taicpu(hp2).oper[1]^.reg = taicpu(hp1).oper[1]^.reg)
                )
              ) then
              begin
                {
                  For example:
      	            movzbl  %al,%eax
      	            movzbl  (ref),%edx
      	            andl    %edx,%eax
                    (%edx deallocated)

                  Change to:
        	    andb	(ref),%al
        	    movzbl	%al,%eax

                  Rules are:
                  - First two instructions have the same opcode and opsize
                  - First instruction's operands are the same super-register
                  - Second instruction operates on a different register
                  - Third instruction is AND, OR, XOR or TEST
                  - Third instruction's operands are the destination registers of the first two instructions
                  - Third instruction writes to the destination register of the first instruction (except with TEST)
                  - Second instruction's destination register is deallocated afterwards
                }
                TransferUsedRegs(TmpUsedRegs);
                UpdateUsedRegs(TmpUsedRegs, tai(p.Next));
                UpdateUsedRegs(TmpUsedRegs, tai(hp1.Next));
                if not RegUsedAfterInstruction(taicpu(hp1).oper[1]^.reg, hp2, TmpUsedRegs) then
                  begin
                    case taicpu(p).opsize of
                      S_BW, S_BL{$ifdef x86_64}, S_BQ{$endif x86_64}:
                        NewSize := S_B;
                      S_WL{$ifdef x86_64}, S_WQ{$endif x86_64}:
                        NewSize := S_W;
{$ifdef x86_64}
                      S_LQ:
                        NewSize := S_L;
{$endif x86_64}
                      else
                        InternalError(2021120301);
                    end;

                    taicpu(hp2).loadoper(0, taicpu(hp1).oper[0]^);
                    taicpu(hp2).loadreg(1, taicpu(p).oper[0]^.reg);
                    taicpu(hp2).opsize := NewSize;

                    RemoveInstruction(hp1);

                    { With TEST, it's best to keep the MOVX instruction at the top }
                    if (taicpu(hp2).opcode <> A_TEST) then
                      begin
                        DebugMsg(SPeepholeOptimization + 'MovxMovxTest2MovxTest', p);
                        asml.Remove(p);
                         { If the third instruction uses the flags, the MOVX instruction won't modify then }
                        asml.InsertAfter(p, hp2);
                        p := hp2;
                      end
                    else
                      DebugMsg(SPeepholeOptimization + 'MovxMovxOp2OpMovx', p);

                    Result := True;
                    Exit;
                  end;
              end;
          end;

        if taicpu(p).opcode=A_MOVZX then
          begin
            { removes superfluous And's after movzx's }
            if reg_and_hp1_is_instr and
              (taicpu(hp1).opcode = A_AND) and
              MatchOpType(taicpu(hp1),top_const,top_reg) and
              ((taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg)
{$ifdef x86_64}
               { check for implicit extension to 64 bit }
               or
               ((taicpu(p).opsize in [S_BL,S_WL]) and
                (taicpu(hp1).opsize=S_Q) and
                SuperRegistersEqual(taicpu(p).oper[1]^.reg,taicpu(hp1).oper[1]^.reg)
               )
{$endif x86_64}
              )
              then
              begin
                case taicpu(p).opsize Of
                  S_BL, S_BW{$ifdef x86_64}, S_BQ{$endif x86_64}:
                    if (taicpu(hp1).oper[0]^.val = $ff) then
                      begin
                        DebugMsg(SPeepholeOptimization + 'MovzAnd2Movz1',p);
                        RemoveInstruction(hp1);
                        Result:=true;
                        exit;
                      end;
                    S_WL{$ifdef x86_64}, S_WQ{$endif x86_64}:
                      if (taicpu(hp1).oper[0]^.val = $ffff) then
                        begin
                          DebugMsg(SPeepholeOptimization + 'MovzAnd2Movz2',p);
                          RemoveInstruction(hp1);
                          Result:=true;
                          exit;
                        end;
{$ifdef x86_64}
                    S_LQ:
                      if (taicpu(hp1).oper[0]^.val = $ffffffff) then
                        begin
                          DebugMsg(SPeepholeOptimization + 'MovzAnd2Movz3',p);
                          RemoveInstruction(hp1);
                          Result:=true;
                          exit;
                        end;
{$endif x86_64}
                    else
                      ;
                end;
                { we cannot get rid of the and, but can we get rid of the movz ?}
                if SuperRegistersEqual(taicpu(p).oper[0]^.reg,taicpu(p).oper[1]^.reg) then
                  begin
                    case taicpu(p).opsize Of
                      S_BL, S_BW{$ifdef x86_64}, S_BQ{$endif x86_64}:
                        if (taicpu(hp1).oper[0]^.val and $ff)=taicpu(hp1).oper[0]^.val then
                          begin
                            DebugMsg(SPeepholeOptimization + 'MovzAnd2And1',p);
                            RemoveCurrentP(p,hp1);
                            Result:=true;
                            exit;
                          end;
                        S_WL{$ifdef x86_64}, S_WQ{$endif x86_64}:
                          if (taicpu(hp1).oper[0]^.val and $ffff)=taicpu(hp1).oper[0]^.val then
                            begin
                              DebugMsg(SPeepholeOptimization + 'MovzAnd2And2',p);
                              RemoveCurrentP(p,hp1);
                              Result:=true;
                              exit;
                            end;
{$ifdef x86_64}
                        S_LQ:
                          if (taicpu(hp1).oper[0]^.val and $ffffffff)=taicpu(hp1).oper[0]^.val then
                            begin
                              DebugMsg(SPeepholeOptimization + 'MovzAnd2And3',p);
                              RemoveCurrentP(p,hp1);
                              Result:=true;
                              exit;
                            end;
{$endif x86_64}
                        else
                          ;
                    end;
                end;
              end;
            { changes some movzx constructs to faster synonyms (all examples
              are given with eax/ax, but are also valid for other registers)}
            if MatchOpType(taicpu(p),top_reg,top_reg) then
                begin
                  case taicpu(p).opsize of
                    { Technically, movzbw %al,%ax cannot be encoded in 32/64-bit mode
                      (the machine code is equivalent to movzbl %al,%eax), but the
                      code generator still generates that assembler instruction and
                      it is silently converted.  This should probably be checked.
                      [Kit] }
                    S_BW:
                      begin
                        if (getsupreg(taicpu(p).oper[0]^.reg)=getsupreg(taicpu(p).oper[1]^.reg)) and
                          (
                            not IsMOVZXAcceptable
                            { and $0xff,%ax has a smaller encoding but risks a partial write penalty }
                            or (
                              (cs_opt_size in current_settings.optimizerswitches) and
                              (taicpu(p).oper[1]^.reg = NR_AX)
                            )
                          ) then
                          {Change "movzbw %al, %ax" to "andw $0x0ffh, %ax"}
                          begin
                            DebugMsg(SPeepholeOptimization + 'var7',p);
                            taicpu(p).opcode := A_AND;
                            taicpu(p).changeopsize(S_W);
                            taicpu(p).loadConst(0,$ff);
                            Result := True;
                          end
                        else if not IsMOVZXAcceptable and
                          GetNextInstruction(p, hp1) and
                          (tai(hp1).typ = ait_instruction) and
                          (taicpu(hp1).opcode = A_AND) and
                          MatchOpType(taicpu(hp1),top_const,top_reg) and
                          (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
                        { Change "movzbw %reg1, %reg2; andw $const, %reg2"
                          to "movw %reg1, reg2; andw $(const1 and $ff), %reg2"}
                          begin
                            DebugMsg(SPeepholeOptimization + 'var8',p);
                            taicpu(p).opcode := A_MOV;
                            taicpu(p).changeopsize(S_W);
                            setsubreg(taicpu(p).oper[0]^.reg,R_SUBW);
                            taicpu(hp1).loadConst(0,taicpu(hp1).oper[0]^.val and $ff);
                            Result := True;
                          end;
                      end;
{$ifndef i8086} { movzbl %al,%eax cannot be encoded in 16-bit mode (the machine code is equivalent to movzbw %al,%ax }
                    S_BL:
                      if not IsMOVZXAcceptable then
                        begin
                          if (getsupreg(taicpu(p).oper[0]^.reg)=getsupreg(taicpu(p).oper[1]^.reg)) then
                            { Change "movzbl %al, %eax" to "andl $0x0ffh, %eax" }
                            begin
                              DebugMsg(SPeepholeOptimization + 'var9',p);
                              taicpu(p).opcode := A_AND;
                              taicpu(p).changeopsize(S_L);
                              taicpu(p).loadConst(0,$ff);
                              Result := True;
                            end
                          else if GetNextInstruction(p, hp1) and
                            (tai(hp1).typ = ait_instruction) and
                            (taicpu(hp1).opcode = A_AND) and
                            MatchOpType(taicpu(hp1),top_const,top_reg) and
                            (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
                            { Change "movzbl %reg1, %reg2; andl $const, %reg2"
                              to "movl %reg1, reg2; andl $(const1 and $ff), %reg2"}
                            begin
                              DebugMsg(SPeepholeOptimization + 'var10',p);
                              taicpu(p).opcode := A_MOV;
                              taicpu(p).changeopsize(S_L);
                              { do not use R_SUBWHOLE
                                as movl %rdx,%eax
                                is invalid in assembler PM }
                              setsubreg(taicpu(p).oper[0]^.reg, R_SUBD);
                              taicpu(hp1).loadConst(0,taicpu(hp1).oper[0]^.val and $ff);
                              Result := True;
                            end;
                        end;
{$endif i8086}
                    S_WL:
                      if not IsMOVZXAcceptable then
                        begin
                          if (getsupreg(taicpu(p).oper[0]^.reg)=getsupreg(taicpu(p).oper[1]^.reg)) then
                            { Change "movzwl %ax, %eax" to "andl $0x0ffffh, %eax" }
                            begin
                              DebugMsg(SPeepholeOptimization + 'var11',p);
                              taicpu(p).opcode := A_AND;
                              taicpu(p).changeopsize(S_L);
                              taicpu(p).loadConst(0,$ffff);
                              Result := True;
                            end
                          else if GetNextInstruction(p, hp1) and
                            (tai(hp1).typ = ait_instruction) and
                            (taicpu(hp1).opcode = A_AND) and
                            (taicpu(hp1).oper[0]^.typ = top_const) and
                            (taicpu(hp1).oper[1]^.typ = top_reg) and
                            (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
                            { Change "movzwl %reg1, %reg2; andl $const, %reg2"
                              to "movl %reg1, reg2; andl $(const1 and $ffff), %reg2"}
                            begin
                              DebugMsg(SPeepholeOptimization + 'var12',p);
                              taicpu(p).opcode := A_MOV;
                              taicpu(p).changeopsize(S_L);
                              { do not use R_SUBWHOLE
                                as movl %rdx,%eax
                                is invalid in assembler PM }
                              setsubreg(taicpu(p).oper[0]^.reg, R_SUBD);
                              taicpu(hp1).loadConst(0,taicpu(hp1).oper[0]^.val and $ffff);
                              Result := True;
                            end;
                        end;
                    else
                      InternalError(2017050705);
                  end;
                end
              else if not IsMOVZXAcceptable and (taicpu(p).oper[0]^.typ = top_ref) then
                  begin
                    if GetNextInstruction(p, hp1) and
                      (tai(hp1).typ = ait_instruction) and
                      (taicpu(hp1).opcode = A_AND) and
                      MatchOpType(taicpu(hp1),top_const,top_reg) and
                      (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
                      begin
                        //taicpu(p).opcode := A_MOV;
                        case taicpu(p).opsize Of
                          S_BL:
                            begin
                              DebugMsg(SPeepholeOptimization + 'var13',p);
                              taicpu(hp1).changeopsize(S_L);
                              taicpu(hp1).loadConst(0,taicpu(hp1).oper[0]^.val and $ff);
                            end;
                          S_WL:
                            begin
                              DebugMsg(SPeepholeOptimization + 'var14',p);
                              taicpu(hp1).changeopsize(S_L);
                              taicpu(hp1).loadConst(0,taicpu(hp1).oper[0]^.val and $ffff);
                            end;
                          S_BW:
                            begin
                              DebugMsg(SPeepholeOptimization + 'var15',p);
                              taicpu(hp1).changeopsize(S_W);
                              taicpu(hp1).loadConst(0,taicpu(hp1).oper[0]^.val and $ff);
                            end;
                          else
                            Internalerror(2017050704)
                        end;
                        Result := True;
                      end;
                  end;
          end;
      end;


    function TX86AsmOptimizer.OptPass1AND(var p : tai) : boolean;
      var
        hp1, hp2 : tai;
        MaskLength : Cardinal;
        MaskedBits : TCgInt;
        ActiveReg : TRegister;
      begin
        Result:=false;

        { There are no optimisations for reference targets }
        if (taicpu(p).oper[1]^.typ <> top_reg) then
          Exit;

        while GetNextInstruction(p, hp1) and
          (hp1.typ = ait_instruction) do
          begin
            if (taicpu(p).oper[0]^.typ = top_const) then
              begin
                case taicpu(hp1).opcode of
                  A_AND:
                    if MatchOpType(taicpu(hp1),top_const,top_reg) and
                      (getsupreg(taicpu(p).oper[1]^.reg) = getsupreg(taicpu(hp1).oper[1]^.reg)) and
                      { the second register must contain the first one, so compare their subreg types }
                      (getsubreg(taicpu(p).oper[1]^.reg)<=getsubreg(taicpu(hp1).oper[1]^.reg)) and
                      (abs(taicpu(p).oper[0]^.val and taicpu(hp1).oper[0]^.val)<$80000000) then
                      { change
                          and const1, reg
                          and const2, reg
                        to
                          and (const1 and const2), reg
                      }
                      begin
                        taicpu(hp1).loadConst(0, taicpu(p).oper[0]^.val and taicpu(hp1).oper[0]^.val);
                        DebugMsg(SPeepholeOptimization + 'AndAnd2And done',hp1);
                        RemoveCurrentP(p, hp1);
                        Result:=true;
                        exit;
                      end;

                  A_CMP:
                    if (PopCnt(DWord(taicpu(p).oper[0]^.val)) = 1) and { Only 1 bit set }
                      MatchOperand(taicpu(hp1).oper[0]^, taicpu(p).oper[0]^.val) and
                      MatchOperand(taicpu(hp1).oper[1]^, taicpu(p).oper[1]^.reg) and
                      { Just check that the condition on the next instruction is compatible }
                      GetNextInstruction(hp1, hp2) and
                      (hp2.typ = ait_instruction) and
                      (taicpu(hp2).condition in [C_Z, C_E, C_NZ, C_NE])
                      then
                        { change
                            and  2^n, reg
                            cmp  2^n, reg
                            j(c) / set(c) / cmov(c)   (c is equal or not equal)
                          to
                            and  2^n, reg
                            test reg, reg
                            j(~c) / set(~c) / cmov(~c)
                        }
                      begin
                        { Keep TEST instruction in, rather than remove it, because
                          it may trigger other optimisations such as MovAndTest2Test }
                        taicpu(hp1).loadreg(0, taicpu(hp1).oper[1]^.reg);
                        taicpu(hp1).opcode := A_TEST;
                        DebugMsg(SPeepholeOptimization + 'AND/CMP/J(c) -> AND/J(~c) with power of 2 constant', p);
                        taicpu(hp2).condition := inverse_cond(taicpu(hp2).condition);
                        Result := True;
                        Exit;
                      end
                    else if ((taicpu(p).oper[0]^.val=$ff) or (taicpu(p).oper[0]^.val=$ffff) or (taicpu(p).oper[0]^.val=$ffffffff)) and
                      MatchOpType(taicpu(hp1),top_const,top_reg) and
                      (taicpu(p).oper[0]^.val>=taicpu(hp1).oper[0]^.val) and
                      SuperRegistersEqual(taicpu(p).oper[1]^.reg,taicpu(hp1).oper[1]^.reg) then
                        { change
                            and  $ff/$ff/$ffff, reg
                            cmp  val<=$ff/val<=$ffff/val<=$ffffffff, reg
                            dealloc reg
                          to
                            cmp  val<=$ff/val<=$ffff/val<=$ffffffff, resized reg
                        }
                      begin
                        TransferUsedRegs(TmpUsedRegs);
                        UpdateUsedRegs(TmpUsedRegs, tai(p.Next));
                        if not RegUsedAfterInstruction(taicpu(p).oper[1]^.reg, hp1, TmpUsedRegs) then
                          begin
                            DebugMsg(SPeepholeOptimization + 'AND/CMP -> CMP', p);
                            case taicpu(p).oper[0]^.val of
                              $ff:
                                begin
                                  setsubreg(taicpu(hp1).oper[1]^.reg, R_SUBL);
                                  taicpu(hp1).opsize:=S_B;
                                end;
                              $ffff:
                                begin
                                  setsubreg(taicpu(hp1).oper[1]^.reg, R_SUBW);
                                  taicpu(hp1).opsize:=S_W;
                                end;
                              $ffffffff:
                                begin
                                  setsubreg(taicpu(hp1).oper[1]^.reg, R_SUBD);
                                  taicpu(hp1).opsize:=S_L;
                                end;
                              else
                                Internalerror(2023030401);
                            end;
                            RemoveCurrentP(p);
                            Result := True;
                            Exit;
                          end;
                      end;

                  A_MOVZX:
                    if MatchOpType(taicpu(hp1),top_reg,top_reg) and
                      SuperRegistersEqual(taicpu(p).oper[1]^.reg,taicpu(hp1).oper[1]^.reg) and
                      (getsupreg(taicpu(hp1).oper[0]^.reg)=getsupreg(taicpu(hp1).oper[1]^.reg)) and
                      (
                        (
                          (taicpu(p).opsize=S_W) and
                          (taicpu(hp1).opsize=S_BW)
                        ) or
                        (
                          (taicpu(p).opsize=S_L) and
                          (taicpu(hp1).opsize in [S_WL,S_BL{$ifdef x86_64},S_BQ,S_WQ{$endif x86_64}])
                        )
{$ifdef x86_64}
                        or
                        (
                          (taicpu(p).opsize=S_Q) and
                          (taicpu(hp1).opsize in [S_BQ,S_WQ,S_BL,S_WL])
                        )
{$endif x86_64}
                      ) then
                      begin
                        if (((taicpu(hp1).opsize) in [S_BW,S_BL{$ifdef x86_64},S_BQ{$endif x86_64}]) and
                            ((taicpu(p).oper[0]^.val and $ff)=taicpu(p).oper[0]^.val)
                             ) or
                           (((taicpu(hp1).opsize) in [S_WL{$ifdef x86_64},S_WQ{$endif x86_64}]) and
                            ((taicpu(p).oper[0]^.val and $ffff)=taicpu(p).oper[0]^.val))
                        then
                          begin
                            { Unlike MOVSX, MOVZX doesn't actually have a version that zero-extends a
                              32-bit register to a 64-bit register, or even a version called MOVZXD, so
                              code that tests for the presence of AND 0xffffffff followed by MOVZX is
                              wasted, and is indictive of a compiler bug if it were triggered. [Kit]

                              NOTE: To zero-extend from 32 bits to 64 bits, simply use the standard MOV.
                            }
                            DebugMsg(SPeepholeOptimization + 'AndMovzToAnd done',p);

                            RemoveInstruction(hp1);

                            { See if there are other optimisations possible }
                            Continue;
                          end;
                      end;

                  A_SHL:
                    if MatchOpType(taicpu(hp1),top_const,top_reg) and
                      (getsupreg(taicpu(p).oper[1]^.reg)=getsupreg(taicpu(hp1).oper[1]^.reg)) then
                      begin
{$ifopt R+}
{$define RANGE_WAS_ON}
{$R-}
{$endif}
                        { get length of potential and mask }
                        MaskLength:=SizeOf(taicpu(p).oper[0]^.val)*8-BsrQWord(taicpu(p).oper[0]^.val)-1;

                        { really a mask? }
{$ifdef RANGE_WAS_ON}
{$R+}
{$endif}
                        if (((QWord(1) shl MaskLength)-1)=taicpu(p).oper[0]^.val) and
                          { unmasked part shifted out? }
                          ((MaskLength+taicpu(hp1).oper[0]^.val)>=topsize2memsize[taicpu(hp1).opsize]) then
                          begin
                            DebugMsg(SPeepholeOptimization + 'AndShlToShl done',p);
                            RemoveCurrentP(p, hp1);
                            Result:=true;
                            exit;
                          end;
                      end;

                  A_SHR:
                    if MatchOpType(taicpu(hp1),top_const,top_reg) and
                      (taicpu(p).oper[1]^.reg = taicpu(hp1).oper[1]^.reg) and
                      (taicpu(hp1).oper[0]^.val <= 63) then
                      begin
                        { Does SHR combined with the AND cover all the bits?

                          e.g. for "andb $252,%reg; shrb $2,%reg" - the "and" can be removed }

                        MaskedBits := taicpu(p).oper[0]^.val or ((TCgInt(1) shl taicpu(hp1).oper[0]^.val) - 1);

                        if ((taicpu(p).opsize = S_B) and ((MaskedBits and $FF) = $FF)) or
                          ((taicpu(p).opsize = S_W) and ((MaskedBits and $FFFF) = $FFFF)) or
                          ((taicpu(p).opsize = S_L) and ((MaskedBits and $FFFFFFFF) = $FFFFFFFF)) then
                          begin
                            DebugMsg(SPeepholeOptimization + 'AndShrToShr done', p);
                            RemoveCurrentP(p, hp1);
                            Result := True;
                            Exit;
                          end;
                      end;

                  A_MOVSX{$ifdef x86_64}, A_MOVSXD{$endif x86_64}:
                    if (taicpu(hp1).oper[0]^.typ = top_reg) and
                      SuperRegistersEqual(taicpu(hp1).oper[0]^.reg, taicpu(hp1).oper[1]^.reg) then
                      begin
                        if SuperRegistersEqual(taicpu(p).oper[1]^.reg, taicpu(hp1).oper[1]^.reg) and
                          (
                            (
                              (taicpu(hp1).opsize in [S_BW,S_BL{$ifdef x86_64},S_BQ{$endif x86_64}]) and
                              ((taicpu(p).oper[0]^.val and $7F) = taicpu(p).oper[0]^.val)
                            ) or (
                              (taicpu(hp1).opsize in [S_WL{$ifdef x86_64},S_WQ{$endif x86_64}]) and
                              ((taicpu(p).oper[0]^.val and $7FFF) = taicpu(p).oper[0]^.val)
{$ifdef x86_64}
                            ) or (
                              (taicpu(hp1).opsize = S_LQ) and
                              ((taicpu(p).oper[0]^.val and $7fffffff) = taicpu(p).oper[0]^.val)
{$endif x86_64}
                            )
                          ) then
                          begin
                            if (taicpu(p).oper[1]^.reg = taicpu(hp1).oper[1]^.reg){$ifdef x86_64} or (taicpu(hp1).opsize = S_LQ){$endif x86_64} then
                              begin
                                DebugMsg(SPeepholeOptimization + 'AndMovsxToAnd',p);
                                RemoveInstruction(hp1);
                                { See if there are other optimisations possible }
                                Continue;
                              end;

                            { The super-registers are the same though.

                              Note that this change by itself doesn't improve
                              code speed, but it opens up other optimisations. }
{$ifdef x86_64}
                            { Convert 64-bit register to 32-bit }
                            case taicpu(hp1).opsize of
                              S_BQ:
                                begin
                                  taicpu(hp1).opsize := S_BL;
                                  taicpu(hp1).oper[1]^.reg := newreg(R_INTREGISTER, getsupreg(taicpu(hp1).oper[1]^.reg), R_SUBD);
                                end;
                              S_WQ:
                                begin
                                  taicpu(hp1).opsize := S_WL;
                                  taicpu(hp1).oper[1]^.reg := newreg(R_INTREGISTER, getsupreg(taicpu(hp1).oper[1]^.reg), R_SUBD);
                                end
                              else
                                ;
                            end;
{$endif x86_64}
                            DebugMsg(SPeepholeOptimization + 'AndMovsxToAndMovzx', hp1);
                            taicpu(hp1).opcode := A_MOVZX;
                            { See if there are other optimisations possible }
                            Continue;
                          end;
                      end;
                  else
                    ;
                end;
              end
            else if MatchOperand(taicpu(p).oper[0]^, taicpu(p).oper[1]^.reg) and
              not RegInUsedRegs(NR_DEFAULTFLAGS, UsedRegs) then
              begin
{$ifdef x86_64}
                if (taicpu(p).opsize = S_Q) then
                  begin
                    { Never necessary }
                    DebugMsg(SPeepholeOptimization + 'Andq2Nop', p);
                    RemoveCurrentP(p, hp1);
                    Result := True;
                    Exit;
                  end;
{$endif x86_64}
                { Forward check to determine necessity of and %reg,%reg }
                TransferUsedRegs(TmpUsedRegs);
                UpdateUsedRegs(TmpUsedRegs, tai(p.Next));

                { Saves on a bunch of dereferences }
                ActiveReg := taicpu(p).oper[1]^.reg;

                case taicpu(hp1).opcode of
                  A_MOV, A_MOVZX, A_MOVSX{$ifdef x86_64}, A_MOVSXD{$endif x86_64}:

                    if (
                        (taicpu(hp1).oper[0]^.typ <> top_ref) or
                        not RegInRef(ActiveReg, taicpu(hp1).oper[0]^.ref^)
                      ) and
                      (
                        (taicpu(hp1).opcode <> A_MOV) or
                        (taicpu(hp1).oper[1]^.typ <> top_ref) or
                        not RegInRef(ActiveReg, taicpu(hp1).oper[1]^.ref^)
                      ) and
                      not (
                        { If mov %reg,%reg is present, remove that instruction instead in OptPass1MOV }
                        (taicpu(hp1).opcode = A_MOV) and
                        MatchOperand(taicpu(hp1).oper[0]^, ActiveReg) and
                        MatchOperand(taicpu(hp1).oper[1]^, ActiveReg)
                      ) and
                      (
                        (
                          (taicpu(hp1).oper[0]^.typ = top_reg) and
                          (taicpu(hp1).oper[0]^.reg = ActiveReg) and
                          SuperRegistersEqual(taicpu(hp1).oper[0]^.reg, taicpu(hp1).oper[1]^.reg)
                        ) or
                        (
{$ifdef x86_64}
                          (
                            { If we read from the register, make sure it's not dependent on the upper 32 bits }
                            (taicpu(hp1).oper[0]^.typ <> top_reg) or
                            not SuperRegistersEqual(taicpu(hp1).oper[0]^.reg, ActiveReg) or
                            (GetSubReg(taicpu(hp1).oper[0]^.reg) <> R_SUBQ)
                          ) and
{$endif x86_64}
                          not RegUsedAfterInstruction(ActiveReg, hp1, TmpUsedRegs)
                        )
                      ) then
                      begin
                        DebugMsg(SPeepholeOptimization + 'AndMovx2Movx', p);
                        RemoveCurrentP(p, hp1);
                        Result := True;
                        Exit;
                      end;
                  A_ADD,
                  A_AND,
                  A_BSF,
                  A_BSR,
                  A_BTC,
                  A_BTR,
                  A_BTS,
                  A_OR,
                  A_SUB,
                  A_XOR:
                    { Register is written to, so this will clear the upper 32 bits (2-operand instructions) }
                    if (
                        (taicpu(hp1).oper[0]^.typ <> top_ref) or
                        not RegInRef(ActiveReg, taicpu(hp1).oper[0]^.ref^)
                      ) and
                      MatchOperand(taicpu(hp1).oper[1]^, ActiveReg) then
                      begin
                        DebugMsg(SPeepholeOptimization + 'AndOp2Op 2', p);
                        RemoveCurrentP(p, hp1);
                        Result := True;
                        Exit;
                      end;
                  A_CMP,
                  A_TEST:
                    if (
                        (taicpu(hp1).oper[0]^.typ <> top_ref) or
                        not RegInRef(ActiveReg, taicpu(hp1).oper[0]^.ref^)
                      ) and
                      MatchOperand(taicpu(hp1).oper[1]^, ActiveReg) and
                      not RegUsedAfterInstruction(ActiveReg, hp1, TmpUsedRegs) then
                      begin
                        DebugMsg(SPeepholeOptimization + 'AND; CMP/TEST -> CMP/TEST', p);
                        RemoveCurrentP(p, hp1);
                        Result := True;
                        Exit;
                      end;
                  A_BSWAP,
                  A_NEG,
                  A_NOT:
                  { Register is written to, so this will clear the upper 32 bits (1-operand instructions) }
                  if MatchOperand(taicpu(hp1).oper[0]^, ActiveReg) then
                    begin
                      DebugMsg(SPeepholeOptimization + 'AndOp2Op 1', p);
                      RemoveCurrentP(p, hp1);
                      Result := True;
                      Exit;
                    end;
                  else
                    ;
                end;
              end;

            if (taicpu(hp1).is_jmp) and
              (taicpu(hp1).opcode<>A_JMP) and
              not(RegInUsedRegs(taicpu(p).oper[1]^.reg,UsedRegs)) then
              begin
                { change
                    and x, reg
                    jxx
                  to
                    test x, reg
                    jxx
                  if reg is deallocated before the
                  jump, but only if it's a conditional jump (PFV)
                }
                DebugMsg(SPeepholeOptimization + 'AndJcc2TestJcc', p);
                taicpu(p).opcode := A_TEST;
                Exit;
              end;

            Break;
          end;

        { Lone AND tests }
        if (taicpu(p).oper[0]^.typ = top_const) then
          begin
            {
              - Convert and $0xFF,reg to and reg,reg if reg is 8-bit
              - Convert and $0xFFFF,reg to and reg,reg if reg is 16-bit
              - Convert and $0xFFFFFFFF,reg to and reg,reg if reg is 32-bit
            }
            if ((taicpu(p).oper[0]^.val = $FF) and (taicpu(p).opsize = S_B)) or
              ((taicpu(p).oper[0]^.val = $FFFF) and (taicpu(p).opsize = S_W)) or
              ((taicpu(p).oper[0]^.val = $FFFFFFFF) and (taicpu(p).opsize = S_L)) then
              begin
                taicpu(p).loadreg(0, taicpu(p).oper[1]^.reg);
                if taicpu(p).opsize = S_L then
                  begin
                    Include(OptsToCheck,aoc_MovAnd2Mov_3);
                    Result := True;
                  end;
              end;
          end;

        { Backward check to determine necessity of and %reg,%reg }
        if (taicpu(p).oper[0]^.typ = top_reg) and
          (taicpu(p).oper[0]^.reg = taicpu(p).oper[1]^.reg) and
          not RegInUsedRegs(NR_DEFAULTFLAGS, UsedRegs) and
          GetLastInstruction(p, hp2) and
          RegModifiedByInstruction(taicpu(p).oper[1]^.reg, hp2) and
          { Check size of adjacent instruction to determine if the AND is
            effectively a null operation }
          (
            (taicpu(p).opsize = taicpu(hp2).opsize) or
            { Note: Don't include S_Q }
            ((taicpu(p).opsize = S_L) and (taicpu(hp2).opsize in [S_BL, S_WL])) or
            ((taicpu(p).opsize = S_W) and (taicpu(hp2).opsize in [S_BW, S_BL, S_WL, S_L])) or
            ((taicpu(p).opsize = S_B) and (taicpu(hp2).opsize in [S_BW, S_BL, S_WL, S_W, S_L]))
          ) then
          begin
            DebugMsg(SPeepholeOptimization + 'And2Nop', p);
            { If GetNextInstruction returned False, hp1 will be nil }
            RemoveCurrentP(p, hp1);
            Result := True;
            Exit;
          end;

      end;


    function TX86AsmOptimizer.OptPass2ADD(var p : tai) : boolean;
      var
        hp1, hp2: tai;
        NewRef: TReference;
        Distance: Cardinal;
        TempTracking: TAllUsedRegs;

        { This entire nested function is used in an if-statement below, but we
          want to avoid all the used reg transfers and GetNextInstruction calls
          until we really have to check }
        function MemRegisterNotUsedLater: Boolean; inline;
          var
            hp2: tai;
          begin
            TransferUsedRegs(TmpUsedRegs);
            hp2 := p;
            repeat
              UpdateUsedRegs(TmpUsedRegs, tai(hp2.Next));
            until not (cs_opt_level3 in current_settings.optimizerswitches) or not GetNextInstruction(hp2, hp2) or (hp2 = hp1);

            Result := not RegUsedAfterInstruction(taicpu(p).oper[1]^.reg, hp1, TmpUsedRegs);
          end;

      begin
        Result := False;

        if (taicpu(p).opsize in [S_L{$ifdef x86_64}, S_Q{$endif}]) and
          (taicpu(p).oper[1]^.typ = top_reg) then
          begin
            Distance := GetNextInstructionUsingRegCount(p, hp1, taicpu(p).oper[1]^.reg);
            if (Distance = 0) or (Distance > 3) { Likely too far to make a meaningful difference } or
              (hp1.typ <> ait_instruction) or
              not
              (
                (cs_opt_level3 in current_settings.optimizerswitches) or
                { GetNextInstructionUsingRegCount just returns the next valid instruction under -O2 and under }
                RegInInstruction(taicpu(p).oper[1]^.reg, hp1)
              ) then
              Exit;

            { Some of the MOV optimisations are much more in-depth.  For example, if we have:
                addq $x,   %rax
                movq %rax, %rdx
                sarq $63,  %rdx
                (%rax still in use)

              ...letting OptPass2ADD run its course (and without -Os) will produce:
                leaq $x(%rax),%rdx
                addq $x,   %rax
                sarq $63,  %rdx

              ...which is okay since it breaks the dependency chain between
                addq and movq, but if OptPass2MOV is called first:

                addq $x,   %rax
                cqto

              ...which is better in all ways, taking only 2 cycles to execute
                and much smaller in code size.
            }

            { The extra register tracking is quite strenuous }
            if (cs_opt_level2 in current_settings.optimizerswitches) and
              MatchInstruction(hp1, A_MOV, []) then
              begin
                { Update the register tracking to the MOV instruction }
                CopyUsedRegs(TempTracking);
                hp2 := p;
                repeat
                  UpdateUsedRegs(tai(hp2.Next));
                until not (cs_opt_level3 in current_settings.optimizerswitches) or not GetNextInstruction(hp2, hp2) or (hp2 = hp1);

                { if hp1 <> hp2 after the call, then hp1 got removed, so let
                  OptPass2ADD get called again }
                if OptPass2MOV(hp1) and (hp1 <> hp2) then
                  begin
                    { Reset the tracking to the current instruction }
                    RestoreUsedRegs(TempTracking);
                    ReleaseUsedRegs(TempTracking);

                    Result := True;
                    Exit;
                  end;

                { Reset the tracking to the current instruction }
                RestoreUsedRegs(TempTracking);
                ReleaseUsedRegs(TempTracking);

                { If OptPass2MOV returned True, we don't need to set Result to
                  True if hp1 didn't change because the ADD instruction didn't
                  get modified and we'll be evaluating hp1 again when the
                  peephole optimizer reaches it }
              end;

            { Change:
                add     %reg2,%reg1
                (%reg2 not modified in between)
                mov/s/z #(%reg1),%reg1  (%reg1 superregisters must be the same)

              To:
                mov/s/z #(%reg1,%reg2),%reg1
            }
            if (taicpu(p).oper[0]^.typ = top_reg) and
              MatchInstruction(hp1, [A_MOV, A_MOVZX, A_MOVSX{$ifdef x86_64}, A_MOVSXD{$endif}], []) and
              MatchOpType(taicpu(hp1), top_ref, top_reg) and
              (taicpu(hp1).oper[0]^.ref^.scalefactor <= 1) and
              (
                (
                  (taicpu(hp1).oper[0]^.ref^.base = taicpu(p).oper[1]^.reg) and
                  (taicpu(hp1).oper[0]^.ref^.index = NR_NO) and
                  { r/esp cannot be an index }
                  (taicpu(p).oper[0]^.reg<>NR_STACK_POINTER_REG)
                ) or (
                  (taicpu(hp1).oper[0]^.ref^.index = taicpu(p).oper[1]^.reg) and
                  (taicpu(hp1).oper[0]^.ref^.base = NR_NO)
                )
              ) and (
                Reg1WriteOverwritesReg2Entirely(taicpu(p).oper[1]^.reg, taicpu(hp1).oper[1]^.reg) or
                (
                  { If the super registers ARE equal, then this MOV/S/Z does a partial write }
                  not SuperRegistersEqual(taicpu(p).oper[1]^.reg, taicpu(hp1).oper[1]^.reg) and
                  MemRegisterNotUsedLater
                )
              ) then
                begin

                if (
                  { Instructions are guaranteed to be adjacent on -O2 and under }
                  (cs_opt_level3 in current_settings.optimizerswitches) and
                  RegModifiedBetween(taicpu(p).oper[0]^.reg, p, hp1)
                ) then
                  begin
                    { If the other register is used in between, move the MOV
                      instruction to right after the ADD instruction so a
                      saving can still be made }
                    Asml.Remove(hp1);
                    Asml.InsertAfter(hp1, p);

                    taicpu(hp1).oper[0]^.ref^.base := taicpu(p).oper[1]^.reg;
                    taicpu(hp1).oper[0]^.ref^.index := taicpu(p).oper[0]^.reg;

                    DebugMsg(SPeepholeOptimization + 'AddMov2Mov done (instruction moved)', p);

                    RemoveCurrentp(p, hp1);
                  end
                else
                  begin
                    AllocRegBetween(taicpu(p).oper[0]^.reg, p, hp1, UsedRegs);
                    taicpu(hp1).oper[0]^.ref^.base := taicpu(p).oper[1]^.reg;
                    taicpu(hp1).oper[0]^.ref^.index := taicpu(p).oper[0]^.reg;

                    DebugMsg(SPeepholeOptimization + 'AddMov2Mov done', p);

                    if (cs_opt_level3 in current_settings.optimizerswitches) then
                      { hp1 may not be the immediate next instruction under -O3 }
                      RemoveCurrentp(p)
                    else
                      RemoveCurrentp(p, hp1);
                  end;

                Result := True;
                Exit;
              end;

            { Change:
                addl/q $x,%reg1
                movl/q %reg1,%reg2
              To:
                leal/q $x(%reg1),%reg2
                addl/q $x,%reg1 (can be removed if %reg1 or the flags are not used afterwards)

              Breaks the dependency chain.
            }
            if (taicpu(p).oper[0]^.typ = top_const) and
              MatchInstruction(hp1, A_MOV, [taicpu(p).opsize]) and
              (taicpu(hp1).oper[1]^.typ = top_reg) and
              MatchOperand(taicpu(hp1).oper[0]^, taicpu(p).oper[1]^.reg) and
              (
                { Instructions are guaranteed to be adjacent on -O2 and under }
                not (cs_opt_level3 in current_settings.optimizerswitches) or
                not RegUsedBetween(taicpu(hp1).oper[1]^.reg, p, hp1)
              ) then
              begin
                TransferUsedRegs(TmpUsedRegs);
                hp2 := p;
                repeat
                  UpdateUsedRegs(TmpUsedRegs, tai(hp2.Next));
                until not (cs_opt_level3 in current_settings.optimizerswitches) or not GetNextInstruction(hp2, hp2) or (hp2 = hp1);

                if (
                    { Don't do AddMov2LeaAdd under -Os, but do allow AddMov2Lea }
                    not (cs_opt_size in current_settings.optimizerswitches) or
                    (
                      not RegUsedAfterInstruction(taicpu(p).oper[1]^.reg, hp1, TmpUsedRegs) and
                      not RegUsedAfterInstruction(NR_DEFAULTFLAGS, hp1, TmpUsedRegs)
                    )
                  ) then
                  begin
                    { Change the MOV instruction to a LEA instruction, and update the
                      first operand }

                    reference_reset(NewRef, 1, []);
                    NewRef.base := taicpu(p).oper[1]^.reg;
                    NewRef.scalefactor := 1;
                    NewRef.offset := asizeint(taicpu(p).oper[0]^.val);

                    taicpu(hp1).opcode := A_LEA;
                    taicpu(hp1).loadref(0, NewRef);

                    if RegUsedAfterInstruction(NewRef.base, hp1, TmpUsedRegs) or
                      RegUsedAfterInstruction(NR_DEFAULTFLAGS, hp1, TmpUsedRegs) then
                      begin
                        hp2 := tai(hp1.Next); { for the benefit of AllocRegBetween }

                        { Move what is now the LEA instruction to before the ADD instruction }
                        Asml.Remove(hp1);
                        Asml.InsertBefore(hp1, p);
                        AllocRegBetween(taicpu(hp1).oper[1]^.reg, hp1, hp2, UsedRegs);

                        DebugMsg(SPeepholeOptimization + 'AddMov2LeaAdd', p);
                        p := hp1;
                      end
                    else
                      begin
                        { Since %reg1 or the flags aren't used afterwards, we can delete p completely }
                        DebugMsg(SPeepholeOptimization + 'AddMov2Lea', hp1);

                        if (cs_opt_level3 in current_settings.optimizerswitches) then
                          { hp1 may not be the immediate next instruction under -O3 }
                          RemoveCurrentp(p)
                        else
                          RemoveCurrentp(p, hp1);
                      end;

                    Result := True;
                  end;
              end;
          end;
      end;


    function TX86AsmOptimizer.OptPass2Lea(var p : tai) : Boolean;
      var
        SubReg: TSubRegister;
      begin
        Result:=false;
        SubReg := getsubreg(taicpu(p).oper[1]^.reg);
        if not (RegInUsedRegs(NR_DEFAULTFLAGS,UsedRegs)) then
          with taicpu(p).oper[0]^.ref^ do
            if (offset = 0) and not Assigned(symbol) and not Assigned(relsymbol) and (index <> NR_NO) then
              begin
                if (scalefactor <= 1) and SuperRegistersEqual(base, taicpu(p).oper[1]^.reg) then
                  begin
                    taicpu(p).loadreg(0, newreg(R_INTREGISTER, getsupreg(index), SubReg));
                    taicpu(p).opcode := A_ADD;
                    DebugMsg(SPeepholeOptimization + 'Lea2AddBase done',p);
                    Result := True;
                  end
                else if SuperRegistersEqual(index, taicpu(p).oper[1]^.reg) then
                  begin
                    if (base <> NR_NO) then
                      begin
                        if (scalefactor <= 1) then
                          begin
                            taicpu(p).loadreg(0, newreg(R_INTREGISTER, getsupreg(base), SubReg));
                            taicpu(p).opcode := A_ADD;
                            DebugMsg(SPeepholeOptimization + 'Lea2AddIndex done',p);
                            Result := True;
                          end;
                      end
                    else
                      { Convert lea (%reg,2^x),%reg to shl x,%reg }
                      if (scalefactor in [2, 4, 8]) then
                        begin
                          { BsrByte is, in essence, the base-2 logarithm of the scale factor }
                          taicpu(p).loadconst(0, BsrByte(scalefactor));
                          taicpu(p).opcode := A_SHL;
                          DebugMsg(SPeepholeOptimization + 'Lea2Shl done',p);
                          Result := True;
                        end;
                  end;
              end;
      end;


    function TX86AsmOptimizer.OptPass2SUB(var p: tai): Boolean;
      var
        hp1, hp2: tai;
        NewRef: TReference;
        Distance: Cardinal;
        TempTracking: TAllUsedRegs;

      begin
        Result := False;

        if (taicpu(p).opsize in [S_L{$ifdef x86_64}, S_Q{$endif}]) and
          MatchOpType(taicpu(p),top_const,top_reg) then
          begin
            Distance := GetNextInstructionUsingRegCount(p, hp1, taicpu(p).oper[1]^.reg);
            if (Distance = 0) or (Distance > 3) { Likely too far to make a meaningful difference } or
              (hp1.typ <> ait_instruction) or
              not
              (
                (cs_opt_level3 in current_settings.optimizerswitches) or
                { GetNextInstructionUsingRegCount just returns the next valid instruction under -O2 and under }
                RegInInstruction(taicpu(p).oper[1]^.reg, hp1)
              ) then
              Exit;

            { Some of the MOV optimisations are much more in-depth.  For example, if we have:
                subq $x,   %rax
                movq %rax, %rdx
                sarq $63,  %rdx
                (%rax still in use)

              ...letting OptPass2SUB run its course (and without -Os) will produce:
                leaq $-x(%rax),%rdx
                movq $x,   %rax
                sarq $63,  %rdx

              ...which is okay since it breaks the dependency chain between
                subq and movq, but if OptPass2MOV is called first:

                subq $x,   %rax
                cqto

              ...which is better in all ways, taking only 2 cycles to execute
                and much smaller in code size.
            }

            { The extra register tracking is quite strenuous }
            if (cs_opt_level2 in current_settings.optimizerswitches) and
              MatchInstruction(hp1, A_MOV, []) then
              begin
                { Update the register tracking to the MOV instruction }
                CopyUsedRegs(TempTracking);
                hp2 := p;
                repeat
                  UpdateUsedRegs(tai(hp2.Next));
                until not (cs_opt_level3 in current_settings.optimizerswitches) or not GetNextInstruction(hp2, hp2) or (hp2 = hp1);

                { if hp1 <> hp2 after the call, then hp1 got removed, so let
                  OptPass2SUB get called again }
                if OptPass2MOV(hp1) and (hp1 <> hp2) then
                  begin
                    { Reset the tracking to the current instruction }
                    RestoreUsedRegs(TempTracking);
                    ReleaseUsedRegs(TempTracking);

                    Result := True;
                    Exit;
                  end;

                { Reset the tracking to the current instruction }
                RestoreUsedRegs(TempTracking);
                ReleaseUsedRegs(TempTracking);

                { If OptPass2MOV returned True, we don't need to set Result to
                  True if hp1 didn't change because the SUB instruction didn't
                  get modified and we'll be evaluating hp1 again when the
                  peephole optimizer reaches it }
              end;

            { Change:
                subl/q $x,%reg1
                movl/q %reg1,%reg2
              To:
                leal/q $-x(%reg1),%reg2
                subl/q $x,%reg1 (can be removed if %reg1 or the flags are not used afterwards)

              Breaks the dependency chain and potentially permits the removal of
              a CMP instruction if one follows.
            }
            if MatchInstruction(hp1, A_MOV, [taicpu(p).opsize]) and
              (taicpu(hp1).oper[1]^.typ = top_reg) and
              MatchOperand(taicpu(hp1).oper[0]^, taicpu(p).oper[1]^.reg) and
              (
                { Instructions are guaranteed to be adjacent on -O2 and under }
                not (cs_opt_level3 in current_settings.optimizerswitches) or
                not RegUsedBetween(taicpu(hp1).oper[1]^.reg, p, hp1)
              ) then
              begin
                TransferUsedRegs(TmpUsedRegs);
                hp2 := p;
                repeat
                  UpdateUsedRegs(TmpUsedRegs, tai(hp2.Next));
                until not (cs_opt_level3 in current_settings.optimizerswitches) or not GetNextInstruction(hp2, hp2) or (hp2 = hp1);

                if (
                    { Don't do SubMov2LeaSub under -Os, but do allow SubMov2Lea }
                    not (cs_opt_size in current_settings.optimizerswitches) or
                    (
                      not RegUsedAfterInstruction(taicpu(p).oper[1]^.reg, hp1, TmpUsedRegs) and
                      not RegUsedAfterInstruction(NR_DEFAULTFLAGS, hp1, TmpUsedRegs)
                    )
                  ) then
                  begin
                    { Change the MOV instruction to a LEA instruction, and update the
                      first operand }
                    reference_reset(NewRef, 1, []);
                    NewRef.base := taicpu(p).oper[1]^.reg;
                    NewRef.scalefactor := 1;
                    NewRef.offset := -taicpu(p).oper[0]^.val;

                    taicpu(hp1).opcode := A_LEA;
                    taicpu(hp1).loadref(0, NewRef);

                    TransferUsedRegs(TmpUsedRegs);
                    UpdateUsedRegs(TmpUsedRegs, tai(p.Next));
                    if RegUsedAfterInstruction(NewRef.base, hp1, TmpUsedRegs) or
                      RegUsedAfterInstruction(NR_DEFAULTFLAGS, hp1, TmpUsedRegs) then
                      begin
                        hp2 := tai(hp1.Next); { for the benefit of AllocRegBetween }

                        { Move what is now the LEA instruction to before the SUB instruction }
                        Asml.Remove(hp1);
                        Asml.InsertBefore(hp1, p);
                        AllocRegBetween(taicpu(hp1).oper[1]^.reg, hp1, hp2, UsedRegs);

                        DebugMsg(SPeepholeOptimization + 'SubMov2LeaSub', p);
                        p := hp1;
                      end
                    else
                      begin
                        { Since %reg1 or the flags aren't used afterwards, we can delete p completely }
                        DebugMsg(SPeepholeOptimization + 'SubMov2Lea', hp1);

                        if (cs_opt_level3 in current_settings.optimizerswitches) then
                          { hp1 may not be the immediate next instruction under -O3 }
                          RemoveCurrentp(p)
                        else
                          RemoveCurrentp(p, hp1);
                      end;

                    Result := True;
                  end;
              end;
          end;
      end;


    function TX86AsmOptimizer.SkipSimpleInstructions(var hp1 : tai) : Boolean;
      begin
        { we can skip all instructions not messing with the stack pointer }
        while assigned(hp1) and {MatchInstruction(hp1,[A_LEA,A_MOV,A_MOVQ,A_MOVSQ,A_MOVSX,A_MOVSXD,A_MOVZX,
          A_AND,A_OR,A_XOR,A_ADD,A_SHR,A_SHL,A_IMUL,A_SETcc,A_SAR,A_SUB,A_TEST,A_CMOVcc,
          A_MOVSS,A_MOVSD,A_MOVAPS,A_MOVUPD,A_MOVAPD,A_MOVUPS,
          A_VMOVSS,A_VMOVSD,A_VMOVAPS,A_VMOVUPD,A_VMOVAPD,A_VMOVUPS],[]) and}
          ({(taicpu(hp1).ops=0) or }
           ({(MatchOpType(taicpu(hp1),top_reg,top_reg) or MatchOpType(taicpu(hp1),top_const,top_reg) or
             (MatchOpType(taicpu(hp1),top_ref,top_reg))
            ) and }
            not(RegInInstruction(NR_STACK_POINTER_REG,hp1)) { and not(RegInInstruction(NR_FRAME_POINTER_REG,hp1))}
           )
          ) do
          GetNextInstruction(hp1,hp1);
        Result:=assigned(hp1);
      end;


    function TX86AsmOptimizer.PostPeepholeOptLea(var p : tai) : Boolean;
      var
        hp1, hp2, hp3, hp4, hp5: tai;
      begin
        Result:=false;
        hp5:=nil;
        { replace
            leal(q) x(<stackpointer>),<stackpointer>
            call   procname
            leal(q) -x(<stackpointer>),<stackpointer>
            ret
          by
            jmp    procname

          but do it only on level 4 because it destroys stack back traces
        }
        if (cs_opt_level4 in current_settings.optimizerswitches) and
          MatchOpType(taicpu(p),top_ref,top_reg) and
          (taicpu(p).oper[0]^.ref^.base=NR_STACK_POINTER_REG) and
          (taicpu(p).oper[0]^.ref^.index=NR_NO) and
          { the -8 or -24 are not required, but bail out early if possible,
            higher values are unlikely }
          ((taicpu(p).oper[0]^.ref^.offset=-8) or
           (taicpu(p).oper[0]^.ref^.offset=-24))  and
          (taicpu(p).oper[0]^.ref^.symbol=nil) and
          (taicpu(p).oper[0]^.ref^.relsymbol=nil) and
          (taicpu(p).oper[1]^.reg=NR_STACK_POINTER_REG) and
          GetNextInstruction(p, hp1) and
          { Take a copy of hp1 }
          SetAndTest(hp1, hp4) and
          { trick to skip label }
          ((hp1.typ=ait_instruction) or GetNextInstruction(hp1, hp1)) and
          SkipSimpleInstructions(hp1) and
          MatchInstruction(hp1,A_CALL,[S_NO]) and
          GetNextInstruction(hp1, hp2) and
          MatchInstruction(hp2,A_LEA,[taicpu(p).opsize]) and
          MatchOpType(taicpu(hp2),top_ref,top_reg) and
          (taicpu(hp2).oper[0]^.ref^.offset=-taicpu(p).oper[0]^.ref^.offset) and
          (taicpu(hp2).oper[0]^.ref^.base=NR_STACK_POINTER_REG) and
          (taicpu(hp2).oper[0]^.ref^.index=NR_NO) and
          (taicpu(hp2).oper[0]^.ref^.symbol=nil) and
          (taicpu(hp2).oper[0]^.ref^.relsymbol=nil) and
          { Segment register will be NR_NO }
          (taicpu(hp2).oper[1]^.reg=NR_STACK_POINTER_REG) and
          GetNextInstruction(hp2, hp3) and
          { trick to skip label }
          ((hp3.typ=ait_instruction) or GetNextInstruction(hp3, hp3)) and
          (MatchInstruction(hp3,A_RET,[S_NO]) or
           (MatchInstruction(hp3,A_VZEROUPPER,[S_NO]) and
            SetAndTest(hp3,hp5) and
            GetNextInstruction(hp3,hp3) and
            MatchInstruction(hp3,A_RET,[S_NO])
           )
          ) and
          (taicpu(hp3).ops=0) then
          begin
            taicpu(hp1).opcode := A_JMP;
            taicpu(hp1).is_jmp := true;
            DebugMsg(SPeepholeOptimization + 'LeaCallLeaRet2Jmp done',p);
            RemoveCurrentP(p, hp4);
            RemoveInstruction(hp2);
            RemoveInstruction(hp3);
            if Assigned(hp5) then
              begin
                AsmL.Remove(hp5);
                ASmL.InsertBefore(hp5,hp1)
              end;
            Result:=true;
          end;
      end;


    function TX86AsmOptimizer.PostPeepholeOptPush(var p : tai) : Boolean;
{$ifdef x86_64}
      var
        hp1, hp2, hp3, hp4, hp5: tai;
{$endif x86_64}
      begin
        Result:=false;
{$ifdef x86_64}
        hp5:=nil;
        { replace
            push %rax
            call   procname
            pop %rcx
            ret
          by
            jmp    procname

          but do it only on level 4 because it destroys stack back traces

          It depends on the fact, that the sequence push rax/pop rcx is used for stack alignment as rcx is volatile
          for all supported calling conventions
        }
        if (cs_opt_level4 in current_settings.optimizerswitches) and
          MatchOpType(taicpu(p),top_reg) and
          (taicpu(p).oper[0]^.reg=NR_RAX) and
          GetNextInstruction(p, hp1) and
          { Take a copy of hp1 }
          SetAndTest(hp1, hp4) and
          { trick to skip label }
          ((hp1.typ=ait_instruction) or GetNextInstruction(hp1, hp1)) and
          SkipSimpleInstructions(hp1) and
          MatchInstruction(hp1,A_CALL,[S_NO]) and
          GetNextInstruction(hp1, hp2) and
          MatchInstruction(hp2,A_POP,[taicpu(p).opsize]) and
          MatchOpType(taicpu(hp2),top_reg) and
          (taicpu(hp2).oper[0]^.reg=NR_RCX) and
          GetNextInstruction(hp2, hp3) and
          { trick to skip label }
          ((hp3.typ=ait_instruction) or GetNextInstruction(hp3, hp3)) and
          (MatchInstruction(hp3,A_RET,[S_NO]) or
           (MatchInstruction(hp3,A_VZEROUPPER,[S_NO]) and
            SetAndTest(hp3,hp5) and
            GetNextInstruction(hp3,hp3) and
            MatchInstruction(hp3,A_RET,[S_NO])
           )
          ) and
          (taicpu(hp3).ops=0) then
          begin
            taicpu(hp1).opcode := A_JMP;
            taicpu(hp1).is_jmp := true;
            DebugMsg(SPeepholeOptimization + 'PushCallPushRet2Jmp done',p);
            RemoveCurrentP(p, hp4);
            RemoveInstruction(hp2);
            RemoveInstruction(hp3);
            if Assigned(hp5) then
              begin
                AsmL.Remove(hp5);
                ASmL.InsertBefore(hp5,hp1)
              end;
            Result:=true;
          end;
{$endif x86_64}
      end;


    function TX86AsmOptimizer.PostPeepholeOptMov(var p : tai) : Boolean;
      var
        Value, RegName: string;
      begin
        Result:=false;
        if (taicpu(p).oper[1]^.typ = top_reg) and (taicpu(p).oper[0]^.typ = top_const) then
          begin

            case taicpu(p).oper[0]^.val of
            0:
              { Don't make this optimisation if the CPU flags are required, since XOR scrambles them }
              if not (RegInUsedRegs(NR_DEFAULTFLAGS,UsedRegs)) then
                begin
                  { change "mov $0,%reg" into "xor %reg,%reg" }
                  taicpu(p).opcode := A_XOR;
                  taicpu(p).loadReg(0,taicpu(p).oper[1]^.reg);
                  Result := True;
{$ifdef x86_64}
                end
              else if (taicpu(p).opsize = S_Q) then
                begin
                  RegName := debug_regname(taicpu(p).oper[1]^.reg); { 64-bit register name }

                  { The actual optimization }
                  setsubreg(taicpu(p).oper[1]^.reg, R_SUBD);
                  taicpu(p).changeopsize(S_L);

                  DebugMsg(SPeepholeOptimization + 'movq $0,' + RegName + ' -> movl $0,' + debug_regname(taicpu(p).oper[1]^.reg) + ' (immediate can be represented with just 32 bits)', p);
                  Result := True;
                end;
            $1..$FFFFFFFF:
              begin
                { Code size reduction by J. Gareth "Kit" Moreton }
                { change 64-bit register to 32-bit register to reduce code size (upper 32 bits will be set to zero) }
                case taicpu(p).opsize of
                  S_Q:
                    begin
                      RegName := debug_regname(taicpu(p).oper[1]^.reg); { 64-bit register name }
                      Value := debug_tostr(taicpu(p).oper[0]^.val);

                      { The actual optimization }
                      setsubreg(taicpu(p).oper[1]^.reg, R_SUBD);
                      taicpu(p).changeopsize(S_L);

                      DebugMsg(SPeepholeOptimization + 'movq $' + Value + ',' + RegName + ' -> movl $' + Value + ',' + debug_regname(taicpu(p).oper[1]^.reg) + ' (immediate can be represented with just 32 bits)', p);
                      Result := True;
                    end;
                  else
                    { Do nothing };
                end;
{$endif x86_64}
              end;
            -1:
              { Don't make this optimisation if the CPU flags are required, since OR scrambles them }
              if (cs_opt_size in current_settings.optimizerswitches) and
                (taicpu(p).opsize <> S_B) and
                not (RegInUsedRegs(NR_DEFAULTFLAGS,UsedRegs)) then
                begin
                  { change "mov $-1,%reg" into "or $-1,%reg" }
                  { NOTES:
                    - No size saving is made when changing a Word-sized assignment unless the register is AX (smaller encoding)
                    - This operation creates a false dependency on the register, so only do it when optimising for size
                    - It is possible to set memory operands using this method, but this creates an even greater false dependency, so don't do this at all
                  }
                  taicpu(p).opcode := A_OR;
                  DebugMsg(SPeepholeOptimization + 'Mov-12Or-1',p);
                  Result := True;
                end;
            else
              { Do nothing };
            end;
          end;
      end;


    { Returns true if the given logic instruction can be converted into a BTx instruction (BT not included) }
    class function TX86AsmOptimizer.IsBTXAcceptable(p : tai) : boolean;
      begin
        Result := False;

        if not (CPUX86_HAS_BTX in cpu_capabilities[current_settings.optimizecputype]) then
          Exit;

        { For sizes less than S_L, the byte size is equal or larger with BTx,
          so don't bother optimising }
        if not MatchInstruction(p, A_AND, A_OR, A_XOR, [S_L{$ifdef x86_64}, S_Q{$endif x86_64}]) then
          Exit;

        if (taicpu(p).oper[0]^.typ <> top_const) or
          { If the value can fit into an 8-bit signed integer, a smaller
            instruction can be encoded with AND/OR/XOR, so don't optimise if it
            falls within this range }
          (
            (taicpu(p).oper[0]^.val > -128) and
            (taicpu(p).oper[0]^.val <= 127)
          ) then
          Exit;

        { If we're optimising for size, this is acceptable }
        if (cs_opt_size in current_settings.optimizerswitches) then
          Exit(True);

        if (taicpu(p).oper[1]^.typ = top_reg) and
          (CPUX86_HINT_FAST_BTX_REG_IMM in cpu_optimization_hints[current_settings.optimizecputype]) then
          Exit(True);

        if (taicpu(p).oper[1]^.typ <> top_reg) and
          (CPUX86_HINT_FAST_BTX_MEM_IMM in cpu_optimization_hints[current_settings.optimizecputype]) then
          Exit(True);
      end;

    function TX86AsmOptimizer.PostPeepholeOptAnd(var p : tai) : boolean;
      var
        hp1: tai;
        Value: TCGInt;
      begin
        Result := False;
        if MatchOpType(taicpu(p), top_const, top_reg) then
          begin
            { Detect:
                andw   x,  %ax (0 <= x < $8000)
                ...
                movzwl %ax,%eax

              Change movzwl %ax,%eax to cwtl (shorter encoding for movswl %ax,%eax)
            }

            if (taicpu(p).oper[1]^.reg = NR_AX) and { This is also enough to determine that opsize = S_W }
              ((taicpu(p).oper[0]^.val and $7FFF) = taicpu(p).oper[0]^.val) and
              GetNextInstructionUsingReg(p, hp1, NR_EAX) and
              MatchInstruction(hp1, A_MOVZX, [S_WL]) and
              MatchOperand(taicpu(hp1).oper[0]^, NR_AX) and
              MatchOperand(taicpu(hp1).oper[1]^, NR_EAX) then
              begin
                DebugMsg(SPeepholeOptimization + 'Converted movzwl %ax,%eax to cwtl (via AndMovz2AndCwtl)', hp1);
                taicpu(hp1).opcode := A_CWDE;
                taicpu(hp1).clearop(0);
                taicpu(hp1).clearop(1);
                taicpu(hp1).ops := 0;

                { A change was made, but not with p, so don't set Result, but
                  notify the compiler that a change was made }
                Include(OptsToCheck, aoc_ForceNewIteration);

                Exit; { and -> btr won't happen because an opsize of S_W won't be optimised anyway }
              end;
          end;

        { If "not x" is a power of 2 (popcnt = 1), change:
            and $x, %reg/ref

          To:
            btr lb(x), %reg/ref
        }
        if IsBTXAcceptable(p) and
          (
            { Make sure a TEST doesn't follow that plays with the register }
            not GetNextInstruction(p, hp1) or
            not MatchInstruction(hp1, A_TEST, A_CMP, [taicpu(p).opsize]) or
            not MatchOperand(taicpu(hp1).oper[1]^, taicpu(p).oper[1]^.reg)
          ) then
          begin
{$push}{$R-}{$Q-}
            { Value is a sign-extended 32-bit integer - just correct it
              if it's represented as an unsigned value.  Also, IsBTXAcceptable
              checks to see if this operand is an immediate. }
            Value := not taicpu(p).oper[0]^.val;
{$pop}
{$ifdef x86_64}
            if taicpu(p).opsize = S_L then
{$endif x86_64}
              Value := Value and $FFFFFFFF;

            if (PopCnt(QWord(Value)) = 1) then
              begin
                DebugMsg(SPeepholeOptimization + 'Changed AND (not $' + debug_hexstr(taicpu(p).oper[0]^.val) + ') to BTR $' + debug_tostr(BsrQWord(Value)) + ' to shrink instruction size (And2Btr)', p);
                taicpu(p).opcode := A_BTR;
                taicpu(p).oper[0]^.val := BsrQWord(Value); { Essentially the base 2 logarithm }
                Result := True;
                Exit;
              end;
          end;
      end;


    function TX86AsmOptimizer.PostPeepholeOptMOVSX(var p : tai) : boolean;
      begin
        Result := False;
        if not MatchOpType(taicpu(p), top_reg, top_reg) then
          Exit;

        { Convert:
            movswl %ax,%eax  -> cwtl
            movslq %eax,%rax -> cdqe

            NOTE: Don't convert movswl %al,%ax to cbw, because cbw and cwde
              refer to the same opcode and depends only on the assembler's
              current operand-size attribute. [Kit]
        }
        with taicpu(p) do
          case opsize of
            S_WL:
              if (oper[0]^.reg = NR_AX) and (oper[1]^.reg = NR_EAX) then
                begin
                  DebugMsg(SPeepholeOptimization + 'Converted movswl %ax,%eax to cwtl', p);
                  opcode := A_CWDE;
                  clearop(0);
                  clearop(1);
                  ops := 0;
                  Result := True;
                end;
{$ifdef x86_64}
            S_LQ:
              if (oper[0]^.reg = NR_EAX) and (oper[1]^.reg = NR_RAX) then
                begin
                  DebugMsg(SPeepholeOptimization + 'Converted movslq %eax,%rax to cltq', p);
                  opcode := A_CDQE;
                  clearop(0);
                  clearop(1);
                  ops := 0;
                  Result := True;
                end;
{$endif x86_64}
            else
              ;
          end;
      end;


    function TX86AsmOptimizer.PostPeepholeOptShr(var p : tai) : boolean;
      var
        hp1, hp2: tai;
        IdentityMask, Shift: TCGInt;
        LimitSize: Topsize;
        DoNotMerge: Boolean;
      begin
        Result := False;

        { All these optimisations work on "shr const,%reg" }
        if not MatchOpType(taicpu(p), top_const, top_reg) then
          Exit;

        DoNotMerge := False;
        Shift := taicpu(p).oper[0]^.val;
        LimitSize := taicpu(p).opsize;

        hp1 := p;
        repeat
          if not GetNextInstructionUsingReg(hp1, hp1, taicpu(p).oper[1]^.reg) or (hp1.typ <> ait_instruction) then
            Break;

          { Detect:
              shr x, %reg
              and y, %reg

            If and y, %reg doesn't actually change the value of %reg (e.g. with
            "shrl $24,%reg; andl $255,%reg", remove the AND instruction.
          }

          case taicpu(hp1).opcode of
            A_AND:
              if (taicpu(hp1).opsize = taicpu(p).opsize) and
                MatchOpType(taicpu(hp1), top_const, top_reg) and
                (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) then
                begin
                  { Make sure the FLAGS register isn't in use }
                  TransferUsedRegs(TmpUsedRegs);
                  hp2 := p;
                  repeat
                    UpdateUsedRegs(TmpUsedRegs, tai(hp2.Next));
                  until not GetNextInstruction(hp2, hp2) or (hp2 = hp1);

                  if not RegUsedAfterInstruction(NR_DEFAULTFLAGS, hp1, TmpUsedRegs) then
                    begin
                      { Generate the identity mask }
                      case taicpu(p).opsize of
                        S_B:
                          IdentityMask := $FF shr Shift;
                        S_W:
                          IdentityMask := $FFFF shr Shift;
                        S_L:
                          IdentityMask := $FFFFFFFF shr Shift;
{$ifdef x86_64}
                        S_Q:
                          { We need to force the operands to be unsigned 64-bit
                            integers otherwise the wrong value is generated }
                          IdentityMask := TCGInt(QWord($FFFFFFFFFFFFFFFF) shr QWord(Shift));
{$endif x86_64}
                        else
                          InternalError(2022081501);
                      end;

                      if (taicpu(hp1).oper[0]^.val and IdentityMask) = IdentityMask then
                        begin
                          DebugMsg(SPeepholeOptimization + 'Removed AND instruction since previous SHR makes this an identity operation (ShrAnd2Shr)', hp1);
                          { All the possible 1 bits are covered, so we can remove the AND }
                          hp2 := tai(hp1.Previous);
                          RemoveInstruction(hp1);

                          { p wasn't actually changed, so don't set Result to True,
                            but a change was nonetheless made elsewhere }
                          Include(OptsToCheck, aoc_ForceNewIteration);

                          { Do another pass in case other AND or MOVZX instructions
                            follow }
                          hp1 := hp2;
                          Continue;
                        end;

                    end;
                end;

            A_TEST, A_CMP, A_Jcc:
              { Skip over conditional jumps and relevant comparisons }
              Continue;

            A_MOVZX:
              if MatchOpType(taicpu(hp1), top_reg, top_reg) and
                SuperRegistersEqual(taicpu(hp1).oper[0]^.reg, taicpu(p).oper[1]^.reg) then
                begin
                  { Since the original register is being read as is, subsequent
                    SHRs must not be merged at this point }
                  DoNotMerge := True;

                  if IsShrMovZFoldable(taicpu(p).opsize, taicpu(hp1).opsize, Shift) then
                    begin
                      if SuperRegistersEqual(taicpu(hp1).oper[0]^.reg, taicpu(hp1).oper[1]^.reg) then
                        begin
                          DebugMsg(SPeepholeOptimization + 'Removed MOVZX instruction since previous SHR makes it unnecessary (ShrMovz2Shr)', hp1);
                          { All the possible 1 bits are covered, so we can remove the AND }
                          hp2 := tai(hp1.Previous);
                          RemoveInstruction(hp1);

                          hp1 := hp2;
                        end
                      else { Different register target }
                        begin
                          DebugMsg(SPeepholeOptimization + 'Converted MOVZX instruction to MOV since previous SHR makes zero-extension unnecessary (ShrMovz2ShrMov 2)', hp1);
                          taicpu(hp1).opcode := A_MOV;
                          setsubreg(taicpu(hp1).oper[0]^.reg, getsubreg(taicpu(hp1).oper[1]^.reg));
                          case taicpu(hp1).opsize of
                            S_BW:
                              taicpu(hp1).opsize := S_W;
                            S_BL, S_WL:
                              taicpu(hp1).opsize := S_L;
                            else
                              InternalError(2022081503);
                          end;
                        end;
                    end
                  else if (Shift > 0) and
                    (taicpu(p).opsize = S_W) and
                    (taicpu(hp1).opsize = S_WL) and
                    (taicpu(hp1).oper[0]^.reg = NR_AX) and
                    (taicpu(hp1).oper[1]^.reg = NR_EAX) then
                    begin
                      { Detect:
                          shr    x,  %ax (x > 0)
                          ...
                          movzwl %ax,%eax

                        Change movzwl %ax,%eax to cwtl (shorter encoding for movswl %ax,%eax)
                      }
                      DebugMsg(SPeepholeOptimization + 'Converted movzwl %ax,%eax to cwtl (via ShrMovz2ShrCwtl)', hp1);
                      taicpu(hp1).opcode := A_CWDE;
                      taicpu(hp1).clearop(0);
                      taicpu(hp1).clearop(1);
                      taicpu(hp1).ops := 0;
                    end;

                  { Move onto the next instruction }
                  Continue;
                end;

            A_SHL, A_SAL, A_SHR:
              if (taicpu(hp1).opsize <= LimitSize) and
                MatchOpType(taicpu(hp1), top_const, top_reg) and
                SuperRegistersEqual(taicpu(hp1).oper[1]^.reg, taicpu(p).oper[1]^.reg) then
                begin
                  { Make sure the sizes don't exceed the register size limit
                    (measured by the shift value falling below the limit) }

                  if taicpu(hp1).opsize < LimitSize then
                    LimitSize := taicpu(hp1).opsize;

                  if taicpu(hp1).opcode = A_SHR then
                    Inc(Shift, taicpu(hp1).oper[0]^.val)
                  else
                    begin
                      Dec(Shift, taicpu(hp1).oper[0]^.val);
                      DoNotMerge := True;
                    end;

                  if Shift < topsize2memsize[taicpu(p).opsize] - topsize2memsize[LimitSize] then
                    Break;

                  { Since we've established that the combined shift is within
                    limits, we can actually combine the adjacent SHR
                    instructions even if they're different sizes }
                  if not DoNotMerge and (taicpu(hp1).opcode = A_SHR) then
                    begin
                      hp2 := tai(hp1.Previous);
                      DebugMsg(SPeepholeOptimization + 'ShrShr2Shr 2', p);
                      Inc(taicpu(p).oper[0]^.val, taicpu(hp1).oper[0]^.val);
                      RemoveInstruction(hp1);
                      hp1 := hp2;
                    end;

                  { Move onto the next instruction }
                  Continue;
                end;
            else
              ;
          end;

          Break;
        until False;

        { Detect the following (looking backwards):
            shr %cl,%reg
            shr x,  %reg

          Swap the two SHR instructions to minimise a pipeline stall.
        }
        if GetLastInstruction(p, hp1) and
          MatchInstruction(hp1, A_SHR, [taicpu(p).opsize]) and
          MatchOpType(taicpu(hp1), top_reg, top_reg) and
          { First operand will be %cl }
          (taicpu(hp1).oper[1]^.reg = taicpu(p).oper[1]^.reg) and
          { Just to be sure }
          (getsupreg(taicpu(hp1).oper[1]^.reg) <> RS_ECX) then
          begin
            DebugMsg(SPeepholeOptimization + 'Swapped variable and constant SHR instructions to minimise pipeline stall (ShrShr2ShrShr)', hp1);

            { Moving the entries this way ensures the register tracking remains correct }
            Asml.Remove(p);
            Asml.InsertBefore(p, hp1);
            p := hp1;
            { Don't set Result to True because the current instruction is now
              "shr %cl,%reg" and there's nothing more we can do with it }
          end;

      end;


    function TX86AsmOptimizer.PostPeepholeOptADDSUB(var p : tai) : boolean;
      var
        hp1, hp2: tai;
        Opposite, SecondOpposite: TAsmOp;
        NewCond: TAsmCond;
      begin
        Result := False;

        { Change:
            add/sub 128,(dest)

          To:
            sub/add -128,(dest)

          This generaally takes fewer bytes to encode because -128 can be stored
          in a signed byte, whereas +128 cannot.
        }
        if (taicpu(p).opsize <> S_B) and MatchOperand(taicpu(p).oper[0]^, 128) then
          begin
            if taicpu(p).opcode = A_ADD then
              Opposite := A_SUB
            else
              Opposite := A_ADD;

            { Be careful if the flags are in use, because the CF flag inverts
              when changing from ADD to SUB and vice versa }
            if RegInUsedRegs(NR_DEFAULTFLAGS, UsedRegs) and
              GetNextInstruction(p, hp1) then
              begin
                TransferUsedRegs(TmpUsedRegs);
                TmpUsedRegs[R_SPECIALREGISTER].Update(tai(p.Next), True);

                hp2 := hp1;

                { Scan ahead to check if everything's safe }
                while Assigned(hp1) and RegInUsedRegs(NR_DEFAULTFLAGS, TmpUsedRegs) do
                  begin
                    if (hp1.typ <> ait_instruction) then
                      { Probably unsafe since the flags are still in use }
                      Exit;

                    if MatchInstruction(hp1, A_CALL, A_JMP, A_RET, []) then
                      { Stop searching at an unconditional jump }
                      Break;

                    if not
                      (
                        MatchInstruction(hp1, A_ADC, A_SBB, []) and
                        (taicpu(hp1).oper[0]^.typ = top_const) { We need to be able to invert a constant }
                      ) and
                      (taicpu(hp1).condition = C_None) and RegInInstruction(NR_DEFAULTFLAGS, hp1) then
                      { Instruction depends on FLAGS (and is not ADC or SBB); break out }
                      Exit;

                    UpdateUsedRegs(TmpUsedRegs, tai(p.Next));
                    TmpUsedRegs[R_SPECIALREGISTER].Update(tai(hp1.Next), True);

                    { Move to the next instruction }
                    GetNextInstruction(hp1, hp1);
                  end;

                while Assigned(hp2) and (hp2 <> hp1) do
                  begin
                    NewCond := C_None;

                    case taicpu(hp2).condition of
                      C_A, C_NBE:
                        NewCond := C_BE;
                      C_B, C_C, C_NAE:
                        NewCond := C_AE;
                      C_AE, C_NB, C_NC:
                        NewCond := C_B;
                      C_BE, C_NA:
                        NewCond := C_A;
                      else
                        { No change needed };
                    end;

                    if NewCond <> C_None then
                      begin
                        DebugMsg(SPeepholeOptimization + 'Condition changed from ' + cond2str[taicpu(hp2).condition] + ' to ' + cond2str[NewCond] +
                          ' to accommodate ' + debug_op2str(taicpu(p).opcode) + ' -> ' + debug_op2str(opposite) + ' above', hp2);

                        taicpu(hp2).condition := NewCond;
                      end
                    else
                      if MatchInstruction(hp2, A_ADC, A_SBB, []) then
                        begin
                          { Because of the flipping of the carry bit, to ensure
                            the operation remains equivalent, ADC becomes SBB
                            and vice versa, and the constant is not-inverted.

                            If multiple ADCs or SBBs appear in a row, each one
                            changed causes the carry bit to invert, so they all
                            need to be flipped }
                          if taicpu(hp2).opcode = A_ADC then
                            SecondOpposite := A_SBB
                          else
                            SecondOpposite := A_ADC;

                          if taicpu(hp2).oper[0]^.typ <> top_const then
                            { Should have broken out of this optimisation already }
                            InternalError(2021112901);

                          DebugMsg(SPeepholeOptimization + debug_op2str(taicpu(hp2).opcode) + debug_opsize2str(taicpu(hp2).opsize) + ' $' + debug_tostr(taicpu(hp2).oper[0]^.val) + ',' + debug_operstr(taicpu(hp2).oper[1]^) + ' -> ' +
                            debug_op2str(SecondOpposite) + debug_opsize2str(taicpu(hp2).opsize) + ' $' + debug_tostr(not taicpu(hp2).oper[0]^.val) + ',' + debug_operstr(taicpu(hp2).oper[1]^) + ' to accommodate inverted carry bit', hp2);

                          { Bit-invert the constant (effectively equivalent to "-1 - val") }
                          taicpu(hp2).opcode := SecondOpposite;
                          taicpu(hp2).oper[0]^.val := not taicpu(hp2).oper[0]^.val;
                        end;

                    { Move to the next instruction }
                    GetNextInstruction(hp2, hp2);
                  end;

                if (hp2 <> hp1) then
                  InternalError(2021111501);
              end;

            DebugMsg(SPeepholeOptimization + debug_op2str(taicpu(p).opcode) + debug_opsize2str(taicpu(p).opsize) + ' $128,' + debug_operstr(taicpu(p).oper[1]^) + ' changed to ' +
              debug_op2str(opposite) + debug_opsize2str(taicpu(p).opsize) + ' $-128,' + debug_operstr(taicpu(p).oper[1]^) + ' to reduce instruction size', p);

            taicpu(p).opcode := Opposite;
            taicpu(p).oper[0]^.val := -128;

            { No further optimisations can be made on this instruction, so move
              onto the next one to save time }
            p := tai(p.Next);
            UpdateUsedRegs(p);

            Result := True;
            Exit;
          end;

        { Detect:
            add/sub %reg2,(dest)
            add/sub x,    (dest)

          (dest can be a register or a reference)

          Swap the instructions to minimise a pipeline stall.  This reverses the
          "Add swap" and "Sub swap" optimisations done in pass 1 if no new
          optimisations could be made.
        }
        if (taicpu(p).oper[0]^.typ = top_reg) and
          not RegInOp(taicpu(p).oper[0]^.reg, taicpu(p).oper[1]^) and
          (
            (
              (taicpu(p).oper[1]^.typ = top_reg) and
              { We can try searching further ahead if we're writing to a register }
              GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[1]^.reg)
            ) or
            (
              (taicpu(p).oper[1]^.typ = top_ref) and
              GetNextInstruction(p, hp1)
            )
          ) and
          MatchInstruction(hp1, A_ADD, A_SUB, [taicpu(p).opsize]) and
          (taicpu(hp1).oper[0]^.typ = top_const) and
          MatchOperand(taicpu(p).oper[1]^, taicpu(hp1).oper[1]^) then
          begin
            { Make doubly sure the flags aren't in use because the order of additions may affect them }
            TransferUsedRegs(TmpUsedRegs);
            UpdateUsedRegs(TmpUsedRegs, tai(p.next));
            hp2 := p;

            while not (cs_opt_level3 in current_settings.optimizerswitches) and
              GetNextInstruction(hp2, hp2) and (hp2 <> hp1) do
              UpdateUsedRegs(TmpUsedRegs, tai(hp2.next));

            if not RegInUsedRegs(NR_DEFAULTFLAGS, TmpUsedRegs) then
              begin
                asml.remove(hp1);
                asml.InsertBefore(hp1, p);

                DebugMsg(SPeepholeOptimization + 'Add/Sub swap 2 done', hp1);
                Result := True;
              end;
          end;

      end;


    function TX86AsmOptimizer.PostPeepholeOptCmp(var p : tai) : Boolean;
      var
        hp1: tai;
      begin
        Result:=false;

        { Final check to see if CMP/MOV pairs can be changed to MOV/CMP }
        while GetNextInstruction(p, hp1) and
          TrySwapMovCmp(p, hp1) do
          begin
            if MatchInstruction(hp1, A_MOV, []) then
              begin
                if RegInUsedRegs(NR_DEFAULTFLAGS, UsedRegs) then
                  begin
                    { A little hacky, but since CMP doesn't read the flags, only
                      modify them, it's safe if they get scrambled by MOV -> XOR }
                    ExcludeRegFromUsedRegs(NR_DEFAULTFLAGS, UsedRegs);
                    Result := PostPeepholeOptMov(hp1);
{$ifdef x86_64}
                    if Result and MatchInstruction(hp1, A_XOR, [S_Q]) then
                      { Used to shrink instruction size }
                      PostPeepholeOptXor(hp1);
{$endif x86_64}
                    IncludeRegInUsedRegs(NR_DEFAULTFLAGS, UsedRegs);
                  end
                else
                  begin
                    Result := PostPeepholeOptMov(hp1);
{$ifdef x86_64}
                    if Result and MatchInstruction(hp1, A_XOR, [S_Q]) then
                      { Used to shrink instruction size }
                      PostPeepholeOptXor(hp1);
{$endif x86_64}
                  end;
              end;

            { Enabling this flag is actually a null operation, but it marks
              the code as 'modified' during this pass }
            Include(OptsToCheck, aoc_ForceNewIteration);
          end;

        { change "cmp $0, %reg" to "test %reg, %reg" }
        if MatchOpType(taicpu(p),top_const,top_reg) and
           (taicpu(p).oper[0]^.val = 0) then
          begin
            taicpu(p).opcode := A_TEST;
            taicpu(p).loadreg(0,taicpu(p).oper[1]^.reg);
            DebugMsg(SPeepholeOptimization + 'Cmp2Test', p);
            Result:=true;
          end;
      end;


    function TX86AsmOptimizer.PostPeepholeOptTestOr(var p : tai) : Boolean;
      var
        IsTestConstX, IsValid : Boolean;
        hp1,hp2 : tai;
      begin
        Result:=false;
        { Final check to see if TEST/MOV pairs can be changed to MOV/TEST }
        if (taicpu(p).opcode = A_TEST) then
          while GetNextInstruction(p, hp1) and
            TrySwapMovCmp(p, hp1) do
            begin
              if MatchInstruction(hp1, A_MOV, []) then
                begin
                  if RegInUsedRegs(NR_DEFAULTFLAGS, UsedRegs) then
                    begin
                      { A little hacky, but since TEST doesn't read the flags, only
                        modify them, it's safe if they get scrambled by MOV -> XOR }
                      ExcludeRegFromUsedRegs(NR_DEFAULTFLAGS, UsedRegs);
                      Result := PostPeepholeOptMov(hp1);
{$ifdef x86_64}
                      if Result and MatchInstruction(hp1, A_XOR, [S_Q]) then
                        { Used to shrink instruction size }
                        PostPeepholeOptXor(hp1);
{$endif x86_64}
                      IncludeRegInUsedRegs(NR_DEFAULTFLAGS, UsedRegs);
                    end
                  else
                    begin
                      Result := PostPeepholeOptMov(hp1);
{$ifdef x86_64}
                      if Result and MatchInstruction(hp1, A_XOR, [S_Q]) then
                        { Used to shrink instruction size }
                        PostPeepholeOptXor(hp1);
{$endif x86_64}
                    end;
                end;

              { Enabling this flag is actually a null operation, but it marks
                the code as 'modified' during this pass }
              Include(OptsToCheck, aoc_ForceNewIteration);
            end;

        { If x is a power of 2 (popcnt = 1), change:
            or  $x, %reg/ref

          To:
            bts lb(x), %reg/ref
        }
        if (taicpu(p).opcode = A_OR) and
          IsBTXAcceptable(p) and
          { IsBTXAcceptable checks to see if oper[0] is an immediate }
          (PopCnt(QWord(taicpu(p).oper[0]^.val)) = 1) and
          (
            { Don't optimise if a test instruction follows }
            not GetNextInstruction(p, hp1) or
            not MatchInstruction(hp1, A_TEST, [taicpu(p).opsize])
          ) then
          begin
            DebugMsg(SPeepholeOptimization + 'Changed OR $' + debug_hexstr(taicpu(p).oper[0]^.val) + ' to BTS $' + debug_tostr(BsrQWord(taicpu(p).oper[0]^.val)) + ' to shrink instruction size (Or2Bts)', p);
            taicpu(p).opcode := A_BTS;
            taicpu(p).oper[0]^.val := BsrQWord(taicpu(p).oper[0]^.val); { Essentially the base 2 logarithm }
            Result := True;
            Exit;
          end;

        { If x is a power of 2 (popcnt = 1), change:
            test $x, %reg/ref
            je / sete / cmove (or jne / setne)

          To:
            bt   lb(x), %reg/ref
            jnc / setnc / cmovnc (or jc / setc / cmovnc)
        }
        if (taicpu(p).opcode = A_TEST) and
          (CPUX86_HAS_BTX in cpu_capabilities[current_settings.optimizecputype]) and
          (taicpu(p).oper[0]^.typ = top_const) and
          (
            (cs_opt_size in current_settings.optimizerswitches) or
            (
              (taicpu(p).oper[1]^.typ = top_reg) and
              (CPUX86_HINT_FAST_BT_REG_IMM in cpu_optimization_hints[current_settings.optimizecputype])
            ) or
            (
              (taicpu(p).oper[1]^.typ <> top_reg) and
              (CPUX86_HINT_FAST_BT_MEM_IMM in cpu_optimization_hints[current_settings.optimizecputype])
            )
          ) and
          (PopCnt(QWord(taicpu(p).oper[0]^.val)) = 1) and
          { For sizes less than S_L, the byte size is equal or larger with BT,
            so don't bother optimising }
          (taicpu(p).opsize >= S_L) then
          begin
            IsValid := True;
            { Check the next set of instructions, watching the FLAGS register
              and the conditions used }
            TransferUsedRegs(TmpUsedRegs);
            UpdateUsedRegs(TmpUsedRegs, tai(p.Next));
            hp1 := p;
            hp2 := nil;

            while GetNextInstruction(hp1, hp1) do
              begin
                if not Assigned(hp2) then
                  { The first instruction after TEST }
                  hp2 := hp1;

                if (hp1.typ <> ait_instruction) then
                  begin
                    { If the flags are no longer in use, everything is fine }
                    if RegInUsedRegs(NR_DEFAULTFLAGS, TmpUsedRegs) then
                      IsValid := False;
                    Break;
                  end;

                case taicpu(hp1).condition of
                  C_None:
                    begin
                      if RegInUsedRegs(NR_DEFAULTFLAGS, TmpUsedRegs) then
                        { Something is not quite normal, so play safe and don't change }
                        IsValid := False;

                      Break;
                    end;
                  C_E, C_Z, C_NE, C_NZ:
                    { This is fine };
                  else
                    begin
                      { Unsupported condition }
                      IsValid := False;
                      Break;
                    end;
                end;

                UpdateUsedRegs(TmpUsedRegs, tai(hp1.Next));
              end;

            if IsValid then
              begin
                while hp2 <> hp1 do
                  begin
                    case taicpu(hp2).condition of
                      C_Z, C_E:
                        taicpu(hp2).condition := C_NC;
                      C_NZ, C_NE:
                        taicpu(hp2).condition := C_C;
                      else
                        { Should not get this by this point }
                        InternalError(2022110701);
                    end;

                    GetNextInstruction(hp2, hp2);
                  end;

                DebugMsg(SPeepholeOptimization + 'Changed TEST $' + debug_hexstr(taicpu(p).oper[0]^.val) + ' to BT $' + debug_tostr(BsrQWord(taicpu(p).oper[0]^.val)) + ' to shrink instruction size (Test2Bt)', p);
                taicpu(p).opcode := A_BT;
                taicpu(p).oper[0]^.val := BsrQWord(taicpu(p).oper[0]^.val); { Essentially the base 2 logarithm }
                Result := True;
                Exit;
              end;
          end;

        { removes the line marked with (x) from the sequence
          and/or/xor/add/sub/... $x, %y
          test/or %y, %y  | test $-1, %y    (x)
          j(n)z _Label
             as the first instruction already adjusts the ZF
             %y operand may also be a reference }
        IsTestConstX:=(taicpu(p).opcode=A_TEST) and
          MatchOperand(taicpu(p).oper[0]^,-1);
        if (OpsEqual(taicpu(p).oper[0]^,taicpu(p).oper[1]^) or IsTestConstX) and
           GetLastInstruction(p, hp1) and
           (tai(hp1).typ = ait_instruction) and
           GetNextInstruction(p,hp2) and
           MatchInstruction(hp2,A_SETcc,A_Jcc,A_CMOVcc,[]) then
          case taicpu(hp1).opcode Of
            A_ADD, A_SUB, A_OR, A_XOR, A_AND,
            { These two instructions set the zero flag if the result is zero }
            A_POPCNT, A_LZCNT:
              begin
                if (
                    { With POPCNT, an input of zero will set the zero flag
                      because the population count of zero is zero }
                    (taicpu(hp1).opcode = A_POPCNT) and
                    (taicpu(hp2).condition in [C_Z,C_NZ,C_E,C_NE]) and
                    (
                      OpsEqual(taicpu(hp1).oper[0]^, taicpu(p).oper[1]^) or
                      { Faster than going through the second half of the 'or'
                        condition below }
                      OpsEqual(taicpu(hp1).oper[1]^, taicpu(p).oper[1]^)
                    )
                  ) or (
                    OpsEqual(taicpu(hp1).oper[1]^, taicpu(p).oper[1]^) and
                    { does not work in case of overflow for G(E)/L(E)/C_O/C_NO }
                    { and in case of carry for A(E)/B(E)/C/NC                  }
                    (
                      (taicpu(hp2).condition in [C_Z,C_NZ,C_E,C_NE]) or
                      (
                        (taicpu(hp1).opcode <> A_ADD) and
                        (taicpu(hp1).opcode <> A_SUB) and
                        (taicpu(hp1).opcode <> A_LZCNT)
                      )
                    )
                  ) then
                  begin
                    DebugMsg(SPeepholeOptimization + 'OpTest/Or2Op (2-op) done', hp1);
                    RemoveCurrentP(p, hp2);
                    Result:=true;
                    Exit;
                  end;
              end;
            A_SHL, A_SAL, A_SHR, A_SAR:
              begin
                if OpsEqual(taicpu(hp1).oper[1]^,taicpu(p).oper[1]^) and
                  { SHL/SAL/SHR/SAR with a value of 0 do not change the flags }
                  { therefore, it's only safe to do this optimization for     }
                  { shifts by a (nonzero) constant                            }
                   (taicpu(hp1).oper[0]^.typ = top_const) and
                   (taicpu(hp1).oper[0]^.val <> 0) and
                  { does not work in case of overflow for G(E)/L(E)/C_O/C_NO }
                  { and in case of carry for A(E)/B(E)/C/NC                  }
                   (taicpu(hp2).condition in [C_Z,C_NZ,C_E,C_NE]) then
                  begin
                    DebugMsg(SPeepholeOptimization + 'OpTest/Or2Op (shift) done', hp1);
                    RemoveCurrentP(p, hp2);
                    Result:=true;
                    Exit;
                  end;
              end;
            A_DEC, A_INC, A_NEG:
              begin
                if OpsEqual(taicpu(hp1).oper[0]^,taicpu(p).oper[1]^) and
                  { does not work in case of overflow for G(E)/L(E)/C_O/C_NO }
                  { and in case of carry for A(E)/B(E)/C/NC                  }
                  (taicpu(hp2).condition in [C_Z,C_NZ,C_E,C_NE]) then
                  begin
                    DebugMsg(SPeepholeOptimization + 'OpTest/Or2Op (1-op) done', hp1);
                    RemoveCurrentP(p, hp2);
                    Result:=true;
                    Exit;
                  end;
              end;
            A_ANDN, A_BZHI:
              begin
                if OpsEqual(taicpu(hp1).oper[2]^,taicpu(p).oper[1]^) and
                  { Only the zero and sign flags are consistent with what the result is }
                  (taicpu(hp2).condition in [C_Z,C_NZ,C_E,C_NE,C_S,C_NS]) then
                  begin
                    DebugMsg(SPeepholeOptimization + 'OpTest/Or2Op (ANDN/BZHI) done', hp1);
                    RemoveCurrentP(p, hp2);
                    Result:=true;
                    Exit;
                  end;
              end;
            A_BEXTR:
              begin
                if OpsEqual(taicpu(hp1).oper[2]^,taicpu(p).oper[1]^) and
                  { Only the zero flag is set }
                  (taicpu(hp2).condition in [C_Z,C_NZ,C_E,C_NE]) then
                  begin
                    DebugMsg(SPeepholeOptimization + 'OpTest/Or2Op (BEXTR) done', hp1);
                    RemoveCurrentP(p, hp2);
                    Result:=true;
                    Exit;
                  end;
              end;
          else
            ;
          end; { case }

        { change "test  $-1,%reg" into "test %reg,%reg" }
        if IsTestConstX and (taicpu(p).oper[1]^.typ=top_reg) then
          taicpu(p).loadoper(0,taicpu(p).oper[1]^);

        { Change "or %reg,%reg" to "test %reg,%reg" as OR generates a false dependency }
        if MatchInstruction(p, A_OR, []) and
          { Can only match if they're both registers }
          MatchOperand(taicpu(p).oper[0]^, taicpu(p).oper[1]^) then
          begin
            DebugMsg(SPeepholeOptimization + 'or %reg,%reg -> test %reg,%reg to remove false dependency (Or2Test)', p);
            taicpu(p).opcode := A_TEST;
            { No need to set Result to True, as we've done all the optimisations we can }
          end;
      end;


    function TX86AsmOptimizer.PostPeepholeOptCall(var p : tai) : Boolean;
      var
        hp1,hp3 : tai;
{$ifndef x86_64}
        hp2 : taicpu;
{$endif x86_64}
      begin
        Result:=false;
        hp3:=nil;
{$ifndef x86_64}
        { don't do this on modern CPUs, this really hurts them due to
          broken call/ret pairing }
        if (current_settings.optimizecputype < cpu_Pentium2) and
           not(cs_create_pic in current_settings.moduleswitches) and
           GetNextInstruction(p, hp1) and
           MatchInstruction(hp1,A_JMP,[S_NO]) and
           MatchOpType(taicpu(hp1),top_ref) and
           (taicpu(hp1).oper[0]^.ref^.refaddr=addr_full) then
          begin
            hp2 := taicpu.Op_sym(A_PUSH,S_L,taicpu(hp1).oper[0]^.ref^.symbol);
            taicpu(hp2).fileinfo := taicpu(p).fileinfo;
            InsertLLItem(p.previous, p, hp2);
            taicpu(p).opcode := A_JMP;
            taicpu(p).is_jmp := true;
            RemoveInstruction(hp1);
            Result:=true;
          end
        else
{$endif x86_64}
        { replace
            call   procname
            ret
          by
            jmp    procname

          but do it only on level 4 because it destroys stack back traces

          else if the subroutine is marked as no return, remove the ret
        }
        if ((cs_opt_level4 in current_settings.optimizerswitches) or
          (po_noreturn in current_procinfo.procdef.procoptions)) and
          GetNextInstruction(p, hp1) and
          (MatchInstruction(hp1,A_RET,[S_NO]) or
           (MatchInstruction(hp1,A_VZEROUPPER,[S_NO]) and
            SetAndTest(hp1,hp3) and
            GetNextInstruction(hp1,hp1) and
            MatchInstruction(hp1,A_RET,[S_NO])
           )
          ) and
          (taicpu(hp1).ops=0) then
          begin
            if (cs_opt_level4 in current_settings.optimizerswitches) and
              { we might destroy stack alignment here if we do not do a call }
              (target_info.stackalign<=sizeof(SizeUInt)) then
              begin
                taicpu(p).opcode := A_JMP;
                taicpu(p).is_jmp := true;
                DebugMsg(SPeepholeOptimization + 'CallRet2Jmp done',p);
              end
            else
              DebugMsg(SPeepholeOptimization + 'CallRet2Call done',p);
            RemoveInstruction(hp1);
            if Assigned(hp3) then
              begin
                AsmL.Remove(hp3);
                AsmL.InsertBefore(hp3,p)
              end;
            Result:=true;
          end;
      end;


    function TX86AsmOptimizer.PostPeepholeOptMovzx(var p : tai) : Boolean;

      function ConstInRange(const Val: TCGInt; const OpSize: TOpSize): Boolean;
        begin
          case OpSize of
            S_B, S_BW, S_BL{$ifdef x86_64}, S_BQ{$endif x86_64}:
              Result := (Val <= $FF) and (Val >= -128);
            S_W, S_WL{$ifdef x86_64}, S_WQ{$endif x86_64}:
              Result := (Val <= $FFFF) and (Val >= -32768);
            S_L{$ifdef x86_64}, S_LQ{$endif x86_64}:
              Result := (Val <= $FFFFFFFF) and (Val >= -2147483648);
            else
              Result := True;
          end;
        end;

      var
        hp1, hp2 : tai;
        SizeChange: Boolean;
        PreMessage: string;
      begin
        Result := False;

        if (taicpu(p).oper[0]^.typ = top_reg) and
          SuperRegistersEqual(taicpu(p).oper[0]^.reg, taicpu(p).oper[1]^.reg) and
          GetNextInstruction(p, hp1) and (hp1.typ = ait_instruction) then
          begin
            { Change (using movzbl %al,%eax as an example):

                movzbl %al, %eax    movzbl %al, %eax
                cmpl   x,   %eax    testl  %eax,%eax

              To:
                cmpb   x,   %al     testb  %al, %al  (Move one back to avoid a false dependency)
                movzbl %al, %eax    movzbl %al, %eax

              Smaller instruction and minimises pipeline stall as the CPU
              doesn't have to wait for the register to get zero-extended. [Kit]

              Also allow if the smaller of the two registers is being checked,
              as this still removes the false dependency.
            }
            if
              (
                (
                  (taicpu(hp1).opcode = A_CMP) and MatchOpType(taicpu(hp1), top_const, top_reg) and
                  ConstInRange(taicpu(hp1).oper[0]^.val, taicpu(p).opsize)
                ) or (
                  { If MatchOperand returns True, they must both be registers }
                  (taicpu(hp1).opcode = A_TEST) and MatchOperand(taicpu(hp1).oper[0]^, taicpu(hp1).oper[1]^)
                )
              ) and
              (reg2opsize(taicpu(hp1).oper[1]^.reg) <= reg2opsize(taicpu(p).oper[1]^.reg)) and
              SuperRegistersEqual(taicpu(p).oper[1]^.reg, taicpu(hp1).oper[1]^.reg) then
              begin
                PreMessage := debug_op2str(taicpu(hp1).opcode) + debug_opsize2str(taicpu(hp1).opsize) + ' ' + debug_operstr(taicpu(hp1).oper[0]^) + ',' + debug_regname(taicpu(hp1).oper[1]^.reg) + ' -> ' + debug_op2str(taicpu(hp1).opcode);

                asml.Remove(hp1);
                asml.InsertBefore(hp1, p);

                { Swap instructions in the case of cmp 0,%reg or test %reg,%reg }
                if (taicpu(hp1).opcode = A_TEST) or (taicpu(hp1).oper[0]^.val = 0) then
                  begin
                    taicpu(hp1).opcode := A_TEST;
                    taicpu(hp1).loadreg(0, taicpu(p).oper[0]^.reg);
                  end;

                taicpu(hp1).oper[1]^.reg := taicpu(p).oper[0]^.reg;

                case taicpu(p).opsize of
                  S_BW, S_BL:
                    begin
                      SizeChange := taicpu(hp1).opsize <> S_B;
                      taicpu(hp1).changeopsize(S_B);
                    end;
                  S_WL:
                    begin
                      SizeChange := taicpu(hp1).opsize <> S_W;
                      taicpu(hp1).changeopsize(S_W);
                    end
                  else
                    InternalError(2020112701);
                end;

                UpdateUsedRegs(tai(p.Next));

                { Check if the register is used aferwards - if not, we can
                  remove the movzx instruction completely }
                if not RegUsedAfterInstruction(taicpu(hp1).oper[1]^.reg, p, UsedRegs) then
                  begin
                    { Hp1 is a better position than p for debugging purposes }
                    DebugMsg(SPeepholeOptimization + 'Movzx2Nop 4a', hp1);
                    RemoveCurrentp(p, hp1);
                    Result := True;
                  end;

                if SizeChange then
                  DebugMsg(SPeepholeOptimization + PreMessage +
                    debug_opsize2str(taicpu(hp1).opsize) + ' ' + debug_operstr(taicpu(hp1).oper[0]^) + ',' + debug_regname(taicpu(hp1).oper[1]^.reg) + ' (smaller and minimises pipeline stall - MovzxCmp2CmpMovzx)', hp1)
                else
                  DebugMsg(SPeepholeOptimization + 'MovzxCmp2CmpMovzx', hp1);

                Exit;
              end;

            { Change (using movzwl %ax,%eax as an example):

                movzwl %ax, %eax
                movb   %al, (dest)  (Register is smaller than read register in movz)

              To:
                movb   %al, (dest)  (Move one back to avoid a false dependency)
                movzwl %ax, %eax
            }
            if (taicpu(hp1).opcode = A_MOV) and
              (taicpu(hp1).oper[0]^.typ = top_reg) and
              not RegInOp(taicpu(hp1).oper[0]^.reg, taicpu(hp1).oper[1]^) and
              SuperRegistersEqual(taicpu(hp1).oper[0]^.reg, taicpu(p).oper[0]^.reg) and
              (reg2opsize(taicpu(hp1).oper[0]^.reg) <= reg2opsize(taicpu(p).oper[0]^.reg)) then
              begin
                DebugMsg(SPeepholeOptimization + 'MovzxMov2MovMovzx', hp1);

                hp2 := tai(hp1.Previous); { Effectively the old position of hp1 }
                asml.Remove(hp1);
                asml.InsertBefore(hp1, p);
                if taicpu(hp1).oper[1]^.typ = top_reg then
                  AllocRegBetween(taicpu(hp1).oper[1]^.reg, hp1, hp2, UsedRegs);

                { Check if the register is used aferwards - if not, we can
                  remove the movzx instruction completely }

                if not RegUsedAfterInstruction(taicpu(hp1).oper[0]^.reg, p, UsedRegs) then
                  begin
                    { Hp1 is a better position than p for debugging purposes }
                    DebugMsg(SPeepholeOptimization + 'Movzx2Nop 4b', hp1);
                    RemoveCurrentp(p, hp1);
                    Result := True;
                  end;

                Exit;
              end;

          end;
      end;


    function TX86AsmOptimizer.PostPeepholeOptXor(var p : tai) : Boolean;
      var
        hp1: tai;
{$ifdef x86_64}
        PreMessage, RegName: string;
{$endif x86_64}
      begin
        Result := False;

        { If x is a power of 2 (popcnt = 1), change:
            xor $x, %reg/ref

          To:
            btc lb(x), %reg/ref
        }
        if IsBTXAcceptable(p) and
          { IsBTXAcceptable checks to see if oper[0] is an immediate }
          (PopCnt(QWord(taicpu(p).oper[0]^.val)) = 1) and
          (
            { Don't optimise if a test instruction follows }
            not GetNextInstruction(p, hp1) or
            not MatchInstruction(hp1, A_TEST, [taicpu(p).opsize])
          ) then
          begin
            DebugMsg(SPeepholeOptimization + 'Changed XOR $' + debug_hexstr(taicpu(p).oper[0]^.val) + ' to BTC $' + debug_tostr(BsrQWord(taicpu(p).oper[0]^.val)) + ' to shrink instruction size (Xor2Btc)', p);
            taicpu(p).opcode := A_BTC;
            taicpu(p).oper[0]^.val := BsrQWord(taicpu(p).oper[0]^.val); { Essentially the base 2 logarithm }
            Result := True;
            Exit;
          end;

{$ifdef x86_64}
        { Code size reduction by J. Gareth "Kit" Moreton }
        { change "xorq %reg,%reg" to "xorl %reg,%reg" for %rax, %rcx, %rdx, %rbx, %rsi, %rdi, %rbp and %rsp,
          as this removes the REX prefix }

        if not OpsEqual(taicpu(p).oper[0]^,taicpu(p).oper[1]^) then
          Exit;

        if taicpu(p).oper[0]^.typ <> top_reg then
          { Should be impossible if both operands were equal, since one of XOR's operands must be a register }
          InternalError(2018011500);

        case taicpu(p).opsize of
          S_Q:
            begin
              RegName := debug_regname(taicpu(p).oper[0]^.reg); { 64-bit register name }
              PreMessage := 'xorq ' + RegName + ',' + RegName + ' -> xorl ';

              { The actual optimization }
              setsubreg(taicpu(p).oper[0]^.reg, R_SUBD);
              setsubreg(taicpu(p).oper[1]^.reg, R_SUBD);
              taicpu(p).changeopsize(S_L);

              RegName := debug_regname(taicpu(p).oper[0]^.reg); { 32-bit register name }

              DebugMsg(SPeepholeOptimization + PreMessage + RegName + ',' + RegName + ' (32-bit register recommended when zeroing 64-bit counterpart)', p);
            end;
          else
            ;
        end;
{$endif x86_64}
      end;

    function TX86AsmOptimizer.PostPeepholeOptVPXOR(var p : tai) : Boolean;
      var
        XReg: TRegister;
      begin
        Result := False;
        { Turn "vpxor %ymmreg2,%ymmreg2,%ymmreg1" to "vpxor %xmmreg2,%xmmreg2,%xmmreg1"
          Smaller encoding and slightly faster on some platforms (also works for
          ZMM-sized registers) }
        if (taicpu(p).opsize in [S_YMM, S_ZMM]) and
          MatchOpType(taicpu(p), top_reg, top_reg, top_reg) then
          begin
            XReg := taicpu(p).oper[0]^.reg;
            if (taicpu(p).oper[1]^.reg = XReg) then
              begin
                taicpu(p).changeopsize(S_XMM);
                setsubreg(taicpu(p).oper[2]^.reg, R_SUBMMX);
                if (cs_opt_size in current_settings.optimizerswitches) then
                  begin
                    { Change input registers to %xmm0 to reduce size.  Note that
                      there's a risk of a false dependency doing this, so only
                      optimise for size here }
                    XReg := NR_XMM0;
                    DebugMsg(SPeepholeOptimization + 'Changed zero-setting vpxor from Y/ZMM to XMM and changed input registers to %xmm0 to reduce size', p);
                  end
                else
                  begin
                    setsubreg(XReg, R_SUBMMX);
                    DebugMsg(SPeepholeOptimization + 'Changed zero-setting vpxor from Y/ZMM to XMM to reduce size and increase efficiency', p);
                  end;
                taicpu(p).oper[0]^.reg := XReg;
                taicpu(p).oper[1]^.reg := XReg;
                Result := True;
              end;
          end;
      end;


    class procedure TX86AsmOptimizer.OptimizeRefs(var p: taicpu);
      var
        OperIdx: Integer;
      begin
        for OperIdx := 0 to p.ops - 1 do
          if p.oper[OperIdx]^.typ = top_ref then
            optimize_ref(p.oper[OperIdx]^.ref^, False);
      end;

end.

