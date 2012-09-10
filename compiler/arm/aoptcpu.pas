{
    Copyright (c) 1998-2002 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit implements the ARM optimizer object

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

{$define DEBUG_PREREGSCHEDULER}
{$define DEBUG_AOPTCPU}

Interface

uses cgbase, cpubase, aasmtai, aasmcpu,aopt, aoptcpub, aoptobj;

Type
  TCpuAsmOptimizer = class(TAsmOptimizer)
    { uses the same constructor as TAopObj }
    function PeepHoleOptPass1Cpu(var p: tai): boolean; override;
    procedure PeepHoleOptPass2;override;
    Function RegInInstruction(Reg: TRegister; p1: tai): Boolean;override;
    procedure RemoveSuperfluousMove(const p: tai; movp: tai; const optimizer: string);
    function RegUsedAfterInstruction(reg: Tregister; p: tai;
                                     var AllUsedRegs: TAllUsedRegs): Boolean;
    { gets the next tai object after current that contains info relevant
      to the optimizer in p1 which used the given register or does a 
      change in program flow.
      If there is none, it returns false and
      sets p1 to nil                                                     }
    Function GetNextInstructionUsingReg(Current: tai; Var Next: tai;reg : TRegister): Boolean;

    { outputs a debug message into the assembler file }
    procedure DebugMsg(const s: string; p: tai);

  protected
    function LookForPostindexedPattern(p: taicpu): boolean;
  End;

  TCpuPreRegallocScheduler = class(TAsmScheduler)
    function SchedulerPass1Cpu(var p: tai): boolean;override;
    procedure SwapRegLive(p, hp1: taicpu);
  end;

  TCpuThumb2AsmOptimizer = class(TCpuAsmOptimizer)
    { uses the same constructor as TAopObj }
    procedure PeepHoleOptPass2;override;
  End;

Implementation

  uses
    cutils,verbose,globals,
    systems,
    cpuinfo,
    cgobj,cgutils,procinfo,
    aasmbase,aasmdata;

  function CanBeCond(p : tai) : boolean;
    begin
      result:=
        (p.typ=ait_instruction) and
        (taicpu(p).condition=C_None) and
        (taicpu(p).opcode<>A_PLD) and
        ((taicpu(p).opcode<>A_BLX) or
         (taicpu(p).oper[0]^.typ=top_reg));
    end;


  function RefsEqual(const r1, r2: treference): boolean;
    begin
      refsequal :=
        (r1.offset = r2.offset) and
        (r1.base = r2.base) and
        (r1.index = r2.index) and (r1.scalefactor = r2.scalefactor) and
        (r1.symbol=r2.symbol) and (r1.refaddr = r2.refaddr) and
        (r1.relsymbol = r2.relsymbol) and
        (r1.signindex = r2.signindex) and
        (r1.shiftimm = r2.shiftimm) and
        (r1.addressmode = r2.addressmode) and
        (r1.shiftmode = r2.shiftmode);
    end;

  function MatchInstruction(const instr: tai; const op: TCommonAsmOps; const cond: TAsmConds; const postfix: TOpPostfixes): boolean;
  begin
    result :=
      (instr.typ = ait_instruction) and
      ((op = []) or ((ord(taicpu(instr).opcode)<256) and (taicpu(instr).opcode in op))) and
      ((cond = []) or (taicpu(instr).condition in cond)) and
      ((postfix = []) or (taicpu(instr).oppostfix in postfix));
  end;

  function MatchInstruction(const instr: tai; const op: TAsmOp; const cond: TAsmConds; const postfix: TOpPostfixes): boolean;
  begin
    result :=
      (instr.typ = ait_instruction) and
      (taicpu(instr).opcode = op) and
      ((cond = []) or (taicpu(instr).condition in cond)) and
      ((postfix = []) or (taicpu(instr).oppostfix in postfix));
  end;

  function MatchOperand(const oper1: TOper; const oper2: TOper): boolean; inline;
    begin
      result := oper1.typ = oper2.typ;

      if result then
        case oper1.typ of
          top_const:
            Result:=oper1.val = oper2.val;
          top_reg:
            Result:=oper1.reg = oper2.reg;
          top_conditioncode:
            Result:=oper1.cc = oper2.cc;
          top_ref:
            Result:=RefsEqual(oper1.ref^, oper2.ref^);
          else Result:=false;
        end
    end;

  function MatchOperand(const oper: TOper; const reg: TRegister): boolean; inline;
    begin
      result := (oper.typ = top_reg) and (oper.reg = reg);
    end;

  procedure RemoveRedundantMove(const cmpp: tai; movp: tai; asml: TAsmList);
    begin
      if (taicpu(movp).condition = C_EQ) and
         (taicpu(cmpp).oper[0]^.reg = taicpu(movp).oper[0]^.reg) and
         (taicpu(cmpp).oper[1]^.val = taicpu(movp).oper[1]^.val) then
      begin
        asml.insertafter(tai_comment.Create(strpnew('Peephole CmpMovMov - Removed redundant moveq')), movp);
        asml.remove(movp);
        movp.free;
      end;
    end;

  function regLoadedWithNewValue(reg: tregister; hp: tai): boolean;
  var
    p: taicpu;
  begin
    p := taicpu(hp);
    regLoadedWithNewValue := false;
    if not ((assigned(hp)) and (hp.typ = ait_instruction)) then
      exit;

    case p.opcode of
      { These operands do not write into a register at all }
      A_CMP, A_CMN, A_TST, A_TEQ, A_B, A_BL, A_BX, A_BLX, A_SWI, A_MSR, A_PLD:
        exit;
      {Take care of post/preincremented store and loads, they will change their base register}
      A_STR, A_LDR:
        regLoadedWithNewValue :=
          (taicpu(p).oper[1]^.typ=top_ref) and
          (taicpu(p).oper[1]^.ref^.addressmode in [AM_PREINDEXED,AM_POSTINDEXED]) and
          (taicpu(p).oper[1]^.ref^.base = reg);
      { These four are writing into the first 2 register, UMLAL and SMLAL will also read from them }
      A_UMLAL, A_UMULL, A_SMLAL, A_SMULL:
        regLoadedWithNewValue :=
          (p.oper[1]^.typ = top_reg) and
          (p.oper[1]^.reg = reg);
      {Loads to oper2 from coprocessor}
      {
      MCR/MRC is currently not supported in FPC
      A_MRC:
        regLoadedWithNewValue :=
          (p.oper[2]^.typ = top_reg) and
          (p.oper[2]^.reg = reg);
      }
      {Loads to all register in the registerset}
      A_LDM:
        regLoadedWithNewValue := (getsupreg(reg) in p.oper[1]^.regset^);
    end;

    if regLoadedWithNewValue then
      exit;

    case p.oper[0]^.typ of
      {This is the case}
      top_reg:
        regLoadedWithNewValue := (p.oper[0]^.reg = reg) or
          { LDRD }
          (p.opcode=A_LDR) and (p.oppostfix=PF_D) and (getsupreg(p.oper[0]^.reg)+1=getsupreg(reg));
      {LDM/STM might write a new value to their index register}
      top_ref:
        regLoadedWithNewValue :=
          (taicpu(p).oper[0]^.ref^.addressmode in [AM_PREINDEXED,AM_POSTINDEXED]) and
          (taicpu(p).oper[0]^.ref^.base = reg);
    end;
  end;


  function AlignedToQWord(const ref : treference) : boolean;
    begin
      { (safe) heuristics to ensure alignment }
      result:=(target_info.abi in [abi_eabi,abi_armeb,abi_eabihf]) and
      (((ref.offset>=0) and
        ((ref.offset mod 8)=0) and
        ((ref.base=NR_R13) or
         (ref.index=NR_R13))
        ) or
       ((ref.offset<=0) and
        { when using NR_R11, it has always a value of <qword align>+4 }
        ((abs(ref.offset+4) mod 8)=0) and
        (current_procinfo.framepointer=NR_R11) and
        ((ref.base=NR_R11) or
         (ref.index=NR_R11))
       )
      );
    end;


  function instructionLoadsFromReg(const reg: TRegister; const hp: tai): boolean;
  var
    p: taicpu;
    i: longint;
  begin
    instructionLoadsFromReg := false;
    if not (assigned(hp) and (hp.typ = ait_instruction)) then
      exit;
    p:=taicpu(hp);

    i:=1;
    {For these instructions we have to start on oper[0]}
    if (p.opcode in [A_STR, A_LDM, A_STM, A_PLD,
                        A_CMP, A_CMN, A_TST, A_TEQ,
                        A_B, A_BL, A_BX, A_BLX,
                        A_SMLAL, A_UMLAL]) then i:=0;

    while(i<p.ops) do
      begin
        case p.oper[I]^.typ of
          top_reg:
            instructionLoadsFromReg := (p.oper[I]^.reg = reg) or
              { STRD }
              ((i=0) and (p.opcode=A_STR) and (p.oppostfix=PF_D) and (getsupreg(p.oper[0]^.reg)+1=getsupreg(reg)));
          top_regset:
            instructionLoadsFromReg := (getsupreg(reg) in p.oper[I]^.regset^);
          top_shifterop:
            instructionLoadsFromReg := p.oper[I]^.shifterop^.rs = reg;
          top_ref:
            instructionLoadsFromReg :=
              (p.oper[I]^.ref^.base = reg) or
              (p.oper[I]^.ref^.index = reg);
        end;
        if instructionLoadsFromReg then exit; {Bailout if we found something}
        Inc(I);
      end;
  end;


  function TCpuAsmOptimizer.RegUsedAfterInstruction(reg: Tregister; p: tai;
    var AllUsedRegs: TAllUsedRegs): Boolean;
    begin
      AllUsedRegs[getregtype(reg)].Update(tai(p.Next),true);
      RegUsedAfterInstruction :=
        AllUsedRegs[getregtype(reg)].IsUsed(reg) and
        not(regLoadedWithNewValue(reg,p)) and
        (
          not(GetNextInstruction(p,p)) or
          instructionLoadsFromReg(reg,p) or
          not(regLoadedWithNewValue(reg,p))
        );
    end;


  function TCpuAsmOptimizer.GetNextInstructionUsingReg(Current: tai;
    var Next: tai; reg: TRegister): Boolean;
    begin
      Next:=Current;
      repeat
        Result:=GetNextInstruction(Next,Next);
      until not(Result) or (Next.typ<>ait_instruction) or (RegInInstruction(reg,Next)) or
        (is_calljmp(taicpu(Next).opcode)) or (RegInInstruction(NR_PC,Next));
    end;

{$ifdef DEBUG_AOPTCPU}
  procedure TCpuAsmOptimizer.DebugMsg(const s: string;p : tai);
    begin
      asml.insertbefore(tai_comment.Create(strpnew(s)), p);
    end;
{$else DEBUG_AOPTCPU}
  procedure TCpuAsmOptimizer.DebugMsg(const s: string;p : tai);inline;
    begin
    end;
{$endif DEBUG_AOPTCPU}

  procedure TCpuAsmOptimizer.RemoveSuperfluousMove(const p: tai; movp: tai; const optimizer: string);
    var
      alloc,
      dealloc : tai_regalloc;
      hp1 : tai;
    begin
      if MatchInstruction(movp, A_MOV, [taicpu(p).condition], [PF_None]) and
         (taicpu(movp).ops=2) and {We can't optimize if there is a shiftop}
         MatchOperand(taicpu(movp).oper[1]^, taicpu(p).oper[0]^.reg) and
         { don't mess with moves to pc }
         (taicpu(movp).oper[0]^.reg<>NR_PC) and
         { don't mess with moves to lr }
         (taicpu(movp).oper[0]^.reg<>NR_R14) and
         { the destination register of the mov might not be used beween p and movp }
         not(RegUsedBetween(taicpu(movp).oper[0]^.reg,p,movp)) and
         {There is a special requirement for MUL and MLA, oper[0] and oper[1] are not allowed to be the same}
         not (
           (taicpu(p).opcode in [A_MLA, A_MUL]) and
           (taicpu(p).oper[1]^.reg = taicpu(movp).oper[0]^.reg)
         ) then
        begin
          dealloc:=FindRegDeAlloc(taicpu(p).oper[0]^.reg,tai(movp.Next));
          if assigned(dealloc) then
            begin
              DebugMsg('Peephole '+optimizer+' removed superfluous mov', movp);

              { taicpu(p).oper[0]^.reg is not used anymore, try to find its allocation
                and remove it if possible }
              GetLastInstruction(p,hp1);
              asml.Remove(dealloc);
              alloc:=FindRegAlloc(taicpu(p).oper[0]^.reg,tai(hp1.Next));
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

              { finally get rid of the mov }
              taicpu(p).loadreg(0,taicpu(movp).oper[0]^.reg);
              asml.remove(movp);
              movp.free;
            end;
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
  function TCpuAsmOptimizer.LookForPostindexedPattern(p: taicpu) : boolean;
    var
      hp1 : tai;
    begin
      Result:=false;
      if (p.oper[1]^.ref^.addressmode=AM_OFFSET) and
        (p.oper[1]^.ref^.index=NR_NO) and
        (p.oper[1]^.ref^.offset=0) and
        GetNextInstructionUsingReg(p, hp1, p.oper[1]^.ref^.base) and
        { we cannot check NR_DEFAULTFLAGS for modification yet so don't allow a condition }
        MatchInstruction(hp1, [A_ADD, A_SUB], [C_None], [PF_None]) and
        (taicpu(hp1).oper[0]^.reg=p.oper[1]^.ref^.base) and
        (taicpu(hp1).oper[1]^.reg=p.oper[1]^.ref^.base) and
        (
         (taicpu(hp1).oper[2]^.typ=top_reg) or
         { valid offset? }
         ((taicpu(hp1).oper[2]^.typ=top_const) and
          ((abs(taicpu(hp1).oper[2]^.val)<256) or
           ((abs(taicpu(hp1).oper[2]^.val)<4096) and (p.oppostfix in [PF_None,PF_B]))
          )
         )
        ) and
        { don't apply the optimization if the base register is loaded }
        (p.oper[0]^.reg<>p.oper[1]^.ref^.base) and
        not(RegModifiedBetween(taicpu(hp1).oper[0]^.reg,p,hp1)) and
        { don't apply the optimization if the (new) index register is loaded }
        (p.oper[0]^.reg<>taicpu(hp1).oper[2]^.reg) and
        not(RegModifiedBetween(taicpu(hp1).oper[2]^.reg,p,hp1)) then
        begin
          DebugMsg('Peephole Str/LdrAdd/Sub2Str/Ldr Postindex done', p);
          p.oper[1]^.ref^.addressmode:=AM_POSTINDEXED;
          if taicpu(hp1).oper[2]^.typ=top_const then
            begin
              if taicpu(hp1).opcode=A_ADD then
                p.oper[1]^.ref^.offset:=taicpu(hp1).oper[2]^.val
              else
                p.oper[1]^.ref^.offset:=-taicpu(hp1).oper[2]^.val;
            end
          else
            begin
              p.oper[1]^.ref^.index:=taicpu(hp1).oper[2]^.reg;
              if taicpu(hp1).opcode=A_ADD then
                p.oper[1]^.ref^.signindex:=1
              else
                p.oper[1]^.ref^.signindex:=-1;
            end;
          asml.Remove(hp1);
          hp1.Free;
          Result:=true;
        end;
    end;


  function TCpuAsmOptimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;
    var
      hp1,hp2: tai;
      i, i2: longint;
      TmpUsedRegs: TAllUsedRegs;
      tempop: tasmop;

    function IsPowerOf2(const value: DWord): boolean; inline;
      begin
        Result:=(value and (value - 1)) = 0;
      end;

    begin
      result := false;
      case p.typ of
        ait_instruction:
          begin
            {
              change
              <op> reg,x,y
              cmp reg,#0
              into
              <op>s reg,x,y
            }
            { this optimization can applied only to the currently enabled operations because
              the other operations do not update all flags and FPC does not track flag usage }
            if MatchInstruction(p, [A_ADC,A_ADD,A_BIC,A_SUB,A_MUL,A_MVN,A_MOV,A_ORR,A_EOR,A_AND,
                                 A_RSB,A_RSC,A_SBC,A_MLA], [C_None], [PF_None]) and
              GetNextInstruction(p, hp1) and
              MatchInstruction(hp1, A_CMP, [C_None], [PF_None]) and
              (taicpu(hp1).oper[1]^.typ = top_const) and
              (taicpu(p).oper[0]^.reg = taicpu(hp1).oper[0]^.reg) and
              (taicpu(hp1).oper[1]^.val = 0) and
              GetNextInstruction(hp1, hp2) and
              { be careful here, following instructions could use other flags
                however after a jump fpc never depends on the value of flags }
              { All above instructions set Z and N according to the following
                Z := result = 0;
                N := result[31];
                EQ = Z=1; NE = Z=0;
                MI = N=1; PL = N=0; }
              MatchInstruction(hp2, A_B, [C_EQ,C_NE,C_MI,C_PL], []) and
              assigned(FindRegDealloc(NR_DEFAULTFLAGS,tai(hp2.Next))) then
             begin
               DebugMsg('Peephole OpCmp2OpS done', p);

               taicpu(p).oppostfix:=PF_S;

               { move flag allocation if possible }
               GetLastInstruction(hp1, hp2);
               hp2:=FindRegAlloc(NR_DEFAULTFLAGS,tai(hp2.Next));
               if assigned(hp2) then
                 begin
                   asml.Remove(hp2);
                   asml.insertbefore(hp2, p);
                 end;

               asml.remove(hp1);
               hp1.free;
             end
           else
              case taicpu(p).opcode of
                A_STR:
                  begin
                    { change
                      str reg1,ref
                      ldr reg2,ref
                      into
                      str reg1,ref
                      mov reg2,reg1
                    }
                    if (taicpu(p).oper[1]^.ref^.addressmode=AM_OFFSET) and
                       (taicpu(p).oppostfix=PF_None) and
                       GetNextInstruction(p,hp1) and
                       MatchInstruction(hp1, A_LDR, [taicpu(p).condition, C_None], [PF_None]) and
                       RefsEqual(taicpu(p).oper[1]^.ref^,taicpu(hp1).oper[1]^.ref^) and
                       (taicpu(hp1).oper[1]^.ref^.addressmode=AM_OFFSET) then
                      begin
                        if taicpu(hp1).oper[0]^.reg=taicpu(p).oper[0]^.reg then
                          begin
                            DebugMsg('Peephole StrLdr2StrMov 1 done', hp1);
                            asml.remove(hp1);
                            hp1.free;                            
                          end
                        else
                          begin
                            taicpu(hp1).opcode:=A_MOV;
                            taicpu(hp1).oppostfix:=PF_None;
                            taicpu(hp1).loadreg(1,taicpu(p).oper[0]^.reg);
                            DebugMsg('Peephole StrLdr2StrMov 2 done', hp1);
                          end;
                        result := true;
                      end
                    { change
                      str reg1,ref
                      str reg2,ref
                      into
                      strd reg1,ref
                    }
                    else if (CPUARM_HAS_EDSP in cpu_capabilities[current_settings.cputype]) and
                       (taicpu(p).oppostfix=PF_None) and
                       (taicpu(p).oper[1]^.ref^.addressmode=AM_OFFSET) and
                       GetNextInstruction(p,hp1) and
                       MatchInstruction(hp1, A_STR, [taicpu(p).condition, C_None], [PF_None]) and
                       not(odd(getsupreg(taicpu(p).oper[0]^.reg))) and
                      (getsupreg(taicpu(p).oper[0]^.reg)+1=getsupreg(taicpu(hp1).oper[0]^.reg)) and
                      { str ensures that either base or index contain no register, else ldr wouldn't
                        use an offset either
                      }
                      (taicpu(p).oper[1]^.ref^.base=taicpu(hp1).oper[1]^.ref^.base) and
                      (taicpu(p).oper[1]^.ref^.index=taicpu(hp1).oper[1]^.ref^.index) and
                      (taicpu(p).oper[1]^.ref^.offset+4=taicpu(hp1).oper[1]^.ref^.offset) and
                      (abs(taicpu(p).oper[1]^.ref^.offset)<256) and
                      AlignedToQWord(taicpu(p).oper[1]^.ref^) then
                      begin
                        DebugMsg('Peephole StrStr2Strd done', p);
                        taicpu(p).oppostfix:=PF_D;
                        asml.remove(hp1);
                        hp1.free;
                      end;
                    LookForPostindexedPattern(taicpu(p));
                  end;
                A_LDR:
                  begin
                    { change
                      ldr reg1,ref
                      ldr reg2,ref
                      into ...
                    }
                    if (taicpu(p).oper[1]^.ref^.addressmode=AM_OFFSET) and
                       GetNextInstruction(p,hp1) and
                       { ldrd is not allowed here }
                       MatchInstruction(hp1, A_LDR, [taicpu(p).condition, C_None], [taicpu(p).oppostfix,PF_None]-[PF_D]) then
                      begin
                        {
                          ...
                          ldr reg1,ref
                          mov reg2,reg1
                        }
                        if RefsEqual(taicpu(p).oper[1]^.ref^,taicpu(hp1).oper[1]^.ref^) and
                         (taicpu(p).oper[0]^.reg<>taicpu(hp1).oper[1]^.ref^.index) and
                         (taicpu(p).oper[0]^.reg<>taicpu(hp1).oper[1]^.ref^.base) and
                         (taicpu(hp1).oper[1]^.ref^.addressmode=AM_OFFSET) then
                          begin
                            if taicpu(hp1).oper[0]^.reg=taicpu(p).oper[0]^.reg then
                              begin
                                DebugMsg('Peephole LdrLdr2Ldr done', hp1);
                                asml.remove(hp1);
                                hp1.free;
                              end
                            else
                              begin
                                DebugMsg('Peephole LdrLdr2LdrMov done', hp1);
                                taicpu(hp1).opcode:=A_MOV;
                                taicpu(hp1).oppostfix:=PF_None;
                                taicpu(hp1).loadreg(1,taicpu(p).oper[0]^.reg);
                              end;
                            result := true;
                          end
                        {
                           ...
                           ldrd reg1,ref
                        }
                        else if (CPUARM_HAS_EDSP in cpu_capabilities[current_settings.cputype]) and
                          { ldrd does not allow any postfixes ... }
                          (taicpu(p).oppostfix=PF_None) and
                          not(odd(getsupreg(taicpu(p).oper[0]^.reg))) and
                          (getsupreg(taicpu(p).oper[0]^.reg)+1=getsupreg(taicpu(hp1).oper[0]^.reg)) and
                          { ldr ensures that either base or index contain no register, else ldr wouldn't
                            use an offset either
                          }
                          (taicpu(p).oper[1]^.ref^.base=taicpu(hp1).oper[1]^.ref^.base) and
                          (taicpu(p).oper[1]^.ref^.index=taicpu(hp1).oper[1]^.ref^.index) and
                          (taicpu(p).oper[1]^.ref^.offset+4=taicpu(hp1).oper[1]^.ref^.offset) and
                          (abs(taicpu(p).oper[1]^.ref^.offset)<256) and
                          AlignedToQWord(taicpu(p).oper[1]^.ref^) then
                          begin
                            DebugMsg('Peephole LdrLdr2Ldrd done', p);
                            taicpu(p).oppostfix:=PF_D;
                            asml.remove(hp1);
                            hp1.free;
                          end;
                      end;

                    LookForPostindexedPattern(taicpu(p));
                    { Remove superfluous mov after ldr
                      changes
                      ldr reg1, ref
                      mov reg2, reg1
                      to
                      ldr reg2, ref

                      conditions are:
                        * no ldrd usage
                        * reg1 must be released after mov
                        * mov can not contain shifterops
                        * ldr+mov have the same conditions
                        * mov does not set flags
                    }
                    if (taicpu(p).oppostfix<>PF_D) and GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) then
                      RemoveSuperfluousMove(p, hp1, 'LdrMov2Ldr');
                  end;
                A_MOV:
                  begin
                    { fold
                      mov reg1,reg0, shift imm1
                      mov reg1,reg1, shift imm2
                    }
                    if (taicpu(p).ops=3) and
                       (taicpu(p).oper[2]^.typ = top_shifterop) and
                       (taicpu(p).oper[2]^.shifterop^.rs = NR_NO) and
                       getnextinstruction(p,hp1) and
                       MatchInstruction(hp1, A_MOV, [taicpu(p).condition], [PF_None]) and
                       (taicpu(hp1).ops=3) and
                       MatchOperand(taicpu(hp1).oper[0]^, taicpu(p).oper[0]^.reg) and
                       MatchOperand(taicpu(hp1).oper[1]^, taicpu(p).oper[0]^.reg) and
                       (taicpu(hp1).oper[2]^.typ = top_shifterop) and
                       (taicpu(hp1).oper[2]^.shifterop^.rs = NR_NO) then
                      begin
                        { fold
                          mov reg1,reg0, lsl 16
                          mov reg1,reg1, lsr 16
                          strh reg1, ...
                          dealloc reg1
                          to
                          strh reg1, ...
                          dealloc reg1
                        }
                        if (taicpu(p).oper[2]^.shifterop^.shiftmode=SM_LSL) and
                          (taicpu(p).oper[2]^.shifterop^.shiftimm=16) and
                          (taicpu(hp1).oper[2]^.shifterop^.shiftmode in [SM_LSR,SM_ASR]) and
                          (taicpu(hp1).oper[2]^.shifterop^.shiftimm=16) and
                          getnextinstruction(hp1,hp2) and
                          MatchInstruction(hp2, A_STR, [taicpu(p).condition], [PF_H]) and
                          MatchOperand(taicpu(hp2).oper[0]^, taicpu(p).oper[0]^.reg) then
                          begin
                            CopyUsedRegs(TmpUsedRegs);
                            UpdateUsedRegs(TmpUsedRegs, tai(p.next));
                            UpdateUsedRegs(TmpUsedRegs, tai(hp1.next));
                            if not(RegUsedAfterInstruction(taicpu(p).oper[0]^.reg,hp2,TmpUsedRegs)) then
                              begin
                                DebugMsg('Peephole optimizer removed superfluous 16 Bit zero extension', hp1);
                                taicpu(hp2).loadreg(0,taicpu(p).oper[1]^.reg);
                                asml.remove(p);
                                asml.remove(hp1);
                                p.free;
                                hp1.free;
                                p:=hp2;
                              end;
                            ReleaseUsedRegs(TmpUsedRegs);
                          end
                        { fold
                          mov reg1,reg0, shift imm1
                          mov reg1,reg1, shift imm2
                          to
                          mov reg1,reg0, shift imm1+imm2
                        }
                        else if (taicpu(p).oper[2]^.shifterop^.shiftmode=taicpu(hp1).oper[2]^.shifterop^.shiftmode) or
                          { asr makes no use after a lsr, the asr can be foled into the lsr }
                           ((taicpu(p).oper[2]^.shifterop^.shiftmode=SM_LSR) and (taicpu(hp1).oper[2]^.shifterop^.shiftmode=SM_ASR) ) then
                          begin
                            inc(taicpu(p).oper[2]^.shifterop^.shiftimm,taicpu(hp1).oper[2]^.shifterop^.shiftimm);
                            { avoid overflows }
                            if taicpu(p).oper[2]^.shifterop^.shiftimm>31 then
                              case taicpu(p).oper[2]^.shifterop^.shiftmode of
                                SM_ROR:
                                  taicpu(p).oper[2]^.shifterop^.shiftimm:=taicpu(p).oper[2]^.shifterop^.shiftimm and 31;
                                SM_ASR:
                                  taicpu(p).oper[2]^.shifterop^.shiftimm:=31;
                                SM_LSR,
                                SM_LSL:
                                  begin
                                    hp1:=taicpu.op_reg_const(A_MOV,taicpu(p).oper[0]^.reg,0);
                                    InsertLLItem(p.previous, p.next, hp1);
                                    p.free;
                                    p:=hp1;
                                  end;
                                else
                                  internalerror(2008072803);
                              end;
                            DebugMsg('Peephole ShiftShift2Shift 1 done', p);
                            asml.remove(hp1);
                            hp1.free;
                            result := true;
                          end
                        { fold
                          mov reg1,reg0, shift imm1
                          mov reg1,reg1, shift imm2
                          mov reg1,reg1, shift imm3 ...
                        }
                        else if getnextinstruction(hp1,hp2) and
                          MatchInstruction(hp2, A_MOV, [taicpu(p).condition], [PF_None]) and
                          (taicpu(hp2).ops=3) and
                          MatchOperand(taicpu(hp2).oper[0]^, taicpu(hp1).oper[0]^.reg) and
                          MatchOperand(taicpu(hp2).oper[1]^, taicpu(hp1).oper[0]^.reg) and
                          (taicpu(hp2).oper[2]^.typ = top_shifterop) and
                          (taicpu(hp2).oper[2]^.shifterop^.rs = NR_NO) then
                          begin
                            { mov reg1,reg0, lsl imm1
                              mov reg1,reg1, lsr/asr imm2
                              mov reg1,reg1, lsl imm3 ...

                              if imm3<=imm1 and imm2>=imm3
                              to
                              mov reg1,reg0, lsl imm1
                              mov reg1,reg1, lsr/asr imm2-imm3
                            }
                            if (taicpu(p).oper[2]^.shifterop^.shiftmode=SM_LSL) and (taicpu(hp2).oper[2]^.shifterop^.shiftmode=SM_LSL) and
                              (taicpu(hp1).oper[2]^.shifterop^.shiftmode in [SM_ASR,SM_LSR]) and
                              (taicpu(hp2).oper[2]^.shifterop^.shiftimm<=taicpu(p).oper[2]^.shifterop^.shiftimm) and
                              (taicpu(hp1).oper[2]^.shifterop^.shiftimm>=taicpu(hp2).oper[2]^.shifterop^.shiftimm) then
                              begin
                                dec(taicpu(hp1).oper[2]^.shifterop^.shiftimm,taicpu(hp2).oper[2]^.shifterop^.shiftimm);
                                DebugMsg('Peephole ShiftShiftShift2ShiftShift 1 done', p);
                                asml.remove(hp2);
                                hp2.free;
                                result := true;
                                if taicpu(hp1).oper[2]^.shifterop^.shiftimm=0 then
                                  begin
                                    asml.remove(hp1);
                                    hp1.free;
                                  end;
                              end
                            { mov reg1,reg0, lsr/asr imm1
                              mov reg1,reg1, lsl imm2
                              mov reg1,reg1, lsr/asr imm3 ...

                              if imm3>=imm1 and imm2>=imm1
                              to
                              mov reg1,reg0, lsl imm2-imm1
                              mov reg1,reg1, lsr/asr imm3 ...
                            }
                            else if (taicpu(p).oper[2]^.shifterop^.shiftmode in [SM_ASR,SM_LSR]) and (taicpu(hp2).oper[2]^.shifterop^.shiftmode in [SM_ASR,SM_LSR]) and
                              (taicpu(hp1).oper[2]^.shifterop^.shiftmode=SM_LSL) and
                              (taicpu(hp2).oper[2]^.shifterop^.shiftimm>=taicpu(p).oper[2]^.shifterop^.shiftimm) and
                              (taicpu(hp1).oper[2]^.shifterop^.shiftimm>=taicpu(p).oper[2]^.shifterop^.shiftimm) then
                              begin
                                dec(taicpu(hp1).oper[2]^.shifterop^.shiftimm,taicpu(p).oper[2]^.shifterop^.shiftimm);
                                taicpu(hp1).oper[1]^.reg:=taicpu(p).oper[1]^.reg;
                                DebugMsg('Peephole ShiftShiftShift2ShiftShift 2 done', p);
                                asml.remove(p);
                                p.free;
                                p:=hp2;
                                if taicpu(hp1).oper[2]^.shifterop^.shiftimm=0 then
                                  begin
                                    taicpu(hp2).oper[1]^.reg:=taicpu(hp1).oper[1]^.reg;
                                    asml.remove(hp1);
                                    hp1.free;
                                    p:=hp2;
                                  end;
                                result := true;
                              end;
                          end;
                      end;
                    { Change the common
                      mov r0, r0, lsr #24
                      and r0, r0, #255

                      and remove the superfluous and

                      This could be extended to handle more cases.
                    }
                    if (taicpu(p).ops=3) and
                       (taicpu(p).oper[2]^.typ = top_shifterop) and
                       (taicpu(p).oper[2]^.shifterop^.rs = NR_NO) and
                       (taicpu(p).oper[2]^.shifterop^.shiftmode = SM_LSR) and
                       (taicpu(p).oper[2]^.shifterop^.shiftimm >= 24 ) and
                       getnextinstruction(p,hp1) and
                       MatchInstruction(hp1, A_AND, [taicpu(p).condition], [taicpu(p).oppostfix]) and
                       (taicpu(hp1).ops=3) and
                       MatchOperand(taicpu(p).oper[0]^, taicpu(hp1).oper[0]^) and
                       MatchOperand(taicpu(p).oper[0]^, taicpu(hp1).oper[1]^) and
                       (taicpu(hp1).oper[2]^.typ = top_const) and
                       { Check if the AND actually would only mask out bits beeing already zero because of the shift
                         For LSR #25 and an AndConst of 255 that whould go like this:
                         255 and ((2 shl (32-25))-1)
                         which results in 127, which is one less a power-of-2, meaning all lower bits are set.

                         LSR #25 and AndConst of 254:
                         254 and ((2 shl (32-25))-1) = 126 -> lowest bit is clear, so we can't remove it.
                       }
                       ispowerof2((taicpu(hp1).oper[2]^.val and ((2 shl (32-taicpu(p).oper[2]^.shifterop^.shiftimm))-1))+1) then
                      begin
                        DebugMsg('Peephole LsrAnd2Lsr done', hp1);
                        asml.remove(hp1);
                        hp1.free;
                      end;

                    {
                      optimize
                      mov rX, yyyy
                      ....
                    }
                    if (taicpu(p).ops = 2) and
                       GetNextInstruction(p,hp1) and
                       (tai(hp1).typ = ait_instruction) then
                      begin
                        {
                          This changes the very common
                          mov r0, #0
                          str r0, [...]
                          mov r0, #0
                          str r0, [...]

                          and removes all superfluous mov instructions
                        }
                        if (taicpu(p).oper[1]^.typ = top_const) and
                           (taicpu(hp1).opcode=A_STR) then
                          while MatchInstruction(hp1, A_STR, [taicpu(p).condition], []) and
                                MatchOperand(taicpu(p).oper[0]^, taicpu(hp1).oper[0]^) and
                                GetNextInstruction(hp1, hp2) and
                                MatchInstruction(hp2, A_MOV, [taicpu(p).condition], [PF_None]) and
                                (taicpu(hp2).ops = 2) and
                                MatchOperand(taicpu(hp2).oper[0]^, taicpu(p).oper[0]^) and
                                MatchOperand(taicpu(hp2).oper[1]^, taicpu(p).oper[1]^) do
                            begin
                              DebugMsg('Peephole MovStrMov done', hp2);
                              GetNextInstruction(hp2,hp1);
                              asml.remove(hp2);
                              hp2.free;
                              if not assigned(hp1) then break;
                            end
                        {
                          This removes the first mov from
                          mov rX,...
                          mov rX,...
                        }
                        else if taicpu(hp1).opcode=A_MOV then
                          while MatchInstruction(hp1, A_MOV, [taicpu(p).condition], [taicpu(p).oppostfix]) and
                                (taicpu(hp1).ops = 2) and
                                MatchOperand(taicpu(p).oper[0]^, taicpu(hp1).oper[0]^) and
                                { don't remove the first mov if the second is a mov rX,rX }
                                not(MatchOperand(taicpu(hp1).oper[0]^, taicpu(hp1).oper[1]^)) do
                            begin
                              DebugMsg('Peephole MovMov done', p);
                              asml.remove(p);
                              p.free;
                              p:=hp1;
                              GetNextInstruction(hp1,hp1);
                              if not assigned(hp1) then
                                break;
                            end;
                      end;
                    {
                      change
                      mov r1, r0
                      add r1, r1, #1
                      to
                      add r1, r0, #1

                      Todo: Make it work for mov+cmp too

                      CAUTION! If this one is successful p might not be a mov instruction anymore!
                    }
                    if (taicpu(p).ops = 2) and
                       (taicpu(p).oper[1]^.typ = top_reg) and
                       (taicpu(p).oppostfix = PF_NONE) and
                       GetNextInstruction(p, hp1) and
                       MatchInstruction(hp1, [A_ADD, A_ADC, A_RSB, A_RSC, A_SUB, A_SBC,
                                              A_AND, A_BIC, A_EOR, A_ORR, A_MOV, A_MVN],
                                        [taicpu(p).condition], []) and
                       {MOV and MVN might only have 2 ops}
                       (taicpu(hp1).ops = 3) and
                       MatchOperand(taicpu(p).oper[0]^, taicpu(hp1).oper[0]^.reg) and
                       (taicpu(hp1).oper[1]^.typ = top_reg) and
                       (taicpu(hp1).oper[2]^.typ in [top_reg, top_const, top_shifterop]) then
                      begin
                      { When we get here we still don't know if the registers match}
                        for I:=1 to 2 do
                          {
                            If the first loop was successful p will be replaced with hp1.
                            The checks will still be ok, because all required information
                            will also be in hp1 then.
                          }
                          if MatchOperand(taicpu(p).oper[0]^, taicpu(hp1).oper[I]^.reg) then
                            begin
                              DebugMsg('Peephole RedundantMovProcess done', hp1);
                              taicpu(hp1).oper[I]^.reg := taicpu(p).oper[1]^.reg;
                              if p<>hp1 then
                              begin
                                asml.remove(p);
                                p.free;
                                p:=hp1;
                              end;
                            end;
                      end;
                    { This folds shifterops into following instructions
                      mov r0, r1, lsl #8
                      add r2, r3, r0

                      to

                      add r2, r3, r1, lsl #8
                      CAUTION! If this one is successful p might not be a mov instruction anymore!
                    }
                    if (taicpu(p).opcode = A_MOV) and
                       (taicpu(p).ops = 3) and
                       (taicpu(p).oper[1]^.typ = top_reg) and
                       (taicpu(p).oper[2]^.typ = top_shifterop) and
                       (taicpu(p).oppostfix = PF_NONE) and
                       GetNextInstruction(p, hp1) and
                       MatchInstruction(hp1, [A_ADD, A_ADC, A_RSB, A_RSC, A_SUB, A_SBC,
                                              A_AND, A_BIC, A_EOR, A_ORR, A_TEQ, A_TST,
                                              A_CMP, A_CMN],
                                        [taicpu(p).condition], [PF_None]) and
                       (taicpu(hp1).ops >= 2) and {Currently we can't fold into another shifterop}
                       (taicpu(hp1).oper[taicpu(hp1).ops-1]^.typ = top_reg) and
                       (
                         {Only ONE of the two src operands is allowed to match}
                         MatchOperand(taicpu(p).oper[0]^, taicpu(hp1).oper[taicpu(hp1).ops-2]^) xor
                         MatchOperand(taicpu(p).oper[0]^, taicpu(hp1).oper[taicpu(hp1).ops-1]^)
                       ) then
                      begin
                        CopyUsedRegs(TmpUsedRegs);
                        UpdateUsedRegs(TmpUsedRegs, tai(p.next));
                        if taicpu(hp1).opcode in [A_TST, A_TEQ, A_CMN] then
                          I2:=0
                        else
                          I2:=1;
                        if not(RegUsedAfterInstruction(taicpu(p).oper[0]^.reg,hp1,TmpUsedRegs)) then
                          for I:=I2 to taicpu(hp1).ops-1 do
                            if MatchOperand(taicpu(p).oper[0]^, taicpu(hp1).oper[I]^.reg) then
                              begin
                                { If the parameter matched on the second op from the RIGHT
                                  we have to switch the parameters, this will not happen for CMP
                                  were we're only evaluating the most right parameter
                                }
                                if I <> taicpu(hp1).ops-1 then
                                  begin
                                    {The SUB operators need to be changed when we swap parameters}
                                    case taicpu(hp1).opcode of
                                      A_SUB: tempop:=A_RSB;
                                      A_SBC: tempop:=A_RSC;
                                      A_RSB: tempop:=A_SUB;
                                      A_RSC: tempop:=A_SBC;
                                      else tempop:=taicpu(hp1).opcode;
                                    end;
                                    if taicpu(hp1).ops = 3 then
                                      hp2:=taicpu.op_reg_reg_reg_shifterop(tempop,
                                           taicpu(hp1).oper[0]^.reg, taicpu(hp1).oper[2]^.reg,
                                           taicpu(p).oper[1]^.reg, taicpu(p).oper[2]^.shifterop^)
                                    else
                                      hp2:=taicpu.op_reg_reg_shifterop(tempop,
                                           taicpu(hp1).oper[0]^.reg, taicpu(p).oper[1]^.reg,
                                           taicpu(p).oper[2]^.shifterop^);
                                  end
                                else
                                  if taicpu(hp1).ops = 3 then
                                    hp2:=taicpu.op_reg_reg_reg_shifterop(taicpu(hp1).opcode,
                                         taicpu(hp1).oper[0]^.reg, taicpu(hp1).oper[1]^.reg,
                                         taicpu(p).oper[1]^.reg, taicpu(p).oper[2]^.shifterop^)
                                  else
                                    hp2:=taicpu.op_reg_reg_shifterop(taicpu(hp1).opcode,
                                         taicpu(hp1).oper[0]^.reg, taicpu(p).oper[1]^.reg,
                                         taicpu(p).oper[2]^.shifterop^);
                                asml.insertbefore(hp2, p);
                                asml.remove(p);
                                asml.remove(hp1);
                                p.free;
                                hp1.free;
                                p:=hp2;
                                GetNextInstruction(p,hp1);
                                DebugMsg('Peephole FoldShiftProcess done', p);
                                break;
                              end;
                        ReleaseUsedRegs(TmpUsedRegs);
                      end;

                    {
                      Often we see shifts and then a superfluous mov to another register
                      In the future this might be handled in RedundantMovProcess when it uses RegisterTracking
                    }
                    if (taicpu(p).opcode = A_MOV) and 
                        GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) then
                      RemoveSuperfluousMove(p, hp1, 'MovMov2Mov');
                  end;
                A_ADD,
                A_ADC,
                A_RSB,
                A_RSC,
                A_SUB,
                A_SBC,
                A_AND,
                A_BIC,
                A_EOR,
                A_ORR,
                A_MLA,
                A_MUL:
                  begin
                        {
                          optimize
                          and reg2,reg1,const1
                          ...
                        }
                    if (taicpu(p).opcode = A_AND) and
                       (taicpu(p).oper[1]^.typ = top_reg) and
                       (taicpu(p).oper[2]^.typ = top_const) then
                      begin
                        {
                          change
                          and reg2,reg1,const1
                          and reg3,reg2,const2
                          to
                          and reg3,reg1,(const1 and const2)
                        }
                        if GetNextInstruction(p, hp1) and
                        MatchInstruction(hp1, A_AND, [taicpu(p).condition], [PF_None]) and
                        { either reg3 and reg2 are equal or reg2 is deallocated after the and }
                        (MatchOperand(taicpu(hp1).oper[0]^, taicpu(p).oper[0]^.reg) or
                         assigned(FindRegDealloc(taicpu(p).oper[0]^.reg,tai(hp1.Next)))) and
                        MatchOperand(taicpu(hp1).oper[1]^, taicpu(p).oper[0]^.reg) and
                        (taicpu(hp1).oper[2]^.typ = top_const) then
                          begin
                            DebugMsg('Peephole AndAnd2And done', p);
                            taicpu(p).loadConst(2,taicpu(p).oper[2]^.val and taicpu(hp1).oper[2]^.val);
                            taicpu(p).oppostfix:=taicpu(hp1).oppostfix;
                            taicpu(p).loadReg(0,taicpu(hp1).oper[0]^.reg);
                            asml.remove(hp1);
                            hp1.free;
                          end
                        {
                          change
                          and reg2,reg1,255
                          strb reg2,[...]
                          dealloc reg2
                          to
                          strb reg1,[...]
                        }
                        else if (taicpu(p).oper[2]^.val = 255) and
                          MatchInstruction(p, A_AND, [C_None], [PF_None]) and
                          GetNextInstructionUsingReg(p,hp1,taicpu(p).oper[0]^.reg) and
                          MatchInstruction(hp1, A_STR, [C_None], [PF_B]) and
                          assigned(FindRegDealloc(taicpu(p).oper[0]^.reg,tai(hp1.Next))) and
                          { the reference in strb might not use reg2 }
                          not(RegInRef(taicpu(p).oper[0]^.reg,taicpu(hp1).oper[1]^.ref^)) and
                          { reg1 might not be modified inbetween }
                          not(RegModifiedBetween(taicpu(p).oper[1]^.reg,p,hp1)) then
                          begin
                            DebugMsg('Peephole AndStrb2Strb done', p);
                            taicpu(hp1).loadReg(0,taicpu(p).oper[1]^.reg);
                            asml.remove(p);
                            p.free;
                            p:=hp1;
                          end;
                      end;
                    {
                      change
                      add/sub reg2,reg1,const1
                      str/ldr reg3,[reg2,const2]
                      dealloc reg2
                      to
                      str/ldr reg3,[reg1,const2+/-const1]
                    }
                    if (taicpu(p).opcode in [A_ADD,A_SUB]) and
                       (taicpu(p).oper[1]^.typ = top_reg) and
                       (taicpu(p).oper[2]^.typ = top_const) then
                      begin
                        hp1:=p;
                        while GetNextInstructionUsingReg(hp1, hp1, taicpu(p).oper[0]^.reg) and
                          { we cannot check NR_DEFAULTFLAGS for modification yet so don't allow a condition }
                          MatchInstruction(hp1, [A_LDR, A_STR], [C_None], []) and
                          (taicpu(hp1).oper[1]^.ref^.base=taicpu(p).oper[0]^.reg) and
                          { don't optimize if the register is stored/overwritten }
                          (taicpu(hp1).oper[0]^.reg<>taicpu(p).oper[1]^.reg) and
                          (taicpu(hp1).oper[1]^.ref^.index=NR_NO) and
                          (taicpu(hp1).oper[1]^.ref^.addressmode=AM_OFFSET) and
                          { new offset must be valid: either in the range of 8 or 12 bit, depend on the
                            ldr postfix }
                          (((taicpu(p).opcode=A_ADD) and
                            (((taicpu(hp1).oppostfix in [PF_None,PF_B]) and
                              (abs(taicpu(hp1).oper[1]^.ref^.offset+taicpu(p).oper[2]^.val)<4096)) or
                             (abs(taicpu(hp1).oper[1]^.ref^.offset+taicpu(p).oper[2]^.val)<256)
                            )
                           ) or
                           ((taicpu(p).opcode=A_SUB) and
                             (((taicpu(hp1).oppostfix in [PF_None,PF_B]) and
                               (abs(taicpu(hp1).oper[1]^.ref^.offset-taicpu(p).oper[2]^.val)<4096)) or
                              (abs(taicpu(hp1).oper[1]^.ref^.offset-taicpu(p).oper[2]^.val)<256)
                             )
                           )
                          ) do
                          begin
                            { neither reg1 nor reg2 might be changed inbetween }
                            if RegModifiedBetween(taicpu(p).oper[0]^.reg,p,hp1) or
                              RegModifiedBetween(taicpu(p).oper[1]^.reg,p,hp1) then
                              break;
                            { reg2 must be either overwritten by the ldr or it is deallocated afterwards }
                            if ((taicpu(hp1).opcode=A_LDR) and (taicpu(p).oper[0]^.reg=taicpu(hp1).oper[0]^.reg)) or
                              assigned(FindRegDeAlloc(taicpu(p).oper[0]^.reg,tai(hp1.Next))) then
                              begin
                                { remember last instruction }
                                hp2:=hp1;
                                DebugMsg('Peephole Add/SubLdr2Ldr done', p);
                                hp1:=p;
                                { fix all ldr/str }
                                while GetNextInstructionUsingReg(hp1, hp1, taicpu(p).oper[0]^.reg) do
                                  begin
                                    taicpu(hp1).oper[1]^.ref^.base:=taicpu(p).oper[1]^.reg;
                                    if taicpu(p).opcode=A_ADD then
                                      inc(taicpu(hp1).oper[1]^.ref^.offset,taicpu(p).oper[2]^.val)
                                    else
                                      dec(taicpu(hp1).oper[1]^.ref^.offset,taicpu(p).oper[2]^.val);
                                    if hp1=hp2 then
                                      break;
                                  end;
                                GetNextInstruction(p,hp1);
                                asml.remove(p);
                                p.free;
                                p:=hp1;
                                break;
                              end;
                          end;
                      end;
                    {
                      change
                      add reg1, ...
                      mov reg2, reg1
                      to
                      add reg2, ...
                    }
                    if GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) then
                      RemoveSuperfluousMove(p, hp1, 'DataMov2Data');
                  end;
                A_CMP:
                  begin
                    {
                      change
                      cmp   reg,const1
                      moveq reg,const1
                      movne reg,const2
                      to
                      cmp   reg,const1
                      movne reg,const2
                    }
                    if (taicpu(p).oper[1]^.typ = top_const) and
                       GetNextInstruction(p, hp1) and
                       MatchInstruction(hp1, A_MOV, [C_EQ, C_NE], [PF_NONE]) and
                       (taicpu(hp1).oper[1]^.typ = top_const) and
                       GetNextInstruction(hp1, hp2) and
                       MatchInstruction(hp2, A_MOV, [C_EQ, C_NE], [PF_NONE]) and
                       (taicpu(hp1).oper[1]^.typ = top_const) then
                      begin
                        RemoveRedundantMove(p, hp1, asml);
                        RemoveRedundantMove(p, hp2, asml);
                      end;
                  end;
              end;
          end;
      end;
    end;


  { instructions modifying the CPSR can be only the last instruction }
  function MustBeLast(p : tai) : boolean;
    begin
      Result:=(p.typ=ait_instruction) and
        ((taicpu(p).opcode in [A_BL,A_BLX,A_CMP,A_CMN,A_SWI,A_TEQ,A_TST,A_CMF,A_CMFE {,A_MSR}]) or
         ((taicpu(p).ops>=1) and (taicpu(p).oper[0]^.typ=top_reg) and (taicpu(p).oper[0]^.reg=NR_PC)) or
         (taicpu(p).oppostfix=PF_S));
    end;


  procedure TCpuAsmOptimizer.PeepHoleOptPass2;
    var
      p,hp1,hp2: tai;
      l : longint;
      condition : tasmcond;
      hp3: tai;
      WasLast: boolean;
      { UsedRegs, TmpUsedRegs: TRegSet; }

    begin
      p := BlockStart;
      { UsedRegs := []; }
      while (p <> BlockEnd) Do
        begin
          { UpdateUsedRegs(UsedRegs, tai(p.next)); }
          case p.Typ Of
            Ait_Instruction:
              begin
                case taicpu(p).opcode Of
                  A_B:
                    if taicpu(p).condition<>C_None then
                      begin
                         { check for
                                Bxx   xxx
                                <several instructions>
                             xxx:
                         }
                         l:=0;
                         WasLast:=False;
                         GetNextInstruction(p, hp1);
                         while assigned(hp1) and
                           (l<=4) and
                           CanBeCond(hp1) and
                           { stop on labels }
                           not(hp1.typ=ait_label) do
                           begin
                              inc(l);
                              if MustBeLast(hp1) then
                                begin
                                  WasLast:=True;
                                  GetNextInstruction(hp1,hp1);
                                  break;
                                end
                              else
                                GetNextInstruction(hp1,hp1);
                           end;
                         if assigned(hp1) then
                           begin
                              if FindLabel(tasmlabel(taicpu(p).oper[0]^.ref^.symbol),hp1) then
                                begin
                                  if (l<=4) and (l>0) then
                                    begin
                                      condition:=inverse_cond(taicpu(p).condition);
                                      hp2:=p;
                                      GetNextInstruction(p,hp1);
                                      p:=hp1;
                                      repeat
                                        if hp1.typ=ait_instruction then
                                          taicpu(hp1).condition:=condition;
                                        if MustBeLast(hp1) then
                                          begin
                                            GetNextInstruction(hp1,hp1);
                                            break;
                                          end
                                        else
                                          GetNextInstruction(hp1,hp1);
                                      until not(assigned(hp1)) or
                                        not(CanBeCond(hp1)) or
                                        (hp1.typ=ait_label);
                                      { wait with removing else GetNextInstruction could
                                        ignore the label if it was the only usage in the
                                        jump moved away }
                                      tasmlabel(taicpu(hp2).oper[0]^.ref^.symbol).decrefs;
                                      asml.remove(hp2);
                                      hp2.free;
                                      continue;
                                    end;
                                end
                              else
                                { do not perform further optimizations if there is inctructon
                                  in block #1 which can not be optimized.
                                 }
                                if not WasLast then
                                begin
                                   { check further for
                                          Bcc   xxx
                                          <several instructions 1>
                                          B   yyy
                                  xxx:
                                          <several instructions 2>
                                  yyy:
                                   }
                                  { hp2 points to jmp yyy }
                                  hp2:=hp1;
                                  { skip hp1 to xxx }
                                  GetNextInstruction(hp1, hp1);
                                  if assigned(hp2) and
                                    assigned(hp1) and
                                    (l<=3) and
                                    (hp2.typ=ait_instruction) and
                                    (taicpu(hp2).is_jmp) and
                                    (taicpu(hp2).condition=C_None) and
                                    { real label and jump, no further references to the
                                      label are allowed }
                                    (tasmlabel(taicpu(p).oper[0]^.ref^.symbol).getrefs=2) and
                                    FindLabel(tasmlabel(taicpu(p).oper[0]^.ref^.symbol),hp1) then
                                     begin
                                       l:=0;
                                       { skip hp1 to <several moves 2> }
                                       GetNextInstruction(hp1, hp1);
                                       while assigned(hp1) and
                                         CanBeCond(hp1) do
                                         begin
                                           inc(l);
                                           GetNextInstruction(hp1, hp1);
                                         end;
                                       { hp1 points to yyy: }
                                       if assigned(hp1) and
                                         FindLabel(tasmlabel(taicpu(hp2).oper[0]^.ref^.symbol),hp1) then
                                         begin
                                            condition:=inverse_cond(taicpu(p).condition);
                                            GetNextInstruction(p,hp1);
                                            hp3:=p;
                                            p:=hp1;
                                            repeat
                                              if hp1.typ=ait_instruction then
                                                taicpu(hp1).condition:=condition;
                                              GetNextInstruction(hp1,hp1);
                                            until not(assigned(hp1)) or
                                              not(CanBeCond(hp1));
                                            { hp2 is still at jmp yyy }
                                            GetNextInstruction(hp2,hp1);
                                            { hp2 is now at xxx: }
                                            condition:=inverse_cond(condition);
                                            GetNextInstruction(hp1,hp1);
                                            { hp1 is now at <several movs 2> }
                                            repeat
                                              taicpu(hp1).condition:=condition;
                                              GetNextInstruction(hp1,hp1);
                                            until not(assigned(hp1)) or
                                              not(CanBeCond(hp1)) or
                                              (hp1.typ=ait_label);
                                            {
                                            asml.remove(hp1.next)
                                            hp1.next.free;
                                            asml.remove(hp1);
                                            hp1.free;
                                            }
                                            { remove Bcc }
                                            tasmlabel(taicpu(hp3).oper[0]^.ref^.symbol).decrefs;
                                            asml.remove(hp3);
                                            hp3.free;
                                            { remove jmp }
                                            tasmlabel(taicpu(hp2).oper[0]^.ref^.symbol).decrefs;
                                            asml.remove(hp2);
                                            hp2.free;
                                            continue;
                                         end;
                                     end;
                                end;
                           end;
                      end;
                end;
              end;
          end;
          p := tai(p.next)
        end;
    end;

  function TCpuAsmOptimizer.RegInInstruction(Reg: TRegister; p1: tai): Boolean;
    begin
      If (p1.typ = ait_instruction) and (taicpu(p1).opcode=A_BL) then
        Result:=true
      else
        Result:=inherited RegInInstruction(Reg, p1);
    end;

  const
    { set of opcode which might or do write to memory }
    { TODO : extend armins.dat to contain r/w info }
    opcode_could_mem_write = [A_B,A_BL,A_BLX,A_BKPT,A_BX,A_STR,A_STRB,A_STRBT,
                              A_STRH,A_STRT,A_STF,A_SFM,A_STM,A_FSTS,A_FSTD];


  { adjust the register live information when swapping the two instructions p and hp1,
    they must follow one after the other }
  procedure TCpuPreRegallocScheduler.SwapRegLive(p,hp1 : taicpu);

    procedure CheckLiveEnd(reg : tregister);
      var
        supreg : TSuperRegister;
        regtype : TRegisterType;
      begin
        if reg=NR_NO then
          exit;
        regtype:=getregtype(reg);
        supreg:=getsupreg(reg);
        if (cg.rg[regtype].live_end[supreg]=hp1) and
          RegInInstruction(reg,p) then
          cg.rg[regtype].live_end[supreg]:=p;
      end;


    procedure CheckLiveStart(reg : TRegister);
      var
        supreg : TSuperRegister;
        regtype : TRegisterType;
      begin
        if reg=NR_NO then
          exit;
        regtype:=getregtype(reg);
        supreg:=getsupreg(reg);
        if (cg.rg[regtype].live_start[supreg]=p) and
          RegInInstruction(reg,hp1) then
         cg.rg[regtype].live_start[supreg]:=hp1;
      end;

    var
      i : longint;
      r : TSuperRegister;
    begin
      { assumption: p is directly followed by hp1 }

      { if live of any reg used by p starts at p and hp1 uses this register then
        set live start to hp1 }
      for i:=0 to p.ops-1 do
        case p.oper[i]^.typ of
          Top_Reg:
            CheckLiveStart(p.oper[i]^.reg);
          Top_Ref:
            begin
              CheckLiveStart(p.oper[i]^.ref^.base);
              CheckLiveStart(p.oper[i]^.ref^.index);
            end;
          Top_Shifterop:
            CheckLiveStart(p.oper[i]^.shifterop^.rs);
          Top_RegSet:
            for r:=RS_R0 to RS_R15 do
               if r in p.oper[i]^.regset^ then
                 CheckLiveStart(newreg(R_INTREGISTER,r,R_SUBWHOLE));
        end;

      { if live of any reg used by hp1 ends at hp1 and p uses this register then
        set live end to p }
      for i:=0 to hp1.ops-1 do
        case hp1.oper[i]^.typ of
          Top_Reg:
            CheckLiveEnd(hp1.oper[i]^.reg);
          Top_Ref:
            begin
              CheckLiveEnd(hp1.oper[i]^.ref^.base);
              CheckLiveEnd(hp1.oper[i]^.ref^.index);
            end;
          Top_Shifterop:
            CheckLiveStart(hp1.oper[i]^.shifterop^.rs);
          Top_RegSet:
            for r:=RS_R0 to RS_R15 do
               if r in hp1.oper[i]^.regset^ then
                 CheckLiveEnd(newreg(R_INTREGISTER,r,R_SUBWHOLE));
        end;
    end;


  function TCpuPreRegallocScheduler.SchedulerPass1Cpu(var p: tai): boolean;

  { TODO : schedule also forward }
  { TODO : schedule distance > 1 }
    var
      hp1,hp2,hp3,hp4,hp5 : tai;
      list : TAsmList;
    begin
      result:=true;

      list:=TAsmList.Create;
      p:=BlockStart;
      while p<>BlockEnd Do
        begin
          if (p.typ=ait_instruction) and
            GetNextInstruction(p,hp1) and
            (hp1.typ=ait_instruction) and
            (taicpu(hp1).opcode in [A_LDR,A_LDRB,A_LDRH,A_LDRSB,A_LDRSH]) and
            { for now we don't reschedule if the previous instruction changes potentially a memory location }
            ( (not(taicpu(p).opcode in opcode_could_mem_write) and
               not(RegModifiedByInstruction(NR_PC,p))
              ) or
              ((taicpu(p).opcode in [A_STM,A_STRB,A_STRH,A_STR]) and
               ((taicpu(hp1).oper[1]^.ref^.base=NR_PC) or
                (assigned(taicpu(hp1).oper[1]^.ref^.symboldata) and
                (taicpu(hp1).oper[1]^.ref^.offset=0)
                )
               ) or
               { try to prove that the memory accesses don't overlapp }
               ((taicpu(p).opcode in [A_STRB,A_STRH,A_STR]) and
                (taicpu(p).oper[1]^.ref^.base=taicpu(hp1).oper[1]^.ref^.base) and
                (taicpu(p).oppostfix=PF_None) and
                (taicpu(hp1).oppostfix=PF_None) and
                (taicpu(p).oper[1]^.ref^.index=NR_NO) and
                (taicpu(hp1).oper[1]^.ref^.index=NR_NO) and
                { get operand sizes and check if the offset distance is large enough to ensure no overlapp }
                (abs(taicpu(p).oper[1]^.ref^.offset-taicpu(hp1).oper[1]^.ref^.offset)>=max(tcgsize2size[reg_cgsize(taicpu(p).oper[0]^.reg)],tcgsize2size[reg_cgsize(taicpu(hp1).oper[0]^.reg)]))
              )
            )
            ) and
            GetNextInstruction(hp1,hp2) and
            (hp2.typ=ait_instruction) and
            { loaded register used by next instruction? }
            (RegInInstruction(taicpu(hp1).oper[0]^.reg,hp2)) and
            { loaded register not used by previous instruction? }
            not(RegInInstruction(taicpu(hp1).oper[0]^.reg,p)) and
            { same condition? }
            (taicpu(p).condition=taicpu(hp1).condition) and
            { first instruction might not change the register used as base }
            ((taicpu(hp1).oper[1]^.ref^.base=NR_NO) or
             not(RegModifiedByInstruction(taicpu(hp1).oper[1]^.ref^.base,p))
            ) and
            { first instruction might not change the register used as index }
            ((taicpu(hp1).oper[1]^.ref^.index=NR_NO) or
             not(RegModifiedByInstruction(taicpu(hp1).oper[1]^.ref^.index,p))
            ) then
            begin
              hp3:=tai(p.Previous);
              hp5:=tai(p.next);
              asml.Remove(p);
              { if there is a reg. dealloc instruction associated with p, move it together with p }

              { before the instruction? }
              while assigned(hp3) and (hp3.typ<>ait_instruction) do
                begin
                  if (hp3.typ=ait_regalloc) and (tai_regalloc(hp3).ratype in [ra_dealloc]) and
                    RegInInstruction(tai_regalloc(hp3).reg,p) then
                    begin
                      hp4:=hp3;
                      hp3:=tai(hp3.Previous);
                      asml.Remove(hp4);
                      list.Concat(hp4);
                    end
                  else
                    hp3:=tai(hp3.Previous);
                end;

              list.Concat(p);
              SwapRegLive(taicpu(p),taicpu(hp1));

              { after the instruction? }
              while assigned(hp5) and (hp5.typ<>ait_instruction) do
                begin
                  if (hp5.typ=ait_regalloc) and (tai_regalloc(hp5).ratype in [ra_dealloc]) and
                    RegInInstruction(tai_regalloc(hp5).reg,p) then
                    begin
                      hp4:=hp5;
                      hp5:=tai(hp5.next);
                      asml.Remove(hp4);
                      list.Concat(hp4);
                    end
                  else
                    hp5:=tai(hp5.Next);
                end;

              asml.Remove(hp1);
{$ifdef DEBUG_PREREGSCHEDULER}
              asml.insertbefore(tai_comment.Create(strpnew('Rescheduled')),hp2);
{$endif DEBUG_PREREGSCHEDULER}
              asml.InsertBefore(hp1,hp2);
              asml.InsertListBefore(hp2,list);
              p:=tai(p.next)
            end
          else if p.typ=ait_instruction then
            p:=hp1
          else
            p:=tai(p.next);
        end;
      list.Free;
    end;


  procedure TCpuThumb2AsmOptimizer.PeepHoleOptPass2;
    begin
      { TODO: Add optimizer code }
    end;

begin
  casmoptimizer:=TCpuAsmOptimizer;
  cpreregallocscheduler:=TCpuPreRegallocScheduler;
End.
