{
    Copyright (c) 1998-2002 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit implements the common RiscV optimizer object

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

unit aoptcpurv;

interface

{$I fpcdefs.inc}

{ $define DEBUG_AOPTCPU}

uses
  cpubase,
  globals, globtype,
  cgbase,
  aoptobj, aoptcpub, aopt,
  aasmtai, aasmcpu;

type

  TRVCpuAsmOptimizer = class(TAsmOptimizer)
    function InstructionLoadsFromReg(const reg: TRegister; const hp: tai): boolean; override;
    function RegLoadedWithNewValue(reg: tregister; hp: tai): boolean; override;
    function RegModifiedByInstruction(Reg: TRegister; p1: tai): boolean; override;
    Function GetNextInstructionUsingReg(Current: tai; Out Next: tai; reg: TRegister): Boolean;
    { outputs a debug message into the assembler file }
    procedure DebugMsg(const s: string; p: tai);

    function PeepHoleOptPass1Cpu(var p: tai): boolean; override;
    function OptPass1OP(var p: tai): boolean;
    function OptPass1FOP(var p: tai;mvop: tasmop): boolean;
    function OptPass1FSGNJ(var p: tai;mvop: tasmop): boolean;

    function OptPass1Add(var p: tai): boolean;
    procedure RemoveInstr(var orig: tai; moveback: boolean=true);
  end;

implementation

  uses
    cutils,
    verbose;

  function MatchInstruction(const instr: tai; const op: TCommonAsmOps; const AConditions: TAsmConds = []): boolean;
    begin
      result :=
        (instr.typ = ait_instruction) and
        (taicpu(instr).opcode in op) and
        ((AConditions=[]) or (taicpu(instr).condition in AConditions));
    end;


  function MatchInstruction(const instr: tai; const op: TAsmOp; const AConditions: TAsmConds = []): boolean;
    begin
      result :=
        (instr.typ = ait_instruction) and
        (taicpu(instr).opcode = op) and
        ((AConditions=[]) or (taicpu(instr).condition in AConditions));
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
          {top_ref:
            Result:=RefsEqual(oper1.ref^, oper2.ref^);}
          else Result:=false;
        end
    end;


  function MatchOperand(const oper: TOper; const reg: TRegister): boolean; inline;
    begin
      result := (oper.typ = top_reg) and (oper.reg = reg);
    end;


{$ifdef DEBUG_AOPTCPU}
  procedure TRVCpuAsmOptimizer.DebugMsg(const s: string;p : tai);
    begin
      asml.insertbefore(tai_comment.Create(strpnew(s)), p);
    end;
{$else DEBUG_AOPTCPU}
  procedure TRVCpuAsmOptimizer.DebugMsg(const s: string;p : tai);inline;
    begin
    end;
{$endif DEBUG_AOPTCPU}


  function TRVCpuAsmOptimizer.InstructionLoadsFromReg(const reg: TRegister; const hp: tai): boolean;
    var
      p: taicpu;
      i: longint;
    begin
      result:=false;
      if not (assigned(hp) and (hp.typ=ait_instruction)) then
        exit;
      p:=taicpu(hp);

      i:=0;
      while(i<p.ops) do
        begin
          case p.oper[I]^.typ of
            top_reg:
              result:=(p.oper[I]^.reg=reg) and (p.spilling_get_operation_type(i)<>operand_write);
            top_ref:
              result:=
                (p.oper[I]^.ref^.base=reg);
            else
              ;
          end;
          if result then exit; {Bailout if we found something}
          Inc(I);
        end;
    end;


  function TRVCpuAsmOptimizer.RegLoadedWithNewValue(reg: tregister; hp: tai): boolean;
    begin
      result:=
        (hp.typ=ait_instruction) and
        (taicpu(hp).ops>1) and
        (taicpu(hp).oper[0]^.typ=top_reg) and
        (taicpu(hp).oper[0]^.reg=reg) and
        (taicpu(hp).spilling_get_operation_type(0)<>operand_read);
    end;


  function TRVCpuAsmOptimizer.RegModifiedByInstruction(Reg: TRegister; p1: tai): boolean;
    var
      i : Longint;
    begin
      result:=false;
      for i:=0 to taicpu(p1).ops-1 do
        case taicpu(p1).oper[i]^.typ of
          top_reg:
            if (taicpu(p1).oper[i]^.reg=Reg) and (taicpu(p1).spilling_get_operation_type(i) in [operand_write,operand_readwrite]) then
              exit(true);
          else
            ;
        end;
    end;


  function TRVCpuAsmOptimizer.GetNextInstructionUsingReg(Current: tai; out Next: tai; reg: TRegister): Boolean;
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


  function TRVCpuAsmOptimizer.OptPass1OP(var p : tai) : boolean;
    var
      hp1 : tai;
    begin
      result:=false;
      { replace
          <Op>   %reg3,%reg2,%reg1
          addi   %reg4,%reg3,0
          dealloc  %reg3

          by
          <Op>   %reg4,%reg2,%reg1
        ?
      }
      if GetNextInstruction(p,hp1) and
        MatchInstruction(hp1,A_ADDI) and
        (taicpu(hp1).oper[2]^.val=0) and
        MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[1]^) then
        begin
          TransferUsedRegs(TmpUsedRegs);
          UpdateUsedRegs(TmpUsedRegs, tai(p.next));
          if not(RegUsedAfterInstruction(taicpu(hp1).oper[1]^.reg,hp1,TmpUsedRegs)) then
            begin
              taicpu(p).loadoper(0,taicpu(hp1).oper[0]^);
              DebugMsg('Peephole OpAddi02Op done',p);
              RemoveInstruction(hp1);
              result:=true;
            end;
        end;
    end;


  function TRVCpuAsmOptimizer.OptPass1FOP(var p: tai;mvop: tasmop) : boolean;
    var
      hp1 : tai;
    begin
      result:=false;
      { replace
          <FOp>   %reg3,%reg2,%reg1
          <mvop>  %reg4,%reg3,%reg3
          dealloc %reg3

        by

          <FOp>   %reg4,%reg2,%reg1
        ?
      }
      if GetNextInstruction(p,hp1) and
        MatchInstruction(hp1,mvop) and
        MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[1]^) and
        MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[2]^) then
        begin
          TransferUsedRegs(TmpUsedRegs);
          UpdateUsedRegs(TmpUsedRegs, tai(p.next));
          if not(RegUsedAfterInstruction(taicpu(hp1).oper[1]^.reg,hp1,TmpUsedRegs)) then
            begin
              taicpu(p).loadoper(0,taicpu(hp1).oper[0]^);
              DebugMsg('Peephole FOpFsgnj02FOp done',p);
              RemoveInstruction(hp1);
              result:=true;
            end;
        end;
    end;


  function TRVCpuAsmOptimizer.OptPass1FSGNJ(var p: tai; mvop: tasmop): boolean;
    var
      hp1 : tai;
    begin
      result:=false;
      { replace
          <mvop>  %reg1,%reg2,%reg2
          <FOp>   %reg3,%reg1,%reg1
          dealloc %reg2

        by

          <FOp>   %reg3,%reg2,%reg2
        ?
      }
      if GetNextInstruction(p,hp1) and (hp1.typ=ait_instruction) and
        (((mvop=A_FSGNJ_S) and (taicpu(hp1).opcode in [A_FADD_S,A_FSUB_S,A_FMUL_S,A_FDIV_S,A_FSQRT_S,
              A_FNEG_S,A_FMADD_S,A_FMSUB_S,A_FNMSUB_S,A_FNMADD_S,A_FMIN_S,A_FMAX_S,A_FCVT_D_S,
              A_FEQ_S])) or
         ((mvop=A_FSGNJ_D) and (taicpu(hp1).opcode in [A_FADD_D,A_FSUB_D,A_FMUL_D,A_FDIV_D,A_FSQRT_D,
              A_FNEG_D,A_FMADD_D,A_FMSUB_D,A_FNMSUB_D,A_FNMADD_D,A_FMIN_D,A_FMAX_D,A_FCVT_S_D,
              A_FEQ_D]))) and
        (MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[1]^) or
        ((taicpu(hp1).ops>=3) and MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[2]^)) or
        ((taicpu(hp1).ops>=4) and MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[3]^))) and
        RegEndOfLife(taicpu(p).oper[0]^.reg, taicpu(hp1)) then
        begin
          if MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[1]^) then
            taicpu(hp1).loadreg(1,taicpu(p).oper[1]^.reg);
          if (taicpu(hp1).ops>=3) and MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[2]^) then
            taicpu(hp1).loadreg(2,taicpu(p).oper[1]^.reg);
          if (taicpu(hp1).ops>=4) and MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[3]^) then
            taicpu(hp1).loadreg(3,taicpu(p).oper[1]^.reg);

          AllocRegBetween(taicpu(p).oper[1]^.reg,p,hp1,UsedRegs);

          DebugMsg('Peephole FMVFOp2FOp performed', hp1);

          RemoveInstr(p);

          result:=true;
        end
    end;


  procedure TRVCpuAsmOptimizer.RemoveInstr(var orig: tai; moveback: boolean = true);
    var
      n: tai;
    begin
      if moveback and (not GetLastInstruction(orig,n)) then
        GetNextInstruction(orig,n);

      AsmL.Remove(orig);
      orig.Free;

      orig:=n;
    end;


  function TRVCpuAsmOptimizer.OptPass1Add(var p: tai): boolean;
    var
      hp1: tai;
    begin
      result:=false;
      {
        Get rid of
          addi x, x, 0
      }
      if (taicpu(p).ops=3) and
        (taicpu(p).oper[2]^.typ=top_const) and
        (taicpu(p).oper[2]^.val=0) and
        MatchOperand(taicpu(p).oper[0]^,taicpu(p).oper[1]^) then
        begin
          DebugMsg('Peephole Addi2Nop performed', p);

          RemoveInstr(p);

          result:=true;
        end
      {
        Changes
          addi x, y, #
          addi/addiw z, x, #
          dealloc x
        To
          addi z, y, #+#
      }
      else if (taicpu(p).ops=3) and
         (taicpu(p).oper[2]^.typ=top_const) and
         GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) and
         MatchInstruction(hp1,[A_ADDI{$ifdef riscv64},A_ADDIW{$endif}]) and
         (taicpu(hp1).ops=3) and
         MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[1]^) and
         (taicpu(hp1).oper[2]^.typ=top_const) and
         is_imm12(taicpu(p).oper[2]^.val+taicpu(hp1).oper[2]^.val) and
         (not RegModifiedBetween(taicpu(p).oper[1]^.reg, p,hp1)) and
         RegEndOfLife(taicpu(p).oper[0]^.reg, taicpu(hp1)) then
        begin
          taicpu(hp1).loadreg(1,taicpu(p).oper[1]^.reg);
          taicpu(hp1).loadconst(2, taicpu(p).oper[2]^.val+taicpu(hp1).oper[2]^.val);

          DebugMsg('Peephole AddiAddi2Addi performed', hp1);

          RemoveInstr(p);

          result:=true;
        end
      {
        Changes
          addi x, z, (ref)
          ld/sd y, 0(x)
          dealloc x
        To
          ld/sd y, 0(ref)(x)
      }
      else if (taicpu(p).ops=3) and
         (taicpu(p).oper[2]^.typ=top_ref) and
         MatchOperand(taicpu(p).oper[0]^,taicpu(p).oper[1]^) and
         GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) and
         MatchInstruction(hp1, [A_LB,A_LBU,A_LH,A_LHU,A_LW,
                                 A_SB,A_SH,A_SW{$ifdef riscv64},A_LD,A_LWU,A_SD{$endif}]) and
         (taicpu(hp1).ops=2) and
         (taicpu(hp1).oper[1]^.typ=top_ref) and
         (taicpu(hp1).oper[1]^.ref^.base=taicpu(p).oper[0]^.reg) and
         (taicpu(hp1).oper[1]^.ref^.offset=0) and
         (not RegModifiedBetween(taicpu(p).oper[1]^.reg, p,hp1)) and
         RegEndOfLife(taicpu(p).oper[0]^.reg, taicpu(hp1)) then
        begin
          taicpu(hp1).loadref(1,taicpu(p).oper[2]^.ref^);
          taicpu(hp1).oper[1]^.ref^.base:=taicpu(p).oper[1]^.reg;

          DebugMsg('Peephole AddiMem2Mem performed', hp1);

          RemoveInstr(p);

          result:=true;
        end
      {
        Changes
          addi x, z, #w
          ld/sd y, 0(x)
          dealloc x
        To
          ld/sd y, #w(z)
      }
      else if (taicpu(p).ops=3) and
         (taicpu(p).oper[2]^.typ=top_const) and
         //MatchOperand(taicpu(p).oper[0]^,taicpu(p).oper[1]^) and
         GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) and
         MatchInstruction(hp1, [A_LB,A_LBU,A_LH,A_LHU,A_LW,
                                 A_SB,A_SH,A_SW{$ifdef riscv64},A_LWU,A_LD,A_SD{$endif}]) and
         (taicpu(hp1).ops=2) and
         (taicpu(hp1).oper[1]^.typ=top_ref) and
         (taicpu(hp1).oper[1]^.ref^.base=taicpu(p).oper[0]^.reg) and
         (taicpu(hp1).oper[1]^.ref^.offset=0) and
         (not RegModifiedBetween(taicpu(p).oper[1]^.reg, p,hp1)) and
         RegEndOfLife(taicpu(p).oper[0]^.reg, taicpu(hp1)) then
        begin
          //taicpu(hp1).loadconst(1,taicpu(p).oper[2]^.ref^);
          taicpu(hp1).oper[1]^.ref^.offset:=taicpu(p).oper[2]^.val;
          taicpu(hp1).oper[1]^.ref^.base:=taicpu(p).oper[1]^.reg;

          DebugMsg('Peephole AddiMem2Mem performed', hp1);

          RemoveInstr(p);

          result:=true;
        end
      {
        Changes
          addi w, z, 0
          op x, y, w
          dealloc w
        To
          op x, y, z
      }
      else if (taicpu(p).ops=3) and
         (taicpu(p).oper[2]^.typ=top_const) and
         (taicpu(p).oper[2]^.val=0) and
         GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) and
         ((MatchInstruction(hp1, [A_SUB,A_ADD,A_SLL,A_SRL,A_SLT,A_AND,A_OR,
            A_ADDI,A_ANDI,A_ORI,A_SRAI,A_SRLI,A_SLLI,A_XORI,A_MUL,
            A_DIV,A_DIVU,A_REM,A_REMU
            {$ifdef riscv64},A_ADDIW,A_SLLIW,A_SRLIW,A_SRAIW,
            A_ADDW,A_SLLW,A_SRLW,A_SUBW,A_SRAW,
            A_DIVUW,A_DIVW,A_REMW,A_REMUW{$endif}]
            ) and
          (taicpu(hp1).ops=3) and
          (MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[2]^) or MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[1]^))) {or

          This is not possible yet as the deallocation after the jump could also mean that the register is in use at the
          jump target.

          (MatchInstruction(hp1, [A_Bxx]) and
          (taicpu(hp1).ops=3) and
          (MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[0]^) or MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[1]^))) }
         ) and
         (not RegModifiedBetween(taicpu(p).oper[1]^.reg, p,hp1)) and
         RegEndOfLife(taicpu(p).oper[0]^.reg, taicpu(hp1)) then
        begin
          { if MatchInstruction(hp1, [A_Bxx]) and MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[0]^) then
            taicpu(hp1).loadreg(0,taicpu(p).oper[1]^.reg); }
          if MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[1]^) then
            taicpu(hp1).loadreg(1,taicpu(p).oper[1]^.reg);
          if MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[2]^) then
            taicpu(hp1).loadreg(2,taicpu(p).oper[1]^.reg);

          AllocRegBetween(taicpu(p).oper[1]^.reg,p,hp1,UsedRegs);

          DebugMsg('Peephole Addi0Op2Op performed', hp1);

          RemoveInstr(p);

          result:=true;
        end
      else
        result:=OptPass1OP(p);
    end;


  function TRVCpuAsmOptimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;
    var
      hp1: tai;
    begin
      result:=false;
      case p.typ of
        ait_instruction:
          begin
            case taicpu(p).opcode of
              A_ADDI:
                result:=OptPass1Add(p);
              A_SUB:
                begin
                  {
                    Turn
                      sub x,y,z
                      bgeu X0,x,...
                      dealloc x
                    Into
                      bne y,x,...
                  }
                  if (taicpu(p).ops=3) and
                     GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) and
                     MatchInstruction(hp1,A_Bxx,[C_GEU,C_EQ]) and
                     (taicpu(hp1).ops=3) and
                     MatchOperand(taicpu(hp1).oper[0]^,NR_X0) and
                     MatchOperand(taicpu(hp1).oper[1]^,taicpu(p).oper[0]^) and
                     (not RegModifiedBetween(taicpu(p).oper[1]^.reg, p,hp1)) and
                     (not RegModifiedBetween(taicpu(p).oper[2]^.reg, p,hp1)) and
                     RegEndOfLife(taicpu(p).oper[0]^.reg, taicpu(hp1)) then
                    begin
                      taicpu(hp1).loadreg(0,taicpu(p).oper[1]^.reg);
                      taicpu(hp1).loadreg(1,taicpu(p).oper[2]^.reg);
                      taicpu(hp1).condition:=C_EQ;

                      DebugMsg('Peephole SubBxx2Beq performed', hp1);

                      RemoveInstr(p);

                      result:=true;
                    end
                  else
                    result:=OptPass1OP(p);
                end;
              A_ANDI:
                begin
                  {
                    Changes
                      andi x, y, #
                      andi z, x, #
                      dealloc x
                    To
                      andi z, y, # and #
                  }
                  if (taicpu(p).ops=3) and
                     (taicpu(p).oper[2]^.typ=top_const) and
                     GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) then
                    begin
                      if MatchInstruction(hp1,A_ANDI) and
                        (taicpu(hp1).ops=3) and
                        MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[1]^) and
                        (taicpu(hp1).oper[2]^.typ=top_const) and
                        is_imm12(taicpu(p).oper[2]^.val and taicpu(hp1).oper[2]^.val) and
                        (not RegModifiedBetween(taicpu(p).oper[1]^.reg, p,hp1)) and
                        RegEndOfLife(taicpu(p).oper[0]^.reg, taicpu(hp1)) then
                        begin
                          taicpu(hp1).loadreg(1,taicpu(p).oper[1]^.reg);
                          taicpu(hp1).loadconst(2, taicpu(p).oper[2]^.val and taicpu(hp1).oper[2]^.val);

                          DebugMsg('Peephole AndiAndi2Andi performed', hp1);

                          RemoveInstr(p);

                          result:=true;
                        end
{$ifndef RISCV32}
                      else if MatchInstruction(hp1,A_ADDIW) and
                        (taicpu(hp1).ops=3) and
                        MatchOperand(taicpu(p).oper[0]^,taicpu(hp1).oper[1]^) and
                        (taicpu(hp1).oper[2]^.typ=top_const) and
                        (taicpu(hp1).oper[2]^.val=0) and
                         is_imm12(taicpu(p).oper[2]^.val) and
                        (not RegModifiedBetween(taicpu(p).oper[1]^.reg, p,hp1)) and
                        RegEndOfLife(taicpu(p).oper[0]^.reg, taicpu(hp1)) then
                        begin
                          taicpu(p).loadreg(0,taicpu(hp1).oper[0]^.reg);

                          DebugMsg('Peephole AndiAddwi02Andi performed', hp1);

                          RemoveInstr(hp1);

                          result:=true;
                         end
{$endif RISCV32}
                      else
                        result:=OptPass1OP(p);
                    end
                  else
                    result:=OptPass1OP(p);
                end;
              A_SLT,
              A_SLTU:
                begin
                  {
                    Turn
                      sltu x,X0,y
                      beq/bne x, X0, ...
                      dealloc x
                    Into
                      bltu/geu X0, y, ...
                  }
                  if (taicpu(p).ops=3) and
                     MatchOperand(taicpu(p).oper[1]^,NR_X0) and
                     GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) and
                     MatchInstruction(hp1,A_Bxx,[C_NE,C_EQ]) and
                     (taicpu(hp1).ops=3) and
                     MatchOperand(taicpu(hp1).oper[0]^,taicpu(p).oper[0]^) and
                     MatchOperand(taicpu(hp1).oper[1]^,NR_X0) and
                     (not RegModifiedBetween(taicpu(p).oper[2]^.reg, p,hp1)) and
                     RegEndOfLife(taicpu(p).oper[0]^.reg, taicpu(hp1)) then
                    begin
                      taicpu(hp1).loadreg(0,NR_X0);
                      taicpu(hp1).loadreg(1,taicpu(p).oper[2]^.reg);

                      if taicpu(p).opcode=A_SLTU then
                        begin
                          if taicpu(hp1).condition=C_NE then
                            taicpu(hp1).condition:=C_LTU
                          else
                            taicpu(hp1).condition:=C_GEU;
                        end
                      else
                        begin    
                          if taicpu(hp1).condition=C_NE then
                            taicpu(hp1).condition:=C_LT
                          else
                            taicpu(hp1).condition:=C_GE;
                        end;

                      DebugMsg('Peephole SltuB2B performed', hp1);

                      RemoveInstr(p);

                      result:=true;
                    end
                  {
                    Turn
                      sltu x,y,z
                      beq/bne x, X0, ...
                      dealloc x
                    Into
                      bltu/geu y, z, ...
                  }
                  else if (taicpu(p).ops=3) and
                     GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) and
                     MatchInstruction(hp1,A_Bxx,[C_NE,C_EQ]) and
                     (taicpu(hp1).ops=3) and
                     MatchOperand(taicpu(hp1).oper[0]^,taicpu(p).oper[0]^) and
                     MatchOperand(taicpu(hp1).oper[1]^,NR_X0) and
                     (not RegModifiedBetween(taicpu(p).oper[1]^.reg, p,hp1)) and
                     (not RegModifiedBetween(taicpu(p).oper[2]^.reg, p,hp1)) and
                     RegEndOfLife(taicpu(p).oper[0]^.reg, taicpu(hp1)) then
                    begin
                      taicpu(hp1).loadreg(0,taicpu(p).oper[1]^.reg);
                      taicpu(hp1).loadreg(1,taicpu(p).oper[2]^.reg);

                      if taicpu(p).opcode=A_SLTU then
                        begin
                          if taicpu(hp1).condition=C_NE then
                            taicpu(hp1).condition:=C_LTU
                          else
                            taicpu(hp1).condition:=C_GEU;
                        end
                      else
                        begin
                          if taicpu(hp1).condition=C_NE then
                            taicpu(hp1).condition:=C_LT
                          else
                            taicpu(hp1).condition:=C_GE;
                        end;

                      DebugMsg('Peephole SltuB2B performed', hp1);

                      RemoveInstr(p);

                      result:=true;
                    end
                  else
                    result:=OptPass1OP(p);
                end;
              A_SLTIU:
                begin
                  {
                    Turn
                      sltiu x,y,1
                      beq/ne x,x0,...
                      dealloc x
                    Into
                      bne y,x0,...
                  }
                  if (taicpu(p).ops=3) and
                     (taicpu(p).oper[2]^.typ=top_const) and
                     (taicpu(p).oper[2]^.val=1) and
                     GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) and
                     MatchInstruction(hp1,A_Bxx,[C_NE,C_EQ]) and
                     (taicpu(hp1).ops=3) and
                     MatchOperand(taicpu(hp1).oper[0]^,taicpu(p).oper[0]^) and
                     MatchOperand(taicpu(hp1).oper[1]^,NR_X0) and
                     (not RegModifiedBetween(taicpu(p).oper[1]^.reg, p,hp1)) and
                     RegEndOfLife(taicpu(p).oper[0]^.reg, taicpu(hp1)) then
                    begin
                      taicpu(hp1).loadreg(0,taicpu(p).oper[1]^.reg);
                      taicpu(hp1).condition:=inverse_cond(taicpu(hp1).condition);

                      DebugMsg('Peephole Sltiu0B2B performed', hp1);

                      RemoveInstr(p);

                      result:=true;
                    end;
                end;
              A_LA,
              A_LUI,
              A_LB,
              A_LBU,
              A_LH,
              A_LHU,
              A_LW,
{$ifdef riscv64}
              A_LWU,
              A_LD,
{$endif riscv64}
              A_ADD,
{$ifdef riscv64}
              A_ADDIW,
              A_SUBW,
{$endif riscv64}
              A_DIV,
              A_DIVU,
{$ifdef riscv64}
              A_DIVW,
              A_DIVUW,
{$endif riscv64}
              A_REM,
              A_REMU,
{$ifdef riscv64}
              A_REMW,
              A_REMUW,
              A_MULW,
{$endif riscv64}
              A_MUL,
              A_MULH,
              A_MULHSU,
              A_MULHU,
              A_ORI,
              A_XORI,
              A_AND,
              A_OR,
              A_XOR,
{$ifdef riscv64}
              A_SLLW,
              A_SRLW,
              A_SRAW,
{$endif riscv64}
              A_SLL,
              A_SRL,
              A_SRA,
              A_NEG,
              A_NOT:
                result:=OptPass1OP(p);
{$ifdef riscv64}
              A_SRAIW,
              A_SRLIW,
              A_SLLIW,
{$endif riscv64}
              A_SRAI,
              A_SRLI,
              A_SLLI:
                begin
                  if (taicpu(p).oper[2]^.val=0) and
                    MatchOperand(taicpu(p).oper[0]^,taicpu(p).oper[1]^) then
                    begin
                      DebugMsg('Peephole S*LI x,x,0 to nop performed', p);
                      RemoveInstr(p);
                      result:=true;
                    end
                  else if (taicpu(p).oper[2]^.val=0) then
                    begin
                      { this enables further optimizations }
                      DebugMsg('Peephole S*LI x,y,0 to addi performed', p);
                      taicpu(p).opcode:=A_ADDI;
                      result:=true;
                    end
                  else
                    result:=OptPass1OP(p);
                end;
              A_SLTI:
                begin
                  {
                    Turn
                      slti x,y,0
                      beq/ne x,x0,...
                      dealloc x
                    Into
                      bge/lt y,x0,...
                  }
                  if (taicpu(p).ops=3) and
                     (taicpu(p).oper[2]^.typ=top_const) and
                     (taicpu(p).oper[2]^.val=0) and
                     GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode=A_Bxx) and
                     (taicpu(hp1).ops=3) and
                     (taicpu(hp1).oper[0]^.typ=top_reg) and
                     (taicpu(hp1).oper[0]^.reg=taicpu(p).oper[0]^.reg) and
                     (taicpu(hp1).oper[1]^.typ=top_reg) and
                     (taicpu(hp1).oper[1]^.reg=NR_X0) and
                     (taicpu(hp1).condition in [C_NE,C_EQ]) and
                     (not RegModifiedBetween(taicpu(p).oper[1]^.reg, p,hp1)) and
                     RegEndOfLife(taicpu(p).oper[0]^.reg, taicpu(hp1)) then
                    begin
                      taicpu(hp1).loadreg(0,taicpu(p).oper[1]^.reg);
                      taicpu(hp1).loadreg(1,NR_X0);

                      if taicpu(hp1).condition=C_NE then
                        taicpu(hp1).condition:=C_LT
                      else
                        taicpu(hp1).condition:=C_GE;

                      DebugMsg('Peephole Slti0B2B performed', hp1);

                      RemoveInstr(p);

                      result:=true;
                    end;
                end;
              A_FADD_S,
              A_FSUB_S,
              A_FMUL_S,
              A_FDIV_S,
              A_FSQRT_S,
              A_FNEG_S,
              A_FLW,
              A_FCVT_D_S,
              A_FMADD_S,A_FMSUB_S,A_FNMSUB_S,A_FNMADD_S,
              A_FMIN_S,A_FMAX_S:
                result:=OptPass1FOP(p,A_FSGNJ_S);
              A_FADD_D,
              A_FSUB_D,
              A_FMUL_D,
              A_FDIV_D,
              A_FSQRT_D,
              A_FNEG_D,
              A_FLD,
              A_FCVT_S_D,
              A_FMADD_D,A_FMSUB_D,A_FNMSUB_D,A_FNMADD_D,
              A_FMIN_D,A_FMAX_D:
                result:=OptPass1FOP(p,A_FSGNJ_D);
              A_FSGNJ_S,
              A_FSGNJ_D:
                result:=OptPass1FSGNJ(p,taicpu(p).opcode);
              else
                ;
            end;
          end;
        else
          ;
      end;
    end;

end.
