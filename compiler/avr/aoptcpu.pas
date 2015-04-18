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

Interface

uses cpubase, cgbase, aasmtai, aopt, aoptcpub;

Type
  TCpuAsmOptimizer = class(TAsmOptimizer)
    Function GetNextInstructionUsingReg(Current: tai; Var Next: tai;reg : TRegister): Boolean;
    function RegInInstruction(Reg: TRegister; p1: tai): Boolean; override;

    { uses the same constructor as TAopObj }
    function PeepHoleOptPass1Cpu(var p: tai): boolean; override;
    procedure PeepHoleOptPass2;override;
  End;

Implementation

  uses
    cpuinfo,
    aasmbase,aasmcpu,
    globals,globtype;

  function CanBeCond(p : tai) : boolean;
    begin
      result:=(p.typ=ait_instruction) and (taicpu(p).condition=C_None);
    end;


  function TCpuAsmOptimizer.RegInInstruction(Reg: TRegister; p1: tai): Boolean;
    begin
      If (p1.typ = ait_instruction) and (taicpu(p1).opcode in [A_MUL,A_MULS,A_FMUL,A_FMULS,A_FMULSU]) and
              ((getsupreg(reg)=RS_R0) or (getsupreg(reg)=RS_R1)) then
        Result:=true
      else
        Result:=inherited RegInInstruction(Reg, p1);
    end;


  function TCpuAsmOptimizer.GetNextInstructionUsingReg(Current: tai;
    var Next: tai; reg: TRegister): Boolean;
    begin
      Next:=Current;
      repeat
        Result:=GetNextInstruction(Next,Next);
      until not(cs_opt_level3 in current_settings.optimizerswitches) or not(Result) or (Next.typ<>ait_instruction) or (RegInInstruction(reg,Next)) or
        (is_calljmp(taicpu(Next).opcode));
    end;


  function TCpuAsmOptimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;
    var
      hp1,hp2,hp3: tai;
      alloc, dealloc: tai_regalloc;
      i: integer;
    begin
      result := false;
      case p.typ of
        ait_instruction:
          begin
            case taicpu(p).opcode of
              A_LDI:
                begin
                  { turn
                    ldi reg0, imm
                    cp reg1, reg0
                    dealloc reg0
                    into
                    cpi reg1, imm
                  }
                  if (taicpu(p).ops=2) and
                     (taicpu(p).oper[0]^.typ=top_reg) and
                     (taicpu(p).oper[1]^.typ=top_const) and
                     GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) and
                     (hp1.typ=ait_instruction) and
                     (not RegModifiedBetween(taicpu(p).oper[0]^.reg, p, hp1)) and
                     (taicpu(hp1).opcode=A_CP) and
                     (taicpu(hp1).ops=2) and
                     (taicpu(hp1).oper[1]^.typ=top_reg) and
                     (getsupreg(taicpu(hp1).oper[0]^.reg) in [16..31]) and
                     (taicpu(hp1).oper[1]^.reg=taicpu(p).oper[0]^.reg) and
                     assigned(FindRegDeAlloc(taicpu(p).oper[0]^.reg,tai(hp1.Next))) then
                    begin
                      taicpu(hp1).opcode:=A_CPI;
                      taicpu(hp1).loadconst(1, taicpu(p).oper[1]^.val);

                      alloc:=FindRegAllocBackward(taicpu(p).oper[0]^.reg,tai(p.Previous));
                      dealloc:=FindRegDeAlloc(taicpu(p).oper[0]^.reg,tai(hp1.Next));

                      if assigned(alloc) and assigned(dealloc) then
                        begin
                          asml.Remove(alloc);
                          alloc.Free;
                          asml.Remove(dealloc);
                          dealloc.Free;
                        end;

                      GetNextInstruction(p,hp1);
                      asml.Remove(p);
                      p.Free;
                      p:=hp1;

                      result:=true;
                    end;
                end;
              A_CLR:
                begin
                  { turn the common
                    clr rX
                    mov/ld rX, rY
                    into
                    mov/ld rX, rY
                  }
                  if (taicpu(p).ops=1) and
                     (taicpu(p).oper[0]^.typ=top_reg) and
                     GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) and
                     (not RegModifiedBetween(taicpu(p).oper[0]^.reg, p, hp1)) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode in [A_MOV,A_LD]) and
                     (taicpu(hp1).ops>0) and
                     (taicpu(hp1).oper[0]^.typ=top_reg) and
                     (taicpu(hp1).oper[0]^.reg=taicpu(p).oper[0]^.reg) then
                    begin
                      asml.Remove(p);
                      p.Free;
                      p:=hp1;
                      result:=true;
                    end
                  { turn
                    clr rX
                    ...
                    adc rY, rX
                    into
                    ...
                    adc rY, r1
                  }
                  else if (taicpu(p).ops=1) and
                     (taicpu(p).oper[0]^.typ=top_reg) and
                     GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) and
                     (not RegModifiedBetween(taicpu(p).oper[0]^.reg, p, hp1)) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode in [A_ADC,A_SBC]) and
                     (taicpu(hp1).ops=2) and
                     (taicpu(hp1).oper[1]^.typ=top_reg) and
                     (taicpu(hp1).oper[1]^.reg=taicpu(p).oper[0]^.reg) and
                     (taicpu(hp1).oper[0]^.reg<>taicpu(p).oper[0]^.reg) and
                     assigned(FindRegDeAlloc(taicpu(p).oper[0]^.reg,tai(hp1.Next))) then
                    begin
                      taicpu(hp1).oper[1]^.reg:=NR_R1;

                      alloc:=FindRegAllocBackward(taicpu(p).oper[0]^.reg,tai(p.Previous));
                      dealloc:=FindRegDeAlloc(taicpu(p).oper[0]^.reg,tai(hp1.Next));

                      if assigned(alloc) and assigned(dealloc) then
                        begin
                          asml.Remove(alloc);
                          alloc.Free;
                          asml.Remove(dealloc);
                          dealloc.Free;
                        end;

                      GetNextInstruction(p,hp1);
                      asml.Remove(p);
                      p.free;
                      p:=hp1;

                      result:=true;
                    end;
                end;
              A_PUSH:
                begin
                  { turn
                    push reg0
                    push reg1
                    pop reg3
                    pop reg2
                    into
                    movw reg2,reg0
                  }
                  if (taicpu(p).ops=1) and
                     (taicpu(p).oper[0]^.typ=top_reg) and
                     GetNextInstruction(p,hp1) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode=A_PUSH) and
                     (getsupreg(taicpu(hp1).oper[0]^.reg)=getsupreg(taicpu(p).oper[0]^.reg)+1) and
                     ((getsupreg(taicpu(p).oper[0]^.reg) mod 2)=0) and

                     GetNextInstruction(hp1,hp2) and
                     (hp2.typ=ait_instruction) and
                     (taicpu(hp2).opcode=A_POP) and

                     GetNextInstruction(hp2,hp3) and
                     (hp3.typ=ait_instruction) and
                     (taicpu(hp3).opcode=A_POP) and
                     (getsupreg(taicpu(hp2).oper[0]^.reg)=getsupreg(taicpu(hp3).oper[0]^.reg)+1) and
                     ((getsupreg(taicpu(hp3).oper[0]^.reg) mod 2)=0) then
                    begin
                      taicpu(p).ops:=2;
                      taicpu(p).opcode:=A_MOVW;

                      taicpu(p).loadreg(1, taicpu(p).oper[0]^.reg);
                      taicpu(p).loadreg(0, taicpu(hp3).oper[0]^.reg);

                      asml.Remove(hp1);
                      hp1.Free;
                      asml.Remove(hp2);
                      hp2.Free;
                      asml.Remove(hp3);
                      hp3.Free;

                      result:=true;
                    end;
                end;
              A_MOV:
                begin
                  { turn
                    mov reg0, reg1
                    push reg0
                    dealloc reg0
                    into
                    push reg1
                  }
                  if (taicpu(p).ops=2) and
                     (taicpu(p).oper[0]^.typ = top_reg) and
                     (taicpu(p).oper[1]^.typ = top_reg) and
                     GetNextInstructionUsingReg(p,hp1,taicpu(p).oper[0]^.reg) and
                     (not RegModifiedBetween(taicpu(p).oper[1]^.reg, p, hp1)) and
                     (hp1.typ = ait_instruction) and
                     (taicpu(hp1).opcode in [A_PUSH,A_MOV,A_CP,A_CPC,A_ADD,A_SUB,A_EOR,A_AND,A_OR]) and
                     RegInInstruction(taicpu(p).oper[0]^.reg, hp1) and
                     (not RegModifiedByInstruction(taicpu(p).oper[0]^.reg, hp1)) and
                     {(taicpu(hp1).ops=1) and
                     (taicpu(hp1).oper[0]^.typ = top_reg) and
                     (taicpu(hp1).oper[0]^.reg=taicpu(p).oper[0]^.reg) and  }
                     assigned(FindRegDeAlloc(taicpu(p).oper[0]^.reg,tai(hp1.Next))) then
                    begin
                      for i := 0 to taicpu(hp1).ops-1 do
                        if taicpu(hp1).oper[i]^.typ=top_reg then
                          if taicpu(hp1).oper[i]^.reg=taicpu(p).oper[0]^.reg then
                            taicpu(hp1).oper[i]^.reg:=taicpu(p).oper[1]^.reg;

                      alloc:=FindRegAllocBackward(taicpu(p).oper[0]^.reg,tai(p.Previous));
                      dealloc:=FindRegDeAlloc(taicpu(p).oper[0]^.reg,tai(hp1.Next));

                      if assigned(alloc) and assigned(dealloc) then
                        begin
                          asml.Remove(alloc);
                          alloc.Free;
                          asml.Remove(dealloc);
                          dealloc.Free;
                        end;

                      GetNextInstruction(p,hp1);
                      asml.Remove(p);
                      p.free;
                      p:=hp1;
                      result:=true;
                    end
                  { remove
                    mov reg0,reg0
                  }
                  else if (taicpu(p).ops=2) and
                     (taicpu(p).oper[0]^.typ = top_reg) and
                     (taicpu(p).oper[1]^.typ = top_reg) and
                     (taicpu(p).oper[0]^.reg = taicpu(p).oper[1]^.reg) then
                    begin
                      GetNextInstruction(p,hp1);
                      asml.remove(p);
                      p.free;
                      p:=hp1;
                      result:=true;
                    end
                  { fold
                    mov reg2,reg0
                    mov reg3,reg1
                    to
                    movw reg2,reg0
                  }
                  else if (CPUAVR_HAS_MOVW in cpu_capabilities[current_settings.cputype]) and
                     (taicpu(p).ops=2) and
                     (taicpu(p).oper[0]^.typ = top_reg) and
                     (taicpu(p).oper[1]^.typ = top_reg) and
                     getnextinstruction(p,hp1) and
                     (hp1.typ = ait_instruction) and
                     (taicpu(hp1).opcode = A_MOV) and
                     (taicpu(hp1).ops=2) and
                     (taicpu(hp1).oper[0]^.typ = top_reg) and
                     (taicpu(hp1).oper[1]^.typ = top_reg) and
                     (getsupreg(taicpu(hp1).oper[0]^.reg)=getsupreg(taicpu(p).oper[0]^.reg)+1) and
                     ((getsupreg(taicpu(p).oper[0]^.reg) mod 2)=0) and
                     ((getsupreg(taicpu(p).oper[1]^.reg) mod 2)=0) and
                     (getsupreg(taicpu(hp1).oper[1]^.reg)=getsupreg(taicpu(p).oper[1]^.reg)+1) then
                    begin
                      alloc:=FindRegAllocBackward(taicpu(hp1).oper[0]^.reg,tai(hp1.Previous));
                      if assigned(alloc) then
                        begin
                          asml.Remove(alloc);
                          asml.InsertBefore(alloc,p);
                        end;

                      taicpu(p).opcode:=A_MOVW;
                      asml.remove(hp1);
                      hp1.free;
                      result:=true;
                    end;
                end;
            end;
          end;
      end;
    end;


  procedure TCpuAsmOptimizer.PeepHoleOptPass2;
    begin
    end;

begin
  casmoptimizer:=TCpuAsmOptimizer;
End.
