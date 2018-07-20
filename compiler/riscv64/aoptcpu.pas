{
    Copyright (c) 1998-2002 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit implements the RiscV64 optimizer object

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

unit aoptcpu;

interface

{$I fpcdefs.inc}

{$define DEBUG_AOPTCPU}

uses
  cpubase,
  globals, globtype,
  cgbase,
  aoptobj, aoptcpub, aopt,
  aasmtai, aasmcpu;

type
  TCpuAsmOptimizer = class(TAsmOptimizer)
    function RegLoadedWithNewValue(reg: tregister; hp: tai): boolean; override;
    Function GetNextInstructionUsingReg(Current: tai; Out Next: tai; reg: TRegister): Boolean;
    { outputs a debug message into the assembler file }
    procedure DebugMsg(const s: string; p: tai);

    function PeepHoleOptPass1Cpu(var p: tai): boolean; override;
  end;

implementation

  uses
    cutils;

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


  function TCpuAsmOptimizer.RegLoadedWithNewValue(reg: tregister; hp: tai): boolean;
    begin
      result:=
        (hp.typ=ait_instruction) and
        (taicpu(hp).ops>1) and
        (taicpu(hp).oper[0]^.typ=top_reg) and
        (taicpu(hp).oper[0]^.reg=reg) and
        (taicpu(hp).spilling_get_operation_type(0)=operand_write);
    end;


  function TCpuAsmOptimizer.GetNextInstructionUsingReg(Current: tai; out Next: tai; reg: TRegister): Boolean;
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


  function TCpuAsmOptimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;
    var
      hp1: tai;
    begin
      result:=false;
      case p.typ of
        ait_instruction:
          begin
            case taicpu(p).opcode of
              A_ADDI:
                begin
                  {
                    Changes
                      addi x, y, #
                      addi z, x, #
                      dealloc x
                    To
                      addi z, y, #+#
                  }
                  if (taicpu(p).ops=3) and
                     (taicpu(p).oper[2]^.typ=top_const) and
                     GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) and
                     (hp1.typ=ait_instruction) and         
                     (taicpu(hp1).opcode=A_ADDI) and
                     (taicpu(hp1).ops=3) and
                     (taicpu(p).oper[2]^.typ=top_const) and
                     is_imm12(taicpu(p).oper[2]^.val+taicpu(hp1).oper[2]^.val) and
                     (not RegModifiedBetween(taicpu(p).oper[1]^.reg, p,hp1)) and
                     RegEndOfLife(taicpu(p).oper[0]^.reg, taicpu(hp1)) then
                    begin                                                                       
                      taicpu(hp1).loadreg(1,taicpu(p).oper[1]^.reg);
                      taicpu(hp1).loadconst(2, taicpu(p).oper[2]^.val+taicpu(hp1).oper[2]^.val);

                      DebugMsg('Peephole AddiAddi2Addi performed', hp1);

                      GetNextInstruction(p,hp1);
                      AsmL.Remove(p);
                      p.Free;
                      p:=hp1;

                      result:=true;
                    end
                  {
                    Changes
                      addi x, x, (ref)
                      ld/sd y, 0(x)
                      dealloc x
                    To
                      ld/sd y, 0(ref)(x)
                  }
                  else if (taicpu(p).ops=3) and
                     (taicpu(p).oper[2]^.typ=top_ref) and
                     (taicpu(p).oper[0]^.reg=taicpu(p).oper[1]^.reg) and
                     GetNextInstructionUsingReg(p, hp1, taicpu(p).oper[0]^.reg) and
                     (hp1.typ=ait_instruction) and
                     (taicpu(hp1).opcode in [A_LB,A_LBU,A_LH,A_LHU,A_LW,A_LWU,A_LD,
                                             A_SB,A_SH,A_SW,A_SD]) and
                     (taicpu(hp1).ops=2) and
                     (taicpu(hp1).oper[1]^.typ=top_ref) and
                     (taicpu(hp1).oper[1]^.ref^.base=taicpu(p).oper[0]^.reg) and
                     (taicpu(hp1).oper[1]^.ref^.offset=0) and
                     (not RegModifiedBetween(taicpu(p).oper[1]^.reg, p,hp1)) and
                     RegEndOfLife(taicpu(p).oper[0]^.reg, taicpu(hp1)) then
                    begin
                      taicpu(hp1).loadref(1,taicpu(p).oper[2]^.ref^);
                      taicpu(hp1).oper[1]^.ref^.base:=taicpu(p).oper[0]^.reg;

                      DebugMsg('Peephole AddiMem2Mem performed', hp1);

                      GetNextInstruction(p,hp1);
                      AsmL.Remove(p);
                      p.Free;
                      p:=hp1;

                      result:=true;
                    end;
                end;
              A_ANDI:
                begin

                end;
            end;
          end;
      end;
    end;

begin
  casmoptimizer := TCpuAsmOptimizer;
end.
