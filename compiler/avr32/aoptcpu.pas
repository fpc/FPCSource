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

uses cpubase, aasmtai, aopt, aoptcpub;

Type

  { TCpuAsmOptimizer }

  TCpuAsmOptimizer = class(TAsmOptimizer)
    { uses the same constructor as TAopObj }
    function PeepHoleOptPass1Cpu(var p: tai): boolean; override;
  End;

Implementation

  uses
    verbose,
    aasmbase,aasmcpu,
    cgbase;

{ TCpuAsmOptimizer }

  function Test_Set(const RegSet,t: tcpuregisterset): boolean;
    begin
      result := ((RegSet*t) = []) or ((RegSet*t) = t);
    end;

  function TCpuAsmOptimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;
    var
      v: LongInt;
      hp1: tai;
    begin
      result := false;
      case p.typ of
        ait_instruction:
          begin
            { Collapse ADD r0,r0,r1 -> ADD r0,r1 }
            if (taicpu(p).opcode in [A_ADD,A_SUB,A_AND,A_EOR,A_OR,A_MUL]) and
               (taicpu(p).ops = 3) and
               (taicpu(p).oper[0]^.typ = top_reg) and
               (taicpu(p).oper[1]^.typ = top_reg) and
               (taicpu(p).oper[2]^.typ = top_reg) and
               (taicpu(p).oper[0]^.reg = taicpu(p).oper[1]^.reg) then
              begin
                hp1:=taicpu.op_reg_reg(taicpu(p).opcode,taicpu(p).oper[0]^.reg,taicpu(p).oper[2]^.reg);
                AsmL.InsertBefore(hp1,p);
                //InsertLLItem(p.Previous,p.Next,hp1);
                AsmL.Remove(p);
                p.Free;
                p:=hp1;
                result:=true;
              end
            { Collapse ADD r0,r1,r0 -> ADD r0,r1
              SUB needs RSUB }
            else if (taicpu(p).opcode in [A_ADD,A_AND,A_EOR,A_OR,A_MUL]) and
               (taicpu(p).ops = 3) and
               (taicpu(p).oper[0]^.typ = top_reg) and
               (taicpu(p).oper[1]^.typ = top_reg) and
               (taicpu(p).oper[2]^.typ = top_reg) and
               (taicpu(p).oper[0]^.reg = taicpu(p).oper[2]^.reg) then
              begin
                hp1:=taicpu.op_reg_reg(taicpu(p).opcode,taicpu(p).oper[0]^.reg,taicpu(p).oper[1]^.reg);
                AsmL.InsertBefore(hp1,p);
                //InsertLLItem(p.Previous,p.Next,hp1);
                AsmL.Remove(p);
                p.Free;
                p:=hp1;
                result:=true;
              end
            else
              case taicpu(p).opcode of
                { Collapse instructions into a compact format }
                A_SUB:
                  begin
                    { SUB r0,r0,imm8 -> SUB r0,imm8 }
                    if (taicpu(p).ops=3) and
                       (taicpu(p).oper[0]^.typ = top_reg) and
                       (taicpu(p).oper[1]^.typ = top_reg) and
                       (taicpu(p).oper[2]^.typ = top_const) and
                       (taicpu(p).oper[0]^.reg = taicpu(p).oper[1]^.reg) and
                       in_signed_bits(taicpu(p).oper[2]^.val, 8) then
                      begin
                        hp1:=taicpu.op_reg_const(taicpu(p).opcode,taicpu(p).oper[0]^.reg,taicpu(p).oper[2]^.val);
                        AsmL.InsertBefore(hp1,p);
                        //InsertLLItem(p.Previous,p.Next,hp1);
                        AsmL.Remove(p);
                        p.Free;
                        p:=hp1;
                        result:=true;
                      end
                    { SUB r0,r1,r0 -> RSUB r0,r1 }
                    else if (taicpu(p).ops=3) and
                            (taicpu(p).oper[0]^.typ = top_reg) and
                            (taicpu(p).oper[1]^.typ = top_reg) and
                            (taicpu(p).oper[2]^.typ = top_reg) and
                            (taicpu(p).oper[0]^.reg = taicpu(p).oper[2]^.reg) then
                      begin
                        hp1:=taicpu.op_reg_reg(A_RSUB,taicpu(p).oper[0]^.reg,taicpu(p).oper[1]^.reg);
                        AsmL.InsertBefore(hp1,p);
                        //InsertLLItem(p.Previous,p.Next,hp1);
                        AsmL.Remove(p);
                        p.Free;
                        p:=hp1;
                        result:=true;
                      end;
                  end;
                A_LSL,
                A_LSR,
                A_ASR:
                  begin
                    { LSL r0,r0,imm8 -> LSL r0,imm8 }
                    if (taicpu(p).ops=3) and
                       (taicpu(p).oper[0]^.typ = top_reg) and
                       (taicpu(p).oper[1]^.typ = top_reg) and
                       (taicpu(p).oper[2]^.typ = top_const) and
                       (taicpu(p).oper[0]^.reg = taicpu(p).oper[1]^.reg) then
                      begin
                        hp1:=taicpu.op_reg_const(taicpu(p).opcode,taicpu(p).oper[0]^.reg,taicpu(p).oper[2]^.val);
                        AsmL.InsertBefore(hp1,p);
                        //InsertLLItem(p.Previous,p.Next,hp1);
                        AsmL.Remove(p);
                        p.Free;
                        p:=hp1;
                        result:=true;
                      end
                  end;
                A_STM:
                  begin
                    { Try to compress STM --sp, LIST down to PUSHM LIST }
                    if (taicpu(p).oper[0]^.ref^.base = NR_STACK_POINTER_REG) and
                       (taicpu(p).oper[0]^.ref^.addressmode = AM_PREINDEXED) and
                       (taicpu(p).oper[1]^.regset^ <> []) then
                      begin
                        if test_set(taicpu(p).oper[1]^.regset^, [0..3]) and
                           test_set(taicpu(p).oper[1]^.regset^, [4..7]) and
                           test_set(taicpu(p).oper[1]^.regset^, [8..9]) and
                           test_set(taicpu(p).oper[1]^.regset^, [10]) and
                           test_set(taicpu(p).oper[1]^.regset^, [11]) and
                           test_set(taicpu(p).oper[1]^.regset^, [12]) and
                           test_set(taicpu(p).oper[1]^.regset^, [14]) and
                           test_set(taicpu(p).oper[1]^.regset^, [15]) and
                           ((taicpu(p).oper[1]^.regset^ * [13]) = []) then
                          begin
                            hp1:=taicpu.op_regset(A_PUSHM,taicpu(p).oper[1]^.regtyp,taicpu(p).oper[1]^.subreg,taicpu(p).oper[1]^.regset^);
                            AsmL.InsertBefore(hp1,p);
                            //InsertLLItem(p.Previous,p.Next,hp1);
                            AsmL.Remove(p);
                            p.Free;
                            p:=hp1;
                            result:=true;
                            exit;
                          end;
                      end;
                  end;
                A_LDM:
                  begin
                    { Try to compress LDM sp++, LIST down to POPM LIST }
                    if (taicpu(p).oper[0]^.ref^.base = NR_STACK_POINTER_REG) and
                       ((taicpu(p).oper[0]^.ref^.addressmode = AM_POSTINDEXED) or
                        (RS_STACK_POINTER_REG in taicpu(p).oper[1]^.regset^)) and
                       (taicpu(p).oper[1]^.regset^ <> []) then
                      begin
                        if test_set(taicpu(p).oper[1]^.regset^, [0..3]) and
                           test_set(taicpu(p).oper[1]^.regset^, [4..7]) and
                           test_set(taicpu(p).oper[1]^.regset^, [8..9]) and
                           test_set(taicpu(p).oper[1]^.regset^, [10]) and
                           test_set(taicpu(p).oper[1]^.regset^, [11]) and
                           test_set(taicpu(p).oper[1]^.regset^, [12]) and
                           test_set(taicpu(p).oper[1]^.regset^, [14]) and
                           test_set(taicpu(p).oper[1]^.regset^, [15]) and
                           ((taicpu(p).oper[1]^.regset^ * [13]) = []) then
                          begin
                            hp1:=taicpu.op_regset(A_POPM,taicpu(p).oper[1]^.regtyp,taicpu(p).oper[1]^.subreg,taicpu(p).oper[1]^.regset^);
                            AsmL.InsertBefore(hp1,p);
                            //InsertLLItem(p.Previous,p.Next,hp1);
                            AsmL.Remove(p);
                            p.Free;
                            p:=hp1;
                            result:=true;
                            exit;
                          end;
                      end;
                  end;
              end;
          end;
      end;
    end;

begin
  casmoptimizer:=TCpuAsmOptimizer;
End.
