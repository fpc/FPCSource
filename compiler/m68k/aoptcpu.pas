{
    Copyright (c) 1998-2014 by the Free Pascal development team

    This unit calls the optimization procedures to optimize the assembler
    code for m68k

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

{$i fpcdefs.inc}

{$define DEBUG_AOPTCPU}

  Interface

    uses
      cpubase, aoptobj, aoptcpub, aopt, aasmtai;

    Type
      TCpuAsmOptimizer = class(TAsmOptimizer)
        function PeepHoleOptPass1Cpu(var p: tai): boolean; override;

        { outputs a debug message into the assembler file }
        procedure DebugMsg(const s: string; p: tai);
      End;

  Implementation

    uses
      cutils, aasmcpu, cgutils;

{$ifdef DEBUG_AOPTCPU}
  procedure TCpuAsmOptimizer.DebugMsg(const s: string; p : tai);
    begin
      asml.insertbefore(tai_comment.Create(strpnew(s)), p);
    end;
{$else DEBUG_AOPTCPU}
  procedure TCpuAsmOptimizer.DebugMsg(const s: string; p : tai);inline;
    begin
    end;
{$endif DEBUG_AOPTCPU}

  function TCpuAsmOptimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;
    var
      next: tai;
      tmpref: treference;
    begin
      result:=false;
      case p.typ of
        ait_instruction:
          begin
            //asml.insertbefore(tai_comment.Create(strpnew('pass1 called for instr')), p);

            case taicpu(p).opcode of
              { LEA (Ax),Ax is a NOP if src and dest reg is equal, so remove it. }
              A_LEA:
                if GetNextInstruction(p,next) and not assigned(taicpu(p).oper[0]^.ref^.symbol) and
                   (((taicpu(p).oper[0]^.ref^.base = taicpu(p).oper[1]^.reg) and
                   (taicpu(p).oper[0]^.ref^.index = NR_NO)) or
                   ((taicpu(p).oper[0]^.ref^.index = taicpu(p).oper[1]^.reg) and
                   (taicpu(p).oper[0]^.ref^.base = NR_NO))) and
                   (taicpu(p).oper[0]^.ref^.offset = 0) then
                  begin
                    DebugMsg('Optimizer: LEA 0(Ax),Ax removed',p);
                    asml.remove(p);
                    p.free;
                    p:=next;
                    result:=true;
                  end;
              { Address register sub/add can be replaced with ADDQ/SUBQ or LEA if the value is in the
                SmallInt range, which is shorter to encode and faster to execute on most 68k }
              A_SUB,A_SUBA,A_ADD,A_ADDA:
                if (taicpu(p).oper[1]^.typ = top_reg) and isaddressregister(taicpu(p).oper[1]^.reg) and
                   (taicpu(p).oper[0]^.typ = top_const) then
                  begin
                    if isvalueforaddqsubq(taicpu(p).oper[0]^.val) then
                      begin
                        DebugMsg('Optimizer: SUB/ADD #val,Ax to SUBQ/ADDQ',p);
                        taicpu(p).opsize:=S_L; // this is safe, because we're targetting an address reg
                        if taicpu(p).opcode in [A_ADD,A_ADDA] then
                          taicpu(p).opcode:=A_ADDQ
                        else
                          taicpu(p).opcode:=A_SUBQ;
                        result:=true;
                      end
                    else
                      if isvalue16bit(abs(taicpu(p).oper[0]^.val)) then
                        begin
                          DebugMsg('Optimizer: SUB/ADD #val,Ax to LEA val(Ax),Ax',p);
                          if taicpu(p).opcode in [A_SUB,A_SUBA] then
                            reference_reset_base(tmpref,taicpu(p).oper[1]^.reg,-taicpu(p).oper[0]^.val,0)
                          else
                            reference_reset_base(tmpref,taicpu(p).oper[1]^.reg,taicpu(p).oper[0]^.val,0);
                          taicpu(p).opcode:=A_LEA;
                          taicpu(p).loadref(0,tmpref);
                          result:=true;
                        end;
                  end;
              { CMP #0,<ea> equals to TST <ea>, just shorter and TST is more flexible anyway }
              A_CMP:
                if (taicpu(p).oper[0]^.typ = top_const) and
                   (taicpu(p).oper[0]^.val = 0) then
                  begin
                    DebugMsg('Optimizer: CMP #0 to TST',p);
                    taicpu(p).opcode:=A_TST;
                    taicpu(p).loadoper(0,taicpu(p).oper[1]^);
                    taicpu(p).clearop(1);
                    taicpu(p).ops:=1;
                    result:=true;
                  end;
            end;
          end;
      end;
    end;

begin
  casmoptimizer:=TCpuAsmOptimizer;
end.
