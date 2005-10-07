{
    Copyright (c) 1998-2002 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit implements the PowerPC optimizer object

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

Interface

{$i fpcdefs.inc}

uses cpubase, aoptobj, aoptcpub, aopt, aasmtai;

Type
  TCpuAsmOptimizer = class(TAsmOptimizer)
    { uses the same constructor as TAopObj }
    function PeepHoleOptPass1Cpu(var p: tai): boolean; override;
  End;

Implementation

  uses
    cutils, aasmcpu;

  function TCpuAsmOptimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;
    var
      next1, next2: tai;
      l1, l2: longint;
    begin
      result := false;
      case p.typ of
        ait_instruction:
          begin
            case taicpu(p).opcode of
              A_RLWINM:
                begin
                  if getnextinstruction(p,next1) and
                     (next1.typ = ait_instruction) and
                     (taicpu(next1).opcode = A_RLWINM) and
                     (taicpu(next1).oper[0]^.reg = taicpu(p).oper[0]^.reg) and
                     // both source and target of next1 must equal target of p
                     (taicpu(next1).oper[1]^.reg = taicpu(p).oper[0]^.reg) and
                     (taicpu(next1).oper[2]^.val = 0) then
                    begin
                      l1 := taicpu(p).oper[4]^.val;
                      if (l1 < taicpu(p).oper[3]^.val) then
                        inc(l1,32);
                      l2 := taicpu(next1).oper[4]^.val;
                      if (l2 < taicpu(next1).oper[3]^.val) then
                        inc(l2,32);

                      if (taicpu(p).oper[3]^.val > l2) or
                         (taicpu(next1).oper[3]^.val > l1) then
                        begin
                          // masks have no bits in common
                          taicpu(p).opcode := A_LI;
                          taicpu(p).loadconst(1,0);
                          taicpu(p).clearop(2);
                          taicpu(p).clearop(3);
                          taicpu(p).clearop(4);
                        end
                      else
                        begin
                          taicpu(p).oper[3]^.val := max(taicpu(p).oper[3]^.val,taicpu(next1).oper[3]^.val);
                          taicpu(p).oper[4]^.val := min(taicpu(p).oper[4]^.val,taicpu(next1).oper[4]^.val);
                        end;
                      asml.remove(next1);
                      next1.free;
                    end;
                end;
            end;
          end;
      end;
    end;

begin
  casmoptimizer:=TCpuAsmOptimizer;
End.

