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
              A_SRWI:
                begin
                  if getnextinstruction(p,next1) and
                     (next1.typ = ait_instruction) and
                     ((taicpu(next1).opcode = A_SLWI) or
                      (taicpu(next1).opcode = A_RLWINM)) and
                     (taicpu(next1).oper[0]^.reg = taicpu(p).oper[0]^.reg) and
                     (taicpu(next1).oper[1]^.reg = taicpu(p).oper[0]^.reg) then
                    case taicpu(next1).opcode of
                      A_SLWI:
                        begin
                          taicpu(p).opcode := A_RLWINM;
                          taicpu(p).ops := 5;
                          taicpu(p).loadconst(2,taicpu(next1).oper[2]^.val-taicpu(p).oper[2]^.val);
                          if (taicpu(p).oper[2]^.val < 0) then
                            begin
                              taicpu(p).loadconst(3,-taicpu(p).oper[2]^.val);
                              taicpu(p).loadconst(4,31-taicpu(next1).oper[2]^.val);
                              inc(taicpu(p).oper[2]^.val,32);
                            end
                          else
                            begin
                              taicpu(p).loadconst(3,0);
                              taicpu(p).loadconst(4,31-taicpu(next1).oper[2]^.val);
                            end;
                          asml.remove(next1);
                          next1.free;
                          result := true;
                        end;
                      A_RLWINM:
                        begin
                          if (taicpu(next1).oper[2]^.val = 0) then
                            begin
                              { convert srwi to rlwinm and see if the rlwinm }
                              { optimization can do something with it        }
                              taicpu(p).opcode := A_RLWINM;
                              taicpu(p).ops := 5;
                              taicpu(p).loadconst(3,taicpu(p).oper[2]^.val);
                              taicpu(p).loadconst(4,31);
                              taicpu(p).loadconst(2,(32-taicpu(p).oper[2]^.val) and 31);
                              result := true;
                            end;
                        end;
                    end;
                end;
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
                          taicpu(p).ops := 2;
                          taicpu(p).opercnt := 2;
                          asml.remove(next1);
                          next1.free;
                        end
                      else
                        // some of the cases with l1>32 or l2>32 can be
                        // optimized, but others can't (like 19,17 and 25,23)
                        if (l1 < 32) and
                           (l2 < 32) then
                        begin
                          taicpu(p).oper[3]^.val := max(taicpu(p).oper[3]^.val,taicpu(next1).oper[3]^.val);
                          taicpu(p).oper[4]^.val := min(taicpu(p).oper[4]^.val,taicpu(next1).oper[4]^.val);
                          asml.remove(next1);
                          next1.free;
                          result := true;
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

