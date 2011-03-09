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
  TCpuAsmOptimizer = class(TAsmOptimizer)
    { uses the same constructor as TAopObj }
    function PeepHoleOptPass1Cpu(var p: tai): boolean; override;
    procedure PeepHoleOptPass2;override;
  End;

Implementation

  uses
    aasmbase,aasmcpu,cgbase;

  function CanBeCond(p : tai) : boolean;
    begin
      result:=(p.typ=ait_instruction) and (taicpu(p).condition=C_None);
    end;


  function TCpuAsmOptimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;
    var
      next1: tai;
    begin
      result := false;
      case p.typ of
        ait_instruction:
          begin
            case taicpu(p).opcode of
              A_MOV:
                begin
                  { fold
                    mov reg2,reg0
                    mov reg3,reg1
                    to
                    movw reg2,reg0
                  }
                  if (taicpu(p).ops=2) and
                     (taicpu(p).oper[0]^.typ = top_reg) and
                     (taicpu(p).oper[1]^.typ = top_reg) and
                     getnextinstruction(p,next1) and
                     (next1.typ = ait_instruction) and
                     (taicpu(next1).opcode = A_MOV) and
                     (taicpu(next1).ops=2) and
                     (taicpu(next1).oper[0]^.typ = top_reg) and
                     (taicpu(next1).oper[1]^.typ = top_reg) and
                     (getsupreg(taicpu(next1).oper[0]^.reg)=getsupreg(taicpu(p).oper[0]^.reg)+1) and
                     ((getsupreg(taicpu(p).oper[0]^.reg) mod 2)=0) and
                     ((getsupreg(taicpu(p).oper[1]^.reg) mod 2)=0) and
                     (getsupreg(taicpu(next1).oper[1]^.reg)=getsupreg(taicpu(p).oper[1]^.reg)+1) then
                    begin
                      taicpu(p).opcode:=A_MOVW;
                      asml.remove(next1);
                      next1.free;
                      result := true;
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
