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

  Interface

    uses
      cpubase, aoptobj, aoptcpub, aopt, aasmtai;

    Type
      TCpuAsmOptimizer = class(TAsmOptimizer)
        function PeepHoleOptPass1Cpu(var p: tai): boolean; override;
      End;

  Implementation

    uses
      cutils, aasmcpu;

  function TCpuAsmOptimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;
    var
      next: tai;
    begin
      result:=false;
      case p.typ of
        ait_instruction:
          begin
            //asml.insertbefore(tai_comment.Create(strpnew('pass1 called for instr')), p);

            { LEA (Ax),Ax is a NOP if src and dest reg is equal, so remove it. }
            if getnextinstruction(p,next) and (taicpu(p).opcode = A_LEA) and
               (not assigned(taicpu(p).oper[0]^.ref^.symbol)) and
               (((taicpu(p).oper[0]^.ref^.base = taicpu(p).oper[1]^.reg) and
               (taicpu(p).oper[0]^.ref^.index = NR_NO)) or
               ((taicpu(p).oper[0]^.ref^.index = taicpu(p).oper[1]^.reg) and
               (taicpu(p).oper[0]^.ref^.base = NR_NO))) and
               (taicpu(p).oper[0]^.ref^.offset = 0) then
              begin
                //asml.insertbefore(tai_comment.Create(strpnew('LEA (Ax),Ax removed')), p);
                asml.remove(p);
                p.free;
                p:=next;
                result:=true;
              end;
          end;
      end;
    end;

begin
  casmoptimizer:=TCpuAsmOptimizer;
end.
