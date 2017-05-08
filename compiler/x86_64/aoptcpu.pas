{
    Copyright (c) 1998-2004 by Jonas Maebe

    This unit calls the optimization procedures to optimize the assembler
    code for sparc

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

interface

uses cgbase, cpubase, aasmtai, aopt, aoptx86, aoptcpub;

type
  TCpuAsmOptimizer = class(TX86AsmOptimizer)
    function PrePeepHoleOptsCpu(var p: tai): boolean; override;
    function PeepHoleOptPass1Cpu(var p: tai): boolean; override;
    function PeepHoleOptPass2Cpu(var p: tai): boolean; override;
    function PostPeepHoleOptsCpu(var p : tai) : boolean; override;
  end;

implementation

uses
  globtype, globals,
  cutils,
  verbose,
  cgutils,
  aoptobj,aoptutils,
  aasmbase, aasmdata, aasmcpu,
  itcpugas;

    function TCpuAsmOptimizer.PrePeepHoleOptsCpu(var p : tai) : boolean;
      begin
        result := false;
        case p.typ of
          ait_instruction:
            begin
              case taicpu(p).opcode of
                A_SAR,A_SHR:
                  result:=PrePeepholeOptSxx(p);
              end;
            end;
        end;
      end;


    function TCpuAsmOptimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;
      begin
        result:=False;
        case p.typ of
          ait_instruction:
            begin
            case taicpu(p).opcode of
              A_AND:
                Result:=OptPass1AND(p);
              A_MOV:
                Result:=OptPass1MOV(p);
              A_MOVSX,
              A_MOVZX:
                Result:=OptPass1Movx(p);
              A_VMOVAPS,
              A_VMOVAPD:
                result:=OptPass1VMOVAP(p);
              A_VDIVSD,
              A_VDIVSS,
              A_VSUBSD,
              A_VSUBSS,
              A_VMULSD,
              A_VMULSS,
              A_VADDSD,
              A_VADDSS:
                result:=OptPass1VOP(p);
            end;
          end;
        end;
      end;


    function TCpuAsmOptimizer.PeepHoleOptPass2Cpu(var p : tai) : boolean;
      begin
        Result := False;
        case p.typ of
          ait_instruction:
            begin
              case taicpu(p).opcode of
                A_MOV:
                  Result:=OptPass2MOV(p);
                A_IMUL:
                  Result:=OptPass2Imul(p);
                A_JMP:
                  Result:=OptPass2Jmp(p);
                A_Jcc:
                  Result:=OptPass2Jcc(p);
              end;
            end;
        end;
      end;


    function TCpuAsmOptimizer.PostPeepHoleOptsCpu(var p: tai): boolean;
      begin
        result := false;
        case p.typ of
          ait_instruction:
            begin
              case taicpu(p).opcode of
                A_MOV:
                  PostPeepholeOptMov(p);
              end;
            end;
        end;
      end;

begin
  casmoptimizer := TCpuAsmOptimizer;
end.

