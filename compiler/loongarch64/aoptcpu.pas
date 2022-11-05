{
    Copyright (c) 1998-2002 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit implements the LoongArch64 optimizer object

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
  cgbase, cutils,
  aoptobj, aoptcpub, aopt,
  aasmtai, aasmcpu;

type

  TCpuAsmOptimizer = class(TAsmOptimizer)
    function PeepHoleOptPass1Cpu(var p: tai): boolean; override;
    function RegLoadedWithNewValue(reg: tregister; hp: tai): boolean; override;
    function GetNextInstructionUsingReg(Current: tai; Out Next: tai; reg: TRegister): Boolean;
    function OptPass1Mov(var p: tai): Boolean;
  end;

implementation

  function TCpuAsmOptimizer.RegLoadedWithNewValue(reg: tregister; hp: tai): boolean;
    var
      p: taicpu;
    begin
      result:=false;
      if not ((assigned(hp)) and (hp.typ = ait_instruction)) then
        exit;
      p := taicpu(hp);
      if p.ops=0 then
        exit;
      case p.oper[0]^.typ of
        top_reg:
          result:=(SuperRegistersEqual(p.oper[0]^.reg,reg)) and
                  (p.spilling_get_operation_type(0)<>operand_read);
      else
          ;
      end;
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

  function TCpuAsmOptimizer.OptPass1Mov(var p: tai): Boolean;
    var
      hp1: tai;
      alloc, dealloc: tai_regalloc;
    begin
      {
        change
        mov reg0,reg1
        mov reg1,reg0
        into
        mov reg0,reg1
      }
      Result := False;
      while GetNextInstruction(p, hp1) and
        MatchInstruction(hp1, A_MOVE, [taicpu(p).condition]) and
        MatchOperand(taicpu(p).oper[0]^, taicpu(hp1).oper[1]^) and
        MatchOperand(taicpu(p).oper[1]^, taicpu(hp1).oper[0]^) do
        begin
          asml.Remove(hp1);
          hp1.free;
          Result:=true;
        end;
    end;

  function TCpuAsmOptimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;
    begin
      result := false;
      if p.typ=ait_instruction then
          begin
            case taicpu(p).opcode of
            A_MOVE:
                Result:=OptPass1Mov(p);
            else
                ;
            end;
          end;
    end;

begin
  casmoptimizer := TCpuAsmOptimizer;
end.
