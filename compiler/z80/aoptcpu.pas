{
    Copyright (c) 1998-2002 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit implements the Z80 optimizer object

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

{$define DEBUG_AOPTCPU}

Interface

uses cpubase, cgbase, aasmtai, aopt,AoptObj, aoptcpub;

Type
  TCpuAsmOptimizer = class(TAsmOptimizer)
    { outputs a debug message into the assembler file }
    procedure DebugMsg(const s: string; p: tai);

    Function GetNextInstructionUsingReg(Current: tai; Var Next: tai;reg : TRegister): Boolean;
    function RegLoadedWithNewValue(reg : tregister; hp : tai) : boolean; override;
    function InstructionLoadsFromReg(const reg : TRegister; const hp : tai) : boolean; override;

    { uses the same constructor as TAopObj }
    function PeepHoleOptPass1Cpu(var p: tai): boolean; override;
    procedure PeepHoleOptPass2;override;
  End;

Implementation

  uses
    cutils,
    verbose,
    cpuinfo,
    aasmbase,aasmcpu,aasmdata,
    globals,globtype,
    cgutils;

  type
    TAsmOpSet = set of TAsmOp;

  function CanBeCond(p : tai) : boolean;
    begin
      result:=(p.typ=ait_instruction) and (taicpu(p).condition=C_None);
    end;


  function RefsEqual(const r1, r2: treference): boolean;
    begin
      refsequal :=
        (r1.offset = r2.offset) and
        (r1.base = r2.base) and
        (r1.index = r2.index) and (r1.scalefactor = r2.scalefactor) and
        (r1.symbol=r2.symbol) and (r1.refaddr = r2.refaddr) and
        (r1.relsymbol = r2.relsymbol);
    end;


  function MatchOperand(const oper1: TOper; const oper2: TOper): boolean; inline;
    begin
      result:=oper1.typ=oper2.typ;

      if result then
        case oper1.typ of
          top_const:
            Result:=oper1.val = oper2.val;
          top_reg:
            Result:=oper1.reg = oper2.reg;
          top_ref:
            Result:=RefsEqual(oper1.ref^, oper2.ref^);
          else Result:=false;
        end
    end;


  function MatchOperand(const oper: TOper; const reg: TRegister): boolean; inline;
    begin
      result := (oper.typ = top_reg) and (oper.reg = reg);
    end;


  function MatchInstruction(const instr: tai; const op: TAsmOp): boolean;
    begin
      result :=
        (instr.typ = ait_instruction) and
        (taicpu(instr).opcode = op);
    end;


  function MatchInstruction(const instr: tai; const ops: TAsmOpSet): boolean;
    begin
      result :=
        (instr.typ = ait_instruction) and
        (taicpu(instr).opcode in ops);
    end;


  function MatchInstruction(const instr: tai; const ops: TAsmOpSet;opcount : byte): boolean;
    begin
      result :=
        (instr.typ = ait_instruction) and
        (taicpu(instr).opcode in ops) and
        (taicpu(instr).ops=opcount);
    end;


  function MatchOpType(const instr : tai;ot0,ot1 : toptype) : Boolean;
    begin
      Result:=(taicpu(instr).ops=2) and
        (taicpu(instr).oper[0]^.typ=ot0) and
        (taicpu(instr).oper[1]^.typ=ot1);
    end;

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


  function TCpuAsmOptimizer.GetNextInstructionUsingReg(Current: tai;
    var Next: tai; reg: TRegister): Boolean;
    begin
      Next:=Current;
      repeat
        Result:=GetNextInstruction(Next,Next);
      until not(cs_opt_level3 in current_settings.optimizerswitches) or not(Result) or (Next.typ<>ait_instruction) or (RegInInstruction(reg,Next)) or
        (is_calljmp(taicpu(Next).opcode));
    end;


  function TCpuAsmOptimizer.RegLoadedWithNewValue(reg: tregister; hp: tai): boolean;
    var
      p: taicpu;
    begin
      if not assigned(hp) or
         (hp.typ <> ait_instruction) then
       begin
         Result := false;
         exit;
       end;
      internalerror(2017032606);
      //p := taicpu(hp);
      //Result := ((p.opcode in [A_LDI,A_MOV,A_LDS]) and (reg=p.oper[0]^.reg) and ((p.oper[1]^.typ<>top_reg) or (reg<>p.oper[0]^.reg))) or
      //  ((p.opcode in [A_LD,A_LDD,A_LPM]) and (reg=p.oper[0]^.reg) and not(RegInRef(reg,p.oper[1]^.ref^))) or
      //  ((p.opcode in [A_MOVW]) and ((reg=p.oper[0]^.reg) or (TRegister(ord(reg)+1)=p.oper[0]^.reg)) and not(reg=p.oper[1]^.reg) and not(TRegister(ord(reg)+1)=p.oper[1]^.reg)) or
      //  ((p.opcode in [A_POP]) and (reg=p.oper[0]^.reg));
    end;


  function TCpuAsmOptimizer.InstructionLoadsFromReg(const reg: TRegister; const hp: tai): boolean;
    var
      p: taicpu;
      i: longint;
    begin
      Result := false;

      internalerror(2017032607);

      //if not (assigned(hp) and (hp.typ = ait_instruction)) then
      //  exit;
      //p:=taicpu(hp);
      //
      //i:=0;
      //
      //{ we do not care about the stack pointer }
      //if p.opcode in [A_POP] then
      //  exit;
      //
      //{ first operand only written?
      //  then skip it }
      //if p.opcode in [A_MOV,A_LD,A_LDD,A_LDS,A_LPM,A_LDI,A_MOVW] then
      //  i:=1;
      //
      //while(i<p.ops) do
      //  begin
      //    case p.oper[I]^.typ of
      //      top_reg:
      //        Result := (p.oper[I]^.reg = reg) or
      //          { MOVW }
      //          ((i=1) and (p.opcode=A_MOVW) and (getsupreg(p.oper[0]^.reg)+1=getsupreg(reg)));
      //      top_ref:
      //        Result :=
      //          (p.oper[I]^.ref^.base = reg) or
      //          (p.oper[I]^.ref^.index = reg);
      //    end;
      //    { Bailout if we found something }
      //    if Result then
      //      exit;
      //    Inc(I);
      //  end;
    end;

  function TCpuAsmOptimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;
    var
      hp1,hp2,hp3,hp4,hp5: tai;
      alloc, dealloc: tai_regalloc;
      i: integer;
      l: TAsmLabel;
      //TmpUsedRegs : TAllUsedRegs;
    begin
      result := false;
      //case p.typ of
      //  ait_instruction:
      //    begin
      //    end;
      //end;
    end;


  procedure TCpuAsmOptimizer.PeepHoleOptPass2;
    begin
    end;

begin
  casmoptimizer:=TCpuAsmOptimizer;
End.
