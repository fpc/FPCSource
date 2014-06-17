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

  Interface

    uses
      cgbase, cpubase, aoptobj, aoptcpub, aopt, aasmtai;

    Type
      TCpuAsmOptimizer = class(TAsmOptimizer)
        function GetNextInstructionUsingReg(Current: tai;
          var Next: tai; reg: TRegister): Boolean;
        function RegUsedAfterInstruction(reg: Tregister; p: tai;
          var AllUsedRegs: TAllUsedRegs): Boolean;
        function TryRemoveMov(var p: tai; opcode: TAsmOp): boolean;
        function PeepHoleOptPass1Cpu(var p: tai): boolean; override;
      End;

  Implementation

     uses
       globals,aasmbase,aasmcpu,cpuinfo,verbose;


  function MatchInstruction(const instr: tai; const op: TAsmOp): boolean;
    begin
      result :=
        (instr.typ = ait_instruction) and
        (taicpu(instr).opcode = op);
    end;


  function MatchOperand(const oper: TOper; reg: TRegister): boolean;
    begin
      result:=(oper.typ=top_reg) and (oper.reg=reg);
    end;


  function IsSameReg(this,next: taicpu): boolean;
    begin
      result:=(next.oper[0]^.typ=top_reg) and
        (next.oper[1]^.typ=top_reg) and
        (next.oper[0]^.reg=next.oper[1]^.reg) and
        (next.oper[0]^.reg=this.oper[0]^.reg);
    end;


  function regLoadedWithNewValue(reg: tregister; hp: tai): boolean;
    var
      p: taicpu;
    begin
      p:=taicpu(hp);
      result:=false;
      if not ((assigned(hp)) and (hp.typ=ait_instruction)) then
        exit;

      case p.opcode of
        { These instructions do not write into a register at all }
        A_NOP,
        A_C_EQ_D,A_C_EQ_S,A_C_LE_D,A_C_LE_S,A_C_LT_D,A_C_LT_S,
        A_BA,A_BC,
        A_SB,A_SH,A_SW,A_SWL,A_SWR,A_SWC1,A_SDC1:
          exit;
      end;

      result:=(p.ops>0) and (p.oper[0]^.typ=top_reg) and
        (p.oper[0]^.reg=reg);
    end;


  function instructionLoadsFromReg(const reg: TRegister; const hp: tai): boolean;
    var
      p: taicpu;
      i: longint;
    begin
      result:=false;
      if not (assigned(hp) and (hp.typ=ait_instruction)) then
        exit;
      p:=taicpu(hp);

      i:=1;
      while(i<p.ops) do
        begin
          case p.oper[I]^.typ of
            top_reg:
              result:=(p.oper[I]^.reg=reg) and (I<2);
            top_ref:
              result:=
                (p.oper[I]^.ref^.base=reg) or
                (p.oper[I]^.ref^.index=reg);
          end;
          if result then exit; {Bailout if we found something}
          Inc(I);
        end;
    end;


  function TCpuAsmOptimizer.GetNextInstructionUsingReg(Current: tai;
    var Next: tai; reg: TRegister): Boolean;
    begin
      Next:=Current;
      repeat
        Result:=GetNextInstruction(Next,Next);
      until {not(cs_opt_level3 in current_settings.optimizerswitches) or} not(Result) or (Next.typ<>ait_instruction) or (RegInInstruction(reg,Next)) or
        (is_calljmp(taicpu(Next).opcode));
      if Result and is_calljmp(taicpu(next).opcode) then
        begin
          result:=false;
          next:=nil;
        end;
    end;


  function TCpuAsmOptimizer.RegUsedAfterInstruction(reg: Tregister; p: tai;
    var AllUsedRegs: TAllUsedRegs): Boolean;
    begin
      AllUsedRegs[getregtype(reg)].Update(tai(p.Next),true);
      RegUsedAfterInstruction :=
        AllUsedRegs[getregtype(reg)].IsUsed(reg) and
        not(regLoadedWithNewValue(reg,p)) and
        (
          not(GetNextInstruction(p,p)) or
          instructionLoadsFromReg(reg,p) or
          not(regLoadedWithNewValue(reg,p))
        );
    end;


  function TCpuAsmOptimizer.TryRemoveMov(var p: tai; opcode: TAsmOp): boolean;
    var
      next,hp1: tai;
      alloc,dealloc: tai_regalloc;
    begin
      { Fold
          op      $reg1,...
          opcode  $reg2,$reg1
          dealloc $reg1
        into
          op   $reg2,...
        opcode may be A_MOVE, A_MOV_s, A_MOV_d, etc.
      }
      result:=false;
      if (taicpu(p).ops>1) and
         GetNextInstructionUsingReg(p,next,taicpu(p).oper[0]^.reg) and
         MatchInstruction(next,opcode) and
         MatchOperand(taicpu(next).oper[1]^,taicpu(p).oper[0]^.reg) and
         { the destination register of mov cannot be used between p and next }
         (not RegUsedBetween(taicpu(next).oper[0]^.reg,p,next)) then

        begin
          dealloc:=FindRegDealloc(taicpu(p).oper[0]^.reg,tai(next.Next));
          if assigned(dealloc) then
            begin
              { taicpu(p).oper[0]^.reg is not used anymore, try to find its allocation
                and remove it if possible }
              GetLastInstruction(p,hp1);

              asml.Remove(dealloc);
              alloc:=FindRegAlloc(taicpu(p).oper[0]^.reg,tai(hp1.Next));
              if assigned(alloc) then
                begin
                  asml.Remove(alloc);
                  alloc.free;
                  dealloc.free;
                end
              else
                asml.InsertAfter(dealloc,p);

              { try to move the allocation of the target register }
              GetLastInstruction(next,hp1);
              alloc:=FindRegAlloc(taicpu(next).oper[0]^.reg,tai(hp1.Next));
              if assigned(alloc) then
                begin
                  asml.Remove(alloc);
                  asml.InsertBefore(alloc,p);
                  { adjust used regs }
                  IncludeRegInUsedRegs(taicpu(next).oper[0]^.reg,UsedRegs);
                end;

              { finally get rid of the mov }
              taicpu(p).loadreg(0,taicpu(next).oper[0]^.reg);
              asml.remove(next);
              next.free;
            end;
        end;
    end;


  function TCpuAsmOptimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;
    var
      next,next2: tai;
      TmpUsedRegs: TAllUsedRegs;
    begin
      result:=false;
      case p.typ of
        ait_instruction:
          begin
            case taicpu(p).opcode of
              A_SLL:
                begin
                  { if this is a sign extension... }
                  if (taicpu(p).oper[2]^.typ=top_const) and
                    GetNextInstruction(p,next) and
                    MatchInstruction(next,A_SRA) and
                    IsSameReg(taicpu(p),taicpu(next)) and
                    (taicpu(next).oper[2]^.typ=top_const) and
                    (taicpu(next).oper[2]^.val=taicpu(p).oper[2]^.val) and
                    (taicpu(next).oper[2]^.val=16) and
                    { ...followed by 16-bit store (possibly with PIC simplification, etc. in between) }
                    GetNextInstructionUsingReg(next,next2,taicpu(p).oper[0]^.reg) and
                    MatchInstruction(next2,A_SH) and
                    (taicpu(next2).oper[0]^.typ=top_reg) and
                    (taicpu(next2).oper[0]^.reg=taicpu(p).oper[0]^.reg) and
                    { the initial register may not be reused }
                    (not RegUsedBetween(taicpu(p).oper[1]^.reg,next,next2)) then
                    begin
                      CopyUsedRegs(TmpUsedRegs);
                      UpdateUsedRegs(TmpUsedRegs, tai(p.next));
                      UpdateUsedRegs(TmpUsedRegs, tai(next.next));
                      if not RegUsedAfterInstruction(taicpu(p).oper[0]^.reg,next2,TmpUsedRegs) then
                        begin
                          taicpu(next2).loadreg(0,taicpu(p).oper[1]^.reg);
                          asml.remove(p);
                          asml.remove(next);
                          p.free;
                          next.free;
                          p:=next2;
                        end;
                      ReleaseUsedRegs(TmpUsedRegs);
                    end
                  else
                    TryRemoveMov(p,A_MOVE);
                end;

              A_SRL:
                begin
                  { Remove 'andi' in sequences
                      srl   Rx,Ry,16
                      andi  Rx,Rx,65535

                      srl   Rx,Ry,24
                      andi  Rx,Rx,255
                    since 'srl' clears all relevant upper bits }
                  if (taicpu(p).oper[2]^.typ=top_const) and
                    GetNextInstruction(p,next) and
                    MatchInstruction(next,A_ANDI) and
                    IsSameReg(taicpu(p),taicpu(next)) and
                    (taicpu(next).oper[2]^.typ=top_const) and
                    ((
                      (taicpu(p).oper[2]^.val>=16) and
                      (taicpu(next).oper[2]^.val=65535)
                    ) or (
                      (taicpu(p).oper[2]^.val>=24) and
                      (taicpu(next).oper[2]^.val=255)
                    )) then
                    begin
                      asml.remove(next);
                      next.free;
                    end
                  else
                    TryRemoveMov(p,A_MOVE);
                end;

              A_ANDI:
                begin
                  { Remove sign extension after 'andi' if bit 7/15 of const operand is clear }
                  if (taicpu(p).oper[2]^.typ=top_const) and
                    GetNextInstruction(p,next) and
                    MatchInstruction(next,A_SLL) and
                    GetNextInstruction(next,next2) and
                    MatchInstruction(next2,A_SRA) and
                    IsSameReg(taicpu(p),taicpu(next)) and
                    IsSameReg(taicpu(p),taicpu(next2)) and
                    (taicpu(next).oper[2]^.typ=top_const) and
                    (taicpu(next2).oper[2]^.typ=top_const) and
                    (taicpu(next).oper[2]^.val=taicpu(next2).oper[2]^.val) and
                    ((
                      (taicpu(p).oper[2]^.val<=$7fff) and
                      (taicpu(next).oper[2]^.val=16)
                    ) or (
                      (taicpu(p).oper[2]^.val<=$7f) and
                      (taicpu(next).oper[2]^.val=24)
                    )) then
                    begin
                      asml.remove(next);
                      asml.remove(next2);
                      next.free;
                      next2.free;
                    end
                  { Remove zero extension if register is used only for byte/word memory store }
                  else if (taicpu(p).oper[2]^.typ=top_const) and
                    GetNextInstruction(p,next) and
                    ((taicpu(p).oper[2]^.val=255) and MatchInstruction(next,A_SB)) or
                    ((taicpu(p).oper[2]^.val=65535) and MatchInstruction(next,A_SH)) and
                    (taicpu(next).oper[0]^.typ=top_reg) and
                    (taicpu(next).oper[0]^.reg=taicpu(p).oper[0]^.reg) then
                    begin
                      CopyUsedRegs(TmpUsedRegs);
                      UpdateUsedRegs(TmpUsedRegs, tai(p.next));
                      if not RegUsedAfterInstruction(taicpu(p).oper[0]^.reg,next,TmpUsedRegs) then
                        begin
                          taicpu(next).loadreg(0,taicpu(p).oper[1]^.reg);
                          asml.remove(p);
                          p.free;
                          p:=next;
                        end;
                      ReleaseUsedRegs(TmpUsedRegs);
                    end
                  else
                    TryRemoveMov(p,A_MOVE);
                end;

              A_ADD,A_ADDU,
              A_ADDI,A_ADDIU,
              A_SUB,A_SUBU,
              A_SRA,A_SRAV,
              A_SRLV,
              A_SLLV,
              A_AND,A_OR,A_XOR,A_ORI,A_XORI:
                TryRemoveMov(p,A_MOVE);

              A_ADD_s, A_SUB_s, A_MUL_s, A_DIV_s,
              A_ABS_s, A_NEG_s, A_SQRT_s,
              A_CVT_s_w, A_CVT_s_l, A_CVT_s_d:
                TryRemoveMov(p,A_MOV_s);

              A_ADD_d, A_SUB_d, A_MUL_d, A_DIV_d,
              A_ABS_d, A_NEG_d, A_SQRT_d,
              A_CVT_d_w, A_CVT_d_l, A_CVT_d_s:
                TryRemoveMov(p,A_MOV_d);
            end;
          end;
      end;
    end;

begin
  casmoptimizer:=TCpuAsmOptimizer;
end.
