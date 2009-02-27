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
    verbose,
    aasmbase,aasmcpu;

  function CanBeCond(p : tai) : boolean;
    begin
      result:=(p.typ=ait_instruction) and (taicpu(p).condition=C_None);
    end;


  function TCpuAsmOptimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;
    var
      next1: tai;
      hp1: tai;
    begin
      result := false;
      case p.typ of
        ait_instruction:
          begin
            case taicpu(p).opcode of
              A_MOV:
                begin
                  { fold
                    mov reg1,reg0, shift imm1
                    mov reg1,reg1, shift imm2
                    to
                    mov reg1,reg0, shift imm1+imm2
                  }
                  if (taicpu(p).ops=3) and
                     (taicpu(p).oper[0]^.typ = top_reg) and
                     (taicpu(p).oper[2]^.typ = top_shifterop) and
                     (taicpu(p).oper[2]^.shifterop^.rs = NR_NO) and
                     getnextinstruction(p,next1) and
                     (next1.typ = ait_instruction) and
                     (taicpu(next1).opcode = A_MOV) and
                     (taicpu(p).condition=taicpu(next1).condition) and
                     (taicpu(next1).ops=3) and
                     (taicpu(next1).oper[0]^.typ = top_reg) and
                     (taicpu(p).oper[0]^.reg=taicpu(next1).oper[0]^.reg) and
                     (taicpu(next1).oper[1]^.typ = top_reg) and
                     (taicpu(p).oper[0]^.reg=taicpu(next1).oper[1]^.reg) and
                     (taicpu(next1).oper[2]^.typ = top_shifterop) and
                     (taicpu(next1).oper[2]^.shifterop^.rs = NR_NO) and
                     (taicpu(p).oper[2]^.shifterop^.shiftmode=taicpu(next1).oper[2]^.shifterop^.shiftmode) then
                    begin
                      inc(taicpu(p).oper[2]^.shifterop^.shiftimm,taicpu(next1).oper[2]^.shifterop^.shiftimm);
                      { avoid overflows }
                      if taicpu(p).oper[2]^.shifterop^.shiftimm>31 then
                        case taicpu(p).oper[2]^.shifterop^.shiftmode of
                          SM_ROR:
                            taicpu(p).oper[2]^.shifterop^.shiftimm:=taicpu(p).oper[2]^.shifterop^.shiftimm and 31;
                          SM_ASR:
                            taicpu(p).oper[2]^.shifterop^.shiftimm:=31;
                          SM_LSR,
                          SM_LSL:
                            begin
                              hp1:=taicpu.op_reg_const(A_MOV,taicpu(p).oper[0]^.reg,0);
                              InsertLLItem(p.previous, p.next, hp1);
                              p.free;
                              p:=hp1;
                            end;
                          else
                            internalerror(2008072803);
                        end;
                      asml.remove(next1);
                      next1.free;
                      result := true;
                    end;
                end;
              A_AND:
                begin
                  {
                    change
                    and reg2,reg1,const1
                    and reg2,reg2,const2
                    to
                    and reg2,reg1,(const1 and const2)
                  }
                  if (taicpu(p).oper[0]^.typ = top_reg) and
                     (taicpu(p).oper[1]^.typ = top_reg) and
                     (taicpu(p).oper[2]^.typ = top_const) and
                     GetNextInstruction(p, hp1) and
                     (tai(hp1).typ = ait_instruction) and
                     (taicpu(hp1).opcode = A_AND) and
                     (taicpu(p).condition=taicpu(hp1).condition) and
                     (taicpu(p).oppostfix=PF_None) and
                     (taicpu(hp1).oper[0]^.typ = top_reg) and
                     (taicpu(hp1).oper[1]^.typ = top_reg) and
                     (taicpu(hp1).oper[2]^.typ = top_const) and
                     (taicpu(p).oper[0]^.reg = taicpu(hp1).oper[0]^.reg) and
                     (taicpu(hp1).oper[0]^.reg = taicpu(hp1).oper[1]^.reg) then
                    begin
                      taicpu(p).loadConst(2,taicpu(p).oper[2]^.val and taicpu(hp1).oper[2]^.val);
                      taicpu(p).oppostfix:=taicpu(hp1).oppostfix;
                      asml.remove(hp1);
                      hp1.free;
                    end;
                end;
            end;
          end;
      end;
    end;


  { instructions modifying the CPSR can be only the last instruction }
  function MustBeLast(p : tai) : boolean;
    begin
      Result:=(p.typ=ait_instruction) and
        ((taicpu(p).opcode in [A_BL,A_BLX,A_CMP,A_CMN,A_SWI,A_TEQ,A_TST,A_CMF,A_CMFE {,A_MSR}]) or
         ((taicpu(p).ops>=1) and (taicpu(p).oper[0]^.typ=top_reg) and (taicpu(p).oper[0]^.reg=NR_PC)) or
         (taicpu(p).oppostfix=PF_S));
    end;


  procedure TCpuAsmOptimizer.PeepHoleOptPass2;
    var
      p,hp1,hp2: tai;
      l : longint;
      condition : tasmcond;
      hp3: tai;
      WasLast: boolean;
      { UsedRegs, TmpUsedRegs: TRegSet; }

    begin
      p := BlockStart;
      { UsedRegs := []; }
      while (p <> BlockEnd) Do
        begin
          { UpdateUsedRegs(UsedRegs, tai(p.next)); }
          case p.Typ Of
            Ait_Instruction:
              begin
                case taicpu(p).opcode Of
                  A_B:
                    if taicpu(p).condition<>C_None then
                      begin
                         { check for
                                Bxx   xxx
                                <several instructions>
                             xxx:
                         }
                         l:=0;
                         WasLast:=False;
                         GetNextInstruction(p, hp1);
                         while assigned(hp1) and
                           (l<=4) and
                           CanBeCond(hp1) and
                           { stop on labels }
                           not(hp1.typ=ait_label) do
                           begin
                              inc(l);
                              if MustBeLast(hp1) then
                                begin
                                  WasLast:=True;
                                  GetNextInstruction(hp1,hp1);
                                  break;
                                end
                              else
                                GetNextInstruction(hp1,hp1);
                           end;
                         if assigned(hp1) then
                           begin
                              if FindLabel(tasmlabel(taicpu(p).oper[0]^.ref^.symbol),hp1) then
                                begin
                                  if (l<=4) and (l>0) then
                                    begin
                                      condition:=inverse_cond(taicpu(p).condition);
                                      hp2:=p;
                                      GetNextInstruction(p,hp1);
                                      p:=hp1;
                                      repeat
                                        if hp1.typ=ait_instruction then
                                          taicpu(hp1).condition:=condition;
                                        if MustBeLast(hp1) then
                                          begin
                                            GetNextInstruction(hp1,hp1);
                                            break;
                                          end
                                        else
                                          GetNextInstruction(hp1,hp1);
                                      until not(assigned(hp1)) or
                                        not(CanBeCond(hp1)) or
                                        (hp1.typ=ait_label);
                                      { wait with removing else GetNextInstruction could
                                        ignore the label if it was the only usage in the
                                        jump moved away }
                                      tasmlabel(taicpu(hp2).oper[0]^.ref^.symbol).decrefs;
                                      asml.remove(hp2);
                                      hp2.free;
                                      continue;
                                    end;
                                end
                              else
                                { do not perform further optimizations if there is inctructon
                                  in block #1 which can not be optimized.
                                 }
                                if not WasLast then
                                begin
                                   { check further for
                                          Bcc   xxx
                                          <several instructions 1>
                                          B   yyy
                                  xxx:
                                          <several instructions 2>
                                  yyy:
                                   }
                                  { hp2 points to jmp yyy }
                                  hp2:=hp1;
                                  { skip hp1 to xxx }
                                  GetNextInstruction(hp1, hp1);
                                  if assigned(hp2) and
                                    assigned(hp1) and
                                    (l<=3) and
                                    (hp2.typ=ait_instruction) and
                                    (taicpu(hp2).is_jmp) and
                                    (taicpu(hp2).condition=C_None) and
                                    { real label and jump, no further references to the
                                      label are allowed }
                                    (tasmlabel(taicpu(p).oper[0]^.ref^.symbol).getrefs=2) and
                                    FindLabel(tasmlabel(taicpu(p).oper[0]^.ref^.symbol),hp1) then
                                     begin
                                       l:=0;
                                       { skip hp1 to <several moves 2> }
                                       GetNextInstruction(hp1, hp1);
                                       while assigned(hp1) and
                                         CanBeCond(hp1) do
                                         begin
                                           inc(l);
                                           GetNextInstruction(hp1, hp1);
                                         end;
                                       { hp1 points to yyy: }
                                       if assigned(hp1) and
                                         FindLabel(tasmlabel(taicpu(hp2).oper[0]^.ref^.symbol),hp1) then
                                         begin
                                            condition:=inverse_cond(taicpu(p).condition);
                                            GetNextInstruction(p,hp1);
                                            hp3:=p;
                                            p:=hp1;
                                            repeat
                                              if hp1.typ=ait_instruction then
                                                taicpu(hp1).condition:=condition;
                                              GetNextInstruction(hp1,hp1);
                                            until not(assigned(hp1)) or
                                              not(CanBeCond(hp1));
                                            { hp2 is still at jmp yyy }
                                            GetNextInstruction(hp2,hp1);
                                            { hp2 is now at xxx: }
                                            condition:=inverse_cond(condition);
                                            GetNextInstruction(hp1,hp1);
                                            { hp1 is now at <several movs 2> }
                                            repeat
                                              taicpu(hp1).condition:=condition;
                                              GetNextInstruction(hp1,hp1);
                                            until not(assigned(hp1)) or
                                              not(CanBeCond(hp1)) or
                                              (hp1.typ=ait_label);
                                            {
                                            asml.remove(hp1.next)
                                            hp1.next.free;
                                            asml.remove(hp1);
                                            hp1.free;
                                            }
                                            { remove Bcc }
                                            tasmlabel(taicpu(hp3).oper[0]^.ref^.symbol).decrefs;
                                            asml.remove(hp3);
                                            hp3.free;
                                            { remove jmp }
                                            tasmlabel(taicpu(hp2).oper[0]^.ref^.symbol).decrefs;
                                            asml.remove(hp2);
                                            hp2.free;
                                            continue;
                                         end;
                                     end;
                                end;
                           end;
                      end;
                end;
              end;
          end;
          p := tai(p.next)
        end;
    end;

begin
  casmoptimizer:=TCpuAsmOptimizer;
End.
