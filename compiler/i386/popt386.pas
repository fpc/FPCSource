{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl and Jonas Maebe

    This unit contains the peephole optimizer.

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
Unit POpt386;

{$i fpcdefs.inc}

Interface

Uses Aasmbase,aasmtai,aasmcpu;

Procedure PrePeepHoleOpts(AsmL: TAAsmOutput; BlockStart, BlockEnd: Tai);
Procedure PeepHoleOptPass1(AsmL: TAAsmOutput; BlockStart, BlockEnd: Tai);
Procedure PeepHoleOptPass2(AsmL: TAAsmOutput; BlockStart, BlockEnd: Tai);
Procedure PostPeepHoleOpts(AsmL: TAAsmOutput; BlockStart, BlockEnd: Tai);

Implementation

Uses
  globtype,systems,
  globals,cgbase,
{$ifdef finaldestdebug}
  cobjects,
{$endif finaldestdebug}
  cpuinfo,cpubase,DAOpt386,cginfo,rgobj;

Function RegUsedAfterInstruction(Reg: TRegister; p: Tai; Var UsedRegs: TRegSet): Boolean;
Begin
  reg := reg32(reg);
  UpdateUsedRegs(UsedRegs, Tai(p.Next));
  RegUsedAfterInstruction :=
    (Reg in UsedRegs) and
    (not(getNextInstruction(p,p)) or
     not(regLoadedWithNewValue(reg,false,p)));
End;

function doFpuLoadStoreOpt(asmL: TAAsmoutput; var p: Tai): boolean;
{ returns true if a "continue" should be done after this optimization }
var hp1, hp2: Tai;
begin
  doFpuLoadStoreOpt := false;
  if (Taicpu(p).oper[0].typ = top_ref) and
     getNextInstruction(p, hp1) and
     (hp1.typ = ait_instruction) and
     (((Taicpu(hp1).opcode = A_FLD) and
       (Taicpu(p).opcode = A_FSTP)) or
      ((Taicpu(p).opcode = A_FISTP) and
       (Taicpu(hp1).opcode = A_FILD))) and
     (Taicpu(hp1).oper[0].typ = top_ref) and
     (Taicpu(hp1).opsize = Taicpu(p).opsize) and
     refsEqual(Taicpu(p).oper[0].ref^, Taicpu(hp1).oper[0].ref^) then
    begin
      if getNextInstruction(hp1, hp2) and
         (hp2.typ = ait_instruction) and
         ((Taicpu(hp2).opcode = A_LEAVE) or
          (Taicpu(hp2).opcode = A_RET)) and
         (Taicpu(p).oper[0].ref^.Base = procinfo^.FramePointer) and
         (Taicpu(p).oper[0].ref^.Offset >= procinfo^.Return_Offset) and
         (Taicpu(p).oper[0].ref^.Index = R_NO) then
        begin
          asml.remove(p);
          asml.remove(hp1);
          p.free;
          hp1.free;
          p := hp2;
          removeLastDeallocForFuncRes(asmL, p);
          doFPULoadStoreOpt := true;
        end
      else
        { fst can't store an extended value! }
        if (Taicpu(p).opsize <> S_FX) and
           (Taicpu(p).opsize <> S_IQ) then
          begin
            if (Taicpu(p).opcode = A_FSTP) then
              Taicpu(p).opcode := A_FST
            else Taicpu(p).opcode := A_FIST;
            asml.remove(hp1);
            hp1.free;
          end
    end;
end;

Procedure PrePeepHoleOpts(AsmL: TAAsmOutput; BlockStart, BlockEnd: Tai);
var
  p,hp1: Tai;
  l: Aword;
  tmpRef: treference;
Begin
  P := BlockStart;
  While (P <> BlockEnd) Do
    Begin
      Case p.Typ Of
        Ait_Instruction:
          Begin
            Case Taicpu(p).opcode Of
              A_IMUL:
                {changes certain "imul const, %reg"'s to lea sequences}
                Begin
                  If (Taicpu(p).oper[0].typ = Top_Const) And
                     (Taicpu(p).oper[1].typ = Top_Reg) And
                     (Taicpu(p).opsize = S_L) Then
                    If (Taicpu(p).oper[0].val = 1) Then
                      If (Taicpu(p).oper[2].typ = Top_None) Then
                       {remove "imul $1, reg"}
                        Begin
                          hp1 := Tai(p.Next);
                          asml.Remove(p);
                          p.free;
                          p := hp1;
                          Continue;
                        End
                      Else
                       {change "imul $1, reg1, reg2" to "mov reg1, reg2"}
                        Begin
                          hp1 := Taicpu.Op_Reg_Reg(A_MOV, S_L, Taicpu(p).oper[1].reg,Taicpu(p).oper[2].reg);
                          InsertLLItem(AsmL, p.previous, p.next, hp1);
                          p.free;
                          p := hp1;
                        End
                    Else If
                     ((Taicpu(p).oper[2].typ = Top_Reg) or
                      (Taicpu(p).oper[2].typ = Top_None)) And
                     (aktoptprocessor < ClassP6) And
                     (Taicpu(p).oper[0].val <= 12) And
                     Not(CS_LittleSize in aktglobalswitches) And
                     (Not(GetNextInstruction(p, hp1)) Or
                       {GetNextInstruction(p, hp1) And}
                       Not((Tai(hp1).typ = ait_instruction) And
                           ((Taicpu(hp1).opcode=A_Jcc) and
                            (Taicpu(hp1).condition in [C_O,C_NO]))))
                    Then
                      Begin
                        reference_reset(tmpref);
                        Case Taicpu(p).oper[0].val Of
                          3: Begin
                             {imul 3, reg1, reg2 to
                                lea (reg1,reg1,2), reg2
                              imul 3, reg1 to
                                lea (reg1,reg1,2), reg1}
                               TmpRef.base := Taicpu(p).oper[1].reg;
                               TmpRef.Index := Taicpu(p).oper[1].reg;
                               TmpRef.ScaleFactor := 2;
                               If (Taicpu(p).oper[2].typ = Top_None) Then
                                 hp1 := Taicpu.op_ref_reg(A_LEA, S_L, TmpRef, Taicpu(p).oper[1].reg)
                               Else
                                 hp1 := Taicpu.op_ref_reg(A_LEA, S_L, TmpRef, Taicpu(p).oper[2].reg);
                               InsertLLItem(AsmL,p.previous, p.next, hp1);
                               p.free;
                               p := hp1;
                            End;
                         5: Begin
                            {imul 5, reg1, reg2 to
                               lea (reg1,reg1,4), reg2
                             imul 5, reg1 to
                               lea (reg1,reg1,4), reg1}
                              TmpRef.base := Taicpu(p).oper[1].reg;
                              TmpRef.Index := Taicpu(p).oper[1].reg;
                              TmpRef.ScaleFactor := 4;
                              If (Taicpu(p).oper[2].typ = Top_None) Then
                                hp1 := Taicpu.op_ref_reg(A_LEA, S_L, TmpRef, Taicpu(p).oper[1].reg)
                              Else
                                hp1 := Taicpu.op_ref_reg(A_LEA, S_L, TmpRef, Taicpu(p).oper[2].reg);
                              InsertLLItem(AsmL,p.previous, p.next, hp1);
                              p.free;
                              p := hp1;
                            End;
                         6: Begin
                            {imul 6, reg1, reg2 to
                               lea (,reg1,2), reg2
                               lea (reg2,reg1,4), reg2
                             imul 6, reg1 to
                               lea (reg1,reg1,2), reg1
                               add reg1, reg1}
                              If (aktoptprocessor <= Class386)
                                Then
                                  Begin
                                    TmpRef.Index := Taicpu(p).oper[1].reg;
                                    If (Taicpu(p).oper[2].typ = Top_Reg)
                                      Then
                                        Begin
                                          TmpRef.base := Taicpu(p).oper[2].reg;
                                          TmpRef.ScaleFactor := 4;
                                          hp1 :=  Taicpu.op_ref_reg(A_LEA, S_L, TmpRef, Taicpu(p).oper[1].reg);
                                        End
                                      Else
                                        Begin
                                          hp1 :=  Taicpu.op_reg_reg(A_ADD, S_L,
                                            Taicpu(p).oper[1].reg,Taicpu(p).oper[1].reg);
                                        End;
                                    InsertLLItem(AsmL,p, p.next, hp1);
                                    reference_reset(tmpref);
                                    TmpRef.Index := Taicpu(p).oper[1].reg;
                                    TmpRef.ScaleFactor := 2;
                                    If (Taicpu(p).oper[2].typ = Top_Reg)
                                      Then
                                        Begin
                                          TmpRef.base := R_NO;
                                          hp1 :=  Taicpu.op_ref_reg(A_LEA, S_L, TmpRef,
                                            Taicpu(p).oper[2].reg);
                                        End
                                      Else
                                        Begin
                                          TmpRef.base := Taicpu(p).oper[1].reg;
                                          hp1 := Taicpu.op_ref_reg(A_LEA, S_L, TmpRef, Taicpu(p).oper[1].reg);
                                        End;
                                    InsertLLItem(AsmL,p.previous, p.next, hp1);
                                    p.free;
                                    p := Tai(hp1.next);
                                  End
                            End;
                          9: Begin
                             {imul 9, reg1, reg2 to
                                lea (reg1,reg1,8), reg2
                              imul 9, reg1 to
                                lea (reg1,reg1,8), reg1}
                               TmpRef.base := Taicpu(p).oper[1].reg;
                               TmpRef.Index := Taicpu(p).oper[1].reg;
                               TmpRef.ScaleFactor := 8;
                               If (Taicpu(p).oper[2].typ = Top_None) Then
                                 hp1 := Taicpu.op_ref_reg(A_LEA, S_L, TmpRef, Taicpu(p).oper[1].reg)
                               Else
                                 hp1 := Taicpu.op_ref_reg(A_LEA, S_L, TmpRef, Taicpu(p).oper[2].reg);
                               InsertLLItem(AsmL,p.previous, p.next, hp1);
                               p.free;
                               p := hp1;
                             End;
                         10: Begin
                            {imul 10, reg1, reg2 to
                               lea (reg1,reg1,4), reg2
                               add reg2, reg2
                             imul 10, reg1 to
                               lea (reg1,reg1,4), reg1
                               add reg1, reg1}
                               If (aktoptprocessor <= Class386) Then
                                 Begin
                                   If (Taicpu(p).oper[2].typ = Top_Reg) Then
                                     hp1 :=  Taicpu.op_reg_reg(A_ADD, S_L,
                                       Taicpu(p).oper[2].reg,Taicpu(p).oper[2].reg)
                                   Else
                                     hp1 := Taicpu.op_reg_reg(A_ADD, S_L,
                                       Taicpu(p).oper[1].reg,Taicpu(p).oper[1].reg);
                                   InsertLLItem(AsmL,p, p.next, hp1);
                                   TmpRef.base := Taicpu(p).oper[1].reg;
                                   TmpRef.Index := Taicpu(p).oper[1].reg;
                                   TmpRef.ScaleFactor := 4;
                                   If (Taicpu(p).oper[2].typ = Top_Reg)
                                     Then
                                       hp1 :=  Taicpu.op_ref_reg(A_LEA, S_L, TmpRef, Taicpu(p).oper[2].reg)
                                     Else
                                       hp1 :=  Taicpu.op_ref_reg(A_LEA, S_L, TmpRef, Taicpu(p).oper[1].reg);
                                   InsertLLItem(AsmL,p.previous, p.next, hp1);
                                   p.free;
                                   p := Tai(hp1.next);
                                 End
                             End;
                         12: Begin
                            {imul 12, reg1, reg2 to
                               lea (,reg1,4), reg2
                               lea (,reg1,8) reg2
                             imul 12, reg1 to
                               lea (reg1,reg1,2), reg1
                               lea (,reg1,4), reg1}
                               If (aktoptprocessor <= Class386)
                                 Then
                                   Begin
                                     TmpRef.Index := Taicpu(p).oper[1].reg;
                                     If (Taicpu(p).oper[2].typ = Top_Reg) Then
                                       Begin
                                         TmpRef.base := Taicpu(p).oper[2].reg;
                                         TmpRef.ScaleFactor := 8;
                                         hp1 :=  Taicpu.op_ref_reg(A_LEA, S_L, TmpRef, Taicpu(p).oper[2].reg);
                                       End
                                     Else
                                       Begin
                                         TmpRef.base := R_NO;
                                         TmpRef.ScaleFactor := 4;
                                         hp1 :=  Taicpu.op_ref_reg(A_LEA, S_L, TmpRef, Taicpu(p).oper[1].reg);
                                       End;
                                     InsertLLItem(AsmL,p, p.next, hp1);
                                     reference_reset(tmpref);
                                     TmpRef.Index := Taicpu(p).oper[1].reg;
                                     If (Taicpu(p).oper[2].typ = Top_Reg) Then
                                       Begin
                                         TmpRef.base := R_NO;
                                         TmpRef.ScaleFactor := 4;
                                         hp1 :=  Taicpu.op_ref_reg(A_LEA, S_L, TmpRef, Taicpu(p).oper[2].reg);
                                       End
                                     Else
                                       Begin
                                         TmpRef.base := Taicpu(p).oper[1].reg;
                                         TmpRef.ScaleFactor := 2;
                                         hp1 :=  Taicpu.op_ref_reg(A_LEA, S_L, TmpRef, Taicpu(p).oper[1].reg);
                                       End;
                                     InsertLLItem(AsmL,p.previous, p.next, hp1);
                                     p.free;
                                     p := Tai(hp1.next);
                                   End
                             End
                        End;
                      End;
                End;
              A_SAR, A_SHR:
                  {changes the code sequence
                   shr/sar const1, x
                   shl     const2, x
                   to either "sar/and", "shl/and" or just "and" depending on const1 and const2}
                Begin
                  If GetNextInstruction(p, hp1) And
                     (Tai(hp1).typ = ait_instruction) and
                     (Taicpu(hp1).opcode = A_SHL) and
                     (Taicpu(p).oper[0].typ = top_const) and
                     (Taicpu(hp1).oper[0].typ = top_const) and
                     (Taicpu(hp1).opsize = Taicpu(p).opsize) And
                     (Taicpu(hp1).oper[1].typ = Taicpu(p).oper[1].typ) And
                     OpsEqual(Taicpu(hp1).oper[1], Taicpu(p).oper[1])
                    Then
                      If (Taicpu(p).oper[0].val > Taicpu(hp1).oper[0].val) And
                         Not(CS_LittleSize In aktglobalswitches)
                        Then
                   { shr/sar const1, %reg
                     shl     const2, %reg
                      with const1 > const2 }
                          Begin
                            Taicpu(p).LoadConst(0,Taicpu(p).oper[0].val-Taicpu(hp1).oper[0].val);
                            Taicpu(hp1).opcode := A_AND;
                            l := (1 shl (Taicpu(hp1).oper[0].val)) - 1;
                            Case Taicpu(p).opsize Of
                              S_L: Taicpu(hp1).LoadConst(0,l Xor aword($ffffffff));
                              S_B: Taicpu(hp1).LoadConst(0,l Xor $ff);
                              S_W: Taicpu(hp1).LoadConst(0,l Xor $ffff);
                            End;
                          End
                        Else
                          If (Taicpu(p).oper[0].val<Taicpu(hp1).oper[0].val) And
                             Not(CS_LittleSize In aktglobalswitches)
                            Then
                   { shr/sar const1, %reg
                     shl     const2, %reg
                      with const1 < const2 }
                              Begin
                                Taicpu(hp1).LoadConst(0,Taicpu(hp1).oper[0].val-Taicpu(p).oper[0].val);
                                Taicpu(p).opcode := A_AND;
                                l := (1 shl (Taicpu(p).oper[0].val))-1;
                                Case Taicpu(p).opsize Of
                                  S_L: Taicpu(p).LoadConst(0,l Xor aword($ffffffff));
                                  S_B: Taicpu(p).LoadConst(0,l Xor $ff);
                                  S_W: Taicpu(p).LoadConst(0,l Xor $ffff);
                                End;
                              End
                            Else
                   { shr/sar const1, %reg
                     shl     const2, %reg
                      with const1 = const2 }
                              if (Taicpu(p).oper[0].val = Taicpu(hp1).oper[0].val) then
                                Begin
                                  Taicpu(p).opcode := A_AND;
                                  l := (1 shl (Taicpu(p).oper[0].val))-1;
                                  Case Taicpu(p).opsize Of
                                    S_B: Taicpu(p).LoadConst(0,l Xor $ff);
                                    S_W: Taicpu(p).LoadConst(0,l Xor $ffff);
                                    S_L: Taicpu(p).LoadConst(0,l Xor aword($ffffffff));
                                  End;
                                  asml.remove(hp1);
                                  hp1.free;
                                End;
                End;
              A_XOR:
                If (Taicpu(p).oper[0].typ = top_reg) And
                   (Taicpu(p).oper[1].typ = top_reg) And
                   (Taicpu(p).oper[0].reg = Taicpu(p).oper[1].reg) then
                 { temporarily change this to 'mov reg,0' to make it easier }
                 { for the CSE. Will be changed back in pass 2              }
                  begin
                    Taicpu(p).opcode := A_MOV;
                    Taicpu(p).loadconst(0,0);
                  end;
            End;
          End;
      End;
      p := Tai(p.next)
    End;
End;



Procedure PeepHoleOptPass1(Asml: TAAsmOutput; BlockStart, BlockEnd: Tai);
{First pass of peepholeoptimizations}

Var
  l,l1 : longint;
  p,hp1,hp2 : Tai;
  hp3,hp4: Tai;

  TmpRef: TReference;

  UsedRegs, TmpUsedRegs: TRegSet;

  TmpBool1, TmpBool2: Boolean;

  Function SkipLabels(hp: Tai; var hp2: Tai): boolean;
  {skips all labels and returns the next "real" instruction}
  Begin
    While assigned(hp.next) and
          (Tai(hp.next).typ In SkipInstr + [ait_label,ait_align]) Do
      hp := Tai(hp.next);
    If assigned(hp.next) Then
      Begin
        SkipLabels := True;
        hp2 := Tai(hp.next)
      End
    Else
      Begin
        hp2 := hp;
        SkipLabels := False
      End;
  End;

  function GetFinalDestination(AsmL: TAAsmOutput; hp: Taicpu; level: longint): boolean;
  {traces sucessive jumps to their final destination and sets it, e.g.
   je l1                je l3
   <code>               <code>
   l1:       becomes    l1:
   je l2                je l3
   <code>               <code>
   l2:                  l2:
   jmp l3               jmp l3

   the level parameter denotes how deeep we have already followed the jump,
   to avoid endless loops with constructs such as "l5: ; jmp l5"           }

  Var p1, p2: Tai;
      l: tasmlabel;

    Function FindAnyLabel(hp: Tai; var l: tasmlabel): Boolean;
    Begin
      FindAnyLabel := false;
      While assigned(hp.next) and
            (Tai(hp.next).typ In (SkipInstr+[ait_align])) Do
        hp := Tai(hp.next);
      If assigned(hp.next) and
         (Tai(hp.next).typ = ait_label) Then
        Begin
          FindAnyLabel := true;
          l := Tai_label(hp.next).l;
        End
    End;

  Begin
    if level > 20 then
      exit;
    GetfinalDestination := false;
    If (tasmlabel(hp.oper[0].sym).labelnr >= LoLab) and
       (tasmlabel(hp.oper[0].sym).labelnr <= HiLab) and   {range check, a jump can go past an assembler block!}
       Assigned(LTable^[tasmlabel(hp.oper[0].sym).labelnr-LoLab].TaiObj) Then
      Begin
        p1 := LTable^[tasmlabel(hp.oper[0].sym).labelnr-LoLab].TaiObj; {the jump's destination}
        SkipLabels(p1,p1);
        If (Tai(p1).typ = ait_instruction) and
           (Taicpu(p1).is_jmp) Then
          If { the next instruction after the label where the jump hp arrives}
             { is unconditional or of the same type as hp, so continue       }
             (Taicpu(p1).condition in [C_None,hp.condition]) or
             { the next instruction after the label where the jump hp arrives}
             { is the opposite of hp (so this one is never taken), but after }
             { that one there is a branch that will be taken, so perform a   }
             { little hack: set p1 equal to this instruction (that's what the}
             { last SkipLabels is for, only works with short bool evaluation)}
             ((Taicpu(p1).condition = inverse_cond[hp.condition]) and
              SkipLabels(p1,p2) and
              (p2.typ = ait_instruction) and
              (Taicpu(p2).is_jmp) and
              (Taicpu(p2).condition in [C_None,hp.condition]) and
              SkipLabels(p1,p1)) Then
            Begin
              { quick check for loops of the form "l5: ; jmp l5 }
              if (tasmlabel(Taicpu(p1).oper[0].sym).labelnr =
                   tasmlabel(hp.oper[0].sym).labelnr) then
                exit;
              if not GetFinalDestination(asml, Taicpu(p1),succ(level)) then
                exit;
              Dec(tasmlabel(hp.oper[0].sym).refs);
              hp.oper[0].sym:=Taicpu(p1).oper[0].sym;
              inc(tasmlabel(hp.oper[0].sym).refs);
            End
          Else
            If (Taicpu(p1).condition = inverse_cond[hp.condition]) then
              if not FindAnyLabel(p1,l) then
                begin
  {$ifdef finaldestdebug}
                  insertllitem(asml,p1,p1.next,Tai_asm_comment.Create(
                    strpnew('previous label inserted'))));
  {$endif finaldestdebug}
                  getlabel(l);
                  insertllitem(asml,p1,p1.next,Tai_label.Create(l));
                  dec(tasmlabel(Taicpu(hp).oper[0].sym).refs);
                  hp.oper[0].sym := l;
                  inc(l.refs);
  {               this won't work, since the new label isn't in the labeltable }
  {               so it will fail the rangecheck. Labeltable should become a   }
  {               hashtable to support this:                                   }
  {               GetFinalDestination(asml, hp);                               }
                end
              else
                begin
  {$ifdef finaldestdebug}
                  insertllitem(asml,p1,p1.next,Tai_asm_comment.Create(
                    strpnew('next label reused'))));
  {$endif finaldestdebug}
                  inc(l.refs);
                  hp.oper[0].sym := l;
                  if not GetFinalDestination(asml, hp,succ(level)) then
                    exit;
                end;
      End;
    GetFinalDestination := true;
  End;

  Function DoSubAddOpt(var p: Tai): Boolean;
  Begin
    DoSubAddOpt := False;
    If GetLastInstruction(p, hp1) And
       (hp1.typ = ait_instruction) And
       (Taicpu(hp1).opsize = Taicpu(p).opsize) then
      Case Taicpu(hp1).opcode Of
        A_DEC:
          If (Taicpu(hp1).oper[0].typ = top_reg) And
             (Taicpu(hp1).oper[0].reg = Taicpu(p).oper[1].reg) Then
            Begin
              Taicpu(p).LoadConst(0,Taicpu(p).oper[0].val+1);
              asml.Remove(hp1);
              hp1.free;
            End;
         A_SUB:
           If (Taicpu(hp1).oper[0].typ = top_const) And
              (Taicpu(hp1).oper[1].typ = top_reg) And
              (Taicpu(hp1).oper[1].reg = Taicpu(p).oper[1].reg) Then
             Begin
               Taicpu(p).LoadConst(0,Taicpu(p).oper[0].val+Taicpu(hp1).oper[0].val);
               asml.Remove(hp1);
               hp1.free;
             End;
         A_ADD:
           If (Taicpu(hp1).oper[0].typ = top_const) And
              (Taicpu(hp1).oper[1].typ = top_reg) And
              (Taicpu(hp1).oper[1].reg = Taicpu(p).oper[1].reg) Then
             Begin
               Taicpu(p).LoadConst(0,AWord(int64(Taicpu(p).oper[0].val)-int64(Taicpu(hp1).oper[0].val)));
               asml.Remove(hp1);
               hp1.free;
               If (Taicpu(p).oper[0].val = 0) Then
                 Begin
                   hp1 := Tai(p.next);
                   asml.Remove(p);
                   p.free;
                   If Not GetLastInstruction(hp1, p) Then
                     p := hp1;
                   DoSubAddOpt := True;
                 End
             End;
       End;
  End;

Begin
  P := BlockStart;
  UsedRegs := [];
  While (P <> BlockEnd) Do
    Begin
      UpDateUsedRegs(UsedRegs, Tai(p.next));
      Case p.Typ Of
        ait_instruction:
          Begin
            { Handle Jmp Optimizations }
            if Taicpu(p).is_jmp then
             begin
     {the following if-block removes all code between a jmp and the next label,
      because it can never be executed}
               If (Taicpu(p).opcode = A_JMP) Then
                 Begin
                   While GetNextInstruction(p, hp1) and
                         (hp1.typ <> ait_label) do
                     If not(hp1.typ in ([ait_label,ait_align]+skipinstr)) Then
                       Begin
                         asml.Remove(hp1);
                         hp1.free;
                       End
                     else break;
                  End;
               { remove jumps to a label coming right after them }
               If GetNextInstruction(p, hp1) then
                 Begin
                   if FindLabel(tasmlabel(Taicpu(p).oper[0].sym), hp1) then
                     Begin
                       hp2:=Tai(hp1.next);
                       asml.remove(p);
                       p.free;
                       p:=hp2;
                       continue;
                     end
                   Else
                     Begin
                       if hp1.typ = ait_label then
                         SkipLabels(hp1,hp1);
                       If (Tai(hp1).typ=ait_instruction) and
                          (Taicpu(hp1).opcode=A_JMP) and
                          GetNextInstruction(hp1, hp2) And
                          FindLabel(tasmlabel(Taicpu(p).oper[0].sym), hp2)
                         Then
                           Begin
                             if Taicpu(p).opcode=A_Jcc then
                              Taicpu(p).condition:=inverse_cond[Taicpu(p).condition]
                             else
                              begin
                                If (LabDif <> 0) Then
                                  GetFinalDestination(asml, Taicpu(p),0);
                                p:=Tai(p.next);
                                continue;
                              end;
                             Dec(Tai_label(hp2).l.refs);
                             Taicpu(p).oper[0].sym:=Taicpu(hp1).oper[0].sym;
                             Inc(Taicpu(p).oper[0].sym.refs);
                             asml.remove(hp1);
                             hp1.free;
                             If (LabDif <> 0) Then
                               GetFinalDestination(asml, Taicpu(p),0);
                           end
                         else
                           If (LabDif <> 0) Then
                             GetFinalDestination(asml, Taicpu(p),0);
                     end;
                 end;
             end
            else
            { All other optimizes }
             begin
            For l := 0 to 2 Do
              If (Taicpu(p).oper[l].typ = top_ref) Then
                With Taicpu(p).oper[l].ref^ Do
                  Begin
                    If (base = R_NO) And
                       (index <> R_NO) And
                       (scalefactor in [0,1])
                      Then
                        Begin
                          base := index;
                          index := R_NO
                        End
                   End;
            Case Taicpu(p).opcode Of
              A_AND:
                Begin
                  If (Taicpu(p).oper[0].typ = top_const) And
                     (Taicpu(p).oper[1].typ = top_reg) And
                     GetNextInstruction(p, hp1) And
                     (Tai(hp1).typ = ait_instruction) And
                     (Taicpu(hp1).opcode = A_AND) And
                     (Taicpu(hp1).oper[0].typ = top_const) And
                     (Taicpu(hp1).oper[1].typ = top_reg) And
                     (Taicpu(p).oper[1].reg = Taicpu(hp1).oper[1].reg)
                    Then
{change "and const1, reg; and const2, reg" to "and (const1 and const2), reg"}
                      Begin
                        Taicpu(p).LoadConst(0,Taicpu(p).oper[0].val And Taicpu(hp1).oper[0].val);
                        asml.Remove(hp1);
                        hp1.free;
                      End
                    Else
{change "and x, reg; jxx" to "test x, reg", if reg is deallocated before the
 jump, but only if it's a conditional jump (PFV) }
                      If (Taicpu(p).oper[1].typ = top_reg) And
                         GetNextInstruction(p, hp1) And
                         (hp1.typ = ait_instruction) And
                         (Taicpu(hp1).is_jmp) and
                         (Taicpu(hp1).opcode<>A_JMP) and
                         Not(Taicpu(p).oper[1].reg in UsedRegs) Then
                        Taicpu(p).opcode := A_TEST;
                End;
              A_CMP:
                Begin
                  If (Taicpu(p).oper[0].typ = top_const) And
                     (Taicpu(p).oper[1].typ in [top_reg,top_ref]) And
                     (Taicpu(p).oper[0].val = 0) and
                     GetNextInstruction(p, hp1) And
                     (hp1.typ = ait_instruction) And
                     (Taicpu(hp1).is_jmp) and
                     (Taicpu(hp1).opcode=A_Jcc) and
                     (Taicpu(hp1).condition in [C_LE,C_BE]) and
                     GetNextInstruction(hp1,hp2) and
                     (hp2.typ = ait_instruction) and
                     (Taicpu(hp2).opcode = A_DEC) And
                     OpsEqual(Taicpu(hp2).oper[0],Taicpu(p).oper[1]) And
                     GetNextInstruction(hp2, hp3) And
                     (hp3.typ = ait_instruction) and
                     (Taicpu(hp3).is_jmp) and
                     (Taicpu(hp3).opcode = A_JMP) And
                     GetNextInstruction(hp3, hp4) And
                     FindLabel(tasmlabel(Taicpu(hp1).oper[0].sym),hp4) Then
                    Begin
                      Taicpu(hp2).Opcode := A_SUB;
                      Taicpu(hp2).Loadoper(1,Taicpu(hp2).oper[0]);
                      Taicpu(hp2).LoadConst(0,1);
                      Taicpu(hp2).ops:=2;
                      Taicpu(hp3).Opcode := A_Jcc;
                      Case Taicpu(hp1).condition of
                        C_LE: Taicpu(hp3).condition := C_GE;
                        C_BE: Taicpu(hp3).condition := C_AE;
                      End;
                      asml.Remove(p);
                      asml.Remove(hp1);
                      p.free;
                      hp1.free;
                      p := hp2;
                      continue;
                    End
                End;
              A_FLD:
                Begin
                  If (Taicpu(p).oper[0].typ = top_reg) And
                     GetNextInstruction(p, hp1) And
                     (hp1.typ = Ait_Instruction) And
                     (Taicpu(hp1).oper[0].typ = top_reg) And
                     (Taicpu(hp1).oper[1].typ = top_reg) And
                     (Taicpu(hp1).oper[0].reg = R_ST) And
                     (Taicpu(hp1).oper[1].reg = R_ST1) Then
                     { change                        to
                         fld      reg               fxxx reg,st
                         fxxxp    st, st1 (hp1)
                       Remark: non commutative operations must be reversed!
                     }
                     begin
                        Case Taicpu(hp1).opcode Of
                          A_FMULP,A_FADDP,
                          A_FSUBP,A_FDIVP,A_FSUBRP,A_FDIVRP:
                            begin
                               Case Taicpu(hp1).opcode Of
                                 A_FADDP: Taicpu(hp1).opcode := A_FADD;
                                 A_FMULP: Taicpu(hp1).opcode := A_FMUL;
                                 A_FSUBP: Taicpu(hp1).opcode := A_FSUBR;
                                 A_FSUBRP: Taicpu(hp1).opcode := A_FSUB;
                                 A_FDIVP: Taicpu(hp1).opcode := A_FDIVR;
                                 A_FDIVRP: Taicpu(hp1).opcode := A_FDIV;
                               End;
                               Taicpu(hp1).oper[0].reg := Taicpu(p).oper[0].reg;
                               Taicpu(hp1).oper[1].reg := R_ST;
                               asml.Remove(p);
                               p.free;
                               p := hp1;
                               Continue;
                            end;
                        end;
                     end
                  else
                  If (Taicpu(p).oper[0].typ = top_ref) And
                     GetNextInstruction(p, hp2) And
                     (hp2.typ = Ait_Instruction) And
                     (Taicpu(hp2).oper[0].typ = top_reg) And
                     (Taicpu(hp2).oper[1].typ = top_reg) And
                     (Taicpu(p).opsize in [S_FS, S_FL]) And
                     (Taicpu(hp2).oper[0].reg = R_ST) And
                     (Taicpu(hp2).oper[1].reg = R_ST1) Then
                    If GetLastInstruction(p, hp1) And
                       (hp1.typ = Ait_Instruction) And
                       ((Taicpu(hp1).opcode = A_FLD) Or
                        (Taicpu(hp1).opcode = A_FST)) And
                       (Taicpu(hp1).opsize = Taicpu(p).opsize) And
                       (Taicpu(hp1).oper[0].typ = top_ref) And
                       RefsEqual(Taicpu(p).oper[0].ref^, Taicpu(hp1).oper[0].ref^) Then
                      If ((Taicpu(hp2).opcode = A_FMULP) Or
                          (Taicpu(hp2).opcode = A_FADDP)) Then

                      { change                      to
                          fld/fst   mem1  (hp1)       fld/fst   mem1
                          fld       mem1  (p)         fadd/
                          faddp/                       fmul     st, st
                           fmulp  st, st1 (hp2) }
                        Begin
                          asml.Remove(p);
                          p.free;
                          p := hp1;
                          If (Taicpu(hp2).opcode = A_FADDP) Then
                            Taicpu(hp2).opcode := A_FADD
                          Else
                            Taicpu(hp2).opcode := A_FMUL;
                          Taicpu(hp2).oper[1].reg := R_ST;
                        End
                      Else
                      { change              to
                          fld/fst mem1 (hp1)   fld/fst mem1
                          fld     mem1 (p)     fld      st}
                        Begin
                          Taicpu(p).changeopsize(S_FL);
                          Taicpu(p).loadreg(0,R_ST);
                        End
                    Else
                      Begin
                        Case Taicpu(hp2).opcode Of
                          A_FMULP,A_FADDP,A_FSUBP,A_FDIVP,A_FSUBRP,A_FDIVRP:
                     { change                        to
                         fld/fst  mem1    (hp1)      fld/fst    mem1
                         fld      mem2    (p)        fxxx       mem2
                         fxxxp    st, st1 (hp2)                      }

                            Begin
                              Case Taicpu(hp2).opcode Of
                                A_FADDP: Taicpu(p).opcode := A_FADD;
                                A_FMULP: Taicpu(p).opcode := A_FMUL;
                                A_FSUBP: Taicpu(p).opcode := A_FSUBR;
                                A_FSUBRP: Taicpu(p).opcode := A_FSUB;
                                A_FDIVP: Taicpu(p).opcode := A_FDIVR;
                                A_FDIVRP: Taicpu(p).opcode := A_FDIV;
                              End;
                              asml.Remove(hp2);
                              hp2.free;
                            End
                        End
                      End
                End;
              A_FSTP,A_FISTP:
                if doFpuLoadStoreOpt(asmL,p) then
                  continue;
              A_LEA:
                Begin
                {removes seg register prefixes from LEA operations, as they
                 don't do anything}
                 Taicpu(p).oper[0].ref^.Segment := R_NO;
                {changes "lea (%reg1), %reg2" into "mov %reg1, %reg2"}
                  If (Taicpu(p).oper[0].ref^.Base In [R_EAX..R_EDI]) And
                     (Taicpu(p).oper[0].ref^.Index = R_NO) And
                     (Not(Assigned(Taicpu(p).oper[0].ref^.Symbol))) Then
                    If (Taicpu(p).oper[0].ref^.Base <> Taicpu(p).oper[1].reg)
                       and (Taicpu(p).oper[0].ref^.Offset = 0)
                       Then
                        Begin
                          hp1 := Taicpu.op_reg_reg(A_MOV, S_L,Taicpu(p).oper[0].ref^.Base,
                            Taicpu(p).oper[1].reg);
                         InsertLLItem(AsmL,p.previous,p.next, hp1);
                         p.free;
                         p := hp1;
                         Continue;
                       End
                     Else
                      if (Taicpu(p).oper[0].ref^.Offset = 0) then
                       Begin
                         hp1 := Tai(p.Next);
                         asml.Remove(p);
                         p.free;
                         p := hp1;
                         Continue;
                       End
                      else
                        with Taicpu(p).oper[0].ref^ do
                          if (Base = Taicpu(p).oper[1].reg) then
                            begin
                              l := offset+offsetfixup;
                              if (l=1) then
                               begin
                                 Taicpu(p).opcode := A_INC;
                                 Taicpu(p).loadreg(0,Taicpu(p).oper[1].reg);
                                 Taicpu(p).ops := 1
                               end
                              else
                               if (l=-1) then
                                begin
                                  Taicpu(p).opcode := A_DEC;
                                  Taicpu(p).loadreg(0,Taicpu(p).oper[1].reg);
                                  Taicpu(p).ops := 1;
                                end
                              else
                               begin
                                 Taicpu(p).opcode := A_ADD;
                                 Taicpu(p).loadconst(0,aword(l));
                               end;
                            end;
                End;
              A_MOV:
                Begin
                  TmpUsedRegs := UsedRegs;
                  If (Taicpu(p).oper[1].typ = top_reg) And
                     (Taicpu(p).oper[1].reg In [R_EAX, R_EBX, R_EDX, R_EDI]) And
                     GetNextInstruction(p, hp1) And
                     (Tai(hp1).typ = ait_instruction) And
                     (Taicpu(hp1).opcode = A_MOV) And
                     (Taicpu(hp1).oper[0].typ = top_reg) And
                     (Taicpu(hp1).oper[0].reg = Taicpu(p).oper[1].reg)
                    Then
                {we have "mov x, %treg; mov %treg, y}
                      If not(RegUsedAfterInstruction(Taicpu(p).oper[1].reg, hp1, TmpUsedRegs)) then
                {we've got "mov x, %treg; mov %treg, y; with %treg is not used after }
                          Case Taicpu(p).oper[0].typ Of
                            top_reg:
                              Begin
                                { change "mov %reg, %treg; mov %treg, y"
                                  to "mov %reg, y" }
                                Taicpu(p).LoadOper(1,Taicpu(hp1).oper[1]);
                                asml.Remove(hp1);
                                hp1.free;
                                continue;
                              End;
                            top_ref:
                              If (Taicpu(hp1).oper[1].typ = top_reg) Then
                               Begin
                                 { change "mov mem, %treg; mov %treg, %reg"
                                   to "mov mem, %reg" }
                                 Taicpu(p).Loadoper(1,Taicpu(hp1).oper[1]);
                                 asml.Remove(hp1);
                                 hp1.free;
                                 continue;
                               End;
                          End
                        Else
                    Else
                  {Change "mov %reg1, %reg2; xxx %reg2, ???" to
                   "mov %reg1, %reg2; xxx %reg1, ???" to avoid a write/read
                   penalty}
                      If (Taicpu(p).oper[0].typ = top_reg) And
                         (Taicpu(p).oper[1].typ = top_reg) And
                         GetNextInstruction(p,hp1) And
                         (Tai(hp1).typ = ait_instruction) And
                         (Taicpu(hp1).oper[0].typ = top_reg) And
                         (Taicpu(hp1).oper[0].reg = Taicpu(p).oper[1].reg)
                        Then
                  {we have "mov %reg1, %reg2; XXX %reg2, ???"}
                          Begin
                            If ((Taicpu(hp1).opcode = A_OR) Or
                                (Taicpu(hp1).opcode = A_TEST)) And
                               (Taicpu(hp1).oper[1].typ = top_reg) And
                               (Taicpu(hp1).oper[0].reg = Taicpu(hp1).oper[1].reg)
                              Then
                   {we have "mov %reg1, %reg2; test/or %reg2, %reg2"}
                                Begin
                                  TmpUsedRegs := UsedRegs;
                                  { reg1 will be used after the first instruction, }
                                  { so update the allocation info                  }
                                  allocRegBetween(asmL,Taicpu(p).oper[0].reg,p,hp1);
                                  If GetNextInstruction(hp1, hp2) And
                                     (hp2.typ = ait_instruction) And
                                     Taicpu(hp2).is_jmp and
                                     Not(RegUsedAfterInstruction(Taicpu(hp1).oper[0].reg, hp1, TmpUsedRegs))
                                    Then
                   {change "mov %reg1, %reg2; test/or %reg2, %reg2; jxx" to
                    "test %reg1, %reg1; jxx"}
                                      Begin
                                        Taicpu(hp1).Loadoper(0,Taicpu(p).oper[0]);
                                        Taicpu(hp1).Loadoper(1,Taicpu(p).oper[0]);
                                        asml.Remove(p);
                                        p.free;
                                        p := hp1;
                                        continue
                                      End
                                    Else
                   {change "mov %reg1, %reg2; test/or %reg2, %reg2" to
                    "mov %reg1, %reg2; test/or %reg1, %reg1"}
                                      Begin
                                        Taicpu(hp1).Loadoper(0,Taicpu(p).oper[0]);
                                        Taicpu(hp1).Loadoper(1,Taicpu(p).oper[0]);
                                      End;
                                End
{                              Else
                                If (Taicpu(p.next)^.opcode
                                   In [A_PUSH, A_OR, A_XOR, A_AND, A_TEST])}
                         {change "mov %reg1, %reg2; push/or/xor/... %reg2, ???" to
                          "mov %reg1, %reg2; push/or/xor/... %reg1, ???"}
                          End
                        Else
                  {leave out the mov from "mov reg, x(%frame_pointer); leave/ret" (with
                   x >= RetOffset) as it doesn't do anything (it writes either to a
                   parameter or to the temporary storage room for the function
                   result)}
                          If GetNextInstruction(p, hp1) And
                             (Tai(hp1).typ = ait_instruction)
                            Then
                              If ((Taicpu(hp1).opcode = A_LEAVE) Or
                                  (Taicpu(hp1).opcode = A_RET)) And
                                 (Taicpu(p).oper[1].typ = top_ref) And
                                 (Taicpu(p).oper[1].ref^.base = procinfo^.FramePointer) And
                                 (Taicpu(p).oper[1].ref^.offset >= procinfo^.Return_Offset) And
                                 (Taicpu(p).oper[1].ref^.index = R_NO) And
                                 (Taicpu(p).oper[0].typ = top_reg)
                                Then
                                  Begin
                                   asml.Remove(p);
                                   p.free;
                                   p := hp1;
                                   RemoveLastDeallocForFuncRes(asmL,p);
                                 End
                               Else
                                 If (Taicpu(p).oper[0].typ = top_reg) And
                                    (Taicpu(p).oper[1].typ = top_ref) And
                                    (Taicpu(p).opsize = Taicpu(hp1).opsize) And
                                    (Taicpu(hp1).opcode = A_CMP) And
                                    (Taicpu(hp1).oper[1].typ = top_ref) And
                                    RefsEqual(Taicpu(p).oper[1].ref^, Taicpu(hp1).oper[1].ref^) Then
            {change "mov reg1, mem1; cmp x, mem1" to "mov reg, mem1; cmp x, reg1"}
                                   begin
                                     Taicpu(hp1).loadreg(1,Taicpu(p).oper[0].reg);
                                     allocRegBetween(asmL,Taicpu(p).oper[0].reg,p,hp1);
                                   end;
                { Next instruction is also a MOV ? }
                  If GetNextInstruction(p, hp1) And
                     (Tai(hp1).typ = ait_instruction) and
                     (Taicpu(hp1).opcode = A_MOV) and
                     (Taicpu(hp1).opsize = Taicpu(p).opsize)
                  Then
                      Begin
                        If (Taicpu(hp1).oper[0].typ = Taicpu(p).oper[1].typ) and
                           (Taicpu(hp1).oper[1].typ = Taicpu(p).oper[0].typ)
                          Then
                            {mov reg1, mem1     or     mov mem1, reg1
                             mov mem2, reg2            mov reg2, mem2}
                            Begin
                              If OpsEqual(Taicpu(hp1).oper[1],Taicpu(p).oper[0]) Then
                            {mov reg1, mem1     or     mov mem1, reg1
                             mov mem2, reg1            mov reg2, mem1}
                                Begin
                                  If OpsEqual(Taicpu(hp1).oper[0],Taicpu(p).oper[1]) Then
                        { Removes the second statement from
                            mov reg1, mem1/reg2
                            mov mem1/reg2, reg1 }
                                    Begin
                                      if (Taicpu(p).oper[0].typ = top_reg) then
                                        AllocRegBetween(asmL,Taicpu(p).oper[0].reg,p,hp1);
                                      asml.remove(hp1);
                                      hp1.free;
                                    End
                                  Else
                                    Begin
                                      TmpUsedRegs := UsedRegs;
                                      UpdateUsedRegs(TmpUsedRegs, Tai(hp1.next));
                                      If (Taicpu(p).oper[1].typ = top_ref) And
                                        { mov reg1, mem1
                                          mov mem2, reg1 }
                                         GetNextInstruction(hp1, hp2) And
                                         (hp2.typ = ait_instruction) And
                                         (Taicpu(hp2).opcode = A_CMP) And
                                         (Taicpu(hp2).opsize = Taicpu(p).opsize) and
                                         (Taicpu(hp2).oper[0].typ = TOp_Ref) And
                                         (Taicpu(hp2).oper[1].typ = TOp_Reg) And
                                         RefsEqual(Taicpu(hp2).oper[0].ref^, Taicpu(p).oper[1].ref^) And
                                         (Taicpu(hp2).oper[1].reg = Taicpu(p).oper[0].reg) And
                                         Not(RegUsedAfterInstruction(Taicpu(p).oper[0].reg, hp2, TmpUsedRegs)) Then
                           { change                   to
                              mov reg1, mem1           mov reg1, mem1
                              mov mem2, reg1           cmp reg1, mem2
                              cmp mem1, reg1                          }
                                        Begin
                                          asml.Remove(hp2);
                                          hp2.free;
                                          Taicpu(hp1).opcode := A_CMP;
                                          Taicpu(hp1).loadref(1,Taicpu(hp1).oper[0].ref^);
                                          Taicpu(hp1).loadreg(0,Taicpu(p).oper[0].reg);
                                        End;
                                    End;
                                End
                              Else
                                Begin
                                  tmpUsedRegs := UsedRegs;
                                  If GetNextInstruction(hp1, hp2) And
                                     (Taicpu(p).oper[0].typ = top_ref) And
                                     (Taicpu(p).oper[1].typ = top_reg) And
                                     (Taicpu(hp1).oper[0].typ = top_reg) And
                                     (Taicpu(hp1).oper[0].reg = Taicpu(p).oper[1].reg) And
                                     (Taicpu(hp1).oper[1].typ = top_ref) And
                                     (Tai(hp2).typ = ait_instruction) And
                                     (Taicpu(hp2).opcode = A_MOV) And
                                     (Taicpu(hp2).opsize = Taicpu(p).opsize) and
                                     (Taicpu(hp2).oper[1].typ = top_reg) And
                                     (Taicpu(hp2).oper[0].typ = top_ref) And
                                     RefsEqual(Taicpu(hp2).oper[0].ref^, Taicpu(hp1).oper[1].ref^)  Then
                                    If not regInRef(Taicpu(hp2).oper[1].reg,Taicpu(hp2).oper[0].ref^) and
                                       (Taicpu(p).oper[1].reg in [R_DI,R_EDI]) and
                                       not(RegUsedAfterInstruction(R_EDI,hp1,tmpUsedRegs)) Then
                             {   mov mem1, %edi
                                 mov %edi, mem2
                                 mov mem2, reg2
                              to:
                                 mov mem1, reg2
                                 mov reg2, mem2}
                                      Begin
                                        AllocRegBetween(asmL,reg32(Taicpu(hp2).oper[1].reg),p,hp2);
                                        Taicpu(p).Loadoper(1,Taicpu(hp2).oper[1]);
                                        Taicpu(hp1).loadoper(0,Taicpu(hp2).oper[1]);
                                        asml.Remove(hp2);
                                        hp2.free;
                                      End
                                    Else
                                      If (Taicpu(p).oper[1].reg <> Taicpu(hp2).oper[1].reg) And
                                         not(RegInRef(Taicpu(p).oper[1].reg,Taicpu(p).oper[0].ref^)) And
                                         not(RegInRef(Taicpu(hp2).oper[1].reg,Taicpu(hp2).oper[0].ref^))
                                        Then
                           {   mov mem1, reg1         mov mem1, reg1
                               mov reg1, mem2         mov reg1, mem2
                               mov mem2, reg2         mov mem2, reg1
                            to:                    to:
                               mov mem1, reg1         mov mem1, reg1
                               mov mem1, reg2         mov reg1, mem2
                               mov reg1, mem2

                         or (if mem1 depends on reg1
                             and/or if mem2 depends on reg2)
                            to:
                               mov mem1, reg1
                               mov reg1, mem2
                               mov reg1, reg2
                         }
                                        Begin
                                          Taicpu(hp1).LoadRef(0,Taicpu(p).oper[0].ref^);
                                          Taicpu(hp1).LoadReg(1,Taicpu(hp2).oper[1].reg);
                                          Taicpu(hp2).LoadRef(1,Taicpu(hp2).oper[0].ref^);
                                          Taicpu(hp2).LoadReg(0,Taicpu(p).oper[1].reg);
                                          allocRegBetween(asmL,Taicpu(p).oper[1].reg,p,hp2);
                                          if (Taicpu(p).oper[0].ref^.base in (rg.usableregsint+[R_EDI])) then
                                            allocRegBetween(asmL,Taicpu(p).oper[0].ref^.base,p,hp2);
                                          if (Taicpu(p).oper[0].ref^.index in (rg.usableregsint+[R_EDI])) then
                                            allocRegBetween(asmL,Taicpu(p).oper[0].ref^.index,p,hp2);
                                        End
                                      Else
                                        If (Taicpu(hp1).Oper[0].reg <> Taicpu(hp2).Oper[1].reg) Then
                                          begin
                                            Taicpu(hp2).LoadReg(0,Taicpu(hp1).Oper[0].reg);
                                            allocRegBetween(asmL,Taicpu(p).oper[1].reg,p,hp2);
                                          end
                                        else
                                          begin
                                            asml.Remove(hp2);
                                            hp2.free;
                                          end
                                End;
                            End
                          Else
(*                          {movl [mem1],reg1
                             movl [mem1],reg2
                            to:
                              movl [mem1],reg1
                              movl reg1,reg2 }
                            If (Taicpu(p).oper[0].typ = top_ref) and
                               (Taicpu(p).oper[1].typ = top_reg) and
                               (Taicpu(hp1).oper[0].typ = top_ref) and
                               (Taicpu(hp1).oper[1].typ = top_reg) and
                               (Taicpu(p).opsize = Taicpu(hp1).opsize) and
                               RefsEqual(TReference(Taicpu(p).oper[0]^),Taicpu(hp1).oper[0]^.ref^) and
                               (Taicpu(p).oper[1].reg<>Taicpu(hp1).oper[0]^.ref^.base) and
                               (Taicpu(p).oper[1].reg<>Taicpu(hp1).oper[0]^.ref^.index) then
                              Taicpu(hp1).LoadReg(0,Taicpu(p).oper[1].reg)
                            Else*)
                            {   movl const1,[mem1]
                                movl [mem1],reg1
                             to:
                                movl const1,reg1
                                movl reg1,[mem1] }
                              If (Taicpu(p).oper[0].typ = top_const) and
                                 (Taicpu(p).oper[1].typ = top_ref) and
                                 (Taicpu(hp1).oper[0].typ = top_ref) and
                                 (Taicpu(hp1).oper[1].typ = top_reg) and
                                 (Taicpu(p).opsize = Taicpu(hp1).opsize) and
                                 RefsEqual(Taicpu(hp1).oper[0].ref^,Taicpu(p).oper[1].ref^) then
                                Begin
                                  allocregbetween(asml,Taicpu(hp1).oper[1].reg,p,hp1);
                                  { allocregbetween doesn't insert this because at }
                                  { this time, no regalloc info is available in    }
                                  { the optinfo field, so do it manually (JM)      }
                                  hp2 := tai_regalloc.Alloc(Taicpu(hp1).oper[1].reg);
                                  insertllitem(asml,p.previous,p,hp2);
                                  Taicpu(hp1).LoadReg(0,Taicpu(hp1).oper[1].reg);
                                  Taicpu(hp1).LoadRef(1,Taicpu(p).oper[1].ref^);
                                  Taicpu(p).LoadReg(1,Taicpu(hp1).oper[0].reg);
                                End
                      End;
                End;
              A_MOVZX:
                Begin
                {removes superfluous And's after movzx's}
                  If (Taicpu(p).oper[1].typ = top_reg) And
                     GetNextInstruction(p, hp1) And
                     (Tai(hp1).typ = ait_instruction) And
                     (Taicpu(hp1).opcode = A_AND) And
                     (Taicpu(hp1).oper[0].typ = top_const) And
                     (Taicpu(hp1).oper[1].typ = top_reg) And
                     (Taicpu(hp1).oper[1].reg = Taicpu(p).oper[1].reg)
                    Then
                      Case Taicpu(p).opsize Of
                        S_BL, S_BW:
                          If (Taicpu(hp1).oper[0].val = $ff) Then
                            Begin
                              asml.Remove(hp1);
                              hp1.free;
                            End;
                        S_WL:
                          If (Taicpu(hp1).oper[0].val = $ffff) Then
                            Begin
                              asml.Remove(hp1);
                              hp1.free;
                            End;
                      End;
                {changes some movzx constructs to faster synonims (all examples
                 are given with eax/ax, but are also valid for other registers)}
                  If (Taicpu(p).oper[1].typ = top_reg) Then
                    If (Taicpu(p).oper[0].typ = top_reg) Then
                      Case Taicpu(p).opsize of
                        S_BW:
                          Begin
                            If (rg.makeregsize(Taicpu(p).oper[0].reg,OS_16)=Taicpu(p).oper[1].reg) And
                               Not(CS_LittleSize In aktglobalswitches)
                              Then
                                {Change "movzbw %al, %ax" to "andw $0x0ffh, %ax"}
                                Begin
                                  Taicpu(p).opcode := A_AND;
                                  Taicpu(p).changeopsize(S_W);
                                  Taicpu(p).LoadConst(0,$ff);
                                End
                              Else
                                If GetNextInstruction(p, hp1) And
                                   (Tai(hp1).typ = ait_instruction) And
                                   (Taicpu(hp1).opcode = A_AND) And
                                   (Taicpu(hp1).oper[0].typ = top_const) And
                                   (Taicpu(hp1).oper[1].typ = top_reg) And
                                   (Taicpu(hp1).oper[1].reg = Taicpu(p).oper[1].reg)
                                  Then
                                    {Change "movzbw %reg1, %reg2; andw $const, %reg2"
                                     to "movw %reg1, reg2; andw $(const1 and $ff), %reg2"}
                                    Begin
                                      Taicpu(p).opcode := A_MOV;
                                      Taicpu(p).changeopsize(S_W);
                                      Taicpu(p).LoadReg(0,rg.makeregsize(Taicpu(p).oper[0].reg,OS_16));
                                      Taicpu(hp1).LoadConst(0,Taicpu(hp1).oper[0].val And $ff);
                                    End;
                          End;
                        S_BL:
                          Begin
                            If (rg.makeregsize(Taicpu(p).oper[0].reg,OS_32)=Taicpu(p).oper[1].reg) And
                               Not(CS_LittleSize in aktglobalswitches)
                              Then
                                {Change "movzbl %al, %eax" to "andl $0x0ffh, %eax"}
                                Begin
                                  Taicpu(p).opcode := A_AND;
                                  Taicpu(p).changeopsize(S_L);
                                  Taicpu(p).loadconst(0,$ff)
                                End
                              Else
                                If GetNextInstruction(p, hp1) And
                                   (Tai(hp1).typ = ait_instruction) And
                                   (Taicpu(hp1).opcode = A_AND) And
                                   (Taicpu(hp1).oper[0].typ = top_const) And
                                   (Taicpu(hp1).oper[1].typ = top_reg) And
                                   (Taicpu(hp1).oper[1].reg = Taicpu(p).oper[1].reg)
                                  Then
                                   {Change "movzbl %reg1, %reg2; andl $const, %reg2"
                                    to "movl %reg1, reg2; andl $(const1 and $ff), %reg2"}
                                    Begin
                                      Taicpu(p).opcode := A_MOV;
                                      Taicpu(p).changeopsize(S_L);
                                      Taicpu(p).LoadReg(0,rg.makeregsize(Taicpu(p).oper[0].reg,OS_32));
                                      Taicpu(hp1).LoadConst(0,Taicpu(hp1).oper[0].val And $ff);
                                    End
                          End;
                        S_WL:
                          Begin
                            If (rg.makeregsize(Taicpu(p).oper[0].reg,OS_32)=Taicpu(p).oper[1].reg) And
                               Not(CS_LittleSize In aktglobalswitches)
                              Then
                               {Change "movzwl %ax, %eax" to "andl $0x0ffffh, %eax"}
                                Begin
                                  Taicpu(p).opcode := A_AND;
                                  Taicpu(p).changeopsize(S_L);
                                  Taicpu(p).LoadConst(0,$ffff);
                                End
                              Else
                                If GetNextInstruction(p, hp1) And
                                   (Tai(hp1).typ = ait_instruction) And
                                   (Taicpu(hp1).opcode = A_AND) And
                                   (Taicpu(hp1).oper[0].typ = top_const) And
                                   (Taicpu(hp1).oper[1].typ = top_reg) And
                                   (Taicpu(hp1).oper[1].reg = Taicpu(p).oper[1].reg)
                                  Then
                                    {Change "movzwl %reg1, %reg2; andl $const, %reg2"
                                     to "movl %reg1, reg2; andl $(const1 and $ffff), %reg2"}
                                    Begin
                                      Taicpu(p).opcode := A_MOV;
                                      Taicpu(p).changeopsize(S_L);
                                      Taicpu(p).LoadReg(0,rg.makeregsize(Taicpu(p).oper[0].reg,OS_32));
                                      Taicpu(hp1).LoadConst(0,Taicpu(hp1).oper[0].val And $ffff);
                                    End;
                          End;
                        End
                      Else
                        If (Taicpu(p).oper[0].typ = top_ref) Then
                          Begin
                            If GetNextInstruction(p, hp1) And
                               (Tai(hp1).typ = ait_instruction) And
                               (Taicpu(hp1).opcode = A_AND) And
                               (Taicpu(hp1).oper[0].typ = Top_Const) And
                               (Taicpu(hp1).oper[1].typ = Top_Reg) And
                               (Taicpu(hp1).oper[1].reg = Taicpu(p).oper[1].reg) Then
                              Begin
                                Taicpu(p).opcode := A_MOV;
                                Case Taicpu(p).opsize Of
                                  S_BL:
                                    Begin
                                      Taicpu(p).changeopsize(S_L);
                                      Taicpu(hp1).LoadConst(0,Taicpu(hp1).oper[0].val And $ff);
                                    End;
                                  S_WL:
                                    Begin
                                      Taicpu(p).changeopsize(S_L);
                                      Taicpu(hp1).LoadConst(0,Taicpu(hp1).oper[0].val And $ffff);
                                    End;
                                  S_BW:
                                    Begin
                                      Taicpu(p).changeopsize(S_W);
                                      Taicpu(hp1).LoadConst(0,Taicpu(hp1).oper[0].val And $ff);
                                    End;
                                End;
                              End;
                          End;
                End;
              A_POP:
                Begin
                  if target_info.system=system_i386_go32v2 then
                   begin
                     { Transform a series of pop/pop/pop/push/push/push to }
                     { 'movl x(%esp),%reg' for go32v2 (not for the rest,   }
                     { because I'm not sure whether they can cope with     }
                     { 'movl x(%esp),%reg' with x > 0, I believe we had    }
                     { such a problem when using esp as frame pointer (JM) }
                     if (Taicpu(p).oper[0].typ = top_reg) then
                       begin
                         hp1 := p;
                         hp2 := p;
                         l := 0;
                         while getNextInstruction(hp1,hp1) and
                               (hp1.typ = ait_instruction) and
                               (Taicpu(hp1).opcode = A_POP) and
                               (Taicpu(hp1).oper[0].typ = top_reg) do
                           begin
                             hp2 := hp1;
                             inc(l,4);
                           end;
                         getLastInstruction(p,hp3);
                         l1 := 0;
                         while (hp2 <> hp3) and
                               assigned(hp1) and
                               (hp1.typ = ait_instruction) and
                               (Taicpu(hp1).opcode = A_PUSH) and
                               (Taicpu(hp1).oper[0].typ = top_reg) and
                               (Taicpu(hp1).oper[0].reg = Taicpu(hp2).oper[0].reg) do
                           begin
                             { change it to a two op operation }
                             Taicpu(hp2).oper[1].typ:=top_none;
                             Taicpu(hp2).ops:=2;
                             Taicpu(hp2).opcode := A_MOV;
                             Taicpu(hp2).Loadoper(1,Taicpu(hp1).oper[0]);
                             reference_reset(tmpref);
                             tmpRef.base := STACK_POINTER_REG;
                             tmpRef.offset := l;
                             Taicpu(hp2).loadRef(0,tmpRef);
                             hp4 := hp1;
                             getNextInstruction(hp1,hp1);
                             asml.remove(hp4);
                             hp4.free;
                             getLastInstruction(hp2,hp2);
                             dec(l,4);
                             inc(l1);
                           end;
                         if l <> -4 then
                           begin
                             inc(l,4);
                             for l1 := l1 downto 1 do
                               begin
                                 getNextInstruction(hp2,hp2);
                                 dec(Taicpu(hp2).oper[0].ref^.offset,l);
                               end
                           end
                       end
                    end
                   else
                    begin
                      if (Taicpu(p).oper[0].typ = top_reg) And
                         GetNextInstruction(p, hp1) And
                         (Tai(hp1).typ=ait_instruction) and
                         (Taicpu(hp1).opcode=A_PUSH) and
                         (Taicpu(hp1).oper[0].typ = top_reg) And
                         (Taicpu(hp1).oper[0].reg=Taicpu(p).oper[0].reg) then
                        Begin
                          { change it to a two op operation }
                          Taicpu(p).oper[1].typ:=top_none;
                          Taicpu(p).ops:=2;
                          Taicpu(p).opcode := A_MOV;
                          Taicpu(p).Loadoper(1,Taicpu(p).oper[0]);
                          reference_reset(tmpref);
                          TmpRef.base := R_ESP;
                          Taicpu(p).LoadRef(0,TmpRef);
                          asml.Remove(hp1);
                          hp1.free;
                        End;
                    end;
                end;
              A_PUSH:
                Begin
                  If (Taicpu(p).opsize = S_W) And
                     (Taicpu(p).oper[0].typ = Top_Const) And
                     GetNextInstruction(p, hp1) And
                     (Tai(hp1).typ = ait_instruction) And
                     (Taicpu(hp1).opcode = A_PUSH) And
                     (Taicpu(hp1).oper[0].typ = Top_Const) And
                     (Taicpu(hp1).opsize = S_W) Then
                    Begin
                      Taicpu(p).changeopsize(S_L);
                      Taicpu(p).LoadConst(0,Taicpu(p).oper[0].val shl 16 + word(Taicpu(hp1).oper[0].val));
                      asml.Remove(hp1);
                      hp1.free;
                    End;
                End;
              A_SHL, A_SAL:
                Begin
                  If (Taicpu(p).oper[0].typ = Top_Const) And
                     (Taicpu(p).oper[1].typ = Top_Reg) And
                     (Taicpu(p).opsize = S_L) And
                     (Taicpu(p).oper[0].val <= 3)
                {Changes "shl const, %reg32; add const/reg, %reg32" to one lea statement}
                    Then
                      Begin
                        TmpBool1 := True; {should we check the next instruction?}
                        TmpBool2 := False; {have we found an add/sub which could be
                                            integrated in the lea?}
                        reference_reset(tmpref);
                        TmpRef.index := Taicpu(p).oper[1].reg;
                        TmpRef.scalefactor := 1 shl Taicpu(p).oper[0].val;
                        While TmpBool1 And
                              GetNextInstruction(p, hp1) And
                              (Tai(hp1).typ = ait_instruction) And
                              ((((Taicpu(hp1).opcode = A_ADD) Or
                                 (Taicpu(hp1).opcode = A_SUB)) And
                                (Taicpu(hp1).oper[1].typ = Top_Reg) And
                                (Taicpu(hp1).oper[1].reg = Taicpu(p).oper[1].reg)) or
                               (((Taicpu(hp1).opcode = A_INC) or
                                 (Taicpu(hp1).opcode = A_DEC)) and
                                (Taicpu(hp1).oper[0].typ = Top_Reg) and
                                (Taicpu(hp1).oper[0].reg = Taicpu(p).oper[1].reg))) Do
                          Begin
                            TmpBool1 := False;
                            If (Taicpu(hp1).oper[0].typ = Top_Const)
                              Then
                                Begin
                                  TmpBool1 := True;
                                  TmpBool2 := True;
                                  case Taicpu(hp1).opcode of
                                    A_ADD:
                                      inc(TmpRef.offset, longint(Taicpu(hp1).oper[0].val));
                                    A_SUB:
                                      dec(TmpRef.offset, longint(Taicpu(hp1).oper[0].val));
                                  end;
                                  asml.Remove(hp1);
                                  hp1.free;
                                End
                            Else
                              If (Taicpu(hp1).oper[0].typ = Top_Reg) And
                                 (((Taicpu(hp1).opcode = A_ADD) And
                                   (TmpRef.base = R_NO)) or
                                  (Taicpu(hp1).opcode = A_INC) or
                                  (Taicpu(hp1).opcode = A_DEC)) Then
                                Begin
                                  TmpBool1 := True;
                                  TmpBool2 := True;
                                  case Taicpu(hp1).opcode of
                                    A_ADD:
                                      TmpRef.base := Taicpu(hp1).oper[0].reg;
                                    A_INC:
                                      inc(TmpRef.offset);
                                    A_DEC:
                                      dec(TmpRef.offset);
                                  end;
                                  asml.Remove(hp1);
                                  hp1.free;
                                End;
                          End;
                        If TmpBool2 Or
                           ((aktoptprocessor < ClassP6) And
                            (Taicpu(p).oper[0].val <= 3) And
                            Not(CS_LittleSize in aktglobalswitches))
                          Then
                            Begin
                              If Not(TmpBool2) And
                                 (Taicpu(p).oper[0].val = 1)
                                Then
                                  Begin
                                    hp1 := Taicpu.Op_reg_reg(A_ADD,Taicpu(p).opsize,
                                               Taicpu(p).oper[1].reg, Taicpu(p).oper[1].reg)
                                  End
                                Else hp1 := Taicpu.op_ref_reg(A_LEA, S_L, TmpRef,
                                                Taicpu(p).oper[1].reg);
                              InsertLLItem(AsmL,p.previous, p.next, hp1);
                              p.free;
                              p := hp1;
                            End;
                      End
                    Else
                      If (aktoptprocessor < ClassP6) And
                         (Taicpu(p).oper[0].typ = top_const) And
                         (Taicpu(p).oper[1].typ = top_reg) Then
                        If (Taicpu(p).oper[0].val = 1)
                          Then
  {changes "shl $1, %reg" to "add %reg, %reg", which is the same on a 386,
   but faster on a 486, and Tairable in both U and V pipes on the Pentium
   (unlike shl, which is only Tairable in the U pipe)}
                            Begin
                              hp1 := Taicpu.Op_reg_reg(A_ADD,Taicpu(p).opsize,
                                         Taicpu(p).oper[1].reg, Taicpu(p).oper[1].reg);
                              InsertLLItem(AsmL,p.previous, p.next, hp1);
                              p.free;
                              p := hp1;
                            End
                          Else If (Taicpu(p).opsize = S_L) and
                                  (Taicpu(p).oper[0].val<= 3) Then
                    {changes "shl $2, %reg" to "lea (,%reg,4), %reg"
                             "shl $3, %reg" to "lea (,%reg,8), %reg}
                                 Begin
                                   reference_reset(tmpref);
                                   TmpRef.index := Taicpu(p).oper[1].reg;
                                   TmpRef.scalefactor := 1 shl Taicpu(p).oper[0].val;
                                   hp1 := Taicpu.Op_ref_reg(A_LEA,S_L,TmpRef, Taicpu(p).oper[1].reg);
                                   InsertLLItem(AsmL,p.previous, p.next, hp1);
                                   p.free;
                                   p := hp1;
                                 End
                End;
              A_SETcc :
                { changes
                    setcc (funcres)             setcc reg
                    movb (funcres), reg      to leave/ret
                    leave/ret                               }
                Begin
                  If (Taicpu(p).oper[0].typ = top_ref) And
                     GetNextInstruction(p, hp1) And
                     GetNextInstruction(hp1, hp2) And
                     (hp2.typ = ait_instruction) And
                     ((Taicpu(hp2).opcode = A_LEAVE) or
                      (Taicpu(hp2).opcode = A_RET)) And
                     (Taicpu(p).oper[0].ref^.Base = procinfo^.FramePointer) And
                     (Taicpu(p).oper[0].ref^.Index = R_NO) And
                     (Taicpu(p).oper[0].ref^.Offset >= procinfo^.Return_Offset) And
                     (hp1.typ = ait_instruction) And
                     (Taicpu(hp1).opcode = A_MOV) And
                     (Taicpu(hp1).opsize = S_B) And
                     (Taicpu(hp1).oper[0].typ = top_ref) And
                     RefsEqual(Taicpu(hp1).oper[0].ref^, Taicpu(p).oper[0].ref^) Then
                    Begin
                      Taicpu(p).LoadReg(0,Taicpu(hp1).oper[1].reg);
                      asml.Remove(hp1);
                      hp1.free;
                    End
                End;
              A_SUB:
                { * change "subl $2, %esp; pushw x" to "pushl x"}
                { * change "sub/add const1, reg" or "dec reg" followed by
                    "sub const2, reg" to one "sub ..., reg" }
                Begin
                  If (Taicpu(p).oper[0].typ = top_const) And
                     (Taicpu(p).oper[1].typ = top_reg) Then
                    If (Taicpu(p).oper[0].val = 2) And
                       (Taicpu(p).oper[1].reg = R_ESP) and
                       { Don't do the sub/push optimization if the sub }
                       { comes from setting up the stack frame (JM)    }
                       (not getLastInstruction(p,hp1) or
                        (hp1.typ <> ait_instruction) or
                        (Taicpu(hp1).opcode <> A_MOV) or
                        (Taicpu(hp1).oper[0].typ <> top_reg) or
                        (Taicpu(hp1).oper[0].reg <> R_ESP) or
                        (Taicpu(hp1).oper[1].typ <> top_reg) or
                        (Taicpu(hp1).oper[1].reg <> R_EBP)) then
                      Begin
                        hp1 := Tai(p.next);
                        While Assigned(hp1) And
                              (Tai(hp1).typ In [ait_instruction]+SkipInstr) And
                               not regReadByInstruction(R_ESP,hp1) and
                               not regModifiedByInstruction(R_ESP,hp1) do
                          hp1 := Tai(hp1.next);
                        If Assigned(hp1) And
                           (Tai(hp1).typ = ait_instruction) And
                           (Taicpu(hp1).opcode = A_PUSH) And
                           (Taicpu(hp1).opsize = S_W)
                          Then
                            Begin
                              Taicpu(hp1).changeopsize(S_L);
                              if Taicpu(hp1).oper[0].typ=top_reg then
                                Taicpu(hp1).LoadReg(0,rg.makeregsize(Taicpu(hp1).oper[0].reg,OS_32));
                              hp1 := Tai(p.next);
                              asml.Remove(p);
                              p.free;
                              p := hp1;
                              Continue
                            End;
                        If DoSubAddOpt(p) Then continue;
                      End
                    Else If DoSubAddOpt(p) Then Continue
                End;
               A_XOR:
                 If (Taicpu(p).oper[0].typ = top_reg) And
                    (Taicpu(p).oper[1].typ = top_reg) And
                    (Taicpu(p).oper[0].reg = Taicpu(p).oper[1].reg) then
                  { temporarily change this to 'mov reg,0' to make it easier }
                  { for the CSE. Will be changed back in pass 2              }
                   begin
                     Taicpu(p).opcode := A_MOV;
                     Taicpu(p).loadconst(0,0);
                   end;
            End;
            end; { if is_jmp }
          End;
{        ait_label:
          Begin
            If labelCanBeSkipped(Tai_label(p))
              Then
                Begin
                  hp1 := Tai(p.next);
                  asml.Remove(p);
                  p.free;
                  p := hp1;
                  Continue
                End;
          End;}
      End;
      updateUsedRegs(UsedRegs,p);
      p:=Tai(p.next);
    end;
end;

function isFoldableArithOp(hp1: Taicpu; reg: tregister): boolean;
begin
  IsFoldableArithOp := False;
  case hp1.opcode of
    A_ADD,A_SUB,A_OR,A_XOR,A_AND,A_SHL,A_SHR,A_SAR:
      isFoldableArithOp :=
        ((Taicpu(hp1).oper[0].typ = top_const) or
         ((Taicpu(hp1).oper[0].typ = top_reg) and
          (Taicpu(hp1).oper[0].reg <> reg))) and
        (Taicpu(hp1).oper[1].typ = top_reg) and
        (Taicpu(hp1).oper[1].reg = reg);
    A_INC,A_DEC:
      isFoldableArithOp :=
        (Taicpu(hp1).oper[0].typ = top_reg) and
        (Taicpu(hp1).oper[0].reg = reg);
  end;
end;


Procedure PeepHoleOptPass2(AsmL: TAAsmOutput; BlockStart, BlockEnd: Tai);

{$ifdef USECMOV}
  function CanBeCMOV(p : Tai) : boolean;

    begin
       CanBeCMOV:=assigned(p) and (p.typ=ait_instruction) and
         (Taicpu(p).opcode=A_MOV) and
         (Taicpu(p).opsize in [S_L,S_W]) and
         (Taicpu(p).oper[0].typ in [top_reg,top_ref]) and
         (Taicpu(p).oper[1].typ in [top_reg]);
    end;
{$endif USECMOV}

var
  p,hp1,hp2: Tai;
{$ifdef  USECMOV}
  l : longint;
  condition : tasmcond;
  hp3: Tai;
{$endif USECMOV}
  UsedRegs, TmpUsedRegs: TRegSet;

Begin
  P := BlockStart;
  UsedRegs := [];
  While (P <> BlockEnd) Do
    Begin
      UpdateUsedRegs(UsedRegs, Tai(p.next));
      Case p.Typ Of
        Ait_Instruction:
          Begin
            Case Taicpu(p).opcode Of
{$ifdef USECMOV}
              A_Jcc:
                if (aktspecificoptprocessor=ClassP6) then
                  begin
                     { check for
                            jCC   xxx
                            <several movs>
                         xxx:
                     }
                     l:=0;
                     GetNextInstruction(p, hp1);
                     while assigned(hp1) And
                       CanBeCMOV(hp1) do
                       begin
                          inc(l);
                          GetNextInstruction(hp1,hp1);
                       end;
                     if assigned(hp1) then
                       begin
                          if FindLabel(tasmlabel(Taicpu(p).oper[0].sym),hp1) then
                            begin
                               if (l<=4) and (l>0) then
                                 begin
                                    condition:=inverse_cond[Taicpu(p).condition];
                                    GetNextInstruction(p,hp1);
                                    asml.remove(p);
                                    p.free;
                                    p:=hp1;
                                    repeat
                                      Taicpu(hp1).opcode:=A_CMOVcc;
                                      Taicpu(hp1).condition:=condition;
                                      GetNextInstruction(hp1,hp1);
                                    until not(assigned(hp1)) or
                                      not(CanBeCMOV(hp1));
                                    asml.remove(hp1);
                                    hp1.free;
                                    continue;
                                 end;
                            end
                          else
                            begin
                               { check further for
                                      jCC   xxx
                                      <several movs>
                                      jmp   yyy
                              xxx:
                                      <several movs>
                              yyy:
                               }
                              { hp2 points to jmp xxx }
                              hp2:=hp1;
                              { skip hp1 to xxx }
                              GetNextInstruction(hp1, hp1);
                              if assigned(hp2) and
                                assigned(hp1) and
                                (l<=3) and
                                (hp2.typ=ait_instruction) and
                                (Taicpu(hp2).is_jmp) and
                                (Taicpu(hp2).condition=C_None) and
                                FindLabel(tasmlabel(Taicpu(p).oper[0].sym),hp1) then
                                 begin
                                    l:=0;
                                    while assigned(hp1) And
                                      CanBeCMOV(hp1) do
                                      begin
                                         inc(l);
                                         GetNextInstruction(hp1, hp1);
                                      end;
                                 end;
                              {
                              if assigned(hp1) and
                                FindLabel(tasmlabel(Taicpu(hp2).oper[0].sym),hp1) then
                                begin
                                   condition:=inverse_cond[Taicpu(p).condition];
                                   GetNextInstruction(p,hp1);
                                   asml.remove(p);
                                   p.free;
                                   p:=hp1;
                                   repeat
                                     Taicpu(hp1).opcode:=A_CMOVcc;
                                     Taicpu(hp1).condition:=condition;
                                     GetNextInstruction(hp1,hp1);
                                   until not(assigned(hp1)) or
                                     not(CanBeCMOV(hp1));
                                   hp2:=hp1.next;
                                   condition:=inverse_cond[condition];

                                   asml.remove(hp1.next)
                                   hp1.next.free;
                                   asml.remove(hp1);
                                   hp1.free;
                                   continue;
                                end;
                              }
                            end;
                       end;
                  end;
{$endif USECMOV}
              A_FSTP,A_FISTP:
                if doFpuLoadStoreOpt(asmL,p) then
                  continue;
              A_IMUL:
                begin
                  if ((Taicpu(p).oper[0].typ = top_const) or
                      (Taicpu(p).oper[0].typ = top_symbol)) and
                     (Taicpu(p).oper[1].typ = top_reg) and
                     ((Taicpu(p).oper[2].typ = top_none) or
                      ((Taicpu(p).oper[2].typ = top_reg) and
                       (Taicpu(p).oper[2].reg = Taicpu(p).oper[1].reg))) and
                     getLastInstruction(p,hp1) and
                     (hp1.typ = ait_instruction) and
                     (Taicpu(hp1).opcode = A_MOV) and
                     (Taicpu(hp1).oper[0].typ = top_reg) and
                     (Taicpu(hp1).oper[1].typ = top_reg) and
                     (Taicpu(hp1).oper[1].reg = Taicpu(p).oper[1].reg) then
              { change "mov reg1,reg2; imul y,reg2" to "imul y,reg1,reg2" }
                    begin
                      Taicpu(p).ops := 3;
                      Taicpu(p).loadreg(1,Taicpu(hp1).oper[0].reg);
                      Taicpu(p).loadreg(2,Taicpu(hp1).oper[1].reg);
                      asml.remove(hp1);
                      hp1.free;
                    end;
                end;
              A_MOV:
                Begin
                  If (Taicpu(p).oper[0].typ = top_reg) And
                     (Taicpu(p).oper[1].typ = top_reg) And
                     GetNextInstruction(p, hp1) And
                     (hp1.typ = ait_Instruction) And
                     ((Taicpu(hp1).opcode = A_MOV) or
                      (Taicpu(hp1).opcode = A_MOVZX) or
                      (Taicpu(hp1).opcode = A_MOVSX)) And
                     (Taicpu(hp1).oper[0].typ = top_ref) And
                     (Taicpu(hp1).oper[1].typ = top_reg) And
                     ((Taicpu(hp1).oper[0].ref^.Base = Taicpu(p).oper[1].reg) Or
                      (Taicpu(hp1).oper[0].ref^.Index = Taicpu(p).oper[1].reg)) And
                     (Reg32(Taicpu(hp1).oper[1].reg) = Taicpu(p).oper[1].reg) Then
              {mov reg1, reg2
               mov/zx/sx (reg2, ..), reg2      to   mov/zx/sx (reg1, ..), reg2}
                    Begin
                      If (Taicpu(hp1).oper[0].ref^.Base = Taicpu(p).oper[1].reg) Then
                        Taicpu(hp1).oper[0].ref^.Base := Taicpu(p).oper[0].reg;
                      If (Taicpu(hp1).oper[0].ref^.Index = Taicpu(p).oper[1].reg) Then
                        Taicpu(hp1).oper[0].ref^.Index := Taicpu(p).oper[0].reg;
                      asml.Remove(p);
                      p.free;
                      p := hp1;
                      Continue;
                    End
                  Else If (Taicpu(p).oper[0].typ = top_ref) And
                    GetNextInstruction(p,hp1) And
                    (hp1.typ = ait_instruction) And
                    IsFoldableArithOp(Taicpu(hp1),Taicpu(p).oper[1].reg) And
                    GetNextInstruction(hp1,hp2) And
                    (hp2.typ = ait_instruction) And
                    (Taicpu(hp2).opcode = A_MOV) And
                    (Taicpu(hp2).oper[0].typ = top_reg) And
                    (Taicpu(hp2).oper[0].reg = Taicpu(p).oper[1].reg) And
                    (Taicpu(hp2).oper[1].typ = top_ref) Then
                   Begin
                     TmpUsedRegs := UsedRegs;
                     UpdateUsedRegs(TmpUsedRegs,Tai(hp1.next));
                     If (RefsEqual(Taicpu(hp2).oper[1].ref^, Taicpu(p).oper[0].ref^) And
                         Not(RegUsedAfterInstruction(Taicpu(p).oper[1].reg,
                              hp2, TmpUsedRegs)))
                       Then
  { change   mov            (ref), reg            }
  {          add/sub/or/... reg2/$const, reg      }
  {          mov            reg, (ref)            }
  {          # release reg                        }
  { to       add/sub/or/... reg2/$const, (ref)    }
                     Begin
                       case Taicpu(hp1).opcode of
                         A_INC,A_DEC:
                           Taicpu(hp1).LoadRef(0,Taicpu(p).oper[0].ref^)
                         else
                           Taicpu(hp1).LoadRef(1,Taicpu(p).oper[0].ref^);
                       end;
                       asml.Remove(p);
                       asml.Remove(hp2);
                       p.free;
                       hp2.free;
                       p := hp1
                     End;
                   End
                End;
            End;
          End;
      End;
      p := Tai(p.next)
    End;
End;

Procedure PostPeepHoleOpts(AsmL: TAAsmOutput; BlockStart, BlockEnd: Tai);
var
  p,hp1,hp2: Tai;
Begin
  P := BlockStart;
  While (P <> BlockEnd) Do
    Begin
      Case p.Typ Of
        Ait_Instruction:
          Begin
            Case Taicpu(p).opcode Of
              A_CALL:
                If (AktOptProcessor < ClassP6) And
                   GetNextInstruction(p, hp1) And
                   (hp1.typ = ait_instruction) And
                   (Taicpu(hp1).opcode = A_JMP) And
                   (Taicpu(hp1).oper[0].typ = top_symbol) Then
                  Begin
                    hp2 := Taicpu.Op_sym(A_PUSH,S_L,Taicpu(hp1).oper[0].sym);
                    InsertLLItem(AsmL, p.previous, p, hp2);
                    Taicpu(p).opcode := A_JMP;
                    Taicpu(p).is_jmp := true;
                    asml.Remove(hp1);
                    hp1.free;
                  End;
              A_CMP:
                Begin
                  if (Taicpu(p).oper[0].typ = top_const) and
                     (Taicpu(p).oper[0].val = 0) and
                     (Taicpu(p).oper[1].typ = top_reg) then
                   {change "cmp $0, %reg" to "test %reg, %reg"}
                    begin
                      Taicpu(p).opcode := A_TEST;
                      Taicpu(p).loadreg(0,Taicpu(p).oper[1].reg);
                      continue;
                    end;
                End;
              A_MOV:
                if (Taicpu(p).oper[0].typ = Top_Const) And
                   (Taicpu(p).oper[0].val = 0) And
                   (Taicpu(p).oper[1].typ = Top_Reg) Then
                  { change "mov $0, %reg" into "xor %reg, %reg" }
                  Begin
                    Taicpu(p).opcode := A_XOR;
                    Taicpu(p).LoadReg(0,Taicpu(p).oper[1].reg);
                  End;
              A_MOVZX:
                { if register vars are on, it's possible there is code like }
                {   "cmpl $3,%eax; movzbl 8(%ebp),%ebx; je .Lxxx"           }
                { so we can't safely replace the movzx then with xor/mov,   }
                { since that would change the flags (JM)                    }
                if not(cs_regalloc in aktglobalswitches) then
                 Begin
                  If (Taicpu(p).oper[1].typ = top_reg) Then
                    If (Taicpu(p).oper[0].typ = top_reg)
                      Then
                        Case Taicpu(p).opsize of
                          S_BL:
                            Begin
                              If IsGP32Reg(Taicpu(p).oper[1].reg) And
                                 Not(CS_LittleSize in aktglobalswitches) And
                                 (aktoptprocessor = ClassP5)
                                Then
                                  {Change "movzbl %reg1, %reg2" to
                                   "xorl %reg2, %reg2; movb %reg1, %reg2" for Pentium and
                                   PentiumMMX}
                                  Begin
                                    hp1 := Taicpu.op_reg_reg(A_XOR, S_L,
                                               Taicpu(p).oper[1].reg, Taicpu(p).oper[1].reg);
                                    InsertLLItem(AsmL,p.previous, p, hp1);
                                    Taicpu(p).opcode := A_MOV;
                                    Taicpu(p).changeopsize(S_B);
                                    Taicpu(p).LoadReg(1,rg.makeregsize(Taicpu(p).oper[1].reg,OS_8));
                                  End;
                            End;
                        End
                      Else
                        If (Taicpu(p).oper[0].typ = top_ref) And
                           (Taicpu(p).oper[0].ref^.base <> Taicpu(p).oper[1].reg) And
                           (Taicpu(p).oper[0].ref^.index <> Taicpu(p).oper[1].reg) And
                           Not(CS_LittleSize in aktglobalswitches) And
                           IsGP32Reg(Taicpu(p).oper[1].reg) And
                           (aktoptprocessor = ClassP5) And
                           (Taicpu(p).opsize = S_BL)
                          Then
                            {changes "movzbl mem, %reg" to "xorl %reg, %reg; movb mem, %reg8" for
                             Pentium and PentiumMMX}
                            Begin
                              hp1 := Taicpu.Op_reg_reg(A_XOR, S_L, Taicpu(p).oper[1].reg,
                                         Taicpu(p).oper[1].reg);
                              Taicpu(p).opcode := A_MOV;
                              Taicpu(p).changeopsize(S_B);
                              Taicpu(p).LoadReg(1,rg.makeregsize(Taicpu(p).oper[1].reg,OS_8));
                              InsertLLItem(AsmL,p.previous, p, hp1);
                            End;
                 End;
              A_TEST, A_OR:
                {removes the line marked with (x) from the sequence
                 And/or/xor/add/sub/... $x, %y
                 test/or %y, %y   (x)
                 j(n)z _Label
                    as the first instruction already adjusts the ZF}
                 Begin
                   If OpsEqual(Taicpu(p).oper[0],Taicpu(p).oper[1]) Then
                    If GetLastInstruction(p, hp1) And
                      (Tai(hp1).typ = ait_instruction) Then
                     Case Taicpu(hp1).opcode Of
                       A_ADD, A_SUB, A_OR, A_XOR, A_AND{, A_SHL, A_SHR}:
                         Begin
                           If OpsEqual(Taicpu(hp1).oper[1],Taicpu(p).oper[0]) Then
                             Begin
                               hp1 := Tai(p.next);
                               asml.remove(p);
                               p.free;
                               p := Tai(hp1);
                               continue
                             End;
                         End;
                       A_DEC, A_INC, A_NEG:
                         Begin
                           If OpsEqual(Taicpu(hp1).oper[0],Taicpu(p).oper[0]) Then
                             Begin
                               Case Taicpu(hp1).opcode Of
                                 A_DEC, A_INC:
 {replace inc/dec with add/sub 1, because inc/dec doesn't set the carry flag}
                                   Begin
                                     Case Taicpu(hp1).opcode Of
                                       A_DEC: Taicpu(hp1).opcode := A_SUB;
                                       A_INC: Taicpu(hp1).opcode := A_ADD;
                                     End;
                                     Taicpu(hp1).Loadoper(1,Taicpu(hp1).oper[0]);
                                     Taicpu(hp1).LoadConst(0,1);
                                     Taicpu(hp1).ops:=2;
                                   End
                                 End;
                               hp1 := Tai(p.next);
                               asml.remove(p);
                               p.free;
                               p := Tai(hp1);
                               continue
                             End;
                         End
                     End
                 End;
            End;
          End;
      End;
      p := Tai(p.next)
    End;
End;



End.

{
  $Log$
  Revision 1.30  2002-07-26 21:15:43  florian
    * rewrote the system handling

  Revision 1.29  2002/07/01 18:46:34  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.28  2002/06/09 12:55:23  jonas
    * fixed detection of register usage

  Revision 1.27  2002/05/18 13:34:25  peter
    * readded missing revisions

  Revision 1.26  2002/05/16 19:46:52  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.24  2002/05/12 16:53:18  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.23  2002/04/21 15:40:49  carl
  * changeregsize -> rg.makeregsize

  Revision 1.22  2002/04/20 21:37:07  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant
  * removing frame pointer in routines is only available for : i386,m68k and vis targets

  Revision 1.21  2002/04/15 19:44:21  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic rg.makeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.20  2002/04/02 20:30:16  jonas
    + support for folding inc/dec in shl/add/sub sequences toa single lea
      instruction

  Revision 1.19  2002/04/02 13:01:58  jonas
    * fixed nasty bug in "and" peepholeoptimization that caused wrong
      optimizations after Peter's big location patch

  Revision 1.18  2002/03/31 20:26:40  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

}
