{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl and Jonas Maebe

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

Interface

Uses Aasm;

Procedure PeepHoleOptPass1(AsmL: PAasmOutput; BlockStart, BlockEnd: Pai);
Procedure PeepHoleOptPass2(AsmL: PAasmOutput; BlockStart, BlockEnd: Pai);

Implementation

Uses
  globtype,systems,
  globals,verbose,hcodegen,
  i386base,i386asm,
  DAOpt386;

Function RegUsedAfterInstruction(Reg: TRegister; p: Pai; Var UsedRegs: TRegSet): Boolean;
Begin
  UpdateUsedRegs(UsedRegs, Pai(p^.Next));
  RegUsedAfterInstruction := Reg in UsedRegs
End;

Procedure PeepHoleOptPass1(Asml: PAasmOutput; BlockStart, BlockEnd: Pai);
{First pass of peepholeoptimizations}

Var
  l : longint;
  p ,hp1, hp2: pai;
  TmpBool1, TmpBool2: Boolean;

  TmpRef: PReference;

  UsedRegs, TmpUsedRegs: TRegSet;

  Procedure GetFinalDestination(hp: pai386_labeled);
  {traces sucessive jumps to their final destination and sets it, e.g.
   je l1                je l3
   <code>               <code>
   l1:       becomes    l1:
   je l2                je l3
   <code>               <code>
   l2:                  l2:
   jmp l3               jmp l3}

  Var p1: pai;

    Function SkipLabels(hp: Pai): Pai;
    {skips all labels and returns the next "real" instruction; it is
     assumed that hp is of the type ait_label}
    Begin
      While assigned(hp^.next) and
            (pai(hp^.next)^.typ In SkipInstr + [ait_label]) Do
        hp := pai(hp^.next);
      If assigned(hp^.next)
        Then SkipLabels := pai(hp^.next)
        Else SkipLabels := hp;
    End;

  Begin
    If (hp^.lab^.nb >= LoLab) and
       (hp^.lab^.nb <= HiLab) and   {range check, a jump can go past an assembler block!}
       Assigned(LTable^[hp^.lab^.nb-LoLab].PaiObj) Then
      Begin
        p1 := LTable^[hp^.lab^.nb-LoLab].PaiObj; {the jump's destination}
        p1 := SkipLabels(p1);
        If (pai(p1)^.typ = ait_labeled_instruction) and
           ((pai386_labeled(p1)^.opcode = A_JMP) or
            ((pai386_labeled(p1)^.opcode = A_Jcc) and (pai386_labeled(p1)^.condition = hp^.condition)))
          Then
            Begin
              GetFinalDestination(pai386_labeled(p1));
              Dec(hp^.lab^.refcount);
              If (hp^.lab^.refcount = 0) Then
                hp^.lab^.is_used := False;
              hp^.lab := pai386_labeled(p1)^.lab;
              Inc(hp^.lab^.refcount);
            End
      End
  End;

Begin
  P := BlockStart;
  UsedRegs := [];
  While (P <> BlockEnd) Do
    Begin
      UpDateUsedRegs(UsedRegs, Pai(p^.next));
      Case P^.Typ Of
        Ait_Labeled_Instruction:
          Begin
  {the following if-block removes all code between a jmp and the next label,
   because it can never be executed}
            If (pai386_labeled(p)^.opcode = A_JMP) Then
              Begin
                hp1 := pai(p^.next);
                While GetNextInstruction(p, hp1) and
                      ((hp1^.typ <> ait_label) or
                { skip unused labels, they're not referenced anywhere }
                       Not(Pai_Label(hp1)^.l^.is_used)) Do
                  If (hp1^.typ <> ait_label) Then
                    Begin
                      AsmL^.Remove(hp1);
                      Dispose(hp1, done);
                    End;
               End;
            If GetNextInstruction(p, hp1) then
              Begin
                If (pai(hp1)^.typ=ait_labeled_instruction) and
                   (pai386_labeled(hp1)^.opcode=A_JMP) and
                   GetNextInstruction(hp1, hp2) And
                   FindLabel(pai386_labeled(p)^.lab, hp2)
                  Then
                    Begin
                      if pai386_labeled(p)^.opcode=A_Jcc then
                       pai386_labeled(p)^.condition:=inverse_cond[pai386_labeled(p)^.condition]
                      else
                       begin
                         If (LabDif <> 0) Then
                           GetFinalDestination(pai386_labeled(p));
                         p:=pai(p^.next);
                         continue;
                       end;
                      Dec(pai_label(hp2)^.l^.refcount);
                      If (pai_label(hp2)^.l^.refcount = 0) Then
                        pai_label(hp2)^.l^.is_used := False;
                      pai386_labeled(p)^.lab:=pai386_labeled(hp1)^.lab;
                      Inc(pai386_labeled(p)^.lab^.refcount);
                      asml^.remove(hp1);
                      dispose(hp1,done);
                      If (LabDif <> 0) Then GetFinalDestination(pai386_labeled(p));
                    end
                  else
                    if FindLabel(pai386_labeled(p)^.lab, hp1) then
                      Begin
                        hp2:=pai(hp1^.next);
                        asml^.remove(p);
                        dispose(p,done);
                        p:=hp2;
                        continue;
                      end
                    Else If (LabDif <> 0) Then GetFinalDestination(pai386_labeled(p));
              end
          end;
        ait_instruction:
          Begin
            If (Pai386(p)^.oper[0].typ = top_ref) Then
              With Pai386(p)^.oper[0].ref^ Do
                Begin
                  If (base = R_NO) And
                     (index <> R_NO) And
                     (scalefactor = 1)
                    Then
                      Begin
                        base := index;
                        index := R_NO
                      End
                 End;
            If (Pai386(p)^.oper[1].typ = top_ref) Then
              With Pai386(p)^.oper[1].ref^ Do
                Begin
                  If (base = R_NO) And
                     (index <> R_NO) And
                     (scalefactor = 1) Then
                    Begin
                      base := index;
                      index := R_NO
                    End
                End;
            Case Pai386(p)^.opcode Of
              A_AND:
                Begin
                  If (Pai386(p)^.oper[0].typ = top_const) And
                     (Pai386(p)^.oper[1].typ = top_reg) And
                     GetNextInstruction(p, hp1) And
                     (Pai(hp1)^.typ = ait_instruction) And
                     (Pai386(hp1)^.opcode = A_AND) And
                     (Pai386(hp1)^.oper[0].typ = top_const) And
                     (Pai386(hp1)^.oper[1].typ = top_reg) And
                     (Pai386(hp1)^.oper[1].reg = Pai386(hp1)^.oper[1].reg)
                    Then
{change "and const1, reg; and const2, reg" to "and (const1 and const2), reg"}
                      Begin
                        Pai386(p)^.LoadConst(0,Pai386(p)^.oper[0].val And Pai386(hp1)^.oper[0].val);
                        AsmL^.Remove(hp1);
                        Dispose(hp1, Done)
                      End
                    Else
{change "and x, reg; jxx" to "test x, reg", if reg is deallocated before the
 jump}
                      If (Pai386(p)^.oper[1].typ = top_reg) And
                         GetNextInstruction(p, hp1) And
                         (hp1^.typ = ait_labeled_instruction) And
                         Not(Pai386(p)^.oper[1].reg in UsedRegs) Then
                        Pai386(p)^.opcode := A_TEST;
                End;
              A_CMP:
                Begin
                  If (Pai386(p)^.oper[0].typ = top_const) And
                     (Pai386(p)^.oper[1].typ = top_reg) And
                     (Pai386(p)^.oper[0].val = 0) Then
                 {change "cmp $0, %reg" to "test %reg, %reg"}
                    Begin
                      Pai386(p)^.opcode := A_TEST;
                      Pai386(p)^.loadreg(0,Pai386(p)^.oper[1].reg);
                    End;
                End;
              A_FLD:
                Begin
                  If (Pai386(p)^.oper[0].typ = top_ref) And
                     GetNextInstruction(p, hp2) And
                     (hp2^.typ = Ait_Instruction) And
                     (Pai386(hp2)^.oper[0].typ = top_reg) And
                     (Pai386(hp2)^.oper[1].typ = top_reg) And
                     (Pai386(p)^.opsize in [S_FS, S_FL]) And
                     (Pai386(hp2)^.oper[0].reg = R_ST) And
                     (Pai386(hp2)^.oper[1].reg = R_ST1) Then
                    If GetLastInstruction(p, hp1) And
                       (hp1^.typ = Ait_Instruction) And
                       ((Pai386(hp1)^.opcode = A_FLD) Or
                        (Pai386(hp1)^.opcode = A_FST)) And
                       (Pai386(hp1)^.opsize = Pai386(p)^.opsize) And
                       (Pai386(hp1)^.oper[0].typ = top_ref) And
                       RefsEqual(Pai386(p)^.oper[0].ref^, Pai386(hp1)^.oper[0].ref^) Then
                      If ((Pai386(hp2)^.opcode = A_FMULP) Or
                          (Pai386(hp2)^.opcode = A_FADDP)) Then

                      { change                      to
                          fld/fst   mem1  (hp1)       fld/fst   mem1
                          fld       mem1  (p)         fadd/
                          faddp/                       fmul     st, st
                           fmulp  st, st1 (hp2) }
                        Begin
                          AsmL^.Remove(p);
                          Dispose(p, Done);
                          p := hp1;
                          If (Pai386(hp2)^.opcode = A_FADDP) Then
                            Pai386(hp2)^.opcode := A_FADD
                          Else
                            Pai386(hp2)^.opcode := A_FMUL;
                          Pai386(hp2)^.oper[1].reg := R_ST;
                        End
                      Else
                      { change              to
                          fld/fst mem1 (hp1)   fld/fst mem1
                          fld     mem1 (p)     fld      st}
                        Begin
                          Pai386(p)^.changeopsize(S_FL);
                          Pai386(p)^.loadreg(0,R_ST);
                        End
                    Else
                      Begin
                        Case Pai386(hp2)^.opcode Of
                          A_FMULP,A_FADDP,A_FSUBP,A_FDIVP,A_FSUBRP,A_FDIVRP:
                     { change                        to
                         fld/fst  mem1    (hp1)      fld/fst    mem1
                         fld      mem2    (p)        fxxx       mem2
                         fxxxp    st, st1 (hp2)                      }

                            Begin
                              Case Pai386(hp2)^.opcode Of
                                A_FADDP: Pai386(p)^.opcode := A_FADD;
                                A_FMULP: Pai386(p)^.opcode := A_FMUL;
                                A_FSUBP: Pai386(p)^.opcode := A_FSUBR;
                                A_FSUBRP: Pai386(p)^.opcode := A_FSUB;
                                A_FDIVP: Pai386(p)^.opcode := A_FDIVR;
                                A_FDIVRP: Pai386(p)^.opcode := A_FDIV;
                              End;
                              AsmL^.Remove(hp2);
                              Dispose(hp2, Done)
                            End
                        End
                      End
                End;
              A_FSTP,A_FISTP:
                Begin
                  If (Pai386(p)^.oper[0].typ = top_ref) And
                     GetNextInstruction(p, hp1) And
                     (Pai(hp1)^.typ = ait_instruction) And
                     (((Pai386(hp1)^.opcode = A_FLD) And
                       (Pai386(p)^.opcode = A_FSTP)) Or
                      ((Pai386(p)^.opcode = A_FISTP) And
                       (Pai386(hp1)^.opcode = A_FILD))) And
                     (Pai386(hp1)^.oper[0].typ = top_ref) And
                     (Pai386(hp1)^.opsize = Pai386(p)^.opsize) And
                     RefsEqual(Pai386(p)^.oper[0].ref^, Pai386(hp1)^.oper[0].ref^)
                    Then
                      Begin
                        If GetNextInstruction(hp1, hp2) And
                           (hp2^.typ = ait_instruction) And
                           ((Pai386(hp2)^.opcode = A_LEAVE) Or
                            (Pai386(hp2)^.opcode = A_RET)) And
                           (Pai386(p)^.oper[0].ref^.Base = ProcInfo.FramePointer) And
                           (Pai386(p)^.oper[0].ref^.Offset >= ProcInfo.RetOffset) And
                           (Pai386(p)^.oper[0].ref^.Index = R_NO)
                          Then
                            Begin
                              AsmL^.Remove(p);
                              AsmL^.Remove(hp1);
                              Dispose(p, Done);
                              Dispose(hp1, Done);
                              p := hp2;
                              Continue
                            End
                          Else
                   {fst can't store an extended value!}
                           If (Pai386(p)^.opsize <> S_FX) And
                              (Pai386(p)^.opsize <> S_IQ) Then
                             Begin
                               If (Pai386(p)^.opcode = A_FSTP) Then
                                 Pai386(p)^.opcode := A_FST
                               Else Pai386(p)^.opcode := A_FIST;
                               AsmL^.Remove(hp1);
                               Dispose(hp1, done)
                             End
                      End;
                End;
              A_IMUL:
                {changes certain "imul const, %reg"'s to lea sequences}
                Begin
                  If (Pai386(p)^.oper[0].typ = Top_Const) And
                     (Pai386(p)^.oper[1].typ = Top_Reg) And
                     (Pai386(p)^.opsize = S_L) Then
                    If (Pai386(p)^.oper[0].val = 1) Then
                      If (Pai386(p)^.oper[2].typ = Top_None) Then
                       {remove "imul $1, reg"}
                        Begin
                          hp1 := Pai(p^.Next);
                          AsmL^.Remove(p);
                          Dispose(p, Done);
                          p := hp1;
                          Continue;
                        End
                      Else
                       {change "imul $1, reg1, reg2" to "mov reg1, reg2"}
                        Begin
                          hp1 := New(Pai386, Op_Reg_Reg(A_MOV, S_L, Pai386(p)^.oper[1].reg,Pai386(p)^.oper[2].reg));
                          hp1^.fileinfo := p^.fileinfo;
                          InsertLLItem(AsmL, p^.previous, p^.next, hp1);
                          Dispose(p, Done);
                          p := hp1;
                        End
                    Else If
                     ((Pai386(p)^.oper[2].typ = Top_Reg) or
                      (Pai386(p)^.oper[2].typ = Top_None)) And
                     (aktoptprocessor < ClassP6) And
                     (Pai386(p)^.oper[0].val <= 12) And
                     Not(CS_LittleSize in aktglobalswitches) And
                     (Not(GetNextInstruction(p, hp1)) Or
                       {GetNextInstruction(p, hp1) And}
                       Not((Pai(hp1)^.typ = ait_labeled_instruction) And
                           ((pai386_labeled(hp1)^.opcode = A_Jcc) and (pai386_labeled(hp1)^.condition in [C_O,C_NO]))))
                    Then
                      Begin
                        New(TmpRef);
                        Reset_reference(tmpref^);
                        Case Pai386(p)^.oper[0].val Of
                          3: Begin
                             {imul 3, reg1, reg2 to
                                lea (reg1,reg1,2), reg2
                              imul 3, reg1 to
                                lea (reg1,reg1,2), reg1}
                               TmpRef^.base := Pai386(p)^.oper[1].reg;
                               TmpRef^.Index := Pai386(p)^.oper[1].reg;
                               TmpRef^.ScaleFactor := 2;
                               If (Pai386(p)^.oper[2].typ = Top_None) Then
                                 hp1 := New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, Pai386(p)^.oper[1].reg))
                               Else
                                 hp1 := New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, Pai386(p)^.oper[2].reg));
                               hp1^.fileinfo := p^.fileinfo;
                               InsertLLItem(AsmL,p^.previous, p^.next, hp1);
                               Dispose(p, Done);
                               p := hp1;
                            End;
                         5: Begin
                            {imul 5, reg1, reg2 to
                               lea (reg1,reg1,4), reg2
                             imul 5, reg1 to
                               lea (reg1,reg1,4), reg1}
                              TmpRef^.base := Pai386(p)^.oper[1].reg;
                              TmpRef^.Index := Pai386(p)^.oper[1].reg;
                              TmpRef^.ScaleFactor := 4;
                              If (Pai386(p)^.oper[2].typ = Top_None) Then
                                hp1 := New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, Pai386(p)^.oper[1].reg))
                              Else
                                hp1 := New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, Pai386(p)^.oper[2].reg));
                              hp1^.fileinfo:= p^.fileinfo;
                              InsertLLItem(AsmL,p^.previous, p^.next, hp1);
                              Dispose(p, Done);
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
                                    TmpRef^.Index := Pai386(p)^.oper[1].reg;
                                    If (Pai386(p)^.oper[2].typ = Top_Reg)
                                      Then
                                        Begin
                                          TmpRef^.base := Pai386(p)^.oper[2].reg;
                                          TmpRef^.ScaleFactor := 4;
                                          hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, Pai386(p)^.oper[1].reg));
                                        End
                                      Else
                                        Begin
                                          Dispose(TmpRef);
                                          hp1 :=  New(Pai386, op_reg_reg(A_ADD, S_L,
                                            Pai386(p)^.oper[1].reg,Pai386(p)^.oper[1].reg));
                                        End;
                                    hp1^.fileinfo := p^.fileinfo;
                                    InsertLLItem(AsmL,p, p^.next, hp1);
                                    New(TmpRef);
                                    Reset_reference(tmpref^);
                                    TmpRef^.Index := Pai386(p)^.oper[1].reg;
                                    TmpRef^.ScaleFactor := 2;
                                    If (Pai386(p)^.oper[2].typ = Top_Reg)
                                      Then
                                        Begin
                                          TmpRef^.base := R_NO;
                                          hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                            Pai386(p)^.oper[2].reg));
                                        End
                                      Else
                                        Begin
                                          TmpRef^.base := Pai386(p)^.oper[1].reg;
                                          hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, Pai386(p)^.oper[1].reg));
                                        End;
                                    hp1^.fileinfo := p^.fileinfo;
                                    InsertLLItem(AsmL,p^.previous, p^.next, hp1);
                                    Dispose(p, Done);
                                    p := Pai(hp1^.next);
                                  End
                                Else Dispose(TmpRef);
                            End;
                          9: Begin
                             {imul 9, reg1, reg2 to
                                lea (reg1,reg1,8), reg2
                              imul 9, reg1 to
                                lea (reg1,reg1,8), reg1}
                               TmpRef^.base := Pai386(p)^.oper[1].reg;
                               TmpRef^.Index := Pai386(p)^.oper[1].reg;
                               TmpRef^.ScaleFactor := 8;
                               If (Pai386(p)^.oper[2].typ = Top_None) Then
                                 hp1 := New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, Pai386(p)^.oper[1].reg))
                               Else
                                 hp1 := New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, Pai386(p)^.oper[2].reg));
                               hp1^.fileinfo := p^.fileinfo;
                               InsertLLItem(AsmL,p^.previous, p^.next, hp1);
                               Dispose(p, Done);
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
                                   If (Pai386(p)^.oper[2].typ = Top_Reg) Then
                                     hp1 :=  New(Pai386, op_reg_reg(A_ADD, S_L,
                                       Pai386(p)^.oper[2].reg,Pai386(p)^.oper[2].reg))
                                   Else
                                     hp1 := New(Pai386, op_reg_reg(A_ADD, S_L,
                                       Pai386(p)^.oper[1].reg,Pai386(p)^.oper[1].reg));
                                   hp1^.fileinfo := p^.fileinfo;
                                   InsertLLItem(AsmL,p, p^.next, hp1);
                                   TmpRef^.base := Pai386(p)^.oper[1].reg;
                                   TmpRef^.Index := Pai386(p)^.oper[1].reg;
                                   TmpRef^.ScaleFactor := 4;
                                   If (Pai386(p)^.oper[2].typ = Top_Reg)
                                     Then
                                       hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, Pai386(p)^.oper[2].reg))
                                     Else
                                       hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, Pai386(p)^.oper[1].reg));
                                   hp1^.fileinfo := p^.fileinfo;
                                   InsertLLItem(AsmL,p^.previous, p^.next, hp1);
                                   Dispose(p, Done);
                                   p := Pai(hp1^.next);
                                 End
                               Else Dispose(TmpRef);
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
                                     TmpRef^.Index := Pai386(p)^.oper[1].reg;
                                     If (Pai386(p)^.oper[2].typ = Top_Reg) Then
                                       Begin
                                         TmpRef^.base := Pai386(p)^.oper[2].reg;
                                         TmpRef^.ScaleFactor := 8;
                                         hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, Pai386(p)^.oper[2].reg));
                                       End
                                     Else
                                       Begin
                                         TmpRef^.base := R_NO;
                                         TmpRef^.ScaleFactor := 4;
                                         hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, Pai386(p)^.oper[1].reg));
                                       End;
                                     hp1^.fileinfo := p^.fileinfo;
                                     InsertLLItem(AsmL,p, p^.next, hp1);
                                     New(TmpRef);
                                     Reset_reference(tmpref^);
                                     TmpRef^.Index := Pai386(p)^.oper[1].reg;
                                     If (Pai386(p)^.oper[2].typ = Top_Reg) Then
                                       Begin
                                         TmpRef^.base := R_NO;
                                         TmpRef^.ScaleFactor := 4;
                                         hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, Pai386(p)^.oper[2].reg));
                                       End
                                     Else
                                       Begin
                                         TmpRef^.base := Pai386(p)^.oper[1].reg;
                                         TmpRef^.ScaleFactor := 2;
                                         hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, Pai386(p)^.oper[1].reg));
                                       End;
                                     hp1^.fileinfo := p^.fileinfo;
                                     InsertLLItem(AsmL,p^.previous, p^.next, hp1);
                                     Dispose(p, Done);
                                     p := Pai(hp1^.next);
                                   End
                                 Else Dispose(TmpRef);
                             End
                          Else Dispose(TmpRef);
                        End;
                      End;
                End;
              A_LEA:
                Begin
                {changes "lea (%reg1), %reg2" into "mov %reg1, %reg2"}
                  If (Pai386(p)^.oper[0].ref^.Base In [R_EAX..R_EDI]) And
                     (Pai386(p)^.oper[0].ref^.Index = R_NO) And
                     (Pai386(p)^.oper[0].ref^.Offset = 0) And
                     (Not(Assigned(Pai386(p)^.oper[0].ref^.Symbol))) Then
                    If (Pai386(p)^.oper[0].ref^.Base <> Pai386(p)^.oper[1].reg)
                      Then
                        Begin
                          hp1 := New(Pai386, op_reg_reg(A_MOV, S_L,Pai386(p)^.oper[0].ref^.Base,
                            Pai386(p)^.oper[1].reg));
                          hp1^.fileinfo := p^.fileinfo;
                         InsertLLItem(AsmL,p^.previous,p^.next, hp1);
                         Dispose(p, Done);
                         p := hp1;
                         Continue;
                       End
                     Else
                       Begin
                         hp1 := Pai(p^.Next);
                         AsmL^.Remove(p);
                         Dispose(p, Done);
                         p := hp1;
                         Continue;
                       End;
                End;
              A_MOV:
                Begin
                  TmpUsedRegs := UsedRegs;
                  If (Pai386(p)^.oper[1].typ = top_reg) And
                     (Pai386(p)^.oper[1].reg In [R_EAX, R_EBX, R_EDX, R_EDI]) And
                     GetNextInstruction(p, hp1) And
                     (Pai(hp1)^.typ = ait_instruction) And
                     (Pai386(hp1)^.opcode = A_MOV) And
                     (Pai386(hp1)^.oper[0].typ = top_reg) And
                     (Pai386(hp1)^.oper[0].reg = Pai386(p)^.oper[1].reg)
                    Then
                {we have "mov x, %treg; mov %treg, y}
                      If not(RegUsedAfterInstruction(Pai386(p)^.oper[1].reg, hp1, TmpUsedRegs)) then
                {we've got "mov x, %treg; mov %treg, y; with %treg is not used after }
                          Case Pai386(p)^.oper[0].typ Of
                            top_reg:
                              Begin
                                { change "mov %reg, %treg; mov %treg, y"
                                  to "mov %reg, y" }
                                Pai386(hp1)^.LoadOper(0,Pai386(p)^.oper[0]);
                                AsmL^.Remove(p);
                                Dispose(p, Done);
                                p := hp1;
                                continue;
                              End;
                            top_ref:
                              If (Pai386(hp1)^.oper[1].typ = top_reg) Then
                               Begin
                                 { change "mov mem, %treg; mov %treg, %reg"
                                   to "mov mem, %reg" }
                                 Pai386(p)^.Loadoper(1,Pai386(hp1)^.oper[1]);
                                 AsmL^.Remove(hp1);
                                 Dispose(hp1, Done);
                                 continue;
                               End;
                          End
                        Else
               {remove an instruction which never makes sense: we've got
                "mov mem, %reg1; mov %reg1, %edi" and then EDI isn't used anymore!}
{                          Begin
                            If (Pai386(hp1)^.oper[1].reg = R_EDI) And
                               Not(GetNextInstruction(hp1, hp2) And
                                   (Pai(hp2)^.typ = ait_instruction) And
                                   (Pai386(hp2)^.oper[1].typ = top_reg) And
                                   (Pai386(hp2)^.oper[1] = Pointer(R_ESI))) Then
                              Begin
                                AsmL^.Remove(hp1);
                                Dispose(hp1, Done);
                                Continue;
                              End
                          End}
                    Else
                  {Change "mov %reg1, %reg2; xxx %reg2, ???" to
                   "mov %reg1, %reg2; xxx %reg1, ???" to avoid a write/read
                   penalty}
                      If (Pai386(p)^.oper[0].typ = top_reg) And
                         (Pai386(p)^.oper[1].typ = top_reg) And
                         GetNextInstruction(p,hp1) And
                         (Pai(hp1)^.typ = ait_instruction) And
                         (Pai386(hp1)^.oper[0].typ = top_reg) And
                         (Pai386(hp1)^.oper[0].reg = Pai386(p)^.oper[1].reg)
                        Then
                  {we have "mov %reg1, %reg2; XXX %reg2, ???"}
                          Begin
                            If ((Pai386(hp1)^.opcode = A_OR) Or
                                (Pai386(hp1)^.opcode = A_TEST)) And
                               (Pai386(hp1)^.oper[1].typ = top_reg) And
                               (Pai386(hp1)^.oper[0].reg = Pai386(hp1)^.oper[1].reg)
                              Then
                   {we have "mov %reg1, %reg2; test/or %reg2, %reg2"}
                                Begin
                                  TmpUsedRegs := UsedRegs;
                                  If GetNextInstruction(hp1, hp2) And
                                     (hp2^.typ = ait_labeled_instruction) And
                                     Not(RegUsedAfterInstruction(Pai386(hp1)^.oper[0].reg, hp1, TmpUsedRegs))
                                    Then
                   {change "mov %reg1, %reg2; test/or %reg2, %reg2; jxx" to
                    "test %reg1, %reg1; jxx"}
                                      Begin
                                        Pai386(hp1)^.Loadoper(0,Pai386(p)^.oper[0]);
                                        Pai386(hp1)^.Loadoper(1,Pai386(p)^.oper[0]);
                                        AsmL^.Remove(p);
                                        Dispose(p, done);
                                        p := hp1;
                                        continue
                                      End
                                    Else
                   {change "mov %reg1, %reg2; test/or %reg2, %reg2" to
                    "mov %reg1, %reg2; test/or %reg1, %reg1"}
                                      Begin
                                        Pai386(hp1)^.Loadoper(0,Pai386(p)^.oper[0]);
                                        Pai386(hp1)^.Loadoper(1,Pai386(p)^.oper[0]);
                                      End;
                                End
{                              Else
                                If (Pai386(p^.next)^.opcode
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
                             (Pai(hp1)^.typ = ait_instruction)
                            Then
                              If ((Pai386(hp1)^.opcode = A_LEAVE) Or
                                  (Pai386(hp1)^.opcode = A_RET)) And
                                 (Pai386(p)^.oper[1].typ = top_ref) And
                                 (Pai386(p)^.oper[1].ref^.base = ProcInfo.FramePointer) And
                                 (Pai386(p)^.oper[1].ref^.offset >= ProcInfo.RetOffset) And
                                 (Pai386(p)^.oper[1].ref^.index = R_NO) And
                                 (Pai386(p)^.oper[0].typ = top_reg)
                                Then
                                  Begin
                                   AsmL^.Remove(p);
                                   Dispose(p, done);
                                   p := hp1;
                                 End
                               Else
                                 If (Pai386(p)^.oper[0].typ = top_reg) And
                                    (Pai386(p)^.oper[1].typ = top_ref) And
                                    (Pai386(p)^.opsize = Pai386(hp1)^.opsize) And
                                    (Pai386(hp1)^.opcode = A_CMP) And
                                    (Pai386(hp1)^.oper[1].typ = top_ref) And
                                    RefsEqual(Pai386(p)^.oper[1].ref^, Pai386(hp1)^.oper[1].ref^)
                                   Then
            {change "mov reg, mem1; cmp x, mem1" to "mov reg, mem1; cmp x, reg1"}
                                    Pai386(hp1)^.loadreg(1,Pai386(p)^.oper[0].reg);
                { Next instruction is also a MOV ? }
                  If GetNextInstruction(p, hp1) And
                     (pai(hp1)^.typ = ait_instruction) and
                     (Pai386(hp1)^.opcode = A_MOV) and
                     (Pai386(hp1)^.opsize = Pai386(p)^.opsize)
                  Then
                      Begin
                        If (Pai386(hp1)^.oper[0].typ = Pai386(p)^.oper[1].typ) and
                           (Pai386(hp1)^.oper[1].typ = Pai386(p)^.oper[0].typ)
                          Then
                            {mov reg1, mem1     or     mov mem1, reg1
                             mov mem2, reg2            mov reg2, mem2}
                            Begin
                              If OpsEqual(Pai386(hp1)^.oper[1],Pai386(p)^.oper[0]) Then
                            {mov reg1, mem1     or     mov mem1, reg1
                             mov mem2, reg1            mov reg2, mem1}
                                Begin
                                  If OpsEqual(Pai386(hp1)^.oper[0],Pai386(p)^.oper[1]) Then
                        { Removes the second statement from
                            mov reg1, mem1
                            mov mem1, reg1 }
                                    Begin
                                      AsmL^.remove(hp1);
                                      Dispose(hp1,done);
                                    End
                                  Else
                                    Begin
                                      TmpUsedRegs := UsedRegs;
                                      UpdateUsedRegs(TmpUsedRegs, Pai(hp1^.next));
                                      If (Pai386(p)^.oper[0].typ = top_reg) And
                                        { mov reg1, mem1
                                          mov mem2, reg1 }
                                         GetNextInstruction(hp1, hp2) And
                                         (hp2^.typ = ait_instruction) And
                                         (Pai386(hp2)^.opcode = A_CMP) And
                                         (Pai386(hp2)^.opsize = Pai386(p)^.opsize) and
                                         (Pai386(hp2)^.oper[0].typ = TOp_Ref) And
                                         (Pai386(hp2)^.oper[1].typ = TOp_Reg) And
                                         RefsEqual(Pai386(hp2)^.oper[0].ref^, Pai386(p)^.oper[1].ref^) And
                                         (Pai386(hp2)^.oper[1].reg = Pai386(p)^.oper[0].reg) And
                                         Not(RegUsedAfterInstruction(Pai386(p)^.oper[0].reg, hp2, TmpUsedRegs)) Then
                           { change                   to
                              mov reg1, mem1           mov reg1, mem1
                              mov mem2, reg1           cmp reg1, mem2
                              cmp mem1, reg1                          }
                                        Begin
                                          AsmL^.Remove(hp2);
                                          Dispose(hp2, Done);
                                          Pai386(hp1)^.opcode := A_CMP;
                                          Pai386(hp1)^.loadref(1,newreference(Pai386(hp1)^.oper[0].ref^));
                                          Pai386(hp1)^.loadreg(0,Pai386(p)^.oper[0].reg);
                                        End;
                                    End;
                                End
                                Else
                                  Begin
                                    If GetNextInstruction(hp1, hp2) And
                                       (Pai386(p)^.oper[0].typ = top_ref) And
                                       (Pai386(p)^.oper[1].typ = top_reg) And
                                       (Pai386(hp1)^.oper[0].typ = top_reg) And
                                       (Pai386(hp1)^.oper[0].reg = Pai386(p)^.oper[1].reg) And
                                       (Pai386(hp1)^.oper[1].typ = top_ref) And
                                       (Pai(hp2)^.typ = ait_instruction) And
                                       (Pai386(hp2)^.opcode = A_MOV) And
                                       (Pai386(hp2)^.opsize = Pai386(p)^.opsize) and
                                       (Pai386(hp2)^.oper[1].typ = top_reg) And
                                       (Pai386(hp2)^.oper[0].typ = top_ref) And
                                       RefsEqual(Pai386(hp2)^.oper[0].ref^, Pai386(hp1)^.oper[1].ref^)
                                      Then
                                        If (Pai386(p)^.oper[1].reg in [R_DI,R_EDI])
                                          Then
                                 {   mov mem1, %edi
                                     mov %edi, mem2
                                     mov mem2, reg2
                                  to:
                                     mov mem1, reg2
                                     mov reg2, mem2}
                                            Begin
                                              Pai386(p)^.Loadoper(1,Pai386(hp2)^.oper[1]);
                                              Pai386(hp1)^.loadoper(0,Pai386(hp2)^.oper[1]);
                                              AsmL^.Remove(hp2);
                                              Dispose(hp2,Done);
                                            End
                                          Else
                                 {   mov mem1, reg1         mov mem1, reg1
                                     mov reg1, mem2         mov reg1, mem2
                                     mov mem2, reg2         mov mem2, reg1
                                  to:                    to:
                                     mov mem1, reg1         mov mem1, reg1
                                     mov mem1, reg2         mov reg1, mem2
                                     mov reg1, mem2}
                                            Begin
                                              If (Pai386(p)^.oper[1].reg <> Pai386(hp2)^.oper[1].reg) Then
                                                Begin
                                                  Pai386(hp1)^.LoadRef(0,newreference(Pai386(p)^.oper[0].ref^));
                                                  Pai386(hp1)^.LoadReg(1,Pai386(hp2)^.oper[1].reg);
                                                End
                                              Else
                                                Begin
                                                  AsmL^.Remove(hp1);
                                                  Dispose(hp1, Done)
                                                End;
                                              Pai386(hp2)^.LoadRef(1,newreference(Pai386(hp2)^.oper[0].ref^));
                                              Pai386(hp2)^.LoadReg(0,Pai386(p)^.oper[1].reg);
                                            End;
                                  End;
                            End
                          Else
(*                          {movl [mem1],reg1
                             movl [mem1],reg2
                            to:
                              movl [mem1],reg1
                              movl reg1,reg2 }
                            If (Pai386(p)^.oper[0].typ = top_ref) and
                               (Pai386(p)^.oper[1].typ = top_reg) and
                               (Pai386(hp1)^.oper[0].typ = top_ref) and
                               (Pai386(hp1)^.oper[1].typ = top_reg) and
                               (Pai386(p)^.opsize = Pai386(hp1)^.opsize) and
                               RefsEqual(TReference(Pai386(p)^.oper[0]^),Pai386(hp1)^.oper[0]^.ref^) and
                               (Pai386(p)^.oper[1].reg<>Pai386(hp1)^.oper[0]^.ref^.base) and
                               (Pai386(p)^.oper[1].reg<>Pai386(hp1)^.oper[0]^.ref^.index) then
                              Pai386(hp1)^.LoadReg(0,Pai386(p)^.oper[1].reg)
                            Else*)
                            {   movl const1,[mem1]
                                movl [mem1],reg1
                             to:
                                movl const1,reg1
                                movl reg1,[mem1] }
                              If (Pai386(p)^.oper[0].typ = top_const) and
                                 (Pai386(p)^.oper[1].typ = top_ref) and
                                 (Pai386(hp1)^.oper[0].typ = top_ref) and
                                 (Pai386(hp1)^.oper[1].typ = top_reg) and
                                 (Pai386(p)^.opsize = Pai386(hp1)^.opsize) and
                                 RefsEqual(Pai386(hp1)^.oper[0].ref^,Pai386(p)^.oper[1].ref^) then
                                Begin
                                  Pai386(hp1)^.LoadReg(0,Pai386(hp1)^.oper[1].reg);
                                  Pai386(hp1)^.LoadRef(1,newreference(Pai386(p)^.oper[1].ref^));
                                  Pai386(p)^.LoadReg(1,Pai386(hp1)^.oper[0].reg);
                                End
                      End;
                       {changes "mov $0, %reg" into "xor %reg, %reg"}
                  If (Pai386(p)^.oper[0].typ = Top_Const) And
                     (Pai386(p)^.oper[0].val = 0) And
                     (Pai386(p)^.oper[1].typ = Top_Reg)
                    Then
                      Begin
                        Pai386(p)^.opcode := A_XOR;
                        Pai386(p)^.LoadReg(0,Pai386(p)^.oper[1].reg);
                      End;
                End;
              A_MOVZX:
                Begin
                {removes superfluous And's after movzx's}
                  If (Pai386(p)^.oper[1].typ = top_reg) And
                     GetNextInstruction(p, hp1) And
                     (Pai(hp1)^.typ = ait_instruction) And
                     (Pai386(hp1)^.opcode = A_AND) And
                     (Pai386(hp1)^.oper[0].typ = top_const) And
                     (Pai386(hp1)^.oper[1].typ = top_reg) And
                     (Pai386(hp1)^.oper[1].reg = Pai386(p)^.oper[1].reg)
                    Then
                      Case Pai386(p)^.opsize Of
                        S_BL, S_BW:
                          If (Pai386(hp1)^.oper[0].val = $ff) Then
                            Begin
                              AsmL^.Remove(hp1);
                              Dispose(hp1, Done);
                            End;
                        S_WL:
                          If (Pai386(hp1)^.oper[0].val = $ffff) Then
                            Begin
                              AsmL^.Remove(hp1);
                              Dispose(hp1, Done);
                            End;
                      End;
                {changes some movzx constructs to faster synonims (all examples
                 are given with eax/ax, but are also valid for other registers)}
                  If (Pai386(p)^.oper[1].typ = top_reg) Then
                    If (Pai386(p)^.oper[0].typ = top_reg) Then
                      Case Pai386(p)^.opsize of
                        S_BW:
                          Begin
                            If (Pai386(p)^.oper[0].reg = Reg16ToReg8(Pai386(p)^.oper[1].reg)) And
                               Not(CS_LittleSize In aktglobalswitches)
                              Then
                                {Change "movzbw %al, %ax" to "andw $0x0ffh, %ax"}
                                Begin
                                  Pai386(p)^.opcode := A_AND;
                                  Pai386(p)^.changeopsize(S_W);
                                  Pai386(p)^.LoadConst(0,$ff);
                                End
                              Else
                                If GetNextInstruction(p, hp1) And
                                   (Pai(hp1)^.typ = ait_instruction) And
                                   (Pai386(hp1)^.opcode = A_AND) And
                                   (Pai386(hp1)^.oper[0].typ = top_const) And
                                   (Pai386(hp1)^.oper[1].typ = top_reg) And
                                   (Pai386(hp1)^.oper[1].reg = Pai386(p)^.oper[1].reg)
                                  Then
                                    {Change "movzbw %reg1, %reg2; andw $const, %reg2"
                                     to "movw %reg1, reg2; andw $(const1 and $ff), %reg2"}
                                    Begin
                                      Pai386(p)^.opcode := A_MOV;
                                      Pai386(p)^.changeopsize(S_W);
                                      Pai386(p)^.LoadReg(0,Reg8ToReg16(Pai386(p)^.oper[0].reg));
                                      Pai386(hp1)^.LoadConst(0,Pai386(hp1)^.oper[0].val And $ff);
                                    End;
                          End;
                        S_BL:
                          Begin
                            If (Pai386(p)^.oper[0].reg = Reg32ToReg8(Pai386(p)^.oper[1].reg)) And
                               Not(CS_LittleSize in aktglobalswitches)
                              Then
                                {Change "movzbl %al, %eax" to "andl $0x0ffh, %eax"}
                                Begin
                                  Pai386(p)^.opcode := A_AND;
                                  Pai386(p)^.changeopsize(S_L);
                                  Pai386(p)^.loadconst(0,$ff)
                                End
                              Else
                                If GetNextInstruction(p, hp1) And
                                   (Pai(hp1)^.typ = ait_instruction) And
                                   (Pai386(hp1)^.opcode = A_AND) And
                                   (Pai386(hp1)^.oper[0].typ = top_const) And
                                   (Pai386(hp1)^.oper[1].typ = top_reg) And
                                   (Pai386(hp1)^.oper[1].reg = Pai386(p)^.oper[1].reg)
                                  Then
                                   {Change "movzbl %reg1, %reg2; andl $const, %reg2"
                                    to "movl %reg1, reg2; andl $(const1 and $ff), %reg2"}
                                    Begin
                                      Pai386(p)^.opcode := A_MOV;
                                      Pai386(p)^.changeopsize(S_L);
                                      Pai386(p)^.LoadReg(0,Reg8ToReg32(Pai386(p)^.oper[0].reg));
                                      Pai386(hp1)^.LoadConst(0,Pai386(hp1)^.oper[0].val And $ff);
                                    End
                          End;
                        S_WL:
                          Begin
                            If (Pai386(p)^.oper[0].reg = Reg32ToReg16(Pai386(p)^.oper[1].reg)) And
                               Not(CS_LittleSize In aktglobalswitches)
                              Then
                               {Change "movzwl %ax, %eax" to "andl $0x0ffffh, %eax"}
                                Begin
                                  Pai386(p)^.opcode := A_AND;
                                  Pai386(p)^.changeopsize(S_L);
                                  Pai386(p)^.LoadConst(0,$ffff);
                                End
                              Else
                                If GetNextInstruction(p, hp1) And
                                   (Pai(hp1)^.typ = ait_instruction) And
                                   (Pai386(hp1)^.opcode = A_AND) And
                                   (Pai386(hp1)^.oper[0].typ = top_const) And
                                   (Pai386(hp1)^.oper[1].typ = top_reg) And
                                   (Pai386(hp1)^.oper[1].reg = Pai386(p)^.oper[1].reg)
                                  Then
                                    {Change "movzwl %reg1, %reg2; andl $const, %reg2"
                                     to "movl %reg1, reg2; andl $(const1 and $ffff), %reg2"}
                                    Begin
                                      Pai386(p)^.opcode := A_MOV;
                                      Pai386(p)^.changeopsize(S_L);
                                      Pai386(p)^.LoadReg(0,Reg16ToReg32(Pai386(p)^.oper[0].reg));
                                      Pai386(hp1)^.LoadConst(0,Pai386(hp1)^.oper[0].val And $ffff);
                                    End;
                          End;
                        End
                      Else
                        If (Pai386(p)^.oper[0].typ = top_ref) Then
                          Begin
                            If GetNextInstruction(p, hp1) And
                               (Pai(hp1)^.typ = ait_instruction) And
                               (Pai386(hp1)^.opcode = A_AND) And
                               (Pai386(hp1)^.oper[0].typ = Top_Const) And
                               (Pai386(hp1)^.oper[1].typ = Top_Reg) And
                               (Pai386(hp1)^.oper[1].reg = Pai386(p)^.oper[1].reg) Then
                              Begin
                                Pai386(p)^.opcode := A_MOV;
                                Case Pai386(p)^.opsize Of
                                  S_BL:
                                    Begin
                                      Pai386(p)^.changeopsize(S_L);
                                      Pai386(hp1)^.LoadConst(0,Pai386(hp1)^.oper[0].val And $ff);
                                    End;
                                  S_WL:
                                    Begin
                                      Pai386(p)^.changeopsize(S_L);
                                      Pai386(hp1)^.LoadConst(0,Pai386(hp1)^.oper[0].val And $ffff);
                                    End;
                                  S_BW:
                                    Begin
                                      Pai386(p)^.changeopsize(S_W);
                                      Pai386(hp1)^.LoadConst(0,Pai386(hp1)^.oper[0].val And $ff);
                                    End;
                                End;
                              End;
                          End;
                End;
              A_POP:
                Begin
                   if (Pai386(p)^.oper[0].typ = top_reg) And
                      GetNextInstruction(p, hp1) And
                      (pai(hp1)^.typ=ait_instruction) and
                      (Pai386(hp1)^.opcode=A_PUSH) and
                      (Pai386(hp1)^.oper[0].typ = top_reg) And
                      (Pai386(hp1)^.oper[0].reg=Pai386(p)^.oper[0].reg) then
                     If (Not(cs_regalloc in aktglobalswitches)) Then
                       Begin
                         hp2:=pai(hp1^.next);
                         asml^.remove(p);
                         asml^.remove(hp1);
                         dispose(p,done);
                         dispose(hp1,done);
                         p:=hp2;
                         continue
                       End
                     Else
                       Begin
                         Pai386(p)^.opcode := A_MOV;
                         Pai386(p)^.Loadoper(1,Pai386(p)^.oper[0]);
                         New(TmpRef);
                         Reset_reference(tmpref^);
                         TmpRef^.base := R_ESP;
                         Pai386(p)^.LoadRef(0,TmpRef);
                         hp1 := Pai(p^.next);
                         AsmL^.Remove(hp1);
                         Dispose(hp1, Done)
                       End
                end;
              A_PUSH:
                Begin
                  If (Pai386(p)^.opsize = S_W) And
                     (Pai386(p)^.oper[0].typ = Top_Const) And
                     GetNextInstruction(p, hp1) And
                     (Pai(hp1)^.typ = ait_instruction) And
                     (Pai386(hp1)^.opcode = A_PUSH) And
                     (Pai386(hp1)^.oper[0].typ = Top_Const) And
                     (Pai386(hp1)^.opsize = S_W) Then
                    Begin
                      Pai386(p)^.changeopsize(S_L);
                      Pai386(p)^.LoadConst(0,Pai386(p)^.oper[0].val shl 16 + Pai386(hp1)^.oper[0].val);
                      AsmL^.Remove(hp1);
                      Dispose(hp1, Done)
                    End;
                End;
              A_SHL, A_SAL:
                Begin
                  If (Pai386(p)^.oper[0].typ = Top_Const) And
                     (Pai386(p)^.oper[1].typ = Top_Reg) And
                     (Pai386(p)^.opsize = S_L) And
                     (Pai386(p)^.oper[0].val <= 3)
                {Changes "shl const, %reg32; add const/reg, %reg32" to one lea statement}
                    Then
                      Begin
                        TmpBool1 := True; {should we check the next instruction?}
                        TmpBool2 := False; {have we found an add/sub which could be
                                            integrated in the lea?}
                        New(TmpRef);
                        Reset_reference(tmpref^);
                        TmpRef^.index := Pai386(p)^.oper[1].reg;
                        TmpRef^.scalefactor := 1 shl Pai386(p)^.oper[0].val;
                        While TmpBool1 And
                              GetNextInstruction(p, hp1) And
                              (Pai(hp1)^.typ = ait_instruction) And
                              ((Pai386(hp1)^.opcode = A_ADD) Or
                               (Pai386(hp1)^.opcode = A_SUB)) And
                              (Pai386(hp1)^.oper[1].typ = Top_Reg) And
                              (Pai386(hp1)^.oper[1].reg = Pai386(p)^.oper[1].reg) Do
                          Begin
                            TmpBool1 := False;
                            If (Pai386(hp1)^.oper[0].typ = Top_Const)
                              Then
                                Begin
                                  TmpBool1 := True;
                                  TmpBool2 := True;
                                  If Pai386(hp1)^.opcode = A_ADD Then
                                    Inc(TmpRef^.offset, Pai386(hp1)^.oper[0].val)
                                  Else
                                    Dec(TmpRef^.offset, Pai386(hp1)^.oper[0].val);
                                  AsmL^.Remove(hp1);
                                  Dispose(hp1, Done);
                                End
                              Else
                                If (Pai386(hp1)^.oper[0].typ = Top_Reg) And
                                   (Pai386(hp1)^.opcode = A_ADD) And
                                   (TmpRef^.base = R_NO) Then
                                  Begin
                                    TmpBool1 := True;
                                    TmpBool2 := True;
                                    TmpRef^.base := Pai386(hp1)^.oper[0].reg;
                                    AsmL^.Remove(hp1);
                                    Dispose(hp1, Done);
                                  End;
                          End;
                        If TmpBool2 Or
                           ((aktoptprocessor < ClassP6) And
                            (Pai386(p)^.oper[0].val <= 3) And
                            Not(CS_LittleSize in aktglobalswitches))
                          Then
                            Begin
                              If Not(TmpBool2) And
                                 (Pai386(p)^.oper[0].val = 1)
                                Then
                                  Begin
                                    Dispose(TmpRef);
                                    hp1 := new(Pai386,op_reg_reg(A_ADD,Pai386(p)^.opsize,
                                               Pai386(p)^.oper[1].reg, Pai386(p)^.oper[1].reg))
                                  End
                                Else hp1 := New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                                Pai386(p)^.oper[1].reg));
                              hp1^.fileinfo := p^.fileinfo;
                              InsertLLItem(AsmL,p^.previous, p^.next, hp1);
                              Dispose(p, Done);
                              p := hp1;
                            End;
                      End
                    Else
                      If (aktoptprocessor < ClassP6) And
                         (Pai386(p)^.oper[0].typ = top_const) And
                         (Pai386(p)^.oper[1].typ = top_reg) Then
                        If (Pai386(p)^.oper[0].val = 1)
                          Then
  {changes "shl $1, %reg" to "add %reg, %reg", which is the same on a 386,
   but faster on a 486, and pairable in both U and V pipes on the Pentium
   (unlike shl, which is only pairable in the U pipe)}
                            Begin
                              hp1 := new(Pai386,op_reg_reg(A_ADD,Pai386(p)^.opsize,
                                         Pai386(p)^.oper[1].reg, Pai386(p)^.oper[1].reg));
                              hp1^.fileinfo := p^.fileinfo;
                              InsertLLItem(AsmL,p^.previous, p^.next, hp1);
                              Dispose(p, done);
                              p := hp1;
                            End
                          Else If (Pai386(p)^.opsize = S_L) and
                                  (Pai386(p)^.oper[0].val<= 3) Then
                    {changes "shl $2, %reg" to "lea (,%reg,4), %reg"
                             "shl $3, %reg" to "lea (,%reg,8), %reg}
                                 Begin
                                   New(TmpRef);
                                   Reset_reference(tmpref^);
                                   TmpRef^.index := Pai386(p)^.oper[1].reg;
                                   TmpRef^.scalefactor := 1 shl Pai386(p)^.oper[0].val;
                                   hp1 := new(Pai386,op_ref_reg(A_LEA,S_L,TmpRef, Pai386(p)^.oper[1].reg));
                                   hp1^.fileinfo := p^.fileinfo;
                                   InsertLLItem(AsmL,p^.previous, p^.next, hp1);
                                   Dispose(p, done);
                                   p := hp1;
                                 End
                End;
              A_SAR, A_SHR:
                  {changes the code sequence
                   shr/sar const1, x
                   shl     const2, x
                   to either "sar/and", "shl/and" or just "and" depending on const1 and const2}
                Begin
                  If GetNextInstruction(p, hp1) And
                     (pai(hp1)^.typ = ait_instruction) and
                     (Pai386(hp1)^.opcode = A_SHL) and
                     (Pai386(p)^.oper[0].typ = top_const) and
                     (Pai386(hp1)^.oper[0].typ = top_const) and
                     (Pai386(hp1)^.opsize = Pai386(p)^.opsize) And
                     (Pai386(hp1)^.oper[1].typ = Pai386(p)^.oper[1].typ) And
                     OpsEqual(Pai386(hp1)^.oper[1], Pai386(p)^.oper[1])
                    Then
                      If (Pai386(p)^.oper[0].val > Pai386(hp1)^.oper[0].val) And
                         Not(CS_LittleSize In aktglobalswitches)
                        Then
                   { shr/sar const1, %reg
                     shl     const2, %reg
                      with const1 > const2 }
                          Begin
                            Pai386(p)^.LoadConst(0,Pai386(p)^.oper[0].val-Pai386(hp1)^.oper[0].val);
                            Pai386(hp1)^.opcode := A_AND;
                            l := 1 shl (Pai386(hp1)^.oper[0].val-1);
                            Case Pai386(p)^.opsize Of
                              S_L: Pai386(hp1)^.LoadConst(0,l Xor $ffffffff);
                              S_B: Pai386(hp1)^.LoadConst(0,l Xor $ff);
                              S_W: Pai386(hp1)^.LoadConst(0,l Xor $ffff);
                            End;
                          End
                        Else
                          If (Pai386(p)^.oper[0].val<Pai386(hp1)^.oper[0].val) And
                             Not(CS_LittleSize In aktglobalswitches)
                            Then
                   { shr/sar const1, %reg
                     shl     const2, %reg
                      with const1 < const2 }
                              Begin
                                Pai386(hp1)^.LoadConst(0,Pai386(hp1)^.oper[0].val-Pai386(p)^.oper[0].val);
                                Pai386(p)^.opcode := A_AND;
                                l := 1 shl (Pai386(p)^.oper[0].val-1);
                                Case Pai386(p)^.opsize Of
                                  S_L: Pai386(p)^.LoadConst(0,l Xor $ffffffff);
                                  S_B: Pai386(p)^.LoadConst(0,l Xor $ff);
                                  S_W: Pai386(p)^.LoadConst(0,l Xor $ffff);
                                End;
                              End
                            Else
                   { shr/sar const1, %reg
                     shl     const2, %reg
                      with const1 = const2 }
                              Begin
                                Pai386(p)^.opcode := A_AND;
                                l := 1 shl (Pai386(p)^.oper[0].val-1);
                                Case Pai386(p)^.opsize Of
                                  S_B: Pai386(p)^.LoadConst(0,l Xor $ff);
                                  S_W: Pai386(p)^.LoadConst(0,l Xor $ffff);
                                  S_L: Pai386(p)^.LoadConst(0,l Xor $ffffffff);
                                End;
                                AsmL^.remove(hp1);
                                dispose(hp1, done);
                              End;
                End;
              A_SETcc :
                Begin
                  If (Pai386(p)^.oper[0].typ = top_ref) And
                     GetNextInstruction(p, hp1) And
                     GetNextInstruction(hp1, hp2) And
                     (hp2^.typ = ait_instruction) And
                     ((Pai386(hp2)^.opcode = A_LEAVE) or
                      (Pai386(hp2)^.opcode = A_RET)) And
                     (Pai386(p)^.oper[0].ref^.Base = ProcInfo.FramePointer) And
                     (Pai386(p)^.oper[0].ref^.Index = R_NO) And
                     (Pai386(p)^.oper[0].ref^.Offset >= ProcInfo.RetOffset) And
                     (hp1^.typ = ait_instruction) And
                     (Pai386(hp1)^.opcode = A_MOV) And
                     (Pai386(hp1)^.opsize = S_B) And
                     (Pai386(hp1)^.oper[0].typ = top_ref) And
                     RefsEqual(Pai386(hp1)^.oper[0].ref^, Pai386(p)^.oper[0].ref^) Then
                    Begin
                      Pai386(p)^.LoadReg(0,Pai386(hp1)^.oper[1].reg);
                      AsmL^.Remove(hp1);
                      Dispose(hp1, Done)
                    End
                End;
              A_SUB:
                { * change "subl $2, %esp; pushw x" to "pushl x"}
                { * change "sub/add const1, reg" or "dec reg" followed by
                    "sub const2, reg" to one "sub ..., reg" }
                Begin
                  If (Pai386(p)^.oper[0].typ = top_const) And
                     (Pai386(p)^.oper[1].typ = top_reg) Then
                    If (Pai386(p)^.oper[0].val = 2) And
                       (Pai386(p)^.oper[1].reg = R_ESP) Then
                      Begin
                        hp1 := Pai(p^.next);
                        While Assigned(hp1) And
                              (Pai(hp1)^.typ In [ait_instruction]+SkipInstr) And
                               Not((Pai(hp1)^.typ = ait_instruction) And
                                   ((Pai386(hp1)^.opcode = A_CALL) or
                                    (Pai386(hp1)^.opcode = A_PUSH) or
                                    ((Pai386(hp1)^.opcode = A_MOV) And
                                     (Pai386(hp1)^.oper[1].typ = top_ref) And
                                     (Pai386(hp1)^.oper[1].ref^.base = R_ESP)))) do
                          hp1 := Pai(hp1^.next);
                        If Assigned(hp1) And
                            (Pai(hp1)^.typ = ait_instruction) And
                            (Pai386(hp1)^.opcode = A_PUSH) And
                            (Pai386(hp1)^.opsize = S_W)
                          Then
                            Begin
                              Pai386(hp1)^.changeopsize(S_L);
                              if Pai386(hp1)^.oper[0].typ=top_reg then
                                Pai386(hp1)^.LoadReg(0,Reg16ToReg32(Pai386(hp1)^.oper[0].reg));
                              hp1 := Pai(p^.next);
                              AsmL^.Remove(p);
                              Dispose(p, Done);
                              p := hp1;
                              Continue
                            End
                          Else
                            If GetLastInstruction(p, hp1) And
                               (Pai(hp1)^.typ = ait_instruction) And
                               (Pai386(hp1)^.opcode = A_SUB) And
                               (Pai386(hp1)^.oper[0].typ = top_const) And
                               (Pai386(hp1)^.oper[1].typ = top_reg) And
                               (Pai386(hp1)^.oper[1].reg = R_ESP)
                              Then
                                Begin
                                  Pai386(p)^.LoadConst(0,Pai386(p)^.oper[0].val+Pai386(hp1)^.oper[0].val);
                                  AsmL^.Remove(hp1);
                                  Dispose(hp1, Done);
                                End;
                      End
                    Else
                      If GetLastInstruction(p, hp1) And
                         (hp1^.typ = ait_instruction) And
                         (Pai386(hp1)^.opsize = Pai386(p)^.opsize) then
                        Case Pai386(hp1)^.opcode Of
                          A_DEC:
                            If (Pai386(hp1)^.oper[0].typ = top_reg) And
                               (Pai386(hp1)^.oper[0].reg = Pai386(p)^.oper[1].reg) Then
                              Begin
                                Pai386(p)^.LoadConst(0,Pai386(p)^.oper[0].val+1);
                                AsmL^.Remove(hp1);
                                Dispose(hp1, Done)
                              End;
                          A_SUB:
                            If (Pai386(hp1)^.oper[0].typ = top_const) And
                               (Pai386(hp1)^.oper[1].typ = top_reg) And
                               (Pai386(hp1)^.oper[1].reg = Pai386(p)^.oper[1].reg) Then
                              Begin
                                Pai386(p)^.LoadConst(0,Pai386(p)^.oper[0].val+Pai386(hp1)^.oper[0].val);
                                AsmL^.Remove(hp1);
                                Dispose(hp1, Done)
                              End;
                          A_ADD:
                            If (Pai386(hp1)^.oper[0].typ = top_const) And
                               (Pai386(hp1)^.oper[1].typ = top_reg) And
                               (Pai386(hp1)^.oper[1].reg = Pai386(p)^.oper[1].reg) Then
                              Begin
                                Pai386(p)^.LoadConst(0,Pai386(p)^.oper[0].val-Pai386(hp1)^.oper[0].val);
                                AsmL^.Remove(hp1);
                                Dispose(hp1, Done)
                              End;
                        End
                End;
              A_TEST, A_OR:
                {removes the line marked with (x) from the sequence
                 And/or/xor/add/sub/... $x, %y
                 test/or %y, %y   (x)
                 j(n)z _Label
                    as the first instruction already adjusts the ZF}
                 Begin
                   If OpsEqual(Pai386(p)^.oper[0],Pai386(p)^.oper[1]) And
                      GetLastInstruction(p, hp1) And
                      (pai(hp1)^.typ = ait_instruction) Then
                     Case Pai386(hp1)^.opcode Of
                       A_ADD, A_SUB, A_OR, A_XOR, A_AND, A_SHL, A_SHR:
                         Begin
                           If OpsEqual(Pai386(hp1)^.oper[1],Pai386(p)^.oper[0]) Then
                             Begin
                               hp1 := pai(p^.next);
                               asml^.remove(p);
                               dispose(p, done);
                               p := pai(hp1);
                               continue
                             End;
                         End;
                       A_DEC, A_INC, A_NEG:
                         Begin
                           If OpsEqual(Pai386(hp1)^.oper[0],Pai386(p)^.oper[0]) Then
                             Begin
                               Case Pai386(hp1)^.opcode Of
                                 A_DEC, A_INC:
 {replace inc/dec with add/sub 1, because inc/dec doesn't set the carry flag}
                                   Begin
                                     Case Pai386(hp1)^.opcode Of
                                       A_DEC: Pai386(hp1)^.opcode := A_SUB;
                                       A_INC: Pai386(hp1)^.opcode := A_ADD;
                                     End;
                                     Pai386(hp1)^.Loadoper(1,Pai386(hp1)^.oper[0]);
                                     Pai386(hp1)^.LoadConst(0,1);
                                   End
                                 End;
                               hp1 := pai(p^.next);
                               asml^.remove(p);
                               dispose(p, done);
                               p := pai(hp1);
                               continue
                             End;
                         End
                     End;
                 End;
            End;
          End;
{        ait_label:
          Begin
            If Not(Pai_Label(p)^.l^.is_used)
              Then
                Begin
                  hp1 := Pai(p^.next);
                  AsmL^.Remove(p);
                  Dispose(p, Done);
                  p := hp1;
                  Continue
                End;
          End;}
      End;
      p:=pai(p^.next);
    end;
end;

Procedure PeepHoleOptPass2(AsmL: PAasmOutput; BlockStart, BlockEnd: Pai);

var
  p,hp1,hp2: pai;
Begin
  P := BlockStart;
  While (P <> BlockEnd) Do
    Begin
      Case P^.Typ Of
        Ait_Instruction:
          Begin
            Case Pai386(p)^.opcode Of
              A_CALL:
                If (AktOptProcessor < ClassP6) And
                   GetNextInstruction(p, hp1) And
                   (hp1^.typ = ait_labeled_instruction) And
                   (pai386_labeled(hp1)^.opcode = A_JMP) Then
                  Begin
                    Inc(pai386_labeled(hp1)^.lab^.refcount);
                    hp2 := New(Pai386,op_sym(A_PUSH,S_L,NewAsmSymbol(Lab2Str(pai386_labeled(hp1)^.lab))));
                    hp2^.fileinfo := p^.fileinfo;
                    InsertLLItem(AsmL, p^.previous, p, hp2);
                    Pai386(p)^.opcode := A_JMP;
                    AsmL^.Remove(hp1);
                    Dispose(hp1, Done)
                  End;
              A_MOV:
                Begin
                  If (Pai386(p)^.oper[0].typ = top_reg) And
                     (Pai386(p)^.oper[1].typ = top_reg) And
                     GetNextInstruction(p, hp1) And
                     (hp1^.typ = ait_Instruction) And
                     (Pai386(hp1)^.opcode = A_MOV) And
                     (Pai386(hp1)^.oper[0].typ = top_ref) And
                     (Pai386(hp1)^.oper[1].typ = top_reg) And
                     ((Pai386(hp1)^.oper[0].ref^.Base = Pai386(p)^.oper[1].reg) Or
                      (Pai386(hp1)^.oper[0].ref^.Index = Pai386(p)^.oper[1].reg)) And
                     (Pai386(hp1)^.oper[1].reg = Pai386(p)^.oper[1].reg) Then
              {mov reg1, reg2
               mov (reg2, ..), reg2      to   mov (reg1, ..), reg2}
                    Begin
                      If (Pai386(hp1)^.oper[0].ref^.Base = Pai386(p)^.oper[1].reg) Then
                        Pai386(hp1)^.oper[0].ref^.Base := Pai386(p)^.oper[0].reg;
                      If (Pai386(hp1)^.oper[0].ref^.Index = Pai386(p)^.oper[1].reg) Then
                        Pai386(hp1)^.oper[0].ref^.Index := Pai386(p)^.oper[0].reg;
                      AsmL^.Remove(p);
                      Dispose(p, Done);
                      p := hp1;
                      Continue;
                    End;
                End;
              A_MOVZX:
                Begin
                  If (Pai386(p)^.oper[1].typ = top_reg) Then
                    If (Pai386(p)^.oper[0].typ = top_reg)
                      Then
                        Case Pai386(p)^.opsize of
                          S_BL:
                            Begin
                              If IsGP32Reg(Pai386(p)^.oper[1].reg) And
                                 Not(CS_LittleSize in aktglobalswitches) And
                                 (aktoptprocessor = ClassP5)
                                Then
                                  {Change "movzbl %reg1, %reg2" to
                                   "xorl %reg2, %reg2; movb %reg1, %reg2" for Pentium and
                                   PentiumMMX}
                                  Begin
                                    hp1 := New(Pai386, op_reg_reg(A_XOR, S_L,
                                               Pai386(p)^.oper[1].reg, Pai386(p)^.oper[1].reg));
                                    hp1^.fileinfo := p^.fileinfo;
                                    InsertLLItem(AsmL,p^.previous, p, hp1);
                                    Pai386(p)^.opcode := A_MOV;
                                    Pai386(p)^.changeopsize(S_B);
                                    Pai386(p)^.LoadReg(1,Reg32ToReg8(Pai386(p)^.oper[1].reg));
                                  End;
                            End;
                        End
                      Else
                        If (Pai386(p)^.oper[0].typ = top_ref) And
                           (Pai386(p)^.oper[0].ref^.base <> Pai386(p)^.oper[1].reg) And
                           (Pai386(p)^.oper[0].ref^.index <> Pai386(p)^.oper[1].reg) And
                           Not(CS_LittleSize in aktglobalswitches) And
                           IsGP32Reg(Pai386(p)^.oper[1].reg) And
                           (aktoptprocessor = ClassP5) And
                           (Pai386(p)^.opsize = S_BL)
                          Then
                            {changes "movzbl mem, %reg" to "xorl %reg, %reg; movb mem, %reg8" for
                             Pentium and PentiumMMX}
                            Begin
                              hp1 := New(Pai386,op_reg_reg(A_XOR, S_L, Pai386(p)^.oper[1].reg,
                                         Pai386(p)^.oper[1].reg));
                              hp1^.fileinfo := p^.fileinfo;
                              Pai386(p)^.opcode := A_MOV;
                              Pai386(p)^.changeopsize(S_B);
                              Pai386(p)^.LoadReg(1,Reg32ToReg8(Pai386(p)^.oper[1].reg));
                              InsertLLItem(AsmL,p^.previous, p, hp1);
                            End;
                End;
            End;
          End;
      End;
      p := Pai(p^.next)
    End;
End;

End.

{
 $Log$
 Revision 1.48  1999-05-01 13:24:34  peter
   * merged nasm compiler
   * old asm moved to oldasm/

 Revision 1.5  1999/04/30 12:36:50  jonas
   * fix from Brussels: call/jmp => push/jmp transformation didn't
     count correctly the jmp references

 Revision 1.4  1999/04/10 16:14:11  peter
   * fixed optimizer

 Revision 1.3  1999/04/09 08:33:18  peter
   * fixed mov reg,treg;mov treg,x bug

 Revision 1.2  1999/03/29 16:05:51  peter
   * optimizer working for ag386bin

 Revision 1.1  1999/03/26 00:01:15  peter
   * first things for optimizer (compiles but cycle crashes)

 Revision 1.39  1999/02/26 00:48:22  peter
   * assembler writers fixed for ag386bin

 Revision 1.38  1999/02/25 21:02:44  peter
   * ag386bin updates
   + coff writer

 Revision 1.37  1999/02/22 02:15:30  peter
   * updates for ag386bin

 Revision 1.36  1999/01/04 22:04:15  jonas
   + mov reg, mem1    to    mov reg, mem1
      mov mem2, reg           cmp reg, mem2
      cmp mem1, reg
     # reg released

 Revision 1.35  1999/01/04 12:58:55  jonas
   * no fistp/fild optimization for S_IQ (fistq doesn't exist)

 Revision 1.34  1998/12/29 18:48:17  jonas
   + optimize pascal code surrounding assembler blocks

 Revision 1.33  1998/12/23 15:16:21  jonas
   * change "inc x/dec x; test x, x"  to "add 1, x/sub 1,x" because inc and dec
     don't affect the carry flag (test does). This *doesn't* fix the problem with
    cardinal, that's a cg issue.

 Revision 1.32  1998/12/16 12:09:29  jonas
   * fixed fistp/fild optimization

 Revision 1.31  1998/12/15 22:30:39  jonas
   + change "sub/add const1, reg" or "dec reg" followed by "sub const2, reg" to one
     "sub const3, reg"
   * some small cleaning up

 Revision 1.30  1998/12/15 15:43:20  jonas
   * fixed bug in shr/shl optimization

 Revision 1.29  1998/12/15 11:53:54  peter
   * removed commentlevel

 Revision 1.28  1998/12/14 22:01:45  jonas
   - removed $ifdef ver0_99_11's

 Revision 1.27  1998/12/11 00:03:35  peter
   + globtype,tokens,version unit splitted from globals

 Revision 1.26  1998/12/09 18:16:13  jonas
   * corrected small syntax error in part between ifdef ver0_99_11
   + added fistp/fild optimization between ifdef ver0_99_11

 Revision 1.25  1998/12/02 16:23:29  jonas
   * changed "if longintvar in set" to case or "if () or () .." statements
   * tree.pas: changed inlinenumber (and associated constructor/vars) to a byte

 Revision 1.24  1998/11/26 15:41:45  jonas
   + change "setxx mem; movb mem, reg8" to "setxx reg8" if mem is a local
     variable/parameter or function result (between $ifdef ver0_99_11)

 Revision 1.23  1998/11/03 16:26:09  jonas
   * "call x;jmp y" optimization not done anymore for P6 and equivalents
   * made FPU optimizations simpler and more effective

 Revision 1.22  1998/10/29 18:37:55  jonas
   + change "call x; jmp y" to "push y; jmp x" (suggestion from Daniel)

 Revision 1.19  1998/10/23 15:38:23  jonas
   + some small FPU peephole optimizations (use value in FP regs instead of loading it
      from memory if possible, mostly with var1+var1 and var1*var1)

 Revision 1.18  1998/10/05 14:41:14  jonas
   * fixed small memory leak
   * fixed small inefficiency
   * tested multiple line comments ability of my new MacCVS client :)

 Revision 1.17  1998/10/02 17:29:56  jonas
   + removal of "lea (reg), reg)", "imul $1, reg", change "mov reg1, reg2; mov (reg2), reg2" to "mov (reg1), reg2"

 Revision 1.16  1998/10/01 20:19:57  jonas
   * moved UpdateUsedRegs (+ bugfix) to daopt386

 Revision 1.15  1998/09/30 12:18:29  peter
   * fixed subl $2,esp;psuhw bug

 Revision 1.14  1998/09/20 17:11:51  jonas
   * released REGALLOC

 Revision 1.13  1998/09/16 18:00:00  jonas
   * optimizer now completely dependant on GetNext/GetLast instruction, works again with -dRegAlloc

 Revision 1.12  1998/09/15 14:05:22  jonas
   * fixed optimizer incompatibilities with freelabel code in psub

 Revision 1.11  1998/08/28 10:57:02  peter
   * removed warnings

 Revision 1.10  1998/08/27 15:17:50  florian
   * reinstated Jonas' bugfix

 Revision 1.9  1998/08/25 16:58:59  pierre
   * removed a line that add no sense and
     introduce garbage in the asmlist
     (uninitialized data !)

 Revision 1.7  1998/08/19 16:07:53  jonas
   * changed optimizer switches + cleanup of DestroyRefs in daopt386.pas

 Revision 1.6  1998/08/10 14:50:14  peter
   + localswitches, moduleswitches, globalswitches splitting

 Revision 1.5  1998/08/06 19:40:28  jonas
   * removed $ before and after Log in comment

 Revision 1.4  1998/08/05 16:27:17  jonas
   * fstp/fld bugfix (fstt does not exist)

 Revision 1.3  1998/08/05 16:00:15  florian
   * some fixes for ansi strings
   * log to Log changed

}
