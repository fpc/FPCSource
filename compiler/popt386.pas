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

Procedure PeepHoleOptPass1(AsmL: PAasmOutput);
Procedure PeepHoleOptPass2(AsmL: PAasmOutput);

Implementation

Uses globals, systems, verbose, hcodegen
   {$ifdef i386}
     ,i386, DAOpt386
   {$endif i386}
     ;

Function RegUsedAfterInstruction(Reg: TRegister; p: Pai; Var UsedRegs: TRegSet): Boolean;
Begin
  UpdateUsedRegs(UsedRegs, Pai(p^.Next));
  RegUsedAfterInstruction := Reg in UsedRegs
End;

Procedure PeepHoleOptPass1(Asml: PAasmOutput);
{First pass of peepholeoptimizations}

Var
  p ,hp1, hp2: pai;
  TmpBool1, TmpBool2: Boolean;

  TmpRef: PReference;

  UsedRegs, TmpUsedRegs: TRegSet;

  Procedure GetFinalDestination(hp: pai_labeled);
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
       (hp^.lab^.nb <= HiLab) and   {range check, necessary?}
       (Pointer(LTable^[hp^.lab^.nb-LoLab].PaiObj) <> Pointer(0)) Then
      Begin
        p1 := LTable^[hp^.lab^.nb-LoLab].PaiObj; {the jump's destination}
        p1 := SkipLabels(p1);
        If (pai(p1)^.typ = ait_labeled_instruction) and
           ((pai_labeled(p1)^._operator = A_JMP) or
            (pai_labeled(p1)^._operator = hp^._operator))
          Then
            Begin
              GetFinalDestination(pai_labeled(p1));
              Dec(hp^.lab^.refcount);
              If (hp^.lab^.refcount = 0) Then
                hp^.lab^.is_used := False;
              hp^.lab := pai_labeled(p1)^.lab;
              Inc(hp^.lab^.refcount);
            End
      End
  End;

Begin
  P := Pai(AsmL^.First);
  UsedRegs := [];
  While Assigned(P) Do
    Begin
      UpDateUsedRegs(UsedRegs, Pai(p^.next));
      Case P^.Typ Of
        Ait_Labeled_Instruction:
          Begin
  {the following if-block removes all code between a jmp and the next label,
   because it can never be executed}
            If (pai_labeled(p)^._operator = A_JMP) Then
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
                   (pai_labeled(hp1)^._operator=A_JMP) and
                   GetNextInstruction(hp1, hp2) And
                   FindLabel(pai_labeled(p)^.lab, hp2)
                  Then
                    Begin
                      Case pai_labeled(p)^._operator Of
                        A_JE : pai_labeled(p)^._operator:=A_JNE;
                        A_JNE : pai_labeled(p)^._operator:=A_JE;
                        A_JL : pai_labeled(p)^._operator:=A_JGE;
                        A_JG : pai_labeled(p)^._operator:=A_JLE;
                        A_JLE : pai_labeled(p)^._operator:=A_JG;
                        A_JGE : pai_labeled(p)^._operator:=A_JL;
                        A_JNZ : pai_labeled(p)^._operator:=A_JZ;
                        A_JNO : pai_labeled(p)^._operator:=A_JO;
                        A_JZ : pai_labeled(p)^._operator:=A_JNZ;
                        A_JS : pai_labeled(p)^._operator:=A_JNS;
                        A_JNS : pai_labeled(p)^._operator:=A_JS;
                        A_JO : pai_labeled(p)^._operator:=A_JNO;
                        A_JC : pai_labeled(p)^._operator:=A_JNC;
                        A_JNC : pai_labeled(p)^._operator:=A_JC;
                        A_JA : pai_labeled(p)^._operator:=A_JBE;
                        A_JAE : pai_labeled(p)^._operator:=A_JB;
                        A_JB : pai_labeled(p)^._operator:=A_JAE;
                        A_JBE : pai_labeled(p)^._operator:=A_JA;
                        Else
                          begin
                            If (LabDif <> 0) Then GetFinalDestination(pai_labeled(p));
                            p:=pai(p^.next);
                            continue;
                          end;
                      end;
                      Dec(pai_label(hp2)^.l^.refcount);
                      If (pai_label(hp2)^.l^.refcount = 0) Then
                        Begin
                          pai_label(hp2)^.l^.is_used := False;
{                          AsmL^.remove(hp2);
                          Dispose(hp2, done);}
                        End;
                      pai_labeled(p)^.lab:=pai_labeled(hp1)^.lab;
                      Inc(pai_labeled(p)^.lab^.refcount);
                      asml^.remove(hp1);
                      dispose(hp1,done);
                      If (LabDif <> 0) Then GetFinalDestination(pai_labeled(p));
                    end
                  else
                    if FindLabel(pai_labeled(p)^.lab, hp1) then
                      Begin
                        hp2:=pai(hp1^.next);
                        asml^.remove(p);
                        dispose(p,done);
                        p:=hp2;
                        continue;
                      end
                    Else If (LabDif <> 0) Then GetFinalDestination(pai_labeled(p));
              end
          end;
        ait_instruction:
          Begin
            If (Pai386(p)^.op1t = top_ref) Then
              With TReference(Pai386(p)^.op1^) Do
                Begin
                  If (base = R_NO) And
                     (scalefactor = 1)
                    Then
                      Begin
                        base := index;
                        index := r_no
                      End
                 End;
            If (Pai386(p)^.op2t = top_ref) Then
              With TReference(Pai386(p)^.op2^) Do
                Begin
                  If (base = R_NO) And
                     (scalefactor = 1)
                    Then
                      Begin
                        base := index;
                        index := r_no
                      End
                End;
            Case Pai386(p)^._operator Of
              A_AND:
                Begin
                  If (Pai386(p)^.op1t = top_const) And
                     (Pai386(p)^.op2t = top_reg) And
                     GetNextInstruction(p, hp1) And
                     (Pai(hp1)^.typ = ait_instruction) And
                     (Pai386(hp1)^._operator = A_AND) And
                     (Pai386(hp1)^.op1t = top_const) And
                     (Pai386(hp1)^.op2t = top_reg) And
                     (Pai386(hp1)^.op2 = Pai386(hp1)^.op2)
                    Then
{change "and const1, reg; and const2, reg" to "and (const1 and const2), reg"}
                      Begin
                        Pai386(p)^.op1 := Pointer(Longint(Pai386(p)^.op1) And Longint(Pai386(hp1)^.op1));
                        AsmL^.Remove(hp1);
                        Dispose(hp1, Done)
                      End
                    Else
{change "and x, reg; jxx" to "test x, reg", if reg is deallocated before the
 jump}
                      If (Pai386(p)^.op2t = top_reg) And
                         GetNextInstruction(p, hp1) And
                         (hp1^.typ = ait_labeled_instruction) And
                         Not(TRegister(Pai386(p)^.op2) in UsedRegs)
                        Then Pai386(p)^._operator := A_TEST;
                End;
              A_CMP:
                Begin
                  If (Pai386(p)^.op1t = top_const) And
                     (Pai386(p)^.op2t = top_reg) And
                     (Pai386(p)^.op1 = Pointer(0)) Then
                 {change "cmp $0, %reg" to "test %reg, %reg"}
                    Begin
                      Pai386(p)^._operator := A_TEST;
                      Pai386(p)^.opxt := Top_reg+Top_reg shl 4;
                      Pai386(p)^.op1 := Pai386(p)^.op2;
                    End;
                End;
              A_FLD:
                Begin
                  If (Pai386(p)^.op1t = top_ref) And
                     GetNextInstruction(p, hp2) And
                     (hp2^.typ = Ait_Instruction) And
                     (Pai386(hp2)^.Op1t = top_reg) And
                     (Pai386(hp2)^.Op2t = top_reg) And
                     (Pai386(p)^.Size in [S_FS, S_FL]) And
                     (TRegister(Pai386(hp2)^.Op1) = R_ST) And
                     (TRegister(Pai386(hp2)^.Op2) = R_ST1) Then
                    If GetLastInstruction(p, hp1) And
                       (hp1^.typ = Ait_Instruction) And
                       ((Pai386(hp1)^._operator = A_FLD) Or
                        (Pai386(hp1)^._operator = A_FST)) And
                       (Pai386(hp1)^.size = Pai386(p)^.size) And
                       (Pai386(hp1)^.op1t = top_ref) And
                       RefsEqual(TReference(Pai386(p)^.Op1^), TReference(Pai386(hp1)^.Op1^)) Then
                      If (Pai386(hp2)^._operator in [A_FMULP, A_FADDP]) Then

                      { change                      to
                          fld/fst   mem1              fld/fst   mem1
                          fld       mem1              fadd/
                          faddp/                       fmul     st, st
                           fmulp  st, st1 }
                        Begin
                          AsmL^.Remove(p);
                          Dispose(p, Done);
                          p := hp1;
                          If (Pai386(hp2)^._operator = A_FADDP)
                            Then Pai386(hp2)^._operator := A_FADD
                            Else Pai386(hp2)^._operator := A_FMUL;
                          Pai386(hp2)^.op2 := Pointer(R_ST);
                        End
                      Else
                      { change              to
                          fld/fst mem1         fld/fst mem1
                          fld      mem1         fld      st}
                        Begin
                          Pai386(p)^.Size := S_FL;
                          Clear_Reference(TReference(Pai386(p)^.Op1^));
                          Pai386(p)^.Op1 := Pointer(R_ST);
                          Pai386(p)^.Opxt := top_reg;
                        End
                    Else
                      Begin
                        If (Pai386(hp2)^._operator in [A_FMULP, A_FADDP, A_FSUBP, A_FDIVP, A_FSUBRP, A_FDIVRP]) Then
                     { change                        to
                         fld/fst  mem1    (hp1)      fld/fst    mem1
                         fld      mem2    (p)        fxxx       mem2
                         fxxxp    st, st1 (hp2)                      }

                          Begin
                            Case Pai386(hp2)^._operator Of
                              A_FADDP: Pai386(p)^._operator := A_FADD;
                              A_FMULP: Pai386(p)^._operator := A_FMUL;
                              A_FSUBP: Pai386(p)^._operator := A_FSUBR;
                              A_FSUBRP: Pai386(p)^._operator := A_FSUB;
                              A_FDIVP: Pai386(p)^._operator := A_FDIVR;
                              A_FDIVRP: Pai386(p)^._operator := A_FDIV;
                            End;
                            AsmL^.Remove(hp2);
                            Dispose(hp2, Done)
                          End
                      End
                End;
              A_FSTP:
                Begin
                  If (Pai386(p)^.op1t = top_ref) And
                     GetNextInstruction(p, hp1) And
                     (Pai(hp1)^.typ = ait_instruction) And
                     (Pai386(hp1)^._operator = A_FLD) And
                     (Pai386(hp1)^.op1t = top_ref) And
                     (Pai386(hp1)^.Size = Pai386(p)^.Size) And
                     RefsEqual(TReference(Pai386(p)^.op1^), TReference(Pai386(hp1)^.op1^))
                    Then
                      Begin
                        If GetNextInstruction(hp1, hp2) And
                           (hp2^.typ = ait_instruction) And
                           ((Pai386(hp2)^._operator = A_LEAVE) Or
                            (Pai386(hp2)^._operator = A_RET)) And
                           (TReference(Pai386(p)^.op1^).Base = ProcInfo.FramePointer) And
                           (TReference(Pai386(p)^.op1^).Offset >= ProcInfo.RetOffset) And
                           (TReference(Pai386(p)^.op1^).Index = R_NO)
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
                           If (Pai386(p)^.Size <> S_FX) Then
                             Begin
                               Pai386(p)^._operator := A_FST;
                               AsmL^.Remove(hp1);
                               Dispose(hp1, done)
                             End
                      End;
                End;
              A_IMUL:
                {changes certain "imul const, %reg"'s to lea sequences}
                Begin
                  If (Pai386(p)^.op1t = Top_Const) And
                     (Pai386(p)^.op2t = Top_Reg) And
                     (Pai386(p)^.Size = S_L) Then
                    If (Longint(Pai386(p)^.op1) = 1) Then
                      If (Pai386(p)^.op3t = Top_None) Then
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
                          hp1 := New(Pai386, Op_Reg_Reg(A_MOV, S_L, TRegister(TwoWords(Pai386(p)^.op2).Word1),
                                                        TRegister(TwoWords(Pai386(p)^.op2).Word2)));
                          hp1^.fileinfo := p^.fileinfo;
                          InsertLLItem(AsmL, p^.previous, p^.next, hp1);
                          Dispose(p, Done);
                          p := hp1;
                        End
                    Else If
                     ((Pai386(p)^.op3t = Top_Reg) or
                      (Pai386(p)^.op3t = Top_None)) And
                     (aktoptprocessor < ClassP6) And
                     (Longint(Pai386(p)^.op1) <= 12) And
                     Not(CS_LittleSize in aktglobalswitches) And
                     (Not(GetNextInstruction(p, hp1)) Or
                       {GetNextInstruction(p, hp1) And}
                       Not((Pai(hp1)^.typ = ait_labeled_instruction) And
                           ((pai_labeled(hp1)^._operator = A_JO) or
                            (pai_labeled(hp1)^._operator = A_JNO))))
                    Then
                      Begin
                        New(TmpRef);
                        TmpRef^.segment := R_DEFAULT_SEG;
                        TmpRef^.symbol := nil;
                        TmpRef^.isintvalue := false;
                        TmpRef^.offset := 0;
                        Case Longint(Pai386(p)^.op1) Of
                          3: Begin
                             {imul 3, reg1, reg2 to
                                lea (reg1,reg1,2), reg2
                              imul 3, reg1 to
                                lea (reg1,reg1,2), reg1}
                               TmpRef^.base := TRegister(twowords(Pai386(p)^.op2).Word1);
                               TmpRef^.Index := TRegister(twowords(Pai386(p)^.op2).Word1);
                               TmpRef^.ScaleFactor := 2;
                               If (Pai386(p)^.op3t = Top_None)
                                 Then hp1 := New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, TRegister(Pai386(p)^.op2)))
                                 Else hp1 := New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                  TRegister(twowords(Pai386(p)^.op2).word2)));
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
                              TmpRef^.base := TRegister(twowords(Pai386(p)^.op2).Word1);
                              TmpRef^.Index := TRegister(twowords(Pai386(p)^.op2).Word1);
                              TmpRef^.ScaleFactor := 4;
                              If (Pai386(p)^.op3t = Top_None)
                                Then hp1 := New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, TRegister(Pai386(p)^.op2)))
                                Else hp1 := New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                 TRegister(twowords(Pai386(p)^.op2).word2)));
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
                                    TmpRef^.Index := TRegister(twowords(Pai386(p)^.op2).Word1);
                                    If (Pai386(p)^.op3t = Top_Reg)
                                      Then
                                        Begin
                                          TmpRef^.base := TRegister(twowords(Pai386(p)^.op2).word2);
                                          TmpRef^.ScaleFactor := 4;
                                          hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                            TRegister(twowords(Pai386(p)^.op2).word2)));
                                        End
                                      Else
                                        Begin
                                          Dispose(TmpRef);
                                          hp1 :=  New(Pai386, op_reg_reg(A_ADD, S_L,
                                            TRegister(Pai386(p)^.op2),TRegister(Pai386(p)^.op2)));
                                        End;
                                    hp1^.fileinfo := p^.fileinfo;
                                    InsertLLItem(AsmL,p, p^.next, hp1);
                                    New(TmpRef);
                                    TmpRef^.segment := R_DEFAULT_SEG;
                                    TmpRef^.symbol := nil;
                                    TmpRef^.isintvalue := false;
                                    TmpRef^.offset := 0;
                                    TmpRef^.Index := TRegister(twowords(Pai386(p)^.op2).Word1);
                                    TmpRef^.ScaleFactor := 2;
                                    If (Pai386(p)^.op3t = Top_Reg)
                                      Then
                                        Begin
                                          TmpRef^.base := R_NO;
                                          hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                            TRegister(twowords(Pai386(p)^.op2).word2)));
                                        End
                                      Else
                                        Begin
                                          TmpRef^.base := TRegister(Pai386(p)^.op2);
                                          hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, TRegister(Pai386(p)^.op2)));
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
                               TmpRef^.base := TRegister(twowords(Pai386(p)^.op2).Word1);
                               TmpRef^.Index := TRegister(twowords(Pai386(p)^.op2).Word1);
                               TmpRef^.ScaleFactor := 8;
                               If (Pai386(p)^.op3t = Top_None)
                                   Then hp1 := New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, TRegister(Pai386(p)^.op2)))
                                   Else hp1 := New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                     TRegister(twowords(Pai386(p)^.op2).word2)));
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
                                   If (Pai386(p)^.op3t = Top_Reg)
                                     Then
                                       hp1 :=  New(Pai386, op_reg_reg(A_ADD, S_L,
                                          Tregister(twowords(Pai386(p)^.op2).word2),
                                          Tregister(twowords(Pai386(p)^.op2).word2)))
                                     Else hp1 := New(Pai386, op_reg_reg(A_ADD, S_L,
                                              TRegister(Pai386(p)^.op2), TRegister(Pai386(p)^.op2)));
                                   hp1^.fileinfo := p^.fileinfo;
                                   InsertLLItem(AsmL,p, p^.next, hp1);
                                   TmpRef^.base := TRegister(twowords(Pai386(p)^.op2).Word1);
                                   TmpRef^.Index := TRegister(twowords(Pai386(p)^.op2).Word1);
                                   TmpRef^.ScaleFactor := 4;
                                   If (Pai386(p)^.op3t = Top_Reg)
                                     Then
                                       hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                          TRegister(twowords(Pai386(p)^.op2).word2)))
                                     Else
                                       hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                          TRegister(Pai386(p)^.op2)));
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
                                     TmpRef^.Index := TRegister(twowords(Pai386(p)^.op2).Word1);
                                     If (Pai386(p)^.op3t = Top_Reg)
                                       Then
                                         Begin
                                           TmpRef^.base := TRegister(twowords(Pai386(p)^.op2).word2);
                                           TmpRef^.ScaleFactor := 8;
                                           hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                             TRegister(twowords(Pai386(p)^.op2).word2)));
                                         End
                                       Else
                                         Begin
                                           TmpRef^.base := R_NO;
                                           TmpRef^.ScaleFactor := 4;
                                           hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                             TRegister(Pai386(p)^.op2)));
                                         End;
                                     hp1^.fileinfo := p^.fileinfo;
                                     InsertLLItem(AsmL,p, p^.next, hp1);
                                     New(TmpRef);
                                     TmpRef^.segment := R_DEFAULT_SEG;
                                     TmpRef^.symbol := nil;
                                     TmpRef^.isintvalue := false;
                                     TmpRef^.offset := 0;
                                     TmpRef^.Index := TRegister(twowords(Pai386(p)^.op2).Word1);
                                     If (Pai386(p)^.op3t = Top_Reg)
                                       Then
                                         Begin
                                           TmpRef^.base := R_NO;
                                           TmpRef^.ScaleFactor := 4;
                                           hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                             TRegister(twowords(Pai386(p)^.op2).word2)));
                                         End
                                       Else
                                         Begin
                                           TmpRef^.base := TRegister(Pai386(p)^.op2);
                                           TmpRef^.ScaleFactor := 2;
                                           hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                             TRegister(Pai386(p)^.op2)));
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
                  If (PReference(Pai386(p)^.op1)^.Base In [R_EAX..R_EDI]) And
                     (PReference(Pai386(p)^.op1)^.Index = R_NO) And
                     (PReference(Pai386(p)^.op1)^.Offset = 0) And
                     (Not(Assigned(PReference(Pai386(p)^.op1)^.Symbol))) Then
                    If (PReference(Pai386(p)^.op1)^.Base <> TRegister(Pai386(p)^.op2))
                      Then
                        Begin
                          hp1 := New(Pai386, op_reg_reg(A_MOV, S_L,PReference(Pai386(p)^.op1)^.Base,
                            TRegister(Pai386(p)^.op2)));
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
                  If (Pai386(p)^.op2t = top_reg) And
                     (TRegister(Pai386(p)^.op2) In [R_EAX, R_EBX, R_EDX, R_EDI]) And
                     GetNextInstruction(p, hp1) And
                     (Pai(hp1)^.typ = ait_instruction) And
                     (Pai386(hp1)^._operator = A_MOV) And
                     (Pai386(hp1)^.op1t = top_reg) And
                     (Pai386(hp1)^.op1 = Pai386(p)^.op2)
                    Then
                {we have "mov x, %treg; mov %treg, y}
                      If (Pai386(hp1)^.op2t <> top_reg) Or
                         (GetNextInstruction(hp1, hp2) And
                          (RegUsedAfterInstruction(TRegister(Pai386(hp1)^.op2), hp1, TmpUsedRegs) or
                {now TmpUsedRegs contains the regalloc data after hp1}
                          (RegInInstruction(TRegister(Pai386(hp1)^.op2), hp2))) And
                         Not(TRegister(Pai386(hp1)^.op1) in TmpUsedRegs))
                        Then
               {we've got "mov x, %treg; mov %treg, y; XXX y" (ie. y is used in
                the third instruction)}
                          Case Pai386(p)^.op1t Of
                            top_reg:
                          {change "mov %reg, %treg; mov %treg, y"
                           to "mov %reg, y"}
                              Begin
                                Pai386(hp1)^.op1 := Pai386(p)^.op1;
                                AsmL^.Remove(p);
                                Dispose(p, Done);
                                p := hp1;
                                continue;
                              End;
                            top_ref:
                              If (Pai386(hp1)^.op2t = top_reg)
                                Then
                             {change "mov mem, %treg; mov %treg, %reg"
                              to "mov mem, %reg"}
                                  Begin
                                    Pai386(p)^.op2 := Pai386(hp1)^.op2;
                                    AsmL^.Remove(hp1);
                                    Dispose(hp1, Done);
                                    continue;
                                  End;
                          End
                        Else
               {remove an instruction which never makes sense: we've got
                "mov mem, %reg1; mov %reg1, %edi" and then EDI isn't used anymore!}
{                          Begin
                            If (TRegister(Pai386(hp1)^.op2) = R_EDI) And
                               Not(GetNextInstruction(hp1, hp2) And
                                   (Pai(hp2)^.typ = ait_instruction) And
                                   (Pai386(hp2)^.op2t = top_reg) And
                                   (Pai386(hp2)^.op2 = Pointer(R_ESI))) Then
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
                      If (Pai386(p)^.op1t = top_reg) And
                         (Pai386(p)^.op2t = top_reg) And
                         GetNextInstruction(p,hp1) And
                         (Pai(hp1)^.typ = ait_instruction) And
                         (Pai386(hp1)^.op1t = top_reg) And
                         (Pai386(hp1)^.op1 = Pai386(p)^.op2)
                        Then
                  {we have "mov %reg1, %reg2; XXX %reg2, ???"}
                          Begin
                            If ((Pai386(hp1)^._operator = A_OR) Or
                                (Pai386(hp1)^._operator = A_TEST)) And
                               (Pai386(hp1)^.op2t = top_reg) And
                               (Pai386(hp1)^.op1 = Pai386(hp1)^.op2)
                              Then
                   {we have "mov %reg1, %reg2; test/or %reg2, %reg2"}
                                Begin
                                  TmpUsedRegs := UsedRegs;
                                  If GetNextInstruction(hp1, hp2) And
                                     (hp2^.typ = ait_labeled_instruction) And
                                     Not(RegUsedAfterInstruction(TRegister(Pai386(hp1)^.op1), hp1, TmpUsedRegs))
                                    Then
                   {change "mov %reg1, %reg2; test/or %reg2, %reg2; jxx" to
                    "test %reg1, %reg1; jxx"}
                                      Begin
                                        Pai386(hp1)^.op1 := Pai386(p)^.op1;
                                        Pai386(hp1)^.op2 := Pai386(p)^.op1;
                                        AsmL^.Remove(p);
                                        Dispose(p, done);
                                        p := hp1;
                                        continue
                                      End
                                    Else
                   {change "mov %reg1, %reg2; test/or %reg2, %reg2" to
                    "mov %reg1, %reg2; test/or %reg1, %reg1"}
                                      Begin
                                        Pai386(hp1)^.op1 := Pai386(p)^.op1;
                                        Pai386(hp1)^.op2 := Pai386(p)^.op1;
                                      End;
                                End
{                              Else
                                If (Pai386(p^.next)^._operator
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
                              If ((Pai386(hp1)^._operator = A_LEAVE) Or
                                  (Pai386(hp1)^._operator = A_RET)) And
                                 (Pai386(p)^.op2t = top_ref) And
                                 (TReference(Pai386(p)^.op2^).base = ProcInfo.FramePointer) And
                                 (TReference(Pai386(p)^.op2^).offset >= ProcInfo.RetOffset) And
                                 (TReference(Pai386(p)^.op2^).index = R_NO) And
                                 (Pai386(p)^.op1t = top_reg)
                                Then
                                  Begin
                                   AsmL^.Remove(p);
                                   Dispose(p, done);
                                   p := hp1;
                                 End
                               Else
                                 If (Pai386(p)^.op1t = top_reg) And
                                    (Pai386(p)^.op2t = top_ref) And
                                    (Pai386(p)^.Size = Pai386(hp1)^.Size) And
                                    (Pai386(hp1)^._operator = A_CMP) And
                                    (Pai386(hp1)^.op2t = top_ref) And
                                    RefsEqual(TReference(Pai386(p)^.op2^),
                                              TReference(Pai386(hp1)^.op2^))
                                   Then
            {change "mov reg, mem1; cmp x, mem1" to "mov reg, mem1; cmp x, reg1"}
                                     Begin
                                       Dispose(PReference(Pai386(hp1)^.op2));
                                       Pai386(hp1)^.opxt := Pai386(hp1)^.op1t + (top_reg shl 4);
                                       Pai386(hp1)^.op2 := Pai386(p)^.op1
                                     End;
                { Next instruction is also a MOV ? }
                  If GetNextInstruction(p, hp1) And
                     (pai(hp1)^.typ = ait_instruction) and
                     (Pai386(hp1)^._operator = A_MOV)
                    Then
                      Begin
                        If (Pai386(hp1)^.op1t = Pai386(p)^.op2t) and
                           (Pai386(hp1)^.op2t = Pai386(p)^.op1t)
                          Then
                            {mov reg1, mem1     or     mov mem1, reg1
                             mov mem2, reg2            mov reg2, mem2}
                            Begin
                              If (Pai386(hp1)^.op2t = top_ref)
                                Then
                                  TmpBool1 := RefsEqual(TReference(Pai386(hp1)^.op2^), TReference(Pai386(p)^.op1^))
                                Else
                                  TmpBool1 := Pai386(hp1)^.op2 = Pai386(p)^.op1;
                              If TmpBool1
                                Then
                            {mov reg1, mem1     or     mov mem1, reg1
                             mov mem2, reg1            mov reg2, mem1}
                                  Begin
                                    If (Pai386(hp1)^.op1t = top_ref)
                                      Then
                                        TmpBool1 := RefsEqual(TReference(Pai386(hp1)^.op1^),
                                                              TReference(Pai386(p)^.op2^))
                                      Else TmpBool1 := (Pai386(hp1)^.op1 = Pai386(p)^.op2);
                                   If TmpBool1 Then
                        { Removes the second statement from
                            mov reg1, mem1
                            mov mem1, reg1 }
                                      Begin
                                        AsmL^.remove(hp1);
                                        Dispose(hp1,done);
                                      End;
                                  End
                                Else
                                  Begin
                                    If GetNextInstruction(hp1, hp2) And
                                       (Pai386(p)^.op1t = top_ref) And
                                       (Pai386(p)^.op2t = top_reg) And
                                       (Pai386(hp1)^.op1t = top_reg) And
                                       (Pai386(hp1)^.op1 = Pai386(p)^.op2) And
                                       (Pai386(hp1)^.op2t = top_ref) And
                                       (Pai(hp2)^.typ = ait_instruction) And
                                       (Pai386(hp2)^._operator = A_MOV) And
                                       (Pai386(hp2)^.op2t = top_reg) And
                                       (Pai386(hp2)^.op1t = top_ref) And
                                       RefsEqual(TReference(Pai386(hp2)^.op1^),
                                                 TReference(Pai386(hp1)^.op2^))
                                      Then
                                        If (TRegister(Pai386(p)^.op2) = R_EDI)
                                          Then
                                 {   mov mem1, %edi
                                     mov %edi, mem2
                                     mov mem2, reg2
                                  to:
                                     mov mem1, reg2
                                     mov reg2, mem2}
                                            Begin
                                              Pai386(p)^.op2 := Pai386(hp2)^.op2;
                                              Pai386(hp1)^.op1 := Pai386(hp2)^.op2;
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
                                              If (Pai386(p)^.op2 <> Pai386(hp2)^.op2) Then
                                                Begin
                                                  Pai386(hp1)^.opxt := top_ref + top_reg shl 4;
                                                  If Assigned(TReference(Pai386(hp1)^.op2^).Symbol)
                                                    Then Freemem(TReference(Pai386(hp1)^.op2^).Symbol,
                                                                 Length(TReference(Pai386(hp1)^.op2^).Symbol^)+1);
                                                  Pai386(hp1)^.op1 := Pai386(hp1)^.op2; {move the treference}
                                                  TReference(Pai386(hp1)^.op1^) := TReference(Pai386(p)^.op1^);
                                                  If Assigned(TReference(Pai386(p)^.op1^).Symbol) Then
                                                    Begin
                                                      Getmem(TReference(Pai386(hp1)^.op1^).Symbol,
                                                             Length(TReference(Pai386(p)^.op1^).Symbol^)+1);
                                                      TReference(Pai386(hp1)^.op1^).Symbol^ :=
                                                          TReference(Pai386(p)^.op1^).Symbol^;
                                                    End;
                                                  Pai386(hp1)^.op2 := Pai386(hp2)^.op2;
                                                End
                                              Else
                                                Begin
                                                  AsmL^.Remove(hp1);
                                                  Dispose(hp1, Done)
                                                End;
                                              Pai386(hp2)^.opxt := top_reg + top_ref shl 4;
                                              Pai386(hp2)^.op2 := Pai386(hp2)^.op1;
                                              Pai386(hp2)^.op1 := Pai386(p)^.op2;
                                            End;
                                  End;
                            End
                          Else
(*                          {movl [mem1],reg1
                             movl [mem1],reg2
                            to:
                              movl [mem1],reg1
                              movl reg1,reg2 }
                            If (Pai386(p)^.op1t = top_ref) and
                               (Pai386(p)^.op2t = top_reg) and
                               (Pai386(hp1)^.op1t = top_ref) and
                               (Pai386(hp1)^.op2t = top_reg) and
                               (Pai386(p)^.size = Pai386(hp1)^.size) and
                               RefsEqual(TReference(Pai386(p)^.op1^),TReference(Pai386(hp1)^.op1^)) and
                               (TRegister(Pai386(p)^.op2)<>TReference(Pai386(hp1)^.op1^).base) and
                               (TRegister(Pai386(p)^.op2)<>TReference(Pai386(hp1)^.op1^).index) then
                              Begin
                                Dispose(PReference(Pai386(hp1)^.op1));
                                Pai386(hp1)^.op1:=Pai386(p)^.op2;
                                Pai386(hp1)^.opxt:=Top_reg+Top_reg shl 4;
                              End
                            Else*)
                            {   movl const1,[mem1]
                                movl [mem1],reg1
                             to:
                                movl const1,reg1
                                movl reg1,[mem1] }
                              If (Pai386(p)^.op1t = top_const) and
                                 (Pai386(p)^.op2t = top_ref) and
                                 (Pai386(hp1)^.op1t = top_ref) and
                                 (Pai386(hp1)^.op2t = top_reg) and
                                 (Pai386(p)^.size = Pai386(hp1)^.size) and
                                 RefsEqual(TReference(Pai386(hp1)^.op1^),TReference(Pai386(p)^.op2^)) then
                                Begin
                                  Pai386(hp1)^.op1:=Pai386(hp1)^.op2;
                                  Pai386(hp1)^.op2:=Pai386(p)^.op2;
                                  Pai386(hp1)^.opxt:=Top_reg+Top_ref shl 4;
                                  Pai386(p)^.op2:=Pai386(hp1)^.op1;
                                  Pai386(p)^.opxt:=Top_const+(top_reg shl 4);
                                End
                      End;
                       {changes "mov $0, %reg" into "xor %reg, %reg"}
                  If (Pai386(p)^.op1t = Top_Const) And
                     (Pai386(p)^.op1 = Pointer(0)) And
                     (Pai386(p)^.op2t = Top_Reg)
                    Then
                      Begin
                        Pai386(p)^._operator := A_XOR;
                        Pai386(p)^.opxt := Top_Reg+Top_reg shl 4;
                        Pai386(p)^.op1 := Pai386(p)^.op2;
                      End;
                End;
              A_MOVZX:
                Begin
                {removes superfluous And's after movzx's}
                  If (Pai386(p)^.op2t = top_reg) And
                     GetNextInstruction(p, hp1) And
                     (Pai(hp1)^.typ = ait_instruction) And
                     (Pai386(hp1)^._operator = A_AND) And
                     (Pai386(hp1)^.op1t = top_const) And
                     (Pai386(hp1)^.op2t = top_reg) And
                     (Pai386(hp1)^.op2 = Pai386(p)^.op2)
                    Then
                      Case Pai386(p)^.Size Of
                        S_BL, S_BW:
                          If (Longint(Pai386(hp1)^.op1) = $ff)
                            Then
                              Begin
                                AsmL^.Remove(hp1);
                                Dispose(hp1, Done);
                              End;
                        S_WL:
                          If (Longint(Pai386(hp1)^.op1) = $ffff)
                            Then
                              Begin
                                AsmL^.Remove(hp1);
                                Dispose(hp1, Done);
                              End;
                      End;
                {changes some movzx constructs to faster synonims (all examples
                 are given with eax/ax, but are also valid for other registers)}
                  If (Pai386(p)^.op2t = top_reg) Then
                    If (Pai386(p)^.op1t = top_reg)
                      Then
                        Case Pai386(p)^.size of
                          S_BW:
                            Begin
                              If (TRegister(Pai386(p)^.op1) = Reg16ToReg8(TRegister(Pai386(p)^.op2))) And
                                 Not(CS_LittleSize In aktglobalswitches)
                                Then
                                  {Change "movzbw %al, %ax" to "andw $0x0ffh, %ax"}
                                  Begin
                                    Pai386(p)^._operator := A_AND;
                                    Pai386(p)^.opxt := top_const+Top_reg shl 4;
                                    Longint(Pai386(p)^.op1) := $ff;
                                    Pai386(p)^.Size := S_W
                                  End
                                Else
                                  If GetNextInstruction(p, hp1) And
                                     (Pai(hp1)^.typ = ait_instruction) And
                                     (Pai386(hp1)^._operator = A_AND) And
                                     (Pai386(hp1)^.op1t = top_const) And
                                     (Pai386(hp1)^.op2t = top_reg) And
                                     (Pai386(hp1)^.op2 = Pai386(p)^.op2)
                                    Then
                                      {Change "movzbw %reg1, %reg2; andw $const, %reg2"
                                       to "movw %reg1, reg2; andw $(const1 and $ff), %reg2"}
                                      Begin
                                        Pai386(p)^._operator := A_MOV;
                                        Pai386(p)^.Size := S_W;
                                        Pai386(p)^.op1 := Pointer(Reg8ToReg16(TRegister(Pai386(p)^.op1)));
                                        Pai386(hp1)^.op1 := Pointer(Longint(Pai386(hp1)^.op1) And $ff);
                                      End;
                            End;
                          S_BL:
                            Begin
                              If (TRegister(Pai386(p)^.op1) = Reg32ToReg8(TRegister(Pai386(p)^.op2))) And
                                 Not(CS_LittleSize in aktglobalswitches)
                                Then
                                  {Change "movzbl %al, %eax" to "andl $0x0ffh, %eax"}
                                  Begin
                                    Pai386(p)^._operator := A_AND;
                                    Pai386(p)^.opxt := top_const+Top_reg shl 4;
                                    Longint(Pai386(p)^.op1) := $ff;
                                    Pai386(p)^.Size := S_L;
                                  End
                                Else
                                  If GetNextInstruction(p, hp1) And
                                     (Pai(hp1)^.typ = ait_instruction) And
                                     (Pai386(hp1)^._operator = A_AND) And
                                     (Pai386(hp1)^.op1t = top_const) And
                                     (Pai386(hp1)^.op2t = top_reg) And
                                     (Pai386(hp1)^.op2 = Pai386(p)^.op2)
                                    Then
                                     {Change "movzbl %reg1, %reg2; andl $const, %reg2"
                                      to "movl %reg1, reg2; andl $(const1 and $ff), %reg2"}
                                      Begin
                                        Pai386(p)^._operator := A_MOV;
                                        Pai386(p)^.Size := S_L;
                                        Pai386(p)^.op1 := Pointer(Reg8ToReg32(TRegister(Pai386(p)^.op1)));
                                        Pai386(hp1)^.op1 := Pointer(Longint(Pai386(hp1)^.op1) And $ff);
                                      End
                            End;
                          S_WL:
                            Begin
                              If (TRegister(Pai386(p)^.op1) = Reg32ToReg16(TRegister(Pai386(p)^.op2))) And
                                 Not(CS_LittleSize In aktglobalswitches)
                                Then
                                 {Change "movzwl %ax, %eax" to "andl $0x0ffffh, %eax"}
                                  Begin
                                    Pai386(p)^._operator := A_AND;
                                    Pai386(p)^.opxt := top_const+Top_reg shl 4;
                                    Longint(Pai386(p)^.op1) := $ffff;
                                    Pai386(p)^.Size := S_L
                                  End
                                Else
                                  If GetNextInstruction(p, hp1) And
                                     (Pai(hp1)^.typ = ait_instruction) And
                                     (Pai386(hp1)^._operator = A_AND) And
                                     (Pai386(hp1)^.op1t = top_const) And
                                     (Pai386(hp1)^.op2t = top_reg) And
                                     (Pai386(hp1)^.op2 = Pai386(p)^.op2)
                                    Then
                                      {Change "movzwl %reg1, %reg2; andl $const, %reg2"
                                       to "movl %reg1, reg2; andl $(const1 and $ffff), %reg2"}
                                      Begin
                                        Pai386(p)^._operator := A_MOV;
                                        Pai386(p)^.Size := S_L;
                                        Pai386(p)^.op1 := Pointer(Reg16ToReg32(TRegister(Pai386(p)^.op1)));
                                        Pai386(hp1)^.op1 := Pointer(Longint(Pai386(hp1)^.op1) And $ffff);
                                      End;
                            End;
                        End
                      Else
                        If (Pai386(p)^.op1t = top_ref) Then
                          Begin
                            If GetNextInstruction(p, hp1) And
                               (Pai(hp1)^.typ = ait_instruction) And
                               (Pai386(hp1)^._operator = A_AND) And
                               (Pai386(hp1)^.op1t = Top_Const) And
                               (Pai386(hp1)^.op2t = Top_Reg) And
                               (Pai386(hp1)^.op2 = Pai386(p)^.op2) Then
                              Begin
                                Pai386(p)^._operator := A_MOV;
                                Case Pai386(p)^.Size Of
                                  S_BL:
                                    Begin
                                      Pai386(p)^.Size := S_L;
                                      Pai386(hp1)^.op1 := Pointer(Longint(Pai386(hp1)^.op1)
                                        And $ff);
                                    End;
                                  S_WL:
                                    Begin
                                      Pai386(p)^.Size := S_L;
                                      Pai386(hp1)^.op1 := Pointer(Longint(Pai386(hp1)^.op1)
                                        And $ffff);
                                    End;
                                  S_BW:
                                    Begin
                                      Pai386(p)^.Size := S_W;
                                      Pai386(hp1)^.op1 := Pointer(Longint(Pai386(hp1)^.op1)
                                        And $ff);
                                    End;
                                End;
                              End;
                          End;
                End;
              A_POP:
                Begin
                   if (Pai386(p)^.op1t = top_reg) And
                      GetNextInstruction(p, hp1) And
                      (pai(hp1)^.typ=ait_instruction) and
                      (Pai386(hp1)^._operator=A_PUSH) and
                      (Pai386(hp1)^.op1t = top_reg) And
                      (Pai386(hp1)^.op1=Pai386(p)^.op1) then
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
                         Pai386(p)^._operator := A_MOV;
                         Pai386(p)^.op2 := Pai386(p)^.op1;
                         Pai386(p)^.opxt := top_ref + top_reg shl 4;
                         New(TmpRef);
                         TmpRef^.segment := R_DEFAULT_SEG;
                         TmpRef^.base := R_ESP;
                         TmpRef^.index := R_NO;
                         TmpRef^.scalefactor := 1;
                         TmpRef^.symbol := nil;
                         TmpRef^.isintvalue := false;
                         TmpRef^.offset := 0;
                         Pai386(p)^.op1 := Pointer(TmpRef);
                         hp1 := Pai(p^.next);
                         AsmL^.Remove(hp1);
                         Dispose(hp1, Done)
                       End
                end;
              A_PUSH:
                Begin
                  If (Pai386(p)^.size = S_W) And
                     (Pai386(p)^.op1t = Top_Const) And
                     GetNextInstruction(p, hp1) And
                     (Pai(hp1)^.typ = ait_instruction) And
                     (Pai386(hp1)^._operator = A_PUSH) And
                     (Pai386(hp1)^.op1t = Top_Const) And
                     (Pai386(hp1)^.size = S_W) Then
                    Begin
                      Pai386(p)^.Size := S_L;
                      Pai386(p)^.op1 := Pointer(Longint(Pai386(p)^.op1) shl 16 + Longint(Pai386(hp1)^.op1));
                      AsmL^.Remove(hp1);
                      Dispose(hp1, Done)
                    End;
                End;
              A_SHL, A_SAL:
                Begin
                  If (Pai386(p)^.op1t = Top_Const) And
                     (Pai386(p)^.op2t = Top_Reg) And
                     (Pai386(p)^.Size = S_L) And
                     (Longint(Pai386(p)^.op1) <= 3)
                {Changes "shl const, %reg32; add const/reg, %reg32" to one lea statement}
                    Then
                      Begin
                        TmpBool1 := True; {should we check the next instruction?}
                        TmpBool2 := False; {have we found an add/sub which could be
                                            integrated in the lea?}
                        New(TmpRef);
                        TmpRef^.segment := R_DEFAULT_SEG;
                        TmpRef^.base := R_NO;
                        TmpRef^.index := TRegister(Pai386(p)^.op2);
                        TmpRef^.scalefactor := 1 shl Longint(Pai386(p)^.op1);
                        TmpRef^.symbol := nil;
                        TmpRef^.isintvalue := false;
                        TmpRef^.offset := 0;
                        While TmpBool1 And
                              GetNextInstruction(p, hp1) And
                              (Pai(hp1)^.typ = ait_instruction) And
                              ((Pai386(hp1)^._operator = A_ADD) Or
                               (Pai386(hp1)^._operator = A_SUB)) And
                              (Pai386(hp1)^.op2t = Top_Reg) And
                              (Pai386(hp1)^.op2 = Pai386(p)^.op2) Do
                          Begin
                            TmpBool1 := False;
                            If (Pai386(hp1)^.op1t = Top_Const)
                              Then
                                Begin
                                  TmpBool1 := True;
                                  TmpBool2 := True;
                                  If Pai386(hp1)^._operator = A_ADD
                                    Then Inc(TmpRef^.offset, Longint(Pai386(hp1)^.op1))
                                    Else Dec(TmpRef^.offset, Longint(Pai386(hp1)^.op1));
                                  AsmL^.Remove(hp1);
                                  Dispose(hp1, Done);
                                End
                              Else
                                If (Pai386(hp1)^.op1t = Top_Reg) And
                                   (Pai386(hp1)^._operator = A_ADD) And
                                   (TmpRef^.base = R_NO) Then
                                  Begin
                                    TmpBool1 := True;
                                    TmpBool2 := True;
                                    TmpRef^.base := TRegister(Pai386(hp1)^.op1);
                                    AsmL^.Remove(hp1);
                                    Dispose(hp1, Done);
                                  End;
                          End;
                        If TmpBool2 Or
                           ((aktoptprocessor < ClassP6) And
                            (Longint(Pai386(p)^.op1) <= 3) And
                            Not(CS_LittleSize in aktglobalswitches))
                          Then
                            Begin
                              If Not(TmpBool2) And
                                 (Longint(Pai386(p)^.op1) = 1)
                                Then
                                  Begin
                                    Dispose(TmpRef);
                                    hp1 := new(Pai386,op_reg_reg(A_ADD,Pai386(p)^.Size,
                                               TRegister(Pai386(p)^.op2), TRegister(Pai386(p)^.op2)))
                                  End
                                Else hp1 := New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                                TRegister(Pai386(p)^.op2)));
                              hp1^.fileinfo := p^.fileinfo;
                              InsertLLItem(AsmL,p^.previous, p^.next, hp1);
                              Dispose(p, Done);
                              p := hp1;
                            End;
                      End
                    Else
                      If (aktoptprocessor < ClassP6) And
                         (Pai386(p)^.op1t = top_const) And
                         (Pai386(p)^.op2t = top_reg) Then
                        If (Longint(Pai386(p)^.op1) = 1)
                          Then
  {changes "shl $1, %reg" to "add %reg, %reg", which is the same on a 386,
   but faster on a 486, and pairable in both U and V pipes on the Pentium
   (unlike shl, which is only pairable in the U pipe)}
                            Begin
                              hp1 := new(Pai386,op_reg_reg(A_ADD,Pai386(p)^.Size,
                                         TRegister(Pai386(p)^.op2), TRegister(Pai386(p)^.op2)));
                              hp1^.fileinfo := p^.fileinfo;
                              InsertLLItem(AsmL,p^.previous, p^.next, hp1);
                              Dispose(p, done);
                              p := hp1;
                            End
                          Else If (Pai386(p)^.size = S_L) and
                                  (Longint(Pai386(p)^.op1) <= 3) Then
                    {changes "shl $2, %reg" to "lea (,%reg,4), %reg"
                             "shl $3, %reg" to "lea (,%reg,8), %reg}
                                 Begin
                                   New(TmpRef);
                                   TmpRef^.segment := R_DEFAULT_SEG;
                                   TmpRef^.base := R_NO;
                                   TmpRef^.index := TRegister(Pai386(p)^.op2);
                                   TmpRef^.scalefactor := 1 shl Longint(Pai386(p)^.op1);
                                   TmpRef^.symbol := nil;
                                   TmpRef^.isintvalue := false;
                                   TmpRef^.offset := 0;
                                   hp1 := new(Pai386,op_ref_reg(A_LEA,S_L,TmpRef, TRegister(Pai386(p)^.op2)));
                                   hp1^.fileinfo := p^.fileinfo;
                                   InsertLLItem(AsmL,p^.previous, p^.next, hp1);
                                   Dispose(p, done);
                                   p := hp1;
                                 End
                End;
              A_SAR, A_SHR:
                  {changes the code sequence
                   shr/sar const1, %reg
                   shl     const2, %reg
                   to either "sar/and", "shl/and" or just "and" depending on const1 and const2}
                Begin
                  If GetNextInstruction(p, hp1) And
                     (pai(hp1)^.typ = ait_instruction) and
                     (Pai386(hp1)^._operator = A_SHL) and
                     (Pai386(p)^.op1t = top_const) and
                     (Pai386(hp1)^.op1t = top_const)
                    Then
                      If (Longint(Pai386(p)^.op1) > Longint(Pai386(hp1)^.op1)) And
                         (Pai386(p)^.op2t = Top_reg) And
                         Not(CS_LittleSize In aktglobalswitches) And
                         ((Pai386(p)^.Size = S_B) Or
                          (Pai386(p)^.Size = S_L))
                        Then
                          Begin
                            Dec(Longint(Pai386(p)^.op1), Longint(Pai386(hp1)^.op1));
                            Pai386(hp1)^._operator := A_And;
                            Pai386(hp1)^.op1 := Pointer(1 shl Longint(Pai386(hp1)^.op1)-1);
                            If (Pai386(p)^.Size = S_L)
                              Then Pai386(hp1)^.op1 := Pointer(Longint(Pai386(hp1)^.op1) Xor $ffffffff)
                              Else Pai386(hp1)^.op1 := Pointer(Longint(Pai386(hp1)^.op1) Xor $ff);
                          End
                        Else
                          If (Longint(Pai386(p)^.op1) < Longint(Pai386(hp1)^.op1)) And
                             (Pai386(p)^.op2t = Top_reg) And
                             Not(CS_LittleSize In aktglobalswitches) And
                             ((Pai386(p)^.Size = S_B) Or
                              (Pai386(p)^.Size = S_L))
                            Then
                              Begin
                                Dec(Longint(Pai386(hp1)^.op1), Longint(Pai386(p)^.op1));
                                Pai386(p)^._operator := A_And;
                                Pai386(p)^.op1 := Pointer(1 shl Longint(Pai386(p)^.op1)-1);
                                If (Pai386(p)^.Size = S_L)
                                  Then Pai386(hp1)^.op1 := Pointer(Longint(Pai386(hp1)^.op1) Xor $ffffffff)
                                  Else Pai386(hp1)^.op1 := Pointer(Longint(Pai386(hp1)^.op1) Xor $ff);
                              End
                            Else
                              Begin
                                Pai386(p)^._operator := A_And;
                                Pai386(p)^.op1 := Pointer(1 shl Longint(Pai386(p)^.op1)-1);
                                Case Pai386(p)^.Size Of
                                  S_B: Pai386(hp1)^.op1 := Pointer(Longint(Pai386(hp1)^.op1) Xor $ff);
                                  S_W: Pai386(hp1)^.op1 := Pointer(Longint(Pai386(hp1)^.op1) Xor $ffff);
                                  S_L: Pai386(hp1)^.op1 := Pointer(Longint(Pai386(hp1)^.op1) Xor
                                         $ffffffff);
                                End;
                                AsmL^.remove(hp1);
                                dispose(hp1, done);
                              End;
                End;
{$IfDef Ver0_99_11}
              A_SETE..A_SETGE,A_SETC,A_SETNC,A_SETA..A_SETBE,A_SETO..A_SETNLE:
                Begin
                  If (Pai386(p)^.Op1t = top_ref) And
                     GetNextInstruction(p, hp1) And
                     GetNextInstruction(hp1, hp2) And
                     (hp2^.typ = ait_instruction) And
                     (Pai386(hp2)^._operator in [A_LEAVE,A_RET]) And
                     (TReference(Pai386(p)^.Op1^).Base = ProcInfo.FramePointer) And
                     (TReference(Pai386(p)^.Op1^).Index = R_NO) And
                     (TReference(Pai386(p)^.Op1^).Offset >= ProcInfo.RetOffset) And
                     (hp1^.typ = ait_instruction) And
                     (Pai386(hp1)^._operator = A_MOV) And
                     (Pai386(hp1)^.Size = S_B) And
                     (Pai386(hp1)^.Op1t = top_ref) And
                     RefsEqual(TReference(Pai386(hp1)^.Op1^), TReference(Pai386(p)^.Op1^)) Then
                    Begin
                      Dispose(PReference(Pai386(p)^.Op1));
                      Pai386(p)^.Op1 := Pai386(hp1)^.Op2;
                      Pai386(p)^.Opxt := top_reg;
                      AsmL^.Remove(hp1);
                      Dispose(hp1, Done)
                    End
                End;
{$EndIf Ver0_99_11}
              A_SUB:
                {change "subl $2, %esp; pushw x" to "pushl x"}
                Begin
                  If (Pai386(p)^.op1t = top_const) And
                     (Longint(Pai386(p)^.op1) = 2) And
                     (Pai386(p)^.op2t = top_reg) And
                     (TRegister(Pai386(p)^.op2) = R_ESP)
                    Then
                      Begin
                        hp1 := Pai(p^.next);
                        While Assigned(hp1) And
                              (Pai(hp1)^.typ In [ait_instruction]+SkipInstr) And
                               Not((Pai(hp1)^.typ = ait_instruction) And
                                   ((Pai386(hp1)^._operator in [A_CALL,A_PUSH]) or
                                    ((Pai386(hp1)^._operator = A_MOV) And
                                     (Pai386(hp1)^.op2t = top_ref) And
                                     (TReference(Pai386(hp1)^.op2^).base = R_ESP)))) do
                          hp1 := Pai(hp1^.next);
                        If Assigned(hp1) And
                            (Pai(hp1)^.typ = ait_instruction) And
                            (Pai386(hp1)^._operator = A_PUSH) And
                            (Pai386(hp1)^.Size = S_W)
                          Then
                            Begin
                              Pai386(hp1)^.size := S_L;
                              If (Pai386(hp1)^.op1t = top_reg) Then
                                Pai386(hp1)^.op1 := Pointer(Reg16ToReg32(TRegister(Pai386(hp1)^.op1)));
                              hp1 := Pai(p^.next);
                              AsmL^.Remove(p);
                              Dispose(p, Done);
                              p := hp1;
                              Continue
                            End
                          Else
                            If GetLastInstruction(p, hp1) And
                               (Pai(hp1)^.typ = ait_instruction) And
                               (Pai386(hp1)^._operator = A_SUB) And
                               (Pai386(hp1)^.op1t = top_const) And
                               (Pai386(hp1)^.op2t = top_reg) And
                               (TRegister(Pai386(hp1)^.Op2) = R_ESP)
                              Then
                                Begin
                                  Inc(Longint(Pai386(p)^.op1), Longint(Pai386(hp1)^.op1));
                                  AsmL^.Remove(hp1);
                                  Dispose(hp1, Done);
                                End;
                      End;
                End;
              A_TEST, A_OR:
                {removes the line marked with (x) from the sequence
                 And/or/xor/add/sub/... $x, %y
                 test/or %y, %y   (x)
                 j(n)z _Label
                    as the first instruction already adjusts the ZF}
                 Begin
                   If (Pai386(p)^.op1 = Pai386(p)^.op2) And
                      GetLastInstruction(p, hp1) And
                      (pai(hp1)^.typ = ait_instruction) Then
                     Case Pai386(hp1)^._operator Of
                       A_ADD, A_SUB, A_OR, A_XOR, A_AND, A_SHL, A_SHR:
                         Begin
                           If (Pai386(hp1)^.op2 = Pai386(p)^.op1) Then
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
                           If (Pai386(hp1)^.op1 = Pai386(p)^.op1) Then
                             Begin
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

Procedure PeepHoleOptPass2(AsmL: PAasmOutput);

var
  p,hp1,hp2: pai;
Begin
  P := Pai(AsmL^.First);
  While Assigned(p) Do
    Begin
      Case P^.Typ Of
        Ait_Instruction:
          Begin
            Case Pai386(p)^._operator Of
              A_CALL:
                If (AktOptProcessor < ClassP6) And
                   GetNextInstruction(p, hp1) And
                   (hp1^.typ = ait_labeled_instruction) And
                   (Pai_Labeled(hp1)^._operator = A_JMP) Then
                  Begin
                    hp2 := New(Pai386,op_csymbol(A_PUSH,S_L,NewCSymbol(Lab2Str(Pai_Labeled(hp1)^.lab),0)));
                    hp2^.fileinfo := p^.fileinfo;
                    InsertLLItem(AsmL, p^.previous, p, hp2);
                    Pai386(p)^._operator := A_JMP;
                    AsmL^.Remove(hp1);
                    Dispose(hp1, Done)
                  End;
              A_MOV:
                Begin
                  If (Pai386(p)^.op1t = top_reg) And
                     (Pai386(p)^.op2t = top_reg) And
                     GetNextInstruction(p, hp1) And
                     (hp1^.typ = ait_Instruction) And
                     (Pai386(hp1)^._operator = A_MOV) And
                     (Pai386(hp1)^.op1t = top_ref) And
                     (Pai386(hp1)^.op2t = top_reg) And
                     ((TReference(Pai386(hp1)^.op1^).Base = TRegister(Pai386(p)^.op2)) Or
                      (TReference(Pai386(hp1)^.op1^).Index = TRegister(Pai386(p)^.op2))) And
                     (TRegister(Pai386(hp1)^.op2) = TRegister(Pai386(p)^.op2)) Then
              {mov reg1, reg2
               mov (reg2, ..), reg2      to   mov (reg1, ..), reg2}
                    Begin
                      If (TReference(Pai386(hp1)^.op1^).Base = TRegister(Pai386(p)^.op2)) Then
                        TReference(Pai386(hp1)^.op1^).Base := TRegister(Pai386(p)^.op1);
                      If (TReference(Pai386(hp1)^.op1^).Index = TRegister(Pai386(p)^.op2)) Then
                        TReference(Pai386(hp1)^.op1^).Index := TRegister(Pai386(p)^.op1);
                      AsmL^.Remove(p);
                      Dispose(p, Done);
                      p := hp1;
                      Continue;
                    End;
                End;
              A_MOVZX:
                Begin
                  If (Pai386(p)^.op2t = top_reg) Then
                    If (Pai386(p)^.op1t = top_reg)
                      Then
                        Case Pai386(p)^.size of
                          S_BL:
                            Begin
                              If IsGP32Reg(TRegister(Pai386(p)^.op2)) And
                                 Not(CS_LittleSize in aktglobalswitches) And
                                 (aktoptprocessor = ClassP5)
                                Then
                                  {Change "movzbl %reg1, %reg2" to
                                   "xorl %reg2, %reg2; movb %reg1, %reg2" for Pentium and
                                   PentiumMMX}
                                  Begin
                                    hp1 := New(Pai386, op_reg_reg(A_XOR, S_L,
                                               TRegister(Pai386(p)^.op2), TRegister(Pai386(p)^.op2)));
                                    hp1^.fileinfo := p^.fileinfo;
                                    InsertLLItem(AsmL,p^.previous, p, hp1);
                                    Pai386(p)^._operator := A_MOV;
                                    Pai386(p)^.size := S_B;
                                    Pai386(p)^.op2 :=
                                      Pointer(Reg32ToReg8(TRegister(Pai386(p)^.op2)));
                                    { Jonas
                                    InsertLLItem(AsmL,p, p^.next, hp2);
                                      I think you forgot to delete this line PM

                                     Indeed, I had forgotten that one (JM) }
                                  End;
                            End;
                        End
                      Else
                        If (Pai386(p)^.op1t = top_ref) And
                           (PReference(Pai386(p)^.op1)^.base <> TRegister(Pai386(p)^.op2)) And
                           (PReference(Pai386(p)^.op1)^.index <> TRegister(Pai386(p)^.op2)) And
                           Not(CS_LittleSize in aktglobalswitches) And
                           IsGP32Reg(TRegister(Pai386(p)^.op2)) And
                           (aktoptprocessor = ClassP5) And
                           (Pai386(p)^.Size = S_BL)
                          Then
                            {changes "movzbl mem, %reg" to "xorl %reg, %reg; movb mem, %reg8" for
                             Pentium and PentiumMMX}
                            Begin
                              hp1 := New(Pai386,op_reg_reg(A_XOR, S_L, TRegister(Pai386(p)^.op2),
                                         TRegister(Pai386(p)^.op2)));
                              hp1^.fileinfo := p^.fileinfo;
                              Pai386(p)^._operator := A_MOV;
                              Pai386(p)^.size := S_B;
                              Pai386(p)^.op2 := Pointer(Reg32ToReg8(TRegister(Pai386(p)^.op2)));
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
 Revision 1.24  1998-11-26 15:41:45  jonas
   + change "setxx mem; movb mem, reg8" to "setxx reg8" if mem is a local
     variable/parameter or function result (between {$ifdef ver0_99_11})

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
