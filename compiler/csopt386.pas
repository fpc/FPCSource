{
    $Id$
    Copyright (c) 1997-98 by Jonas Maebe

    This unit contains the common subexpression elimination procedure.

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
Unit CSOpt386;

Interface

Uses aasm;

{Procedure CSOpt386(First, Last: Pai);}
Procedure CSE(AsmL: PAasmOutput; First, Last: Pai);

Implementation

Uses CObjects, verbose
   {$ifdef i386}
     ,i386, DAOpt386
   {$endif i386}
     ;

Function CheckSequence(p: Pai; Reg: TRegister; Var Found: Longint): Boolean;
{checks whether the current instruction sequence (starting with p) and the
 one between StartMod and EndMod of Reg are the same. If so, the number of
 instructions that match is stored in Found and true is returned, otherwise
 Found holds the number of instructions between StartMod and EndMod and false
 is returned}
Var hp2, hp3, EndMod: Pai;
    TmpResult: Boolean;
    RegsNotYetChecked: Set Of TRegister;
    Counter: Byte;

  Function NoChangedRegInRef(oldp, newp: Pai): Boolean;
  Var TmpP: Pai;
  {checks if the first operator of newp is a reference and in that case checks
   whether that reference includes regs that have been changed since oldp. This
   to avoid wrong optimizations like

   movl 8(%epb), %eax                          movl 8(%epb), %eax
   movl 12(%epb), %edx                         movl 12(%epb), %edx
   movl (%eax,%edx,1), %edi                    movl (%eax,%edx,1), %edi
   pushl %edi              being converted to  pushl %edi
   movl 8(%epb), %eax                          movl 16(%ebp), %edx
   movl 16(%epb), %edx                         pushl %edi
   movl (%eax,%edx,1), %edi
   pushl %edi

  because first is checked whether %eax isn't changed (it isn't) and
  consequently all instructions containg %eax are removed}
  Begin
    TmpResult := True;
    If (Pai(oldp)^.typ = ait_instruction) Then {oldp and newp are the same instruction}
      Case Pai386(oldp)^.op1t Of
        Top_Reg:
          If (Reg32(TRegister(Pai386(oldp)^.op1)) in RegsNotYetChecked) Then
            Begin
              RegsNotYetChecked := RegsNotYetChecked - [Reg32(TRegister(Pai386(oldp)^.op1))];
              TmpP := NewP;
              While GetLastInstruction(TmpP, TmpP) And
                    PPaiProp(TmpP^.fileinfo.Line)^.CanBeRemoved Do;
              TmpResult := Assigned(TmpP) And
                             RegsSameContent(oldp, TmpP, Reg32(TRegister(Pai386(oldp)^.op1)))
            End;
        Top_Ref:
          With TReference(Pai386(oldp)^.op1^) Do
            Begin
              If (Base in RegsNotYetChecked) And
                 (Base <> R_NO) Then
                Begin
                  RegsNotYetChecked := RegsNotYetChecked - [Base];
                  TmpP := NewP;
                  While GetLastInstruction(TmpP, TmpP) And
                        PPaiProp(TmpP^.fileinfo.Line)^.CanBeRemoved Do;
                  TmpResult := Assigned(TmpP) And
                               RegsSameContent(oldp, TmpP, Base)
                End;
              If TmpResult And
                 (Index <> R_NO) And
                 (Index in RegsNotYetChecked) Then
                Begin
                  RegsNotYetChecked := RegsNotYetChecked - [Index];
                  TmpP := NewP;
                  While GetLastInstruction(TmpP, TmpP) And
                        PPaiProp(TmpP^.fileinfo.Line)^.CanBeRemoved Do;
                  TmpResult := Assigned(TmpP) And
                               RegsSameContent(oldp, TmpP, Index)
                End;
            End;
      End;
    NoChangedRegInRef := TmpResult;
  End;

Begin {CheckSequence}
  Reg := Reg32(Reg);
  Found := 0;
  GetLastInstruction(p, hp3);
  hp2 := PPaiProp(hp3^.fileinfo.line)^.Regs[Reg].StartMod;
  EndMod := PPaiProp(hp3^.fileinfo.line)^.Regs[Reg].StartMod;
  If (PPaiProp(hp3^.fileinfo.line)^.Regs[Reg].NrOfMods = 1)
    Then Counter := 1
    Else
      For Counter := 2 to PPaiProp(hp3^.fileinfo.line)^.Regs[Reg].NrOfMods Do
        GetNextInstruction(EndMod, EndMod);
  hp3 := p;
  RegsNotYetChecked := [R_EAX..R_EDI];
  While (Found <> Counter) And
         InstructionsEqual(hp2, hp3) And
         NoChangedRegInRef(EndMod, hp3) Do
    Begin
      GetNextInstruction(hp2, hp2);
      GetNextInstruction(hp3, hp3);
      Inc(Found)
    End;
  If (Found <> Counter)
    Then
{hack to be able to optimize
     mov (mem), reg
     mov (reg), reg
     mov (reg), reg [*]
     test reg, reg       and the oposite (where the marked instructions are
     jne l1              switched)
     mov (mem), reg
     mov (reg), reg
     movzx (reg), reg [*]}

       Begin
         If ((Found+1) = Counter) And
           Assigned(hp2) And
           (Pai(hp2)^.typ = ait_instruction) And
           (Pai386(hp2)^._operator In [A_MOV, A_MOVZX]) And
           (Pai386(hp2)^.op1t = top_ref) And
           (Pai386(hp2)^.op2t = top_reg) And
           Assigned(hp3) And
           (Pai(hp3)^.typ = ait_instruction) And
           (Pai386(hp3)^._operator In [A_MOV, A_MOVZX]) And
           (Pai386(hp3)^.op1t = top_ref) And
           (Pai386(hp3)^.op2t = top_reg) And
           (Pai386(hp2)^._operator <> Pai386(hp3)^._operator) And
           RefsEqual(TReference(Pai386(hp2)^.op1^),TReference(Pai386(hp3)^.op1^)) And
           NoChangedRegInRef(EndMod, hp3)
          Then
            If (Pai386(hp2)^._operator = A_MOV)
              Then
                Begin
                 If (Pai386(hp2)^.Size = S_B) And
                     (Reg8toReg32(TRegister(Pai386(hp2)^.op2)) =
                      TRegister(Pai386(hp3)^.op2))
                    Then
                      Begin
                        Pai386(hp2)^._operator := A_MOVZX;
                        Pai386(hp2)^.op2 := Pai386(hp3)^.op2;
                        Pai386(hp2)^.Size := S_BL;
                        Inc(Found);
                        CheckSequence := True;
                      End
                    Else
                      Begin
                        CheckSequence := False;
                        If (Found > 0) Then
                          Found := PPaiProp(Pai(p)^.fileinfo.line)^.Regs[Reg].NrOfMods
                      End
                End
              Else
                Begin
                  If (Pai386(hp3)^.Size = S_B) And
                     (Reg8toReg32(TRegister(Pai386(hp3)^.op2)) =
                      TRegister(Pai386(hp2)^.op2))
                    Then
                      Begin
                        CheckSequence := True;
                        Inc(Found)
                      End
                    Else
                      Begin
                        CheckSequence := False;
                        If (Found > 0) Then
                          Found := PPaiProp(Pai(p)^.fileinfo.line)^.Regs[Reg].NrOfMods
                      End
                End
          Else
            Begin
              CheckSequence := False;
              If (found > 0) then
 {this is correct because we only need to turn off the CanBeRemoved flag
  when an instruction has already been processed by CheckSequence
  (otherwise CanBeRemoved can't be true and thus can't have to be turned off).
  If it has already been processed by CheckSequence and flagged to be
  removed, it means that it has been checked against a previous sequence
  and that it was equal (otherwise CheckSequence would have returned false
  and the instruction wouldn't have been removed). If this "If found > 0"
  check is left out, incorrect optimizations are performed.}
                Found := PPaiProp(Pai(p)^.fileinfo.line)^.Regs[Reg].NrOfMods
            End
       End
     Else CheckSequence := True;
End; {CheckSequence}

Procedure DoCSE(First, Last: Pai);
{marks the instructions that can be removed by RemoveInstructs. They're not
 removed immediately because sometimes an instruction needs to be checked in
 two different sequences}
Var Cnt, Cnt2: Longint;
    p, hp1, hp2: Pai;
Begin
  p := First;
  If (p^.typ in SkipInstr) Then
    GetNextInstruction(p, p);
  First := p;
  While Assigned(p) Do
    Begin
      Case p^.typ Of
        ait_label, ait_labeled_instruction:;
        ait_instruction:
          Begin
            Case Pai386(p)^._operator Of
              A_CLD: If GetLastInstruction(p, hp1) And
                        (PPaiProp(hp1^.fileinfo.line)^.DirFlag = F_NotSet) Then
                       PPaiProp(Pai(p)^.fileinfo.line)^.CanBeRemoved := True;
              A_MOV, A_MOVZX, A_MOVSX:
                Begin
                  Case Pai386(p)^.op1t Of
{                    Top_Reg:
                      Case Pai386(p)^.op2t Of
                        Top_Reg:;
                        Top_Ref:;
                      End;}
                    Top_Ref:
                      Begin {destination is always a register in this case}
                        With PPaiProp(p^.fileinfo.line)^.Regs[Reg32(Tregister(Pai386(p)^.op2))] Do
                          Begin
                            If GetLastInstruction(p, hp1) And
                               (PPaiProp(hp1^.fileinfo.line)^.
                                Regs[Reg32(TRegister(Pai386(p)^.op2))].typ = con_ref) Then
  {so we don't try to check a sequence when the register only contains a constant}
                               If CheckSequence(p, TRegister(Pai386(p)^.op2), Cnt) And
                                  (Cnt > 0)
                                 Then
                                   Begin
                                     hp1 := nil;
   {although it's perfectly ok to remove an instruction which doesn't contain
    the register that we've just checked (CheckSequence takes care of that),
    the sequence containing this other register should also be completely
    checked and removed, otherwise we may get situations like this:

      movl 12(%ebp), %edx                       movl 12(%ebp), %edx
      movl 16(%ebp), %eax                       movl 16(%ebp), %eax
      movl 8(%edx), %edx                        movl 8(%edx), %edx
      movl (%eax), eax                          movl (%eax), eax
      cmpl %eax, %edx                           cmpl %eax, %edx
      jnz  l123           getting converted to  jnz  l123
      movl 12(%ebp), %edx                       movl 4(%eax), eax
      movl 16(%ebp), %eax
      movl 8(%edx), %edx
      movl 4(%eax), eax}
                                     hp2 := p;
                                     Cnt2 := 1;
                                     While Cnt2 <= Cnt Do
                                       Begin
                                         If (hp1 = nil) And
                                            Not(RegInInstruction(Tregister(Pai386(hp2)^.op2), p))
                                           Then hp1 := p;
                                         PPaiProp(p^.fileinfo.line)^.CanBeRemoved := True;
                                         Inc(Cnt2);
                                         GetNextInstruction(p, p);
                                       End;
                                     If hp1 <> nil Then p := hp1;
                                     Continue;
                                   End
                                 Else
                                   If (Cnt > 0) And
                                      (PPaiProp(p^.fileinfo.line)^.CanBeRemoved) Then
                                     Begin
                                       hp2 := p;
                                       Cnt2 := 1;
                                       While Cnt2 <= Cnt Do
                                         Begin
                                           If RegInInstruction(Tregister(Pai386(hp2)^.op2), p)
                                             Then PPaiProp(p^.fileinfo.line)^.CanBeRemoved := False;
                                           Inc(Cnt2);
                                           GetNextInstruction(p, p);
                                         End;
                                       Continue;
                                     End;
                          End;
                      End;
                    Top_Const:
                      Begin
                        Case Pai386(p)^.op2t Of
                          Top_Reg:
                            Begin
                              If GetLastInstruction(p, hp1) Then
                                With PPaiProp(hp1^.fileinfo.line)^.Regs[Reg32(TRegister(Pai386(p)^.op2))] Do
                                  If (Typ = Con_Const) And
                                     (StartMod = Pai386(p)^.op1) Then
                                    PPaiProp(p^.fileinfo.line)^.CanBeRemoved := True;
                            End;
{                          Top_Ref:;}
                        End;
                      End;
                  End;
                End;
              A_STD: If GetLastInstruction(p, hp1) And
                        (PPaiProp(hp1^.fileinfo.line)^.DirFlag = F_Set) Then
                        PPaiProp(Pai(p)^.fileinfo.line)^.CanBeRemoved := True;
              A_XOR:
                Begin
                  If (Pai386(p)^.op1t = top_reg) And
                     (Pai386(p)^.op2t = top_reg) And
                     (Pai386(p)^.op1 = Pai386(p)^.op2) And
                     GetLastInstruction(p, hp1) And
                     (PPaiProp(hp1^.fileinfo.line)^.Regs[Reg32(Tregister(Pai386(p)^.op1))].typ = con_const) And
                     (PPaiProp(hp1^.fileinfo.line)^.Regs[Reg32(Tregister(Pai386(p)^.op1))].StartMod = Pointer(0))
                    Then PPaiProp(p^.fileinfo.line)^.CanBeRemoved := True
                End
            End
          End;
      End;
      GetNextInstruction(p, p);
    End;
End;

Procedure RemoveInstructs(AsmL: PAasmOutput; First, Last: Pai);
{Removes the marked instructions and disposes the PPaiProps of the other
 instructions, restoring theirline number}
Var p, hp1: Pai;
    TmpLine, InstrCnt: Longint;
Begin
  p := First;
  If (p^.typ in SkipInstr) Then
    GetNextInstruction(p, p);
  InstrCnt := 1;
  While Assigned(p) Do
    If PPaiProp(p^.fileinfo.line)^.CanBeRemoved
      Then
        Begin
{$IfDef TP}
          Dispose(PPaiProp(p^.fileinfo.line));
{$EndIf}
          GetNextInstruction(p, hp1);
          AsmL^.Remove(p);
          Dispose(p, Done);
          p := hp1;
          Inc(InstrCnt)
        End
      Else
        Begin
{$IfDef TP}
          TmpLine := PPaiProp(p^.fileinfo.line)^.linesave;
          Dispose(PPaiProp(p^.fileinfo.line));
          p^.fileinfo.line := TmpLine;
{$Else TP}
          p^.fileinfo.line := PPaiProp(p^.fileinfo.line)^.linesave;
{$EndIf TP}
          GetNextInstruction(p, p);
          Inc(InstrCnt)
        End;
{$IfNDef TP}
    FreeMem(PaiPropBlock, NrOfPaiObjs*(((SizeOf(TPaiProp)+3)div 4)*4))
{$EndIf TP}
End;

Procedure CSE(AsmL: PAasmOutput; First, Last: Pai);
Begin
  DoCSE(First, Last);
  RemoveInstructs(AsmL, First, Last);
End;

End.

{
 $Log$
 Revision 1.5  1998-09-16 17:59:59  jonas
   * optimizer now completely dependant on GetNext/GetLast instruction, works again with -dRegAlloc

 Revision 1.4  1998/08/06 19:40:27  jonas
   * removed $ before and after Log in comment

 Revision 1.3  1998/08/05 16:00:12  florian
   * some fixes for ansi strings
   * log to Log changed

}
