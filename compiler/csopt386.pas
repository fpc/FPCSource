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
              If Assigned(newp^.previous)
                Then
                  Begin
                    TmpP := Pai(newp^.previous);
                    While Assigned (TmpP^.previous) And
                          PPaiProp(TmpP^.fileinfo.Line)^.CanBeRemoved Do
                      TmpP := Pai(TmpP^.previous);
                    TmpResult := Assigned(TmpP) And
                                 RegsSameContent(oldp, TmpP, Reg32(TRegister(Pai386(oldp)^.op1)))
                  End
                Else TmpResult := False;
            End;
        Top_Ref:
          With TReference(Pai386(oldp)^.op1^) Do
            Begin
              If (Base in RegsNotYetChecked) And
                 (Base <> R_NO) Then
                Begin
                  RegsNotYetChecked := RegsNotYetChecked - [Base];
                  If Assigned(newp^.previous)
                    Then
                      Begin
                        TmpP := Pai(newp^.previous);
                        While Assigned (TmpP^.previous) And
                              PPaiProp(TmpP^.fileinfo.Line)^.CanBeRemoved Do
                          TmpP := Pai(TmpP^.previous);
                        TmpResult := Assigned(TmpP) And
                                     RegsSameContent(oldp, TmpP, Base)
                      End
                    Else TmpResult := False;
                End;
              If TmpResult And
                 (Index <> R_NO) And
                 (Index in RegsNotYetChecked) Then
                Begin
                  RegsNotYetChecked := RegsNotYetChecked - [Index];
                  If Assigned(newp^.previous)
                    Then
                      Begin
                        TmpP := Pai(newp^.previous);
                        While Assigned (TmpP^.previous) And
                              PPaiProp(TmpP^.fileinfo.Line)^.CanBeRemoved Do
                          TmpP := Pai(TmpP^.previous);
                        TmpResult := Assigned(TmpP) And
                                     RegsSameContent(oldp, TmpP, Index)
                      End
                    Else TmpResult := False;
                End;
            End;
      End;
    NoChangedRegInRef := TmpResult;
  End;

Begin {CheckSequence}
  Reg := Reg32(Reg);
  Found := 0;
  hp2 := PPaiProp(Pai(p^.previous)^.fileinfo.line)^.Regs[Reg].StartMod;
  hp3 := p;
  EndMod := PPaiProp(Pai(p^.previous)^.fileinfo.line)^.Regs[Reg].StartMod;
  RegsNotYetChecked := [R_EAX..R_EDI];
  For Counter := 2 to PPaiProp(Pai(p^.previous)^.fileinfo.line)^.Regs[Reg].NrOfMods Do
    EndMod := Pai(EndMod^.Next);
  While (Found <> PPaiProp(Pai(p^.previous)^.fileinfo.line)^.Regs[Reg].NrOfMods) And
         InstructionsEqual(hp2, hp3) And
         NoChangedRegInRef(EndMod, hp3) Do
    Begin
      hp2 := Pai(hp2^.next);
      hp3 := Pai(hp3^.next);
      Inc(Found)
    End;
  If (Found <> PPaiProp(Pai(p^.previous)^.fileinfo.line)^.Regs[Reg].NrOfMods)
     Then
       Begin
         If ((Found+1) = PPaiProp(Pai(p^.previous)^.fileinfo.line)^.Regs[Reg].NrOfMods) And
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
  (otherwise CanBeRemoved can't be true, or can't have to be turned off).
  If it has already been processed by checkSequence and flagged to be
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
  While (p <> Pai(Last^.Next)) Do
    Begin
      Case p^.typ Of
        ait_label, ait_labeled_instruction:;
        ait_instruction:
          Begin
            Case Pai386(p)^._operator Of
              A_CLD: If Assigned(p^.previous) And
                        (PPaiProp(Pai(p^.previous)^.fileinfo.line)^.DirFlag = F_NotSet) Then
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
                            If Assigned(p^.previous) And
                               (PPaiProp(Pai(p^.previous)^.fileinfo.line)^.
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
                                     For Cnt2 := 1 to Cnt Do
                                       Begin
                                         { Note to Jonas :
                                           ait_stab_function_name is only at the begin of one function
                                           ait_stabn is only inserted in ag so you should not see any
                                           ait_stabs are only in head and tail of procs
                                           so you should no have problems with those neither !! (PM)
                                           Tell me if I am wrong
                                         If Not(Pai(p)^.typ In [ait_stabs, ait_stabn, ait_stab_function_name]) Then }
                                           Begin
                                             If (hp1 = nil) And
                                                Not(RegInInstruction(Tregister(Pai386(hp2)^.op2), p))
                                               Then hp1 := p;
                                             PPaiProp(p^.fileinfo.line)^.CanBeRemoved := True;
                                           End;
                                         p := Pai(p^.next);
                                       End;
                                     If hp1 <> nil Then p := hp1;
                                     Continue;
                                   End
                                 Else
                                   If (Cnt > 0) And
                                      (PPaiProp(p^.fileinfo.line)^.CanBeRemoved) Then
                                     Begin
                                       hp2 := p;
                                       For Cnt2 := 1 to Cnt Do
                                         Begin
                                           If RegInInstruction(Tregister(Pai386(hp2)^.op2), p)
                                             Then PPaiProp(p^.fileinfo.line)^.CanBeRemoved := False;
                                           p := Pai(p^.Next)
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
                              If Assigned(p^.previous) Then
                                With PPaiProp(Pai(p^.previous)^.fileinfo.line)^.Regs[Reg32(TRegister(Pai386(p)^.op2))] Do
                                  If (Typ = Con_Const) And
                                     (StartMod = Pai386(p)^.op1) Then
                                    PPaiProp(p^.fileinfo.line)^.CanBeRemoved := True;
                            End;
                          Top_Ref:;
                        End;
                      End;
                  End;
                End;
              A_STD: If Assigned(p^.previous) And
                        (PPaiProp(Pai(p^.previous)^.fileinfo.line)^.DirFlag = F_Set) Then
                        PPaiProp(Pai(p)^.fileinfo.line)^.CanBeRemoved := True;
              A_XOR:
                Begin
                  If (Pai386(p)^.op1t = top_reg) And
                     (Pai386(p)^.op2t = top_reg) And
                     (Pai386(p)^.op1 = Pai386(p)^.op2) And
                     Assigned(p^.previous) And
                     (PPaiProp(Pai(p^.previous)^.fileinfo.line)^.Regs[Reg32(Tregister(Pai386(p)^.op1))].typ = con_const) And
                     (PPaiProp(Pai(p^.previous)^.fileinfo.line)^.Regs[Reg32(Tregister(Pai386(p)^.op1))].StartMod = Pointer(0))
                    Then PPaiProp(p^.fileinfo.line)^.CanBeRemoved := True
                End
            End
          End;
      End;
      p := Pai(p^.next);
    End;
End;

Procedure RemoveInstructs(AsmL: PAasmOutput; First, Last: Pai);
{Removes the marked instructions and disposes the PPaiProps of the other
 instructions, restoring theirline number}
Var p, hp1: Pai;
    TmpLine, InstrCnt: Longint;
Begin
  p := First;
  InstrCnt := 1;
  While (p <> Pai(Last^.Next)) Do
    If PPaiProp(p^.fileinfo.line)^.CanBeRemoved
      Then
        Begin
          If (InstrCnt > NrOfPaiFast) Then
            Dispose(PPaiProp(p^.fileinfo.line));
          hp1 := Pai(p^.Next);
          AsmL^.Remove(p);
          Dispose(p, Done);
          p := hp1;
          Inc(InstrCnt)
        End
      Else
        Begin
          If (InstrCnt > NrOfPaiFast)
            Then
              Begin
                TmpLine := PPaiProp(p^.fileinfo.line)^.linesave;
                Dispose(PPaiProp(p^.fileinfo.line));
                p^.fileinfo.line := TmpLine;
              End
            Else p^.fileinfo.line := PPaiProp(p^.fileinfo.line)^.linesave;
          p := Pai(p^.Next);
          Inc(InstrCnt)
        End;
  If (NrOfPaiFast > 0) Then
{$IfDef TP}
    Freemem(PaiPropBlock, NrOfPaiFast*(((SizeOf(TPaiProp)+1)div 2)*2))
{$Else}
    FreeMem(PaiPropBlock, NrOfPaiFast*(((SizeOf(TPaiProp)+3)div 4)*4))
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
 Revision 1.4  1998-08-06 19:40:27  jonas
   * removed $ before and after Log in comment

 Revision 1.3  1998/08/05 16:00:12  florian
   * some fixes for ansi strings
   * log to Log changed

}
