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

Uses CObjects, verbose, hcodegen, globals
   {$ifdef i386}
     ,i386, DAOpt386
   {$endif i386}
     ;
{
Function PaiInSequence(P: Pai; Const Seq: TContent): Boolean;
Var P1: Pai;
    Counter: Byte;
    TmpResult: Boolean;
Begin
  TmpResult := False;
  P1 := Seq.StartMod;
  Counter := 1;
  While Not(TmpResult) And
        (Counter <= Seq.NrOfMods) Do
    Begin
      If (P = P1) Then TmpResult := True;
      Inc(Counter);
      p1 := Pai(p1^.Next);
    End;
  PaiInSequence := TmpResult;
End;
}
Function CheckSequence(p: Pai; Reg: TRegister; Var Found: Longint; Var RegInfo: TRegInfo): Boolean;
{checks whether the current instruction sequence (starting with p) and the
 one between StartMod and EndMod of Reg are the same. If so, the number of
 instructions that match is stored in Found and true is returned, otherwise
 Found holds the number of instructions between StartMod and EndMod and false
 is returned}
Var hp2, hp3, EndMod: Pai;
    PrevNonRemovablePai: Pai;
    OrgRegInfo, HighRegInfo: TRegInfo;
    HighFound, OrgRegFound: Byte;
    RegCounter: TRegister;
    OrgRegResult: Boolean;
    TmpResult: Boolean;
    OldNrOfMods: Byte;
Begin {CheckSequence}
  Reg := Reg32(Reg);
  TmpResult := False;
  FillChar(OrgRegInfo, SizeOf(OrgRegInfo), 0);
  OrgRegFound := 0;
  HighFound := 0;
  OrgRegResult := False;
  RegCounter := R_EAX;
  GetLastInstruction(p, PrevNonRemovablePai);
  While (PPaiProp(PrevNonRemovablePai^.fileinfo.line)^.Regs[RegCounter].Typ <> Con_Ref)
         And (RegCounter <= R_EDI) Do
    Inc(RegCounter);
  While (RegCounter <= R_EDI) Do
    Begin
      FillChar(RegInfo, SizeOf(RegInfo), 0);
      RegInfo.RegsEncountered := [ProcInfo.FramePointer, R_ESP];
      RegInfo.SubstRegs[ProcInfo.FramePointer] := ProcInfo.FramePointer;
      RegInfo.SubstRegs[R_ESP] := R_ESP;
      Found := 0;
      hp2 := PPaiProp(PrevNonRemovablePai^.fileinfo.line)^.Regs[RegCounter].StartMod;
      If (PrevNonRemovablePai <> PPaiProp(PrevNonRemovablePai^.fileinfo.line)^.Regs[RegCounter].StartMod)
        Then OldNrOfMods := PPaiProp(PrevNonRemovablePai^.fileinfo.line)^.Regs[RegCounter].NrOfMods
        Else OldNrOfMods := 1;
      hp3 := p;
      While (Found <> OldNrOfMods) And
                                  { old  new }
             InstructionsEquivalent(hp2, hp3, RegInfo) Do
        Begin
          GetNextInstruction(hp2, hp2);
          GetNextInstruction(hp3, hp3);
          Inc(Found)
        End;
      If (Found <> OldNrOfMods)
        Then
          Begin
(*              If ((Found+1) = OldNrOfMods) And
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
{$IfDef RegInfo}
              RefsEquivalent(TReference(Pai386(hp2)^.op1^),TReference(Pai386(hp3)^.op1^), RegInfo)
{$Else RegInfo}
              RefsEqual(TReference(Pai386(hp2)^.op1^),TReference(Pai386(hp3)^.op1^))
{$EndIf RegInfo}
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

               If (Pai386(hp2)^._operator = A_MOV)
                 Then
                   Begin
                    If (Pai386(hp2)^.Size = S_B) And
{$IfDef RegInfo}
                       RegsEquivalent(Reg8toReg32(TRegister(Pai386(hp2)^.op2)),
                                      TRegister(Pai386(hp3)^.op2), RegInfo)
{$Else RegInfo}
                        (Reg8toReg32(TRegister(Pai386(hp2)^.op2)) =
                         TRegister(Pai386(hp3)^.op2))
{$EndIf RegInfo}
                       Then
                         Begin
                           Pai386(hp2)^._operator := A_MOVZX;
                           Pai386(hp2)^.op2 := Pai386(hp3)^.op2;
                           Pai386(hp2)^.Size := S_BL;
                           Inc(Found);
                           TmpResult := True;
                         End
                       Else
                         Begin
                           TmpResult := False;
                           If (Found > 0) Then
                             Found := PPaiProp(Pai(p)^.fileinfo.line)^.Regs[Reg].NrOfMods
                         End
                   End
                 Else
                   Begin
                     If (Pai386(hp3)^.Size = S_B) And
{$IfDef RegInfo}
                       RegsEquivalent(TRegister(Pai386(hp2)^.op2),
                                      Reg8toReg32(TRegister(Pai386(hp3)^.op2)),
                                      RegInfo)
{$Else RegInfo}
                        (Reg8toReg32(TRegister(Pai386(hp3)^.op2)) =
                         TRegister(Pai386(hp2)^.op2))
{$EndIf RegInfo}
                       Then
                         Begin
                           TmpResult := True;
                           Inc(Found)
                         End
                       Else
                         Begin
                           TmpResult := False;
                           If (Found > 0) Then
                             Found := PPaiProp(Pai(p)^.fileinfo.line)^.Regs[Reg].NrOfMods
                         End
                   End
             Else *)
                Begin
                  TmpResult := False;
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
        Else TmpResult := True;
      If TmpResult And
         (Found > HighFound)
        Then
          Begin
            HighFound := Found;
            HighRegInfo := RegInfo;
          End;
      If (RegCounter = Reg) Then
        Begin
          OrgRegFound := Found;
          OrgRegResult := TmpResult;
          OrgRegInfo := RegInfo
        End;
      Repeat
        Inc(RegCounter);
      Until (RegCounter > R_EDI) or
            ((PPaiProp(PrevNonRemovablePai^.fileinfo.line)^.Regs[RegCounter].Typ = Con_Ref) {And
             ((Regcounter = Reg) Or
              Not(PaiInSequence(p, PPaiProp(PrevNonRemovablePai^.fileinfo.line)^.Regs[RegCounter]))) }
            );
    End;
  If (HighFound > 0) And
     (Not(OrgRegResult) Or
      (HighFound > OrgRegFound))
    Then
      Begin
        CheckSequence := True;
        RegInfo := HighRegInfo;
        Found := HighFound
      End
    Else
      Begin
        CheckSequence := OrgRegResult;
        Found := OrgRegFound;
        RegInfo := OrgRegInfo;
      End;
End; {CheckSequence}

Procedure DoCSE(AsmL: PAasmOutput; First, Last: Pai);
{marks the instructions that can be removed by RemoveInstructs. They're not
 removed immediately because sometimes an instruction needs to be checked in
 two different sequences}
Var Cnt, Cnt2: Longint;
    p, hp1, hp2: Pai;
    hp3, hp4: Pai;
{$ifdef csdebug}
    hp5: pai;
{$endif csdebug}
    RegInfo: TRegInfo;
    RegCounter: TRegister;
    TmpState: Word;
Begin
  p := First;
  If (p^.typ in (SkipInstr+[ait_marker])) Then
    GetNextInstruction(p, p);
  First := p;
  While Assigned(p) Do
    Begin
      Case p^.typ Of
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
  {so we don't try to check a sequence when the register only contains a constant}
                               If CheckSequence(p, TRegister(Pai386(p)^.op2), Cnt, RegInfo) And
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
                                            Not(RegInInstruction(Tregister(Pai386(hp2)^.op2), p) Or
                                                RegInInstruction(Reg32(Tregister(Pai386(hp2)^.op2)), p))
                                           Then hp1 := p;
{$ifndef noremove}
                                         PPaiProp(p^.fileinfo.line)^.CanBeRemoved := True;
{$endif noremove}
                                         Inc(Cnt2);
                                         GetNextInstruction(p, p);
                                       End;
                                     hp3 := New(Pai_Marker,Init(NoPropInfoStart));
                                     InsertLLItem(AsmL, Pai(hp2^.Previous), hp2, hp3);
   {imagine the following code:
         normal                    wrong optimized
     movl 8(%ebp), %eax           movl 8(%ebp), %eax
     movl (%eax), %eax            movl (%eax), %eax
     cmpl 8(%ebp), %eax           cmpl 8(%ebp), %eax
     jne l1                       jne l1
     movl 8(%ebp), %eax
     movl (%eax), %edi            movl %eax, %edi
     movl %edi, -4(%ebp)          movl %edi, -4(%ebp)
     movl 8(%ebp), %eax
     pushl 70(%eax)               pushl 70(%eax)

    The error is that at the moment that the last instruction is executed,
    %eax doesn't contain 8(%ebp) anymore. Solution: the contents of registers
    that are completely removed from a sequence, have to be changed to their
    contents from before the sequence.}

   {hp4 is used to get the contents of the registers before the sequence}
                                     GetLastInstruction(hp2, hp4);
   {If some registers were different in the old and the new sequence, move
    the contents of those old registers to the new ones}
{$IfDef CSDebug}
                For RegCounter := R_EAX To R_EDI Do
                  If (RegCounter in RegInfo.RegsLoadedForRef) Then
                    Begin
             hp5 := new(pai_asm_comment,init(strpnew('New: '+att_reg2str[RegCounter]+', Old: '+
                                                    att_reg2str[RegInfo.SubstRegs[RegCounter]])));
             InsertLLItem(AsmL, Pai(hp2^.previous), hp2, hp5);
                    End;
{$EndIf CSDebug}
                                     For RegCounter := R_EAX To R_EDI Do
                                       Begin
                                         If (RegInfo.SubstRegs[RegCounter] <> R_NO) Then
                                           If Not(RegCounter In RegInfo.RegsLoadedForRef) And
                                                          {new reg              old reg}
                                              (RegInfo.SubstRegs[RegCounter] <> RegCounter) Then

                                             Begin
                                               hp3 := New(Pai386,Op_Reg_Reg(A_MOV, S_L,
                                                                    {old reg          new reg}
                                                      RegInfo.SubstRegs[RegCounter], RegCounter));
                                               hp3^.fileinfo := hp2^.fileinfo;
                                               hp3^.fileinfo.line := PPaiProp(hp2^.fileinfo.line)^.LineSave;
                                               InsertLLItem(AsmL, Pai(hp2^.previous), hp2, hp3);
                                             End
                                           Else
                                             If (RegCounter In RegInfo.RegsLoadedForRef) Then
 {change the contents of this register to the its contents before the
  sequence (for all instructions in and after the sequence, until the register
  is reloaded)}
                                             Begin
 {load Cnt2 with the total number of instructions of this sequence}
                                               Cnt2 := PPaiProp(hp4^.fileinfo.line)^.
                                                       Regs[RegInfo.SubstRegs[RegCounter]].NrOfMods;
 {sometimes, a register can not be removed from a sequence, because it's
  still used afterwards:

  movl    -8(%ebp), %eax                        movl    -8(%ebp), %eax
  movl    70(%eax), %eax                        movl    70(%eax), %eax
  cmpl    74(%eax), %eax                        cmpl    74(%eax), %eax
  jne     l1               can't be changed to  jne     l1
  movl    -8(%ebp), %eax
  movl    70(%eax), %edi                        movl    %eax, %edi
  boundl  R_282, %edi                           boundl  R_282, %edi
  pushl   70(%eax)                              pushl   70(%eax)

  because eax now contains the wrong value when 70(%eax) is pushed}

                                               hp3 := hp2;
                                               For Cnt := 1 to Pred(Cnt2) Do
                                                 GetNextInstruction(hp3, hp3);
                                               TmpState := PPaiProp(hp3^.fileinfo.line)^.Regs[RegCounter].State;
                                               GetNextInstruction(hp3, hp3);
                                               If (TmpState <> PPaiProp(hp3^.fileinfo.line)^.Regs[RegCounter].State) Or
                                                  Not(RegCounter in PPaiProp(hp3^.fileinfo.line)^.UsedRegs) Then
                                                 Begin
{$ifdef csdebug}
             Writeln('Cnt2: ',Cnt2);
             hp5 := new(pai_asm_comment,init(strpnew('starting here...')));
             InsertLLItem(AsmL, Pai(hp2^.previous), hp2, hp5);
{$endif csdebug}
                                                   hp3 := hp2;
 {first change the contents of the register inside the sequence}
                                                   For Cnt := 1 to Cnt2 Do
                                                     Begin
 {save the state of the last pai object of the sequence for later use}
                                                       TmpState := PPaiProp(hp3^.fileinfo.line)^.Regs[RegCounter].State;
{$ifdef csdebug}
             hp5 := new(pai_asm_comment,init(strpnew('State for '+att_reg2str[Regcounter]+': '
                                                      +tostr(tmpstate))));
             InsertLLItem(AsmL, hp3, pai(hp3^.next), hp5);
{$endif csdebug}
                                                       PPaiProp(hp3^.fileinfo.line)^.Regs[RegCounter] :=
                                                         PPaiProp(hp4^.fileinfo.line)^.Regs[RegCounter];
                                                       GetNextInstruction(hp3, hp3);
                                                     End;
 {here, hp3 = p = Pai object right after the sequence, TmpState = state of
  RegCounter at the last Pai object of the sequence}
                                                   GetLastInstruction(hp3, hp3);
                                                   While GetNextInstruction(hp3, hp3) And
                                                         (PPaiProp(hp3^.fileinfo.line)^.Regs[RegCounter].State
                                                          = TmpState) Do
{$ifdef csdebug}
        begin
             hp5 := new(pai_asm_comment,init(strpnew('State for '+att_reg2str[Regcounter]+': '+
                                                      tostr(PPaiProp(hp3^.fileinfo.line)^.Regs[RegCounter].State))));
             InsertLLItem(AsmL, hp3, pai(hp3^.next), hp5);
{$endif csdebug}
                                                     PPaiProp(hp3^.fileinfo.line)^.Regs[RegCounter] :=
                                                       PPaiProp(hp4^.fileinfo.line)^.Regs[RegCounter];
{$ifdef csdebug}
        end;
{$endif csdebug}
                                                 End
                                               Else
                                                 Begin
{$ifdef csdebug}
                                                   Writeln('Got there for ',att_Reg2Str[RegCounter]);
{$endif csdebug}
                                                   hp3 := hp2;
                                                   For Cnt := 1 to Cnt2 Do
                                                     Begin
                                                       If RegModifiedByInstruction(RegCounter, hp3)
                                                         Then PPaiProp(hp3^.fileinfo.line)^.CanBeRemoved := False;
                                                       GetNextInstruction(hp3, hp3);
                                                     End;
                                                 End;
{$ifdef csdebug}
             hp5 := new(pai_asm_comment,init(strpnew('stopping here...')));
             InsertLLItem(AsmL, hp3, pai(hp3^.next), hp5);
{$endif csdebug}
                                             End;
                                       End;
                                     hp3 := New(Pai_Marker,Init(NoPropInfoEnd));
                                     InsertLLItem(AsmL, Pai(hp2^.Previous), hp2, hp3);
                                     If hp1 <> nil Then p := hp1;
                                     Continue;
                                   End
                                 Else
                                   If (Cnt > 0) And
                                      (PPaiProp(p^.fileinfo.line)^.
                                        Regs[Reg32(TRegister(Pai386(p)^.op2))].Typ = Con_Ref) And
                                      (PPaiProp(p^.fileinfo.line)^.CanBeRemoved) Then
                                     Begin
                                       hp2 := p;
                                       Cnt2 := 1;
                                       While Cnt2 <= Cnt Do
                                         Begin
                                           If RegInInstruction(Tregister(Pai386(hp2)^.op2), p) Or
                                              RegInInstruction(Reg32(Tregister(Pai386(hp2)^.op2)), p)
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
{$IfDef TP}
    TmpLine: Longint;
{$EndIf TP}
    InstrCnt: Longint;
Begin
  p := First;
  If (p^.typ in (SkipInstr + [ait_marker])) Then
    GetNextInstruction(p, p);
  InstrCnt := 1;
  While Assigned(p) Do
    Begin
{$ifndef noinstremove}
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
            Inc(InstrCnt);
          End
        Else
{$endif noinstremove}
          Begin
{$IfDef TP}
            TmpLine := PPaiProp(p^.fileinfo.line)^.linesave;
            Dispose(PPaiProp(p^.fileinfo.line));
            p^.fileinfo.line := TmpLine;
{$Else TP}
            p^.fileinfo.line := PPaiProp(p^.fileinfo.line)^.linesave;
{$EndIf TP}
            GetNextInstruction(p, p);
            Inc(InstrCnt);
          End;
    End;
{$IfNDef TP}
    FreeMem(PaiPropBlock, NrOfPaiObjs*(((SizeOf(TPaiProp)+3)div 4)*4))
{$EndIf TP}
End;

Procedure CSE(AsmL: PAasmOutput; First, Last: Pai);
Begin
  DoCSE(AsmL, First, Last);
  RemoveInstructs(AsmL, First, Last);
End;

End.

{
 $Log$
 Revision 1.10  1998-10-02 17:29:23  jonas
   * much better interregister CSE

 Revision 1.9  1998/10/01 20:21:49  jonas
   * inter-register CSE, still requires some tweaks (peepholeoptpass2, better  RegAlloc)

 Revision 1.8  1998/09/21 08:45:09  pierre
   + added vmt_offset in tobjectdef.write for fututre use
     (first steps to have objects without vmt if no virtual !!)
   + added fpu_used field for tabstractprocdef  :
     sets this level to 2 if the functions return with value in FPU
     (is then set to correct value at parsing of implementation)
     THIS MIGHT refuse some code with FPU expression too complex
     that were accepted before and even in some cases
     that don't overflow in fact
     ( like if f : float; is a forward that finally in implementation
      only uses one fpu register !!)
     Nevertheless I think that it will improve security on
     FPU operations !!
   * most other changes only for UseBrowser code
     (added symtable references for record and objects)
     local switch for refs to args and local of each function
     (static symtable still missing)
     UseBrowser still not stable and probably broken by
     the definition hash array !!

 Revision 1.7  1998/09/20 17:12:35  jonas
 * small fix for uncertain optimizations & more cleaning up

 Revision 1.5  1998/09/16 17:59:59  jonas
   * optimizer now completely dependant on GetNext/GetLast instruction, works again with -dRegAlloc

 Revision 1.4  1998/08/06 19:40:27  jonas
   * removed $ before and after Log in comment

 Revision 1.3  1998/08/05 16:00:12  florian
   * some fixes for ansi strings
   * log to Log changed

}
