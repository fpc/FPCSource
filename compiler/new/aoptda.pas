{
    $Id$
    Copyright (c) 1999 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit contains the assembler optimizer data flow analyzer.

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
Unit aoptda;

Interface

uses Aasm;

Type TAsmDFA = Object(TAoptCpu)
       Constructor Init(_AsmL: PaasmOutPut; _Blockstart, _Blockend: Pai);
{ returns a pointer to the LabelInfo table }
       Function GetLabelInfo: PLabelInfo;
       Destructor Done;

       private

       AsmL: PAasmOutput;
       LabelInfo: TLabelInfo;

{ How many instructions are between the current instruction and the last one }
{ that modified the register                                                 }
      NrOfInstrSinceLastMod: TInstrSinceLastMod;

      Procedure BuildLabelTableAndFixRegAlloc;
      Function FindLoHiLabels(BlockStart: Pai): Pai;

     End;

Implementation

uses aoptmsc
{$ifdef i386}
, ao386msc
{$endif i386}
;

Constructor TAsmDFA.Init(_AsmL: PaasmOutPut; _Blockstart: Pai;
                         Var _BlockEnd: Pai);
Begin
  AsmL := _AsmL;
  LabelInfo.Lowabel := High(AWord);
  BlockStart := _BlockStart;
{ initializes BlockEnd and through the methodcall also the labeltable,       }
{ lolab, hilab and labeldif. Also, the regalloc info gets corrected.         }
  BlockEnd := FindLoHiLabels;
  BuildLabelTableAndFixRegAlloc;
End;

Function TAsmDFA.GetLabelInfo: TLabelInfo;
Begin
  GetLabelInfo := LabelInfo;
End;

Function TAsmDFA.FindLoHiLabels: Pai;
{ Walks through the paasmlist to find the lowest and highest label number.  }
{ Returns the last Pai object of the current block                          }
Var LabelFound: Boolean;
    P: Pai;
Begin
  LabelFound := False;
  P := BlockStart;
  While Assigned(P) And
        ((P^.typ <> Ait_Marker) Or
         (Pai_Marker(P)^.Kind <> AsmBlockStart)) Do
    Begin
      If (Pai(p)^.typ = ait_label) Then
        If (Pai_Label(p)^.l^.is_used)
          Then
            Begin
              LabelFound := True;
              If (Pai_Label(p)^.l^.labelnr < LowLabel) Then
                LowLabel := Pai_Label(p)^.l^.labelnr;
              If (Pai_Label(p)^.l^.labelnr > HighLabel) Then
                HighLabel := Pai_Label(p)^.l^.labelnr;
            End;
      GetNextInstruction(p, p);
    End;
  FindLoHiLabels := p;
  If LabelFound
    Then LabelDif := HighLabel-LowLabel+1
    Else LabelDif := 0;
End;

Procedure TAsmDFA.BuildLabelTableAndFixRegAlloc;
{ Builds a table with the locations of the labels in the paasmoutput.       }
{ Also fixes some RegDeallocs like "# %eax released; push (%eax)"           }
Var p, hp1, hp2: Pai;
    UsedRegs: TRegSet;
Begin
  UsedRegs := [];
  With LabelInfo Do
    If (LabelDif <> 0) Then
      Begin
        GetMem(LabelTable, LabelDif*SizeOf(TLabelTableItem));
        FillChar(LabelTable^, LabelDif*SizeOf(TLabelTableItem), 0);
        p := BlockStart;
        While (P <> BlockEnd) Do
          Begin
            Case p^.typ Of
              ait_Label:
                If Pai_Label(p)^.l^.is_used Then
                  LabelTable^[Pai_Label(p)^.l^.labelnr-LowLabel].PaiObj := p;
              ait_regAlloc:
                begin
                  if PairegAlloc(p)^.Allocation then
                    Begin
                      If Not(PaiRegAlloc(p)^.Reg in UsedRegs) Then
                        UsedRegs := UsedRegs + [PaiRegAlloc(p)^.Reg]
                      Else
                        Begin
                          hp1 := p;
                          hp2 := nil;
                          While GetLastInstruction(hp1, hp1) And
                                Not(RegInInstruction(PaiRegAlloc(p)^.Reg, hp1)) Do
                            hp2 := hp1;
                          If hp2 <> nil Then
                            Begin
                              hp1 := New(PaiRegAlloc, DeAlloc(PaiRegAlloc(p)^.Reg));
                              InsertLLItem(AsmL, Pai(hp2^.previous), hp2, hp1);
                            End;
                        End;
                    End
                  else
                    Begin
                      UsedRegs := UsedRegs - [PaiRegAlloc(p)^.Reg];
                      hp1 := p;
                      hp2 := nil;
                      While Not(FindRegAlloc(PaiRegAlloc(p)^.Reg, Pai(hp1^.Next))) And
                            GetNextInstruction(hp1, hp1) And
                            RegInInstruction(PaiRegAlloc(p)^.Reg, hp1) Do
                        hp2 := hp1;
                      If hp2 <> nil Then
                        Begin
                          hp1 := Pai(p^.previous);
                          AsmL^.Remove(p);
                          InsertLLItem(AsmL, hp2, Pai(hp2^.Next), p);
                          p := hp1;
                        End;
                End;
            end;
          End;
        P := Pai(p^.Next);
        While Assigned(p) And
              (p^.typ in (SkipInstr - [ait_regalloc])) Do
          P := Pai(P^.Next);
      End;
End;

Destructor TAsmDFA.Done;
Begin
  If Assigned(LabelInfo.LabelTable) Then Dispose(LabelInfo.LabelTable);
End;

Procedure TAsmOptimizer.DoDFAPass2;
{ Analyzes the Data Flow of an assembler list. Analyses the reg contents     }
{ for the instructions between blockstart and blockend. Returns the last pai }
{ which has been processed                                                   }
Var
    CurProp: PPaiProp;
    Cnt, InstrCnt : Longint;
    InstrProp: TAsmInstrucProp;
    UsedRegs: TRegSet;
    p, hp, NewBlockStart : Pai;
    TmpRef: TReference;
    TmpReg: TRegister;
{$ifdef AnalyzeLoops}
    TmpState: Byte;
{$endif AnalyzeLoops}
Begin
  p := BlockStart;
  UsedRegs.init;
  UsedRegs.Update(p);
  NewBlockStart := SkipHead(p);
  InstrCnt := 1;
{ done implicitely by the constructor
  FillChar(NrOfInstrSinceLastMod, SizeOf(NrOfInstrSinceLastMod), 0); }
  While (P <> BlockEnd) Do
    Begin
      CurProp := New(PPaiProp, init);
      If (p <> NewBlockStart)
        Then
          Begin
{$ifdef JumpAnal}
            If (p^.Typ <> ait_label) Then
{$endif JumpAnal}
              Begin
                GetLastInstruction(p, hp);
                CurProp^.Regs := PPaiProp(hp^.OptInfo)^.Regs;
                CurProp^.CondRegs.Flags :=
                  PPaiProp(hp^.OptInfo)^.CondRegs.Flags;
              End
          End;
      CurProp^.UsedRegs.InitWithValue(UsedRegs.GetUsedRegs);
{      CurProp^.CanBeRemoved := False;}
      UsedRegs.Update(Pai(p^.Next)));
      PPaiProp(p^.OptInfo) := CurProp;
      For TmpReg := R_EAX To R_EDI Do
        Inc(NrOfInstrSinceLastMod[TmpReg]);
      Case p^.typ Of
        ait_label:
{$Ifndef JumpAnal}
          If (Pai_label(p)^.l^.is_used) Then
            CurProp^.DestroyAllRegs;
{$Else JumpAnal}
          Begin
           If (Pai_Label(p)^.is_used) Then
             With LTable^[Pai_Label(p)^.l^.labelnr-LoLab] Do
{$IfDef AnalyzeLoops}
              If (RefsFound = Pai_Label(p)^.l^.RefCount)
{$Else AnalyzeLoops}
              If (JmpsProcessed = Pai_Label(p)^.l^.RefCount)
{$EndIf AnalyzeLoops}
                Then
{all jumps to this label have been found}
{$IfDef AnalyzeLoops}
                  If (JmpsProcessed > 0)
                    Then
{$EndIf AnalyzeLoops}
 {we've processed at least one jump to this label}
                      Begin
                        If (GetLastInstruction(p, hp) And
                           Not(((hp^.typ = ait_instruction)) And
                                (pai386_labeled(hp)^.is_jmp))
                          Then
  {previous instruction not a JMP -> the contents of the registers after the
   previous intruction has been executed have to be taken into account as well}
                            For TmpReg := R_EAX to R_EDI Do
                              Begin
                                If (CurProp^.Regs[TmpReg].WState <>
                                    PPaiProp(hp^.OptInfo)^.Regs[TmpReg].WState)
                                  Then DestroyReg(CurProp, TmpReg)
                              End
                      End
{$IfDef AnalyzeLoops}
                    Else
 {a label from a backward jump (e.g. a loop), no jump to this label has
  already been processed}
                      If GetLastInstruction(p, hp) And
                         Not(hp^.typ = ait_instruction) And
                            (pai386_labeled(hp)^.opcode = A_JMP))
                        Then
  {previous instruction not a jmp, so keep all the registers' contents from the
   previous instruction}
                          Begin
                            CurProp^.Regs := PPaiProp(hp^.OptInfo)^.Regs;
                            CurProp^.DirFlag := PPaiProp(hp^.OptInfo)^.DirFlag;
                          End
                        Else
  {previous instruction a jmp and no jump to this label processed yet}
                          Begin
                            hp := p;
                            Cnt := InstrCnt;
     {continue until we find a jump to the label or a label which has already
      been processed}
                            While GetNextInstruction(hp, hp) And
                                  Not((hp^.typ = ait_instruction) And
                                      (pai386(hp)^.is_jmp) and
                                      (pasmlabel(pai386(hp)^.oper[0].sym)^.labelnr = Pai_Label(p)^.l^.labelnr)) And
                                  Not((hp^.typ = ait_label) And
                                      (LTable^[Pai_Label(hp)^.l^.labelnr-LoLab].RefsFound
                                       = Pai_Label(hp)^.l^.RefCount) And
                                      (LTable^[Pai_Label(hp)^.l^.labelnr-LoLab].JmpsProcessed > 0)) Do
                              Inc(Cnt);
                            If (hp^.typ = ait_label)
                              Then
   {there's a processed label after the current one}
                                Begin
                                  CurProp^.Regs := PaiPropBlock^[Cnt].Regs;
                                  CurProp^.DirFlag := PaiPropBlock^[Cnt].DirFlag;
                                End
                              Else
   {there's no label anymore after the current one, or they haven't been
    processed yet}
                                Begin
                                  GetLastInstruction(p, hp);
                                  CurProp^.Regs := PPaiProp(hp^.OptInfo)^.Regs;
                                  CurProp^.DirFlag := PPaiProp(hp^.OptInfo)^.DirFlag;
                                  DestroyAllRegs(PPaiProp(hp^.OptInfo))
                                End
                          End
{$EndIf AnalyzeLoops}
                Else
{not all references to this label have been found, so destroy all registers}
                  Begin
                    GetLastInstruction(p, hp);
                    CurProp^.Regs := PPaiProp(hp^.OptInfo)^.Regs;
                    CurProp^.DirFlag := PPaiProp(hp^.OptInfo)^.DirFlag;
                    DestroyAllRegs(CurProp)
                  End;
          End;
{$EndIf JumpAnal}

{$ifdef GDB}
        ait_stabs, ait_stabn, ait_stab_function_name:;
{$endif GDB}

        ait_instruction:
          Begin
            if pai386(p)^.is_jmp then
             begin
{$IfNDef JumpAnal}
  ;
{$Else JumpAnal}
          With LTable^[pasmlabel(pai386(p)^.oper[0].sym)^.labelnr-LoLab] Do
            If (RefsFound = pasmlabel(pai386(p)^.oper[0].sym)^.RefCount) Then
              Begin
                If (InstrCnt < InstrNr)
                  Then
                {forward jump}
                    If (JmpsProcessed = 0) Then
                {no jump to this label has been processed yet}
                      Begin
                        PaiPropBlock^[InstrNr].Regs := CurProp^.Regs;
                        PaiPropBlock^[InstrNr].DirFlag := CurProp^.DirFlag;
                        Inc(JmpsProcessed);
                      End
                    Else
                      Begin
                        For TmpReg := R_EAX to R_EDI Do
                          If (PaiPropBlock^[InstrNr].Regs[TmpReg].WState <>
                             CurProp^.Regs[TmpReg].WState) Then
                            DestroyReg(@PaiPropBlock^[InstrNr], TmpReg);
                        Inc(JmpsProcessed);
                      End
{$ifdef AnalyzeLoops}
                  Else
{                backward jump, a loop for example}
{                    If (JmpsProcessed > 0) Or
                       Not(GetLastInstruction(PaiObj, hp) And
                           (hp^.typ = ait_labeled_instruction) And
                           (pai386_labeled(hp)^.opcode = A_JMP))
                      Then}
{instruction prior to label is not a jmp, or at least one jump to the label
 has yet been processed}
                        Begin
                          Inc(JmpsProcessed);
                          For TmpReg := R_EAX to R_EDI Do
                            If (PaiPropBlock^[InstrNr].Regs[TmpReg].WState <>
                                CurProp^.Regs[TmpReg].WState)
                              Then
                                Begin
                                  TmpState := PaiPropBlock^[InstrNr].Regs[TmpReg].WState;
                                  Cnt := InstrNr;
                                  While (TmpState = PaiPropBlock^[Cnt].Regs[TmpReg].WState) Do
                                    Begin
                                      DestroyReg(@PaiPropBlock^[Cnt], TmpReg);
                                      Inc(Cnt);
                                    End;
                                  While (Cnt <= InstrCnt) Do
                                    Begin
                                      Inc(PaiPropBlock^[Cnt].Regs[TmpReg].WState);
                                      Inc(Cnt)
                                    End
                                End;
                        End
{                      Else }
{instruction prior to label is a jmp and no jumps to the label have yet been
 processed}
{                        Begin
                          Inc(JmpsProcessed);
                          For TmpReg := R_EAX to R_EDI Do
                            Begin
                              TmpState := PaiPropBlock^[InstrNr].Regs[TmpReg].WState;
                              Cnt := InstrNr;
                              While (TmpState = PaiPropBlock^[Cnt].Regs[TmpReg].WState) Do
                                Begin
                                  PaiPropBlock^[Cnt].Regs[TmpReg] := CurProp^.Regs[TmpReg];
                                  Inc(Cnt);
                                End;
                              TmpState := PaiPropBlock^[InstrNr].Regs[TmpReg].WState;
                              While (TmpState = PaiPropBlock^[Cnt].Regs[TmpReg].WState) Do
                                Begin
                                  DestroyReg(@PaiPropBlock^[Cnt], TmpReg);
                                  Inc(Cnt);
                                End;
                              While (Cnt <= InstrCnt) Do
                                Begin
                                  Inc(PaiPropBlock^[Cnt].Regs[TmpReg].WState);
                                  Inc(Cnt)
                                End
                            End
                        End}
{$endif AnalyzeLoops}
          End;
{$EndIf JumpAnal}
          end
          else
           begin
            InstrProp := AsmInstr[PInstr(p)^.opcode];
            If IsStoreInstr(p) Then
              Begin
                CurProp^.ReadReg(PInstr(p)^.oper[StoreSrc].reg);
                CurProp^.ReadRef(PInstr(p)^.oper[StoreDst].ref);
                CurProp^.DestroyRefs(PInstr(p)^.oper[StoreDst].ref^,
                                     PInstr(p)^.oper[StoreSrc].reg);
              End
            Else If IsLoadInstr(p) Then
              Begin
                CurProp^.ReadRef(PInstr(p)^.oper[LoadSrc].ref);
                CurProp^.ReadReg(PInstr(p)^.oper[LoadDst].reg);
                TmpReg := RegMaxSize(PInstr(p)^.oper[1].reg);
                        If RegInRef(TmpReg, Pai386(p)^.oper[0].ref^) And
                           (CurProp^.Regs[TmpReg].Typ = Con_Ref)
                          Then
                            Begin
                              With CurProp^.Regs[TmpReg] Do
                                Begin
                                  IncState(WState);
 {also store how many instructions are part of the sequence in the first
  instructions PPaiProp, so it can be easily accessed from within
  CheckSequence}
                                  Inc(NrOfMods, NrOfInstrSinceLastMod[TmpReg]);
                                  PPaiProp(Pai(StartMod)^.OptInfo)^.Regs[TmpReg].NrOfMods := NrOfMods;
                                  NrOfInstrSinceLastMod[TmpReg] := 0;
                                End;
                            End
                          Else
                            Begin
                              DestroyReg(CurProp, TmpReg);
                              If Not(RegInRef(TmpReg, Pai386(p)^.oper[0].ref^)) Then
                                With CurProp^.Regs[TmpReg] Do
                                  Begin
                                    Typ := Con_Ref;
                                    StartMod := p;
                                    NrOfMods := 1;
                                  End
                            End;
{$ifdef StateDebug}
                  hp := new(pai_asm_comment,init(strpnew(att_reg2str[TmpReg]+': '+tostr(CurProp^.Regs[TmpReg].WState))));
                  InsertLLItem(AsmL, p, p^.next, hp);
{$endif StateDebug}

                      End;
                    Top_Const:
                      Begin
                        Case Pai386(p)^.oper[1].typ Of
                          Top_Reg:
                            Begin
                              TmpReg := Reg32(Pai386(p)^.oper[1].reg);
                              With CurProp^.Regs[TmpReg] Do
                                Begin
                                  DestroyReg(CurProp, TmpReg);
                                  typ := Con_Const;
                                  StartMod := p;
                                End
                            End;
                          Top_Ref:
                            Begin
                              ReadRef(CurProp, Pai386(p)^.oper[1].ref);
                              DestroyRefs(P, Pai386(p)^.oper[1].ref^, R_NO);
                            End;
                        End;
                      End;
                End;
              End;
              A_IMUL:
                Begin
                  ReadOp(CurProp, Pai386(p)^.oper[0]);
                  ReadOp(CurProp, Pai386(p)^.oper[1]);
                  If (Pai386(p)^.oper[2].typ = top_none) Then
                     If (Pai386(p)^.oper[1].typ = top_none) Then
                         Begin
                           DestroyReg(CurProp, R_EAX);
                           DestroyReg(CurProp, R_EDX)
                         End
                       Else
{$ifdef arithopt}
                         AddInstr2OpContents(Pai386(p), Pai386(p)^.oper[1])
{$else arithopt}
                         DestroyOp(p, Pai386(p)^.oper[1])
{$endif arithopt}
                  Else
{$ifdef arithopt}
                    AddInstr2OpContents(Pai386(p), Pai386(p)^.oper[2]);
{$else arithopt}
                    DestroyOp(p, Pai386(p)^.oper[2]);
{$endif arithopt}
                End;
              A_XOR:
                Begin
                  ReadOp(CurProp, Pai386(p)^.oper[0]);
                  ReadOp(CurProp, Pai386(p)^.oper[1]);
                  If (Pai386(p)^.oper[0].typ = top_reg) And
                     (Pai386(p)^.oper[1].typ = top_reg) And
                     (Pai386(p)^.oper[0].reg = Pai386(p)^.oper[1].reg)
                    Then
                      Begin
                        DestroyReg(CurProp, Pai386(p)^.oper[0].reg);
                        CurProp^.Regs[Reg32(Pai386(p)^.oper[0].reg)].typ := Con_Const;
                        CurProp^.Regs[Reg32(Pai386(p)^.oper[0].reg)].StartMod := Pointer(0)
                      End
                    Else
                      DestroyOp(p, Pai386(p)^.oper[1]);
                End
              Else
                Begin
                  Cnt := 1;
                  While (Cnt <= MaxCh) And
                        (InstrProp.Ch[Cnt] <> C_None) Do
                    Begin
                      Case InstrProp.Ch[Cnt] Of
                        C_REAX..C_REDI: ReadReg(CurProp,TCh2Reg(InstrProp.Ch[Cnt]));
                        C_WEAX..C_RWEDI:
                          Begin
                            If (InstrProp.Ch[Cnt] >= C_RWEAX) Then
                              ReadReg(CurProp, TCh2Reg(InstrProp.Ch[Cnt]));
                            DestroyReg(CurProp, TCh2Reg(InstrProp.Ch[Cnt]));
                          End;
{$ifdef arithopt}
                        C_MEAX..C_MEDI:
                          AddInstr2RegContents({$ifdef statedebug} asml, {$endif}
                                               Pai386(p),
                                               TCh2Reg(InstrProp.Ch[Cnt]));
{$endif arithopt}
                        C_CDirFlag: CurProp^.DirFlag := F_NotSet;
                        C_SDirFlag: CurProp^.DirFlag := F_Set;
                        C_Rop1: ReadOp(CurProp, Pai386(p)^.oper[0]);
                        C_Rop2: ReadOp(CurProp, Pai386(p)^.oper[1]);
                        C_ROp3: ReadOp(CurProp, Pai386(p)^.oper[2]);
                        C_Wop1..C_RWop1:
                          Begin
                            If (InstrProp.Ch[Cnt] in [C_RWop1]) Then
                              ReadOp(CurProp, Pai386(p)^.oper[0]);
                            DestroyOp(p, Pai386(p)^.oper[0]);
                          End;
{$ifdef arithopt}
                        C_Mop1:
                          AddInstr2OpContents({$ifdef statedebug} asml, {$endif}
                          Pai386(p), Pai386(p)^.oper[0]);
{$endif arithopt}
                        C_Wop2..C_RWop2:
                          Begin
                            If (InstrProp.Ch[Cnt] = C_RWop2) Then
                              ReadOp(CurProp, Pai386(p)^.oper[1]);
                            DestroyOp(p, Pai386(p)^.oper[1]);
                          End;
{$ifdef arithopt}
                        C_Mop2:
                          AddInstr2OpContents({$ifdef statedebug} asml, {$endif}
                          Pai386(p), Pai386(p)^.oper[1]);
{$endif arithopt}
                        C_WOp3..C_RWOp3:
                          Begin
                            If (InstrProp.Ch[Cnt] = C_RWOp3) Then
                              ReadOp(CurProp, Pai386(p)^.oper[2]);
                            DestroyOp(p, Pai386(p)^.oper[2]);
                          End;
{$ifdef arithopt}
                        C_Mop3:
                          AddInstr2OpContents({$ifdef statedebug} asml, {$endif}
                          Pai386(p), Pai386(p)^.oper[2]);
{$endif arithopt}
                        C_WMemEDI:
                          Begin
                            ReadReg(CurProp, R_EDI);
                            FillChar(TmpRef, SizeOf(TmpRef), 0);
                            TmpRef.Base := R_EDI;
                            DestroyRefs(p, TmpRef, R_NO)
                          End;
                        C_RFlags, C_WFlags, C_RWFlags, C_FPU:
                        Else
                          Begin
                            DestroyAllRegs(CurProp);
                          End;
                      End;
                      Inc(Cnt);
                    End
                End;
              end;
            End;
          End
        Else
          Begin
            DestroyAllRegs(CurProp);
          End;
      End;
      Inc(InstrCnt);
      GetNextInstruction(p, p);
    End;
End;


End.