{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl and Jonas Maebe

    This unit does optimizations on the assembler code for i386+

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
Unit aopt386;

  interface

    uses aasm;

    { does simple optimizations like jumps and remove pop/pushes }
    procedure peepholeopt(asml : paasmoutput);

  implementation

    uses
       cobjects,globals,symtable,strings,verbose,hcodegen
{$ifdef i386}
       ,i386
       ,cgi386
{$else}
{$endif}
       ;

  Type
{$ifdef tp}
       TLabelTable = Array[0..10000] Of Pai;
{$else}
       TLabelTable = Array[0..2500000] Of Pai;
{$endif}
       PLabelTable = ^TLabelTable;

Var LoLab, HiLab, LabDif: Longint;
    LTable: PLabelTable;

  Function RefsEqual(const r1,r2 : treference) : boolean;

  begin
     if r1.isintvalue
       then RefsEqual:=r2.isintvalue and (r1.offset=r2.offset)
       else if (r1.offset=r2.offset) and (r1.base=r2.base) and
               (r1.index=r2.index) and (r1.segment=r2.segment) and
               (r1.scalefactor=r2.scalefactor)
              then
                begin
                  if assigned(r1.symbol)
                    then RefsEqual:=assigned(r2.symbol) and (r1.symbol^=r2.symbol^)
                    else RefsEqual:=not(assigned(r2.symbol));
                end
              Else RefsEqual := False;
  end;

{$i aopt386.inc}
{aopt386.inc contains the reloading optimizer}

  Function FindLabel(L: PLabel; Var hp: Pai): Boolean;

  {searches for the specified label starting from hp as long as the
   encountered instructions are labels, to be able to optimize constructs like

   jne l2              jmp l2
   jmp l3     and      l1:
   l1:                 l2:
   l2:}

  Var TempP: Pai;

  Begin
    TempP := hp;
    While Assigned(TempP) and (pai(TempP)^.typ = ait_label) Do
      If (pai_label(TempP)^.l <> L)
        Then TempP := Pai(TempP^.next)
        Else
          Begin
            hp := TempP;
            FindLabel := True;
            exit
          End;
    FindLabel := False
  End;

  Function PowerOf2(L: Longint): Longint;
  Var Counter, TempVal: Longint;
  Begin
    TempVal := 1;
    For Counter := 1 to L Do
      TempVal := TempVal * 2;
    PowerOf2 := TempVal;
  End;

  Procedure DoOptimize(asml : paasmoutput);

  var
      p,hp1,hp2 : pai;
      TmpBool1, TmpBool2: Boolean;

      TmpRef: PReference;


    { inserts new_one between prev and foll }
    Procedure InsertLLItem(prev, foll, new_one: PLinkedList_Item);
    Begin
      If Assigned(prev)
        Then
          If Assigned(foll)
            Then
              Begin
                If Assigned(new_one) Then
                  Begin
                    new_one^.last := prev;
                    new_one^.next := foll;
                    prev^.next := new_one;
                    foll^.last := new_one;
                  End;
              End
            Else AsmL^.Concat(new_one)
        Else If Assigned(Foll) Then AsmL^.Insert(new_one)
    End;


    Function GetNextInstr(hp: Pai): Pai;
    {skips all labels and returns the next "real" instruction; it is assumed
     that hp is of the type ait_label}
    Begin
      While assigned(hp^.next) and (pai(hp^.next)^.typ = ait_label) Do
        hp := pai(hp^.next);
      If assigned(hp^.next)
        Then GetNextInstr := pai(hp^.next)
        Else GetNextInstr := hp;
    End;

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

    Begin
      If (hp^.lab^.nb >= LoLab) and
         (hp^.lab^.nb <= HiLab) and   {range check, necessary?}
         (Pointer(LTable^[hp^.lab^.nb-LoLab]) <> Pointer(0)) Then
        Begin
          p1 := LTable^[hp^.lab^.nb-LoLab]; {the jump's destination}
          p1 := GetNextInstr(p1);
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

    Function IsGP32Reg(Reg: TRegister): Boolean;
    {Checks if the register is a 32 bit general purpose register}
    Begin
      If (Reg >= R_EAX) and (Reg <= R_EBX)
        Then IsGP32Reg := True
        Else IsGP32reg := False
    End;

  type  twowords=record
                word1,word2:word;
            end;

  begin
    p:=pai(asml^.first);
    while assigned(p) do
       begin
         if (p^.typ=ait_labeled_instruction) then
           begin
  {the following if-block removes all code between a jmp and the next label,
   because it can never be executed}
             If (pai_labeled(p)^._operator = A_JMP) Then
               Begin
                 hp1 := pai(p^.next);
                 While Assigned(hp1) and (hp1^.typ <> ait_label) Do
                   Begin
                     AsmL^.Remove(hp1);
                     Dispose(hp1, done);
                     hp1 := pai(p^.next);
                   End;
               End;
             if (assigned(p^.next)) then
               begin
                 hp2 := pai(p^.next^.next);
                 if (pai(p^.next)^.typ=ait_labeled_instruction) and
                    (pai_labeled(p^.next)^._operator=A_JMP) and
                    FindLabel(pai_labeled(p)^.lab, hp2) then
                    begin
                      case pai_labeled(p)^._operator of
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
                        else
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
                          AsmL^.remove(hp2);
                          Dispose(hp2, done);
                        End;
                      pai_labeled(p)^.lab:=pai_labeled(p^.next)^.lab;
                      Inc(pai_labeled(p)^.lab^.refcount);
                      hp1:=pai(p^.next);
                      asml^.remove(hp1);
                      dispose(hp1,done);
                      If (LabDif <> 0) Then GetFinalDestination(pai_labeled(p));
                    end
                  else
                    Begin
                      hp2:=pai(p^.next);
                      if FindLabel(pai_labeled(p)^.lab, hp2) then
                        begin
                          hp1:=pai(hp2^.next);
                          asml^.remove(p);
                          dispose(p,done);
                          If Not(pai_label(hp2)^.l^.is_used) Then
                            Begin
                              AsmL^.remove(hp2);
                              Dispose(hp2, done);
                            End;
                          p:=hp1;
                          continue;
                        end;
                      If (LabDif <> 0) Then GetFinalDestination(pai_labeled(p));
                    end;
               end
           end
          else
            if p^.typ=ait_instruction
              Then
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
                            Assigned(p^.next) And
                            (Pai(p^.next)^.typ = ait_instruction) And
                            (Pai386(p^.next)^._operator = A_AND) And
                            (Pai386(p^.next)^.op1t = top_const) And
                            (Pai386(p^.next)^.op2t = top_reg) And
                            (Pai386(p)^.op2 = Pai386(p^.next)^.op2)
                           Then
{change "and const1, reg; and const2, reg" to "and (const1 and const2), reg"}
                             Begin
                               Pai386(p)^.op1 := Pointer(Longint(Pai386(p)^.op1) And Longint(Pai386(p^.next)^.op1));
                               hp1 := Pai(p^.next);
                               AsmL^.Remove(hp1);
                               Dispose(hp1, Done)
                             End;
                         {
                         Else
                           If (Pai386(p)^.op2t = top_reg) And
                              Assigned(p^.next) And
                              (Pai(p^.next)^.typ = ait_labeled_instruction)
                             Then Pai386(p)^._operator := A_TEST;
                         change "and x, reg; jxx" to "test x, reg
                         }
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
                     A_FSTP:
                       Begin
                         If (Pai386(p)^.op1t = top_ref) And
                            Assigned(p^.next) And
                            (Pai(p^.next)^.typ = ait_instruction) And
                            (Pai386(p^.next)^._operator = A_FLD) And
                            (Pai386(p^.next)^.op1t = top_ref) And
                            (Pai386(p)^.Size = Pai386(p)^.Size) And
                            RefsEqual(TReference(Pai386(p)^.op1^), TReference(Pai386(p^.next)^.op1^))
                           Then
                             Begin
                               hp1 := pai(p^.next^.next);
                               If Assigned(hp1) And
                                  (hp1^.typ = ait_instruction) And
                                  ((Pai386(hp1)^._operator = A_LEAVE) Or
                                   (Pai386(hp1)^._operator = A_RET)) And
                                  (TReference(Pai386(p)^.op1^).Base = ProcInfo.FramePointer) And
                                  (TReference(Pai386(p)^.op1^).Offset >= ProcInfo.RetOffset) And
                                  (TReference(Pai386(p)^.op1^).Index = R_NO)
                                 Then
                                   Begin
                                     hp2 := Pai(p^.next);
                                     AsmL^.Remove(p);
                                     AsmL^.Remove(hp2);
                                     Dispose(p, Done);
                                     Dispose(hp2, Done);
                                     p := hp1;
                                     Continue
                                   End
                                 Else
                                   Begin
                                     Pai386(p)^._operator := A_FST;
                                     hp1 := Pai(p^.next);
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
                            (Pai386(p)^.Size = S_L) And
                            ((Pai386(p)^.op3t = Top_Reg) or
                             (Pai386(p)^.op3t = Top_None)) And
                            (Opt_Processors < PentiumPro) And
                            (Longint(Pai386(p)^.op1) <= 12) And
                            Not(CS_LittleSize in AktSwitches) And
                            ((Assigned(p^.next) And
                              Not((Pai(p^.next)^.typ = ait_labeled_instruction) And
                                  ((pai_labeled(p^.next)^._operator = A_JO) or
                                   (pai_labeled(p^.next)^._operator = A_JNO)))) or
                            Not(Assigned(p^.next))) Then
                           Begin
                             New(TmpRef);
                             TmpRef^.segment := R_DEFAULT_SEG;
                             TmpRef^.symbol := nil;
                             TmpRef^.isintvalue := false;
                             TmpRef^.offset := 0;
                             Case Longint(Pai386(p)^.op1) Of
                               3: Begin
                                    TmpRef^.base := TRegister(Pai386(p)^.op2);
                                    TmpRef^.Index := TRegister(Pai386(p)^.op2);
                                    TmpRef^.ScaleFactor := 2;
                                    If (Pai386(p)^.op3t = Top_None)
                                      Then hp1 := New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, TRegister(Pai386(p)^.op2)))
                                      Else hp1 := New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                       TRegister(twowords(Pai386(p)^.op2).word2)));
                                    hp1^.line := p^.line;
                                    InsertLLItem(p^.last, p^.next, hp1);
                                    Dispose(p, Done);
                                    p := hp1;
                                 End;
                               5: Begin
                                    TmpRef^.base := TRegister(Pai386(p)^.op2);
                                    TmpRef^.Index := TRegister(Pai386(p)^.op2);
                                    TmpRef^.ScaleFactor := 4;
                                    If (Pai386(p)^.op3t = Top_None)
                                      Then hp1 := New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, TRegister(Pai386(p)^.op2)))
                                      Else hp1 := New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                       TRegister(twowords(Pai386(p)^.op2).word2)));
                                    hp1^.line:= p^.line;
                                    InsertLLItem(p^.last, p^.next, hp1);
                                    Dispose(p, Done);
                                    p := hp1;
                                  End;
                               6: Begin
                                    If (Opt_Processors <= i486) Then
                                      Begin
                                        TmpRef^.Index := TRegister(Pai386(p)^.op2);
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
                                              TmpRef^.base := R_NO;
                                              TmpRef^.ScaleFactor := 2;
                                              hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                               TRegister(Pai386(p)^.op2)));
                                            End;
                                        hp1^.line := p^.line;
                                        InsertLLItem(p, p^.next, hp1);
                                        New(TmpRef);
                                        TmpRef^.segment := R_DEFAULT_SEG;
                                        TmpRef^.symbol := nil;
                                        TmpRef^.isintvalue := false;
                                        TmpRef^.offset := 0;
                                        TmpRef^.Index := TRegister(Pai386(p)^.op2);
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
                                        hp1^.line := p^.line;
                                        InsertLLItem(p^.last, p^.next, hp1);
                                        Dispose(p, Done);
                                        p := Pai(hp1^.next);
                                      End
                                     Else Dispose(TmpRef);
                                  End;
                               9: Begin
                                    TmpRef^.base := TRegister(Pai386(p)^.op2);
                                    TmpRef^.Index := TRegister(Pai386(p)^.op2);
                                    TmpRef^.ScaleFactor := 8;
                                    If (Pai386(p)^.op3t = Top_None)
                                      Then hp1 := New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef, TRegister(Pai386(p)^.op2)))
                                      Else hp1 := New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                       TRegister(twowords(Pai386(p)^.op2).word2)));
                                    hp1^.line := p^.line;
                                    InsertLLItem(p^.last, p^.next, hp1);
                                    Dispose(p, Done);
                                    p := hp1;
                                  End;
                               10: Begin
                                     If (Opt_Processors <= i486) Then
                                       Begin
                                         TmpRef^.Index := TRegister(Pai386(p)^.op2);
                                         If (Pai386(p)^.op3t = Top_Reg)
                                           Then
                                             Begin
                                               TmpRef^.base := TRegister(twowords(Pai386(p)^.op2).word2);
                                               TmpRef^.ScaleFactor := 8;
                                               hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                                Tregister(twowords(Pai386(p)^.op2).word2)));
                                             End
                                           Else
                                             Begin
                                               TmpRef^.base := R_NO;
                                               TmpRef^.ScaleFactor := 2;
                                               hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                                TRegister(Pai386(p)^.op2)));
                                            End;
                                         hp1^.line := p^.line;
                                         InsertLLItem(p, p^.next, hp1);
                                         New(TmpRef);
                                         TmpRef^.segment := R_DEFAULT_SEG;
                                         TmpRef^.symbol := nil;
                                         TmpRef^.isintvalue := false;
                                         TmpRef^.offset := 0;
                                         TmpRef^.Index := TRegister(Pai386(p)^.op2);
                                         If (Pai386(p)^.op3t = Top_Reg)
                                           Then
                                             Begin
                                               TmpRef^.ScaleFactor := 2;
                                               TmpRef^.base := R_NO;
                                               hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                                TRegister(twowords(Pai386(p)^.op2).word2)));
                                             End
                                           Else
                                             Begin
                                               TmpRef^.ScaleFactor := 4;
                                               TmpRef^.base := TRegister(Pai386(p)^.op2);
                                               hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                                TRegister(Pai386(p)^.op2)));
                                             End;
                                         hp1^.line := p^.line;
                                         InsertLLItem(p^.last, p^.next, hp1);
                                         Dispose(p, Done);
                                         p := Pai(hp1^.next);
                                       End
                                     Else Dispose(TmpRef);
                                   End;
                               12: Begin
                                     If (Opt_Processors <= i486) Then
                                       Begin
                                         TmpRef^.Index := TRegister(Pai386(p)^.op2);
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
                                         hp1^.line := p^.line;
                                         InsertLLItem(p, p^.next, hp1);
                                         New(TmpRef);
                                         TmpRef^.segment := R_DEFAULT_SEG;
                                         TmpRef^.symbol := nil;
                                         TmpRef^.isintvalue := false;
                                         TmpRef^.offset := 0;
                                         TmpRef^.Index := TRegister(Pai386(p)^.op2);
                                         TmpRef^.ScaleFactor := 4;
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
                                               hp1 :=  New(Pai386, op_ref_reg(A_LEA, S_L, TmpRef,
                                                TRegister(Pai386(p)^.op2)));
                                             End;
                                         hp1^.line := p^.line;
                                         InsertLLItem(p^.last, p^.next, hp1);
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
                         If (PReference(Pai386(p)^.op1)^.Base >= R_EAX) And
                            (PReference(Pai386(p)^.op1)^.Base <= R_EDI) And
                            (PReference(Pai386(p)^.op1)^.Index = R_NO) And
                            (PReference(Pai386(p)^.op1)^.Offset = 0) And
                            (Not(Assigned(PReference(Pai386(p)^.op1)^.Symbol))) Then
                           Begin
                             hp1 := New(Pai386, op_reg_reg(A_MOV, S_L,PReference(Pai386(p)^.op1)^.Base,
                              TRegister(Pai386(p)^.op2)));
                             hp1^.line := p^.line;
                             InsertLLItem(p^.last,p^.next, hp1);
                             Dispose(p, Done);
                             p := hp1;
                             Continue;
                          End;
                       End;
                     A_MOV:
                       Begin
                         If (Pai386(p)^.op2t = top_reg) And
                            (TRegister(Pai386(p)^.op2) In [R_EAX, R_EBX, R_EDX, R_EDI]) And
                            Assigned(p^.next) And
                            (Pai(p^.next)^.typ = ait_instruction) And
                            (Pai386(p^.next)^._operator = A_MOV) And
                            (Pai386(p^.next)^.op1t = top_reg) And
                            (Pai386(p^.next)^.op1 = Pai386(p)^.op2)
                           Then
                       {we have "mov x, %treg; mov %treg, y}
                             If (Pai386(p^.next)^.op2t <> top_reg) Or
                              RegInInstruction(TRegister(Pai386(p^.next)^.op2), Pai(p^.next^.next))
                               Then
                      {we've got "mov x, %treg; mov %treg, y; XXX y" (ie. y is used in
                       the third instruction)}
                                 Case Pai386(p)^.op1t Of
                                   top_reg:
                                 {change "mov %reg, %treg; mov %treg, y"
                                  to "mov %reg, y"}
                                     Begin
                                       Pai386(p^.next)^.op1 := Pai386(p)^.op1;
                                       hp1 := Pai(p^.next);
                                       AsmL^.Remove(p);
                                       Dispose(p, Done);
                                       p := hp1;
                                       continue;
                                     End;
                                   top_ref:
                                     If (Pai386(p^.next)^.op2t = top_reg)
                                       Then
                                    {change "mov mem, %treg; mov %treg, %reg"
                                          to "mov mem, %reg"}
                                         Begin
                                           Pai386(p)^.op2 := Pai386(p^.next)^.op2;
                                           hp1 := Pai(p^.next);
                                           AsmL^.Remove(hp1);
                                           Dispose(hp1, Done);
                                           continue;
                                         End;
                                 End
                               Else
                      {remove an instruction which never makes sense: we've got
                       "mov mem, %reg1; mov %reg1, %edi" and then EDI isn't used anymore!}
                                 Begin
                                   If (TRegister(Pai386(p^.next)^.op2) = R_EDI) And
                                      Not(Assigned(p^.next^.next) And
                                          (Pai(p^.next^.next)^.typ = ait_instruction) And
                                          (Pai386(p^.next^.next)^.op2t = top_reg) And
                                          (Pai386(p^.next^.next)^.op2 = Pointer(R_ESI))) Then
                                     Begin
                                       hp1 := pai(p^.next);
                                       AsmL^.Remove(hp1);
                                       Dispose(hp1, Done);
                                       Continue;
                                     End
                                 End
                           Else
                         {Change "mov %reg1, %reg2; xxx %reg2, ???" to
                              "mov %reg1, %reg2; xxx %reg1, ???" to
                              avoid a write/read penalty}
                             If (Pai386(p)^.op1t = top_reg) And
                                (Pai386(p)^.op2t = top_reg) And
                                Assigned(p^.next) And
                                (Pai(p^.next)^.typ = ait_instruction) And
                                (Pai386(p^.next)^.op1t = top_reg) And
                                (Pai386(p^.next)^.op1 = Pai386(p)^.op2)
                               Then
                         {we have "mov %reg1, %reg2; XXX %reg2, ???"}
                                 Begin
                                   If ((Pai386(p^.next)^._operator = A_OR) Or
                                       (Pai386(p^.next)^._operator = A_TEST)) And
                                      (Pai386(p^.next)^.op2t = top_reg) And
                                      (Pai386(p^.next)^.op1 = Pai386(p^.next)^.op2)
                                     Then
                          {we have "mov %reg1, %reg2; test/or %reg2, %reg2"}
                                       Begin
                                         If Assigned(p^.next^.next) And
                                            (Pai(p^.next^.next)^.typ = ait_labeled_instruction) And
                                            (TRegister(Pai386(p)^.op2) <> R_ESI)
                                           Then
                            {change "mov %reg1, %reg2; test/or %reg2, %reg2; jxx" to
                                 "test %reg1, %reg1; jxx"}
                                             Begin
                                               hp1 := pai(p^.next);
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
                                               Pai386(p^.next)^.op1 := Pai386(p)^.op1;
                                               Pai386(p^.next)^.op2 := Pai386(p)^.op1;
                                             End;
                                       End
                                     Else
{                                      If (Pai386(p^.next)^._operator
                                          In [A_PUSH, A_OR, A_XOR, A_AND, A_TEST])}
                             {change "mov %reg1, %reg2; push/or/xor/... %reg2, ???" to
                              "mov %reg1, %reg2; push/or/xor/... %reg1, ???"}
                                 End
                               Else
                         {leave out the mov from "mov reg, x(%frame_pointer); leave/ret" (with
                          x >= RetOffset) as it doesn't do anything (it writes either to a
                          parameter or to the temporary storage room for the function
                      result)}
                                 If Assigned(p^.next) And
                                    (Pai(p^.next)^.typ = ait_instruction)
                                   Then
                                     If ((Pai386(p^.next)^._operator = A_LEAVE) Or
                                         (Pai386(p^.next)^._operator = A_RET)) And
                                        (Pai386(p)^.op2t = top_ref) And
                                        (TReference(Pai386(p)^.op2^).base = ProcInfo.FramePointer) And
                                        (TReference(Pai386(p)^.op2^).offset >= ProcInfo.RetOffset) And
                                        (TReference(Pai386(p)^.op2^).index = R_NO) And
                                        (Pai386(p)^.op1t = top_reg)
                                       Then
                                         Begin
                                          hp1 := Pai(p^.next);
                                          AsmL^.Remove(p);
                                          Dispose(p, done);
                                          p := hp1;
                                        End
                                      Else
                                        If (Pai386(p)^.op1t = top_reg) And
                                           (Pai386(p)^.op2t = top_ref) And
                                           (Pai386(p)^.Size = Pai386(p^.next)^.Size) And
                                           (Pai386(p^.next)^._operator = A_CMP) And
                                           (Pai386(p^.next)^.op2t = top_ref) And
                                           RefsEqual(TReference(Pai386(p)^.op2^),
                                                     TReference(Pai386(p^.next)^.op2^))
                                          Then
                   {change "mov reg, mem1; cmp x, mem1" to "mov reg, mem1; cmp x, reg1"}
                                            Begin
                                              Dispose(PReference(Pai386(p^.next)^.op2));
                                              Pai386(p^.next)^.opxt := Pai386(p^.next)^.op1t + (top_reg shl 4);
                                              Pai386(p^.next)^.op2 := Pai386(p)^.op1
                                            End;
                       { Next instruction is also a MOV ? }
                         If assigned(p^.next) and
                            (pai(p^.next)^.typ = ait_instruction) and
                            (Pai386(p^.next)^._operator = A_MOV)
                           Then
                             Begin
                               { Removes the second statement from
                                   mov %reg, mem
                                   mov mem, %reg }
                               If (Pai386(p^.next)^.op1t = Pai386(p)^.op2t) and
                                  (Pai386(p^.next)^.op2t = Pai386(p)^.op1t) Then
                                Begin
                                  If (Pai386(p^.next)^.op2t = top_ref) Then
                                   TmpBool1 := RefsEqual(TReference(Pai386(p^.next)^.op2^), TReference(Pai386(p)^.op1^))
                                  Else
                                   TmpBool1 := Pai386(p^.next)^.op2 = Pai386(p)^.op1;
                                  If TmpBool1
                                    Then
                                      Begin
                                        If (Pai386(p^.next)^.op1t = top_ref)
                                          Then
                                            TmpBool1 := RefsEqual(TReference(Pai386(p^.next)^.op1^),
                                                                  TReference(Pai386(p)^.op2^))
                                          Else TmpBool1 := (Pai386(p^.next)^.op1 = Pai386(p)^.op2);
                                        If TmpBool1 Then
                                          Begin
                                            hp1 := pai(p^.next);
                                            AsmL^.remove(hp1);
                                            Dispose(hp1,done);
                                          End;
                                      End
                                    Else
                                                        Begin
                                                          hp1 := pai(p^.next^.next);
                                                          If (Pai386(p)^.op1t = top_ref) And
                                                             (Pai386(p)^.op2t = top_reg) And
                                                             (Pai386(p^.next)^.op1t = top_reg) And
                                                             (Pai386(p^.next)^.op1 = Pai386(p)^.op2) And
                                                             (Pai386(p^.next)^.op2t = top_ref) And
                                                             Assigned(hp1) And
                                                             (Pai(hp1)^.typ = ait_instruction) And
                                                             (Pai386(hp1)^._operator = A_MOV) And
                                                             (Pai386(hp1)^.op2t = top_reg) And
                                                             (Pai386(hp1)^.op1t = top_ref) And
                                                             RefsEqual(TReference(Pai386(hp1)^.op1^),
                                                                       TReference(Pai386(p^.next)^.op2^))
                                                            Then
                                                       {   mov mem1, reg1
                                                           mov reg1, mem2
                                                           mov mem2, reg2
                                                        to:
                                                           mov mem1, reg2
                                                           mov reg2, mem2}
                                                              If (TRegister(Pai386(p)^.op2) <> R_ESI)
                                                                Then
                                                                  Begin
                                                                    Pai386(p)^.op2 := Pai386(hp1)^.op2;
                                                                    Pai386(p^.next)^.op1 := Pai386(hp1)^.op2;
                                                                    AsmL^.Remove(hp1);
                                                                    Dispose(hp1,Done);
                                                                  End
                                                                Else
                                                       {   mov mem1, esi
                                                           mov esi, mem2
                                                           mov mem2, reg2
                                                        to:
                                                           mov mem1, esi
                                                           mov mem1, reg2
                                                           mov esi, mem2}
                                                                  Begin
                                                                    Pai386(p^.next)^.opxt := top_ref + top_reg shl 4;
                                                                    Pai386(p^.next)^.op1 := Pai386(p)^.op2;
                                                                    TReference(Pai386(p^.next)^.op1^) := TReference(Pai386(p)^.op1^);
                                                                    Pai386(p^.next)^.op2 := Pai386(hp1)^.op2;
                                                                    Pai386(hp1)^.opxt := top_reg + top_ref shl 4;
                                                                    Pai386(hp1)^.op2 := Pai386(hp1)^.op1;
                                                                    Pai386(hp1)^.op1 := Pointer(R_ESI)
                                                                  End;
                                                        End;
                                End
                               Else
(*                               {   movl [mem1],reg1
                                   movl [mem1],reg2
                                to:
                                   movl [mem1],reg1
                                   movl reg1,reg2 }
                                If (Pai386(p)^.op1t = top_ref) and
                                   (Pai386(p)^.op2t = top_reg) and
                                   (Pai386(p^.next)^.op1t = top_ref) and
                                   (Pai386(p^.next)^.op2t = top_reg) and
                                   (Pai386(p)^.size = Pai386(p^.next)^.size) and
                                   RefsEqual(TReference(Pai386(p)^.op1^),TReference(Pai386(p^.next)^.op1^)) and
                                   (TRegister(Pai386(p)^.op2)<>TReference(Pai386(p^.next)^.op1^).base) and
                                   (TRegister(Pai386(p)^.op2)<>TReference(Pai386(p^.next)^.op1^).index) then
                                  Begin
                                    Dispose(PReference(Pai386(p^.next)^.op1));
                                    Pai386(p^.next)^.op1:=Pai386(p)^.op2;
                                    Pai386(p^.next)^.opxt:=Top_reg+Top_reg shl 4;
                                  End
                               Else*)
                               {   movl const1,[mem1]
                                   movl [mem1],reg1
                                to:
                                   movl const1,reg1
                                   movl reg1,[mem1] }
                                If (Pai386(p)^.op1t = top_const) and
                                   (Pai386(p)^.op2t = top_ref) and
                                   (Pai386(p^.next)^.op1t = top_ref) and
                                   (Pai386(p^.next)^.op2t = top_reg) and
                                   (Pai386(p)^.size = Pai386(p^.next)^.size) and
                                   RefsEqual(TReference(Pai386(p^.next)^.op1^),TReference(Pai386(p)^.op2^)) then
                                  Begin
                                    Pai386(p^.next)^.op1:=Pai386(p^.next)^.op2;
                                    Pai386(p^.next)^.op2:=Pai386(p)^.op2;
                                    Pai386(p^.next)^.opxt:=Top_reg+Top_ref shl 4;
                                    Pai386(p)^.op2:=Pai386(p^.next)^.op1;
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
                            Assigned(p^.next) And
                            (Pai(p^.next)^.typ = ait_instruction) And
                            (Pai386(p^.next)^._operator = A_AND) And
                            (Pai386(p^.next)^.op1t = top_const) And
                            (Pai386(p^.next)^.op2t = top_reg) And
                            (Pai386(p^.next)^.op2 = Pai386(p)^.op2)
                           Then
                             Case Pai386(p)^.Size Of
                               S_BL, S_BW:
                                 If (Longint(Pai386(p^.next)^.op1) = $ff)
                                   Then
                                     Begin
                                       hp1 := Pai(p^.next);
                                       AsmL^.Remove(hp1);
                                       Dispose(hp1, Done);
                                     End;
                               S_WL:
                                 If (Longint(Pai386(p^.next)^.op1) = $ffff)
                                   Then
                                     Begin
                                       hp1 := Pai(p^.next);
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
                                         Not(CS_LittleSize In AktSwitches)
                                        Then
                                          {Change "movzbw %al, %ax" to "andw $0x0ffh, %ax"}
                                          Begin
                                            Pai386(p)^._operator := A_AND;
                                            Pai386(p)^.opxt := top_const+Top_reg shl 4;
                                            Longint(Pai386(p)^.op1) := $ff;
                                            Pai386(p)^.Size := S_W
                                          End
                                        Else
                                          If Assigned(p^.next) And
                                            (Pai(p^.next)^.typ = ait_instruction) And
                                            (Pai386(p^.next)^._operator = A_AND) And
                                            (Pai386(p^.next)^.op1t = top_const) And
                                            (Pai386(p^.next)^.op2t = top_reg) And
                                            (Pai386(p^.next)^.op2 = Pai386(p)^.op2)
                                              Then
                                                {Change "movzbw %reg1, %reg2; andw $const, %reg2"
                                                 to "movw %reg1, reg2; andw $(const1 and $ff), %reg2"}
                                                Begin
                                                  Pai386(p)^._operator := A_MOV;
                                                  Pai386(p)^.Size := S_W;
                                                  Pai386(p)^.op1 := Pointer(Reg8ToReg16(TRegister(Pai386(p)^.op1)));
                                                  Pai386(p^.next)^.op1 := Pointer(Longint(Pai386(p^.next)^.op1)
                                                                          And $ff);
                                                End;
                                    End;
                                  S_BL:
                                    Begin
                                      If (TRegister(Pai386(p)^.op1) = Reg32ToReg8(TRegister(Pai386(p)^.op2))) And
                                         Not(CS_LittleSize in AktSwitches)
                                        Then
                                          {Change "movzbl %al, %eax" to "andl $0x0ffh, %eax"}
                                          Begin
                                            Pai386(p)^._operator := A_AND;
                                            Pai386(p)^.opxt := top_const+Top_reg shl 4;
                                            Longint(Pai386(p)^.op1) := $ff;
                                            Pai386(p)^.Size := S_L;
                                          End
                                        Else
                                          If Assigned(p^.next) And
                                            (Pai(p^.next)^.typ = ait_instruction) And
                                            (Pai386(p^.next)^._operator = A_AND) And
                                            (Pai386(p^.next)^.op1t = top_const) And
                                            (Pai386(p^.next)^.op2t = top_reg) And
                                            (Pai386(p^.next)^.op2 = Pai386(p)^.op2)
                                              Then
                                                {Change "movzbl %reg1, %reg2; andl $const, %reg2"
                                                 to "movl %reg1, reg2; andl $(const1 and $ff), %reg2"}
                                                Begin
                                                  Pai386(p)^._operator := A_MOV;
                                                  Pai386(p)^.Size := S_L;
                                                  Pai386(p)^.op1 := Pointer(Reg8ToReg32(TRegister(Pai386(p)^.op1)));
                                                  Pai386(p^.next)^.op1 := Pointer(Longint(Pai386(p^.next)^.op1)
                                                                          And $ff);
                                                End
                                              Else
                                                If IsGP32Reg(TRegister(Pai386(p)^.op2)) And
                                                   Not(CS_LittleSize in AktSwitches) And
                                                   (Opt_Processors >= Pentium) And
                                                   (Opt_Processors < PentiumPro)
                                                   Then
                                                     {Change "movzbl %reg1, %reg2" to
                                                      "xorl %reg2, %reg2; movb %reg1, %reg2" for Pentium and
                                                       PentiumMMX}
                                                     Begin
                                                       hp1 := New(Pai386, op_reg_reg(A_XOR, S_L,
                                                                  TRegister(Pai386(p)^.op2),
                                                                  TRegister(Pai386(p)^.op2)));
                                                       hp1^.line := p^.line;
                                                       InsertLLItem(p^.last, p, hp1);
                                                       Pai386(p)^._operator := A_MOV;
                                                       Pai386(p)^.size := S_B;
                                                       Pai386(p)^.op2 :=
                                                           Pointer(Reg32ToReg8(TRegister(Pai386(p)^.op2)));
                                                       InsertLLItem(p, p^.next, hp2);
                                                     End;
                                    End;
                                  S_WL:
                                    Begin
                                      If (TRegister(Pai386(p)^.op1) = Reg32ToReg16(TRegister(Pai386(p)^.op2))) And
                                         Not(CS_LittleSize In AktSwitches)
                                        Then
                                          {Change "movzwl %ax, %eax" to "andl $0x0ffffh, %eax"}
                                          Begin
                                            Pai386(p)^._operator := A_AND;
                                            Pai386(p)^.opxt := top_const+Top_reg shl 4;
                                            Longint(Pai386(p)^.op1) := $ffff;
                                            Pai386(p)^.Size := S_L
                                          End
                                        Else
                                          If Assigned(p^.next) And
                                            (Pai(p^.next)^.typ = ait_instruction) And
                                            (Pai386(p^.next)^._operator = A_AND) And
                                            (Pai386(p^.next)^.op1t = top_const) And
                                            (Pai386(p^.next)^.op2t = top_reg) And
                                            (Pai386(p^.next)^.op2 = Pai386(p)^.op2)
                                              Then
                                                {Change "movzwl %reg1, %reg2; andl $const, %reg2"
                                                 to "movl %reg1, reg2; andl $(const1 and $ffff), %reg2"}
                                                Begin
                                                  Pai386(p)^._operator := A_MOV;
                                                  Pai386(p)^.Size := S_L;
                                                  Pai386(p)^.op1 := Pointer(Reg16ToReg32(TRegister(Pai386(p)^.op1)));
                                                  Pai386(p^.next)^.op1 := Pointer(Longint(Pai386(p^.next)^.op1)
                                                                          And $ffff);
                                                End;
                                    End;
                                End
                             Else
                               If (Pai386(p)^.op1t = top_ref) Then
                                  Begin
                                    If (PReference(Pai386(p)^.op1)^.base <> TRegister(Pai386(p)^.op2)) And
                                       (PReference(Pai386(p)^.op1)^.index <> TRegister(Pai386(p)^.op2)) And
                                       Not(CS_LittleSize in AktSwitches) And
                                       IsGP32Reg(TRegister(Pai386(p)^.op2)) And
                                       (Opt_Processors >= Pentium) And
                                       (Opt_Processors < PentiumPro) And
                                       (Pai386(p)^.Size = S_BL)
                                         Then
                                           {changes "movzbl mem, %reg" to "xorl %reg, %reg; movb mem, %reg8" for
                                            Pentium and PentiumMMX}
                                           Begin
                                             hp1 := New(Pai386,op_reg_reg(A_XOR, S_L, TRegister(Pai386(p)^.op2),
                                             TRegister(Pai386(p)^.op2)));
                                             hp1^.line := p^.line;
                                             Pai386(p)^._operator := A_MOV;
                                             Pai386(p)^.size := S_B;
                                             Pai386(p)^.op2 := Pointer(Reg32ToReg8(TRegister(Pai386(p)^.op2)));
                                             InsertLLItem(p^.last, p, hp1);
                                           End
                                         Else
                                           If Assigned(p^.next) And
                                              (Pai(p^.next)^.typ = ait_instruction) And
                                              (Pai386(p^.next)^._operator = A_AND) And
                                              (Pai386(p^.next)^.op1t = Top_Const) And
                                              (Pai386(p^.next)^.op2t = Top_Reg) And
                                              (Pai386(p^.next)^.op2 = Pai386(p)^.op2) Then
                                                Begin
                                                  Pai386(p)^._operator := A_MOV;
                                                  Case Pai386(p)^.Size Of
                                                    S_BL:
                                                      Begin
                                                        Pai386(p)^.Size := S_L;
                                                        Pai386(p^.next)^.op1 := Pointer(Longint(Pai386(p^.next)^.op1)
                                                                                And $ff);
                                                      End;
                                                    S_WL:
                                                      Begin
                                                        Pai386(p)^.Size := S_L;
                                                        Pai386(p^.next)^.op1 := Pointer(Longint(Pai386(p^.next)^.op1)
                                                                                        And $ffff);
                                                      End;
                                                    S_BW:
                                                      Begin
                                                        Pai386(p)^.Size := S_W;
                                                        Pai386(p^.next)^.op1 := Pointer(Longint(Pai386(p^.next)^.op1)
                                                                                And $ff);
                                                      End;
                                                  End;
                                                End;
                                        End;
                       End;
                     A_POP:
                       Begin
                         if (Pai386(p)^.op1t = top_reg) And
                            (assigned(p^.next)) and
                            (pai(p^.next)^.typ=ait_instruction) and
                            (Pai386(p^.next)^._operator=A_PUSH) and
                            (Pai386(p^.next)^.op1t = top_reg) And
                            (Pai386(p^.next)^.op1=Pai386(p)^.op1) then
                              begin
                                hp2:=pai(p^.next^.next);
                                hp1:=pai(p^.next);
                                asml^.remove(p);
                                asml^.remove(hp1);
                                dispose(p,done);
                                dispose(hp1,done);
                                p:=hp2;
                                continue;
{                                Pai386(p)^._operator := A_MOV;
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
                                Dispose(hp1, Done)}
                              end;
                       end;
                     A_PUSH:
                       Begin
                         If (Pai386(p)^.size = S_W) And
                            (Pai386(p)^.op1t = Top_Const) And
                            Assigned(p^.next) And
                            (Pai(p^.next)^.typ = ait_instruction) And
                            (Pai386(p^.next)^._operator = A_PUSH) And
                            (Pai386(p^.next)^.op1t = Top_Const) And
                            (Pai386(p^.next)^.size = S_W) Then
                              Begin
                                hp1 := Pai(p^.next);
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
                               TmpRef^.scalefactor := PowerOf2(Longint(Pai386(p)^.op1));
                               TmpRef^.symbol := nil;
                               TmpRef^.isintvalue := false;
                               TmpRef^.offset := 0;
                               While  TmpBool1 And
                                      Assigned(p^.next) And
                                      (Pai(p^.next)^.typ = ait_instruction) And
                                      ((Pai386(p^.next)^._operator = A_ADD) Or
                                       (Pai386(p^.next)^._operator = A_SUB)) And
                                      (Pai386(p^.next)^.op2t = Top_Reg) And
                                      (Pai386(p^.next)^.op2 = Pai386(p)^.op2) Do
                                  Begin
                                    TmpBool1 := False;
                                    If (Pai386(p^.next)^.op1t = Top_Const)
                                      Then
                                        Begin
                                          TmpBool1 := True;
                                          TmpBool2 := True;
                                          If Pai386(p^.next)^._operator = A_ADD
                                            Then Inc(TmpRef^.offset, Longint(Pai386(p^.next)^.op1))
                                            Else Dec(TmpRef^.offset, Longint(Pai386(p^.next)^.op1));
                                          hp1 := Pai(p^.next);
                                          AsmL^.Remove(hp1);
                                          Dispose(hp1, Done);
                                        End
                                      Else
                                        If (Pai386(p^.next)^.op1t = Top_Reg) And
                                           (Pai386(p^.next)^._operator = A_ADD) And
                                           (TmpRef^.base = R_NO) Then
                                          Begin
                                            TmpBool1 := True;
                                            TmpBool2 := True;
                                            TmpRef^.base := TRegister(Pai386(p^.next)^.op1);
                                            hp1 := Pai(p^.next);
                                            AsmL^.Remove(hp1);
                                            Dispose(hp1, Done);
                                          End;
                                  End;
                                If TmpBool2 Or
                                   ((Opt_Processors < PentiumPro) And
                                    (Longint(Pai386(p)^.op1) <= 3) And
                                    Not(CS_LittleSize in AktSwitches))
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
                                       hp1^.line := p^.line;
                                       InsertLLItem(p^.last, p^.next, hp1);
                                       Dispose(p, Done);
                                       p := hp1;
                                     End;
                             End
                           Else
                             If (Opt_Processors < PentiumPro) And
                                (Pai386(p)^.op1t = top_const) And
                                (Pai386(p)^.op2t = top_reg) Then
                               If (Longint(Pai386(p)^.op1) = 1)
                                 Then
                           {changes "shl $1, %reg" to "add %reg, %reg", which
                            is the same on a 386, but faster on a 486, and pairable in both U and V
                            pipes on the Pentium (unlike shl, which is only pairable in the U pipe)}
                                   Begin
                                     hp1 := new(Pai386,op_reg_reg(A_ADD,Pai386(p)^.Size,
                                                TRegister(Pai386(p)^.op2), TRegister(Pai386(p)^.op2)));
                                     hp1^.line := p^.line;
                                     InsertLLItem(p^.last, p^.next, hp1);
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
                                            TmpRef^.scalefactor := PowerOf2(Longint(Pai386(p)^.op1));
                                            TmpRef^.symbol := nil;
                                            TmpRef^.isintvalue := false;
                                            TmpRef^.offset := 0;
                                            hp1 := new(Pai386,op_ref_reg(A_LEA,S_L,TmpRef, TRegister(Pai386(p)^.op2)));
                                            hp1^.line := p^.line;
                                            InsertLLItem(p^.last, p^.next, hp1);
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
                         hp1 := pai(p^.next);
                         If Assigned(hp1) and
                            (pai(hp1)^.typ = ait_instruction) and
                            (Pai386(hp1)^._operator = A_SHL) and
                            (Pai386(p)^.op1t = top_const) and
                            (Pai386(hp1)^.op1t = top_const) Then
                              If (Longint(Pai386(p)^.op1) > Longint(Pai386(hp1)^.op1)) Then
                                If (Pai386(p)^.op2t = Top_reg) And
                                   Not(CS_LittleSize In AktSwitches) And
                                   ((Pai386(p)^.Size = S_B) Or
                                    (Pai386(p)^.Size = S_L))
                                    Then
                                      Begin
                                        Dec(Longint(Pai386(p)^.op1), Longint(Pai386(hp1)^.op1));
                                        Pai386(hp1)^._operator := A_And;
                                        Pai386(hp1)^.op1 := Pointer(PowerOf2(Longint(Pai386(hp1)^.op1))-1);
                                        If (Pai386(p)^.Size = S_L)
                                          Then Pai386(hp1)^.op1 := Pointer(Longint(Pai386(hp1)^.op1) Xor $ffffffff)
                                          Else Pai386(hp1)^.op1 := Pointer(Longint(Pai386(hp1)^.op1) Xor $ff);
                                      End
                                    Else
                                      If (Longint(Pai386(p)^.op1) < Longint(Pai386(hp1)^.op1)) Then
                                        If (Pai386(p)^.op2t = Top_reg) And
                                          Not(CS_LittleSize In AktSwitches) And
                                          ((Pai386(p)^.Size = S_B) Or
                                           (Pai386(p)^.Size = S_L))
                                           Then
                                             Begin
                                               Dec(Longint(Pai386(hp1)^.op1), Longint(Pai386(p)^.op1));
                                               Pai386(p)^._operator := A_And;
                                               Pai386(p)^.op1 := Pointer(PowerOf2(Longint(Pai386(p)^.op1))-1);
                                               If (Pai386(p)^.Size = S_L)
                                                 Then Pai386(hp1)^.op1 := Pointer(Longint(Pai386(hp1)^.op1) Xor $ffffffff)
                                                 Else Pai386(hp1)^.op1 := Pointer(Longint(Pai386(hp1)^.op1) Xor $ff);
                                             End
                                           Else
                                             Begin
                                               Pai386(p)^._operator := A_And;
                                               Pai386(p)^.op1 := Pointer(PowerOf2(Longint(Pai386(p)^.op1))-1);
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
                                     (Pai(hp1)^.typ = ait_instruction) And
                                     (Pai386(hp1)^._operator <> A_PUSH) Do
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
                                  If Assigned(p^.last) And
                                     (Pai(p^.last)^.typ = ait_instruction) And
                                     (Pai386(p^.last)^._operator = A_SUB) And
                                     (Pai386(p^.last)^.op1t = top_const) And
                                     (Pai386(p^.last)^.op2t = top_reg) And
                                     (TRegister(Pai386(p^.last)^.Op2) = R_ESP)
                                    Then
                                      Begin
                                        hp1 := Pai(p^.last);
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
                             (assigned(p^.last)) And
                             (pai(p^.last)^.typ = ait_instruction) Then
                             Case Pai386(p^.last)^._operator Of
                               A_ADD, A_SUB, A_OR, A_XOR, A_AND, A_SHL, A_SHR:
                               {There are probably more instructions which can be included}
                                 Begin
                                   If (Pai386(p^.last)^.op2 = Pai386(p)^.op1) Then
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
                                   If (Pai386(p^.last)^.op1 = Pai386(p)^.op1) Then
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
                End
              Else
                If (Pai(p)^.typ = ait_label)
                  Then
                    If Not(Pai_Label(p)^.l^.is_used)
                      Then
                        Begin
                          hp1 := Pai(p^.next);
                          AsmL^.Remove(p);
                          Dispose(p, Done);
                          p := hp1;
                          Continue
                        End;
         p:=pai(p^.next);
       end;
end;


  Procedure peepholeopt(AsmL : paasmoutput);

    Procedure FindLoHiLabels;
    {Walks through the paasmlist to find the lowest and highest label number;
     Since 0.9.3: also removes unused labels}
    Var LabelFound: Boolean;
        P, hp1: Pai;
    Begin
      LabelFound := False;
      LoLab := MaxLongint;
      HiLab := 0;
      p := Pai(AsmL^.first);
      While Assigned(p) Do
        Begin
          If (Pai(p)^.typ = ait_label) Then
            If (Pai_Label(p)^.l^.is_used)
              Then
                Begin
                  LabelFound := True;
                  If (Pai_Label(p)^.l^.nb < LoLab) Then
                  LoLab := Pai_Label(p)^.l^.nb;
                  If (Pai_Label(p)^.l^.nb > HiLab) Then
                  HiLab := Pai_Label(p)^.l^.nb;
                End
              Else
                Begin
                  hp1 := pai(p^.next);
                  AsmL^.Remove(p);
                  Dispose(p, Done);
                  p := hp1;
                  continue;
                End;
          p := pai(p^.next);
        End;
      If LabelFound
        Then LabDif := HiLab+1-LoLab
        Else LabDif := 0;
    End;

    Procedure BuildLabelTable;
    {Builds a table with the locations of the labels in the paasmoutput}
    Var p: Pai;
    Begin
      If (LabDif <> 0) Then
        Begin
          If (MaxAvail >= LabDif*SizeOf(Pai))
            Then
              Begin
                GetMem(LTable, LabDif*SizeOf(Pai));
                FillChar(LTable^, LabDif*SizeOf(Pai), 0);
                p := pai(AsmL^.first);
                While Assigned(p) Do
                  Begin
                    If (Pai(p)^.typ = ait_label) Then
                      LTable^[Pai_Label(p)^.l^.nb-LoLab] := p;
                    p := pai(p^.next);
                  End;
              End
            Else LabDif := 0;
        End;
    End;

  Begin
    FindLoHiLabels;
    BuildLabelTable;
    DoOptimize(AsmL);
    DoOptimize(AsmL);
    If LabDif <> 0 Then Freemem(LTable, LabDif*SizeOf(Pai));
    ReloadOpt(AsmL)
  End;

End.
{
  $Log$
  Revision 1.2  1998-03-28 23:09:53  florian
    * secondin bugfix (m68k and i386)
    * overflow checking bugfix (m68k and i386) -- pretty useless in
      secondadd, since everything is done using 32-bit
    * loading pointer to routines hopefully fixed (m68k)
    * flags problem with calls to RTL internal routines fixed (still strcmp
      to fix) (m68k)
    * #ELSE was still incorrect (didn't take care of the previous level)
    * problem with filenames in the command line solved
    * problem with mangledname solved
    * linking name problem solved (was case insensitive)
    * double id problem and potential crash solved
    * stop after first error
    * and=>test problem removed
    * correct read for all float types
    * 2 sigsegv fixes and a cosmetic fix for Internal Error
    * push/pop is now correct optimized (=> mov (%esp),reg)

  Revision 1.1.1.1  1998/03/25 11:18:12  root
  * Restored version

  Revision 1.29  1998/03/24 21:48:29  florian
    * just a couple of fixes applied:
         - problem with fixed16 solved
         - internalerror 10005 problem fixed
         - patch for assembler reading
         - small optimizer fix
         - mem is now supported

  Revision 1.28  1998/03/19 18:57:05  florian
    * small fixes applied

  Revision 1.27  1998/03/18 22:50:10  florian
    + fstp/fld optimization
    * routines which contains asm aren't longer optimzed
    * wrong ifdef TEST_FUNCRET corrected
    * wrong data generation for array[0..n] of char = '01234'; fixed
    * bug0097 is fixed partial
    * bug0116 fixed (-Og doesn't use enter of the stack frame is greater than
      65535)

  Revision 1.26  1998/03/10 23:48:35  florian
    * a couple of bug fixes to get the compiler with -OGaxz compiler, sadly
      enough, it doesn't run

  Revision 1.25  1998/03/10 01:17:14  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.24  1998/03/04 19:09:59  jonas
    * fixed incompatibility with new code generator concerning "mov mem, reg; mov reg, edi" optimization

  Revision 1.23  1998/03/03 22:37:09  peter
    - uses errors

  Revision 1.22  1998/03/03 14:48:31  jonas
    * added errors to the uses clause (required for aopt386.inc)

  Revision 1.21  1998/03/02 21:35:15  jonas
    * added comments from last update

  Revision 1.20  1998/03/02 21:29:04  jonas
   * change "mov reg, mem; cmp x, mem" to "mov reg, mem; cmp x, reg"
   * change "and x, reg; jxx" to "test reg, x; jxx" (also allows some extra reloading opts)


  Revision 1.19  1998/03/02 01:47:58  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.18  1998/02/27 16:33:26  florian
    * syntax errors and line too long errors fixed

  Revision 1.17  1998/02/26 17:20:31  jonas
    * re-enabled mov optimizations, re-commented out the "mov mem, reg1; mov mem, reg2" optimization

  Revision 1.16  1998/02/26 11:56:55  daniel
  * New assembler optimizations commented out, because of bugs.
  * Use of dir-/name- and extstr.

  Revision 1.15  1998/02/25 14:08:30  daniel
  * Compiler uses less memory. *FIX*

  Revision 1.14  1998/02/25 12:32:12  daniel
  * Compiler uses even less memory.

  Revision 1.13  1998/02/24 21:18:12  jonas
    * file name back to lower case

  Revision 1.2  1998/02/24 20:32:11  jonas
    * added comments from latest commit

  Revision 1.1  1998/02/24 20:27:50  jonas
    + change "cmp $0, reg" to "test reg, reg"
    + add correct line numbers to Pai386 objects created by the optimizer
    * dispose TReference of second instructions optimized from "mov mem, reg1; mov
     mem, reg2" to "mov mem, reg; mov reg1, reg2"
    + optimize "mov mem, reg1; mov reg1, reg2" to "mov mem, reg2" if reg1 <> esi
    - disabled changing "mov mem, reg1; mov mem reg2" to "mov mem reg1; mov reg1,
     reg2" because of conflict with the above optimization
    + remove second instruction from "mov mem, reg; mov reg, %edi" because edi isn't
     used anymore afterwards
    + remove first instruction from "mov %eax, x(%ebp); leave/ret" because it is a
     write to either a parameter or a temporary function result
    + change "mov reg1, reg2; mov reg2, mem" to "mov reg1, mem" if reg2 <> esi
    + change "mov reg1, reg2; test/or reg2, reg2; jxx" to "test/or reg1, reg1" if
     reg2 <> esi
    + change "mov reg1, reg2; test/or reg2, reg2" to "mov reg1, reg2; test/or reg1,
     reg1" to avoid a read/write pnealty if reg2 = esi
    * took FindLoHiLabel and BuildLabelTable out of the main loop, so they're both
     called only once per code fragment that has to be optimized

  Revision 1.12  1998/02/19 22:46:55  peter
    * Fixed linebreaks

  Revision 1.11  1998/02/13 10:34:32  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.10  1998/02/12 17:18:51  florian
    * fixed to get remake3 work, but needs additional fixes (output, I don't like
      also that aktswitches isn't a pointer)

  Revision 1.9  1998/02/12 11:49:39  daniel
  Yes! Finally! After three retries, my patch!

  Changes:

  Complete rewrite of psub.pas.
  Added support for DLL's.
  Compiler requires less memory.
  Platform units for each platform.

  Revision 1.8  1998/02/10 21:57:21  peter
    + mov [mem1],reg1;mov [mem1],reg2 -> mov [mem1],reg1;mov reg1,reg2
    + mov const,[mem1];mov [mem1],reg -> mov const,reg;mov reg,[mem1]

  Revision 1.7  1998/02/07 10:10:34  michael
    + superfluous AND's after MOVZX' removed
    + change "subl $2, %esp; ... ; pushw x" to "pushl x"
    + fold "subl $const, %esp; subl $2, %esp" into one instruction

  Revision 1.5  1998/02/02 17:25:43  jonas
    * back to CVS version; change "lea (reg1), reg2" to "mov reg1, reg2"


  Revision 1.2  1997/12/09 13:19:36  carl
  + renamed pai_labeled --> pai_labeled

  Revision 1.1.1.1  1997/11/27 08:32:50  michael
  FPC Compiler CVS start

  Pre-CVS log:

  FK   Florian Klampfl (FK)
  JM   Jonas Maebe

  + feature added
  - removed
  * bug fixed or changed

  History (started with version 0.9.0):
       5th november 1996:
         * adapted to 0.9.0
      30th december 1996:
         * runs with 0.9.1
      25th July 1996:
         + removal of superfluous "test %reg, %reg" instructions (JM)
      28th July 1997:
         + change "shl $1, %reg" to "add %reg, %reg" (not working) (JM)
         * fixed bugs in test optimization (tested and working) (JM)
      29th July 1997:
         * fixed some pointer bugs in SHL optimization, but it still doesn't
           work :( (JM)
      30th July 1997:
         + change "sar const1, %reg; shl const2, %reg" to one statement (JM)
         * I finally correctly understand the structure of the pai(386)
           object <g> and fixed the shl optimization (tested and working) (JM)
      31th July 1997:
         + removal of some superfluous reloading of registers (not working) (JM)
       4th August 1997:
         * fixed reloading optimization (thanks Florian!) (JM)
       6th August 1997:
         + removal of labels which are not referenced by any instruction
           (allows for easier and better optimization), but it is slow :( (JM)
       8th August 1997:
         - removed label-removal procedure as it seems to be impossible to
           find out if there are labels which are referenced through a jump
           table (JM)
      15th August 1997:
         + removal of superfluous "or %reg, %reg" instructions (JM)
      22th september 1997:
         * test is also removed if it follows neg, shl and shr (FK)
         - removed the sar/shl optimization because:
             movl $0xff,%eax
             shrl $0x3,%eax
             shll $0x3,%eax

               => EAX is $0xf8  !!!   (FK)
      23th September 1997:
         + function FindLabel() so sequences like "jmp l2;l1:;l2:" can be
           optimized (JM)
      24th September 1997:
         + successive jumps reduced to one jump (see explanation at
           GetFinalDestination). Works fine, but seems to enlarge the code...
           I suppose because there are more >128 bytes-jumps and their opcodes
           are longer. If (cs_littlesize in aktwitches^), this optimization is
           not performed (JM)
      26th September 1997:
         * removed the "Var" in front of the parameters of InsertLLItem, which
           had introduced the need for the temp var p1 (also removed) (JM)
         * fixed a bug in FindLabel() that caused false positives in some
           cases (JM)
         * removed the unit systems from the uses clause because it isn't
           needed anymore (it was needed for the label-removal procedure) (JM)
         * adapted for 0.9.3 and 0.9.4 (still bugged) (JM)
      27th September 1997:
         * fixed 0.9.3+ related bugs (JM)
         * make peepholeopt optimize the code twice, because after the first
           pass several labels can be removed (those unset by
           GetFinalDestination) which sometimes allows extra optimizations
           (not when (cs_littlesize in aktswitches^), because then
           GetFinalDestination is never called)) (JM)
       1st October 1997:
         * adapted to use with tp (tlabeltable too large and lines to long) (FK)
         + removal of dead code (which sits between a jmp and the next label), also
           sometimes allows some extra optimizations during the second pass (JM)
       2nd October 1997:
         + successive conditional jumps reduced to one jump (JM)
        3rd October 1997:
         * made FindLabel a little shorter&faster (JM)
         * make peepholeopt always go through the code twice, because the dead
           code removal can allow some extra optimizations (JM)
       10th October 1997:
         * optimized remove_mov code a little (JM)
       12th October 1997:
         * bugfixed remove_mov change (JM)
       20th October 1997:
         * changed the combiTmpBoolnation of two adds (which replaced "shl 2, %reg")
           to a "lea %reg, (,%reg,4)" if the register is 32 bit (JM)
       21th October 1997:
         + change movzx to faster equivalents (not working) (thanks to Daniel
           Mantoine for the initial idea) (JM)
       30th October 1997:
         * found out that "shl $const, %reg" is a pairable instruction after
           all and therefore removed the dual "add %reg, %reg" sequences (JM)
         * replace "shl $3, %reg" with "lea %reg, (,%reg,8)" (JM)
        2nd November 1997:
         * fixed movzx replacements (JM)
        3rd November 1997:
         * some changes in the optimization logic to generate better PPro
           code (JM)
         * change two consecutive 16 bit immediatie pushes to one 32 bit push
           (thanks to Synopsis for the suggestion) (JM)
        4th November 1997:
         + replace some constant multiplies with lea sequences (suggestion from
           Synopsis, Daniel Mantoine and Florian Klaempfl) (JM)
        5th November 1997:
         * finally bugfixed sar/shl optimization and reactivated it (JM)
         + some extra movzx optimizations (JM)
        6th November 1997:
         + change shl/add/sub sequences to one lea instruction if possible (JM)
         * bugfixed some imul replacements (JM)
       30th November 1997:
         * merge two consecutive "and $const, %reg"'s to one statement (JM)
        5th December 1997:
         + change "mov $0, %reg" to "xor %reg, %reg" (JM)
         * adapted to TP (typecasted pointer to longint for comparisons
           and one line too long) (JM)
}

