{
    $Id$
    Copyright (c) 1999 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit implements the i386 optimizer object

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

Interface

uses cpubase, aoptobj, aoptcpub;

Type
  TRegInfoCpu = Object(TRegInfo)
    Procedure AddReg(OldReg, NewReg: TRegister); Virtual;
  End;

Type
  TAOptCpu = Object(TAoptObj)
    { uses the same constructor as TAopObj }
    Function RegMaxSize(Reg: TRegister): TRegister; Virtual;
    Function RegsSameSize(Reg1, Reg2: TRegister): Boolean; Virtual;
    Function IsLoadInstr(p: pai): Boolean; Virtual;
    Function IsStoreInstr(p: pai): Boolean; Virtual;
    Function TCh2Reg(Ch: TChange): TRegister; Virtual;
    Function RegReadByInstrCPU(Reg: TRegister; p: Pai); Virtual;
  End;

Implementation

{*********************** TAoptCpu **************************}
Function TAOptCpu.RegMaxSize(Reg: TRegister): TRegister;
Begin
  RegMaxSize := Reg;
  If (Reg >= R_AX)
    Then
      If (Reg <= R_DI)
        Then RegMaxSize := Reg16ToReg32(Reg)
        Else
          If (Reg <= R_BL)
            Then RegMaxSize := Reg8toReg32(Reg)
End;

Function TAOptCpu.RegsSameSize(Reg1, Reg2: TRegister): Boolean;
Begin
  If (Reg1 <= R_EDI)
    Then RegsSameSize := (Reg2 <= R_EDI)
    Else
      If (Reg1 <= R_DI)
        Then RegsSameSize := (Reg2 in [R_AX..R_DI])
        Else
          If (Reg1 <= R_BL)
            Then RegsSameSize := (Reg2 in [R_AL..R_BL])
            Else RegsSameSize := False
End;

Function TAOptCpu.IsLoadInstr(p: pai): Boolean;
Begin
  IsLoadInstr :=
    (p^.typ = ait_instruction) and
    ((PInstr(p)^.OpCode = A_MOV) or
     (PInstr(p)^.OpCode = A_MOVZX) or
     (PInstr(p)^.OpCode = A_MOVSX)) And
    (PInstr(p)^.oper[LoadSrc].typ = top_ref));
End;

Function TAOptCpu.IsStoreInstr(p: pai): Boolean;
Begin
  IsLoadInstr :=
    (p^.typ = ait_instruction) and
    ((PInstr(p)^.OpCode = A_MOV) or
     (PInstr(p)^.OpCode = A_MOVZX) or
     (PInstr(p)^.OpCode = A_MOVSX)) And
    (PInstr(p)^.oper[StoreDst].typ = top_ref));
End;

Function TAOptCpu.TCh2Reg(Ch: TChange): TRegister;
Begin
  If (Ch <= C_REDI) Then
    TCh2Reg := TRegister(Byte(Ch))
  Else
    If (Ch <= C_WEDI) Then
      TCh2Reg := TRegister(Byte(Ch) - Byte(C_REDI))
    Else
      If (Ch <= C_RWEDI) Then
        TCh2Reg := TRegister(Byte(Ch) - Byte(C_WEDI))
      Else InternalError($db)
End;

Function TAOptCpu.RegReadByInstr(Reg: TRegister; p: Pai);
Var Cnt: AWord;
    InstrProp: TAsmInstrucProp;
    TmpResult: Boolean;
Begin
  TmpResult := False;
  If (p^.typ = ait_instruction) Then
    Case p^.opcode of
      A_IMUL:
        With PInstr(p)^ Do
          TmpResult :=
            RegInOp(Reg,op[0]) or
            RegInOp(Reg,op[1]) or
            ((ops = 1) and
             (reg = R_EAX]))
      A_DIV, A_IDIV, A_MUL:
        TmpResult :=
          RegInOp(Reg,op[0]) or
          (Reg = R_EAX) or
          ((Reg = R_EDX) and
           ((p^.opcode = A_DIV) or
            (p^.opcode = A_IDIV)) and
           (p^.size = S_L))
      Else
        Begin
          Cnt := 1;
          InstrProp := AsmInstr[PInstr(p)^.OpCode);
          While (Cnt <= MaxCh) And
                (InstrProp.Ch[Cnt] <> C_None) And
                Not(TmpResult) Do
            Begin
              Case InstrProp.Ch[Cnt] Of
                C_REAX..C_REDI,C_RWEAX..C_RWEDI
  {$ifdef arithopt}
                ,C_MEAX..C_MEDI
  {$endif arithopt}:
                  TmpResult := Reg = TCh2Reg(InstrProp.Ch[Cnt]);
                C_ROp1,C_RWOp1{$ifdef arithopt},C_Mop1{$endif arithopt}:
                  TmpResult := RegInOp(PInstr(p)^.oper[0]);
                C_ROp2,C_RWOp2{$ifdef arithopt},C_Mop2{$endif arithopt}:
                  TmpResult := RegInOp(PInstr(p)^.oper[1]);
                C_ROp3,C_RWOp3{$ifdef arithopt},C_Mop3{$endif arithopt}:
                  TmpResult := RegInOp(PInstr(p)^.oper[2]);
                C_WOp1: TmpResult := (PInstr^.oper[0].typ = top_ref) And
                                     (RegInRef(Reg,PInstr^.oper[0].ref);
                C_WOp2: TmpResult := (PInstr^.oper[0].typ = top_ref) And
                                     (RegInRef(Reg,PInstr^.oper[0].ref);
                C_WOp3: TmpResult := (PInstr^.oper[0].typ = top_ref) And
                                     (RegInRef(Reg,PInstr^.oper[0].ref);
                C_WMemEDI: TmpResult := (Reg = R_EDI);
                C_FPU: TmpResult := Reg in [R_ST..R_ST7,R_MM0..R_MM7]
              End;
              Inc(Cnt)
            End
        End
    End
End;

{ ********************* TRegInfoCpu *****************}

Procedure TRegInfoCpu.AddReg(OldReg, NewReg: TRegister);
Begin
  NewRegsEncountered := NewRegsEncountered + [NewReg];
  OldRegsEncountered := OldRegsEncountered + [OldReg];
  New2OldReg[NewReg] := OldReg;
  Case OldReg Of
    R_EAX..R_EDI:
      Begin
        NewRegsEncountered := NewRegsEncountered + [Reg32toReg16(NewReg)];
        OldRegsEncountered := OldRegsEncountered + [Reg32toReg16(OldReg)];
        New2OldReg[Reg32toReg16(NewReg)] := Reg32toReg16(OldReg);
        If (NewReg in [R_EAX..R_EBX]) And
           (OldReg in [R_EAX..R_EBX]) Then
          Begin
            NewRegsEncountered := NewRegsEncountered + [Reg32toReg8(NewReg)];
            OldRegsEncountered := OldRegsEncountered + [Reg32toReg8(OldReg)];
            New2OldReg[Reg32toReg8(NewReg)] := Reg32toReg8(OldReg);
          End;
      End;
    R_AX..R_DI:
      Begin
        NewRegsEncountered := NewRegsEncountered + [Reg16toReg32(NewReg)];
        OldRegsEncountered := OldRegsEncountered + [Reg16toReg32(OldReg)];
        New2OldReg[Reg16toReg32(NewReg)] := Reg16toReg32(OldReg);
        If (NewReg in [R_AX..R_BX]) And
           (OldReg in [R_AX..R_BX]) Then
          Begin
            NewRegsEncountered := NewRegsEncountered + [Reg16toReg8(NewReg)];
            OldRegsEncountered := OldRegsEncountered + [Reg16toReg8(OldReg)];
            New2OldReg[Reg16toReg8(NewReg)] := Reg16toReg8(OldReg);
          End;
      End;
    R_AL..R_BL:
      Begin
        NewRegsEncountered := NewRegsEncountered + [Reg8toReg32(NewReg)]
                           + [Reg8toReg16(NewReg)];
        OldRegsEncountered := OldRegsEncountered + [Reg8toReg32(OldReg)]
                           + [Reg8toReg16(OldReg)];
        New2OldReg[Reg8toReg32(NewReg)] := Reg8toReg32(OldReg);
      End;
  End;
End;

End.
{
 $Log$
 Revision 1.4  1999-08-11 14:23:39  jonas
   * some fixes to RegReadByInstr

 Revision 1.3  1999/08/10 12:40:20  jonas
   + implemented RegReadByInstr

 Revision 1.1  1999/08/08 13:24:50  jonas
   + added copyright header/GNU license info
   * made the assembler optimizer almost completely OOP
   * some code style clean up and extra comments
   * moved from the new/aopt to the /new and /new/i386 dirs

}