 {
    $Id$
    Copyright (c) 1998-2000 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit contains the processor specific implementation of the
    assembler optimizer common subexpression elimination object.

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
unit aoptcpuc;

Interface

Uses
  CpuBase,AOptCs;

Type
  TRegInfoCpu = Object(TRegInfo)
    Procedure AddReg(OldReg, NewReg: TRegister); Virtual;
  End;


Implementation

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
  Revision 1.2  2002-09-07 15:25:14  peter
    * old logs removed and tabs fixed

}
