 {
    $Id$
    Copyright (c) 1998-2000 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit contains several types and constants necessary for the
    optimizer to work on the 80x86 architecture

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
Unit aoptcpub; { Assembler OPTimizer CPU specific Base }

{ enable the following define if memory references can have both a base and }
{ index register in 1 operand                                               }

{$define RefsHaveIndexReg}

{ enable the following define if memory references can have a scaled index }

{$define RefsHaveScale}

{ enable the following define if memory references can have a segment }
{ override                                                            }

{$define RefsHaveSegment}

Interface

uses aasm, cpubase, cpuasm, aoptbase;

Type

{ possible actions on an operand: read, write or modify (= read & write) }
  TOpAction = (OpAct_Read, OpAct_Write, OpAct_Modify, OpAct_Unknown);

{ type of a normal instruction }
  TInstr = Taicpu;
  PInstr = ^TInstr;

  TFlag = (DirFlag);

  TFlagContents = (F_Unknown, F_Clear, F_Set);

{ ************************************************************************* }
{ **************************** TCondRegs ********************************** }
{ ************************************************************************* }
{ Info about the conditional registers                                      }
  TCondRegs = Object
    Flags: Array[TFlag] of TFlagContents;
    Constructor Init;
    Procedure InitFlag(f: TFlag);
    Procedure SetFlag(f: TFlag);
    Procedure ClearFlag(f: TFlag);
    Function GetFlag(f: TFlag): TFlagContents;
    Destructor Done;
  End;

{ ************************************************************************* }
{ **************************** TAoptBaseCpu ******************************* }
{ ************************************************************************* }

  TAoptBaseCpu = Object(TAoptBase)
    Function RegMaxSize(Reg: TRegister): TRegister; Virtual;
    Function RegsSameSize(Reg1, Reg2: TRegister): Boolean; Virtual;
    Function IsLoadMemReg(p: pai): Boolean; Virtual;
    Function IsLoadConstReg(p: pai): Boolean; Virtual;
    Function IsStoreRegMem(p: pai): Boolean; Virtual;

    Function a_load_reg_reg(reg1, reg2: TRegister): paicpu; virtual;
  End;

{ ************************************************************************* }
{ ******************************* Constants ******************************* }
{ ************************************************************************* }
Const
{ the maximum number of operands an instruction has }

  MaxOps = 3;

{Oper index of operand that contains the source (reference) with a load }
{instruction                                                            }

  LoadSrc = 0;

{Oper index of operand that contains the destination (register) with a load }
{instruction                                                                }

  LoadDst = 1;

{Oper index of operand that contains the source (register) with a store }
{instruction                                                            }

  StoreSrc = 0;

{Oper index of operand that contains the destination (reference) with a load }
{instruction                                                                 }

  StoreDst = 1;


Implementation

uses cpuinfo;

{ ************************************************************************* }
{ **************************** TCondRegs ********************************** }
{ ************************************************************************* }
Constructor TCondRegs.init;
Begin
  FillChar(Flags, SizeOf(Flags), Byte(F_Unknown))
End;

Procedure TCondRegs.InitFlag(f: TFlag);
Begin
  Flags[f] := F_Unknown
End;

Procedure TCondRegs.SetFlag(f: TFlag);
Begin
  Flags[f] := F_Set
End;

Procedure TCondRegs.ClearFlag(f: TFlag);
Begin
  Flags[f] := F_Clear
End;

Function TCondRegs.GetFlag(f: TFlag): TFlagContents;
Begin
  GetFlag := Flags[f]
End;

Destructor TCondRegs.Done; {$ifdef inl} inline; {$endif inl}
Begin
End;
{ ************************************************************************* }
{ **************************** TAoptBaseCpu ******************************* }
{ ************************************************************************* }

Function TAoptBaseCpu.RegMaxSize(Reg: TRegister): TRegister;
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

Function TAOptBaseCpu.RegsSameSize(Reg1, Reg2: TRegister): Boolean;
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

Function TAOptBaseCpu.IsLoadMemReg(p: pai): Boolean;
Begin
  IsLoadMemReg :=
    (p^.typ = ait_instruction) and
    ((PInstr(p)^.OpCode = A_MOV) or
     (PInstr(p)^.OpCode = A_MOVZX) or
     (PInstr(p)^.OpCode = A_MOVSX)) And
    (PInstr(p)^.oper[LoadSrc].typ = top_ref);
End;

Function TAOptBaseCpu.IsLoadConstReg(p: pai): Boolean;
Begin
  IsLoadConstReg :=
    (p^.typ = ait_instruction) and
    (PInstr(p)^.OpCode = A_MOV) And
    (PInstr(p)^.oper[LoadSrc].typ = top_const);
End;

Function TAOptBaseCpu.IsStoreRegMem(p: pai): Boolean;
Begin
  IsStoreRegMem :=
    (p^.typ = ait_instruction) and
    ((PInstr(p)^.OpCode = A_MOV) or
     (PInstr(p)^.OpCode = A_MOVZX) or
     (PInstr(p)^.OpCode = A_MOVSX)) And
    (PInstr(p)^.oper[StoreDst].typ = top_ref);
End;

Function TAOptBaseCpu.a_load_reg_reg(reg1, reg2: TRegister): paicpu;
Begin
  a_load_reg_Reg := New(paicpu,Op_Reg_Reg(A_MOV, S_L, reg1, reg2))
End;


End.

{
 $Log$
 Revision 1.2  2002-09-07 15:25:14  peter
   * old logs removed and tabs fixed

}
