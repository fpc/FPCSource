{
    $Id$
    Copyright (c) 1998-2000 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit contains the base of all optimizer related objects

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
unit aoptbase;

Interface

uses aasm, cpuasm,cpubase;

{ the number of tai objects processed by an optimizer object since the last }
{ time a register was modified                                              }
Type TInstrSinceLastMod = Array[LoGPReg..HiGPReg] of byte;

{ the TAopBase object implements the basic methods that most other }
{ assembler optimizer objects require                              }
Type
  TAoptBase = Object
    { processor independent methods }

    constructor init;
    destructor done;
    { returns true if register Reg is used by instruction p1 }
    Function RegInInstruction(Reg: TRegister; p1: tai): Boolean;
    { returns true if register Reg occurs in operand op }
    Function RegInOp(Reg: TRegister; const op: toper): Boolean;
    { returns true if register Reg is used in the reference Ref }
    Function RegInRef(Reg: TRegister; Const Ref: TReference): Boolean;

    { returns true if the references are completely equal }
    {Function RefsEqual(Const R1, R2: TReference): Boolean;}

    { gets the next tai object after current that contains info relevant }
    { to the optimizer in p1. If there is none, it returns false and     }
    { sets p1 to nil                                                     }
    Function GetNextInstruction(Current: tai; Var Next: tai): Boolean;
    { gets the previous tai object after current that contains info  }
    { relevant to the optimizer in last. If there is none, it retuns }
    { false and sets last to nil                                     }
    Function GetLastInstruction(Current: tai; Var Last: tai): Boolean;


    { processor dependent methods }

    { returns the maximum width component of Reg. Only has to be }
    { overridden for the 80x86 (afaik)                           }
    Function RegMaxSize(Reg: TRegister): TRegister; Virtual;
    { returns true if Reg1 and Reg2 are of the samae width. Only has to }
    { overridden for the 80x86 (afaik)                                  }
    Function RegsSameSize(Reg1, Reg2: TRegister): Boolean; Virtual;
    { returns whether P is a load instruction (load contents from a }
    { memory location or (register) variable into a register)       }
    Function IsLoadMemReg(p: tai): Boolean; Virtual;
    { returns whether P is a load constant instruction (load a constant }
    { into a register)                                                  }
    Function IsLoadConstReg(p: tai): Boolean; Virtual;
    { returns whether P is a store instruction (store contents from a }
    { register to a memory location or to a (register) variable)      }
    Function IsStoreRegMem(p: tai): Boolean; Virtual;

    { create a paicpu Object that loads the contents of reg1 into reg2 }
    Function a_load_reg_reg(reg1, reg2: TRegister): taicpu; Virtual;

end;

Function RefsEqual(Const R1, R2: TReference): Boolean;


Implementation

uses globals, aoptcpub, cpuinfo;

Function RefsEqual(Const R1, R2: TReference): Boolean;
Begin
  If R1.is_immediate Then
    RefsEqual := R2.is_immediate and (R1.Offset = R2.Offset)
  Else
    RefsEqual := (R1.Offset+R1.OffsetFixup = R2.Offset+R2.OffsetFixup)
                 And (R1.Base = R2.Base)
{$ifdef RefsHaveindex}
                 And (R1.Index = R2.Index)
{$endif RefsHaveindex}
{$ifdef RefsHaveScale}
                 And (R1.ScaleFactor = R2.ScaleFactor)
{$endif RefsHaveScale}
                 And (R1.Symbol = R2.Symbol)
{$ifdef RefsHaveSegment}
                 And (R1.Segment = R2.Segment)
{$endif RefsHaveSegment}
End;


constructor taoptbase.init;
begin
end;

destructor taoptbase.done;
begin
end;

Function TAOptBase.RegInInstruction(Reg: TRegister; p1: tai): Boolean;
Var Count: AWord;
    TmpResult: Boolean;
Begin
  TmpResult := False;
  Count := 0;
  If (p1.typ = ait_instruction) Then
    Repeat
      TmpResult := RegInOp(Reg, PInstr(p1)^.oper[Count]);
      Inc(Count)
    Until (Count = MaxOps) or TmpResult;
  RegInInstruction := TmpResult
End;


Function TAOptBase.RegInOp(Reg: TRegister; const op: toper): Boolean;
Begin
  Case op.typ Of
    Top_Reg: RegInOp := Reg = op.reg;
    Top_Ref: RegInOp := RegInRef(Reg, op.ref^)
    Else RegInOp := False
  End
End;


Function TAOptBase.RegInRef(Reg: TRegister; Const Ref: TReference): Boolean;
Begin
  Reg := RegMaxSize(Reg);
  RegInRef := (Ref.Base = Reg)
{$ifdef RefsHaveIndexReg}
  Or (Ref.Index = Reg)
{$endif RefsHaveIndexReg}
End;

Function TAOptBase.GetNextInstruction(Current: tai; Var Next: tai): Boolean;
Begin
  Repeat
    Current := tai(Current.Next);
    While Assigned(Current) And
          ((Current.typ In SkipInstr) or
           ((Current.typ = ait_label) And
            Not(Tai_Label(Current).l.is_used))) Do
      Current := tai(Current.Next);
    If Assigned(Current) And
       (Current.typ = ait_Marker) And
       (Tai_Marker(Current).Kind = NoPropInfoStart) Then
      Begin
        While Assigned(Current) And
              ((Current.typ <> ait_Marker) Or
               (Tai_Marker(Current).Kind <> NoPropInfoEnd)) Do
          Current := Tai(Current.Next);
      End;
  Until Not(Assigned(Current)) Or
        (Current.typ <> ait_Marker) Or
        (Tai_Marker(Current).Kind <> NoPropInfoEnd);
  Next := Current;
  If Assigned(Current) And
     Not((Current.typ In SkipInstr) or
         ((Current.typ = ait_label) And
          Not(Tai_Label(Current).l.is_used)))
    Then GetNextInstruction := True
    Else
      Begin
        Next := Nil;
        GetNextInstruction := False;
      End;
End;

Function TAOptBase.GetLastInstruction(Current: tai; Var Last: tai): Boolean;
Begin
  Repeat
    Current := Tai(Current.previous);
    While Assigned(Current) And
          (((Current.typ = ait_Marker) And
            Not(Tai_Marker(Current).Kind in [AsmBlockEnd,NoPropInfoEnd])) or
           (Current.typ In SkipInstr) or
           ((Current.typ = ait_label) And
             Not(Tai_Label(Current).l.is_used))) Do
      Current := Tai(Current.previous);
    If Assigned(Current) And
       (Current.typ = ait_Marker) And
       (Tai_Marker(Current).Kind = NoPropInfoEnd) Then
      Begin
        While Assigned(Current) And
              ((Current.typ <> ait_Marker) Or
               (Tai_Marker(Current).Kind <> NoPropInfoStart)) Do
          Current := Tai(Current.previous);
      End;
  Until Not(Assigned(Current)) Or
        (Current.typ <> ait_Marker) Or
        (Tai_Marker(Current).Kind <> NoPropInfoStart);
  If Not(Assigned(Current)) or
     (Current.typ In SkipInstr) or
     ((Current.typ = ait_label) And
      Not(Tai_Label(Current).l.is_used)) or
     ((Current.typ = ait_Marker) And
      (Tai_Marker(Current).Kind = AsmBlockEnd))
    Then
      Begin
        Last := Nil;
        GetLastInstruction := False
      End
    Else
      Begin
        Last := Current;
        GetLastInstruction := True;
      End;
End;


{ ******************* Processor dependent stuff *************************** }

Function TAOptBase.RegMaxSize(Reg: TRegister): TRegister;
Begin
  RegMaxSize := Reg
End;

Function TAOptBase.RegsSameSize(Reg1, Reg2: TRegister): Boolean;
Begin
  RegsSameSize := True
End;

Function TAOptBase.IsLoadMemReg(p: tai): Boolean;
Begin
  Abstract
End;

Function TAOptBase.IsLoadConstReg(p: tai): Boolean;
Begin
  Abstract
End;

Function TAOptBase.IsStoreRegMem(p: tai): Boolean;
Begin
  Abstract
End;

Function TAoptBase.a_load_reg_reg(reg1, reg2: TRegister): taicpu;
Begin
  Abstract
End;

End.

{
  $Log$
  Revision 1.4  2002-05-16 19:46:34  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.1  2001/08/26 13:36:35  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.1  2000/07/13 06:30:07  michael
  + Initial import

  Revision 1.5  2000/01/07 01:14:51  peter
    * updated copyright to 2000

  Revision 1.4  1999/11/09 22:57:08  peter
    * compiles again both i386,alpha both with optimizer

  Revision 1.3  1999/09/08 15:01:31  jonas
    * some small changes so the noew optimizer is again compilable

  Revision 1.2  1999/08/23 14:41:12  jonas
    + checksequence (processor independent)\n  + processor independent part of docse

  Revision 1.1  1999/08/18 14:32:21  jonas
    + compilable!
    + dataflow analyzer finished
    + start of CSE units
    + aoptbase which contains a base object for all optimizer objects
    * some constants and type definitions moved around to avoid circular
      dependencies
    * moved some methods from base objects to specialized objects because
      they're not used anywhere else

}
