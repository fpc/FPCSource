{
    $Id$
    Copyright (c) 1999 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit contains the common subexpression elimination object of the
    assembler optimizer.

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
unit aoptcs;

interface

uses aasm, aoptcpu, aoptobj;

{ ************************************************************************* }
{ info about the equivalence of registers when comparing two code sequences }
{ ************************************************************************* }

  TRegInfo = Object(TAoptBaseCpu)
    { registers encountered in the new and old sequence }
    NewRegsEncountered, OldRegsEncountered,
    { registers which only have been loaded for use as base or index in a }
    { reference later on                                                  }
    RegsLoadedForRef: TRegSet;
    { to which register in the old sequence corresponds every register in }
    { the new sequence                                                    }
    New2OldReg: TRegArray;

    Constructor init;
    { clear all information store in the object }
    Procedure Clear;
    { the contents of OldReg in the old sequence are now being loaded into }
    { NewReg in the new sequence                                           }
    Procedure AddReg(OldReg, NewReg: TRegister); Virtual;
    { the contents of OldOp in the old sequence are now being loaded into }
    { NewOp in the new sequence. It is assumed that OldOp and NewOp are   }
    { equivalent                                                          }
    Procedure AddOp(const OldOp, NewOp:Toper);
    { check if a register in the old sequence (OldReg) can be equivalent to }
    { a register in the new sequence (NewReg) if the operation OpAct is     }
    { performed on it. The RegInfo is updated (not necessary to call AddReg }
    { afterwards)                                                           }
    Function RegsEquivalent(OldReg, NewReg: TRegister; OpAct: TopAction):
      Boolean;
    { check if a reference in the old sequence (OldRef) can be equivalent   }
    { to a reference in the new sequence (NewRef) if the operation OpAct is }
    { performed on it. The RegInfo is updated (not necessary to call AddOp  }
    { afterwards)                                                           }
    Function RefsEquivalent(Const OldRef, NewRef: TReference; OpAct:
      TOpAction): Boolean;
    { check if an operand in the old sequence (OldOp) can be equivalent to }
    { an operand in the new sequence (NewOp) if the operation OpAct is     }
    { performed on it. The RegInfo is updated (not necessary to call AddOp }
    { afterwards)                                                          }
    Function OpsEquivalent(const OldOp, NewOp: toper; OpAct: TopAction):
      Boolean;
    { check if an instruction in the old sequence (OldP) can be equivalent  }
    { to an instruction in the new sequence (Newp). The RegInfo is updated  }
    Function InstructionsEquivalent(OldP, NewP: Pai): Boolean;
  End;


{ ************************************************************************* }
{ *************** The common subexpression elimination object ************* }
{ ************************************************************************* }

Type TAoptCSE = Object(TAoptObj)
       { returns true if the instruction p1 modifies the register Reg }
       Function RegModifiedByInstruction(Reg: TRegister; p1: Pai): Boolean;
     End;

Implementation

{ ************************************************************************* }
{ ******************************* TReginfo ******************************** }
{ ************************************************************************* }

Constructor TRegInfo.Init;
Begin
  Clear;
End;

Procedure TRegInfo.Clear;
Begin
  RegsLoadedForRef   := [];
  NewRegsEncountered := [ProcInfo.FramePointer, stack_pointer];
  OldRegsEncountered := [ProcInfo.FramePointer, stack_pointer];
  New2OldReg[ProcInfo.FramePointer] := ProcInfo.FramePointer;
  New2OldReg[R_ESP] := R_ESP;
End;

Procedure TRegInfo.AddReg(OldReg, NewReg: TRegister);
{ updates the ???RegsEncountered and ???2???Reg fields of RegInfo. Assumes  }
{ that OldReg and NewReg have the same size (has to be chcked in advance    }
{ with RegsSameSize) and that neither equals R_NO                           }
{ has to be overridden for architectures like the 80x86 when not all GP     }
{ regs are of the same size                                                 }
Begin
  NewRegsEncountered := NewRegsEncountered + [NewReg];
  OldRegsEncountered := OldRegsEncountered + [OldReg];
  New2OldReg[NewReg] := OldReg;
End;

Procedure TRegInfo.AddOp(const OldOp, NewOp:Toper);
Begin
  Case OldOp.typ Of
    Top_Reg:
      If (OldOp.reg <> R_NO) Then
        AddReg(OldOp.reg, NewOp.reg);
    Top_Ref:
      Begin
        If OldOp.ref^.base <> R_NO Then
          AddReg(OldOp.ref^.base, NewOp.ref^.base);
{$ifdef RefsHaveIndexReg}
        If OldOp.ref^.index <> R_NO Then
          AddReg(OldOp.ref^.index, NewOp.ref^.index);
{$endif RefsHaveIndexReg}
      End;
  End;
End;

Function TRegInfo.RegsEquivalent(OldReg, NewReg: TRegister;
           OPAct: TOpAction): Boolean;
Begin
  If Not((OldReg = R_NO) Or (NewReg = R_NO)) Then
    If RegsSameSize(OldReg, NewReg) Then
{ here we always check for the 32 bit component, because it is possible    }
{ that the 8 bit component has not been set, event though NewReg already   }
{ has been processed. This happens if it has been compared with a register }
{ that doesn't have an 8 bit component (such as EDI). In that case the 8   }
{ bit component is still set to R_NO and the comparison in the Else-part   }
{ will fail                                                                }
      If (RegMaxSize(OldReg) in OldRegsEncountered) Then
        If (RegMaxSize(NewReg) in NewRegsEncountered) Then
          RegsEquivalent := (OldReg = New2OldReg[NewReg])
{ If we haven't encountered the new register yet, but we have encountered }
{ the old one already, the new one can only be correct if it's being      }
{ written to (and consequently the old one is also being written to),     }
{ otherwise                                                               }
{                                                                         }
{  movl -8(%ebp), %eax        and         movl -8(%ebp), %eax             }
{  movl (%eax), %eax                      movl (%edx), %edx               }
{                                                                         }
{  are considered equivalent                                              }
        Else
          If (OpAct = OpAct_Write) Then
            Begin
              AddReg(OldReg, NewReg);
              RegsEquivalent := True
            End
          Else Regsequivalent := False
      Else
        If Not(RegMaxSize(NewReg) in NewRegsEncountered) Then
          Begin
            AddReg(OldReg, NewReg);
            RegsEquivalent := True
          End
        Else RegsEquivalent := False
    Else RegsEquivalent := False
  Else RegsEquivalent := OldReg = NewReg
End;

Function TRegInfo.RefsEquivalent(Const OldRef, NewRef: TReference;
           OpAct: TOpAction): Boolean;
Begin
  If OldRef.is_immediate Then
    RefsEquivalent := NewRef.is_immediate and (OldRef.Offset = NewRef.Offset)
  Else
    RefsEquivalent := (OldRef.Offset+OldRef.OffsetFixup =
                         NewRef.Offset+NewRef.OffsetFixup) And
                      RegsEquivalent(OldRef.Base, NewRef.Base, OpAct)
{$ifdef RefsHaveindexReg}
                      And RegsEquivalent(OldRef.Index, NewRef.Index, OpAct)
{$endif RefsHaveIndexReg}
{$ifdef RefsHaveScale}
                      And (OldRef.ScaleFactor = NewRef.ScaleFactor)
{$endif RefsHaveScale}
                      And (OldRef.Symbol = NewRef.Symbol)
{$ifdef RefsHaveSegment}
                      And (OldRef.Segment = NewRef.Segment)
{$endif RefsHaveSegment}
                      ;
End;

Function TRegInfo.OpsEquivalent(const OldOp, NewOp: toper; OpAct: TopAction):
           Boolean;
Begin
  OpsEquivalent := False;
  if OldOp.typ=NewOp.typ then
    Case OldOp.typ Of
      Top_Const: OpsEquivalent := OldOp.val = NewOp.val;
      Top_Reg: OpsEquivalent := RegsEquivalent(OldOp.reg,NewOp.reg, OpAct);
      Top_Ref: OpsEquivalent := RefsEquivalent(OldOp.ref^, NewOp.ref^, OpAct);
      Top_None: OpsEquivalent := True
    End;
End;

Function TRegInfo.InstructionsEquivalent(OldP, NewP: Pai): Boolean;

  Function OperandTypesEqual: Boolean;
  Var Count: AWord;
  Begin
    OperandTypesEqual := False;
    For Count := 0 to max_operands-1 Do
      If (PInstr(OldP)^.oper[Count].typ <> PInstr(NewP)^.oper[Count].typ) Then
        Exit;
    OperandTypesEqual := True
  End;
  
Var Count: AWord;
    TmpResult: Boolean;
Begin
  If Assigned(OldP) And Assigned(NewP) And
     (Pai(OldP)^.typ = ait_instruction) And
     (Pai(NewP)^.typ = ait_instruction) And
     (PInstr(OldP)^.opcode = PInstr(NewP)^.opcode) And
     OperandTypesEqual
    Then
{ both instructions have the same structure:                }
{ "<operator> <operand of type1>, <operand of type 2>, ..." }
      If IsLoadMemReg(OldP) Then
{ then also NewP = loadmemreg because of the previous check }
        If Not(RegInRef(PInstr(OldP)^.oper[LoadDst].reg,
                 PInstr(OldP)^.oper[LoadSrc].ref^)) Then
{ the "old" instruction is a load of a register with a new value, not with }
{ a value based on the contents of this register (so no "mov (reg), reg")  }
          If Not(RegInRef(PInstr(NewP)^.oper[LoadDst].reg,
                          PInstr(NewP)^.oper[LoadSrc].ref^)) And
             RefsEqual(PInstr(OldP)^.oper[LoadSrc].ref^,
                       PInstr(NewP)^.oper[LoadSrc].ref^)
            Then
{ the "new" instruction is also a load of a register with a new value, and }
{ this value is fetched from the same memory location                      }
              Begin
                With PInstr(NewP)^.oper[LoadSrc].ref^ Do
                  Begin
                    If Not(Base in [ProcInfo.FramePointer, R_NO, stack_pointer])
{ it won't do any harm if the register is already in RegsLoadedForRef }
                      Then RegsLoadedForRef := RegsLoadedForRef + [Base];
{$ifdef RefsHaveIndexReg}
                    If Not(Index in [ProcInfo.FramePointer, R_NO, stack_pointer])
                      Then RegsLoadedForRef := RegsLoadedForRef + [Index];
{$endif RefsHaveIndexReg}
                  End;
{ add the registers from the reference (.oper[Src]) to the RegInfo, all }
{ registers from the reference are the same in the old and in the new   }
{ instruction sequence (refsequal returned true)                        }
                AddOp(PInstr(OldP)^.oper[LoadSrc], PInstr(OldP)^.oper[LoadSrc]);
{ the registers from .oper[Dest] have to be equivalent, but not necessarily }
{ equal                                                                     }
                InstructionsEquivalent :=
                  RegsEquivalent(PInstr(OldP)^.oper[LoadDst].reg,
                                 PInstr(NewP)^.oper[LoadDst].reg, OpAct_Write);
              End
{ the registers are loaded with values from different memory locations. If }
{ this were allowed, the instructions "mov -4(%esi),%eax" and              }
{  "mov -4(%ebp),%eax" would be considered equivalent                      }
            Else InstructionsEquivalent := False
        Else
{ load register with a value based on the current value of this register }
          Begin
            With PInstr(NewP)^.oper[0].ref^ Do
{ Assume the registers occurring in the reference have only been loaded with }
{ the value they contain now to calculate an address (so the value they have }
{ now, won't be stored to memory later on)                                   }
              Begin
                If Not(Base in [ProcInfo.FramePointer,
                                RegMaxSize(PInstr(NewP)^.oper[LoadDst].reg),
                                R_NO,stack_pointer])
{ It won't do any harm if the register is already in RegsLoadedForRef }
                  Then
                    Begin
                      RegsLoadedForRef := RegsLoadedForRef + [Base];
{$ifdef csdebug}
                      Writeln(att_reg2str[base], ' added');
{$endif csdebug}
                    end;
{$Ifdef RefsHaveIndexReg}
                If Not(Index in [ProcInfo.FramePointer,
                                 RegMaxSize(PInstr(NewP)^.oper[LoadDst].reg),
                                 R_NO,StackPtr])
                  Then
                    Begin
                      RegsLoadedForRef := RegsLoadedForRef + [Index];
{$ifdef csdebug}
                      Writeln(att_reg2str[index], ' added');
{$endif csdebug}
                    end;
{$endif RefsHaveIndexReg}
              End;

{ now, remove the destination register of the load from the                 }
{ RegsLoadedForReg, since if it's loaded with a new value, it certainly     }
{ will still be used later on                                               }
            If Not(RegMaxSize(PInstr(NewP)^.oper[LoadDst].reg) In
                [ProcInfo.FramePointer,R_NO,stack_pointer])
              Then
                Begin
                  RegsLoadedForRef := RegsLoadedForRef -
                    [RegMaxSize(PInstr(NewP)^.oper[LoadDst].reg)];
{$ifdef csdebug}
                  Writeln(att_reg2str[RegMaxSize(PInstr(NewP)^.oper[1].reg)], ' removed');
{$endif csdebug}
                end;
            InstructionsEquivalent :=
               OpsEquivalent(PInstr(OldP)^.oper[LoadSrc],
                             PInstr(NewP)^.oper[LoadSrc], OpAct_Read) And
               OpsEquivalent(PInstr(OldP)^.oper[LoadDst],
                             PInstr(NewP)^.oper[LoadDst], OpAct_Write)
          End
      Else
{ OldP and NewP are not a load instruction, but have the same structure }
{ (opcode, operand types), so they're equivalent if all operands are    }
{ equivalent                                                            }
       Begin
         Count := 0;
         TmpResult := true;
         Repeat
           TmpResult :=
             OpsEquivalent(PInstr(OldP)^.oper[Count], PInstr(NewP)^.oper[Count],
                           OpAct_Unknown);
           Inc(Count)
         Until (Count = MaxOps) or not(TmpResult);
         InstructionsEquivalent := TmpResult
       End
{ the instructions haven't even got the same structure, so they're certainly }
{ not equivalent                                                             }
    Else InstructionsEquivalent := False;
End;

{ ************************************************************************* }
{ ******************************* TAOptCSE ******************************** }
{ ************************************************************************* }


Function TAOptCSE.RegModifiedByInstruction(Reg: TRegister; p1: Pai): Boolean;
Var hp: Pai;
Begin
  If GetLastInstruction(p1, hp)
    Then
      RegModifiedByInstruction :=
        PPAiProp(p1^.OptInfo)^.GetWState <>
          PPAiProp(hp^.OptInfo)^.GetWState
    Else RegModifiedByInstruction := True;
End;

End.

{
  $Log$
  Revision 1.1  1999-08-18 14:32:21  jonas
    + compilable!
    + dataflow analyzer finished
    + start of CSE units
    + aoptbase which contains a base object for all optimizer objects
    * some constants and type definitions moved around to avoid circular
      dependencies
    * moved some methods from base objects to specialized objects because
      they're not used anywhere else

}