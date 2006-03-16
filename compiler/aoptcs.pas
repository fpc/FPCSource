{
    Copyright (c) 1998-2002 by Jonas Maebe, member of the Free Pascal
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
  NewRegsEncountered := [FRAME_POINTER_REG, STACK_POINTER_REG];
  OldRegsEncountered := [FRAME_POINTER_REG, STACK_POINTER_REG];
  New2OldReg[FRAME_POINTER_REG] := FRAME_POINTER_REG;
  New2OldReg[STACK_POINTER_REG] := STACK_POINTER_REG;
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
                    If Not(Base in [ProcInfo.FramePointer, R_NO, STACK_POINTER_REG])
{ it won't do any harm if the register is already in RegsLoadedForRef }
                      Then RegsLoadedForRef := RegsLoadedForRef + [Base];
{$ifdef RefsHaveIndexReg}
                    If Not(Index in [ProcInfo.FramePointer, R_NO, STACK_POINTER_REG])
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
                                R_NO,STACK_POINTER_REG])
{ It won't do any harm if the register is already in RegsLoadedForRef }
                  Then
                    Begin
                      RegsLoadedForRef := RegsLoadedForRef + [Base];
{$ifdef csdebug}
                      Writeln(std_reg2str[base], ' added');
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
                      Writeln(std_reg2str[index], ' added');
{$endif csdebug}
                    end;
{$endif RefsHaveIndexReg}
              End;

{ now, remove the destination register of the load from the                 }
{ RegsLoadedForReg, since if it's loaded with a new value, it certainly     }
{ will still be used later on                                               }
            If Not(RegMaxSize(PInstr(NewP)^.oper[LoadDst].reg) In
                [ProcInfo.FramePointer,R_NO,STACK_POINTER_REG])
              Then
                Begin
                  RegsLoadedForRef := RegsLoadedForRef -
                    [RegMaxSize(PInstr(NewP)^.oper[LoadDst].reg)];
{$ifdef csdebug}
                  Writeln(std_reg2str[RegMaxSize(PInstr(NewP)^.oper[1].reg)], ' removed');
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


Function TRegInfo.CheckSequence(p: Pai; Reg: TRegister; Var Found: Longint):
           Boolean;
{checks whether the current instruction sequence (starting with p) and the
 one between StartMod and EndMod of Reg are the same. If so, the number of
 instructions that match is stored in Found and true is returned, otherwise
 Found holds the number of instructions between StartMod and EndMod and false
 is returned}

{ note: the NrOfMods field can hold two deifferent values depending on      }
{ which instruction it belongs to:                                          }
{   * if it is the first instruction of a sequence that describes the       }
{     contents of a register, NrOfMods contains how many instructions are   }
{      in the sequence                                                      }
{   * otherwise, NrOfMods contains how many instructions are in the         }
{     describing the contents of the register after the current instruction }
{     has been executed                                                     }

Var oldp, newp: Pai;
    PrevNonRemovablePai: Pai;
    OrgRegInfo, HighRegInfo: PRegInfo;
    HighFound, OrgRegFound: Byte;
    RegCounter: TRegister;
    OrgRegResult: Boolean;
    TmpResult: Boolean;
    OldNrOfMods: Byte;
Begin {CheckSequence}
  Reg := RegMaxSize(Reg);
{ have we found a sequence of instructions equivalent to the new one? }
  TmpResult := False;
{ HighRegInfo will contain the RegInfo for the longest sequence of matching }
{ instructions found                                                        }
  New(HighRegInfo, Init);
{ how many instructions are in the sequence describing the content of Reg }
{ (the parameter) in the old sequence                                     }
  OrgRegFound := 0;
{ how many instructions are in the longest sequence of matching }
{ instructions found until now?                                 }
  HighFound := 0;
{ does the content of Reg in the old equence match the content of Reg in }
{ the new sequence                                                       }
  OrgRegResult := False;
  RegCounter := LoGPReg;
{ PrevNonRemovablePai's OptInfo contains the contents of the registers   }
{ before the current instruction is executed. It will be used to compare }
{ the new contents with and to see whether the new instructions can be   }
{ removed                                                                }
  GetLastInstruction(p, PrevNonRemovablePai);
{ don't check registers that only contain a constant or something unknown }
  While (RegCounter <= HiGPReg And
        (PPaiProp(PrevNonRemovablePai^.OptInfo)^.Regs[RegCounter].Typ <> Con_Ref) Do
    Inc(RegCounter);
  While (RegCounter <= HiGPReg) Do
    Begin
      { reinitialize the reginfo fields }
      Init;
      { no matching instructions found yet }
      Found := 0;
      With PPaiProp(PrevNonRemovablePai^.OptInfo)^.Regs[RegCounter] Do
        Begin
          { get the first instruction that describes the content of the }
          { the register we're going to check the way it was before the }
          { current instruction got executed                            }
          oldp := StartMod;
          { how many instructions describe the content of the register }
          { before the current instructions got executed?              }
          OldNrOfMods := NrOfMods
        End;
      { p is the first instruction that describes the content of Reg }
      { after p (= the current instruction) got executed             }
      newp := p;
      { it's possible that the old contents of the current register are   }
      { described by a sequence of instructions that also contains the    }
      { one in parameter p. In that case, we have to compare until we     }
      { encounter p. Otherwise, compare as much instructions as there are }
      { in the old sequence or until there's a mismatch                   }
      While  (p <> oldp) And
             (Found < OldNrOfMods) And
                                  { old  new }
             InstructionsEquivalent(oldp, newp, RegInfo) Do
        Begin
          GetNextInstruction(oldp, oldp);
          GetNextInstruction(newp, newp);
          Inc(Found)
        End;
      If (Found < OldNrOfMods) Then
        Begin
          { the old sequence was longer than than the new one, so no match }
          TmpResult := False;
          { If there is no match, we have to set the CanBeRemoved flag of   }
          { all pai objects part of the new sequence to false, because it's }
          { possible that some of them have already been scheduled for      }
          { removal after checking another sequence (an instruction can be  }
          { of more than one sequence). If we return false, the number      }
          { returned in found denotes how many instructions have to have    }
          { their CanBeRemoved flag set to false                            }
          { We only have to set those flags to false if their was a partial }
          { match of instructions (found > 0), because otherwise they can't }
          { have been set to true in a previous comparison                  }
          If (found > 0) Then
            Found := PPaiProp(Pai(p)^.OptInfo)^.Regs[Reg].NrOfMods
        End
      Else TmpResult := True;
      If (RegCounter = Reg) Then
        Begin
          OrgRegFound := Found;
          OrgRegResult := TmpResult;
          New(OrgRegInfo, InitWithValue(RegInfo));
        End
      Else
        If TmpResult And
           (Found > HighFound) Then
          Begin
            HighFound := Found;
            HighRegInfo^.InitWithValue(RegInfo);
          End;
      RegInfo.Done;
      Repeat
        Inc(RegCounter);
      Until (RegCounter > HiGPReg) or
            (PPaiProp(PrevNonRemovablePai^.OptInfo)^.Regs[RegCounter].Typ =
              Con_Ref);
    End;
  If (HighFound > 0) And
     (Not(OrgRegResult) Or
      (HighFound > OrgRegFound)) Then
    Begin
      CheckSequence := True;
      Found := HighFound
      InitWithValue(HighRegInfo);
    End
  Else
    Begin
      CheckSequence := OrgRegResult;
      Found := OrgRegFound;
      InitWithValue(OrgRegInfo);
    End;
    Dispose(HighRegInfo, Done);
    Dispose(OrgRegInfo, Done)
End; {CheckSequence}


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

Procedure TAoptCSE.RestoreContents(Current: Pai; Reg: TRegister);
Var Prev, hp3, hp5: Pai;
    TmpState: TStateInt;
    Cnt, Cnt2: Byte;
Begin
{ load Cnt2 with the total number of instructions of this sequence }
  Cnt2 := PPaiProp(Prev^.OptInfo)^.Regs[RegInfo.New2OldReg[reg]].
    NrOfMods;
{ sometimes, a register can not be removed from a sequence, because it's }
{ still used afterwards:                                                 }
{                                                                        }
{ movl    -8(%ebp), %eax                        movl    -8(%ebp), %eax   }
{ movl    70(%eax), %eax                        movl    70(%eax), %eax   }
{ cmpl    74(%eax), %eax                        cmpl    74(%eax), %eax   }
{ jne     l1               can't be changed to  jne     l1               }
{ movl    -8(%ebp), %eax                                                 }
{ movl    70(%eax), %edi                        movl    %eax, %edi       }
{ boundl  R_282, %edi                           boundl  R_282, %edi      }
{ pushl   70(%eax)                              pushl   70(%eax)         }
{                                                                        }
{ because eax now contains the wrong value when 70(%eax) is pushed       }

{ start at the first instruction of the sequence }
  hp3 := Current;
  For Cnt := 1 to Pred(Cnt2) Do
    GetNextInstruction(hp3, hp3);
{ hp3 now containts the last instruction of the sequence }
{ get the writestate at this point of the register in TmpState }
  TmpState := PPaiProp(hp3^.OptInfo)^.GetWState(reg);
{ hp3 := first instruction after the sequence }
  GetNextInstruction(hp3, hp3);

{ now, even though reg is in RegsLoadedForRef, sometimes it's still used  }
{ afterwards. It is not if either it is not in usedregs anymore after the }
{ sequence, or if it is loaded with a new value right after the sequence  }
  If (TmpState <> PPaiProp(hp3^.OptInfo)^.Regs[reg].WState) Or
     Not(reg in PPaiProp(hp3^.OptInfo)^.UsedRegs) Then
{ the register is not used anymore after the sequence! }
    Begin
{$ifdef csdebug}
      Writeln('Cnt2: ',Cnt2);
      hp5 := new(pai_asm_comment,init(strpnew('starting here...')));
      InsertLLItem(Pai(Current^.previous), Current, hp5);
{$endif csdebug}
      hp3 := Current;
{ first change the contents of the register inside the sequence }
      For Cnt := 1 to Cnt2 Do
        Begin
 {save the WState of the last pai object of the sequence for later use}
          TmpState := PPaiProp(hp3^.OptInfo)^.Regs[reg].WState;
{$ifdef csdebug}
          hp5 := new(pai_asm_comment,init(strpnew('WState for '+
            std_reg2str[reg]+': '+tostr(tmpstate))));
          InsertLLItem(hp3, pai(hp3^.next), hp5);
{$endif csdebug}
          PPaiProp(hp3^.OptInfo)^.Regs[reg] :=
            PPaiProp(Prev^.OptInfo)^.Regs[reg];
          GetNextInstruction(hp3, hp3);
        End;
{ here, hp3 = p = Pai object right after the sequence, TmpState = WState of }
{ reg at the last Pai object of the sequence                                }
      GetLastInstruction(hp3, hp3);
{ now, as long as the register isn't modified after the sequence, set its }
{ contents to what they were before the sequence                          }
      While GetNextInstruction(hp3, hp3) And
            (PPaiProp(hp3^.OptInfo)^.GetWState(Reg) = TmpState) Do
{$ifdef csdebug}
        begin
          hp5 := new(pai_asm_comment,init(strpnew('WState for '+std_reg2str[reg]+': '+
                 tostr(PPaiProp(hp3^.OptInfo)^.GetWState(reg)))));
             InsertLLItem(hp3, pai(hp3^.next), hp5);
{$endif csdebug}
          PPaiProp(hp3^.OptInfo)^.Regs[reg] :=
            PPaiProp(Prev^.OptInfo)^.Regs[reg];
{$ifdef csdebug}
        end;
{$endif csdebug}
    End
  Else
{ the register is still used after the sequence, so undelete all }
{ instructions in the sequence that modify reg                   }
    Begin
{$ifdef csdebug}
      Writeln('Got there for ',std_reg2str[reg]);
{$endif csdebug}
      hp3 := Current;
      For Cnt := 1 to Cnt2 Do
        Begin
          If RegModifiedByInstruction(reg, hp3) Then
            PPaiProp(hp3^.OptInfo)^.CanBeRemoved := False;
          GetNextInstruction(hp3, hp3);
        End;
    End;
{$ifdef csdebug}
  hp5 := new(pai_asm_comment,init(strpnew('stopping here...')));
  InsertLLItem(AsmL, hp3, pai(hp3^.next), hp5);
{$endif csdebug}
End;

Procedure TAoptCSE.DoCSE;
{marks the instructions that can be removed by RemoveInstructs. They're not
 removed immediately because sometimes an instruction needs to be checked in
 two different sequences}
Var Cnt, Cnt2: Longint;
    p, hp1, Current: Pai;
    hp3, Prev: Pai;
{$ifdef csdebug}
    hp5: pai;
{$endif csdebug}
    RegInfo: TRegInfo;
    RegCounter: TRegister;
    TmpState: Byte;
Begin
  p := SkipHead(BlockStart);
  While (p <> BlockEnd) Do
    Begin
      Case p^.typ Of
        ait_instruction:
          Begin
{            Case PInstr(p)^.opcode Of
              A_CLD: If GetLastInstruction(p, hp1) And
                        (PPaiProp(hp1^.OptInfo)^.DirFlag = F_NotSet) Then
                       PPaiProp(Pai(p)^.OptInfo)^.CanBeRemoved := True;}
              If IsLoadMemReg(p) Then
                Begin
                  If (p = PPaiProp(p^.OptInfo)^.Regs[RegMaxSize(
                       PInstr(p)^.oper[LoadDst].reg)].StartMod) And
                     GetLastInstruction (p, hp1) And
                     (hp1^.typ <> ait_marker) Then
{so we don't try to check a sequence when p is the first instruction of the block}
                    If CheckSequence(p, PInstr(p)^.oper[LoadDst].reg, Cnt) And
                       (Cnt > 0) Then
                      Begin
                        hp1 := nil;
{ although it's perfectly ok to remove an instruction which doesn't contain }
{ the register that we've just checked (CheckSequence takes care of that),  }
{   the sequence containing this other register should also be completely   }
{   checked (and either removed or marked as non-removable), otherwise we   }
{ may get situations like this:                                             }
{                                                                           }
{     movl 12(%ebp), %edx                       movl 12(%ebp), %edx         }
{     movl 16(%ebp), %eax                       movl 16(%ebp), %eax         }
{     movl 8(%edx), %edx                        movl 8(%edx), %edx          }
{     movl (%eax), eax                          movl (%eax), eax            }
{     cmpl %eax, %edx                           cmpl %eax, %edx             }
{     jnz  l123           getting converted to  jnz  l123                   }
{     movl 12(%ebp), %edx                       movl 4(%eax), eax           }
{     movl 16(%ebp), %eax                                                   }
{     movl 8(%edx), %edx                                                    }
{     movl 4(%eax), eax                                                     }
                        Current := p;
                        Cnt2 := 1;
{ after this while loop, if hp1 <> nil it will contain the pai object }
{ that's the start of a sequence that's not completely checked yet    }
                        While Cnt2 <= Cnt Do
                          Begin
                            If (hp1 = nil) And
                               Not(RegInInstruction(
                                     PInstr(Current)^.oper[LoadDst].reg,p) Or
                                   RegInInstruction(RegMaxSize(PInstr(
                                     Current)^.oper[LoadDst].reg), p)) And
{ do not recheck a sequence if it's completely part of the one we just }
{ checked                                                              }
                               Not(IsLoadMemReg(p) And
                                   (PPaiProp(p^.OptInfo)^.Regs[RegMaxSize(
                                      PInstr(p)^.Oper[LoadDst].reg)]
                                      .NrOfMods <= (Cnt - Cnt2 + 1))) Then
                              hp1 := p;
{$ifndef noremove}
                            PPaiProp(p^.OptInfo)^.CanBeRemoved := True;
{$endif noremove}
                            Inc(Cnt2);
                            GetNextInstruction(p, p);
                          End;
{ insert a marker noting that for the following instructions no PPaiProp's }
{ (containing optimizer info) have been generated, so GetNext/             }
{ LastInstruction will ignore them (it will use the original instructions) }
                        hp3 := New(Pai_Marker,Init(mark_NoPropInfoStart));
                        InsertLLItem(Pai(Current^.Previous), Current, hp3);
{ Prev is used to get the contents of the registers before the sequence }
                        GetLastInstruction(Current, Prev);
{ If some registers were different in the old and the new sequence, move }
{  the contents of those old registers to the new ones, e.g.             }
{                                                                        }
{   mov mem1, reg1                        mov mem1, reg1                 }
{   ...               can be changed to   ...                            }
{   mov mem1, reg2                        mov reg1, reg2                 }

{$IfDef CSDebug}
                        For RegCounter := LoGPReg To HiGPReg Do
                          If (RegCounter in RegInfo.RegsLoadedForRef) Then
                            Begin
                              hp5 := new(pai_asm_comment,init(strpnew(
                                'New: '+std_reg2str[RegCounter]+', Old: '+
                                std_reg2str[RegInfo.New2OldReg[RegCounter]])));
                              InsertLLItem(AsmL, Pai(Current^.previous), Current, hp5);
                            End;
{$EndIf CSDebug}
                        For RegCounter := LoGPReg to HiGPReg Do
                          Begin
{ if New2OldReg[RegCounter] = R_NO, it means this register doesn't appear }
{ the new nor the old sequence                                            }
                            If (RegInfo.New2OldReg[RegCounter] <> R_NO) Then
{ if a register is in RegsLoadedForRef, it means this register was loaded }
{ with a value only to function as a base or index in a reference. The    }
{ practical upshot of this is that this value won't be used anymore later }
{ on, so even if another register was used in the new sequence for this,  }
{ we don't have to load it. E.g.                                          }
{                                                                         }
{ movl 8(%ebp), %eax                        "                             }
{ movl 4(%eax), %eax                        "                             }
{ movl (%eax), %edi                         "                             }
{ movl %edi, 12(%ebp)                       "                             }
{ ...                   can be changed to   "                             }
{ movl 8(%ebp), %edx                                                      }
{ movl 4(%edx), %edx                                                      }
{ movl (%edx), %ebx                         movl %edi, %ebx               }
{                                                                         }
{ There is no need to also add a "movl %eax, %edx"                        }
                              If Not(RegCounter In RegInfo.RegsLoadedForRef) And
                                             {old reg              new reg}
{ no need to reload the register if it's the same in the old and new }
{ sequence                                                           }
                                 (RegInfo.New2OldReg[RegCounter] <> RegCounter) Then

                                Begin
                                  hp3 := a_load_reg_reg(
                                                 {old reg          new reg}
                                    RegInfo.New2OldReg[RegCounter], RegCounter));
                                  InsertLLItem(Pai(Current^.previous), Current, hp3);
                                End
                              Else
{ As noted before, if a register is in RegsLoadedForRef, it doesn't have  }
{ to be loaded. However, when data flow analyzer processed this code, the }
{ was loaded, so we need to change that. This is done by setting the      }
{ contents of the register to its contents before the new sequence, for   }
{ every instruction until the first load of the register with a new value }
                                If (RegCounter In RegInfo.RegsLoadedForRef) Then
                                  RestoreOrigContents(Current, RegCounter);

                          End;
{ the end of the area where instructions without optimizer info can occur }
                        hp3 := New(Pai_Marker,Init(mark_NoPropInfoEnd));
                        InsertLLItem(AsmL, Pai(Current^.Previous), Current, hp3);
{ if we found an instruction sequence that needs complete re-evaluation, }
{ process it                                                             }
                        If hp1 <> nil Then p := hp1;
                        Continue;
                      End
                    Else
{ checksequence returned false. In that case, if the current instruction }
{ was already deleted (as part of another sequence), we have to undelete }
{ all instructions pertaining to the register whose sequence we just     }
{ checked                                                                }
                      If (Cnt > 0) And
                         (PPaiProp(p^.OptInfo)^. Regs[RegMaxSize(PInstr(p)^.
                            oper[LoadDst].reg)].Typ = Con_Ref) And
                         (PPaiProp(p^.OptInfo)^.CanBeRemoved) Then
                        Begin
                          Current := p;
                          Cnt2 := 1;
                          While Cnt2 <= Cnt Do
                            Begin
                              If RegInInstruction(PInstr(Current)^.
                                   oper[LoadDst].reg, p) Or
                                 RegInInstruction(RegMaxSize(PInstr(Current)^.
                                   oper[LoadDst].reg), p) Then
                                PPaiProp(p^.OptInfo)^.CanBeRemoved := False;
                              Inc(Cnt2);
                              GetNextInstruction(p, p);
                            End;
                          Continue;
                        End;
                End
              Else if IsLoadConstReg(p) Then
                Begin
                  If GetLastInstruction(p, hp1) Then
                    With PPaiProp(hp1^.OptInfo)^.Regs[
                           RegMaxSize(PInstr(p)^.oper[LoadDst].reg)] Do
                      If (Typ = Con_Const) And
                         (StartMod = p) Then
                        PPaiProp(p^.OptInfo)^.CanBeRemoved := True;
                End
              Else
                CpuCSE(p);
{              A_STD: If GetLastInstruction(p, hp1) And
                        (PPaiProp(hp1^.OptInfo)^.DirFlag = F_Set) Then
                        PPaiProp(Pai(p)^.OptInfo)^.CanBeRemoved := True;
              A_XOR:
                Begin
                  If (Paicpu(p)^.oper[0].typ = top_reg) And
                     (Paicpu(p)^.oper[0].typ = top_reg) And
                     (Paicpu(p)^.oper[1].reg = Paicpu(p)^.oper[1].reg) And
                     GetLastInstruction(p, hp1) And
                     (PPaiProp(hp1^.OptInfo)^.Regs[Reg32(Paicpu(p)^.oper[1].reg)].typ = con_const) And
                     (PPaiProp(hp1^.OptInfo)^.Regs[Reg32(Paicpu(p)^.oper[1].reg)].StartMod = nil)
                    Then PPaiProp(p^.OptInfo)^.CanBeRemoved := True
                End
          End;
      End;
      GetNextInstruction(p, p);
    End;
End;

Procedure RemoveInstructs;
{Removes the marked instructions and disposes the PPaiProps of the other
 instructions, restoring their line number}
Var p, hp1: Pai;
    InstrCnt: Longint;
Begin
 p := SkipHead(BlockStart);
  InstrCnt := 1;
  While (p <> BlockEnd) Do
    Begin
{$ifndef noinstremove}
      If PPaiProp(p^.OptInfo)^.CanBeRemoved
        Then
          Begin
            Dispose(PPaiProp(p^.OptInfo));
            GetNextInstruction(p, hp1);
            AsmL^.Remove(p);
            Dispose(p, Done);
            p := hp1;
            Inc(InstrCnt);
          End
        Else
{$endif noinstremove}
          Begin
            Dispose(PPaiProp(p^.OptInfo));
            p^.OptInfo := nil;
            GetNextInstruction(p, p);
            Inc(InstrCnt);
          End;
    End;
End;

Procedure TAoptCSE.CSE;
Begin
  DoCSE;
  RemoveInstructs;
End;



End.
