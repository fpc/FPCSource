{
    $Id$
    Copyright (c) 1999 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit contains the processor independent assembler optimizer
    object.

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
Unit AoptObj;

{ general, processor independent objects for use by the assembler optimizer }

Interface

uses Cobjects, cpubase, aoptcpub;

{***************** Constants *****************}

Const

{ ait_* types which don't result in executable code or which don't influence }
{ the way the program runs/behaves                                           }

  SkipInstr = [ait_comment, ait_align, ait_symbol
{$ifdef GDB}
  ,ait_stabs, ait_stabn, ait_stab_function_name
{$endif GDB}
  ,ait_regalloc, ait_tempalloc
  ];

{Possible register content types}
  con_Unknown = 0;
  con_ref = 1;
  con_const = 2;

{***************** Types ****************}

Type

  TRegArray = Array[LoReg..HiReg] of TRegister;
  TRegSet = Set of LoReg..HiReg;
  PRegInfo = ^TReginfo;

 { info about the equivalence of registers when comparing two code sequences}
  TRegInfo = Object
                { registers encountered in the new and old sequence }
                NewRegsEncountered, OldRegsEncountered,
                { registers which only have been loaded for use as base or }
                { index in a reference later on                            }
                RegsLoadedForRef: TRegSet;
                { to which register in the old sequence corresponds every }
                { register in the new sequence                            }
                New2OldReg: TRegArray;

                Constructor init;
                { clear all information store in the object }
                Procedure Clear;
                { the contents of OldReg in the old sequence are now being }
                { loaded into NewReg in the new sequence                   }
                Procedure AddReg(OldReg, NewReg: TRegister); Virtual;
                { the contents of OldOp in the old sequence are now being   }
                { loaded into NewOp in the new sequence. It is assumed that }
                { OldOp and NewOp are equivalent                            }
                Procedure AddOp(const OldOp, NewOp:Toper);
                { check if a register in the old sequence (OldReg) can be  }
                { equivalent to a register in the new sequence (NewReg) if }
                { the operation OpAct is performed on it. The RegInfo is   }
                { updated (not necessary to call AddReg afterwards         }
                Function RegsEquivalent(OldReg, NewReg: TRegister;
                           OpAct: TopAction): Boolean;
                { check if a reference in the old sequence (OldRef) can be  }
                { equivalent to a reference in the new sequence (NewRef) if }
                { the operation OpAct is performed on it. The RegInfo is    }
                { updated (not necessary to call AddOp afterwards           }
                Function RefsEquivalent(Const OldRef, NewRef: TReference;
                           OpAct: TOpAction): Boolean;
                { check if an operand in the old sequence (OldOp) can be  }
                { equivalent to an operand in the new sequence (NewOp) if }
                { the operation OpAct is performed on it. The RegInfo is  }
                { updated (not necessary to call AddOp afterwards         }
                Function OpsEquivalent(const OldOp, NewOp: toper;
                           OpAct: TopAction): Boolean;
                { check if an instruction in the old sequence (OldP) can be }
                { equivalent to an instruction in the new sequence (Newp)   }
                { The RegInfo is  updated                                   }
                Function InstructionsEquivalent(OldP, NewP: Pai): Boolean;
              End;


{ possible actions on an operand: read, write or modify (= read & write) }
  TOpAction = (OpAct_Read, OpAct_Write, OpAct_Modify, OpAct_Unknown);

{ the properties of a cpu instruction }
  TAsmInstrucProp = Record
                      { what it changes }
                      Ch: Array[1..MaxCh] of TChange;
                    End;

{ Object to hold on information on which regiters are in use and which not }
  TUsedRegs = Object
                Constructor init;
                Constructor InitWithValue(_RegSet: TRegSet);
                { update the info with the pairegalloc objects coming after }
                { p                                                         }
                Procedure Update(p: Pai);
                { is Reg currently in use }
                Function IsUsed(Reg: TRegister): Boolean;
                { get all the currently used registers }
                Function GetUsedRegs: TRegSet;
                Destructor Done;

                Private

                UsedRegs: TRegSet;
              End;

 { size of the integer that holds the state number of a register. Can be any }
 { integer type, so it can be changed to reduce the size of the TContent     }
 { structure or to improve alignment                                         }
  TStateInt = Byte;

  TContent = Packed Record
               { start and end of block instructions that defines the }
               { content of this register. If Typ = con_const, then   }
               { Longint(StartMod) = value of the constant)           }                               }
               StartMod: pai;
               { starts at 0, gets increased everytime the register is }
               { written to                                            }
               WState: TStateInt;
               { starts at 0, gets increased everytime the register is read }
               { from                                                       }
               RState: TStateInt;
               { how many instructions starting with StarMod does the block }
               { consist of                                                 }
               NrOfMods: Byte;
               { the type of the content of the register: unknown, memory   }
               { (variable) or constant                                     }
               Typ: Byte;
             End;

{ Contents of the integer registers }

  TRegContent = Array[LoGPReg..HiGPReg] Of TContent;

  PPaiProp = ^TPaiProp;

{ information object with the contents of every register. Every Pai object   }
{ gets one of these assigned: a pointer to it is stored in the OptInfo field }
  TPaiProp = Object
               Regs: TRegContent;
               { info about allocation of general purpose integer registers }
               UsedRegs: TUsedRegs;
               { info about the conditional registers }
               CondRegs: TCondRegs;
               { can this instruction be removed? }
               CanBeRemoved: Boolean;

               Constructor init;

               { destroy the contents of a register, as well as those whose }
               { contents are based on those of that register               }
               Procedure DestroyReg(Reg: TRegister; var NrOfInstrSinceLastMod:
                                      TInstrSinceLastMod);
               { if the contents of WhichReg (can be R_NO in case of a    }
               { constant) are written to memory at the location Ref, the }
               { contents of the registers that depend on Ref have to be  }
               { destroyed                                                }
               Procedure DestroyRefs(Const Ref: TReference; WhichReg: TRegister);
               { an instruction reads from operand o }
               Procedure ReadOp(const o:toper);
               { an instruction reads from reference Ref }
               Procedure ReadRef(Ref: PReference);
               { an instruction reads from register Reg }
               Procedure ReadReg(Reg: TRegister);
               { an instruction writes/modifies operand o and this has      }
               { special side-effects or modifies the contents in such a    }
               { way that we can't simply add this instruction to the       }
               { sequence of instructions that describe the contents of the }
               { operand, so destroy it                                     }
               Procedure DestroyOp(const o:Toper);
               { destroy the contetns of all registers }
               Procedure DestroyAllRegs;
{$ifdef arithopt}
               { a register's contents are modified, but not destroyed }
               { (the new value depends on the old one)                }
               Procedure ModifyReg(reg: TRegister);
               { an operand's contents are modified, but not destroyed }
               { (the new value depends on the old one)                }
               Procedure ModifyOp(const oper: TOper);
{$endif arithopt}

               { increase the write state of a register (call every time a }
               { register is written to)                                   }
               Procedure IncWState(Reg: TRegister);
               { increase the read state of a register (call every time a }
               { register is read from                                    }
               Procedure IncRState(Reg: TRegister);
               { get the write state of a register }
               Function GetWState(Reg: TRegister): TStateInt;
               { get the read state of a register }
               Function GetRState(Reg: TRegister): TStateInt;

               { get the type of contents of a register }
               Function GetRegContentKind(Reg: TRegister): Byte;

               Destructor Done;

               Private

               Procedure IncState(Reg: TRegister);

             End;

  { the number of instructions that we have encountered since the last }
  { modification of a register                                         }
  TInstrSinceLastMod = Array[LoGPReg..HiGPReg] Of Byte;

  TLabelTableItem = Record
                      PaiObj: Pai;
{$IfDef JumpAnal}
                      InstrNr: Longint;
                      RefsFound: Word;
                      JmpsProcessed: Word
{$EndIf JumpAnal}
                    End;

  TLabelTable = Array[0..2500000] Of TLabelTableItem;
  PLabelTable = ^TLabelTable;
  TLabelInfo = Record
    { the highest and lowest label number occurring in the current code }
    { fragment                                                          }
    LowLabel, HighLabel: AWord;
    LabelDif: AWord;
    { table that contains the addresses of the Pai_Label objects associated }
    { with each label number                                                }
    LabelTable: PLableTable;
  End;


{***** General optimizer object, used to derive others from *****}

Type TAOptObj = Object
       { the PAasmOutput list this optimizer instance works on }
       AsmL: PAasmOutput;

       { The labelinfo record contains the addresses of the Pai objects }
       { that are labels, how many labels there are and the min and max }
       { label numbers                                                  }
       LabelInfo: PLabelInfo;

       { Start and end of the block that is currently being optimized }
       BlockStart, BlockEnd: Pai;

       { _AsmL is the PAasmOutpout list that has to be optimized,     }
       { _BlockStart and _BlockEnd the start and the end of the block }
       { that has to be optimized and _LabelInfo a pointer to a       }
       { TLabelInfo record                                            }
       Constructor Init(_AsmL: PAasmOutput; _BlockStart, _BlockEnd: Pai;
                          _LabelInfo: PLabelInfo);

       { processor independent methods }

       { returns true if the label L is found between hp and the next }
       { instruction                                                  }
       Function FindLabel(L: PasmLabel; Var hp: Pai): Boolean;

       { inserts new_one between prev and foll in AsmL }
       Procedure InsertLLItem(AsmL: PAasmOutput; prev, foll, new_one:
                   PLinkedList_Item);

       { returns true if register Reg is used by instruction p1 }
       Function RegInInstruction(Reg: TRegister; p1: Pai): Boolean;
       { returns true if register Reg is used in the reference Ref }
       Function RegInRef(Reg: TRegister; Const Ref: TReference): Boolean;
       { returns whether the reference Ref is used somewhere in the loading }
       { sequence Constent                                                  }
       Function TAOptObj.RefInSequence(Const Ref: TReference;
                                         Content: TContent): Boolean;
       { returns whether the instruction P reads from and/or writes to Reg }
       Function TAOptObj.RefInInstruction(Const Ref: TReference; p: Pai): Boolean;
       { returns true if the instruction p1 modifies the register Reg }
       Function RegModifiedByInstruction(Reg: TRegister; p1: Pai): Boolean;

       { gets the next Pai object after current that contains info relevant }
       { to the optimizer in p1. If there is none, it returns false and     }
       { sets p1 to nil                                                     }
       Function GetNextInstruction(Current: Pai; Var Next: Pai): Boolean;
       { gets the previous Pai object after current that contains info  }
       { relevant to the optimizer in last. If there is none, it retuns }
       { false and sets last to nil                                     }
       Function GetLastInstruction(Current: Pai; Var Last: Pai): Boolean;
       { If P is a Pai object releveant to the optimizer, it is left  }
       { unchanged. If it is not relevant tot he optimizer, the first }
       { object after P that is relevant is stored in P               }
       Procedure SkipHead(var P: Pai);

       { returns true if the operands o1 and o2 are completely equal }
       Function OpsEqual(const o1,o2:toper): Boolean;

       { Returns true if a ait_alloc object for Reg is found in the block }
       { of Pai's starting with StartPai and ending with the next "real"  }
       { instruction                                                      }
       Function FindRegAlloc(Reg: TRegister; StartPai: Pai): Boolean;

       { processor dependent methods }

       { returns the maximum width component of Reg. Only has to be }
       { overridden for the 80x86 (afaik)                           }
       Function RegMaxSize(Reg: TRegister): TRegister; Virtual;
       { returns true if Reg1 and Reg2 are of the samae width. Only has to }
       { overridden for the 80x86 (afaik)                                  }
       Function RegsSameSize(Reg1, Reg2: TRegister): Boolean; Virtual;
       { returns whether P is a load instruction (load contents from a }
       { memory location or (register) variable into a register)       }
       Function IsLoadInstr(p: pai): Boolean; Virtual;
       { returns whether P is a store instruction (store contents from a }
       { register to a memory location or to a (register) variable)      }
       Function IsStoreInstr(p: pai): Boolean; Virtual;
       { returns whether the instruction P reads from register Reg }
       Function RegReadByInstr(Reg: TRegister; p: Pai); Virtual;
       { convert a TChange value into the corresponding register }
       Function TCh2Reg(Ch: TChange): TRegister; Virtual;
     End;

{***************** Implementation *****************}

Implementation

{*********************** TReginfo ***********************}

Constructor TRegInfo.Init;
Begin
  Clear;
End;

Constructor TRegInfo.Init;
Begin
  RegsLoadedForRef   := [];
  NewRegsEncountered := [ProcInfo.FramePointer, R_ESP];
  OldRegsEncountered := [ProcInfo.FramePointer, R_ESP];
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
{here we always check for the 32 bit component, because it is possible that
 the 8 bit component has not been set, event though NewReg already has been
 processed. This happens if it has been compared with a register that doesn't
 have an 8 bit component (such as EDI). In that case the 8 bit component is
 still set to R_NO and the comparison in the Else-part will fail}
      If (RegMaxSize(OldReg) in OldRegsEncountered) Then
        If (RegMaxSize(NewReg) in NewRegsEncountered) Then
          RegsEquivalent := (OldReg = New2OldReg[NewReg])

 { If we haven't encountered the new register yet, but we have encountered the
   old one already, the new one can only be correct if it's being written to
   (and consequently the old one is also being written to), otherwise

   movl -8(%ebp), %eax        and         movl -8(%ebp), %eax
   movl (%eax), %eax                      movl (%edx), %edx

   are considered equivalent}

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
{$ifdef RefsHaveindex}
                      And RegsEquivalent(OldRef.Index, NewRef.Index, OpAct)
{$endif RefsHaveIndex}
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
Begin {checks whether the two ops are equivalent}
  OpsEquivalent := False;
  if OldOp.typ=NewOp.typ then
    Case OldOp.typ Of
      Top_Const: OpsEquivalent := OldOp.value = NewOp.value;
      Top_Reg:
        OpsEquivalent := RegsEquivalent(OldOp.reg,NewOp.reg, OpAct);
      Top_Ref:
        OpsEquivalent := RefsEquivalent(OldOp.ref^, NewOp.ref^, OpAct);
      Top_None:
        OpsEquivalent := True
    End;
End;

Function TRegInfo.InstructionsEquivalent(OldP, NewP: Pai): Boolean;
{ checks whether two PInstr instructions are equal                     }
Var Count: TNatInt;
    TmpResult: Boolean;
Begin
  If Assigned(OldP) And Assigned(NewP) And
     (Pai(OldP)^.typ = ait_instruction) And
     (Pai(NewP)^.typ = ait_instruction) And
     (PInstr(OldP)^.opcode = PInstr(NewP)^.opcode) And
     (PInstr(OldP)^.oper[0].typ = PInstr(NewP)^.oper[0].typ) And
     (PInstr(OldP)^.oper[1].typ = PInstr(NewP)^.oper[1].typ) And
     (PInstr(OldP)^.oper[2].typ = PInstr(NewP)^.oper[2].typ)
    Then
{ both instructions have the same structure:                                }
{ "<operator> <operand of type1>, <operand of type 2>"                      }
      If IsLoadInstr(OldP) {then also NewP = loadinstr} Then
        If Not(RegInRef(PInstr(OldP)^.oper[LoadDst].reg,
                 PInstr(OldP)^.oper[LoadSrc].ref^)) Then
{ the "old" instruction is a load of a register with a new value, not with  }
{ a value based on the contents of this register (so no "mov (reg), reg")   }
          If Not(RegInRef(PInstr(NewP)^.oper[LoadDst].reg,
                          PInstr(NewP)^.oper[LoadSrc].ref^)) And
             RefsEqual(PInstr(OldP)^.oper[LoadSrc].ref^,
                       PInstr(NewP)^.oper[LoadSrc].ref^)
            Then
{ the "new" instruction is also a load of a register with a new value, and  }
{ this value is fetched from the same memory location                       }
              Begin
                With PInstr(NewP)^.oper[LoadSrc].ref^ Do
                  Begin
                    If Not(Base in [ProcInfo.FramePointer, R_NO, StackPtr])
{ it won't do any harm if the register is already in RegsLoadedForRef       }
                      Then RegsLoadedForRef := RegsLoadedForRef + [Base];
{$ifdef RefsHaveIndex}
                    If Not(Index in [ProcInfo.FramePointer, R_NO, R_StackPtr])
                      Then RegsLoadedForRef := RegsLoadedForRef + [Index];
{$endif RefsHaveIndex}
                  End;
{ add the registers from the reference (.oper[Src]) to the RegInfo, all     }
{ registers from the reference are the same in the old and in the new       }
{ instruction sequence                                                      }
                AddOp(PInstr(OldP)^.oper[Src], PInstr(OldP)^.oper[Src]);
{ the registers from .oper[Dest] have to be equivalent, but not necessarily }
{ equal                                                                     }
                InstructionsEquivalent :=
                  RegsEquivalent(PInstr(OldP)^.oper[Dest].reg,
                                 PInstr(NewP)^.oper[Dest].reg, OpAct_Write);
              End
{ the registers are loaded with values from different memory locations. If  }
{ this were allowed, the instructions "mov -4(%esi),%eax" and               }
{  "mov -4(%ebp),%eax" would be considered equivalent                       }
            Else InstructionsEquivalent := False
        Else
{ load register with a value based on the current value of this register    }
          Begin
            With PInstr(NewP)^.oper[0].ref^ Do
{ Assume the registers occurring in the reference have only been loaded with }
{ the value they contain now to calculate an address (so the value they have }
{ now, won't be stored to memory later on)}
              Begin
                If Not(Base in [ProcInfo.FramePointer,
                                RegMaxSize(PInstr(NewP)^.oper[LoadDst].reg),
                                R_NO,StackPtr])
{ It won't do any harm if the register is already in RegsLoadedForRef        }
                  Then
                    Begin
                      RegsLoadedForRef := RegsLoadedForRef + [Base];
{$ifdef csdebug}
                      Writeln(att_reg2str[base], ' added');
{$endif csdebug}
                    end;
{$Ifdef RefsHaveIndex}
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
{$endif RefsHaveIndex}
              End;

{ now, remove the destination register of the load from the                 }
{ RegsLoadedForReg, since if it's loaded with a new value, it certainly     }
{ will still be used later on                                               }
            If Not(RegMaxSize(PInstr(NewP)^.oper[LoadDst].reg) In
                [ProcInfo.FramePointer,R_NO,StackPtr])
              Then
                Begin
                  RegsLoadedForRef := RegsLoadedForRef -
                    [RegMaxSize(PInstr(NewP)^.oper[LoadDst].reg)];
{$ifdef csdebug}
                  Writeln(att_reg2str[Reg32(Pai386(NewP)^.oper[1].reg)], ' removed');
{$endif csdebug}
                end;
            InstructionsEquivalent :=
               OpsEquivalent(PInstr(OldP)^.oper[LoadSrc],
                             PInstr(NewP)^.oper[LoadSrc], OpAct_Read) And
               OpsEquivalent(PInstr(OldP)^.oper[LoadDst],
                             PInstr(NewP)^.oper[LoadDst], OpAct_Write)
          End
      Else
 {an instruction that's not a load instruction}
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

{*************************** TUsedRegs ***************************}

Constructor TUsedRegs.init;
Begin
  UsedRegs := [];
End;

Constructor TUsedRegisters.InitWithValue(Const _RegSet: TRegSet);
Begin
  RegSet := _RegSet;
End;

Procedure TUsedRegs.Update(p: Pai);
{updates UsedRegs with the RegAlloc Information coming after P}
Begin
  Repeat
    While Assigned(p) And
          ((p^.typ in (SkipInstr - [ait_RegAlloc])) or
           ((p^.typ = ait_label) And
            Not(Pai_Label(p)^.l^.is_used))) Do
         p := Pai(p^.next);
    While Assigned(p) And
          (p^.typ=ait_RegAlloc) Do
      Begin
        if pairegalloc(p)^.allocation then
          UsedRegs := UsedRegs + [PaiRegAlloc(p)^.Reg]
        else
          UsedRegs := UsedRegs - [PaiRegAlloc(p)^.Reg];
        p := pai(p^.next);
      End;
  Until Not(Assigned(p)) Or
        (Not(p^.typ in SkipInstr) And
         Not((p^.typ = ait_label) And
            Not(Pai_Label(p)^.l^.is_used)));
End;

Function TUsedRegs.IsUsed(Reg: TRegister): Boolean;
Begin
  IsUsed := Reg in UsedRegs
End;

Function TUsedRegs.GetUsedRegs: TRegSet;
Begin
  GetUsedRegs := UsedRegs;
End;

{*************************** TPaiProp ***************************}

Constructor TPaiProp.Init;
Begin
  UsedRegs.Init;
  CondRegs.init;
{  DirFlag: TFlagContents; I386 specific}
End;

Procedure TPaiProp.DestroyReg(Reg: TRegister; var NrOfInstrSinceLastMod:
                                                TInstrSinceLastMod);
{ Destroys the contents of the register Reg in the PPaiProp p1, as well as }
{ the contents of registers are loaded with a memory location based on Reg }
Var TmpWState, TmpRState: Byte;
    Counter: TRegister;
Begin
  Reg := Reg32(Reg);
  NrOfInstrSinceLastMod[Reg] := 0;
  If (Reg >= R_EAX) And (Reg <= R_EDI)
    Then
      Begin
        With Regs[Reg] Do
          Begin
            IncState(WState);
            TmpWState := WState;
            TmpRState := RState;
            FillChar(Regs[Reg], SizeOf(TContent), 0);
            WState := TmpWState;
            RState := TmpRState;
          End;
        For Counter := R_EAX to R_EDI Do
          With Regs[Counter] Do
            If (Typ = Con_Ref) And
               RegInSequence(Reg, Regs[Counter])
              Then
                Begin
                  IncState(WState);
                  TmpWState := WState;
                  TmpRState := RState;
                  FillChar(Regs[Counter], SizeOf(TContent), 0);
                  WState := TmpWState;
                  RState := TmpRState;
                End;
       End;
End;

Procedure TPaiProp.DestroyRefs(Const Ref: TReference; WhichReg: TRegister);
{destroys all registers which possibly contain a reference to Ref, WhichReg
 is the register whose contents are being written to memory (if this proc
 is called because of a "mov?? %reg, (mem)" instruction)}
Var Counter: TRegister;
Begin
  WhichReg := RegMaxSize(WhichReg);
  If ((Ref.base = ProcInfo.FramePointer) And
{$ifdef refsHaveIndex}
      (Ref.Index = R_NO)
{$endif refsHaveIndex}
      ) Or
     Assigned(Ref.Symbol)
    Then
{write something to a parameter, a local or global variable, so
   * with uncertain optimizations on:
      - destroy the contents of registers whose contents have somewhere a
        "mov?? (Ref), %reg". WhichReg (this is the register whose contents
        are being written to memory) is not destroyed if it's StartMod is
        of that form and NrOfMods = 1 (so if it holds ref, but is not a
        pointer or value based on Ref)
    * with uncertain optimizations off:
       - also destroy registers that contain any pointer}
      For Counter := LoGPReg to HiGPReg Do
        With Regs[Counter] Do
          Begin
            If (typ = Con_Ref) And
               ((Not(cs_UncertainOpts in aktglobalswitches) And
                 (NrOfMods <> 1)
                ) Or
                (RefInSequence(Ref,Regs[Counter]) And
                 ((Counter <> WhichReg) Or
                  ((NrOfMods <> 1) And
 {StarMod is always of the type ait_instruction}
                   (PInstr(StartMod)^.oper[0].typ = top_ref) And
                   RefsEqual(PInstr(StartMod)^.oper[0].ref^, Ref)
                  )
                 )
                )
               )
              Then
                DestroyReg(Counter)
          End
    Else
{write something to a pointer location, so
   * with uncertain optimzations on:
      - do not destroy registers which contain a local/global variable or a
        parameter, except if DestroyRefs is called because of a "movsl"
   * with uncertain optimzations off:
      - destroy every register which contains a memory location
      }
      For Counter := LoGPReg to HiGPReg Do
        With Regs[Counter] Do
          If (typ = Con_Ref) And
             (Not(cs_UncertainOpts in aktglobalswitches) Or
{$ifdef i386}
        {for movsl}
              (Ref.Base = R_EDI) Or
{$endif}
        {don't destroy if reg contains a parameter, local or global variable}
              Not((NrOfMods = 1) And
                  (PInstr(StartMod)^.oper[0].typ = top_ref) And
                  ((PInstr(StartMod)^.oper[0].ref^.base = ProcInfo.FramePointer) Or
                    Assigned(PInstr(StartMod)^.oper[0].ref^.Symbol)
                  )
                 )
             )
          Then DestroyReg(Counter)
End;

Procedure TPaiProp.DestroyAllRegs;
Var Counter: TRegister;
Begin {initializes/desrtoys all registers}
  For Counter := R_EAX To R_EDI Do
    DestroyReg(Counter);
  CondRegs.Init;
{ FPURegs.Init; }
End;

Procedure TPaiProp.DestroyOp(const o:Toper);
Begin
  Case o.typ Of
    top_reg: DestroyReg(o.reg);
    top_ref: DestroyRefs(o.ref^, R_NO);
    top_symbol:;
  End;
End;

Procedure TPaiProp.ReadReg(Reg: TRegister);
Begin
  Reg := RegMaxSize(Reg);
  If Reg in General_Registers Then
    IncRState(RegMaxSize(Reg))
End;

Procedure TPaiProp.ReadRef(Ref: PReference);
Begin
  If Ref^.Base <> R_NO Then
    ReadReg(Ref^.Base);
  If Ref^.Index <> R_NO Then
    ReadReg(Ref^.Index);
End;

Procedure TPaiProp.ReadOp(const o:toper);
Begin
  Case o.typ Of
    top_reg: ReadReg(o.reg);
    top_ref: ReadRef(o.ref);
    top_symbol : ;
  End;
End;

{$ifdef arithopt}
Procedure TPaiProp.ModifyReg(reg: TRegister; Var NrOfInstrSinceLastMod:
                               TInstrSinceLastMod);
Begin
  With Regs[reg] Do
    If (Typ = Con_Ref)
      Then
        Begin
          IncState(WState);
 {also store how many instructions are part of the sequence in the first
  instructions PPaiProp, so it can be easily accessed from within
  CheckSequence}
          Inc(NrOfMods, NrOfInstrSinceLastMod[Reg]);
          PPaiProp(StartMod^.OptInfo)^.Regs[Reg].NrOfMods := NrOfMods;
          NrOfInstrSinceLastMod[Reg] := 0;
        End
      Else
        DestroyReg(Reg, NrOfInstrSinceLastMod);
End;

Procedure TPaiProp.ModifyOp(const oper: TOper);
Begin
  If oper.typ = top_reg Then
    ModifyReg(RegMaxSize(oper.reg))
  Else
    Begin
      ReadOp(oper);
      DestroyOp(oper);
    End
End;
{$endif arithopt}

Procedure TPaiProp.IncWState(Reg: TRegister);{$ifdef fpc} inline;{$endif fpc}
Begin
  IncState(Regs[Reg].WState);
End;

Procedure TPaiProp.IncRState(Reg: TRegister);{$ifdef fpc} inline;{$endif fpc}
Begin
  IncState(Regs[Reg].RState);
End;

Procedure TPaiProp.IncState(Var s: TStateInt); {$ifdef fpc} inline;{$endif fpc}
Begin
  If s <> $ff Then Inc(s)
  Else s := 0
End;

Function TPaiProp.GetWState(Reg: TRegister): TStateInt;
Begin
  GetWState := Regs[Reg].WState
End;

Function TPaiProp.GetRState(Reg: TRegister): TStateInt;
Begin
  GetRState := Regs[Reg].RState
End;

Function TPaiProp.GetRegContentKind(Reg: TRegister): Byte;
Begin
  GetRegContentKind := Regs[Reg].typ
End;

{******************* TAOptObj *******************}

Constructor TAoptObj.Init(_AsmL: PAasmOutput; _BlockStart, _BlockEnd: Pai;
                            _LabelInfo: PLabelInfo);
Begin
  AsmL := _AsmL;
  BlockStart := _BlockStart;
  BlockEnd := _BlockEnd;
  LabelInfo := _LabelInfo
End;

Function TAOptObj.FindLabel(L: PasmLabel; Var hp: Pai): Boolean;
Var TempP: Pai;
Begin
  TempP := hp;
  While Assigned(TempP) and
       (TempP^.typ In SkipInstr + [ait_label]) Do
    If (TempP^.typ <> ait_Label) Or
       (pai_label(TempP)^.l <> L)
      Then GetNextInstruction(TempP, TempP)
      Else
        Begin
          hp := TempP;
          FindLabel := True;
          exit
        End;
  FindLabel := False;
End;

Procedure TAOptObj.InsertLLItem(AsmL: PAasmOutput; prev, foll, new_one:
                                  PLinkedList_Item);
Begin
  If Assigned(prev) Then
    If Assigned(foll) Then
      Begin
        If Assigned(new_one) Then
          Begin
            new_one^.previous := prev;
            new_one^.next := foll;
            prev^.next := new_one;
            foll^.previous := new_one;
            new_one^.fileinfo := foll^.fileinfo
          End
      End
    Else AsmL^.Concat(new_one)
  Else If Assigned(Foll) Then AsmL^.Insert(new_one)
End;

Function TAOptObj.RegInInstruction(Reg: TRegister; p1: Pai): Boolean;
Var Count: TNatInt;
    TmpResult: Boolean;
Begin
  TmpResult := False;
  Count := 0;
  If (p1^.typ = ait_instruction) Then
    Repeat
      TmpResult := RegInOp(Reg, PInstr(p1)^.oper[Count]);
      Inc(Count)
    Until (Count = MaxOps) or TmpResult;
  RegInInstruction := TmpResult
End;

Function TAOptObj.RegInOp(Reg: TRegister; const op: toper): Boolean;
Begin
  Case op.typ Of
    Top_Reg: RegInOp := Reg = op.reg;
    Top_Ref: RegInOp := RegInRef(Reg, op.ref^)
    Else RegInOp := False
  End
End;

Function TAOptObj.RegModifiedByInstruction(Reg: TRegister; p1: Pai): Boolean;
Var hp: Pai;
Begin
  If GetLastInstruction(p1, hp)
    Then
      RegModifiedByInstruction :=
        PPAiProp(p1^.OptInfo)^.Regs[Reg].WState <>
          PPAiProp(hp^.OptInfo)^.Regs[Reg].WState
    Else RegModifiedByInstruction := True;
End;

Function TAOptObj.GetNextInstruction(Current: Pai; Var Next: Pai): Boolean;
Begin
  Repeat
    Current := Pai(Current^.Next);
    While Assigned(Current) And
          ((Current^.typ In SkipInstr) or
           ((Current^.typ = ait_label) And
            Not(Pai_Label(Current)^.l^.is_used))) Do
      Current := Pai(Current^.Next);
    If Assigned(Current) And
       (Current^.typ = ait_Marker) And
       (Pai_Marker(Current)^.Kind = NoPropInfoStart) Then
      Begin
        While Assigned(Current) And
              ((Current^.typ <> ait_Marker) Or
               (Pai_Marker(Current)^.Kind <> NoPropInfoEnd)) Do
          Current := Pai(Current^.Next);
      End;
  Until Not(Assigned(Current)) Or
        (Current^.typ <> ait_Marker) Or
        (Pai_Marker(Current)^.Kind <> NoPropInfoEnd);
  Next := Current;
  If Assigned(Current) And
     Not((Current^.typ In SkipInstr) or
         ((Current^.typ = ait_label) And
          Not(Pai_Label(Current)^.l^.is_used)))
    Then GetNextInstruction := True
    Else
      Begin
        Next := Nil;
        GetNextInstruction := False;
      End;
End;

Function TAOptObj.GetLastInstruction(Current: Pai; Var Last: Pai): Boolean;
Begin
  Repeat
    Current := Pai(Current^.previous);
    While Assigned(Current) And
          (((Current^.typ = ait_Marker) And
            Not(Pai_Marker(Current)^.Kind in [AsmBlockEnd,NoPropInfoEnd])) or
           (Current^.typ In SkipInstr) or
           ((Current^.typ = ait_label) And
             Not(Pai_Label(Current)^.l^.is_used))) Do
      Current := Pai(Current^.previous);
    If Assigned(Current) And
       (Current^.typ = ait_Marker) And
       (Pai_Marker(Current)^.Kind = NoPropInfoEnd) Then
      Begin
        While Assigned(Current) And
              ((Current^.typ <> ait_Marker) Or
               (Pai_Marker(Current)^.Kind <> NoPropInfoStart)) Do
          Current := Pai(Current^.previous);
      End;
  Until Not(Assigned(Current)) Or
        (Current^.typ <> ait_Marker) Or
        (Pai_Marker(Current)^.Kind <> NoPropInfoStart);
  If Not(Assigned(Current)) or
     (Current^.typ In SkipInstr) or
     ((Current^.typ = ait_label) And
      Not(Pai_Label(Current)^.l^.is_used)) or
     ((Current^.typ = ait_Marker) And
      (Pai_Marker(Current)^.Kind = AsmBlockEnd))
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

Procedure TAOptObj.SkipHead(var P: Pai);
{ skips Pai objects at the start of a block that don't do anything }
Var OldP: Pai;
Begin
  Repeat
    OldP := P;
    If (P^.typ in SkipInstr) Or
       ((P^.typ = ait_marker) And
        (Pai_Marker(P)^.Kind = AsmBlockEnd)) Then
      GetNextInstruction(P, P)
    Else If ((P^.Typ = Ait_Marker) And
        (Pai_Marker(P)^.Kind = NoPropInfoStart)) Then
 { a marker of the type NoPropInfoStart can't be the first instruction of a }
 { paasmoutput list                                                         }
      GetNextInstruction(Pai(P^.Previous),P);
    If (P^.Typ = Ait_Marker) And
       (Pai_Marker(P)^.Kind = AsmBlockStart) Then
      Begin
        P := Pai(P^.Next);
        While (P^.typ <> Ait_Marker) Or
              (Pai_Marker(P)^.Kind <> AsmBlockEnd) Do
          P := Pai(P^.Next)
      End;
    Until P = OldP
End;

Function TAOptObj.OpsEqual(const o1,o2:toper): Boolean;
Begin
  if o1.typ=o2.typ then
    Case o1.typ Of
      Top_Reg :
        OpsEqual:=o1.reg=o2.reg;
      Top_Ref :
        OpsEqual := RefsEqual(o1.ref^, o2.ref^);
      Top_Const :
        OpsEqual:=o1.val=o2.val;
      Top_Symbol :
        OpsEqual:=(o1.sym=o2.sym) and (o1.symofs=o2.symofs);
      Top_None :
        OpsEqual := True
      else OpsEqual := False
    End;
End;

Function TAOptObj.FindRegAlloc(Reg: TRegister; StartPai: Pai): Boolean;
Begin
  FindRegAlloc:=False;
  Repeat
    While Assigned(StartPai) And
          ((StartPai^.typ in (SkipInstr - [ait_regAlloc])) Or
           ((StartPai^.typ = ait_label) and
            Not(Pai_Label(StartPai)^.l^.Is_Used))) Do
      StartPai := Pai(StartPai^.Next);
    If Assigned(StartPai) And
       (StartPai^.typ = ait_regAlloc) and (PairegAlloc(StartPai)^.allocation) Then
      Begin
        if PairegAlloc(StartPai)^.Reg = Reg then
         begin
           FindRegAlloc:=true;
           exit;
         end;
        StartPai := Pai(StartPai^.Next);
      End
    else
      exit;
  Until false;
End;

Function TAOptObj.RefsEqual(Const R1, R2: TReference): Boolean;
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
                 ;

Function TAOptObj.RegInRef(Reg: TRegister; Const Ref: TReference): Boolean;
Begin
  Reg := RegMaxSize(Reg);
  RegInRef := (Ref.Base = Reg)
{$ifdef RefsHaveIndexReg}
  Or (Ref.Index = Reg)
{$endif RefsHaveIndexReg}
End;

Function TAOptObj.RegModifiedByInstruction(Reg: TRegister; p1: Pai): Boolean;
Var hp: Pai;
Begin
  If GetLastInstruction(p1, hp)
    Then
      RegModifiedByInstruction :=
        PPAiProp(p1^.OptInfo)^.GetWState <>
          PPAiProp(hp^.OptInfo)^.GetWState
    Else RegModifiedByInstruction := True;
End;

Function TAOptObj.RefInInstruction(Const Ref: TReference; p: Pai): Boolean;
Var Count: TNatInt;
    TmpResult: Boolean;
Begin
  TmpResult := False;
  If (p^.typ = ait_instruction) Then
    Begin
      Count := 0;
      Repeat
        If (PInstr(p)^.oper[Count].typ = Top_Ref) Then
          TmpResult := RefsEqual(Ref, PInstr(p)^.oper[Count].ref^);
        Inc(Count);
      Until (Count = MaxOps) or TmpResult;
    End;
  RefInInstruction := TmpResult;
End;

Function TAOptObj.RefInSequence(Const Ref: TReference; Content: TContent):
           Boolean;
Var p: Pai;
    Counter: Byte;
    TmpResult: Boolean;
Begin
  p := Content.StartMod;
  TmpResult := False;
  Counter := 1;
  While Not(TmpResult) And
        (Counter <= Content.NrOfMods) Do
    Begin
      If (p^.typ = ait_instruction) And
         RefInInstruction(Ref, p)
        Then TmpResult := True;
      Inc(Counter);
      GetNextInstruction(p,p)
    End;
  RefInSequence := TmpResult
End;

Function TAOptObj.RegMaxSize(Reg: TRegister): TRegister;
Begin
  RegMaxSize := Reg
End;

Function TAOptObj.RegsSameSize(Reg1, Reg2: TRegister): Boolean;
Begin
  RegsSameSize := True
End;

Function TAOptObj.IsLoadInstr(p: pai): Boolean;
Begin
  Abstract
End;

Function TAOptObj.RegReadByInstr(Reg: TRegister; p: Pai);
Begin
  Abstract
End;

Function TAOptObj.IsStoreInstr(p: pai): Boolean;
Begin
  Abstract
End;

Function TAOptObj.TCh2Reg(Ch: TChange): TRegister;
Begin
  Abstract
End;

End.

{
 $Log$
 Revision 1.2  1999-08-09 14:07:24  jonas
 commit.msg

 Revision 1.1  1999/08/08 13:24:50  jonas
   + added copyright header/GNU license info
   * made the assembler optimizer almost completely OOP
   * some code style clean up and extra comments
   * moved from the new/aopt to the /new and /new/i386 dirs

}