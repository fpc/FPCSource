{
    $Id$
    Copyright (c) 1998-2000 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit contains the processor independent assembler optimizer
    object, base for the dataflow analyzer, peepholeoptimizer and
    common subexpression elimination objects.

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

uses aasm, Cobjects, cpuinfo, cpubase, aoptbase, aoptcpub;

{ ************************************************************************* }
{ ********************************* Constants ***************************** }
{ ************************************************************************* }

Const

{Possible register content types}
  con_Unknown = 0;
  con_ref = 1;
  con_const = 2;

{***************** Types ****************}

Type

{ ************************************************************************* }
{ ************************* Some general type definitions ***************** }
{ ************************************************************************* }
  TRefCompare = Function(const r1, r2: TReference): Boolean;
  TRegArray = Array[LoReg..HiReg] of TRegister;
  TRegSet = Set of LoReg..HiReg;
{ possible actions on an operand: read, write or modify (= read & write) }
  TOpAction = (OpAct_Read, OpAct_Write, OpAct_Modify, OpAct_Unknown);

{ ************************************************************************* }
{ * Object to hold information on which regiters are in use and which not * }
{ ************************************************************************* }
  TUsedRegs = Object
    Constructor init;
    Constructor InitWithValue(Const _RegSet: TRegSet);
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

{ ************************************************************************* }
{ ******************* Contents of the integer registers ******************* }
{ ************************************************************************* }

 { size of the integer that holds the state number of a register. Can be any }
 { integer type, so it can be changed to reduce the size of the TContent     }
 { structure or to improve alignment                                         }
  TStateInt = Byte;

  TContent = Packed Record
    { start and end of block instructions that defines the }
    { content of this register. If Typ = con_const, then   }
    { Longint(StartMod) = value of the constant)           }
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

  TRegContent = Array[LoGPReg..HiGPReg] Of TContent;

{ ************************************************************************** }
{ information object with the contents of every register. Every Pai object   }
{ gets one of these assigned: a pointer to it is stored in the OptInfo field }
{ ************************************************************************** }

  PPaiProp = ^TPaiProp;

  TPaiProp = Object(TAoptBaseCpu)
    Regs: TRegContent;
    { info about allocation of general purpose integer registers }
    UsedRegs: TUsedRegs;
    { info about the conditional registers }
    CondRegs: TCondRegs;
    { can this instruction be removed? }
    CanBeRemoved: Boolean;

    Constructor init;

    { checks the whole sequence of which (so regs[which].StartMod and and  }
    { the next NrOfMods Pai objects) to see whether Reg is used somewhere, }
    { without it being loaded with something else first                    }
    Function RegInSequence(Reg, which: TRegister): Boolean;
    { destroy the contents of a register, as well as those whose contents }
    { are based on those of that register                                 }
    Procedure DestroyReg(Reg: TRegister; var InstrSinceLastMod:
      TInstrSinceLastMod);
    { if the contents of WhichReg (can be R_NO in case of a constant) are  }
    { written to memory at the location Ref, the contents of the registers }
    { that depend on Ref have to be  destroyed                             }
    Procedure DestroyRefs(Const Ref: TReference; WhichReg: TRegister; var
      InstrSinceLastMod: TInstrSinceLastMod);

    { an instruction reads from operand o }
    Procedure ReadOp(const o:toper);
    { an instruction reads from reference Ref }
    Procedure ReadRef(Ref: PReference);
    { an instruction reads from register Reg }
    Procedure ReadReg(Reg: TRegister);

    { an instruction writes/modifies operand o and this has special     }
    { side-effects or modifies the contents in such a way that we can't }
    { simply add this instruction to the sequence of instructions that  }
    { describe the contents of the operand, so destroy it               }
    Procedure DestroyOp(const o:Toper; var InstrSinceLastMod:
      TInstrSinceLastMod);
    { destroy the contents of all registers }
    Procedure DestroyAllRegs(var InstrSinceLastMod: TInstrSinceLastMod);
    { a register's contents are modified, but not destroyed (the new value }
    { depends on the old one)                                              }
    Procedure ModifyReg(reg: TRegister; var InstrSinceLastMod:
      TInstrSinceLastMod);
    { an operand's contents are modified, but not destroyed (the new value }
    { depends on the old one)                                              }
    Procedure ModifyOp(const oper: TOper; var InstrSinceLastMod:
      TInstrSinceLastMod);

    { increase the write state of a register (call every time a register is }
    { written to)                                                           }
    Procedure IncWState(Reg: TRegister);
    { increase the read state of a register (call every time a register is }
    { read from)                                                           }
    Procedure IncRState(Reg: TRegister);
    { get the write state of a register }
    Function GetWState(Reg: TRegister): TStateInt;
    { get the read state of a register }
    Function GetRState(Reg: TRegister): TStateInt;

    { get the type of contents of a register }
    Function GetRegContentType(Reg: TRegister): Byte;

    Destructor Done;

    Private

    Procedure IncState(var s: TStateInt);

    { returns whether the reference Ref is used somewhere in the loading }
    { sequence Content                                                   }
    Function RefInSequence(Const Ref: TReference; Content: TContent;
      RefsEq: TRefCompare): Boolean;

   { returns whether the instruction P reads from and/or writes }
   { to Reg                                                     }
   Function RefInInstruction(Const Ref: TReference; p: Pai;
     RefsEq: TRefCompare): Boolean;

   { returns whether two references with at least one pointing to an array }
   { may point to the same memory location                                 }

  End;


{ ************************************************************************* }
{ ************************ Label information ****************************** }
{ ************************************************************************* }
  TLabelTableItem = Record
    PaiObj: Pai;
  End;

{$ifndef TP}
  TLabelTable = Array[0..2500000] Of TLabelTableItem;
{$else TP}
  TLabelTable = Array[0..(65520 div sizeof(TLabelTableItem))] Of TLabelTableItem;
{$endif TP}
  PLabelTable = ^TLabelTable;
  PLabelInfo = ^TLabelInfo;
  TLabelInfo = Record
    { the highest and lowest label number occurring in the current code }
    { fragment                                                          }
    LowLabel, HighLabel: AWord;
    LabelDif: AWord;
    { table that contains the addresses of the Pai_Label objects associated }
    { with each label number                                                }
    LabelTable: PLabelTable;
  End;

{ ************************************************************************* }
{ ********** General optimizer object, used to derive others from ********* }
{ ************************************************************************* }

  TAOptObj = Object(TAoptBaseCpu)
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
    Procedure InsertLLItem(prev, foll, new_one: PLinkedList_Item);


    { If P is a Pai object releveant to the optimizer, P is returned   }
    { If it is not relevant tot he optimizer, the first object after P }
    { that is relevant is returned                                     }
    Function SkipHead(P: Pai): Pai;

    { returns true if the operands o1 and o2 are completely equal }
    Function OpsEqual(const o1,o2:toper): Boolean;

    { Returns true if a ait_alloc object for Reg is found in the block }
    { of Pai's starting with StartPai and ending with the next "real"  }
    { instruction                                                      }
    Function FindRegAlloc(Reg: TRegister; StartPai: Pai): Boolean;

    { processor dependent methods }

  End;

   Function ArrayRefsEq(const r1, r2: TReference): Boolean;

{ ***************************** Implementation **************************** }

Implementation

uses globtype, globals, cgbase, tainst;

{ ************************************************************************* }
{ ******************************** TUsedRegs ****************************** }
{ ************************************************************************* }

Constructor TUsedRegs.init;
Begin
  UsedRegs := [];
End;

Constructor TUsedRegs.InitWithValue(Const _RegSet: TRegSet);
Begin
  UsedRegs := _RegSet;
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

Destructor TUsedRegs.Done; {$ifdef inl} inline; {$endif inl}
Begin
end;

{ ************************************************************************* }
{ **************************** TPaiProp *********************************** }
{ ************************************************************************* }

Constructor TPaiProp.Init;
Begin
  UsedRegs.Init;
  CondRegs.init;
{  DirFlag: TFlagContents; I386 specific}
End;

Function TPaiProp.RegInSequence(Reg, which: TRegister): Boolean;
Var p: Pai;
    RegsChecked: TRegSet;
    content: TContent;
    Counter: Byte;
    TmpResult: Boolean;
Begin
  RegsChecked := [];
  content := regs[which];
  p := content.StartMod;
  TmpResult := False;
  Counter := 1;
  While Not(TmpResult) And
        (Counter <= Content.NrOfMods) Do
    Begin
      If IsLoadMemReg(p) Then
        With PInstr(p)^.oper[LoadSrc].ref^ Do
          If (Base = ProcInfo^.FramePointer)
{$ifdef RefsHaveIndexReg}
             And (Index = R_NO)
{$endif RefsHaveIndexReg} Then
            Begin
              RegsChecked := RegsChecked +
                [RegMaxSize(PInstr(p)^.oper[LoadDst].reg)];
              If Reg = RegMaxSize(PInstr(p)^.oper[LoadDst].reg) Then
                Break;
            End
          Else
            Begin
              If (Base = Reg) And
                 Not(Base In RegsChecked)
                Then TmpResult := True;
{$ifdef RefsHaveIndexReg}
              If Not(TmpResult) And
                 (Index = Reg) And
                   Not(Index In RegsChecked)
                Then TmpResult := True;
{$Endif RefsHaveIndexReg}
            End
      Else TmpResult := RegInInstruction(Reg, p);
      Inc(Counter);
      GetNextInstruction(p,p)
    End;
  RegInSequence := TmpResult
End;


Procedure TPaiProp.DestroyReg(Reg: TRegister; var InstrSinceLastMod:
            TInstrSinceLastMod);
{ Destroys the contents of the register Reg in the PPaiProp p1, as well as }
{ the contents of registers are loaded with a memory location based on Reg }
Var TmpWState, TmpRState: Byte;
    Counter: TRegister;
Begin
  Reg := RegMaxSize(Reg);
  If (Reg in [LoGPReg..HiGPReg]) Then
    For Counter := LoGPReg to HiGPReg Do
      With Regs[Counter] Do
        If (Counter = reg) Or
           ((Typ = Con_Ref) And
            RegInSequence(Reg, Counter)) Then
          Begin
            InstrSinceLastMod[Counter] := 0;
            IncWState(Counter);
            TmpWState := GetWState(Counter);
            TmpRState := GetRState(Counter);
            FillChar(Regs[Counter], SizeOf(TContent), 0);
            WState := TmpWState;
            RState := TmpRState
          End
End;

Function ArrayRefsEq(const r1, r2: TReference): Boolean;
Begin
  ArrayRefsEq := (R1.Offset+R1.OffsetFixup = R2.Offset+R2.OffsetFixup) And
{$ifdef refsHaveSegmentReg}
                 (R1.Segment = R2.Segment) And
{$endif}
                 (R1.Base = R2.Base) And
                 (R1.Symbol=R2.Symbol);
End;

Procedure TPaiProp.DestroyRefs(Const Ref: TReference; WhichReg: TRegister;
            var InstrSinceLastMod: TInstrSinceLastMod);
{ destroys all registers which possibly contain a reference to Ref, WhichReg }
{ is the register whose contents are being written to memory (if this proc   }
{ is called because of a "mov?? %reg, (mem)" instruction)                    }
Var RefsEq: TRefCompare;
    Counter: TRegister;
Begin
  WhichReg := RegMaxSize(WhichReg);
  If (Ref.base = procinfo^.FramePointer) or
      Assigned(Ref.Symbol) Then
    Begin
      If
{$ifdef refsHaveIndexReg}
         (Ref.Index = R_NO) And
{$endif refsHaveIndexReg}
         (Not(Assigned(Ref.Symbol)) or
          (Ref.base = R_NO)) Then
  { local variable which is not an array }
        RefsEq := {$ifdef fpc}@{$endif}RefsEqual
      Else
  { local variable which is an array }
        RefsEq := {$ifdef fpc}@{$endif}ArrayRefsEq;
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
                (RefInSequence(Ref,Regs[Counter], RefsEq) And
                 ((Counter <> WhichReg) Or
                  ((NrOfMods <> 1) And
 {StarMod is always of the type ait_instruction}
                   (PInstr(StartMod)^.oper[0].typ = top_ref) And
                   RefsEq(PInstr(StartMod)^.oper[0].ref^, Ref)
                  )
                 )
                )
               )
              Then
                DestroyReg(Counter, InstrSinceLastMod)
          End
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
                  ((PInstr(StartMod)^.oper[0].ref^.base = ProcInfo^.FramePointer) Or
                    Assigned(PInstr(StartMod)^.oper[0].ref^.Symbol)
                  )
                 )
             )
          Then DestroyReg(Counter, InstrSinceLastMod)
End;

Procedure TPaiProp.DestroyAllRegs(var InstrSinceLastMod: TInstrSinceLastMod);
Var Counter: TRegister;
Begin {initializes/desrtoys all registers}
  For Counter := LoGPReg To HiGPReg Do
    Begin
      ReadReg(Counter);
      DestroyReg(Counter, InstrSinceLastMod);
    End;
  CondRegs.Init;
{ FPURegs.Init; }
End;

Procedure TPaiProp.DestroyOp(const o:Toper; var InstrSinceLastMod:
            TInstrSinceLastMod);
Begin
  Case o.typ Of
    top_reg: DestroyReg(o.reg, InstrSinceLastMod);
    top_ref:
      Begin
        ReadRef(o.ref);
        DestroyRefs(o.ref^, R_NO, InstrSinceLastMod);
      End;
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
{$ifdef refsHaveIndexReg}
  If Ref^.Index <> R_NO Then
    ReadReg(Ref^.Index);
{$endif}
End;

Procedure TPaiProp.ReadOp(const o:toper);
Begin
  Case o.typ Of
    top_reg: ReadReg(o.reg);
    top_ref: ReadRef(o.ref);
    top_symbol : ;
  End;
End;

Procedure TPaiProp.ModifyReg(reg: TRegister; Var InstrSinceLastMod:
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
        DestroyReg(Reg, InstrSinceLastMod);
End;

Procedure TPaiProp.ModifyOp(const oper: TOper; var InstrSinceLastMod:
            TInstrSinceLastMod);
Begin
  If oper.typ = top_reg Then
    ModifyReg(RegMaxSize(oper.reg))
  Else
    Begin
      ReadOp(oper);
      DestroyOp(oper, InstrSinceLastMod);
    End
End;

Procedure TPaiProp.IncWState(Reg: TRegister);{$ifdef inl} inline;{$endif inl}
Begin
  IncState(Regs[Reg].WState);
End;

Procedure TPaiProp.IncRState(Reg: TRegister);{$ifdef inl} inline;{$endif inl}
Begin
  IncState(Regs[Reg].RState);
End;

Function TPaiProp.GetWState(Reg: TRegister): TStateInt; {$ifdef inl} inline;{$endif inl}
Begin
  GetWState := Regs[Reg].WState
End;

Function TPaiProp.GetRState(Reg: TRegister): TStateInt; {$ifdef inl} inline;{$endif inl}
Begin
  GetRState := Regs[Reg].RState
End;

Function TPaiProp.GetRegContentType(Reg: TRegister): Byte; {$ifdef inl} inline;{$endif inl}
Begin
  GetRegContentType := Regs[Reg].typ
End;

Destructor TPaiProp.Done;
Begin
  UsedRegs.Done;
  CondRegs.Done;
{  DirFlag: TFlagContents; I386 specific}
End;
{ ************************ private TPaiProp stuff ************************* }

Procedure TPaiProp.IncState(Var s: TStateInt); {$ifdef inl} inline;{$endif inl}
Begin
  If s <> High(TStateInt) Then Inc(s)
  Else s := 0
End;

Function TPaiProp.RefInInstruction(Const Ref: TReference; p: Pai;
  RefsEq: TRefCompare): Boolean;
Var Count: AWord;
    TmpResult: Boolean;
Begin
  TmpResult := False;
  If (p^.typ = ait_instruction) Then
    Begin
      Count := 0;
      Repeat
        If (PInstr(p)^.oper[Count].typ = Top_Ref) Then
          TmpResult := RefsEq(Ref, PInstr(p)^.oper[Count].ref^);
        Inc(Count);
      Until (Count = MaxOps) or TmpResult;
    End;
  RefInInstruction := TmpResult;
End;

Function TPaiProp.RefInSequence(Const Ref: TReference; Content: TContent;
  RefsEq: TRefCompare): Boolean;
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
         RefInInstruction(Ref, p, {$ifdef fpc}@{$endif}RefsEqual)
        Then TmpResult := True;
      Inc(Counter);
      GetNextInstruction(p,p)
    End;
  RefInSequence := TmpResult
End;

{ ************************************************************************* }
{ ***************************** TAoptObj ********************************** }
{ ************************************************************************* }

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

Procedure TAOptObj.InsertLLItem(prev, foll, new_one: PLinkedList_Item);
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
            Pai(new_one)^.fileinfo := Pai(foll)^.fileinfo
          End
      End
    Else AsmL^.Concat(new_one)
  Else If Assigned(Foll) Then AsmL^.Insert(new_one)
End;


Function TAOptObj.SkipHead(P: Pai): Pai;
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
    Until P = OldP;
  SkipHead := P;
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

End.

{
 $Log$
 Revision 1.3  2002-09-07 15:25:14  peter
   * old logs removed and tabs fixed

}
