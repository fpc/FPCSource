{
    $Id$
    Copyright (c) 1998-2002 by Jonas Maebe, member of the Freepascal
      development team

    This unit contains the data flow analyzer and several helper procedures
    and functions.

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
Unit DAOpt386;

{$i fpcdefs.inc}

Interface

Uses
  GlobType,
  CClasses,Aasmbase,aasmtai,aasmcpu,
  cpubase,optbase;

{******************************* Constants *******************************}

Const

{Possible register content types}
  con_Unknown = 0;
  con_ref = 1;
  con_const = 2;
  { The contents aren't usable anymore for CSE, but they may still be   }
  { usefull for detecting whether the result of a load is actually used }
  con_invalid = 3;
  { the reverse of the above (in case a (conditional) jump is encountered): }
  { CSE is still possible, but the original instruction can't be removed    }
  con_noRemoveRef = 4;
  { same, but for constants }
  con_noRemoveConst = 5;

{********************************* Types *********************************}

type
  TRegArray = Array[R_EAX..R_BL] of TRegister;
  TRegSet = Set of R_EAX..R_BL;
  TRegInfo = Record
                NewRegsEncountered, OldRegsEncountered: TRegSet;
                RegsLoadedForRef: TRegSet;
                regsStillUsedAfterSeq: TRegSet;
                lastReload: array[R_EAX..R_EDI] of Tai;
                New2OldReg: TRegArray;
              End;

{possible actions on an operand: read, write or modify (= read & write)}
  TOpAction = (OpAct_Read, OpAct_Write, OpAct_Modify, OpAct_Unknown);

{the possible states of a flag}
  TFlagContents = (F_Unknown, F_NotSet, F_Set);

  TContent = Packed Record
      {start and end of block instructions that defines the
       content of this register.}
               StartMod: Tai;
               MemWrite: Taicpu;
      {how many instructions starting with StarMod does the block consist of}
               NrOfMods: Byte;
      {the type of the content of the register: unknown, memory, constant}
               Typ: Byte;
               case byte of
      {starts at 0, gets increased everytime the register is written to}
                 1: (WState: Byte;
      {starts at 0, gets increased everytime the register is read from}
                       RState: Byte);
      { to compare both states in one operation }
                 2: (state: word);
             End;

{Contents of the integer registers}
  TRegContent = Array[R_EAX..R_EDI] Of TContent;

{contents of the FPU registers}
  TRegFPUContent = Array[R_ST..R_ST7] Of TContent;

{$ifdef tempOpts}
{ linked list which allows searching/deleting based on value, no extra frills}
  PSearchLinkedListItem = ^TSearchLinkedListItem;
  TSearchLinkedListItem = object(TLinkedList_Item)
    constructor init;
    function equals(p: PSearchLinkedListItem): boolean; virtual;
  end;

  PSearchDoubleIntItem = ^TSearchDoubleInttem;
  TSearchDoubleIntItem = object(TLinkedList_Item)
    constructor init(_int1,_int2: longint);
    function equals(p: PSearchLinkedListItem): boolean; virtual;
   private
    int1, int2: longint;
  end;

  PSearchLinkedList = ^TSearchLinkedList;
  TSearchLinkedList = object(TLinkedList)
    function searchByValue(p: PSearchLinkedListItem): boolean;
    procedure removeByValue(p: PSearchLinkedListItem);
  end;
{$endif tempOpts}

{information record with the contents of every register. Every Tai object
 gets one of these assigned: a pointer to it is stored in the OptInfo field}
  TTaiProp = Record
               Regs: TRegContent;
{               FPURegs: TRegFPUContent;} {currently not yet used}
    { allocated Registers }
               UsedRegs: TRegSet;
    { status of the direction flag }
               DirFlag: TFlagContents;
{$ifdef tempOpts}
    { currently used temps }
               tempAllocs: PSearchLinkedList;
{$endif tempOpts}
    { can this instruction be removed? }
               CanBeRemoved: Boolean;
               { are the resultflags set by this instruction used? }
               FlagsUsed: Boolean;
             End;

  PTaiProp = ^TTaiProp;

  TTaiPropBlock = Array[1..250000] Of TTaiProp;
  PTaiPropBlock = ^TTaiPropBlock;

  TInstrSinceLastMod = Array[R_EAX..R_EDI] Of Byte;

  TLabelTableItem = Record
                      TaiObj: Tai;
{$IfDef JumpAnal}
                      InstrNr: Longint;
                      RefsFound: Word;
                      JmpsProcessed: Word
{$EndIf JumpAnal}
                    End;
  TLabelTable = Array[0..2500000] Of TLabelTableItem;
  PLabelTable = ^TLabelTable;


{*********************** Procedures and Functions ************************}

Procedure InsertLLItem(AsmL: TAAsmOutput; prev, foll, new_one: TLinkedListItem);


function changeregsize(r:tregister;size:topsize):tregister;
Function Reg32(Reg: TRegister): TRegister;
Function RefsEquivalent(Const R1, R2: TReference; Var RegInfo: TRegInfo; OpAct: TOpAction): Boolean;
Function RefsEqual(Const R1, R2: TReference): Boolean;
Function IsGP32Reg(Reg: TRegister): Boolean;
Function RegInRef(Reg: TRegister; Const Ref: TReference): Boolean;
function RegReadByInstruction(reg: TRegister; hp: Tai): boolean;
function RegModifiedByInstruction(Reg: TRegister; p1: Tai): Boolean;
function RegInInstruction(r: ToldRegister; p1: Tai): Boolean;
function RegInOp(Reg: TRegister; const o:toper): Boolean;
function instrWritesFlags(p: Tai): boolean;
function instrReadsFlags(p: Tai): boolean;

function writeToMemDestroysContents(regWritten: tregister; const ref: treference;
  reg: tregister; const c: tcontent; var invalsmemwrite: boolean): boolean;
function writeToRegDestroysContents(destReg: tregister; reg: tregister;
  const c: tcontent): boolean;
function writeDestroysContents(const op: toper; reg: tregister;
  const c: tcontent): boolean;


Function GetNextInstruction(Current: Tai; Var Next: Tai): Boolean;
Function GetLastInstruction(Current: Tai; Var Last: Tai): Boolean;
Procedure SkipHead(var P: Tai);
function labelCanBeSkipped(p: Tai_label): boolean;

Procedure RemoveLastDeallocForFuncRes(asmL: TAAsmOutput; p: Tai);
Function regLoadedWithNewValue(reg: tregister; canDependOnPrevValue: boolean;
           hp: Tai): boolean;
Procedure UpdateUsedRegs(Var UsedRegs: TRegSet; p: Tai);
Procedure AllocRegBetween(AsmL: TAAsmOutput; Reg: TRegister; p1, p2: Tai);
function FindRegDealloc(reg: tregister; p: Tai): boolean;

Function RegsEquivalent(OldReg, NewReg: TRegister; Var RegInfo: TRegInfo; OpAct: TopAction): Boolean;
Function InstructionsEquivalent(p1, p2: Tai; Var RegInfo: TRegInfo): Boolean;
function sizescompatible(loadsize,newsize: topsize): boolean;
Function OpsEqual(const o1,o2:toper): Boolean;

Function DFAPass1(AsmL: TAAsmOutput; BlockStart: Tai): Tai;
Function DFAPass2(
{$ifdef statedebug}
                   AsmL: TAAsmOutPut;
{$endif statedebug}
                                      BlockStart, BlockEnd: Tai): Boolean;
Procedure ShutDownDFA;

Function FindLabel(L: tasmlabel; Var hp: Tai): Boolean;

Procedure IncState(Var S: Byte; amount: longint);

{******************************* Variables *******************************}

Var
{the amount of TaiObjects in the current assembler list}
  NrOfTaiObjs: Longint;

{Array which holds all TTaiProps}
  TaiPropBlock: PTaiPropBlock;

  LoLab, HiLab, LabDif: Longint;

  LTable: PLabelTable;

{*********************** End of Interface section ************************}


Implementation

Uses
  globals, systems, verbose, cgbase, symconst, symsym, cginfo, cgobj,
   rgobj;

Type
  TRefCompare = function(const r1, r2: TReference): Boolean;

Var
 {How many instructions are between the current instruction and the last one
  that modified the register}
  NrOfInstrSinceLastMod: TInstrSinceLastMod;

{$ifdef tempOpts}
  constructor TSearchLinkedListItem.init;
  begin
  end;

  function TSearchLinkedListItem.equals(p: PSearchLinkedListItem): boolean;
  begin
    equals := false;
  end;

  constructor TSearchDoubleIntItem.init(_int1,_int2: longint);
  begin
    int1 := _int1;
    int2 := _int2;
  end;

  function TSearchDoubleIntItem.equals(p: PSearchLinkedListItem): boolean;
  begin
    equals := (TSearchDoubleIntItem(p).int1 = int1) and
              (TSearchDoubleIntItem(p).int2 = int2);
  end;

  function TSearchLinkedList.searchByValue(p: PSearchLinkedListItem): boolean;
  var temp: PSearchLinkedListItem;
  begin
    temp := first;
    while (temp <> last.next) and
          not(temp.equals(p)) do
      temp := temp.next;
    searchByValue := temp <> last.next;
  end;

  procedure TSearchLinkedList.removeByValue(p: PSearchLinkedListItem);
  begin
    temp := first;
    while (temp <> last.next) and
          not(temp.equals(p)) do
      temp := temp.next;
    if temp <> last.next then
      begin
        remove(temp);
        dispose(temp,done);
      end;
  end;

Procedure updateTempAllocs(Var UsedRegs: TRegSet; p: Tai);
{updates UsedRegs with the RegAlloc Information coming after P}
Begin
  Repeat
    While Assigned(p) And
          ((p.typ in (SkipInstr - [ait_RegAlloc])) or
           ((p.typ = ait_label) And
            labelCanBeSkipped(Tai_label(current)))) Do
         p := Tai(p.next);
    While Assigned(p) And
          (p.typ=ait_RegAlloc) Do
      Begin
        if tai_regalloc(p).allocation then
          UsedRegs := UsedRegs + [tai_regalloc(p).Reg]
        else
          UsedRegs := UsedRegs - [tai_regalloc(p).Reg];
        p := Tai(p.next);
      End;
  Until Not(Assigned(p)) Or
        (Not(p.typ in SkipInstr) And
         Not((p.typ = ait_label) And
             labelCanBeSkipped(Tai_label(current))));
End;

{$endif tempOpts}

{************************ Create the Label table ************************}

Function FindLoHiLabels(Var LowLabel, HighLabel, LabelDif: Longint; BlockStart: Tai): Tai;
{Walks through the TAAsmlist to find the lowest and highest label number}
Var LabelFound: Boolean;
    P, lastP: Tai;
Begin
  LabelFound := False;
  LowLabel := MaxLongint;
  HighLabel := 0;
  P := BlockStart;
  lastP := p;
  While Assigned(P) Do
    Begin
      If (Tai(p).typ = ait_label) Then
        If not labelCanBeSkipped(Tai_label(p))
          Then
            Begin
              LabelFound := True;
              If (Tai_Label(p).l.labelnr < LowLabel) Then
                LowLabel := Tai_Label(p).l.labelnr;
              If (Tai_Label(p).l.labelnr > HighLabel) Then
                HighLabel := Tai_Label(p).l.labelnr;
            End;
      lastP := p;
      GetNextInstruction(p, p);
    End;
  if (lastP.typ = ait_marker) and
     (Tai_marker(lastp).kind = asmBlockStart) then
    FindLoHiLabels := lastP
  else FindLoHiLabels := nil;
  If LabelFound
    Then LabelDif := HighLabel+1-LowLabel
    Else LabelDif := 0;
End;

Function FindRegAlloc(Reg: Tregister; StartTai: Tai; alloc: boolean): Boolean;
{ Returns true if a ait_alloc object for Reg is found in the block of Tai's }
{ starting with StartTai and ending with the next "real" instruction        }
Begin
  if reg.enum>lastreg then
    internalerror(200301081);
  FindRegAlloc := false;
  Repeat
    While Assigned(StartTai) And
          ((StartTai.typ in (SkipInstr - [ait_regAlloc])) Or
           ((StartTai.typ = ait_label) and
            labelCanBeSkipped(Tai_label(startTai)))) Do
      StartTai := Tai(StartTai.Next);
    If Assigned(StartTai) and
       (StartTai.typ = ait_regAlloc) then
      begin
        if Tai_regalloc(startTai).reg.enum>lastreg then
          internalerror(200301081);
        if (tai_regalloc(StartTai).allocation = alloc) and
           (tai_regalloc(StartTai).Reg.enum = Reg.enum) then
          begin
            FindRegAlloc:=true;
            break;
          end;
        StartTai := Tai(StartTai.Next);
      end
    else
      break;
  Until false;
End;

Procedure RemoveLastDeallocForFuncRes(asmL: TAAsmOutput; p: Tai);

  Procedure DoRemoveLastDeallocForFuncRes(asmL: TAAsmOutput; reg: ToldRegister);
  var
    hp2: Tai;
  begin
    hp2 := p;
    repeat
      hp2 := Tai(hp2.previous);
      if assigned(hp2) and
         (hp2.typ = ait_regalloc) and
         not(tai_regalloc(hp2).allocation) and
         (tai_regalloc(hp2).reg.enum = reg) then
        begin
          asml.remove(hp2);
          hp2.free;
          break;
        end;
    until not(assigned(hp2)) or regInInstruction(reg,hp2);
  end;

begin
    case current_procdef.rettype.def.deftype of
      arraydef,recorddef,pointerdef,
         stringdef,enumdef,procdef,objectdef,errordef,
         filedef,setdef,procvardef,
         classrefdef,forwarddef:
        DoRemoveLastDeallocForFuncRes(asmL,R_EAX);
      orddef:
        if current_procdef.rettype.def.size <> 0 then
          begin
            DoRemoveLastDeallocForFuncRes(asmL,R_EAX);
            { for int64/qword }
            if current_procdef.rettype.def.size = 8 then
              DoRemoveLastDeallocForFuncRes(asmL,R_EDX);
          end;
    end;
end;

procedure getNoDeallocRegs(var regs: TRegSet);
var regCounter: ToldRegister;
begin
  regs := [];
    case current_procdef.rettype.def.deftype of
      arraydef,recorddef,pointerdef,
         stringdef,enumdef,procdef,objectdef,errordef,
         filedef,setdef,procvardef,
         classrefdef,forwarddef:
       regs := [R_EAX];
      orddef:
        if current_procdef.rettype.def.size <> 0 then
          begin
            regs := [R_EAX];
            { for int64/qword }
            if current_procdef.rettype.def.size = 8 then
              regs := regs + [R_EDX];
          end;
    end;
  for regCounter := R_EAX to R_EBX do
{    if not(regCounter in rg.usableregsint) then}
      include(regs,regCounter);
end;

Procedure AddRegDeallocFor(asmL: TAAsmOutput; reg: TRegister; p: Tai);
var hp1: Tai;
    funcResRegs: TRegset;
    funcResReg: boolean;
begin

  if reg.enum>lastreg then
    internalerror(200301081);
{ if not(reg.enum in rg.usableregsint) then
    exit;}
 if not(reg.enum in [R_EDI]) then
    exit;
  getNoDeallocRegs(funcResRegs);
{  funcResRegs := funcResRegs - rg.usableregsint;}
{  funcResRegs := funcResRegs - [R_EDI];}
  funcResRegs := funcResRegs - [R_EAX,R_EBX,R_ECX,R_EDX,R_ESI];
  funcResReg := reg.enum in funcResRegs;
  hp1 := p;
{  while not(funcResReg and
            (p.typ = ait_instruction) and
            (Taicpu(p).opcode = A_JMP) and
            (tasmlabel(Taicpu(p).oper[0].sym) = aktexit2label)) and
        getLastInstruction(p, p) And
        not(regInInstruction(reg.enum, p)) Do
    hp1 := p; }
  { don't insert a dealloc for registers which contain the function result }
  { if they are followed by a jump to the exit label (for exit(...))       }
  {if not(funcResReg) or
     not((hp1.typ = ait_instruction) and
         (Taicpu(hp1).opcode = A_JMP) and
         (tasmlabel(Taicpu(hp1).oper[0].sym) = aktexit2label)) then }
    begin
      p := tai_regalloc.deAlloc(reg);
      insertLLItem(AsmL, hp1.previous, hp1, p);
    end;
end;

Procedure BuildLabelTableAndFixRegAlloc(asmL: TAAsmOutput; Var LabelTable: PLabelTable; LowLabel: Longint;
            Var LabelDif: Longint; BlockStart, BlockEnd: Tai);
{Builds a table with the locations of the labels in the TAAsmoutput.
 Also fixes some RegDeallocs like "# %eax released; push (%eax)"}
Var p, hp1, hp2, lastP: Tai;
    regCounter: TRegister;
    UsedRegs, noDeallocRegs: TRegSet;
Begin
  UsedRegs := [];
  If (LabelDif <> 0) Then
    Begin
      GetMem(LabelTable, LabelDif*SizeOf(TLabelTableItem));
      FillChar(LabelTable^, LabelDif*SizeOf(TLabelTableItem), 0);
    End;
  p := BlockStart;
  lastP := p;
  While (P <> BlockEnd) Do
    Begin
      Case p.typ Of
        ait_Label:
          If not labelCanBeSkipped(Tai_label(p)) Then
            LabelTable^[Tai_Label(p).l.labelnr-LowLabel].TaiObj := p;
        ait_regAlloc:
          { ESI and EDI are (de)allocated manually, don't mess with them }
          if not(tai_regalloc(p).Reg.enum in [R_EDI]) then
            begin
              if tai_regalloc(p).Allocation then
                Begin
                  If Not(tai_regalloc(p).Reg.enum in UsedRegs) Then
                    UsedRegs := UsedRegs + [tai_regalloc(p).Reg.enum]
                  Else
                    addRegDeallocFor(asmL, tai_regalloc(p).reg, p);
                End
              else
                begin
                  UsedRegs := UsedRegs - [tai_regalloc(p).Reg.enum];
                  hp1 := p;
                  hp2 := nil;
                  While Not(FindRegAlloc(tai_regalloc(p).Reg, Tai(hp1.Next),true)) And
                        GetNextInstruction(hp1, hp1) And
                        RegInInstruction(tai_regalloc(p).Reg.enum, hp1) Do
                    hp2 := hp1;
                  If hp2 <> nil Then
                    Begin
                      hp1 := Tai(p.previous);
                      AsmL.Remove(p);
                      InsertLLItem(AsmL, hp2, Tai(hp2.Next), p);
                      p := hp1;
                    end;
                end;
            end;
      end;
      repeat
        lastP := p;
        P := Tai(P.Next);
      until not(Assigned(p)) or
            not(p.typ in (SkipInstr - [ait_regalloc]));
    End;
  { don't add deallocation for function result variable or for regvars}
  getNoDeallocRegs(noDeallocRegs);
  usedRegs := usedRegs - noDeallocRegs;
  for regCounter.enum := R_EAX to R_EDI do
    if regCounter.enum in usedRegs then
      addRegDeallocFor(asmL,regCounter,lastP);
End;

{************************ Search the Label table ************************}

Function FindLabel(L: tasmlabel; Var hp: Tai): Boolean;

{searches for the specified label starting from hp as long as the
 encountered instructions are labels, to be able to optimize constructs like

 jne l2              jmp l2
 jmp l3     and      l1:
 l1:                 l2:
 l2:}

Var TempP: Tai;

Begin
  TempP := hp;
  While Assigned(TempP) and
       (Tempp.typ In SkipInstr + [ait_label,ait_align]) Do
    If (Tempp.typ <> ait_Label) Or
       (Tai_label(Tempp).l <> L)
      Then GetNextInstruction(TempP, TempP)
      Else
        Begin
          hp := TempP;
          FindLabel := True;
          exit
        End;
  FindLabel := False;
End;

{************************ Some general functions ************************}

  const
    reg2reg32 : array[firstreg..lastreg] of Toldregister = (R_NO,
      R_EAX,R_ECX,R_EDX,R_EBX,R_ESP,R_EBP,R_ESI,R_EDI,
      R_EAX,R_ECX,R_EDX,R_EBX,R_ESP,R_EBP,R_ESI,R_EDI,
      R_EAX,R_ECX,R_EDX,R_EBX,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO
    );
    reg2reg16 : array[firstreg..lastreg] of Toldregister = (R_NO,
      R_AX,R_CX,R_DX,R_BX,R_SP,R_BP,R_SI,R_DI,
      R_AX,R_CX,R_DX,R_BX,R_SP,R_BP,R_SI,R_DI,
      R_AX,R_CX,R_DX,R_BX,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO
    );
    reg2reg8 : array[firstreg..lastreg] of Toldregister = (R_NO,
      R_AL,R_CL,R_DL,R_BL,R_NO,R_NO,R_NO,R_NO,
      R_AL,R_CL,R_DL,R_BL,R_NO,R_NO,R_NO,R_NO,
      R_AL,R_CL,R_DL,R_BL,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,
      R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO,R_NO
    );

    { convert a register to a specfied register size }
    function changeregsize(r:tregister;size:topsize):tregister;
      var
        reg : tregister;
      begin
        case size of
          S_B :
            reg.enum:=reg2reg8[r.enum];
          S_W :
            reg.enum:=reg2reg16[r.enum];
          S_L :
            reg.enum:=reg2reg32[r.enum];
          else
            internalerror(200204101);
        end;
        if reg.enum=R_NO then
         internalerror(200204102);
        changeregsize:=reg;
      end;

Function TCh2Reg(Ch: TInsChange): ToldRegister;
{converts a TChange variable to a TRegister}
Begin
  If (Ch <= Ch_REDI) Then
    TCh2Reg := ToldRegister(Byte(Ch))
  Else
    If (Ch <= Ch_WEDI) Then
      TCh2Reg := ToldRegister(Byte(Ch) - Byte(Ch_REDI))
    Else
      If (Ch <= Ch_RWEDI) Then
        TCh2Reg := ToldRegister(Byte(Ch) - Byte(Ch_WEDI))
      Else
        If (Ch <= Ch_MEDI) Then
          TCh2Reg := ToldRegister(Byte(Ch) - Byte(Ch_RWEDI))
        Else InternalError($db)
End;

Function Reg32(Reg: TRegister): TRegister;
{Returns the 32 bit component of Reg if it exists, otherwise Reg is returned}
Begin
  if reg.enum>lastreg then
    internalerror(200301081);
  Reg32 := Reg;
  If (Reg.enum >= R_AX)
    Then
      If (Reg.enum <= R_DI)
        Then Reg32 := changeregsize(Reg,S_L)
        Else
          If (Reg.enum <= R_BL)
            Then Reg32 := changeregsize(Reg,S_L);
End;

{ inserts new_one between prev and foll }
Procedure InsertLLItem(AsmL: TAAsmOutput; prev, foll, new_one: TLinkedListItem);
Begin
  If Assigned(prev) Then
    If Assigned(foll) Then
      Begin
        If Assigned(new_one) Then
          Begin
            new_one.previous := prev;
            new_one.next := foll;
            prev.next := new_one;
            foll.previous := new_one;
            { shgould we update line information }
            if (not (Tai(new_one).typ in SkipLineInfo)) and
               (not (Tai(foll).typ in SkipLineInfo)) then
            Tailineinfo(new_one).fileinfo := Tailineinfo(foll).fileinfo;
          End;
      End
    Else asml.Concat(new_one)
  Else If Assigned(Foll) Then asml.Insert(new_one)
End;

{********************* Compare parts of Tai objects *********************}

Function RegsSameSize(Reg1, Reg2: TRegister): Boolean;
{returns true if Reg1 and Reg2 are of the same size (so if they're both
 8bit, 16bit or 32bit)}
Begin
  if reg1.enum>lastreg then
    internalerror(200301081);
  if reg2.enum>lastreg then
    internalerror(200301081);
  If (Reg1.enum <= R_EDI)
    Then RegsSameSize := (Reg2.enum <= R_EDI)
    Else
      If (Reg1.enum <= R_DI)
        Then RegsSameSize := (Reg2.enum in [R_AX..R_DI])
        Else
          If (Reg1.enum <= R_BL)
            Then RegsSameSize := (Reg2.enum in [R_AL..R_BL])
            Else RegsSameSize := False
End;

Procedure AddReg2RegInfo(OldReg, NewReg: TRegister; Var RegInfo: TRegInfo);
{updates the ???RegsEncountered and ???2???Reg fields of RegInfo. Assumes that
 OldReg and NewReg have the same size (has to be chcked in advance with
 RegsSameSize) and that neither equals R_NO}
Begin
  With RegInfo Do
    Begin
      if newreg.enum>lastreg then
        internalerror(200301081);
      if oldreg.enum>lastreg then
        internalerror(200301081);
      NewRegsEncountered := NewRegsEncountered + [NewReg.enum];
      OldRegsEncountered := OldRegsEncountered + [OldReg.enum];
      New2OldReg[NewReg.enum] := OldReg;
      Case OldReg.enum Of
        R_EAX..R_EDI:
          Begin
            NewRegsEncountered := NewRegsEncountered + [changeregsize(NewReg,S_W).enum];
            OldRegsEncountered := OldRegsEncountered + [changeregsize(OldReg,S_W).enum];
            New2OldReg[changeregsize(NewReg,S_W).enum] := changeregsize(OldReg,S_W);
            If (NewReg.enum in [R_EAX..R_EBX]) And
               (OldReg.enum in [R_EAX..R_EBX]) Then
              Begin
                NewRegsEncountered := NewRegsEncountered + [changeregsize(NewReg,S_B).enum];
                OldRegsEncountered := OldRegsEncountered + [changeregsize(OldReg,S_B).enum];
                New2OldReg[changeregsize(NewReg,S_B).enum] := changeregsize(OldReg,S_B);
              End;
          End;
        R_AX..R_DI:
          Begin
            NewRegsEncountered := NewRegsEncountered + [changeregsize(NewReg,S_L).enum];
            OldRegsEncountered := OldRegsEncountered + [changeregsize(OldReg,S_L).enum];
            New2OldReg[changeregsize(NewReg,S_L).enum] := changeregsize(OldReg,S_L);
            If (NewReg.enum in [R_AX..R_BX]) And
               (OldReg.enum in [R_AX..R_BX]) Then
              Begin
                NewRegsEncountered := NewRegsEncountered + [changeregsize(NewReg,S_B).enum];
                OldRegsEncountered := OldRegsEncountered + [changeregsize(OldReg,S_B).enum];
                New2OldReg[changeregsize(NewReg,S_B).enum] := changeregsize(OldReg,S_B);
              End;
          End;
        R_AL..R_BL:
          Begin
            NewRegsEncountered := NewRegsEncountered + [changeregsize(NewReg,S_L).enum]
                               + [changeregsize(NewReg,S_W).enum];
            OldRegsEncountered := OldRegsEncountered + [changeregsize(OldReg,S_L).enum]
                               + [changeregsize(OldReg,S_B).enum];
            New2OldReg[changeregsize(NewReg,S_L).enum] := changeregsize(OldReg,S_L);
          End;
      End;
    End;
End;

Procedure AddOp2RegInfo(const o:Toper; Var RegInfo: TRegInfo);
Begin
  Case o.typ Of
    Top_Reg:
      If (o.reg.enum <> R_NO) Then
        AddReg2RegInfo(o.reg, o.reg, RegInfo);
    Top_Ref:
      Begin
        If o.ref^.base.enum <> R_NO Then
          AddReg2RegInfo(o.ref^.base, o.ref^.base, RegInfo);
        If o.ref^.index.enum <> R_NO Then
          AddReg2RegInfo(o.ref^.index, o.ref^.index, RegInfo);
      End;
  End;
End;


Function RegsEquivalent(OldReg, NewReg: TRegister; Var RegInfo: TRegInfo; OPAct: TOpAction): Boolean;
Begin
  if oldreg.enum>lastreg then
    internalerror(200301081);
  if newreg.enum>lastreg then
    internalerror(200301081);
  If Not((OldReg.enum = R_NO) Or (NewReg.enum = R_NO)) Then
    If RegsSameSize(OldReg, NewReg) Then
      With RegInfo Do
{here we always check for the 32 bit component, because it is possible that
 the 8 bit component has not been set, event though NewReg already has been
 processed. This happens if it has been compared with a register that doesn't
 have an 8 bit component (such as EDI). In that case the 8 bit component is
 still set to R_NO and the comparison in the Else-part will fail}
        If (Reg32(OldReg).enum in OldRegsEncountered) Then
          If (Reg32(NewReg).enum in NewRegsEncountered) Then
            RegsEquivalent := (OldReg.enum = New2OldReg[NewReg.enum].enum)

 { If we haven't encountered the new register yet, but we have encountered the
   old one already, the new one can only be correct if it's being written to
   (and consequently the old one is also being written to), otherwise

   movl -8(%ebp), %eax        and         movl -8(%ebp), %eax
   movl (%eax), %eax                      movl (%edx), %edx

   are considered equivalent}

          Else
            If (OpAct = OpAct_Write) Then
              Begin
                AddReg2RegInfo(OldReg, NewReg, RegInfo);
                RegsEquivalent := True
              End
            Else Regsequivalent := False
        Else
           If Not(Reg32(NewReg).enum in NewRegsEncountered) and
              ((OpAct = OpAct_Write) or
               (newReg.enum = oldReg.enum)) Then
             Begin
               AddReg2RegInfo(OldReg, NewReg, RegInfo);
               RegsEquivalent := True
             End
           Else RegsEquivalent := False
    Else RegsEquivalent := False
  Else RegsEquivalent := OldReg.enum = NewReg.enum
End;

Function RefsEquivalent(Const R1, R2: TReference; var RegInfo: TRegInfo; OpAct: TOpAction): Boolean;
Begin
  RefsEquivalent := (R1.Offset+R1.OffsetFixup = R2.Offset+R2.OffsetFixup) And
                    RegsEquivalent(R1.Base, R2.Base, RegInfo, OpAct) And
                    RegsEquivalent(R1.Index, R2.Index, RegInfo, OpAct) And
                    (R1.Segment.enum = R2.Segment.enum) And (R1.ScaleFactor = R2.ScaleFactor) And
                    (R1.Symbol = R2.Symbol);
End;


Function RefsEqual(Const R1, R2: TReference): Boolean;
Begin
  RefsEqual := (R1.Offset+R1.OffsetFixup = R2.Offset+R2.OffsetFixup) And
               (R1.Segment.enum = R2.Segment.enum) And (R1.Base.enum = R2.Base.enum) And
               (R1.Index.enum = R2.Index.enum) And (R1.ScaleFactor = R2.ScaleFactor) And
               (R1.Symbol=R2.Symbol);
End;

Function IsGP32Reg(Reg: TRegister): Boolean;
{Checks if the register is a 32 bit general purpose register}
Begin
  if reg.enum>lastreg then
    internalerror(200301081);
  If (Reg.enum >= R_EAX) and (Reg.enum <= R_EBX)
    Then IsGP32Reg := True
    Else IsGP32reg := False
End;

Function RegInRef(Reg: TRegister; Const Ref: TReference): Boolean;
Begin {checks whether Ref contains a reference to Reg}
  if reg.enum>lastreg then
    internalerror(200301081);
  Reg := Reg32(Reg);
  RegInRef := (Ref.Base.enum = Reg.enum) Or (Ref.Index.enum = Reg.enum)
End;

function RegReadByInstruction(reg: TRegister; hp: Tai): boolean;
var p: Taicpu;
    opCount: byte;
begin
  if reg.enum>lastreg then
    internalerror(200301081);
  RegReadByInstruction := false;
  reg := reg32(reg);
  if hp.typ <> ait_instruction then
    exit;
  p := Taicpu(hp);
  case p.opcode of
    A_IMUL:
      case p.ops of
        1: regReadByInstruction := (reg.enum = R_EAX) or reginOp(reg,p.oper[0]);
        2,3:
          regReadByInstruction := regInOp(reg,p.oper[0]) or
            regInOp(reg,p.oper[1]);
      end;
    A_IDIV,A_DIV,A_MUL:
      begin
        regReadByInstruction :=
          regInOp(reg,p.oper[0]) or (reg.enum in [R_EAX,R_EDX]);
      end;
    else
      begin
        for opCount := 0 to 2 do
          if (p.oper[opCount].typ = top_ref) and
             RegInRef(reg,p.oper[opCount].ref^) then
            begin
              RegReadByInstruction := true;
              exit
            end;
        for opCount := 1 to MaxCh do
          case InsProp[p.opcode].Ch[opCount] of
            Ch_REAX..CH_REDI,CH_RWEAX..Ch_MEDI:
              if reg.enum = TCh2Reg(InsProp[p.opcode].Ch[opCount]) then
                begin
                  RegReadByInstruction := true;
                  exit
                end;
            Ch_RWOp1,Ch_ROp1,Ch_MOp1:
              if (p.oper[0].typ = top_reg) and
                 (reg32(p.oper[0].reg).enum = reg.enum) then
                begin
                  RegReadByInstruction := true;
                  exit
                end;
            Ch_RWOp2,Ch_ROp2,Ch_MOp2:
              if (p.oper[1].typ = top_reg) and
                 (reg32(p.oper[1].reg).enum = reg.enum) then
                begin
                  RegReadByInstruction := true;
                  exit
                end;
            Ch_RWOp3,Ch_ROp3,Ch_MOp3:
              if (p.oper[2].typ = top_reg) and
                 (reg32(p.oper[2].reg).enum = reg.enum) then
                begin
                  RegReadByInstruction := true;
                  exit
                end;
          end;
      end;
  end;
end;

function regInInstruction(r: ToldRegister; p1: Tai): Boolean;
{ Checks if Reg is used by the instruction p1                              }
{ Difference with "regReadBysinstruction() or regModifiedByInstruction()": }
{ this one ignores CH_ALL opcodes, while regModifiedByInstruction doesn't  }
var p: Taicpu;
    opCount: byte;
    reg:Tregister;
begin
  reg.enum:=r;
  reg := reg32(reg);
  regInInstruction := false;
  if p1.typ <> ait_instruction then
    exit;
  p := Taicpu(p1);
  case p.opcode of
    A_IMUL:
      case p.ops of
        1: regInInstruction := (reg.enum = R_EAX) or reginOp(reg,p.oper[0]);
        2,3:
          regInInstruction := regInOp(reg,p.oper[0]) or
            regInOp(reg,p.oper[1]) or regInOp(reg,p.oper[2]);
      end;
    A_IDIV,A_DIV,A_MUL:
      regInInstruction :=
        regInOp(reg,p.oper[0]) or
         (reg.enum in [R_EAX,R_EDX])
    else
      begin
        for opCount := 1 to MaxCh do
          case InsProp[p.opcode].Ch[opCount] of
            CH_REAX..CH_MEDI:
              if tch2reg(InsProp[p.opcode].Ch[opCount]) = reg.enum then
                begin
                  regInInstruction := true;
                  exit;
                end;
            Ch_ROp1..Ch_MOp1:
              if regInOp(reg,p.oper[0]) then
                begin
                  regInInstruction := true;
                  exit
                end;
            Ch_ROp2..Ch_MOp2:
              if regInOp(reg,p.oper[1]) then
                begin
                  regInInstruction := true;
                  exit
                end;
            Ch_ROp3..Ch_MOp3:
              if regInOp(reg,p.oper[2]) then
                begin
                  regInInstruction := true;
                  exit
                end;
          end;
      end;
  end;
end;

Function RegInOp(Reg: TRegister; const o:toper): Boolean;
Begin
  RegInOp := False;
  reg := reg32(reg);
  Case o.typ Of
    top_reg: RegInOp := Reg.enum = reg32(o.reg).enum;
    top_ref: RegInOp := (Reg.enum = o.ref^.Base.enum) Or
                        (Reg.enum = o.ref^.Index.enum);
  End;
End;

Function RegModifiedByInstruction(Reg: TRegister; p1: Tai): Boolean;
Var InstrProp: TInsProp;
    TmpResult: Boolean;
    Cnt: Byte;
Begin
  TmpResult := False;
  Reg := Reg32(Reg);
  If (p1.typ = ait_instruction) Then
    Case Taicpu(p1).opcode of
      A_IMUL:
        With Taicpu(p1) Do
          TmpResult :=
            ((ops = 1) and (reg.enum in [R_EAX,R_EDX])) or
            ((ops = 2) and (Reg32(oper[1].reg).enum = reg.enum)) or
            ((ops = 3) and (Reg32(oper[2].reg).enum = reg.enum));
      A_DIV, A_IDIV, A_MUL:
        With Taicpu(p1) Do
          TmpResult :=
            (Reg.enum in [R_EAX,R_EDX]);
      Else
        Begin
          Cnt := 1;
          InstrProp := InsProp[Taicpu(p1).OpCode];
          While (Cnt <= MaxCh) And
                (InstrProp.Ch[Cnt] <> Ch_None) And
                Not(TmpResult) Do
            Begin
              Case InstrProp.Ch[Cnt] Of
                Ch_WEAX..Ch_MEDI:
                  TmpResult := Reg.enum = TCh2Reg(InstrProp.Ch[Cnt]);
                Ch_RWOp1,Ch_WOp1,Ch_Mop1:
                  TmpResult := (Taicpu(p1).oper[0].typ = top_reg) and
                               (Reg32(Taicpu(p1).oper[0].reg).enum = reg.enum);
                Ch_RWOp2,Ch_WOp2,Ch_Mop2:
                  TmpResult := (Taicpu(p1).oper[1].typ = top_reg) and
                               (Reg32(Taicpu(p1).oper[1].reg).enum = reg.enum);
                Ch_RWOp3,Ch_WOp3,Ch_Mop3:
                  TmpResult := (Taicpu(p1).oper[2].typ = top_reg) and
                               (Reg32(Taicpu(p1).oper[2].reg).enum = reg.enum);
                Ch_FPU: TmpResult := Reg.enum in [R_ST..R_ST7,R_MM0..R_MM7];
                Ch_ALL: TmpResult := true;
              End;
              Inc(Cnt)
            End
        End
    End;
  RegModifiedByInstruction := TmpResult
End;


function instrWritesFlags(p: Tai): boolean;
var
  l: longint;
begin
  instrWritesFlags := true;
  case p.typ of
    ait_instruction:
      begin
        for l := 1 to MaxCh do
          if InsProp[Taicpu(p).opcode].Ch[l] in [Ch_WFlags,Ch_RWFlags,Ch_All] then
            exit;
      end;
    ait_label:
      exit;
    else
      instrWritesFlags := false;
  end;
end;

function instrReadsFlags(p: Tai): boolean;
var
  l: longint;
begin
  instrReadsFlags := true;
  case p.typ of
    ait_instruction:
      begin
        for l := 1 to MaxCh do
          if InsProp[Taicpu(p).opcode].Ch[l] in [Ch_RFlags,Ch_RWFlags,Ch_All] then
            exit;
      end;
    ait_label:
      exit;
    else
      instrReadsFlags := false;
  end;
end;


{********************* GetNext and GetLastInstruction *********************}
Function GetNextInstruction(Current: Tai; Var Next: Tai): Boolean;
{ skips ait_regalloc, ait_regdealloc and ait_stab* objects and puts the }
{ next Tai object in Next. Returns false if there isn't any             }
Begin
  Repeat
    If (Current.typ = ait_marker) And
       (Tai_Marker(current).Kind = AsmBlockStart) Then
      Begin
        GetNextInstruction := False;
        Next := Nil;
        Exit
      End;
    Current := Tai(current.Next);
    While Assigned(Current) And
          ((current.typ In skipInstr) or
           ((current.typ = ait_label) and
            labelCanBeSkipped(Tai_label(current)))) do
      Current := Tai(current.Next);
{    If Assigned(Current) And
       (current.typ = ait_Marker) And
       (Tai_Marker(current).Kind = NoPropInfoStart) Then
      Begin
        While Assigned(Current) And
              ((current.typ <> ait_Marker) Or
               (Tai_Marker(current).Kind <> NoPropInfoEnd)) Do
          Current := Tai(current.Next);
      End;}
  Until Not(Assigned(Current)) Or
        (current.typ <> ait_Marker) Or
        not(Tai_Marker(current).Kind in [NoPropInfoStart,NoPropInfoEnd]);
  Next := Current;
  If Assigned(Current) And
     Not((current.typ In SkipInstr) or
         ((current.typ = ait_label) And
          labelCanBeSkipped(Tai_label(current))))
    Then
      GetNextInstruction :=
         not((current.typ = ait_marker) and
             (Tai_marker(current).kind = asmBlockStart))
    Else
      Begin
        GetNextInstruction := False;
        Next := nil;
      End;
End;

Function GetLastInstruction(Current: Tai; Var Last: Tai): Boolean;
{skips the ait-types in SkipInstr puts the previous Tai object in
 Last. Returns false if there isn't any}
Begin
  Repeat
    Current := Tai(current.previous);
    While Assigned(Current) And
          (((current.typ = ait_Marker) And
            Not(Tai_Marker(current).Kind in [AsmBlockEnd{,NoPropInfoEnd}])) or
           (current.typ In SkipInstr) or
           ((current.typ = ait_label) And
            labelCanBeSkipped(Tai_label(current)))) Do
      Current := Tai(current.previous);
{    If Assigned(Current) And
       (current.typ = ait_Marker) And
       (Tai_Marker(current).Kind = NoPropInfoEnd) Then
      Begin
        While Assigned(Current) And
              ((current.typ <> ait_Marker) Or
               (Tai_Marker(current).Kind <> NoPropInfoStart)) Do
          Current := Tai(current.previous);
      End;}
  Until Not(Assigned(Current)) Or
        (current.typ <> ait_Marker) Or
        not(Tai_Marker(current).Kind in [NoPropInfoStart,NoPropInfoEnd]);
  If Not(Assigned(Current)) or
     (current.typ In SkipInstr) or
     ((current.typ = ait_label) And
      labelCanBeSkipped(Tai_label(current))) or
     ((current.typ = ait_Marker) And
      (Tai_Marker(current).Kind = AsmBlockEnd))
    Then
      Begin
        Last := nil;
        GetLastInstruction := False
      End
    Else
      Begin
        Last := Current;
        GetLastInstruction := True;
      End;
End;

Procedure SkipHead(var P: Tai);
Var OldP: Tai;
Begin
  Repeat
    OldP := P;
    If (p.typ in SkipInstr) Or
       ((p.typ = ait_marker) And
        (Tai_Marker(p).Kind in [AsmBlockEnd,inlinestart,inlineend])) Then
      GetNextInstruction(P, P)
    Else If ((p.Typ = Ait_Marker) And
        (Tai_Marker(p).Kind = nopropinfostart)) Then
   {a marker of the NoPropInfoStart can't be the first instruction of a
    TAAsmoutput list}
      GetNextInstruction(Tai(p.Previous),P);
    Until P = OldP
End;

function labelCanBeSkipped(p: Tai_label): boolean;
begin
  labelCanBeSkipped := not(p.l.is_used) or p.l.is_addr;
end;

{******************* The Data Flow Analyzer functions ********************}

function regLoadedWithNewValue(reg: tregister; canDependOnPrevValue: boolean;
           hp: Tai): boolean;
{ assumes reg is a 32bit register }
var p: Taicpu;
begin
  if reg.enum>lastreg then
    internalerror(200301081);
  if not assigned(hp) or
     (hp.typ <> ait_instruction) then
   begin
     regLoadedWithNewValue := false;
     exit;
   end;
  p := Taicpu(hp);
  regLoadedWithNewValue :=
    (((p.opcode = A_MOV) or
      (p.opcode = A_MOVZX) or
      (p.opcode = A_MOVSX) or
      (p.opcode = A_LEA)) and
     (p.oper[1].typ = top_reg) and
     (Reg32(p.oper[1].reg).enum = reg.enum) and
     (canDependOnPrevValue or
      (p.oper[0].typ <> top_ref) or
      not regInRef(reg,p.oper[0].ref^)) or
     ((p.opcode = A_POP) and
      (Reg32(p.oper[0].reg).enum = reg.enum)));
end;

Procedure UpdateUsedRegs(Var UsedRegs: TRegSet; p: Tai);
{updates UsedRegs with the RegAlloc Information coming after P}
Begin
  Repeat
    While Assigned(p) And
          ((p.typ in (SkipInstr - [ait_RegAlloc])) or
           ((p.typ = ait_label) And
            labelCanBeSkipped(Tai_label(p)))) Do
         p := Tai(p.next);
    While Assigned(p) And
          (p.typ=ait_RegAlloc) Do
      Begin
        if tai_regalloc(p).allocation then
          UsedRegs := UsedRegs + [tai_regalloc(p).Reg.enum]
        else
          UsedRegs := UsedRegs - [tai_regalloc(p).Reg.enum];
        p := Tai(p.next);
      End;
  Until Not(Assigned(p)) Or
        (Not(p.typ in SkipInstr) And
         Not((p.typ = ait_label) And
             labelCanBeSkipped(Tai_label(p))));
End;

Procedure AllocRegBetween(AsmL: TAAsmOutput; Reg: TRegister; p1, p2: Tai);
{ allocates register Reg between (and including) instructions p1 and p2 }
{ the type of p1 and p2 must not be in SkipInstr                        }
var
  hp, start: Tai;
  lastRemovedWasDealloc, firstRemovedWasAlloc, first: boolean;
Begin
  if reg.enum>lastreg then
    internalerror(200301081);
{ If not(reg.enum in rg.usableregsint+[R_EDI,R_ESI]) or
     not(assigned(p1)) then}
 If not(reg.enum in [R_EAX,R_EBX,R_ECX,R_EDX,R_EDI,R_ESI]) or
     not(assigned(p1)) then
    { this happens with registers which are loaded implicitely, outside the }
    { current block (e.g. esi with self)                                    }
    exit;
  { make sure we allocate it for this instruction }
  if p1 = p2 then
    getnextinstruction(p2,p2);
  lastRemovedWasDealloc := false;
  firstRemovedWasAlloc := false;
  first := true;
{$ifdef allocregdebug}
  hp := tai_comment.Create(strpnew('allocating '+std_reg2str[reg.enum]+
    ' from here...')));
  insertllitem(asml,p1.previous,p1,hp);
  hp := tai_comment.Create(strpnew('allocated '+std_reg2str[reg.enum]+
    ' till here...')));
  insertllitem(asml,p2,p1.next,hp);
{$endif allocregdebug}
  start := p1;
  Repeat
    If Assigned(p1.OptInfo) Then
      Include(PTaiProp(p1.OptInfo)^.UsedRegs,Reg.enum);
    p1 := Tai(p1.next);
    Repeat
      While assigned(p1) and
            (p1.typ in (SkipInstr-[ait_regalloc])) Do
        p1 := Tai(p1.next);
{ remove all allocation/deallocation info about the register in between }
      If assigned(p1) and
         (p1.typ = ait_regalloc) Then
        If (tai_regalloc(p1).Reg.enum = Reg.enum) Then
          Begin
            if first then
              begin
                firstRemovedWasAlloc := tai_regalloc(p1).allocation;
                first := false;
              end;
            lastRemovedWasDealloc := not tai_regalloc(p1).allocation;
            hp := Tai(p1.Next);
            asml.Remove(p1);
            p1.free;
            p1 := hp;
          End
        Else p1 := Tai(p1.next);
    Until not(assigned(p1)) or
          Not(p1.typ in SkipInstr);
  Until not(assigned(p1)) or
        (p1 = p2);
  if assigned(p1) then
    begin
      if assigned(p1.optinfo) then
        include(PTaiProp(p1.OptInfo)^.UsedRegs,Reg.enum);
      if lastRemovedWasDealloc then
        begin
          hp := tai_regalloc.DeAlloc(reg);
          insertLLItem(asmL,p1,p1.next,hp);
        end;
    end;
  if firstRemovedWasAlloc then
    begin
      hp := tai_regalloc.Alloc(reg);
      insertLLItem(asmL,start.previous,start,hp);
    end;
End;

function FindRegDealloc(reg: tregister; p: Tai): boolean;
{ assumes reg is a 32bit register }
var
  hp: Tai;
  first: boolean;
begin
  if reg.enum>lastreg then
    internalerror(200301081);
  findregdealloc := false;
  first := true;
  while assigned(p.previous) and
        ((Tai(p.previous).typ in (skipinstr+[ait_align])) or
         ((Tai(p.previous).typ = ait_label) and
          labelCanBeSkipped(Tai_label(p.previous)))) do
    begin
      p := Tai(p.previous);
      if (p.typ = ait_regalloc) and
         (tai_regalloc(p).reg.enum = reg.enum) then
        if not(tai_regalloc(p).allocation) then
          if first then
            begin
              findregdealloc := true;
              break;
            end
          else
            begin
              findRegDealloc :=
                getNextInstruction(p,hp) and
                 regLoadedWithNewValue(reg,false,hp);
              break
            end
        else
          first := false;
    end
end;



Procedure IncState(Var S: Byte; amount: longint);
{Increases S by 1, wraps around at $ffff to 0 (so we won't get overflow
 errors}
Begin
  if (s <= $ff - amount) then
    inc(s, amount)
  else s := longint(s) + amount - $ff;
End;

Function sequenceDependsonReg(Const Content: TContent; seqReg, Reg: TRegister): Boolean;
{ Content is the sequence of instructions that describes the contents of   }
{ seqReg. Reg is being overwritten by the current instruction. If the      }
{ content of seqReg depends on reg (ie. because of a                       }
{ "movl (seqreg,reg), seqReg" instruction), this function returns true     }
Var p: Tai;
    Counter: Byte;
    TmpResult: Boolean;
    RegsChecked: TRegSet;
Begin
  RegsChecked := [];
  p := Content.StartMod;
  TmpResult := False;
  Counter := 1;
  While Not(TmpResult) And
        (Counter <= Content.NrOfMods) Do
    Begin
      If (p.typ = ait_instruction) and
         ((Taicpu(p).opcode = A_MOV) or
          (Taicpu(p).opcode = A_MOVZX) or
          (Taicpu(p).opcode = A_MOVSX) or
          (Taicpu(p).opcode = A_LEA)) and
         (Taicpu(p).oper[0].typ = top_ref) Then
        With Taicpu(p).oper[0].ref^ Do
          If ((Base.enum = current_procinfo.FramePointer.enum) or
              (assigned(symbol) and (base.enum = R_NO))) And
             (Index.enum = R_NO) Then
            Begin
              RegsChecked := RegsChecked + [Reg32(Taicpu(p).oper[1].reg).enum];
              If Reg.enum = Reg32(Taicpu(p).oper[1].reg).enum Then
                Break;
            End
          Else
            tmpResult :=
              regReadByInstruction(reg,p) and
              regModifiedByInstruction(seqReg,p)
      Else
        tmpResult :=
          regReadByInstruction(reg,p) and
          regModifiedByInstruction(seqReg,p);
      Inc(Counter);
      GetNextInstruction(p,p)
    End;
  sequenceDependsonReg := TmpResult
End;

procedure invalidateDependingRegs(p1: pTaiProp; reg: tregister);
var
  counter: Tregister;
begin
  if reg.enum>lastreg then
    internalerror(200301081);
  for counter.enum := R_EAX to R_EDI do
    if counter.enum <> reg.enum then
      with p1^.regs[counter.enum] Do
        begin
          if (typ in [con_ref,con_noRemoveRef]) and
             sequenceDependsOnReg(p1^.Regs[counter.enum],counter,reg) then
            if typ in [con_ref,con_invalid] then
              typ := con_invalid
            { con_invalid and con_noRemoveRef = con_unknown }
            else typ := con_unknown;
          if assigned(memwrite) and
             regInRef(counter,memwrite.oper[1].ref^) then
            memwrite := nil;
        end;
end;

Procedure DestroyReg(p1: PTaiProp; Reg: TRegister; doIncState:Boolean);
{Destroys the contents of the register Reg in the PTaiProp p1, as well as the
 contents of registers are loaded with a memory location based on Reg.
 doIncState is false when this register has to be destroyed not because
 it's contents are directly modified/overwritten, but because of an indirect
 action (e.g. this register holds the contents of a variable and the value
 of the variable in memory is changed) }
Begin
  if reg.enum>lastreg then
    internalerror(200301081);
  Reg := Reg32(Reg);
  { the following happens for fpu registers }
  if (reg.enum < low(NrOfInstrSinceLastMod)) or
     (reg.enum > high(NrOfInstrSinceLastMod)) then
    exit;
  NrOfInstrSinceLastMod[Reg.enum] := 0;
  with p1^.regs[reg.enum] do
    begin
      if doIncState then
        begin
          incState(wstate,1);
          typ := con_unknown;
          startmod := nil;
        end
      else
        if typ in [con_ref,con_const,con_invalid] then
          typ := con_invalid
        { con_invalid and con_noRemoveRef = con_unknown }
        else typ := con_unknown;
      memwrite := nil;
    end;
  invalidateDependingRegs(p1,reg);
End;

{Procedure AddRegsToSet(p: Tai; Var RegSet: TRegSet);
Begin
  If (p.typ = ait_instruction) Then
    Begin
      Case Taicpu(p).oper[0].typ Of
        top_reg:
          If Not(Taicpu(p).oper[0].reg in [R_NO,R_ESP,current_procinfo.FramePointer]) Then
            RegSet := RegSet + [Taicpu(p).oper[0].reg];
        top_ref:
          With TReference(Taicpu(p).oper[0]^) Do
            Begin
              If Not(Base in [current_procinfo.FramePointer,R_NO,R_ESP])
                Then RegSet := RegSet + [Base];
              If Not(Index in [current_procinfo.FramePointer,R_NO,R_ESP])
                Then RegSet := RegSet + [Index];
            End;
      End;
      Case Taicpu(p).oper[1].typ Of
        top_reg:
          If Not(Taicpu(p).oper[1].reg in [R_NO,R_ESP,current_procinfo.FramePointer]) Then
            If RegSet := RegSet + [TRegister(TwoWords(Taicpu(p).oper[1]).Word1];
        top_ref:
          With TReference(Taicpu(p).oper[1]^) Do
            Begin
              If Not(Base in [current_procinfo.FramePointer,R_NO,R_ESP])
                Then RegSet := RegSet + [Base];
              If Not(Index in [current_procinfo.FramePointer,R_NO,R_ESP])
                Then RegSet := RegSet + [Index];
            End;
      End;
    End;
End;}

Function OpsEquivalent(const o1, o2: toper; Var RegInfo: TRegInfo; OpAct: TopAction): Boolean;
Begin {checks whether the two ops are equivalent}
  OpsEquivalent := False;
  if o1.typ=o2.typ then
    Case o1.typ Of
      Top_Reg:
        OpsEquivalent :=RegsEquivalent(o1.reg,o2.reg, RegInfo, OpAct);
      Top_Ref:
        OpsEquivalent := RefsEquivalent(o1.ref^, o2.ref^, RegInfo, OpAct);
      Top_Const:
        OpsEquivalent := o1.val = o2.val;
      Top_None:
        OpsEquivalent := True
    End;
End;


Function OpsEqual(const o1,o2:toper): Boolean;
Begin {checks whether the two ops are equal}
  OpsEqual := False;
  if o1.typ=o2.typ then
    Case o1.typ Of
      Top_Reg :
        OpsEqual:=o1.reg.enum=o2.reg.enum;
      Top_Ref :
        OpsEqual := RefsEqual(o1.ref^, o2.ref^);
      Top_Const :
        OpsEqual:=o1.val=o2.val;
      Top_Symbol :
        OpsEqual:=(o1.sym=o2.sym) and (o1.symofs=o2.symofs);
      Top_None :
        OpsEqual := True
    End;
End;


function sizescompatible(loadsize,newsize: topsize): boolean;
  begin
    case loadsize of
      S_B,S_BW,S_BL:
        sizescompatible := (newsize = loadsize) or (newsize = S_B);
      S_W,S_WL:
        sizescompatible := (newsize = loadsize) or (newsize = S_W);
      else
        sizescompatible := newsize = S_L;
    end;
  end;


function opscompatible(p1,p2: Taicpu): boolean;
begin
  case p1.opcode of
    A_MOVZX,A_MOVSX:
      opscompatible :=
        ((p2.opcode = p1.opcode) or (p2.opcode = A_MOV)) and
        sizescompatible(p1.opsize,p2.opsize);
    else
      opscompatible :=
        (p1.opcode = p2.opcode) and
        (p1.opsize = p2.opsize);
  end;
end;

Function InstructionsEquivalent(p1, p2: Tai; Var RegInfo: TRegInfo): Boolean;
{$ifdef csdebug}
var
  hp: Tai;
{$endif csdebug}
Begin {checks whether two Taicpu instructions are equal}
  If Assigned(p1) And Assigned(p2) And
     (Tai(p1).typ = ait_instruction) And
     (Tai(p2).typ = ait_instruction) And
     opscompatible(Taicpu(p1),Taicpu(p2)) and
     (Taicpu(p1).oper[0].typ = Taicpu(p2).oper[0].typ) And
     (Taicpu(p1).oper[1].typ = Taicpu(p2).oper[1].typ) And
     (Taicpu(p1).oper[2].typ = Taicpu(p2).oper[2].typ)
    Then
 {both instructions have the same structure:
  "<operator> <operand of type1>, <operand of type 2>"}
      If ((Taicpu(p1).opcode = A_MOV) or
          (Taicpu(p1).opcode = A_MOVZX) or
          (Taicpu(p1).opcode = A_MOVSX)  or
          (Taicpu(p1).opcode = A_LEA)) And
         (Taicpu(p1).oper[0].typ = top_ref) {then .oper[1]t = top_reg} Then
        If Not(RegInRef(Taicpu(p1).oper[1].reg, Taicpu(p1).oper[0].ref^)) Then
 {the "old" instruction is a load of a register with a new value, not with
  a value based on the contents of this register (so no "mov (reg), reg")}
          If Not(RegInRef(Taicpu(p2).oper[1].reg, Taicpu(p2).oper[0].ref^)) And
             RefsEqual(Taicpu(p1).oper[0].ref^, Taicpu(p2).oper[0].ref^)
            Then
 {the "new" instruction is also a load of a register with a new value, and
  this value is fetched from the same memory location}
              Begin
                With Taicpu(p2).oper[0].ref^ Do
                  Begin
                    If Not(Base.enum in [current_procinfo.FramePointer.enum, R_NO, R_ESP]) Then
                      RegInfo.RegsLoadedForRef := RegInfo.RegsLoadedForRef + [Base.enum];
                    If Not(Index.enum in [current_procinfo.FramePointer.enum, R_NO, R_ESP]) Then
                      RegInfo.RegsLoadedForRef := RegInfo.RegsLoadedForRef + [Index.enum];
                  End;
 {add the registers from the reference (.oper[0]) to the RegInfo, all registers
  from the reference are the same in the old and in the new instruction
  sequence}
                AddOp2RegInfo(Taicpu(p1).oper[0], RegInfo);
 {the registers from .oper[1] have to be equivalent, but not necessarily equal}
                InstructionsEquivalent :=
                  RegsEquivalent(reg32(Taicpu(p1).oper[1].reg),
                    reg32(Taicpu(p2).oper[1].reg), RegInfo, OpAct_Write);
              End
 {the registers are loaded with values from different memory locations. If
  this was allowed, the instructions "mov -4(esi),eax" and "mov -4(ebp),eax"
  would be considered equivalent}
            Else InstructionsEquivalent := False
        Else
 {load register with a value based on the current value of this register}
          Begin
            With Taicpu(p2).oper[0].ref^ Do
              Begin
                If Not(Base.enum in [current_procinfo.FramePointer.enum,
                     Reg32(Taicpu(p2).oper[1].reg).enum,R_NO,R_ESP]) Then
 {it won't do any harm if the register is already in RegsLoadedForRef}
                  Begin
                    RegInfo.RegsLoadedForRef := RegInfo.RegsLoadedForRef + [Base.enum];
{$ifdef csdebug}
                    Writeln(std_reg2str[base], ' added');
{$endif csdebug}
                  end;
                If Not(Index.enum in [current_procinfo.FramePointer.enum,
                     Reg32(Taicpu(p2).oper[1].reg).enum,R_NO,R_ESP]) Then
                  Begin
                    RegInfo.RegsLoadedForRef := RegInfo.RegsLoadedForRef + [Index.enum];
{$ifdef csdebug}
                    Writeln(std_reg2str[index.enum], ' added');
{$endif csdebug}
                  end;

              End;
            If Not(Reg32(Taicpu(p2).oper[1].reg).enum In [current_procinfo.FramePointer.enum,R_NO,R_ESP])
              Then
                Begin
                  RegInfo.RegsLoadedForRef := RegInfo.RegsLoadedForRef -
                                                 [Reg32(Taicpu(p2).oper[1].reg).enum];
{$ifdef csdebug}
                  Writeln(std_reg2str[Reg32(Taicpu(p2).oper[1].reg)], ' removed');
{$endif csdebug}
                end;
            InstructionsEquivalent :=
               OpsEquivalent(Taicpu(p1).oper[0], Taicpu(p2).oper[0], RegInfo, OpAct_Read) And
               OpsEquivalent(Taicpu(p1).oper[1], Taicpu(p2).oper[1], RegInfo, OpAct_Write)
          End
      Else
 {an instruction <> mov, movzx, movsx}
       begin
  {$ifdef csdebug}
         hp := tai_comment.Create(strpnew('checking if equivalent'));
         hp.previous := p2;
         hp.next := p2^.next;
         p2^.next^.previous := hp;
         p2^.next := hp;
  {$endif csdebug}
         InstructionsEquivalent :=
           OpsEquivalent(Taicpu(p1).oper[0], Taicpu(p2).oper[0], RegInfo, OpAct_Unknown) And
           OpsEquivalent(Taicpu(p1).oper[1], Taicpu(p2).oper[1], RegInfo, OpAct_Unknown) And
           OpsEquivalent(Taicpu(p1).oper[2], Taicpu(p2).oper[2], RegInfo, OpAct_Unknown)
       end
 {the instructions haven't even got the same structure, so they're certainly
  not equivalent}
    Else
      begin
  {$ifdef csdebug}
        hp := tai_comment.Create(strpnew('different opcodes/format'));
        hp.previous := p2;
        hp.next := p2^.next;
        p2^.next^.previous := hp;
        p2^.next := hp;
  {$endif csdebug}
        InstructionsEquivalent := False;
      end;
  {$ifdef csdebug}
    hp := tai_comment.Create(strpnew('instreq: '+tostr(byte(instructionsequivalent))));
    hp.previous := p2;
    hp.next := p2^.next;
    p2^.next^.previous := hp;
    p2^.next := hp;
  {$endif csdebug}
End;

(*
Function InstructionsEqual(p1, p2: Tai): Boolean;
Begin {checks whether two Taicpu instructions are equal}
  InstructionsEqual :=
    Assigned(p1) And Assigned(p2) And
    ((Tai(p1).typ = ait_instruction) And
     (Tai(p1).typ = ait_instruction) And
     (Taicpu(p1).opcode = Taicpu(p2).opcode) And
     (Taicpu(p1).oper[0].typ = Taicpu(p2).oper[0].typ) And
     (Taicpu(p1).oper[1].typ = Taicpu(p2).oper[1].typ) And
     OpsEqual(Taicpu(p1).oper[0].typ, Taicpu(p1).oper[0], Taicpu(p2).oper[0]) And
     OpsEqual(Taicpu(p1).oper[1].typ, Taicpu(p1).oper[1], Taicpu(p2).oper[1]))
End;
*)

Procedure ReadReg(p: PTaiProp; Reg: TRegister);
Begin
  if reg.enum>lastreg then
    internalerror(200301081);
  Reg := Reg32(Reg);
  If Reg.enum in [R_EAX..R_EDI] Then
    incState(p^.regs[Reg.enum].rstate,1)
End;


Procedure ReadRef(p: PTaiProp; Const Ref: PReference);
Begin
  If Ref^.Base.enum <> R_NO Then
    ReadReg(p, Ref^.Base);
  If Ref^.Index.enum <> R_NO Then
    ReadReg(p, Ref^.Index);
End;

Procedure ReadOp(P: PTaiProp;const o:toper);
Begin
  Case o.typ Of
    top_reg: ReadReg(P, o.reg);
    top_ref: ReadRef(P, o.ref);
    top_symbol : ;
  End;
End;


Function RefInInstruction(Const Ref: TReference; p: Tai;
           RefsEq: TRefCompare): Boolean;
{checks whehter Ref is used in P}
Var TmpResult: Boolean;
Begin
  TmpResult := False;
  If (p.typ = ait_instruction) Then
    Begin
      If (Taicpu(p).oper[0].typ = Top_Ref) Then
        TmpResult := RefsEq(Ref, Taicpu(p).oper[0].ref^);
      If Not(TmpResult) And (Taicpu(p).oper[1].typ = Top_Ref) Then
        TmpResult := RefsEq(Ref, Taicpu(p).oper[1].ref^);
      If Not(TmpResult) And (Taicpu(p).oper[2].typ = Top_Ref) Then
        TmpResult := RefsEq(Ref, Taicpu(p).oper[2].ref^);
    End;
  RefInInstruction := TmpResult;
End;

Function RefInSequence(Const Ref: TReference; Content: TContent;
           RefsEq: TRefCompare): Boolean;
{checks the whole sequence of Content (so StartMod and and the next NrOfMods
 Tai objects) to see whether Ref is used somewhere}
Var p: Tai;
    Counter: Byte;
    TmpResult: Boolean;
Begin
  p := Content.StartMod;
  TmpResult := False;
  Counter := 1;
  While Not(TmpResult) And
        (Counter <= Content.NrOfMods) Do
    Begin
      If (p.typ = ait_instruction) And
         RefInInstruction(Ref, p, RefsEq)
        Then TmpResult := True;
      Inc(Counter);
      GetNextInstruction(p,p)
    End;
  RefInSequence := TmpResult
End;

Function ArrayRefsEq(const r1, r2: TReference): Boolean;
Begin
  ArrayRefsEq := (R1.Offset+R1.OffsetFixup = R2.Offset+R2.OffsetFixup) And
                 (R1.Segment.enum = R2.Segment.enum) And
                 (R1.Symbol=R2.Symbol) And
                 (R1.Base.enum = R2.Base.enum)
End;

function isSimpleRef(const ref: treference): boolean;
{ returns true if ref is reference to a local or global variable, to a  }
{ parameter or to an object field (this includes arrays). Returns false }
{ otherwise.                                                            }
begin
  isSimpleRef :=
    assigned(ref.symbol) or
    (ref.base.enum = current_procinfo.framepointer.enum);
end;

function containsPointerRef(p: Tai): boolean;
{ checks if an instruction contains a reference which is a pointer location }
var
  hp: Taicpu;
  count: longint;
begin
  containsPointerRef := false;
  if p.typ <> ait_instruction then
    exit;
  hp := Taicpu(p);
  for count := low(hp.oper) to high(hp.oper) do
    begin
      case hp.oper[count].typ of
        top_ref:
          if not isSimpleRef(hp.oper[count].ref^) then
            begin
              containsPointerRef := true;
              exit;
            end;
        top_none:
          exit;
      end;
    end;
end;


function containsPointerLoad(c: tcontent): boolean;
{ checks whether the contents of a register contain a pointer reference }
var
  p: Tai;
  count: longint;
begin
  containsPointerLoad := false;
  p := c.startmod;
  for count := c.nrOfMods downto 1 do
    begin
      if containsPointerRef(p) then
        begin
          containsPointerLoad := true;
          exit;
        end;
      getnextinstruction(p,p);
    end;
end;

function writeToMemDestroysContents(regWritten: tregister; const ref: treference;
  reg: tregister; const c: tcontent; var invalsmemwrite: boolean): boolean;
{ returns whether the contents c of reg are invalid after regWritten is }
{ is written to ref                                                     }
var
  refsEq: trefCompare;
begin
  reg := reg32(reg);
  regWritten := reg32(regWritten);
  if isSimpleRef(ref) then
    begin
      if (ref.index.enum <> R_NO) or
         (assigned(ref.symbol) and
          (ref.base.enum <> R_NO)) then
        { local/global variable or parameter which is an array }
        refsEq := {$ifdef fpc}@{$endif}arrayRefsEq
      else
        { local/global variable or parameter which is not an array }
        refsEq := {$ifdef fpc}@{$endif}refsEqual;
      invalsmemwrite :=
        assigned(c.memwrite) and
        ((not(cs_uncertainOpts in aktglobalswitches) and
          containsPointerRef(c.memwrite)) or
         refsEq(c.memwrite.oper[1].ref^,ref));
      if not(c.typ in [con_ref,con_noRemoveRef,con_invalid]) then
        begin
          writeToMemDestroysContents := false;
          exit;
        end;

     { write something to a parameter, a local or global variable, so          }
     {  * with uncertain optimizations on:                                     }
     {    - destroy the contents of registers whose contents have somewhere a  }
     {      "mov?? (Ref), %reg". WhichReg (this is the register whose contents }
     {      are being written to memory) is not destroyed if it's StartMod is  }
     {      of that form and NrOfMods = 1 (so if it holds ref, but is not a    }
     {      expression based on Ref)                                           }
     {  * with uncertain optimizations off:                                    }
     {    - also destroy registers that contain any pointer                    }
      with c do
        writeToMemDestroysContents :=
          (typ in [con_ref,con_noRemoveRef]) and
          ((not(cs_uncertainOpts in aktglobalswitches) and
            containsPointerLoad(c)
           ) or
           (refInSequence(ref,c,refsEq) and
            ((reg.enum <> regWritten.enum) or
             not((nrOfMods = 1) and
                 {StarMod is always of the type ait_instruction}
                 (Taicpu(StartMod).oper[0].typ = top_ref) and
                 refsEq(Taicpu(StartMod).oper[0].ref^, ref)
                )
            )
           )
          );
    end
  else
    { write something to a pointer location, so                               }
    {   * with uncertain optimzations on:                                     }
    {     - do not destroy registers which contain a local/global variable or }
    {       a parameter, except if DestroyRefs is called because of a "movsl" }
    {   * with uncertain optimzations off:                                    }
    {     - destroy every register which contains a memory location           }
    begin
      invalsmemwrite :=
        assigned(c.memwrite) and
        (not(cs_UncertainOpts in aktglobalswitches) or
         containsPointerRef(c.memwrite));
      if not(c.typ in [con_ref,con_noRemoveRef,con_invalid]) then
        begin
          writeToMemDestroysContents := false;
          exit;
        end;
      with c do
        writeToMemDestroysContents :=
          (typ in [con_ref,con_noRemoveRef]) and
          (not(cs_UncertainOpts in aktglobalswitches) or
         { for movsl }
           ((ref.base.enum = R_EDI) and (ref.index.enum = R_EDI)) or
         { don't destroy if reg contains a parameter, local or global variable }
           containsPointerLoad(c)
          );
    end;
end;

function writeToRegDestroysContents(destReg: tregister; reg: tregister;
  const c: tcontent): boolean;
{ returns whether the contents c of reg are invalid after destReg is }
{ modified                                                           }
begin
  writeToRegDestroysContents :=
    (c.typ in [con_ref,con_noRemoveRef,con_invalid]) and
    sequenceDependsOnReg(c,reg,reg32(destReg));
end;

function writeDestroysContents(const op: toper; reg: tregister;
  const c: tcontent): boolean;
{ returns whether the contents c of reg are invalid after regWritten is }
{ is written to op                                                      }
var
  dummy: boolean;
  r:Tregister;
begin
  reg := reg32(reg);
  r.enum:=R_NO;
  case op.typ of
    top_reg:
      writeDestroysContents :=
        writeToRegDestroysContents(op.reg,reg,c);
    top_ref:
      writeDestroysContents :=
        writeToMemDestroysContents(r,op.ref^,reg,c,dummy);
  else
    writeDestroysContents := false;
  end;
end;

procedure destroyRefs(p: Tai; const ref: treference; regWritten: tregister);
{ destroys all registers which possibly contain a reference to Ref, regWritten }
{ is the register whose contents are being written to memory (if this proc     }
{ is called because of a "mov?? %reg, (mem)" instruction)                      }
var
  counter: TRegister;
  destroymemwrite: boolean;
begin
  for counter.enum := R_EAX to R_EDI Do
    begin
      if writeToMemDestroysContents(regWritten,ref,counter,
           pTaiProp(p.optInfo)^.regs[counter.enum],destroymemwrite) then
        destroyReg(pTaiProp(p.optInfo), counter, false)
      else if destroymemwrite then
        pTaiProp(p.optinfo)^.regs[counter.enum].MemWrite := nil;
    end;
End;

Procedure DestroyAllRegs(p: PTaiProp; read, written: boolean);
Var Counter: TRegister;
Begin {initializes/desrtoys all registers}
  For Counter.enum := R_EAX To R_EDI Do
    Begin
      if read then
        ReadReg(p, Counter);
      DestroyReg(p, Counter, written);
      p^.regs[counter.enum].MemWrite := nil;
    End;
  p^.DirFlag := F_Unknown;
End;

Procedure DestroyOp(TaiObj: Tai; const o:Toper);
var
{$ifdef statedebug}
    hp: Tai;
{$endif statedebug}
    r:Tregister;

Begin
  Case o.typ Of
    top_reg:
      begin
{$ifdef statedebug}
        hp := tai_comment.Create(strpnew('destroying '+std_reg2str[o.reg]));
        hp.next := Taiobj^.next;
        hp.previous := Taiobj;
        Taiobj^.next := hp;
        if assigned(hp.next) then
          hp.next^.previous := hp;
{$endif statedebug}
        DestroyReg(PTaiProp(TaiObj.OptInfo), reg32(o.reg), true);
      end;
    top_ref:
      Begin
        ReadRef(PTaiProp(TaiObj.OptInfo), o.ref);
        r.enum:=R_NO;
        DestroyRefs(TaiObj, o.ref^, r);
      End;
    top_symbol:;
  End;
End;

Function DFAPass1(AsmL: TAAsmOutput; BlockStart: Tai): Tai;
{gathers the RegAlloc data... still need to think about where to store it to
 avoid global vars}
Var BlockEnd: Tai;
Begin
  BlockEnd := FindLoHiLabels(LoLab, HiLab, LabDif, BlockStart);
  BuildLabelTableAndFixRegAlloc(AsmL, LTable, LoLab, LabDif, BlockStart, BlockEnd);
  DFAPass1 := BlockEnd;
End;

Procedure AddInstr2RegContents({$ifdef statedebug} asml: TAAsmoutput; {$endif}
p: Taicpu; reg: TRegister);
{$ifdef statedebug}
var hp: Tai;
{$endif statedebug}
Begin
  if reg.enum>lastreg then
    internalerror(200301081);
  Reg := Reg32(Reg);
  With PTaiProp(p.optinfo)^.Regs[reg.enum] Do
    if (typ in [con_ref,con_noRemoveRef])
      Then
        Begin
          incState(wstate,1);
 {also store how many instructions are part of the sequence in the first
  instructions PTaiProp, so it can be easily accessed from within
  CheckSequence}
          Inc(NrOfMods, NrOfInstrSinceLastMod[Reg.enum]);
          PTaiProp(Tai(StartMod).OptInfo)^.Regs[Reg.enum].NrOfMods := NrOfMods;
          NrOfInstrSinceLastMod[Reg.enum] := 0;
          invalidateDependingRegs(p.optinfo,reg);
          pTaiprop(p.optinfo)^.regs[reg.enum].memwrite := nil;
{$ifdef StateDebug}
          hp := tai_comment.Create(strpnew(std_reg2str[reg]+': '+tostr(PTaiProp(p.optinfo)^.Regs[reg].WState)
                + ' -- ' + tostr(PTaiProp(p.optinfo)^.Regs[reg].nrofmods))));
          InsertLLItem(AsmL, p, p.next, hp);
{$endif StateDebug}
        End
      Else
        Begin
{$ifdef statedebug}
          hp := tai_comment.Create(strpnew('destroying '+std_reg2str[reg]));
          insertllitem(asml,p,p.next,hp);
{$endif statedebug}
          DestroyReg(PTaiProp(p.optinfo), Reg, true);
{$ifdef StateDebug}
          hp := tai_comment.Create(strpnew(std_reg2str[reg]+': '+tostr(PTaiProp(p.optinfo)^.Regs[reg.enum].WState)));
          InsertLLItem(AsmL, p, p.next, hp);
{$endif StateDebug}
        End
End;

Procedure AddInstr2OpContents({$ifdef statedebug} asml: TAAsmoutput; {$endif}
p: Taicpu; const oper: TOper);
Begin
  If oper.typ = top_reg Then
    AddInstr2RegContents({$ifdef statedebug} asml, {$endif}p, oper.reg)
  Else
    Begin
      ReadOp(PTaiProp(p.optinfo), oper);
      DestroyOp(p, oper);
    End
End;

Procedure DoDFAPass2(
{$Ifdef StateDebug}
AsmL: TAAsmOutput;
{$endif statedebug}
BlockStart, BlockEnd: Tai);
{Analyzes the Data Flow of an assembler list. Starts creating the reg
 contents for the instructions starting with p. Returns the last Tai which has
 been processed}
Var
    CurProp, LastFlagsChangeProp: PTaiProp;
    Cnt, InstrCnt : Longint;
    InstrProp: TInsProp;
    UsedRegs: TRegSet;
    prev,p  : Tai;
    TmpRef: TReference;
    TmpReg: TRegister;
{$ifdef AnalyzeLoops}
    hp : Tai;
    TmpState: Byte;
{$endif AnalyzeLoops}
Begin
  p := BlockStart;
  LastFlagsChangeProp := nil;
  prev := nil;
  UsedRegs := [];
  UpdateUsedregs(UsedRegs, p);
  SkipHead(P);
  BlockStart := p;
  InstrCnt := 1;
  FillChar(NrOfInstrSinceLastMod, SizeOf(NrOfInstrSinceLastMod), 0);
  While (P <> BlockEnd) Do
    Begin
      CurProp := @TaiPropBlock^[InstrCnt];
      If assigned(prev)
        Then
          Begin
{$ifdef JumpAnal}
            If (p.Typ <> ait_label) Then
{$endif JumpAnal}
              Begin
                CurProp^.regs := PTaiProp(prev.OptInfo)^.Regs;
                CurProp^.DirFlag := PTaiProp(prev.OptInfo)^.DirFlag;
                CurProp^.FlagsUsed := false;
              End
          End
        Else
          Begin
            FillChar(CurProp^, SizeOf(CurProp^), 0);
{            For TmpReg := R_EAX to R_EDI Do
              CurProp^.regs[TmpReg].WState := 1;}
          End;
      CurProp^.UsedRegs := UsedRegs;
      CurProp^.CanBeRemoved := False;
      UpdateUsedRegs(UsedRegs, Tai(p.Next));
      For TmpReg.enum := R_EAX To R_EDI Do
        if NrOfInstrSinceLastMod[TmpReg.enum] < 255 then
          Inc(NrOfInstrSinceLastMod[TmpReg.enum])
        else
          begin
            NrOfInstrSinceLastMod[TmpReg.enum] := 0;
            curprop^.regs[TmpReg.enum].typ := con_unknown;
          end;
      Case p.typ Of
        ait_marker:;
        ait_label:
{$Ifndef JumpAnal}
          if not labelCanBeSkipped(Tai_label(p)) then
            DestroyAllRegs(CurProp,false,false);
{$Else JumpAnal}
          Begin
           If not labelCanBeSkipped(Tai_label(p)) Then
             With LTable^[Tai_Label(p).l^.labelnr-LoLab] Do
{$IfDef AnalyzeLoops}
              If (RefsFound = Tai_Label(p).l^.RefCount)
{$Else AnalyzeLoops}
              If (JmpsProcessed = Tai_Label(p).l^.RefCount)
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
                           Not(((hp.typ = ait_instruction)) And
                                (Taicpu_labeled(hp).is_jmp))
                          Then
  {previous instruction not a JMP -> the contents of the registers after the
   previous intruction has been executed have to be taken into account as well}
                            For TmpReg.enum := R_EAX to R_EDI Do
                              Begin
                                If (CurProp^.regs[TmpReg.enum].WState <>
                                    PTaiProp(hp.OptInfo)^.Regs[TmpReg.enum].WState)
                                  Then DestroyReg(CurProp, TmpReg.enum, true)
                              End
                      End
{$IfDef AnalyzeLoops}
                    Else
 {a label from a backward jump (e.g. a loop), no jump to this label has
  already been processed}
                      If GetLastInstruction(p, hp) And
                         Not(hp.typ = ait_instruction) And
                            (Taicpu_labeled(hp).opcode = A_JMP))
                        Then
  {previous instruction not a jmp, so keep all the registers' contents from the
   previous instruction}
                          Begin
                            CurProp^.regs := PTaiProp(hp.OptInfo)^.Regs;
                            CurProp.DirFlag := PTaiProp(hp.OptInfo)^.DirFlag;
                          End
                        Else
  {previous instruction a jmp and no jump to this label processed yet}
                          Begin
                            hp := p;
                            Cnt := InstrCnt;
     {continue until we find a jump to the label or a label which has already
      been processed}
                            While GetNextInstruction(hp, hp) And
                                  Not((hp.typ = ait_instruction) And
                                      (Taicpu(hp).is_jmp) and
                                      (tasmlabel(Taicpu(hp).oper[0].sym).labelnr = Tai_Label(p).l^.labelnr)) And
                                  Not((hp.typ = ait_label) And
                                      (LTable^[Tai_Label(hp).l^.labelnr-LoLab].RefsFound
                                       = Tai_Label(hp).l^.RefCount) And
                                      (LTable^[Tai_Label(hp).l^.labelnr-LoLab].JmpsProcessed > 0)) Do
                              Inc(Cnt);
                            If (hp.typ = ait_label)
                              Then
   {there's a processed label after the current one}
                                Begin
                                  CurProp^.regs := TaiPropBlock^[Cnt].Regs;
                                  CurProp.DirFlag := TaiPropBlock^[Cnt].DirFlag;
                                End
                              Else
   {there's no label anymore after the current one, or they haven't been
    processed yet}
                                Begin
                                  GetLastInstruction(p, hp);
                                  CurProp^.regs := PTaiProp(hp.OptInfo)^.Regs;
                                  CurProp.DirFlag := PTaiProp(hp.OptInfo)^.DirFlag;
                                  DestroyAllRegs(PTaiProp(hp.OptInfo),true,true)
                                End
                          End
{$EndIf AnalyzeLoops}
                Else
{not all references to this label have been found, so destroy all registers}
                  Begin
                    GetLastInstruction(p, hp);
                    CurProp^.regs := PTaiProp(hp.OptInfo)^.Regs;
                    CurProp.DirFlag := PTaiProp(hp.OptInfo)^.DirFlag;
                    DestroyAllRegs(CurProp,true,true)
                  End;
          End;
{$EndIf JumpAnal}

{$ifdef GDB}
        ait_stabs, ait_stabn, ait_stab_function_name:;
{$endif GDB}
        ait_align: ; { may destroy flags !!! }
        ait_instruction:
          Begin
            if Taicpu(p).is_jmp or
               (Taicpu(p).opcode = A_JMP) then
             begin
{$IfNDef JumpAnal}
                for tmpReg.enum := R_EAX to R_EDI do
                  with curProp^.regs[tmpReg.enum] do
                    case typ of
                      con_ref: typ := con_noRemoveRef;
                      con_const: typ := con_noRemoveConst;
                      con_invalid: typ := con_unknown;
                    end;
{$Else JumpAnal}
          With LTable^[tasmlabel(Taicpu(p).oper[0].sym).labelnr-LoLab] Do
            If (RefsFound = tasmlabel(Taicpu(p).oper[0].sym).RefCount) Then
              Begin
                If (InstrCnt < InstrNr)
                  Then
                {forward jump}
                    If (JmpsProcessed = 0) Then
                {no jump to this label has been processed yet}
                      Begin
                        TaiPropBlock^[InstrNr].Regs := CurProp^.regs;
                        TaiPropBlock^[InstrNr].DirFlag := CurProp.DirFlag;
                        Inc(JmpsProcessed);
                      End
                    Else
                      Begin
                        For TmpReg := R_EAX to R_EDI Do
                          If (TaiPropBlock^[InstrNr].Regs[TmpReg].WState <>
                             CurProp^.regs[TmpReg].WState) Then
                            DestroyReg(@TaiPropBlock^[InstrNr], TmpReg, true);
                        Inc(JmpsProcessed);
                      End
{$ifdef AnalyzeLoops}
                  Else
{                backward jump, a loop for example}
{                    If (JmpsProcessed > 0) Or
                       Not(GetLastInstruction(TaiObj, hp) And
                           (hp.typ = ait_labeled_instruction) And
                           (Taicpu_labeled(hp).opcode = A_JMP))
                      Then}
{instruction prior to label is not a jmp, or at least one jump to the label
 has yet been processed}
                        Begin
                          Inc(JmpsProcessed);
                          For TmpReg := R_EAX to R_EDI Do
                            If (TaiPropBlock^[InstrNr].Regs[TmpReg].WState <>
                                CurProp^.regs[TmpReg].WState)
                              Then
                                Begin
                                  TmpState := TaiPropBlock^[InstrNr].Regs[TmpReg].WState;
                                  Cnt := InstrNr;
                                  While (TmpState = TaiPropBlock^[Cnt].Regs[TmpReg].WState) Do
                                    Begin
                                      DestroyReg(@TaiPropBlock^[Cnt], TmpReg, true);
                                      Inc(Cnt);
                                    End;
                                  While (Cnt <= InstrCnt) Do
                                    Begin
                                      Inc(TaiPropBlock^[Cnt].Regs[TmpReg].WState);
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
                              TmpState := TaiPropBlock^[InstrNr].Regs[TmpReg].WState;
                              Cnt := InstrNr;
                              While (TmpState = TaiPropBlock^[Cnt].Regs[TmpReg].WState) Do
                                Begin
                                  TaiPropBlock^[Cnt].Regs[TmpReg] := CurProp^.regs[TmpReg];
                                  Inc(Cnt);
                                End;
                              TmpState := TaiPropBlock^[InstrNr].Regs[TmpReg].WState;
                              While (TmpState = TaiPropBlock^[Cnt].Regs[TmpReg].WState) Do
                                Begin
                                  DestroyReg(@TaiPropBlock^[Cnt], TmpReg, true);
                                  Inc(Cnt);
                                End;
                              While (Cnt <= InstrCnt) Do
                                Begin
                                  Inc(TaiPropBlock^[Cnt].Regs[TmpReg].WState);
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
            InstrProp := InsProp[Taicpu(p).opcode];
            Case Taicpu(p).opcode Of
              A_MOV, A_MOVZX, A_MOVSX:
                Begin
                  Case Taicpu(p).oper[0].typ Of
                    top_ref, top_reg:
                      case Taicpu(p).oper[1].typ Of
                        top_reg:
                          Begin
{$ifdef statedebug}
                            hp := tai_comment.Create(strpnew('destroying '+
                              std_reg2str[Taicpu(p).oper[1].reg])));
                            insertllitem(asml,p,p.next,hp);
{$endif statedebug}

                            readOp(curprop, Taicpu(p).oper[0]);
                            tmpreg := reg32(Taicpu(p).oper[1].reg);
                            if tmpreg.enum>lastreg then
                              internalerror(200301081);
                            if regInOp(tmpreg, Taicpu(p).oper[0]) and
                               (curProp^.regs[tmpReg.enum].typ in [con_ref,con_noRemoveRef]) then
                              begin
                                with curprop^.regs[tmpreg.enum] Do
                                  begin
                                    incState(wstate,1);
 { also store how many instructions are part of the sequence in the first }
 { instruction's PTaiProp, so it can be easily accessed from within       }
 { CheckSequence                                                          }
                                    inc(nrOfMods, nrOfInstrSinceLastMod[tmpreg.enum]);
                                    pTaiprop(startmod.optinfo)^.regs[tmpreg.enum].nrOfMods := nrOfMods;
                                    nrOfInstrSinceLastMod[tmpreg.enum] := 0;
                                   { Destroy the contents of the registers  }
                                   { that depended on the previous value of }
                                   { this register                          }
                                    invalidateDependingRegs(curprop,tmpreg);
                                    curprop^.regs[tmpreg.enum].memwrite := nil;
                                end;
                            end
                          else
                            begin
{$ifdef statedebug}
                              hp := tai_comment.Create(strpnew('destroying & initing '+std_reg2str[tmpreg.enum]));
                              insertllitem(asml,p,p.next,hp);
{$endif statedebug}
                              destroyReg(curprop, tmpreg, true);
                              if not(reginop(tmpreg, Taicpu(p).oper[0])) then
                                with curprop^.regs[tmpreg.enum] Do
                                  begin
                                    typ := con_ref;
                                    startmod := p;
                                    nrOfMods := 1;
                                  end
                            end;
{$ifdef StateDebug}
                  hp := tai_comment.Create(strpnew(std_reg2str[TmpReg.enum]+': '+tostr(CurProp^.regs[TmpReg.enum].WState)));
                  InsertLLItem(AsmL, p, p.next, hp);
{$endif StateDebug}
                          End;
                        Top_Ref:
                          Begin
                            tmpreg.enum:=R_NO;
                            ReadRef(CurProp, Taicpu(p).oper[1].ref);
                            if taicpu(p).oper[0].typ = top_reg then
                              begin
                                ReadReg(CurProp, Taicpu(p).oper[0].reg);
                                DestroyRefs(p, Taicpu(p).oper[1].ref^, Taicpu(p).oper[0].reg);
                                pTaiProp(p.optinfo)^.regs[reg32(Taicpu(p).oper[0].reg).enum].memwrite :=
                                  Taicpu(p);
                              end
                            else
                              DestroyRefs(p, Taicpu(p).oper[1].ref^, tmpreg);
                          End;
                      End;
                    top_symbol,Top_Const:
                      Begin
                        Case Taicpu(p).oper[1].typ Of
                          Top_Reg:
                            Begin
                              TmpReg := Reg32(Taicpu(p).oper[1].reg);
{$ifdef statedebug}
          hp := tai_comment.Create(strpnew('destroying '+std_reg2str[tmpreg]));
          insertllitem(asml,p,p.next,hp);
{$endif statedebug}
                              With CurProp^.regs[TmpReg.enum] Do
                                Begin
                                  DestroyReg(CurProp, TmpReg, true);
                                  typ := Con_Const;
                                  StartMod := p;
                                End
                            End;
                          Top_Ref:
                            Begin
                              tmpreg.enum:=R_NO;
                              ReadRef(CurProp, Taicpu(p).oper[1].ref);
                              DestroyRefs(P, Taicpu(p).oper[1].ref^, tmpreg);
                            End;
                        End;
                      End;
                  End;
                End;
              A_DIV, A_IDIV, A_MUL:
                Begin
                  ReadOp(Curprop, Taicpu(p).oper[0]);
                  tmpreg.enum:=R_EAX;
                  ReadReg(CurProp,tmpreg);
                  If (Taicpu(p).OpCode = A_IDIV) or
                     (Taicpu(p).OpCode = A_DIV) Then
                    begin
                      tmpreg.enum:=R_EDX;
                      ReadReg(CurProp,tmpreg);
                    end;
{$ifdef statedebug}
                  hp := tai_comment.Create(strpnew('destroying eax and edx'));
                  insertllitem(asml,p,p.next,hp);
{$endif statedebug}
{                  DestroyReg(CurProp, R_EAX, true);}
                  tmpreg.enum:=R_EAX;
                  AddInstr2RegContents({$ifdef statedebug}asml,{$endif}
                    Taicpu(p), tmpreg);
                  tmpreg.enum:=R_EDX;
                  DestroyReg(CurProp, tmpreg, true)
                End;
              A_IMUL:
                Begin
                  ReadOp(CurProp,Taicpu(p).oper[0]);
                  ReadOp(CurProp,Taicpu(p).oper[1]);
                  If (Taicpu(p).oper[2].typ = top_none) Then
                    If (Taicpu(p).oper[1].typ = top_none) Then
                      Begin
                        tmpreg.enum:=R_EAX;
                        ReadReg(CurProp,tmpreg);
{$ifdef statedebug}
                        hp := tai_comment.Create(strpnew('destroying eax and edx'));
                        insertllitem(asml,p,p.next,hp);
{$endif statedebug}
{                        DestroyReg(CurProp, R_EAX, true); }
                        AddInstr2RegContents({$ifdef statedebug}asml,{$endif}
                          Taicpu(p), tmpreg);
                        tmpreg.enum:=R_EDX;
                        DestroyReg(CurProp,tmpreg, true)
                      End
                    Else
                      AddInstr2OpContents(
                        {$ifdef statedebug}asml,{$endif}
                          Taicpu(p), Taicpu(p).oper[1])
                  Else
                    AddInstr2OpContents({$ifdef statedebug}asml,{$endif}
                      Taicpu(p), Taicpu(p).oper[2]);
                End;
              A_LEA:
                begin
                  readop(curprop,Taicpu(p).oper[0]);
                  if reginref(Taicpu(p).oper[1].reg,Taicpu(p).oper[0].ref^) then
                    AddInstr2RegContents({$ifdef statedebug}asml,{$endif}
                      Taicpu(p), Taicpu(p).oper[1].reg)
                  else
                    begin
{$ifdef statedebug}
                      hp := tai_comment.Create(strpnew('destroying & initing'+
                        std_reg2str[Taicpu(p).oper[1].reg])));
                      insertllitem(asml,p,p.next,hp);
{$endif statedebug}
                      destroyreg(curprop,Taicpu(p).oper[1].reg,true);
                      with curprop^.regs[Taicpu(p).oper[1].reg.enum] Do
                         begin
                           typ := con_ref;
                           startmod := p;
                           nrOfMods := 1;
                         end
                    end;
                end;
              Else
                Begin
                  Cnt := 1;
                  While (Cnt <= MaxCh) And
                        (InstrProp.Ch[Cnt] <> Ch_None) Do
                    Begin
                      Case InstrProp.Ch[Cnt] Of
                        Ch_REAX..Ch_REDI:
                          begin
                            tmpreg.enum:=TCh2Reg(InstrProp.Ch[Cnt]);
                            ReadReg(CurProp,tmpreg);
                          end;
                        Ch_WEAX..Ch_RWEDI:
                          Begin
                            If (InstrProp.Ch[Cnt] >= Ch_RWEAX) Then
                              begin
                                tmpreg.enum:=TCh2Reg(InstrProp.Ch[Cnt]);
                                ReadReg(CurProp,tmpreg);
                              end;
{$ifdef statedebug}
                            hp := tai_comment.Create(strpnew('destroying '+
                              std_reg2str[TCh2Reg(InstrProp.Ch[Cnt])])));
                            insertllitem(asml,p,p.next,hp);
{$endif statedebug}
                            tmpreg.enum:=TCh2Reg(InstrProp.Ch[Cnt]);
                            DestroyReg(CurProp,tmpreg, true);
                          End;
                        Ch_MEAX..Ch_MEDI:
                          begin
                            tmpreg.enum:=TCh2Reg(InstrProp.Ch[Cnt]);
                            AddInstr2RegContents({$ifdef statedebug} asml,{$endif}
                                                 Taicpu(p),tmpreg);
                          end;
                        Ch_CDirFlag: CurProp^.DirFlag := F_NotSet;
                        Ch_SDirFlag: CurProp^.DirFlag := F_Set;
                        Ch_Rop1: ReadOp(CurProp, Taicpu(p).oper[0]);
                        Ch_Rop2: ReadOp(CurProp, Taicpu(p).oper[1]);
                        Ch_ROp3: ReadOp(CurProp, Taicpu(p).oper[2]);
                        Ch_Wop1..Ch_RWop1:
                          Begin
                            If (InstrProp.Ch[Cnt] in [Ch_RWop1]) Then
                              ReadOp(CurProp, Taicpu(p).oper[0]);
                            DestroyOp(p, Taicpu(p).oper[0]);
                          End;
                        Ch_Mop1:
                          AddInstr2OpContents({$ifdef statedebug} asml, {$endif}
                          Taicpu(p), Taicpu(p).oper[0]);
                        Ch_Wop2..Ch_RWop2:
                          Begin
                            If (InstrProp.Ch[Cnt] = Ch_RWop2) Then
                              ReadOp(CurProp, Taicpu(p).oper[1]);
                            DestroyOp(p, Taicpu(p).oper[1]);
                          End;
                        Ch_Mop2:
                          AddInstr2OpContents({$ifdef statedebug} asml, {$endif}
                          Taicpu(p), Taicpu(p).oper[1]);
                        Ch_WOp3..Ch_RWOp3:
                          Begin
                            If (InstrProp.Ch[Cnt] = Ch_RWOp3) Then
                              ReadOp(CurProp, Taicpu(p).oper[2]);
                            DestroyOp(p, Taicpu(p).oper[2]);
                          End;
                        Ch_Mop3:
                          AddInstr2OpContents({$ifdef statedebug} asml, {$endif}
                          Taicpu(p), Taicpu(p).oper[2]);
                        Ch_WMemEDI:
                          Begin
                            tmpreg.enum:=R_EDI;
                            ReadReg(CurProp, tmpreg);
                            FillChar(TmpRef, SizeOf(TmpRef), 0);
                            TmpRef.Base.enum := R_EDI;
                            tmpRef.index.enum := R_EDI;
                            tmpreg.enum:=R_NO;
                            DestroyRefs(p, TmpRef,tmpreg)
                          End;
                        Ch_RFlags:
                          if assigned(LastFlagsChangeProp) then
                            LastFlagsChangeProp^.FlagsUsed := true;
                        Ch_WFlags:
                          LastFlagsChangeProp := CurProp;
                        Ch_RWFlags:
                          begin
                            if assigned(LastFlagsChangeProp) then
                              LastFlagsChangeProp^.FlagsUsed := true;
                            LastFlagsChangeProp := CurProp;
                          end;
                         Ch_FPU:;
                        Else
                          Begin
{$ifdef statedebug}
                            hp := tai_comment.Create(strpnew(
                              'destroying all regs for prev instruction')));
                            insertllitem(asml,p, p.next,hp);
{$endif statedebug}
                            DestroyAllRegs(CurProp,true,true);
                            LastFlagsChangeProp := CurProp;
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
{$ifdef statedebug}
            hp := tai_comment.Create(strpnew(
              'destroying all regs: unknown Tai: '+tostr(ord(p.typ)))));
            insertllitem(asml,p, p.next,hp);
{$endif statedebug}
            DestroyAllRegs(CurProp,true,true);
          End;
      End;
      Inc(InstrCnt);
      prev := p;
      GetNextInstruction(p, p);
    End;
End;

Function InitDFAPass2(BlockStart, BlockEnd: Tai): Boolean;
{reserves memory for the PTaiProps in one big memory block when not using
 TP, returns False if not enough memory is available for the optimizer in all
 cases}
Var p: Tai;
    Count: Longint;
{    TmpStr: String; }
Begin
  P := BlockStart;
  SkipHead(P);
  NrOfTaiObjs := 0;
  While (P <> BlockEnd) Do
    Begin
{$IfDef JumpAnal}
      Case p.Typ Of
        ait_label:
          Begin
            If not labelCanBeSkipped(Tai_label(p)) Then
              LTable^[Tai_Label(p).l^.labelnr-LoLab].InstrNr := NrOfTaiObjs
          End;
        ait_instruction:
          begin
            if Taicpu(p).is_jmp then
             begin
               If (tasmlabel(Taicpu(p).oper[0].sym).labelnr >= LoLab) And
                  (tasmlabel(Taicpu(p).oper[0].sym).labelnr <= HiLab) Then
                 Inc(LTable^[tasmlabel(Taicpu(p).oper[0].sym).labelnr-LoLab].RefsFound);
             end;
          end;
{        ait_instruction:
          Begin
           If (Taicpu(p).opcode = A_PUSH) And
              (Taicpu(p).oper[0].typ = top_symbol) And
              (PCSymbol(Taicpu(p).oper[0])^.offset = 0) Then
             Begin
               TmpStr := StrPas(PCSymbol(Taicpu(p).oper[0])^.symbol);
               If}
      End;
{$EndIf JumpAnal}
      Inc(NrOfTaiObjs);
      GetNextInstruction(p, p);
    End;
{Uncomment the next line to see how much memory the reloading optimizer needs}
{  Writeln(NrOfTaiObjs*SizeOf(TTaiProp));}
{no need to check mem/maxavail, we've got as much virtual memory as we want}
  If NrOfTaiObjs <> 0 Then
    Begin
      InitDFAPass2 := True;
      GetMem(TaiPropBlock, NrOfTaiObjs*SizeOf(TTaiProp));
      fillchar(TaiPropBlock^,NrOfTaiObjs*SizeOf(TTaiProp),0);
      p := BlockStart;
      SkipHead(p);
      For Count := 1 To NrOfTaiObjs Do
        Begin
          PTaiProp(p.OptInfo) := @TaiPropBlock^[Count];
          GetNextInstruction(p, p);
        End;
    End
  Else InitDFAPass2 := False;
End;

Function DFAPass2(
{$ifdef statedebug}
                   AsmL: TAAsmOutPut;
{$endif statedebug}
                                      BlockStart, BlockEnd: Tai): Boolean;
Begin
  If InitDFAPass2(BlockStart, BlockEnd) Then
    Begin
      DoDFAPass2(
{$ifdef statedebug}
         asml,
{$endif statedebug}
         BlockStart, BlockEnd);
      DFAPass2 := True
    End
  Else DFAPass2 := False;
End;

Procedure ShutDownDFA;
Begin
  If LabDif <> 0 Then
    FreeMem(LTable, LabDif*SizeOf(TLabelTableItem));
End;

End.

{
  $Log$
  Revision 1.51  2003-06-03 21:09:05  peter
    * internal changeregsize for optimizer
    * fix with a hack to not remove the first instruction of a block
      which will leave blockstart pointing to invalid memory

  Revision 1.50  2003/05/26 21:17:18  peter
    * procinlinenode removed
    * aktexit2label removed, fast exit removed
    + tcallnode.inlined_pass_2 added

  Revision 1.49  2003/04/27 11:21:35  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.48  2003/03/28 19:16:57  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.47  2003/02/26 21:15:43  daniel
    * Fixed the optimizer

  Revision 1.46  2003/02/19 22:00:15  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.45  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.44  2002/11/17 16:31:59  carl
    * memory optimization (3-4%) : cleanup of tai fields,
       cleanup of tdef and tsym fields.
    * make it work for m68k

  Revision 1.43  2002/08/18 20:06:29  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.42  2002/08/17 09:23:44  florian
    * first part of procinfo rewrite

  Revision 1.41  2002/07/01 18:46:31  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.40  2002/06/24 12:43:00  jonas
    * fixed errors found with new -CR code from Peter when cycling with -O2p3r

  Revision 1.39  2002/06/09 12:56:04  jonas
    * IDIV reads edx too (but now the div/mod optimization fails :/ )

  Revision 1.38  2002/05/18 13:34:22  peter
    * readded missing revisions

  Revision 1.37  2002/05/16 19:46:51  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.34  2002/05/12 16:53:16  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.33  2002/04/21 15:32:59  carl
  * changeregsize -> changeregsize

  Revision 1.32  2002/04/20 21:37:07  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant
  * removing frame pointer in routines is only available for : i386,m68k and vis targets

  Revision 1.31  2002/04/15 19:44:20  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.30  2002/04/15 19:12:09  carl
  + target_info.size_of_pointer -> pointer_size
  + some cleanup of unused types/variables
  * move several constants from cpubase to their specific units
    (where they are used)
  + att_Reg2str -> gas_reg2str
  + int_reg2str -> std_reg2str

  Revision 1.29  2002/04/14 17:00:49  carl
  + att_reg2str -> std_reg2str

  Revision 1.28  2002/04/02 17:11:34  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.27  2002/03/31 20:26:38  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

  Revision 1.26  2002/03/04 19:10:13  peter
    * removed compiler warnings

}
