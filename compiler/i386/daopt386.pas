{
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
    Foundation, inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit daopt386;

{$i fpcdefs.inc}

interface

uses
  globtype,
  cclasses,aasmbase,aasmtai,aasmdata,aasmcpu,cgbase,cgutils,
  cpubase;

{******************************* Constants *******************************}

const

{ Possible register content types }
  con_Unknown = 0;
  con_ref = 1;
  con_const = 2;
  { The contents aren't usable anymore for CSE, but they may still be   }
  { useful for detecting whether the result of a load is actually used }
  con_invalid = 3;
  { the reverse of the above (in case a (conditional) jump is encountered): }
  { CSE is still possible, but the original instruction can't be removed    }
  con_noRemoveRef = 4;
  { same, but for constants }
  con_noRemoveConst = 5;


const
  topsize2tcgsize: array[topsize] of tcgsize = (OS_NO,
    OS_8,OS_16,OS_32,OS_64,OS_16,OS_32,OS_32,
    OS_16,OS_32,OS_64,
    OS_F32,OS_F64,OS_F80,OS_C64,OS_F128,
    OS_M32,
    OS_ADDR,OS_NO,OS_NO,
    OS_NO,
    OS_NO,
    OS_NO);



{********************************* Types *********************************}

type
  TRegEnum = RS_EAX..RS_ESP;
  TRegArray = Array[TRegEnum] of tsuperregister;
  TRegSet = Set of TRegEnum;
  toptreginfo = Record
                NewRegsEncountered, OldRegsEncountered: TRegSet;
                RegsLoadedForRef: TRegSet;
                lastReload: array[RS_EAX..RS_ESP] of tai;
                New2OldReg: TRegArray;
              end;

{possible actions on an operand: read, write or modify (= read & write)}
  TOpAction = (OpAct_Read, OpAct_Write, OpAct_Modify, OpAct_Unknown);

{the possible states of a flag}
  TFlagContents = (F_Unknown, F_notSet, F_Set);

  TContent = Packed Record
      {start and end of block instructions that defines the
       content of this register.}
               StartMod: tai;
               MemWrite: taicpu;
      {how many instructions starting with StarMod does the block consist of}
               NrOfMods: Word;
      {the type of the content of the register: unknown, memory, constant}
               Typ: Byte;
               case byte of
      {starts at 0, gets increased everytime the register is written to}
                 1: (WState: Byte;
      {starts at 0, gets increased everytime the register is read from}
                       RState: Byte);
      { to compare both states in one operation }
                 2: (state: word);
             end;

{Contents of the integer registers}
  TRegContent = Array[RS_EAX..RS_ESP] Of TContent;

{contents of the FPU registers}
//  TRegFPUContent = Array[RS_ST..RS_ST7] Of TContent;

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

{information record with the contents of every register. Every tai object
 gets one of these assigned: a pointer to it is stored in the OptInfo field}
  TtaiProp = Record
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
             end;

  ptaiprop = ^TtaiProp;

  TtaiPropBlock = Array[1..250000] Of TtaiProp;
  PtaiPropBlock = ^TtaiPropBlock;

  TInstrSinceLastMod = Array[RS_EAX..RS_ESP] Of Word;

  TLabelTableItem = Record
                      taiObj: tai;
{$ifDef JumpAnal}
                      InstrNr: Longint;
                      RefsFound: Word;
                      JmpsProcessed: Word
{$endif JumpAnal}
                    end;
  TLabelTable = Array[0..2500000] Of TLabelTableItem;
  PLabelTable = ^TLabelTable;


{*********************** procedures and functions ************************}

procedure InsertLLItem(AsmL: TAsmList; prev, foll, new_one: TLinkedListItem);


function RefsEqual(const R1, R2: TReference): Boolean;
function isgp32reg(supreg: tsuperregister): Boolean;
function reginref(supreg: tsuperregister; const ref: treference): boolean;
function RegReadByInstruction(supreg: tsuperregister; hp: tai): boolean;
function RegModifiedByInstruction(supreg: tsuperregister; p1: tai): boolean;
function RegInInstruction(supreg: tsuperregister; p1: tai): boolean;
function reginop(supreg: tsuperregister; const o:toper): boolean;
function instrWritesFlags(p: tai): boolean;
function instrReadsFlags(p: tai): boolean;

function writeToMemDestroysContents(regWritten: tsuperregister; const ref: treference;
  supreg: tsuperregister; size: tcgsize; const c: tcontent; var invalsmemwrite: boolean): boolean;
function writeToRegDestroysContents(destReg, supreg: tsuperregister;
  const c: tcontent): boolean;
function writeDestroysContents(const op: toper; supreg: tsuperregister; size: tcgsize;
  const c: tcontent; var memwritedestroyed: boolean): boolean;

function sequenceDependsonReg(const Content: TContent; seqreg: tsuperregister; supreg: tsuperregister): Boolean;

function GetNextInstruction(Current: tai; var Next: tai): Boolean;
function GetLastInstruction(Current: tai; var Last: tai): Boolean;
procedure SkipHead(var p: tai);
function labelCanBeSkipped(p: tai_label): boolean;

procedure RemoveLastDeallocForFuncRes(asmL: TAsmList; p: tai);
function regLoadedWithNewValue(supreg: tsuperregister; canDependOnPrevValue: boolean;
           hp: tai): boolean;
procedure UpdateUsedRegs(var UsedRegs: TRegSet; p: tai);
procedure AllocRegBetween(asml: TAsmList; reg: tregister; p1, p2: tai; var initialusedregs: tregset);
function FindRegDealloc(supreg: tsuperregister; p: tai): boolean;

function InstructionsEquivalent(p1, p2: tai; var RegInfo: toptreginfo): Boolean;
function sizescompatible(loadsize,newsize: topsize): boolean;
function OpsEqual(const o1,o2:toper): Boolean;


type
  tdfaobj = class
    constructor create(_list: TAsmList); virtual;

    function pass_1(_blockstart: tai): tai;
    function pass_generate_code: boolean;
    procedure clear;

    function getlabelwithsym(sym: tasmlabel): tai;

   private
    { asm list we're working on }
    list: TAsmList;

    { current part of the asm list }
    blockstart, blockend: tai;

    { the amount of taiObjects in the current part of the assembler list }
    nroftaiobjs: longint;

    { Array which holds all TtaiProps }
    taipropblock: ptaipropblock;

    { all labels in the current block: their value mapped to their location }
    lolab, hilab, labdif: longint;
    labeltable: plabeltable;

    { Walks through the list to find the lowest and highest label number, inits the }
    { labeltable and fixes/optimizes some regallocs                                 }
     procedure initlabeltable;

    function initdfapass2: boolean;
    procedure dodfapass2;
  end;


function FindLabel(L: tasmlabel; var hp: tai): Boolean;

procedure incState(var S: Byte; amount: longint);

{******************************* Variables *******************************}

var
  dfa: tdfaobj;

{*********************** end of Interface section ************************}


Implementation

Uses
{$ifdef csdebug}
  cutils,
{$else}
  {$ifdef statedebug}
    cutils,
  {$else}
    {$ifdef allocregdebug}
      cutils,
    {$endif}
  {$endif}
{$endif}
  globals, systems, verbose, symconst, cgobj,procinfo;

Type
  TRefCompare = function(const r1, r2: treference; size1, size2: tcgsize): boolean;

var
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

  function TSearchLinkedList.FindByValue(p: PSearchLinkedListItem): boolean;
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

procedure updateTempAllocs(var UsedRegs: TRegSet; p: tai);
{updates UsedRegs with the RegAlloc Information coming after p}
begin
  repeat
    while assigned(p) and
          ((p.typ in (SkipInstr - [ait_RegAlloc])) or
           ((p.typ = ait_label) and
            labelCanBeSkipped(tai_label(current)))) Do
         p := tai(p.next);
    while assigned(p) and
          (p.typ=ait_RegAlloc) Do
      begin
        case tai_regalloc(p).ratype of
          ra_alloc :
            Include(UsedRegs, TRegEnum(getsupreg(tai_regalloc(p).reg)));
          ra_dealloc :
            Exclude(UsedRegs, TRegEnum(getsupreg(tai_regalloc(p).reg)));
        end;
        p := tai(p.next);
      end;
  until not(assigned(p)) or
        (not(p.typ in SkipInstr) and
         not((p.typ = ait_label) and
             labelCanBeSkipped(tai_label(current))));
end;

{$endif tempOpts}

{************************ Create the Label table ************************}

function findregalloc(supreg: tsuperregister; starttai: tai; ratyp: tregalloctype): boolean;
{ Returns true if a ait_alloc object for reg is found in the block of tai's }
{ starting with Starttai and ending with the next "real" instruction        }
begin
  findregalloc := false;
  repeat
    while assigned(starttai) and
          ((starttai.typ in (skipinstr - [ait_regalloc])) or
           ((starttai.typ = ait_label) and
            labelcanbeskipped(tai_label(starttai)))) do
      starttai := tai(starttai.next);
    if assigned(starttai) and
       (starttai.typ = ait_regalloc) then
      begin
        if (tai_regalloc(Starttai).ratype = ratyp) and
           (getsupreg(tai_regalloc(Starttai).reg) = supreg) then
          begin
            findregalloc:=true;
            break;
          end;
        starttai := tai(starttai.next);
      end
    else
      break;
  until false;
end;

procedure RemoveLastDeallocForFuncRes(asml: TAsmList; p: tai);

  procedure DoRemoveLastDeallocForFuncRes(asml: TAsmList; supreg: tsuperregister);
  var
    hp2: tai;
  begin
    hp2 := p;
    repeat
      hp2 := tai(hp2.previous);
      if assigned(hp2) and
         (hp2.typ = ait_regalloc) and
         (tai_regalloc(hp2).ratype=ra_dealloc) and
         (getregtype(tai_regalloc(hp2).reg) = R_INTREGISTER) and
         (getsupreg(tai_regalloc(hp2).reg) = supreg) then
        begin
          asml.remove(hp2);
          hp2.free;
          break;
        end;
    until not(assigned(hp2)) or regInInstruction(supreg,hp2);
  end;

begin
    case current_procinfo.procdef.returndef.typ of
      arraydef,recorddef,pointerdef,
         stringdef,enumdef,procdef,objectdef,errordef,
         filedef,setdef,procvardef,
         classrefdef,forwarddef:
        DoRemoveLastDeallocForFuncRes(asml,RS_EAX);
      orddef:
        if current_procinfo.procdef.returndef.size <> 0 then
          begin
            DoRemoveLastDeallocForFuncRes(asml,RS_EAX);
            { for int64/qword }
            if current_procinfo.procdef.returndef.size = 8 then
              DoRemoveLastDeallocForFuncRes(asml,RS_EDX);
          end;
    end;
end;

procedure getNoDeallocRegs(var regs: tregset);
var
  regCounter: TSuperRegister;
begin
  regs := [];
  case current_procinfo.procdef.returndef.typ of
    arraydef,recorddef,pointerdef,
       stringdef,enumdef,procdef,objectdef,errordef,
       filedef,setdef,procvardef,
       classrefdef,forwarddef:
     regs := [RS_EAX];
    orddef:
      if current_procinfo.procdef.returndef.size <> 0 then
        begin
          regs := [RS_EAX];
          { for int64/qword }
          if current_procinfo.procdef.returndef.size = 8 then
            regs := regs + [RS_EDX];
        end;
  end;
  for regCounter := RS_EAX to RS_EBX do
{    if not(regCounter in rg.usableregsint) then}
      include(regs,regcounter);
end;


procedure AddRegDeallocFor(asml: TAsmList; reg: tregister; p: tai);
var
  hp1: tai;
  funcResRegs: tregset;
{  funcResReg: boolean;}
begin
{ if not(supreg in rg.usableregsint) then
    exit;}
{ if not(supreg in [RS_EDI]) then
    exit;}
  getNoDeallocRegs(funcresregs);
{  funcResRegs := funcResRegs - rg.usableregsint;}
{  funcResRegs := funcResRegs - [RS_EDI];}
{  funcResRegs := funcResRegs - [RS_EAX,RS_EBX,RS_ECX,RS_EDX,RS_ESI]; }
{  funcResReg := getsupreg(reg) in funcresregs;}

  hp1 := p;
{


  while not(funcResReg and
            (p.typ = ait_instruction) and
            (taicpu(p).opcode = A_JMP) and
            (tasmlabel(taicpu(p).oper[0]^.sym) = aktexit2label)) and
        getLastInstruction(p, p) and
        not(regInInstruction(supreg, p)) do
    hp1 := p;
}
  { don't insert a dealloc for registers which contain the function result }
  { if they are followed by a jump to the exit label (for exit(...))       }
{  if not(funcResReg) or
     not((hp1.typ = ait_instruction) and
         (taicpu(hp1).opcode = A_JMP) and
         (tasmlabel(taicpu(hp1).oper[0]^.sym) = aktexit2label)) then }
    begin
      p := tai_regalloc.deAlloc(reg,nil);
      insertLLItem(AsmL, hp1.previous, hp1, p);
    end;
end;



{************************ Search the Label table ************************}

function findlabel(l: tasmlabel; var hp: tai): boolean;

{searches for the specified label starting from hp as long as the
 encountered instructions are labels, to be able to optimize constructs like

 jne l2              jmp l2
 jmp l3     and      l1:
 l1:                 l2:
 l2:}

var
  p: tai;

begin
  p := hp;
  while assigned(p) and
       (p.typ in SkipInstr + [ait_label,ait_align]) Do
    if (p.typ <> ait_Label) or
       (tai_label(p).labsym <> l) then
      GetNextInstruction(p, p)
    else
       begin
          hp := p;
          findlabel := true;
          exit
        end;
  findlabel := false;
end;

{************************ Some general functions ************************}

function tch2reg(ch: tinschange): tsuperregister;
{converts a TChange variable to a TRegister}
const
  ch2reg: array[CH_REAX..CH_REDI] of tsuperregister = (RS_EAX,RS_ECX,RS_EDX,RS_EBX,RS_ESP,RS_EBP,RS_ESI,RS_EDI);
begin
  if (ch <= CH_REDI) then
    tch2reg := ch2reg[ch]
  else if (ch <= CH_WEDI) then
    tch2reg := ch2reg[tinschange(ord(ch) - ord(CH_REDI))]
  else if (ch <= CH_RWEDI) then
    tch2reg := ch2reg[tinschange(ord(ch) - ord(CH_WEDI))]
  else if (ch <= CH_MEDI) then
    tch2reg := ch2reg[tinschange(ord(ch) - ord(CH_RWEDI))]
  else
    InternalError($db)
end;


{ inserts new_one between prev and foll }

procedure InsertLLItem(AsmL: TAsmList; prev, foll, new_one: TLinkedListItem);
begin
  if assigned(prev) then
    if assigned(foll) then
      begin
        if assigned(new_one) then
          begin
            new_one.previous := prev;
            new_one.next := foll;
            prev.next := new_one;
            foll.previous := new_one;
            { shgould we update line information }
            if (not (tai(new_one).typ in SkipLineInfo)) and
               (not (tai(foll).typ in SkipLineInfo)) then
            tailineinfo(new_one).fileinfo := tailineinfo(foll).fileinfo;
          end;
      end
    else
      asml.Concat(new_one)
  else
    if assigned(foll) then
      asml.Insert(new_one)
end;

{********************* Compare parts of tai objects *********************}

function regssamesize(reg1, reg2: tregister): boolean;
{returns true if Reg1 and Reg2 are of the same size (so if they're both
 8bit, 16bit or 32bit)}
begin
  if (reg1 = NR_NO) or (reg2 = NR_NO) then
    internalerror(2003111602);
  regssamesize := getsubreg(reg1) = getsubreg(reg2);
end;


procedure AddReg2RegInfo(OldReg, NewReg: TRegister; var RegInfo: toptreginfo);
{updates the ???RegsEncountered and ???2???reg fields of RegInfo. Assumes that
 OldReg and NewReg have the same size (has to be chcked in advance with
 RegsSameSize) and that neither equals RS_INVALID}
var
  newsupreg, oldsupreg: tsuperregister;
begin
  if (newreg = NR_NO) or (oldreg = NR_NO) then
    internalerror(2003111601);
  newsupreg := getsupreg(newreg);
  oldsupreg := getsupreg(oldreg);
  with RegInfo Do
    begin
      NewRegsEncountered := NewRegsEncountered + [newsupreg];
      OldRegsEncountered := OldRegsEncountered + [oldsupreg];
      New2OldReg[newsupreg] := oldsupreg;
    end;
end;


procedure AddOp2RegInfo(const o:toper; var reginfo: toptreginfo);
begin
  case o.typ Of
    top_reg:
      if (o.reg <> NR_NO) then
        AddReg2RegInfo(o.reg, o.reg, RegInfo);
    top_ref:
      begin
        if o.ref^.base <> NR_NO then
          AddReg2RegInfo(o.ref^.base, o.ref^.base, RegInfo);
        if o.ref^.index <> NR_NO then
          AddReg2RegInfo(o.ref^.index, o.ref^.index, RegInfo);
      end;
  end;
end;


function RegsEquivalent(oldreg, newreg: tregister; const oldinst, newinst: taicpu; var reginfo: toptreginfo; opact: topaction): Boolean;
begin
  if not((oldreg = NR_NO) or (newreg = NR_NO)) then
    if RegsSameSize(oldreg, newreg) then
      with reginfo do
{here we always check for the 32 bit component, because it is possible that
 the 8 bit component has not been set, event though NewReg already has been
 processed. This happens if it has been compared with a register that doesn't
 have an 8 bit component (such as EDI). in that case the 8 bit component is
 still set to RS_NO and the comparison in the else-part will fail}
        if (getsupreg(oldReg) in OldRegsEncountered) then
          if (getsupreg(NewReg) in NewRegsEncountered) then
            RegsEquivalent := (getsupreg(oldreg) = New2OldReg[getsupreg(newreg)])

 { if we haven't encountered the new register yet, but we have encountered the
   old one already, the new one can only be correct if it's being written to
   (and consequently the old one is also being written to), otherwise

   movl -8(%ebp), %eax        and         movl -8(%ebp), %eax
   movl (%eax), %eax                      movl (%edx), %edx

   are considered equivalent}

          else
            if (opact = opact_write) then
              begin
                AddReg2RegInfo(oldreg, newreg, reginfo);
                RegsEquivalent := true
              end
            else
              Regsequivalent := false
        else
           if not(getsupreg(newreg) in NewRegsEncountered) and
              ((opact = opact_write) or
               ((newreg = oldreg) and
                (ptaiprop(oldinst.optinfo)^.regs[getsupreg(oldreg)].wstate =
                 ptaiprop(newinst.optinfo)^.regs[getsupreg(oldreg)].wstate) and
                not(regmodifiedbyinstruction(getsupreg(oldreg),oldinst)))) then
             begin
               AddReg2RegInfo(oldreg, newreg, reginfo);
               RegsEquivalent := true
             end
           else
             RegsEquivalent := false
    else
      RegsEquivalent := false
  else
    RegsEquivalent := oldreg = newreg
end;


function RefsEquivalent(const r1, r2: treference; const oldinst, newinst: taicpu; var regInfo: toptreginfo): boolean;
begin
  RefsEquivalent :=
    (r1.offset = r2.offset) and
    RegsEquivalent(r1.base, r2.base, oldinst, newinst, reginfo, OpAct_Read) and
    RegsEquivalent(r1.index, r2.index, oldinst, newinst, reginfo, OpAct_Read) and
    (r1.segment = r2.segment) and (r1.scalefactor = r2.scalefactor) and
    (r1.symbol = r2.symbol) and (r1.refaddr = r2.refaddr) and
    (r1.relsymbol = r2.relsymbol);
end;


function refsequal(const r1, r2: treference): boolean;
begin
  refsequal :=
    (r1.offset = r2.offset) and
    (r1.segment = r2.segment) and (r1.base = r2.base) and
    (r1.index = r2.index) and (r1.scalefactor = r2.scalefactor) and
    (r1.symbol=r2.symbol) and (r1.refaddr = r2.refaddr) and
    (r1.relsymbol = r2.relsymbol);
end;


{$push}
{$q-}

// checks whether a write to r2 of size "size" contains address r1
function refsoverlapping(const r1, r2: treference; size1, size2: tcgsize): boolean;
var
  realsize1, realsize2: aint;
begin
  realsize1 := tcgsize2size[size1];
  realsize2 := tcgsize2size[size2];
  refsoverlapping :=
    (r2.offset <= r1.offset+realsize1) and
    (r1.offset <= r2.offset+realsize2) and
    (r1.segment = r2.segment) and (r1.base = r2.base) and
    (r1.index = r2.index) and (r1.scalefactor = r2.scalefactor) and
    (r1.symbol=r2.symbol) and (r1.refaddr = r2.refaddr) and
    (r1.relsymbol = r2.relsymbol);
end;

{$pop}


function isgp32reg(supreg: tsuperregister): boolean;
{Checks if the register is a 32 bit general purpose register}
begin
  isgp32reg := false;
{$push}{$warnings off}
  if (supreg >= RS_EAX) and (supreg <= RS_EBX) then
    isgp32reg := true
{$pop}
end;


function reginref(supreg: tsuperregister; const ref: treference): boolean;
begin {checks whether ref contains a reference to reg}
  reginref :=
     ((ref.base <> NR_NO) and
      (getsupreg(ref.base) = supreg)) or
     ((ref.index <> NR_NO) and
      (getsupreg(ref.index) = supreg))
end;


function RegReadByInstruction(supreg: tsuperregister; hp: tai): boolean;
var
  p: taicpu;
  opcount: longint;
begin
  RegReadByInstruction := false;
  if hp.typ <> ait_instruction then
    exit;
  p := taicpu(hp);
  case p.opcode of
    A_CALL:
      regreadbyinstruction := true;
    A_IMUL:
      case p.ops of
        1:
          regReadByInstruction :=
             (supreg = RS_EAX) or reginop(supreg,p.oper[0]^);
        2,3:
          regReadByInstruction :=
            reginop(supreg,p.oper[0]^) or
            reginop(supreg,p.oper[1]^);
      end;
    A_IDIV,A_DIV,A_MUL:
      begin
        regReadByInstruction :=
          reginop(supreg,p.oper[0]^) or (supreg in [RS_EAX,RS_EDX]);
      end;
    else
      begin
        for opcount := 0 to p.ops-1 do
          if (p.oper[opCount]^.typ = top_ref) and
             reginref(supreg,p.oper[opcount]^.ref^) then
            begin
              RegReadByInstruction := true;
              exit
            end;
        for opcount := 1 to maxinschanges do
          case insprop[p.opcode].ch[opcount] of
            CH_REAX..CH_REDI,CH_RWEAX..CH_MEDI:
              if supreg = tch2reg(insprop[p.opcode].ch[opcount]) then
                begin
                  RegReadByInstruction := true;
                  exit
                end;
            CH_RWOP1,CH_ROP1,CH_MOP1:
              if //(p.oper[0]^.typ = top_reg) and
                 reginop(supreg,p.oper[0]^) then
                begin
                  RegReadByInstruction := true;
                  exit
                end;
            Ch_RWOP2,Ch_ROP2,Ch_MOP2:
              if //(p.oper[1]^.typ = top_reg) and
                 reginop(supreg,p.oper[1]^) then
                begin
                  RegReadByInstruction := true;
                  exit
                end;
            Ch_RWOP3,Ch_ROP3,Ch_MOP3:
              if //(p.oper[2]^.typ = top_reg) and
                 reginop(supreg,p.oper[2]^) then
                begin
                  RegReadByInstruction := true;
                  exit
                end;
          end;
      end;
  end;
end;


function regInInstruction(supreg: tsuperregister; p1: tai): boolean;
{ Checks if reg is used by the instruction p1                              }
{ Difference with "regReadBysinstruction() or regModifiedByInstruction()": }
{ this one ignores CH_ALL opcodes, while regModifiedByInstruction doesn't  }
var
  p: taicpu;
  opcount: longint;
begin
  regInInstruction := false;
  if p1.typ <> ait_instruction then
    exit;
  p := taicpu(p1);
  case p.opcode of
    A_CALL:
      regininstruction := true;
    A_IMUL:
      case p.ops of
        1:
          regInInstruction :=
            (supreg = RS_EAX) or reginop(supreg,p.oper[0]^);
        2,3:
          regInInstruction :=
            reginop(supreg,p.oper[0]^) or
            reginop(supreg,p.oper[1]^) or
            (assigned(p.oper[2]) and
             reginop(supreg,p.oper[2]^));
      end;
    A_IDIV,A_DIV,A_MUL:
      regInInstruction :=
        reginop(supreg,p.oper[0]^) or
         (supreg in [RS_EAX,RS_EDX])
    else
      begin
        for opcount := 0 to p.ops-1 do
          if (p.oper[opCount]^.typ = top_ref) and
             reginref(supreg,p.oper[opcount]^.ref^) then
            begin
              regInInstruction := true;
              exit
            end;
        for opcount := 1 to maxinschanges do
          case insprop[p.opcode].Ch[opCount] of
            CH_REAX..CH_MEDI:
              if tch2reg(InsProp[p.opcode].Ch[opCount]) = supreg then
                begin
                  regInInstruction := true;
                  exit;
                end;
            CH_ROp1..CH_MOp1:
              if reginop(supreg,p.oper[0]^) then
                begin
                  regInInstruction := true;
                  exit
                end;
            Ch_ROp2..Ch_MOp2:
              if reginop(supreg,p.oper[1]^) then
                begin
                  regInInstruction := true;
                  exit
                end;
            Ch_ROp3..Ch_MOp3:
              if reginop(supreg,p.oper[2]^) then
                begin
                  regInInstruction := true;
                  exit
                end;
          end;
      end;
  end;
end;


function reginop(supreg: tsuperregister; const o:toper): boolean;
begin
  reginop := false;
  case o.typ Of
    top_reg:
      reginop :=
        (getregtype(o.reg) = R_INTREGISTER) and
        (supreg = getsupreg(o.reg));
    top_ref:
      reginop :=
        ((o.ref^.base <> NR_NO) and
         (supreg = getsupreg(o.ref^.base))) or
        ((o.ref^.index <> NR_NO) and
         (supreg = getsupreg(o.ref^.index)));
  end;
end;


function RegModifiedByInstruction(supreg: tsuperregister; p1: tai): boolean;
var
  InstrProp: TInsProp;
  TmpResult: Boolean;
  Cnt: Word;
begin
  TmpResult := False;
  Result := False;
  if supreg = RS_INVALID then
    exit;
  if (p1.typ = ait_instruction) then
    case taicpu(p1).opcode of
      A_IMUL:
        With taicpu(p1) Do
          TmpResult :=
            ((ops = 1) and (supreg in [RS_EAX,RS_EDX])) or
            ((ops = 2) and (getsupreg(oper[1]^.reg) = supreg)) or
            ((ops = 3) and (getsupreg(oper[2]^.reg) = supreg));
      A_DIV, A_IDIV, A_MUL:
        With taicpu(p1) Do
          TmpResult :=
            (supreg in [RS_EAX,RS_EDX]);
      else
        begin
          Cnt := 1;
          InstrProp := InsProp[taicpu(p1).OpCode];
          while (Cnt <= maxinschanges) and
                (InstrProp.Ch[Cnt] <> Ch_None) and
                not(TmpResult) Do
            begin
              case InstrProp.Ch[Cnt] Of
                Ch_WEAX..Ch_MEDI:
                  TmpResult := supreg = tch2reg(InstrProp.Ch[Cnt]);
                Ch_RWOp1,Ch_WOp1,Ch_Mop1:
                  TmpResult := (taicpu(p1).oper[0]^.typ = top_reg) and
                               reginop(supreg,taicpu(p1).oper[0]^);
                Ch_RWOp2,Ch_WOp2,Ch_Mop2:
                  TmpResult := (taicpu(p1).oper[1]^.typ = top_reg) and
                               reginop(supreg,taicpu(p1).oper[1]^);
                Ch_RWOp3,Ch_WOp3,Ch_Mop3:
                  TmpResult := (taicpu(p1).oper[2]^.typ = top_reg) and
                               reginop(supreg,taicpu(p1).oper[2]^);
                Ch_FPU: TmpResult := false; // supreg is supposed to be an intreg!! supreg in [RS_ST..RS_ST7,RS_MM0..RS_MM7];
                Ch_ALL: TmpResult := true;
              end;
              inc(Cnt)
            end
        end
    end;
  RegModifiedByInstruction := TmpResult
end;


function instrWritesFlags(p: tai): boolean;
var
  l: longint;
begin
  instrWritesFlags := true;
  case p.typ of
    ait_instruction:
      begin
        for l := 1 to maxinschanges do
          if InsProp[taicpu(p).opcode].Ch[l] in [Ch_WFlags,Ch_RWFlags,Ch_All] then
            exit;
      end;
    ait_label:
      exit;
  end;
  instrWritesFlags := false;
end;


function instrReadsFlags(p: tai): boolean;
var
  l: longint;
begin
  instrReadsFlags := true;
  case p.typ of
    ait_instruction:
      begin
        for l := 1 to maxinschanges do
          if InsProp[taicpu(p).opcode].Ch[l] in [Ch_RFlags,Ch_RWFlags,Ch_All] then
            exit;
      end;
    ait_label:
      exit;
  end;
  instrReadsFlags := false;
end;


{********************* GetNext and GetLastInstruction *********************}
function GetNextInstruction(Current: tai; var Next: tai): Boolean;
{ skips ait_regalloc, ait_regdealloc and ait_stab* objects and puts the }
{ next tai object in Next. Returns false if there isn't any             }
begin
  repeat
    if (Current.typ = ait_marker) and
       (tai_Marker(current).Kind = mark_AsmBlockStart) then
      begin
        GetNextInstruction := False;
        Next := Nil;
        Exit
      end;
    Current := tai(current.Next);
    while assigned(Current) and
          ((current.typ in skipInstr) or
           ((current.typ = ait_label) and
            labelCanBeSkipped(tai_label(current)))) do
      Current := tai(current.Next);
{    if assigned(Current) and
       (current.typ = ait_Marker) and
       (tai_Marker(current).Kind = mark_NoPropInfoStart) then
      begin
        while assigned(Current) and
              ((current.typ <> ait_Marker) or
               (tai_Marker(current).Kind <> mark_NoPropInfoEnd)) Do
          Current := tai(current.Next);
      end;}
  until not(assigned(Current)) or
        (current.typ <> ait_Marker) or
        not(tai_Marker(current).Kind in [mark_NoPropInfoStart,mark_NoPropInfoEnd]);
  Next := Current;
  if assigned(Current) and
     not((current.typ in SkipInstr) or
         ((current.typ = ait_label) and
          labelCanBeSkipped(tai_label(current))))
    then
      GetNextInstruction :=
         not((current.typ = ait_marker) and
             (tai_marker(current).kind = mark_AsmBlockStart))
    else
      begin
        GetNextInstruction := False;
        Next := nil;
      end;
end;


function GetLastInstruction(Current: tai; var Last: tai): boolean;
{skips the ait-types in SkipInstr puts the previous tai object in
 Last. Returns false if there isn't any}
begin
  repeat
    Current := tai(current.previous);
    while assigned(Current) and
          (((current.typ = ait_Marker) and
            not(tai_Marker(current).Kind in [mark_AsmBlockEnd{,mark_NoPropInfoEnd}])) or
           (current.typ in SkipInstr) or
           ((current.typ = ait_label) and
            labelCanBeSkipped(tai_label(current)))) Do
      Current := tai(current.previous);
{    if assigned(Current) and
       (current.typ = ait_Marker) and
       (tai_Marker(current).Kind = mark_NoPropInfoEnd) then
      begin
        while assigned(Current) and
              ((current.typ <> ait_Marker) or
               (tai_Marker(current).Kind <> mark_NoPropInfoStart)) Do
          Current := tai(current.previous);
      end;}
  until not(assigned(Current)) or
        (current.typ <> ait_Marker) or
        not(tai_Marker(current).Kind in [mark_NoPropInfoStart,mark_NoPropInfoEnd]);
  if not(assigned(Current)) or
     (current.typ in SkipInstr) or
     ((current.typ = ait_label) and
      labelCanBeSkipped(tai_label(current))) or
     ((current.typ = ait_Marker) and
      (tai_Marker(current).Kind = mark_AsmBlockEnd))
    then
      begin
        Last := nil;
        GetLastInstruction := False
      end
    else
      begin
        Last := Current;
        GetLastInstruction := True;
      end;
end;


procedure SkipHead(var p: tai);
var
 oldp: tai;
begin
  repeat
    oldp := p;
    if (p.typ in SkipInstr) or
       ((p.typ = ait_marker) and
        (tai_Marker(p).Kind in [mark_AsmBlockEnd,mark_NoLineInfoStart,mark_NoLineInfoEnd])) then
      GetNextInstruction(p,p)
    else if ((p.Typ = Ait_Marker) and
        (tai_Marker(p).Kind = mark_NoPropInfoStart)) then
   {a marker of the mark_NoPropInfoStart can't be the first instruction of a
    TAsmList list}
      GetNextInstruction(tai(p.previous),p);
    until p = oldp
end;


function labelCanBeSkipped(p: tai_label): boolean;
begin
  labelCanBeSkipped := not(p.labsym.is_used) or (p.labsym.labeltype<>alt_jump);
end;

{******************* The Data Flow Analyzer functions ********************}

function regLoadedWithNewValue(supreg: tsuperregister; canDependOnPrevValue: boolean;
           hp: tai): boolean;
{ assumes reg is a 32bit register }
var
  p: taicpu;
begin
  if not assigned(hp) or
     (hp.typ <> ait_instruction) then
   begin
     regLoadedWithNewValue := false;
     exit;
   end;
  p := taicpu(hp);
  regLoadedWithNewValue :=
    (((p.opcode = A_MOV) or
      (p.opcode = A_MOVZX) or
      (p.opcode = A_MOVSX) or
      (p.opcode = A_LEA)) and
     (p.oper[1]^.typ = top_reg) and
     (getsupreg(p.oper[1]^.reg) = supreg) and
     (canDependOnPrevValue or
      (p.oper[0]^.typ = top_const) or
      ((p.oper[0]^.typ = top_reg) and
       (getsupreg(p.oper[0]^.reg) <> supreg)) or
      ((p.oper[0]^.typ = top_ref) and
       not regInRef(supreg,p.oper[0]^.ref^)))) or
    ((p.opcode = A_POP) and
     (getsupreg(p.oper[0]^.reg) = supreg));
end;

procedure UpdateUsedRegs(var UsedRegs: TRegSet; p: tai);
{updates UsedRegs with the RegAlloc Information coming after p}
begin
  repeat
    while assigned(p) and
          ((p.typ in (SkipInstr - [ait_RegAlloc])) or
           ((p.typ = ait_label) and
            labelCanBeSkipped(tai_label(p))) or
           ((p.typ = ait_marker) and
            (tai_Marker(p).Kind in [mark_AsmBlockEnd,mark_NoLineInfoStart,mark_NoLineInfoEnd]))) do
         p := tai(p.next);
    while assigned(p) and
          (p.typ=ait_RegAlloc) Do
      begin
        if (getregtype(tai_regalloc(p).reg) = R_INTREGISTER) then
          begin
            case tai_regalloc(p).ratype of
              ra_alloc :
                Include(UsedRegs, TRegEnum(getsupreg(tai_regalloc(p).reg)));
              ra_dealloc :
                Exclude(UsedRegs, TRegEnum(getsupreg(tai_regalloc(p).reg)));
            end;
          end;
        p := tai(p.next);
      end;
  until not(assigned(p)) or
        (not(p.typ in SkipInstr) and
         not((p.typ = ait_label) and
             labelCanBeSkipped(tai_label(p))));
end;


procedure AllocRegBetween(asml: TAsmList; reg: tregister; p1, p2: tai; var initialusedregs: tregset);
{ allocates register reg between (and including) instructions p1 and p2 }
{ the type of p1 and p2 must not be in SkipInstr                        }
{ note that this routine is both called from the peephole optimizer     }
{ where optinfo is not yet initialised) and from the cse (where it is)  }
var
  hp, start: tai;
  removedsomething,
  firstRemovedWasAlloc,
  lastRemovedWasDealloc: boolean;
  supreg: tsuperregister;
begin
{$ifdef EXTDEBUG}
  if assigned(p1.optinfo) and
     (ptaiprop(p1.optinfo)^.usedregs <> initialusedregs) then
   internalerror(2004101010);
{$endif EXTDEBUG}
  start := p1;
 if (reg = NR_ESP) or
    (reg = current_procinfo.framepointer) or
     not(assigned(p1)) then
    { this happens with registers which are loaded implicitely, outside the }
    { current block (e.g. esi with self)                                    }
    exit;
  supreg := getsupreg(reg);
  { make sure we allocate it for this instruction }
  getnextinstruction(p2,p2);
  lastRemovedWasDealloc := false;
  removedSomething := false;
  firstRemovedWasAlloc := false;
{$ifdef allocregdebug}
  hp := tai_comment.Create(strpnew('allocating '+std_regname(newreg(R_INTREGISTER,supreg,R_SUBWHOLE))+
    ' from here...'));
  insertllitem(asml,p1.previous,p1,hp);
  hp := tai_comment.Create(strpnew('allocated '+std_regname(newreg(R_INTREGISTER,supreg,R_SUBWHOLE))+
    ' till here...'));
  insertllitem(asml,p2,p2.next,hp);
{$endif allocregdebug}
  if not(supreg in initialusedregs) then
    begin
      hp := tai_regalloc.alloc(reg,nil);
      insertllItem(asmL,p1.previous,p1,hp);
      include(initialusedregs,supreg);
    end;
  while assigned(p1) and
        (p1 <> p2) do
    begin
      if assigned(p1.optinfo) then
        include(ptaiprop(p1.optinfo)^.usedregs,supreg);
      p1 := tai(p1.next);
      repeat
        while assigned(p1) and
              (p1.typ in (SkipInstr-[ait_regalloc])) Do
          p1 := tai(p1.next);
{ remove all allocation/deallocation info about the register in between }
        if assigned(p1) and
           (p1.typ = ait_regalloc) then
          if (getsupreg(tai_regalloc(p1).reg) = supreg) then
            begin
              if not removedSomething then
                begin
                  firstRemovedWasAlloc := tai_regalloc(p1).ratype=ra_alloc;
                  removedSomething := true;
                end;
              lastRemovedWasDealloc := (tai_regalloc(p1).ratype=ra_dealloc);
              hp := tai(p1.Next);
              asml.Remove(p1);
              p1.free;
              p1 := hp;
            end
          else p1 := tai(p1.next);
      until not(assigned(p1)) or
            not(p1.typ in SkipInstr);
    end;
  if assigned(p1) then
    begin
      if firstRemovedWasAlloc then
        begin
          hp := tai_regalloc.Alloc(reg,nil);
          insertLLItem(asmL,start.previous,start,hp);
        end;
      if lastRemovedWasDealloc then
        begin
          hp := tai_regalloc.DeAlloc(reg,nil);
          insertLLItem(asmL,p1.previous,p1,hp);
        end;
    end;
end;


function FindRegDealloc(supreg: tsuperregister; p: tai): boolean;
var
  hp: tai;
  first: boolean;
begin
  findregdealloc := false;
  first := true;
  while assigned(p.previous) and
        ((tai(p.previous).typ in (skipinstr+[ait_align])) or
         ((tai(p.previous).typ = ait_label) and
          labelCanBeSkipped(tai_label(p.previous)))) do
    begin
      p := tai(p.previous);
      if (p.typ = ait_regalloc) and
         (getregtype(tai_regalloc(p).reg) = R_INTREGISTER) and
         (getsupreg(tai_regalloc(p).reg) = supreg) then
        if (tai_regalloc(p).ratype=ra_dealloc) then
          if first then
            begin
              findregdealloc := true;
              break;
            end
          else
            begin
              findRegDealloc :=
                getNextInstruction(p,hp) and
                 regLoadedWithNewValue(supreg,false,hp);
              break
            end
        else
          first := false;
    end
end;



procedure incState(var S: Byte; amount: longint);
{increases S by 1, wraps around at $ffff to 0 (so we won't get overflow
 errors}
begin
  if (s <= $ff - amount) then
    inc(s, amount)
  else s := longint(s) + amount - $ff;
end;


function sequenceDependsonReg(const Content: TContent; seqreg: tsuperregister; supreg: tsuperregister): Boolean;
{ Content is the sequence of instructions that describes the contents of   }
{ seqReg. reg is being overwritten by the current instruction. if the      }
{ content of seqReg depends on reg (ie. because of a                       }
{ "movl (seqreg,reg), seqReg" instruction), this function returns true     }
var
  p: tai;
  Counter: Word;
  TmpResult: Boolean;
  RegsChecked: TRegSet;
begin
  RegsChecked := [];
  p := Content.StartMod;
  TmpResult := False;
  Counter := 1;
  while not(TmpResult) and
        (Counter <= Content.NrOfMods) Do
    begin
      if (p.typ = ait_instruction) and
         ((taicpu(p).opcode = A_MOV) or
          (taicpu(p).opcode = A_MOVZX) or
          (taicpu(p).opcode = A_MOVSX) or
          (taicpu(p).opcode = A_LEA)) and
         (taicpu(p).oper[0]^.typ = top_ref) then
        With taicpu(p).oper[0]^.ref^ Do
          if ((base = current_procinfo.FramePointer) or
              (assigned(symbol) and (base = NR_NO))) and
             (index = NR_NO) then
            begin
              RegsChecked := RegsChecked + [getsupreg(taicpu(p).oper[1]^.reg)];
              if supreg = getsupreg(taicpu(p).oper[1]^.reg) then
                break;
            end
          else
            tmpResult :=
              regReadByInstruction(supreg,p) and
              regModifiedByInstruction(seqReg,p)
      else
        tmpResult :=
          regReadByInstruction(supreg,p) and
          regModifiedByInstruction(seqReg,p);
      inc(Counter);
      GetNextInstruction(p,p)
    end;
  sequenceDependsonReg := TmpResult
end;


procedure invalidateDependingRegs(p1: ptaiprop; supreg: tsuperregister);
var
  counter: tsuperregister;
begin
  for counter := RS_EAX to RS_EDI do
    if counter <> supreg then
      with p1^.regs[counter] Do
        begin
          if (typ in [con_ref,con_noRemoveRef]) and
             sequenceDependsOnReg(p1^.Regs[counter],counter,supreg) then
            if typ in [con_ref, con_invalid] then
              typ := con_invalid
            { con_noRemoveRef = con_unknown }
            else
              typ := con_unknown;
          if assigned(memwrite) and
             regInRef(counter,memwrite.oper[1]^.ref^) then
            memwrite := nil;
        end;
end;


procedure DestroyReg(p1: ptaiprop; supreg: tsuperregister; doincState:Boolean);
{Destroys the contents of the register reg in the ptaiprop p1, as well as the
 contents of registers are loaded with a memory location based on reg.
 doincState is false when this register has to be destroyed not because
 it's contents are directly modified/overwritten, but because of an indirect
 action (e.g. this register holds the contents of a variable and the value
 of the variable in memory is changed) }
begin
{$push}{$warnings off}
  { the following happens for fpu registers }
  if (supreg < low(NrOfInstrSinceLastMod)) or
     (supreg > high(NrOfInstrSinceLastMod)) then
    exit;
{$pop}
  NrOfInstrSinceLastMod[supreg] := 0;
  with p1^.regs[supreg] do
    begin
      if doincState then
        begin
          incState(wstate,1);
          typ := con_unknown;
          startmod := nil;
        end
      else
        if typ in [con_ref,con_const,con_invalid] then
          typ := con_invalid
        { con_noRemoveRef = con_unknown }
        else
          typ := con_unknown;
      memwrite := nil;
    end;
  invalidateDependingRegs(p1,supreg);
end;

{procedure AddRegsToSet(p: tai; var RegSet: TRegSet);
begin
  if (p.typ = ait_instruction) then
    begin
      case taicpu(p).oper[0]^.typ Of
        top_reg:
          if not(taicpu(p).oper[0]^.reg in [RS_NO,RS_ESP,current_procinfo.FramePointer]) then
            RegSet := RegSet + [taicpu(p).oper[0]^.reg];
        top_ref:
          With TReference(taicpu(p).oper[0]^) Do
            begin
              if not(base in [current_procinfo.FramePointer,RS_NO,RS_ESP])
                then RegSet := RegSet + [base];
              if not(index in [current_procinfo.FramePointer,RS_NO,RS_ESP])
                then RegSet := RegSet + [index];
            end;
      end;
      case taicpu(p).oper[1]^.typ Of
        top_reg:
          if not(taicpu(p).oper[1]^.reg in [RS_NO,RS_ESP,current_procinfo.FramePointer]) then
            if RegSet := RegSet + [TRegister(TwoWords(taicpu(p).oper[1]^).Word1];
        top_ref:
          With TReference(taicpu(p).oper[1]^) Do
            begin
              if not(base in [current_procinfo.FramePointer,RS_NO,RS_ESP])
                then RegSet := RegSet + [base];
              if not(index in [current_procinfo.FramePointer,RS_NO,RS_ESP])
                then RegSet := RegSet + [index];
            end;
      end;
    end;
end;}

function OpsEquivalent(const o1, o2: toper; const oldinst, newinst: taicpu; var RegInfo: toptreginfo; OpAct: TopAction): Boolean;
begin {checks whether the two ops are equivalent}
  OpsEquivalent := False;
  if o1.typ=o2.typ then
    case o1.typ Of
      top_reg:
        OpsEquivalent :=RegsEquivalent(o1.reg,o2.reg, oldinst, newinst, RegInfo, OpAct);
      top_ref:
        OpsEquivalent := RefsEquivalent(o1.ref^, o2.ref^, oldinst, newinst, RegInfo);
      Top_Const:
        OpsEquivalent := o1.val = o2.val;
      Top_None:
        OpsEquivalent := True
    end;
end;


function OpsEqual(const o1,o2:toper): Boolean;
begin {checks whether the two ops are equal}
  OpsEqual := False;
  if o1.typ=o2.typ then
    case o1.typ Of
      top_reg :
        OpsEqual:=o1.reg=o2.reg;
      top_ref :
        OpsEqual := RefsEqual(o1.ref^, o2.ref^);
      Top_Const :
        OpsEqual:=o1.val=o2.val;
      Top_None :
        OpsEqual := True
    end;
end;


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


function opscompatible(p1,p2: taicpu): boolean;
begin
  case p1.opcode of
    A_MOVZX,A_MOVSX:
      opscompatible :=
        ((p2.opcode = p1.opcode) or (p2.opcode = A_MOV)) and
        sizescompatible(p1.opsize,p2.opsize);
    else
      opscompatible :=
        (p1.opcode = p2.opcode) and
        (p1.ops = p2.ops) and
        (p1.opsize = p2.opsize);
  end;
end;


function InstructionsEquivalent(p1, p2: tai; var RegInfo: toptreginfo): Boolean;
{$ifdef csdebug}
var
  hp: tai;
{$endif csdebug}
begin {checks whether two taicpu instructions are equal}
  if assigned(p1) and assigned(p2) and
     (tai(p1).typ = ait_instruction) and
     (tai(p2).typ = ait_instruction) and
     opscompatible(taicpu(p1),taicpu(p2)) and
     (not(assigned(taicpu(p1).oper[0])) or
      (taicpu(p1).oper[0]^.typ = taicpu(p2).oper[0]^.typ)) and
     (not(assigned(taicpu(p1).oper[1])) or
      (taicpu(p1).oper[1]^.typ = taicpu(p2).oper[1]^.typ)) and
     (not(assigned(taicpu(p1).oper[2])) or
      (taicpu(p1).oper[2]^.typ = taicpu(p2).oper[2]^.typ)) then
 {both instructions have the same structure:
  "<operator> <operand of type1>, <operand of type 2>"}
    if ((taicpu(p1).opcode = A_MOV) or
        (taicpu(p1).opcode = A_MOVZX) or
        (taicpu(p1).opcode = A_MOVSX)  or
        (taicpu(p1).opcode = A_LEA)) and
       (taicpu(p1).oper[0]^.typ = top_ref) {then .oper[1]^t = top_reg} then
      if not(RegInRef(getsupreg(taicpu(p1).oper[1]^.reg), taicpu(p1).oper[0]^.ref^)) then
 {the "old" instruction is a load of a register with a new value, not with
  a value based on the contents of this register (so no "mov (reg), reg")}
        if not(RegInRef(getsupreg(taicpu(p2).oper[1]^.reg), taicpu(p2).oper[0]^.ref^)) and
           RefsEquivalent(taicpu(p1).oper[0]^.ref^, taicpu(p2).oper[0]^.ref^,taicpu(p1), taicpu(p2), reginfo) then
 {the "new" instruction is also a load of a register with a new value, and
  this value is fetched from the same memory location}
          begin
            With taicpu(p2).oper[0]^.ref^ Do
              begin
                if (base <> NR_NO) and
                    (not(getsupreg(base) in [getsupreg(current_procinfo.FramePointer), RS_ESP])) then
                  include(RegInfo.RegsLoadedForRef, getsupreg(base));
                if (index <> NR_NO) and
                    (not(getsupreg(index) in [getsupreg(current_procinfo.FramePointer), RS_ESP])) then
                  include(RegInfo.RegsLoadedForRef, getsupreg(index));
              end;
 {add the registers from the reference (.oper[0]^) to the RegInfo, all registers
  from the reference are the same in the old and in the new instruction
  sequence}
            AddOp2RegInfo(taicpu(p1).oper[0]^, RegInfo);
 {the registers from .oper[1]^ have to be equivalent, but not necessarily equal}
            InstructionsEquivalent :=
              RegsEquivalent(taicpu(p1).oper[1]^.reg,
                taicpu(p2).oper[1]^.reg, taicpu(p1), taicpu(p2), RegInfo, OpAct_Write);
          end
 {the registers are loaded with values from different memory locations. if
  this was allowed, the instructions "mov -4(esi),eax" and "mov -4(ebp),eax"
  would be considered equivalent}
        else
          InstructionsEquivalent := False
      else
 {load register with a value based on the current value of this register}
        begin
          With taicpu(p2).oper[0]^.ref^ Do
            begin
              if (base <> NR_NO) and
                 (not(getsupreg(base) in [getsupreg(current_procinfo.FramePointer),
                   getsupreg(taicpu(p2).oper[1]^.reg),RS_ESP])) then
 {it won't do any harm if the register is already in RegsLoadedForRef}
                begin
                  include(RegInfo.RegsLoadedForRef, getsupreg(base));
{$ifdef csdebug}
                  Writeln(std_regname(base), ' added');
{$endif csdebug}
                end;
              if (index <> NR_NO) and
                 (not(getsupreg(index) in [getsupreg(current_procinfo.FramePointer),
                   getsupreg(taicpu(p2).oper[1]^.reg),RS_ESP])) then
                begin
                  include(RegInfo.RegsLoadedForRef, getsupreg(index));
{$ifdef csdebug}
                  Writeln(std_regname(index), ' added');
{$endif csdebug}
                end;

            end;
          if (taicpu(p2).oper[1]^.reg <> NR_NO) and
             (not(getsupreg(taicpu(p2).oper[1]^.reg) in [getsupreg(current_procinfo.FramePointer),RS_ESP])) then
            begin
              RegInfo.RegsLoadedForRef := RegInfo.RegsLoadedForRef -
                                              [getsupreg(taicpu(p2).oper[1]^.reg)];
{$ifdef csdebug}
              Writeln(std_regname(newreg(R_INTREGISTER,getsupreg(taicpu(p2).oper[1]^.reg),R_SUBWHOLE)), ' removed');
{$endif csdebug}
            end;
          InstructionsEquivalent :=
             OpsEquivalent(taicpu(p1).oper[0]^, taicpu(p2).oper[0]^, taicpu(p1), taicpu(p2), RegInfo, OpAct_Read) and
             OpsEquivalent(taicpu(p1).oper[1]^, taicpu(p2).oper[1]^, taicpu(p1), taicpu(p2), RegInfo, OpAct_Write)
        end
    else
 {an instruction <> mov, movzx, movsx}
      begin
  {$ifdef csdebug}
        hp := tai_comment.Create(strpnew('checking if equivalent'));
        hp.previous := p2;
        hp.next := p2.next;
        p2.next.previous := hp;
        p2.next := hp;
  {$endif csdebug}
        InstructionsEquivalent :=
          (not(assigned(taicpu(p1).oper[0])) or
           OpsEquivalent(taicpu(p1).oper[0]^, taicpu(p2).oper[0]^, taicpu(p1), taicpu(p2), RegInfo, OpAct_Unknown)) and
          (not(assigned(taicpu(p1).oper[1])) or
           OpsEquivalent(taicpu(p1).oper[1]^, taicpu(p2).oper[1]^, taicpu(p1), taicpu(p2), RegInfo, OpAct_Unknown)) and
          (not(assigned(taicpu(p1).oper[2])) or
           OpsEquivalent(taicpu(p1).oper[2]^, taicpu(p2).oper[2]^, taicpu(p1), taicpu(p2), RegInfo, OpAct_Unknown))
       end
 {the instructions haven't even got the same structure, so they're certainly
  not equivalent}
    else
      begin
  {$ifdef csdebug}
        hp := tai_comment.Create(strpnew('different opcodes/format'));
        hp.previous := p2;
        hp.next := p2.next;
        p2.next.previous := hp;
        p2.next := hp;
  {$endif csdebug}
        InstructionsEquivalent := False;
      end;
  {$ifdef csdebug}
    hp := tai_comment.Create(strpnew('instreq: '+tostr(byte(instructionsequivalent))));
    hp.previous := p2;
    hp.next := p2.next;
    p2.next.previous := hp;
    p2.next := hp;
  {$endif csdebug}
end;

(*
function InstructionsEqual(p1, p2: tai): Boolean;
begin {checks whether two taicpu instructions are equal}
  InstructionsEqual :=
    assigned(p1) and assigned(p2) and
    ((tai(p1).typ = ait_instruction) and
     (tai(p1).typ = ait_instruction) and
     (taicpu(p1).opcode = taicpu(p2).opcode) and
     (taicpu(p1).oper[0]^.typ = taicpu(p2).oper[0]^.typ) and
     (taicpu(p1).oper[1]^.typ = taicpu(p2).oper[1]^.typ) and
     OpsEqual(taicpu(p1).oper[0]^.typ, taicpu(p1).oper[0]^, taicpu(p2).oper[0]^) and
     OpsEqual(taicpu(p1).oper[1]^.typ, taicpu(p1).oper[1]^, taicpu(p2).oper[1]^))
end;
*)

procedure readreg(p: ptaiprop; supreg: tsuperregister);
begin
  if supreg in [RS_EAX..RS_EDI] then
    incState(p^.regs[supreg].rstate,1)
end;


procedure readref(p: ptaiprop; const ref: preference);
begin
  if ref^.base <> NR_NO then
    readreg(p, getsupreg(ref^.base));
  if ref^.index <> NR_NO then
    readreg(p, getsupreg(ref^.index));
end;


procedure ReadOp(p: ptaiprop;const o:toper);
begin
  case o.typ Of
    top_reg: readreg(p, getsupreg(o.reg));
    top_ref: readref(p, o.ref);
  end;
end;


function RefInInstruction(const ref: TReference; p: tai;
           RefsEq: TRefCompare; size: tcgsize): Boolean;
{checks whehter ref is used in p}
var
  mysize: tcgsize;
  TmpResult: Boolean;
begin
  TmpResult := False;
  if (p.typ = ait_instruction) then
    begin
      mysize := topsize2tcgsize[taicpu(p).opsize];
      if (taicpu(p).ops >= 1) and
         (taicpu(p).oper[0]^.typ = top_ref) then
        TmpResult := RefsEq(taicpu(p).oper[0]^.ref^,ref,mysize,size);
      if not(TmpResult) and
         (taicpu(p).ops >= 2) and
         (taicpu(p).oper[1]^.typ = top_ref) then
        TmpResult := RefsEq(taicpu(p).oper[1]^.ref^,ref,mysize,size);
      if not(TmpResult) and
         (taicpu(p).ops >= 3) and
         (taicpu(p).oper[2]^.typ = top_ref) then
        TmpResult := RefsEq(taicpu(p).oper[2]^.ref^,ref,mysize,size);
    end;
  RefInInstruction := TmpResult;
end;


function RefInSequence(const ref: TReference; Content: TContent;
           RefsEq: TRefCompare; size: tcgsize): Boolean;
{checks the whole sequence of Content (so StartMod and and the next NrOfMods
 tai objects) to see whether ref is used somewhere}
var p: tai;
    Counter: Word;
    TmpResult: Boolean;
begin
  p := Content.StartMod;
  TmpResult := False;
  Counter := 1;
  while not(TmpResult) and
        (Counter <= Content.NrOfMods) Do
    begin
      if (p.typ = ait_instruction) and
         RefInInstruction(ref, p, RefsEq, size)
        then TmpResult := True;
      inc(Counter);
      GetNextInstruction(p,p)
    end;
  RefInSequence := TmpResult
end;

{$push}
{$q-}
// checks whether a write to r2 of size "size" contains address r1
function arrayrefsoverlapping(const r1, r2: treference; size1, size2: tcgsize): Boolean;
var
  realsize1, realsize2: aint;
begin
  realsize1 := tcgsize2size[size1];
  realsize2 := tcgsize2size[size2];
  arrayrefsoverlapping :=
    (r2.offset <= r1.offset+realsize1) and
    (r1.offset <= r2.offset+realsize2) and
    (r1.segment = r2.segment) and
    (r1.symbol=r2.symbol) and
    (r1.base = r2.base)
end;
{$pop}

function isSimpleRef(const ref: treference): boolean;
{ returns true if ref is reference to a local or global variable, to a  }
{ parameter or to an object field (this includes arrays). Returns false }
{ otherwise.                                                            }
begin
  isSimpleRef :=
    assigned(ref.symbol) or
    (ref.base = current_procinfo.framepointer);
end;


function containsPointerRef(p: tai): boolean;
{ checks if an instruction contains a reference which is a pointer location }
var
  hp: taicpu;
  count: longint;
begin
  containsPointerRef := false;
  if p.typ <> ait_instruction then
    exit;
  hp := taicpu(p);
  for count := 0 to hp.ops-1 do
    begin
      case hp.oper[count]^.typ of
        top_ref:
          if not isSimpleRef(hp.oper[count]^.ref^) then
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
  p: tai;
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


function writeToMemDestroysContents(regWritten: tsuperregister; const ref: treference;
  supreg: tsuperregister; size: tcgsize; const c: tcontent; var invalsmemwrite: boolean): boolean;
{ returns whether the contents c of reg are invalid after regWritten is }
{ is written to ref                                                     }
var
  refsEq: trefCompare;
begin
  if isSimpleRef(ref) then
    begin
      if (ref.index <> NR_NO) or
         (assigned(ref.symbol) and
          (ref.base <> NR_NO)) then
        { local/global variable or parameter which is an array }
        refsEq := @arrayRefsOverlapping
      else
        { local/global variable or parameter which is not an array }
        refsEq := @refsOverlapping;
      invalsmemwrite :=
        assigned(c.memwrite) and
        ((not(cs_opt_size in current_settings.optimizerswitches) and
          containsPointerRef(c.memwrite)) or
         refsEq(c.memwrite.oper[1]^.ref^,ref,topsize2tcgsize[c.memwrite.opsize],size));
      if not(c.typ in [con_ref,con_noRemoveRef,con_invalid]) then
        begin
          writeToMemDestroysContents := false;
          exit;
        end;

     { write something to a parameter, a local or global variable, so          }
     {  * with uncertain optimizations on:                                     }
     {    - destroy the contents of registers whose contents have somewhere a  }
     {      "mov?? (ref), %reg". WhichReg (this is the register whose contents }
     {      are being written to memory) is not destroyed if it's StartMod is  }
     {      of that form and NrOfMods = 1 (so if it holds ref, but is not a    }
     {      expression based on ref)                                           }
     {  * with uncertain optimizations off:                                    }
     {    - also destroy registers that contain any pointer                    }
      with c do
        writeToMemDestroysContents :=
          (typ in [con_ref,con_noRemoveRef]) and
          ((not(cs_opt_size in current_settings.optimizerswitches) and
            containsPointerLoad(c)
           ) or
           (refInSequence(ref,c,refsEq,size) and
            ((supreg <> regWritten) or
             not((nrOfMods = 1) and
                 {StarMod is always of the type ait_instruction}
                 (taicpu(StartMod).oper[0]^.typ = top_ref) and
                 refsEq(taicpu(StartMod).oper[0]^.ref^, ref, topsize2tcgsize[taicpu(StartMod).opsize],size)
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
        (not(cs_opt_size in current_settings.optimizerswitches) or
         containsPointerRef(c.memwrite));
      if not(c.typ in [con_ref,con_noRemoveRef,con_invalid]) then
        begin
          writeToMemDestroysContents := false;
          exit;
        end;
      with c do
        writeToMemDestroysContents :=
          (typ in [con_ref,con_noRemoveRef]) and
          (not(cs_opt_size in current_settings.optimizerswitches) or
         { for movsl }
           ((ref.base = NR_EDI) and (ref.index = NR_EDI)) or
         { don't destroy if reg contains a parameter, local or global variable }
           containsPointerLoad(c)
          );
    end;
end;


function writeToRegDestroysContents(destReg, supreg: tsuperregister;
  const c: tcontent): boolean;
{ returns whether the contents c of reg are invalid after destReg is }
{ modified                                                           }
begin
  writeToRegDestroysContents :=
    (c.typ in [con_ref,con_noRemoveRef,con_invalid]) and
    sequenceDependsOnReg(c,supreg,destReg);
end;


function writeDestroysContents(const op: toper; supreg: tsuperregister; size: tcgsize;
  const c: tcontent; var memwritedestroyed: boolean): boolean;
{ returns whether the contents c of reg are invalid after regWritten is }
{ is written to op                                                      }
begin
  memwritedestroyed := false;
  case op.typ of
    top_reg:
      writeDestroysContents :=
        (getregtype(op.reg) = R_INTREGISTER) and
        writeToRegDestroysContents(getsupreg(op.reg),supreg,c);
    top_ref:
      writeDestroysContents :=
        writeToMemDestroysContents(RS_INVALID,op.ref^,supreg,size,c,memwritedestroyed);
  else
    writeDestroysContents := false;
  end;
end;


procedure destroyRefs(p: tai; const ref: treference; regwritten: tsuperregister; size: tcgsize);
{ destroys all registers which possibly contain a reference to ref, regWritten }
{ is the register whose contents are being written to memory (if this proc     }
{ is called because of a "mov?? %reg, (mem)" instruction)                      }
var
  counter: tsuperregister;
  destroymemwrite: boolean;
begin
  for counter := RS_EAX to RS_EDI Do
    begin
      if writeToMemDestroysContents(regwritten,ref,counter,size,
           ptaiprop(p.optInfo)^.regs[counter],destroymemwrite) then
        destroyReg(ptaiprop(p.optInfo), counter, false)
      else if destroymemwrite then
        ptaiprop(p.optinfo)^.regs[counter].MemWrite := nil;
    end;
end;


procedure DestroyAllRegs(p: ptaiprop; read, written: boolean);
var Counter: tsuperregister;
begin {initializes/desrtoys all registers}
  For Counter := RS_EAX To RS_EDI Do
    begin
      if read then
        readreg(p, Counter);
      DestroyReg(p, Counter, written);
      p^.regs[counter].MemWrite := nil;
    end;
  p^.DirFlag := F_Unknown;
end;


procedure DestroyOp(taiObj: tai; const o:Toper);
{$ifdef statedebug}
var
    hp: tai;
{$endif statedebug}
begin
  case o.typ Of
    top_reg:
      begin
{$ifdef statedebug}
        hp := tai_comment.Create(strpnew('destroying '+std_regname(o.reg)));
        hp.next := taiobj.next;
        hp.previous := taiobj;
        taiobj.next := hp;
        if assigned(hp.next) then
          hp.next.previous := hp;
{$endif statedebug}
        DestroyReg(ptaiprop(taiObj.OptInfo), getsupreg(o.reg), true);
      end;
    top_ref:
      begin
        readref(ptaiprop(taiObj.OptInfo), o.ref);
        DestroyRefs(taiObj, o.ref^, RS_INVALID,topsize2tcgsize[(taiobj as taicpu).opsize]);
      end;
  end;
end;


procedure AddInstr2RegContents({$ifdef statedebug} asml: TAsmList; {$endif}
p: taicpu; supreg: tsuperregister);
{$ifdef statedebug}
var
  hp: tai;
{$endif statedebug}
begin
  With ptaiprop(p.optinfo)^.regs[supreg] Do
    if (typ in [con_ref,con_noRemoveRef]) then
      begin
        incState(wstate,1);
        { also store how many instructions are part of the sequence in the first }
        { instructions ptaiprop, so it can be easily accessed from within        }
        { CheckSequence}
        inc(NrOfMods, NrOfInstrSinceLastMod[supreg]);
        ptaiprop(tai(StartMod).OptInfo)^.Regs[supreg].NrOfMods := NrOfMods;
        NrOfInstrSinceLastMod[supreg] := 0;
        invalidateDependingRegs(p.optinfo,supreg);
        ptaiprop(p.optinfo)^.regs[supreg].memwrite := nil;
{$ifdef StateDebug}
        hp := tai_comment.Create(strpnew(std_regname(newreg(R_INTREGISTER,supreg,R_SUBWHOLE))+': '+tostr(ptaiprop(p.optinfo)^.Regs[supreg].WState)
              + ' -- ' + tostr(ptaiprop(p.optinfo)^.Regs[supreg].nrofmods)));
        InsertLLItem(AsmL, p, p.next, hp);
{$endif StateDebug}
      end
    else
      begin
{$ifdef statedebug}
        hp := tai_comment.Create(strpnew('destroying '+std_regname(newreg(R_INTREGISTER,supreg,R_SUBWHOLE))));
        insertllitem(asml,p,p.next,hp);
{$endif statedebug}
        DestroyReg(ptaiprop(p.optinfo), supreg, true);
{$ifdef StateDebug}
        hp := tai_comment.Create(strpnew(std_regname(newreg(R_INTREGISTER,supreg,R_SUBWHOLE))+': '+tostr(ptaiprop(p.optinfo)^.Regs[supreg].WState)));
        InsertLLItem(AsmL, p, p.next, hp);
{$endif StateDebug}
      end
end;


procedure AddInstr2OpContents({$ifdef statedebug} asml: TAsmList; {$endif}
p: taicpu; const oper: TOper);
begin
  if oper.typ = top_reg then
    AddInstr2RegContents({$ifdef statedebug} asml, {$endif}p, getsupreg(oper.reg))
  else
    begin
      ReadOp(ptaiprop(p.optinfo), oper);
      DestroyOp(p, oper);
    end
end;


{*************************************************************************************}
{************************************** TDFAOBJ **************************************}
{*************************************************************************************}

constructor tdfaobj.create(_list: TAsmList);
begin
  list := _list;
  blockstart := nil;
  blockend := nil;
  nroftaiobjs := 0;
  taipropblock := nil;
  lolab := 0;
  hilab := 0;
  labdif := 0;
  labeltable := nil;
end;


procedure tdfaobj.initlabeltable;
var
  labelfound: boolean;
  p, prev: tai;
  hp1, hp2: tai;
{$ifdef i386}
  regcounter,
  supreg : tsuperregister;
{$endif i386}
  usedregs, nodeallocregs: tregset;
begin
  labelfound := false;
  lolab := maxlongint;
  hilab := 0;
  p := blockstart;
  prev := p;
  while assigned(p) do
    begin
      if (tai(p).typ = ait_label) then
        if not labelcanbeskipped(tai_label(p)) then
          begin
            labelfound := true;
             if (tai_Label(p).labsym.labelnr < lolab) then
               lolab := tai_label(p).labsym.labelnr;
             if (tai_Label(p).labsym.labelnr > hilab) then
               hilab := tai_label(p).labsym.labelnr;
          end;
      prev := p;
      getnextinstruction(p, p);
    end;
  if (prev.typ = ait_marker) and
     (tai_marker(prev).kind = mark_AsmBlockStart) then
    blockend := prev
  else blockend := nil;
  if labelfound then
    labdif := hilab+1-lolab
  else labdif := 0;

  usedregs := [];
  if (labdif <> 0) then
    begin
      getmem(labeltable, labdif*sizeof(tlabeltableitem));
      fillchar(labeltable^, labdif*sizeof(tlabeltableitem), 0);
    end;
  p := blockstart;
  prev := p;
  while (p <> blockend) do
    begin
      case p.typ of
        ait_label:
          if not labelcanbeskipped(tai_label(p)) then
            labeltable^[tai_label(p).labsym.labelnr-lolab].taiobj := p;
{$ifdef i386}
        ait_regalloc:
         if (getregtype(tai_regalloc(p).reg) = R_INTREGISTER) then
          begin
            supreg:=getsupreg(tai_regalloc(p).reg);
            case tai_regalloc(p).ratype of
              ra_alloc :
                begin
                  if not(supreg in usedregs) then
                    include(usedregs, supreg)
                  else
                    begin
                      //addregdeallocfor(list, tai_regalloc(p).reg, p);
                      hp1 := tai(p.previous);
                      list.remove(p);
                      p.free;
                      p := hp1;
                    end;
                end;
              ra_dealloc :
                begin
                  exclude(usedregs, supreg);
                  hp1 := p;
                  hp2 := nil;
                  while not(findregalloc(supreg,tai(hp1.next),ra_alloc)) and
                        getnextinstruction(hp1, hp1) and
                        regininstruction(getsupreg(tai_regalloc(p).reg), hp1) Do
                    hp2 := hp1;
                  if hp2 <> nil then
                    begin
                      hp1 := tai(p.previous);
                      list.remove(p);
                      insertllitem(list, hp2, tai(hp2.next), p);
                      p := hp1;
                    end
                  else if findregalloc(getsupreg(tai_regalloc(p).reg), tai(p.next),ra_alloc)
                          and getnextinstruction(p,hp1) then
                    begin
                      hp1 := tai(p.previous);
                      list.remove(p);
                      p.free;
                      p := hp1;
//                      don't include here, since then the allocation will be removed when it's processed
//                      include(usedregs,supreg);
                    end;
                end;
             end;
           end;
{$endif i386}
      end;
      repeat
        prev := p;
        p := tai(p.next);
      until not(assigned(p)) or
            (p = blockend) or
            not(p.typ in (skipinstr - [ait_regalloc]));
    end;
{$ifdef i386}
  { don't add deallocation for function result variable or for regvars}
  getNoDeallocRegs(noDeallocRegs);
  usedRegs := usedRegs - noDeallocRegs;
  for regCounter := RS_EAX to RS_EDI do
    if regCounter in usedRegs then
      addRegDeallocFor(list,newreg(R_INTREGISTER,regCounter,R_SUBWHOLE),prev);
{$endif i386}
end;


function tdfaobj.pass_1(_blockstart: tai): tai;
begin
  blockstart := _blockstart;
  initlabeltable;
  pass_1 := blockend;
end;



function tdfaobj.initdfapass2: boolean;
{reserves memory for the PtaiProps in one big memory block when not using
 TP, returns False if not enough memory is available for the optimizer in all
 cases}
var
  p: tai;
  count: Longint;
{    TmpStr: String; }
begin
  p := blockstart;
  skiphead(p);
  nroftaiobjs := 0;
  while (p <> blockend) do
    begin
{$ifDef JumpAnal}
      case p.typ of
        ait_label:
          begin
            if not labelcanbeskipped(tai_label(p)) then
              labeltable^[tai_label(p).labsym.labelnr-lolab].instrnr := nroftaiobjs
          end;
        ait_instruction:
          begin
            if taicpu(p).is_jmp then
             begin
               if (tasmlabel(taicpu(p).oper[0]^.sym).labsymabelnr >= lolab) and
                  (tasmlabel(taicpu(p).oper[0]^.sym).labsymabelnr <= hilab) then
                 inc(labeltable^[tasmlabel(taicpu(p).oper[0]^.sym).labsymabelnr-lolab].refsfound);
             end;
          end;
{        ait_instruction:
          begin
           if (taicpu(p).opcode = A_PUSH) and
              (taicpu(p).oper[0]^.typ = top_symbol) and
              (PCSymbol(taicpu(p).oper[0]^)^.offset = 0) then
             begin
               TmpStr := StrPas(PCSymbol(taicpu(p).oper[0]^)^.symbol);
               if}
      end;
{$endif JumpAnal}
      inc(NrOftaiObjs);
      getnextinstruction(p,p);
    end;
  if nroftaiobjs <> 0 then
    begin
      initdfapass2 := True;
      getmem(taipropblock, nroftaiobjs*sizeof(ttaiprop));
      fillchar(taiPropblock^,nroftaiobjs*sizeof(ttaiprop),0);
      p := blockstart;
      skiphead(p);
      for count := 1 To nroftaiobjs do
        begin
          ptaiprop(p.optinfo) := @taipropblock^[count];
          getnextinstruction(p, p);
        end;
    end
  else
    initdfapass2 := false;
end;


procedure tdfaobj.dodfapass2;
{Analyzes the Data Flow of an assembler list. Starts creating the reg
 contents for the instructions starting with p. Returns the last tai which has
 been processed}
var
    curprop, LastFlagsChangeProp: ptaiprop;
    Cnt, InstrCnt : Longint;
    InstrProp: TInsProp;
    UsedRegs: TRegSet;
    prev,p  : tai;
    tmpref: TReference;
    tmpsupreg: tsuperregister;
{$ifdef statedebug}
    hp : tai;
{$endif}
{$ifdef AnalyzeLoops}
    hp : tai;
    TmpState: Byte;
{$endif AnalyzeLoops}
begin
  p := BlockStart;
  LastFlagsChangeProp := nil;
  prev := nil;
  UsedRegs := [];
  UpdateUsedregs(UsedRegs, p);
  SkipHead(p);
  BlockStart := p;
  InstrCnt := 1;
  fillchar(NrOfInstrSinceLastMod, SizeOf(NrOfInstrSinceLastMod), 0);
  while (p <> Blockend) Do
    begin
      curprop := @taiPropBlock^[InstrCnt];
      if assigned(prev)
        then
          begin
{$ifdef JumpAnal}
            if (p.Typ <> ait_label) then
{$endif JumpAnal}
              begin
                curprop^.regs := ptaiprop(prev.OptInfo)^.Regs;
                curprop^.DirFlag := ptaiprop(prev.OptInfo)^.DirFlag;
                curprop^.FlagsUsed := false;
              end
          end
        else
          begin
            fillchar(curprop^, SizeOf(curprop^), 0);
{            For tmpreg := RS_EAX to RS_EDI Do
              curprop^.regs[tmpreg].WState := 1;}
          end;
      curprop^.UsedRegs := UsedRegs;
      curprop^.CanBeRemoved := False;
      UpdateUsedRegs(UsedRegs, tai(p.Next));
      For tmpsupreg := RS_EAX To RS_EDI Do
        if NrOfInstrSinceLastMod[tmpsupreg] < 255 then
          inc(NrOfInstrSinceLastMod[tmpsupreg])
        else
          begin
            NrOfInstrSinceLastMod[tmpsupreg] := 0;
            curprop^.regs[tmpsupreg].typ := con_unknown;
          end;
      case p.typ Of
        ait_marker:;
        ait_label:
{$ifndef JumpAnal}
          if not labelCanBeSkipped(tai_label(p)) then
            DestroyAllRegs(curprop,false,false);
{$else JumpAnal}
          begin
           if not labelCanBeSkipped(tai_label(p)) then
             With LTable^[tai_Label(p).labsym^.labelnr-LoLab] Do
{$ifDef AnalyzeLoops}
              if (RefsFound = tai_Label(p).labsym^.RefCount)
{$else AnalyzeLoops}
              if (JmpsProcessed = tai_Label(p).labsym^.RefCount)
{$endif AnalyzeLoops}
                then
{all jumps to this label have been found}
{$ifDef AnalyzeLoops}
                  if (JmpsProcessed > 0)
                    then
{$endif AnalyzeLoops}
 {we've processed at least one jump to this label}
                      begin
                        if (GetLastInstruction(p, hp) and
                           not(((hp.typ = ait_instruction)) and
                                (taicpu_labeled(hp).is_jmp))
                          then
  {previous instruction not a JMP -> the contents of the registers after the
   previous intruction has been executed have to be taken into account as well}
                            For tmpsupreg := RS_EAX to RS_EDI Do
                              begin
                                if (curprop^.regs[tmpsupreg].WState <>
                                    ptaiprop(hp.OptInfo)^.Regs[tmpsupreg].WState)
                                  then DestroyReg(curprop, tmpsupreg, true)
                              end
                      end
{$ifDef AnalyzeLoops}
                    else
 {a label from a backward jump (e.g. a loop), no jump to this label has
  already been processed}
                      if GetLastInstruction(p, hp) and
                         not(hp.typ = ait_instruction) and
                            (taicpu_labeled(hp).opcode = A_JMP))
                        then
  {previous instruction not a jmp, so keep all the registers' contents from the
   previous instruction}
                          begin
                            curprop^.regs := ptaiprop(hp.OptInfo)^.Regs;
                            curprop.DirFlag := ptaiprop(hp.OptInfo)^.DirFlag;
                          end
                        else
  {previous instruction a jmp and no jump to this label processed yet}
                          begin
                            hp := p;
                            Cnt := InstrCnt;
     {continue until we find a jump to the label or a label which has already
      been processed}
                            while GetNextInstruction(hp, hp) and
                                  not((hp.typ = ait_instruction) and
                                      (taicpu(hp).is_jmp) and
                                      (tasmlabel(taicpu(hp).oper[0]^.sym).labsymabelnr = tai_Label(p).labsym^.labelnr)) and
                                  not((hp.typ = ait_label) and
                                      (LTable^[tai_Label(hp).labsym^.labelnr-LoLab].RefsFound
                                       = tai_Label(hp).labsym^.RefCount) and
                                      (LTable^[tai_Label(hp).labsym^.labelnr-LoLab].JmpsProcessed > 0)) Do
                              inc(Cnt);
                            if (hp.typ = ait_label)
                              then
   {there's a processed label after the current one}
                                begin
                                  curprop^.regs := taiPropBlock^[Cnt].Regs;
                                  curprop.DirFlag := taiPropBlock^[Cnt].DirFlag;
                                end
                              else
   {there's no label anymore after the current one, or they haven't been
    processed yet}
                                begin
                                  GetLastInstruction(p, hp);
                                  curprop^.regs := ptaiprop(hp.OptInfo)^.Regs;
                                  curprop.DirFlag := ptaiprop(hp.OptInfo)^.DirFlag;
                                  DestroyAllRegs(ptaiprop(hp.OptInfo),true,true)
                                end
                          end
{$endif AnalyzeLoops}
                else
{not all references to this label have been found, so destroy all registers}
                  begin
                    GetLastInstruction(p, hp);
                    curprop^.regs := ptaiprop(hp.OptInfo)^.Regs;
                    curprop.DirFlag := ptaiprop(hp.OptInfo)^.DirFlag;
                    DestroyAllRegs(curprop,true,true)
                  end;
          end;
{$endif JumpAnal}

        ait_stab, ait_force_line, ait_function_name:;
        ait_align: ; { may destroy flags !!! }
        ait_instruction:
          begin
            if taicpu(p).is_jmp or
               (taicpu(p).opcode = A_JMP) then
             begin
{$ifNDef JumpAnal}
                for tmpsupreg := RS_EAX to RS_EDI do
                  with curprop^.regs[tmpsupreg] do
                    case typ of
                      con_ref: typ := con_noRemoveRef;
                      con_const: typ := con_noRemoveConst;
                      con_invalid: typ := con_unknown;
                    end;
{$else JumpAnal}
          With LTable^[tasmlabel(taicpu(p).oper[0]^.sym).labsymabelnr-LoLab] Do
            if (RefsFound = tasmlabel(taicpu(p).oper[0]^.sym).RefCount) then
              begin
                if (InstrCnt < InstrNr)
                  then
                {forward jump}
                    if (JmpsProcessed = 0) then
                {no jump to this label has been processed yet}
                      begin
                        taiPropBlock^[InstrNr].Regs := curprop^.regs;
                        taiPropBlock^[InstrNr].DirFlag := curprop.DirFlag;
                        inc(JmpsProcessed);
                      end
                    else
                      begin
                        For tmpreg := RS_EAX to RS_EDI Do
                          if (taiPropBlock^[InstrNr].Regs[tmpreg].WState <>
                             curprop^.regs[tmpreg].WState) then
                            DestroyReg(@taiPropBlock^[InstrNr], tmpreg, true);
                        inc(JmpsProcessed);
                      end
{$ifdef AnalyzeLoops}
                  else
{                backward jump, a loop for example}
{                    if (JmpsProcessed > 0) or
                       not(GetLastInstruction(taiObj, hp) and
                           (hp.typ = ait_labeled_instruction) and
                           (taicpu_labeled(hp).opcode = A_JMP))
                      then}
{instruction prior to label is not a jmp, or at least one jump to the label
 has yet been processed}
                        begin
                          inc(JmpsProcessed);
                          For tmpreg := RS_EAX to RS_EDI Do
                            if (taiPropBlock^[InstrNr].Regs[tmpreg].WState <>
                                curprop^.regs[tmpreg].WState)
                              then
                                begin
                                  TmpState := taiPropBlock^[InstrNr].Regs[tmpreg].WState;
                                  Cnt := InstrNr;
                                  while (TmpState = taiPropBlock^[Cnt].Regs[tmpreg].WState) Do
                                    begin
                                      DestroyReg(@taiPropBlock^[Cnt], tmpreg, true);
                                      inc(Cnt);
                                    end;
                                  while (Cnt <= InstrCnt) Do
                                    begin
                                      inc(taiPropBlock^[Cnt].Regs[tmpreg].WState);
                                      inc(Cnt)
                                    end
                                end;
                        end
{                      else }
{instruction prior to label is a jmp and no jumps to the label have yet been
 processed}
{                        begin
                          inc(JmpsProcessed);
                          For tmpreg := RS_EAX to RS_EDI Do
                            begin
                              TmpState := taiPropBlock^[InstrNr].Regs[tmpreg].WState;
                              Cnt := InstrNr;
                              while (TmpState = taiPropBlock^[Cnt].Regs[tmpreg].WState) Do
                                begin
                                  taiPropBlock^[Cnt].Regs[tmpreg] := curprop^.regs[tmpreg];
                                  inc(Cnt);
                                end;
                              TmpState := taiPropBlock^[InstrNr].Regs[tmpreg].WState;
                              while (TmpState = taiPropBlock^[Cnt].Regs[tmpreg].WState) Do
                                begin
                                  DestroyReg(@taiPropBlock^[Cnt], tmpreg, true);
                                  inc(Cnt);
                                end;
                              while (Cnt <= InstrCnt) Do
                                begin
                                  inc(taiPropBlock^[Cnt].Regs[tmpreg].WState);
                                  inc(Cnt)
                                end
                            end
                        end}
{$endif AnalyzeLoops}
          end;
{$endif JumpAnal}
          end
          else
           begin
            InstrProp := InsProp[taicpu(p).opcode];
            case taicpu(p).opcode Of
              A_MOV, A_MOVZX, A_MOVSX:
                begin
                  case taicpu(p).oper[0]^.typ Of
                    top_ref, top_reg:
                      case taicpu(p).oper[1]^.typ Of
                        top_reg:
                          begin
{$ifdef statedebug}
                            hp := tai_comment.Create(strpnew('destroying '+std_regname(taicpu(p).oper[1]^.reg)));
                            insertllitem(list,p,p.next,hp);
{$endif statedebug}

                            readOp(curprop, taicpu(p).oper[0]^);
                            tmpsupreg := getsupreg(taicpu(p).oper[1]^.reg);
                            if reginop(tmpsupreg, taicpu(p).oper[0]^) and
                               (curprop^.regs[tmpsupreg].typ in [con_ref,con_noRemoveRef]) then
                              begin
                                with curprop^.regs[tmpsupreg] Do
                                  begin
                                    incState(wstate,1);
 { also store how many instructions are part of the sequence in the first }
 { instruction's ptaiprop, so it can be easily accessed from within       }
 { CheckSequence                                                          }
                                    inc(nrOfMods, nrOfInstrSinceLastMod[tmpsupreg]);
                                    ptaiprop(startmod.optinfo)^.regs[tmpsupreg].nrOfMods := nrOfMods;
                                    nrOfInstrSinceLastMod[tmpsupreg] := 0;
                                   { Destroy the contents of the registers  }
                                   { that depended on the previous value of }
                                   { this register                          }
                                    invalidateDependingRegs(curprop,tmpsupreg);
                                    curprop^.regs[tmpsupreg].memwrite := nil;
                                end;
                            end
                          else
                            begin
{$ifdef statedebug}
                              hp := tai_comment.Create(strpnew('destroying & initing '+std_regname(newreg(R_INTREGISTER,tmpsupreg,R_SUBWHOLE))));
                              insertllitem(list,p,p.next,hp);
{$endif statedebug}
                              destroyReg(curprop, tmpsupreg, true);
                              if not(reginop(tmpsupreg, taicpu(p).oper[0]^)) then
                                with curprop^.regs[tmpsupreg] Do
                                  begin
                                    typ := con_ref;
                                    startmod := p;
                                    nrOfMods := 1;
                                  end
                            end;
{$ifdef StateDebug}
                            hp := tai_comment.Create(strpnew(std_regname(newreg(R_INTREGISTER,tmpsupreg,R_SUBWHOLE))+': '+tostr(curprop^.regs[tmpsupreg].WState)));
                            insertllitem(list,p,p.next,hp);
{$endif StateDebug}
                          end;
                        top_ref:
                          begin
                            readref(curprop, taicpu(p).oper[1]^.ref);
                            if taicpu(p).oper[0]^.typ = top_reg then
                              begin
                                readreg(curprop, getsupreg(taicpu(p).oper[0]^.reg));
                                DestroyRefs(p, taicpu(p).oper[1]^.ref^, getsupreg(taicpu(p).oper[0]^.reg),topsize2tcgsize[taicpu(p).opsize]);
                                ptaiprop(p.optinfo)^.regs[getsupreg(taicpu(p).oper[0]^.reg)].memwrite :=
                                  taicpu(p);
                              end
                            else
                              DestroyRefs(p, taicpu(p).oper[1]^.ref^, RS_INVALID,topsize2tcgsize[taicpu(p).opsize]);
                          end;
                      end;
                    top_Const:
                      begin
                        case taicpu(p).oper[1]^.typ Of
                          top_reg:
                            begin
                              tmpsupreg := getsupreg(taicpu(p).oper[1]^.reg);
{$ifdef statedebug}
                              hp := tai_comment.Create(strpnew('destroying '+std_regname(newreg(R_INTREGISTER,tmpsupreg,R_SUBWHOLE))));
                              insertllitem(list,p,p.next,hp);
{$endif statedebug}
                              With curprop^.regs[tmpsupreg] Do
                                begin
                                  DestroyReg(curprop, tmpsupreg, true);
                                  typ := Con_Const;
                                  StartMod := p;
                                  nrOfMods := 1;
                                end
                            end;
                          top_ref:
                            begin
                              readref(curprop, taicpu(p).oper[1]^.ref);
                              DestroyRefs(p, taicpu(p).oper[1]^.ref^, RS_INVALID,topsize2tcgsize[taicpu(p).opsize]);
                            end;
                        end;
                      end;
                  end;
                end;
              A_DIV, A_IDIV, A_MUL:
                begin
                  ReadOp(curprop, taicpu(p).oper[0]^);
                  readreg(curprop,RS_EAX);
                  if (taicpu(p).OpCode = A_IDIV) or
                     (taicpu(p).OpCode = A_DIV) then
                    begin
                      readreg(curprop,RS_EDX);
                    end;
{$ifdef statedebug}
                  hp := tai_comment.Create(strpnew('destroying eax and edx'));
                  insertllitem(list,p,p.next,hp);
{$endif statedebug}
{                  DestroyReg(curprop, RS_EAX, true);}
                  AddInstr2RegContents({$ifdef statedebug}list,{$endif}
                    taicpu(p), RS_EAX);
                  DestroyReg(curprop, RS_EDX, true);
                  LastFlagsChangeProp := curprop;
                end;
              A_IMUL:
                begin
                  ReadOp(curprop,taicpu(p).oper[0]^);
                  if (taicpu(p).ops >= 2) then
                    ReadOp(curprop,taicpu(p).oper[1]^);
                  if (taicpu(p).ops <= 2) then
                    if (taicpu(p).ops=1) then
                      begin
                        readreg(curprop,RS_EAX);
{$ifdef statedebug}
                        hp := tai_comment.Create(strpnew('destroying eax and edx'));
                        insertllitem(list,p,p.next,hp);
{$endif statedebug}
{                        DestroyReg(curprop, RS_EAX, true); }
                        AddInstr2RegContents({$ifdef statedebug}list,{$endif}
                          taicpu(p), RS_EAX);
                        DestroyReg(curprop,RS_EDX, true)
                      end
                    else
                      AddInstr2OpContents(
                        {$ifdef statedebug}list,{$endif}
                          taicpu(p), taicpu(p).oper[1]^)
                  else
                    AddInstr2OpContents({$ifdef statedebug}list,{$endif}
                      taicpu(p), taicpu(p).oper[2]^);
                  LastFlagsChangeProp := curprop;
                end;
              A_LEA:
                begin
                  readop(curprop,taicpu(p).oper[0]^);
                  if reginref(getsupreg(taicpu(p).oper[1]^.reg),taicpu(p).oper[0]^.ref^) then
                    AddInstr2RegContents({$ifdef statedebug}list,{$endif}
                      taicpu(p), getsupreg(taicpu(p).oper[1]^.reg))
                  else
                    begin
{$ifdef statedebug}
                      hp := tai_comment.Create(strpnew('destroying & initing'+
                        std_regname(taicpu(p).oper[1]^.reg)));
                      insertllitem(list,p,p.next,hp);
{$endif statedebug}
                      destroyreg(curprop,getsupreg(taicpu(p).oper[1]^.reg),true);
                      with curprop^.regs[getsupreg(taicpu(p).oper[1]^.reg)] Do
                         begin
                           typ := con_ref;
                           startmod := p;
                           nrOfMods := 1;
                         end
                    end;
                end;
              else
                begin
                  Cnt := 1;
                  while (Cnt <= maxinschanges) and
                        (InstrProp.Ch[Cnt] <> Ch_None) Do
                    begin
                      case InstrProp.Ch[Cnt] Of
                        Ch_REAX..Ch_REDI:
                          begin
                            tmpsupreg:=tch2reg(InstrProp.Ch[Cnt]);
                            readreg(curprop,tmpsupreg);
                          end;
                        Ch_WEAX..Ch_RWEDI:
                          begin
                            if (InstrProp.Ch[Cnt] >= Ch_RWEAX) then
                              begin
                                tmpsupreg:=tch2reg(InstrProp.Ch[Cnt]);
                                readreg(curprop,tmpsupreg);
                              end;
{$ifdef statedebug}
                            hp := tai_comment.Create(strpnew('destroying '+
                              std_regname(tch2reg(InstrProp.Ch[Cnt]))));
                            insertllitem(list,p,p.next,hp);
{$endif statedebug}
                            tmpsupreg:=tch2reg(InstrProp.Ch[Cnt]);
                            DestroyReg(curprop,tmpsupreg, true);
                          end;
                        Ch_MEAX..Ch_MEDI:
                          begin
                            tmpsupreg:=tch2reg(InstrProp.Ch[Cnt]);
                            AddInstr2RegContents({$ifdef statedebug} list,{$endif}
                                                 taicpu(p),tmpsupreg);
                          end;
                        Ch_CDirFlag: curprop^.DirFlag := F_notSet;
                        Ch_SDirFlag: curprop^.DirFlag := F_Set;
                        Ch_Rop1: ReadOp(curprop, taicpu(p).oper[0]^);
                        Ch_Rop2: ReadOp(curprop, taicpu(p).oper[1]^);
                        Ch_ROp3: ReadOp(curprop, taicpu(p).oper[2]^);
                        Ch_Wop1..Ch_RWop1:
                          begin
                            if (InstrProp.Ch[Cnt] in [Ch_RWop1]) then
                              ReadOp(curprop, taicpu(p).oper[0]^);
                            DestroyOp(p, taicpu(p).oper[0]^);
                          end;
                        Ch_Mop1:
                          AddInstr2OpContents({$ifdef statedebug} list, {$endif}
                            taicpu(p), taicpu(p).oper[0]^);
                        Ch_Wop2..Ch_RWop2:
                          begin
                            if (InstrProp.Ch[Cnt] = Ch_RWop2) then
                              ReadOp(curprop, taicpu(p).oper[1]^);
                            DestroyOp(p, taicpu(p).oper[1]^);
                          end;
                        Ch_Mop2:
                          AddInstr2OpContents({$ifdef statedebug} list, {$endif}
                            taicpu(p), taicpu(p).oper[1]^);
                        Ch_WOp3..Ch_RWOp3:
                          begin
                            if (InstrProp.Ch[Cnt] = Ch_RWOp3) then
                              ReadOp(curprop, taicpu(p).oper[2]^);
                            DestroyOp(p, taicpu(p).oper[2]^);
                          end;
                        Ch_Mop3:
                          AddInstr2OpContents({$ifdef statedebug} list, {$endif}
                            taicpu(p), taicpu(p).oper[2]^);
                        Ch_WMemEDI:
                          begin
                            readreg(curprop, RS_EDI);
                            fillchar(tmpref, SizeOf(tmpref), 0);
                            tmpref.base := NR_EDI;
                            tmpref.index := NR_EDI;
                            DestroyRefs(p, tmpref,RS_INVALID,OS_32)
                          end;
                        Ch_RFlags:
                          if assigned(LastFlagsChangeProp) then
                            LastFlagsChangeProp^.FlagsUsed := true;
                        Ch_WFlags:
                          LastFlagsChangeProp := curprop;
                        Ch_RWFlags:
                          begin
                            if assigned(LastFlagsChangeProp) then
                              LastFlagsChangeProp^.FlagsUsed := true;
                            LastFlagsChangeProp := curprop;
                          end;
                         Ch_FPU:;
                        else
                          begin
{$ifdef statedebug}
                            hp := tai_comment.Create(strpnew(
                              'destroying all regs for prev instruction'));
                            insertllitem(list,p, p.next,hp);
{$endif statedebug}
                            DestroyAllRegs(curprop,true,true);
                            LastFlagsChangeProp := curprop;
                          end;
                      end;
                      inc(Cnt);
                    end
                end;
              end;
            end;
          end
        else
          begin
{$ifdef statedebug}
            hp := tai_comment.Create(strpnew(
              'destroying all regs: unknown tai: '+tostr(ord(p.typ))));
            insertllitem(list,p, p.next,hp);
{$endif statedebug}
            DestroyAllRegs(curprop,true,true);
          end;
      end;
      inc(InstrCnt);
      prev := p;
      GetNextInstruction(p, p);
    end;
end;


function tdfaobj.pass_generate_code: boolean;
begin
  if initdfapass2 then
    begin
      dodfapass2;
      pass_generate_code := true
    end
  else
    pass_generate_code := false;
end;

{$push}
{$r-}
function tdfaobj.getlabelwithsym(sym: tasmlabel): tai;
begin
  if (sym.labelnr >= lolab) and
     (sym.labelnr <= hilab) then   { range check, a jump can go past an assembler block! }
    getlabelwithsym := labeltable^[sym.labelnr-lolab].taiobj
  else
    getlabelwithsym := nil;
end;
{$pop}


procedure tdfaobj.clear;
begin
  if labdif <> 0 then
    begin
      freemem(labeltable);
      labeltable := nil;
    end;
  if assigned(taipropblock) then
    begin
      freemem(taipropblock, nroftaiobjs*sizeof(ttaiprop));
      taipropblock := nil;
    end;
end;


end.
