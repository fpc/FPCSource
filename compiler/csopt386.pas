{
    $Id$
    Copyright (c) 1998-2000 by Jonas Maebe, member of the Free Pascal
      development team

    This unit contains the common subexpression elimination procedure.

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
Unit CSOpt386;


Interface

Uses aasm;

{Procedure CSOpt386(First, Last: Pai);}
Procedure CSE(AsmL: PAasmOutput; First, Last: Pai);

Implementation

Uses
  {$ifdef replaceregdebug}cutils,{$endif}
  verbose, hcodegen, globals,cpubase,cpuasm,DAOpt386, tgeni386;

{
Function PaiInSequence(P: Pai; Const Seq: TContent): Boolean;
Var P1: Pai;
    Counter: Byte;
    TmpResult: Boolean;
Begin
  TmpResult := False;
  P1 := Seq.StartMod;
  Counter := 1;
  While Not(TmpResult) And
        (Counter <= Seq.NrOfMods) Do
    Begin
      If (P = P1) Then TmpResult := True;
      Inc(Counter);
      p1 := Pai(p1^.Next);
    End;
  PaiInSequence := TmpResult;
End;
}

function modifiesMemLocation(p1: pai): boolean;
var p: paicpu;
    opCount: byte;
begin
  modifiesMemLocation := false;
  if p1^.typ <> ait_instruction then
    exit;
  p := paicpu(p1);
  for opCount := 1 to MaxCh do
    case InsProp[p^.opcode].Ch[opCount] of
      Ch_MOp1,CH_WOp1,CH_RWOp1:
        if p^.oper[0].typ = top_ref then
          begin
            modifiesMemLocation := true;
            exit
          end;
      Ch_MOp2,CH_WOp2,CH_RWOp2:
        if p^.oper[1].typ = top_ref then
          begin
            modifiesMemLocation := true;
            exit
          end;
      Ch_MOp3,CH_WOp3,CH_RWOp3:
        if p^.oper[2].typ = top_ref then
          begin
            modifiesMemLocation := true;
            exit
          end;
      Ch_WMemEDI:
        begin
          modifiesMemLocation := true;
          exit;
        end;
    end;
end;

function getPrevSequence(reg: tregister; current: pai; var prev: pai; var passedJump: boolean):
  tregister;

  function stillValid(p: pai): boolean;
  begin
    stillValid :=
      (p^.typ = ait_instruction) and
      (paicpu(p)^.opcode <> a_jmp) and
      (ppaiprop(p^.optinfo)^.regs[reg].state =
         ppaiprop(current^.optinfo)^.regs[reg].state) and
      { in case destroyreg is called with doIncState = false }
      (ppaiprop(p^.optinfo)^.regs[reg].typ =
        ppaiprop(current^.optinfo)^.regs[reg].typ);
    passedJump :=
      (p^.typ = ait_instruction) and
      (paicpu(p)^.is_jmp);
  end;

  function findChangedRegister(p: pai): tregister;
  var
    regCounter: tregister;
  begin
    for regCounter := R_EAX to R_EDI do
      with ppaiprop(p^.optinfo)^.regs[regCounter] do
      if ((startmod <>
            ppaiprop(current^.optinfo)^.regs[regCounter].startmod)  or
          (nrOfMods <>
            ppaiprop(current^.optinfo)^.regs[regCounter].nrOfMods)) and
           (not ppaiprop(p^.optinfo)^.canBeRemoved) and
         (ppaiprop(p^.optinfo)^.regs[regCounter].typ in
           [con_ref,con_noRemoveRef]) then
        begin
          findChangedRegister := regCounter;
          exit;
        end;
    findChangedRegister := R_NO;
  end;

var
  hp, prevFound: pai;
  tmpResult: tregister;
begin
  getPrevSequence := R_NO;
  { no memory writes (could be refined further) }
    passedJump := passedJump or
      ((current^.typ = ait_instruction) and
       (paicpu(current)^.is_jmp));
  if modifiesMemLocation(current) or
     (passedJump and not(reg in (usableregs+[R_EDI]))) or
     not getLastInstruction(current,hp) then
    exit;
  tmpResult := R_NO;
  while (tmpResult = R_NO) and
        stillValid(hp) do
    begin
      { in case getPreviousInstruction fails and sets hp to nil in the }
      { next iteration                                                 }
      prevFound := hp;
      tmpResult := findChangedRegister(hp);
      if modifiesMemLocation(hp) or
        { do not load the self pointer or a regvar before a (conditional)  }
        { jump with a new value, since if the jump is taken, the old value }
        { is (probably) still necessary                                    } 
        (passedJump and not(reg in (usableregs+[R_EDI]))) or
         not getLastInstruction(hp,hp) then
        break;
    end;
  getPrevSequence := tmpResult;
  if tmpResult <> R_NO then
    prev := prevFound;
end;


{checks whether the current instruction sequence (starting with p) and the
 one between StartMod and EndMod of Reg are the same. If so, the number of
 instructions that match is stored in Found and true is returned, otherwise
 Found holds the number of instructions between StartMod and EndMod and false
 is returned}
Function CheckSequence(p: Pai; var prev: pai; Reg: TRegister; Var Found: Longint;
           Var RegInfo: TRegInfo): Boolean;


  function getNextRegToTest(var orgP: pai; currentReg: tregister): tregister;
  const
    checkingPrevSequences: boolean = false;
    passedJump: boolean = false;
  begin
    if currentReg = R_NO then
      checkingPrevSequences := false;
    if not checkingPrevSequences then
      begin
        Repeat
          Inc(currentReg);
        Until (currentReg > R_EDI) or
              (ppaiprop(orgP^.optInfo)^.regs[currentReg].typ
                 in [con_ref,con_noRemoveRef]);
        if currentReg > R_EDI then
          begin
            if not modifiesMemLocation(orgP) and
               (ppaiprop(orgP^.optinfo)^.regs[reg].rstate =
                  ppaiprop(p^.optinfo)^.regs[reg].rstate) then
              begin
                checkingPrevSequences := true;
                passedJump := false;
              end
            else
              getNextRegToTest := R_NO;
          end
        else getNextRegToTest := currentReg;
      end;
    if checkingPrevSequences then
      getNextRegToTest := getPrevSequence(reg,orgP,orgP, passedJump);
  end;

Var hp2, hp3{, EndMod},highPrev, orgPrev: Pai;
    {Cnt,} OldNrOfMods: Longint;
    startRegInfo, OrgRegInfo, HighRegInfo: TRegInfo;
    HighFound, OrgRegFound: Byte;
    RegCounter, regCounter2: TRegister;
    OrgRegResult: Boolean;
    TmpResult: Boolean;
    {TmpState: Byte;}
Begin {CheckSequence}
  Reg := Reg32(Reg);
  TmpResult := False;
  FillChar(OrgRegInfo, SizeOf(OrgRegInfo), 0);
  FillChar(startRegInfo, sizeof(startRegInfo), 0);
  OrgRegFound := 0;
  HighFound := 0;
  OrgRegResult := False;
  with startRegInfo do
    begin
      newRegsEncountered := [procinfo^.FramePointer, stack_pointer];
      new2OldReg[procinfo^.FramePointer] := procinfo^.FramePointer;
      new2OldReg[stack_pointer] := stack_pointer;
      oldRegsEncountered := newRegsEncountered;
    end;

  GetLastInstruction(p, prev);
  regCounter := getNextRegToTest(prev,R_NO);
  While (RegCounter <> R_NO) Do
    Begin
      regInfo := startRegInfo;
      Found := 0;
      hp2 := PPaiProp(prev^.OptInfo)^.Regs[RegCounter].StartMod;
      If (prev <> PPaiProp(prev^.OptInfo)^.Regs[RegCounter].StartMod)
        Then OldNrOfMods := PPaiProp(prev^.OptInfo)^.Regs[RegCounter].NrOfMods
        Else OldNrOfMods := 1;
      hp3 := p;
      While (Found <> OldNrOfMods) And
                                  { old  new }
             InstructionsEquivalent(hp2, hp3, RegInfo) Do
        Begin
          if (hp3^.typ = ait_instruction) and
             ((paicpu(hp3)^.opcode = A_MOV) or
              (paicpu(hp3)^.opcode = A_MOVZX) or
              (paicpu(hp3)^.opcode = A_MOVSX)) and
             (paicpu(hp3)^.oper[0].typ in
               [top_const,top_ref,top_symbol]) and
             (paicpu(hp3)^.oper[1].typ = top_reg) and
             not(regInRef(reg32(paicpu(hp3)^.oper[1].reg),
                   paicpu(hp3)^.oper[0].ref^)) then
            regInfo.lastReload
              [reg32(paicpu(hp3)^.oper[1].reg)] := hp3;
          GetNextInstruction(hp2, hp2);
          GetNextInstruction(hp3, hp3);
          Inc(Found)
        End;
      for regCounter2 := R_EAX to R_EDX do
        if (regInfo.new2OldReg[regCounter2] <> R_NO) and
           (regCounter2 in PPaiProp(hp3^.optInfo)^.usedRegs) and
           not regLoadedWithNewValue(regCounter2,false,hp3) then
          include(regInfo.regsStillUsedAfterSeq,regCounter2);
      If (Found <> OldNrOfMods) or
 { the following is to avoid problems with rangecheck code (see testcse2) }
         (assigned(hp3) and
          ((reg in regInfo.regsLoadedForRef) and
           (reg in PPaiProp(hp3^.optInfo)^.usedRegs) and
           not regLoadedWithNewValue(reg,false,hp3))) then
        Begin
          TmpResult := False;
          If (found > 0) then
{this is correct because we only need to turn off the CanBeRemoved flag
 when an instruction has already been processed by CheckSequence
 (otherwise CanBeRemoved can't be true and thus can't have to be turned off).
 If it has already been processed by CheckSequence and flagged to be
 removed, it means that it has been checked against a previous sequence
 and that it was equal (otherwise CheckSequence would have returned false
 and the instruction wouldn't have been removed). If this "If found > 0"
 check is left out, incorrect optimizations are performed.}
            Found := PPaiProp(Pai(p)^.OptInfo)^.Regs[Reg].NrOfMods
        End
      Else TmpResult := True;
      If TmpResult And
         (Found > HighFound)
        Then
          Begin
            highPrev := prev;
            HighFound := Found;
            HighRegInfo := RegInfo;
          End;
      If (RegCounter = Reg) Then
        Begin
          orgPrev := prev;
          OrgRegFound := Found;
          OrgRegResult := TmpResult;
          OrgRegInfo := RegInfo
        End;
      regCounter := getNextRegToTest(prev,regCounter);
    End;
  If (HighFound > 0) And
     (Not(OrgRegResult) Or
      (HighFound > OrgRegFound))
    Then
      Begin
{$ifndef fpc}
        TmpResult := True;
{$else fpc}
        CheckSequence := True;
{$endif fpc}
        prev := highPrev;
        RegInfo := HighRegInfo;
        Found := HighFound
      End
    Else
      Begin
{$ifndef fpc}
        TmpResult := OrgRegResult;
{$else fpc}
        CheckSequence := OrgRegResult;
{$endif fpc}
        prev := orgPrev;
        Found := OrgRegFound;
        RegInfo := OrgRegInfo;
      End;
{$ifndef fpc}
  CheckSequence := TmpResult;
{$endif fpc}
End; {CheckSequence}

Procedure SetAlignReg(p: Pai);
Const alignSearch = 12;
var regsUsable: TRegSet;
    prevInstrCount, nextInstrCount: Longint;
    prevState, nextWState,nextRState: Array[R_EAX..R_EDI] of byte;
    regCounter, lastRemoved: TRegister;
    prev, next: Pai;
{$ifdef alignregdebug}
    temp: Pai;
{$endif alignregdebug}
begin
  regsUsable := [R_EAX,R_ECX,R_EDX,R_EBX,{R_ESP,R_EBP,}R_ESI,R_EDI];
  for regCounter := R_EAX to R_EDI do
    begin
      prevState[regCounter] := PPaiProp(p^.optInfo)^.Regs[regCounter].wState;
      nextWState[regCounter] := PPaiProp(p^.optInfo)^.Regs[regCounter].wState;
      nextRState[regCounter] := PPaiProp(p^.optInfo)^.Regs[regCounter].rState;
    end;
  getLastInstruction(p,prev);
  getNextInstruction(p,next);
  lastRemoved := pai_align(p)^.reg;
  nextInstrCount := 0;
  prevInstrCount := 0;
  while ((assigned(prev) and
          assigned(prev^.optInfo) and
          (prevInstrCount < alignSearch)) or
         (assigned(next) and
          assigned(next^.optInfo) and
          (nextInstrCount < alignSearch))) And
        (regsUsable <> []) do
    begin
{$ifdef alignregdebug}
      if assigned(prev) then
        begin
          temp := new(pai_asm_comment,init(strpnew('got here')));
          temp^.next := prev^.next;
          temp^.previous := prev;
          prev^.next := temp;
          if assigned(temp^.next) then
            temp^.next^.previous := temp;
        end;
{$endif alignregdebug}
      if assigned(prev) and assigned(prev^.optinfo) and
         (prevInstrCount < alignSearch) then
        begin
          if (prev^.typ = ait_instruction) And
             (insProp[PaiCpu(prev)^.opcode].ch[1] <> Ch_ALL) and
             (PaiCpu(prev)^.opcode <> A_JMP) then
            begin
              inc(prevInstrCount);
              for regCounter := R_EAX to R_EDI do
                begin
                  if (regCounter in regsUsable) And
                     (PPaiProp(prev^.optInfo)^.Regs[regCounter].wState <>
                       prevState[regCounter]) then
                    begin
                      lastRemoved := regCounter;
                      exclude(regsUsable,regCounter);
{$ifdef alignregdebug}
                      temp := new(pai_asm_comment,init(strpnew(
                                att_reg2str[regCounter]+' removed')));
                      temp^.next := prev^.next;
                      temp^.previous := prev;
                      prev^.next := temp;
                      if assigned(temp^.next) then
                        temp^.next^.previous := temp;
                      if regsUsable = [] then
                        begin
                          temp := new(pai_asm_comment,init(strpnew(
                                    'regsUsable empty here')));
                          temp^.next := prev^.next;
                          temp^.previous := prev;
                          prev^.next := temp;
                          if assigned(temp^.next) then
                            temp^.next^.previous := temp;
                        end;
{$endif alignregdebug}
                    end;
                  prevState[regCounter] :=
                    PPaiProp(prev^.optInfo)^.Regs[regCounter].wState;
                end;
              getLastInstruction(prev,prev);
            end
          else
            If GetLastInstruction(prev,prev) and
               assigned(prev^.optinfo) then
              for regCounter := R_EAX to R_EDI do
                prevState[regCounter] :=
                  PPaiProp(prev^.optInfo)^.Regs[regCounter].wState
        end;
      if assigned(next) and assigned(next^.optInfo) and
         (nextInstrCount < alignSearch) then
        begin
          if (next^.typ = ait_instruction) and
             (insProp[PaiCpu(next)^.opcode].ch[1] <> Ch_ALL) and
             (PaiCpu(next)^.opcode <> A_JMP) then
            begin
              inc(nextInstrCount);
              for regCounter := R_EAX to R_EDI do
                begin
                  if (regCounter in regsUsable) And
                     ((PPaiProp(next^.optInfo)^.Regs[regCounter].wState <>
                       nextWState[regCounter]) or
                      (PPaiProp(next^.optInfo)^.Regs[regCounter].rState <>
                       nextRState[regCounter])) Then
                    begin
                      lastRemoved := regCounter;
                      exclude(regsUsable,regCounter);
{$ifdef alignregdebug}
                      temp := new(pai_asm_comment,init(strpnew(
                                att_reg2str[regCounter]+' removed')));
                      temp^.next := next^.next;
                      temp^.previous := next;
                      next^.next := temp;
                      if assigned(temp^.next) then
                        temp^.next^.previous := temp;
                      if regsUsable = [] then
                        begin
                          temp := new(pai_asm_comment,init(strpnew(
                                    'regsUsable empty here')));
                          temp^.next := next^.next;
                          temp^.previous := next;
                          next^.next := temp;
                          if assigned(temp^.next) then
                            temp^.next^.previous := temp;
                        end;
{$endif alignregdebug}
                    end;
                  nextWState[regCounter] :=
                    PPaiProp(next^.optInfo)^.Regs[regCounter].wState;
                  nextRState[regCounter] :=
                    PPaiProp(next^.optInfo)^.Regs[regCounter].rState;
                end
            end
          else
            for regCounter := R_EAX to R_EDI do
              begin
                nextWState[regCounter] :=
                  PPaiProp(next^.optInfo)^.Regs[regCounter].wState;
                nextRState[regCounter] :=
                  PPaiProp(next^.optInfo)^.Regs[regCounter].rState;
              end;
          getNextInstruction(next,next);
        end;
    end;
  if regsUsable <> [] then
    for regCounter := R_EAX to R_EDI do
      if regCounter in regsUsable then
        begin
          lastRemoved := regCounter;
          break
        end;
{$ifdef alignregdebug}
  next := new(pai_asm_comment,init(strpnew(att_reg2str[lastRemoved]+
               ' chosen as alignment register')));
  next^.next := p^.next;
  next^.previous := p;
  p^.next := next;
  if assigned(next^.next) then
    next^.next^.previous := next;
{$endif alignregdebug}
  pai_align(p)^.reg := lastRemoved;
End;

Procedure RestoreRegContentsTo(reg: TRegister; const c: TContent; p, endP: pai);
var
{$ifdef replaceregdebug}
    hp: pai;
    l: longint;
{$endif replaceregdebug}
    tmpState: byte;
begin
{$ifdef replaceregdebug}
  l := random(1000);
  hp := new(pai_asm_comment,init(strpnew(
          'restored '+att_reg2str[reg]+' with data from here... '+tostr(l))));
  hp^.next := p;
  hp^.previous := p^.previous;
  p^.previous := hp;
  if assigned(hp^.previous) then
    hp^.previous^.next := hp;
{$endif replaceregdebug}
{  PPaiProp(p^.optInfo)^.Regs[reg] := c;}
  While (p <> endP) Do
    Begin
      PPaiProp(p^.optInfo)^.Regs[reg] := c;
      getNextInstruction(p,p);
    end;
  tmpState := PPaiProp(p^.optInfo)^.Regs[reg].wState;
  repeat
    PPaiProp(p^.optInfo)^.Regs[reg] := c;
  until not getNextInstruction(p,p) or
        (PPaiProp(p^.optInfo)^.Regs[reg].wState <> tmpState);
{$ifdef replaceregdebug}
  if assigned(p) then
    begin
      hp := new(pai_asm_comment,init(strpnew(
        'restored '+att_reg2str[reg]+' till here... '+tostr(l))));
      hp^.next := p;
      hp^.previous := p^.previous;
      p^.previous := hp;
      if assigned(hp^.previous) then
        hp^.previous^.next := hp;
    end;
{$endif replaceregdebug}
end;


function FindRegDealloc(reg: tregister; p: pai): boolean;
{ assumes reg is a 32bit register }
var
  hp: pai;
  first: boolean;
begin
  findregdealloc := false;
  first := true;
  while assigned(p^.previous) and
        ((Pai(p^.previous)^.typ in (skipinstr+[ait_align])) or
         ((Pai(p^.previous)^.typ = ait_label) and
          labelCanBeSkipped(pai_label(p^.previous)))) do
    begin
      p := pai(p^.previous);
      if (p^.typ = ait_regalloc) and
         (pairegalloc(p)^.reg = reg) then
        if not(pairegalloc(p)^.allocation) then
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

Procedure ClearRegContentsFrom(reg: TRegister; p, endP: pai);
{ first clears the contents of reg from p till endP. Then the contents are }
{ cleared until the first instruction that changes reg                     }
var
{$ifdef replaceregdebug}
    hp: pai;
    l: longint;
{$endif replaceregdebug}
    oldStartmod: pai;
begin
{$ifdef replaceregdebug}
  l := random(1000);
  hp := new(pai_asm_comment,init(strpnew(
          'cleared '+att_reg2str[reg]+' from here... '+tostr(l))));
  hp^.next := p;
  hp^.previous := p^.previous;
  p^.previous := hp;
  if assigned(hp^.previous) then
    hp^.previous^.next := hp;
{$endif replaceregdebug}
  PPaiProp(p^.optInfo)^.Regs[reg].typ := con_unknown;
  While (p <> endP) Do
    Begin
      PPaiProp(p^.optInfo)^.Regs[reg].typ := con_unknown;
      getNextInstruction(p,p);
    end;
  oldStartmod := PPaiProp(p^.optInfo)^.Regs[reg].startmod;
  repeat
    PPaiProp(p^.optInfo)^.Regs[reg].typ := con_unknown;
  until not getNextInstruction(p,p) or
        (PPaiProp(p^.optInfo)^.Regs[reg].startmod <> oldStartmod);
{$ifdef replaceregdebug}
  if assigned(p) then
    begin
      hp := new(pai_asm_comment,init(strpnew(
        'cleared '+att_reg2str[reg]+' till here... '+tostr(l))));
      hp^.next := p;
      hp^.previous := p^.previous;
      p^.previous := hp;
      if assigned(hp^.previous) then
        hp^.previous^.next := hp;
    end;
{$endif replaceregdebug}
end;

function NoHardCodedRegs(p: paicpu; orgReg, newReg: tRegister): boolean;
var chCount: byte;
begin
  case p^.opcode of
    A_IMUL: noHardCodedRegs := p^.ops <> 1;
    A_SHL,A_SHR,A_SHLD,A_SHRD: noHardCodedRegs :=
      (p^.oper[0].typ <> top_reg) or
      ((orgReg <> R_ECX) and (newReg <> R_ECX));
    else
      begin
        NoHardCodedRegs := true;
        with InsProp[p^.opcode] do
          for chCount := 1 to MaxCh do
            if Ch[chCount] in ([Ch_REAX..Ch_MEDI,Ch_WMemEDI,Ch_All]-[Ch_RESP,Ch_WESP,Ch_RWESP]) then
              begin
                NoHardCodedRegs := false;
                break
              end;
      end;
  end;
end;

function ChangeReg(var Reg: TRegister; orgReg, newReg: TRegister): boolean;
begin
  changeReg := true;
  if reg = newReg then
    reg := orgReg
  else if reg = regtoreg8(newReg) then
         reg := regtoreg8(orgReg)
  else if reg = regtoreg16(newReg) then
         reg := regtoreg16(orgReg)
  else changeReg := false;
end;

function changeOp(var o: toper; orgReg, newReg: tregister): boolean;
begin
  case o.typ of
    top_reg: changeOp := changeReg(o.reg,orgReg,newReg);
    top_ref:
      begin
        changeOp :=
          changeReg(o.ref^.base,orgReg,newReg) or
          changeReg(o.ref^.index,orgReg,newReg);
      end;
  end;
end;

procedure updateStates(orgReg,newReg: tregister; hp: pai; writeStateToo: boolean);
var
  prev: pai;
  newOrgRegRState, newOrgRegWState: byte;
begin
  if getLastInstruction(hp,prev) then
    with ppaiprop(prev^.optinfo)^ do
      begin
        newOrgRegRState := regs[orgReg].rState +
          ppaiprop(hp^.optinfo)^.regs[newReg].rState - regs[newReg].rstate;
        if writeStateToo then
          newOrgRegWState := regs[orgReg].wState +
            ppaiprop(hp^.optinfo)^.regs[newReg].wState - regs[newReg].wstate;
      end
  else
    with ppaiprop(hp^.optinfo)^.regs[newReg] do
      begin
        newOrgRegRState := rState;
        if writeStateToo then
          newOrgRegWState := wState;
      end;
  with ppaiprop(hp^.optinfo)^.regs[orgReg] do
    begin
      rState := newOrgRegRState;
      if writeStateToo then
        wState := newOrgRegwState;
    end;
end;

function doReplaceReg(orgReg,newReg: tregister; hp: paicpu): boolean;
var
  opCount: byte;
  tmpResult: boolean;
begin
  for opCount := 0 to 2 do
    tmpResult :=
      changeOp(hp^.oper[opCount],orgReg,newReg) or tmpResult;
  doReplaceReg := tmpResult;
end;

function RegSizesOK(oldReg,newReg: TRegister; p: paicpu): boolean;
{ oldreg and newreg must be 32bit components }
var opCount: byte;
begin
  RegSizesOK := true;
  { if only one of them is a general purpose register ... }
  if (IsGP32reg(oldReg) xor IsGP32Reg(newReg)) then
    begin
      for opCount := 0 to 2 do
        if (p^.oper[opCount].typ = top_reg) and
           (p^.oper[opCount].reg in [R_AL..R_DH]) then
          begin
            RegSizesOK := false;
            break
          end
    end;
end;

function doReplaceReadReg(orgReg,newReg: tregister; p: paicpu): boolean;
var opCount: byte;
begin
  doReplaceReadReg := false;
  { handle special case }
  case p^.opcode of
    A_IMUL:
      begin
        case p^.ops of
          1: internalerror(1301001);
          2,3:
            begin
              if changeOp(p^.oper[0],orgReg,newReg) then
                begin
{                  updateStates(orgReg,newReg,p,false);}
                  doReplaceReadReg := true;
                end;
             if p^.ops = 3 then
                if changeOp(p^.oper[1],orgReg,newReg) then
                  begin
{                    updateStates(orgReg,newReg,p,false);}
                    doReplaceReadReg := true;
                  end;
            end;
        end;
      end;
    A_DIV,A_IDIV,A_MUL: internalerror(1301002);
    else
      begin
        for opCount := 0 to 2 do
          if p^.oper[opCount].typ = top_ref then
            if changeOp(p^.oper[opCount],orgReg,newReg) then
              begin
{                updateStates(orgReg,newReg,p,false);}
                doReplaceReadReg := true;
              end;
        for opCount := 1 to MaxCh do
          case InsProp[p^.opcode].Ch[opCount] of
            Ch_ROp1:
              if p^.oper[0].typ = top_reg then
                if changeReg(p^.oper[0].reg,orgReg,newReg) then
                  begin
{                    updateStates(orgReg,newReg,p,false);}
                    doReplaceReadReg := true;
                  end;
            Ch_ROp2:
              if p^.oper[1].typ = top_reg then
                if changeReg(p^.oper[1].reg,orgReg,newReg) then
                  begin
{                    updateStates(orgReg,newReg,p,false);}
                    doReplaceReadReg := true;
                  end;
            Ch_ROp3:
              if p^.oper[2].typ = top_reg then
                if changeReg(p^.oper[2].reg,orgReg,newReg) then
                  begin
{                    updateStates(orgReg,newReg,p,false);}
                    doReplaceReadReg := true;
                  end;
          end;
      end;
  end;
end;


procedure updateState(reg: tregister; p: pai);
{ this procedure updates the read and write states of the instructions }
{ coming after p. It's called when the read/write state of p has been  }
{ changed and this change has to be propagated to the following        }
{ instructions as well                                                 }
var
  newRState, newWState: byte;
  prevRState, prevWState: byte;
  doRState, doWState: boolean;
begin
  { get the new read/write states from p }
  with ppaiprop(p^.optinfo)^.regs[reg] do
    begin
      newRState := rState;
      newWState := wState;
    end;
  if not GetNextInstruction(p,p) then
    exit;
  { get the old read/write states from the next instruction, to know }
  { when we can stop updating                                        }
  with ppaiprop(p^.optinfo)^.regs[reg] do
    begin
      prevRState := rState;
      prevWState := wState;
    end;
  { adjust the states if this next instruction reads/writes the register }
  if regReadByInstruction(reg,p) then
    incState(newRState,1);
  if regModifiedByInstruction(reg,p) then
    incState(newWState,1);
  { do we still have to update the read and/or write states? }
  doRState := true;
  doWState := true;
  repeat
    { update the states }
    with ppaiprop(p^.optinfo)^.regs[reg] do
      begin
        if doRState then
          rState := newRState;
        if doWState then
          wState := newWState;
      end;
    if not getNextInstruction(p,p) then
      break;
    with ppaiprop(p^.optinfo)^.regs[reg] do
      begin
        { stop updating the read state if it changes }
        doRState :=
          doRState and (rState = prevRState);
        { if, by accident, this changed state is the same as the one }
        { we've been using, change it to a value that's definitely   }
        { different from the previous and next state                 }
        if not doRState and
           (rState = newRState) then
          begin
            incState(newRState,1);
            prevRState := rState; 
            doRState := true;
          end;
        { ditto for the write state }
        doWState :=
          doWState and (WState = prevWState);
        if not doWState and
           (wState = newWState) then
          begin
            incState(newWState,1);
            prevWState := wState; 
            doWState := true;
          end;
      end;
  { stop when we don't have to update either state anymore }
  until not(doRState or doWState);
end;


function ReplaceReg(asmL: PaasmOutput; orgReg, newReg: TRegister; p: pai;
           const c: TContent; orgRegCanBeModified: Boolean;
           var returnEndP: pai): Boolean;
{ Tries to replace orgreg with newreg in all instructions coming after p }
{ until orgreg gets loaded with a new value. Returns true if successful, }
{ false otherwise. If successful, the contents of newReg are set to c,   }
{ which should hold the contents of newReg before the current sequence   }
{ started                                                                }
{ if the function returns true, returnEndP holds the last instruction    }
{ where newReg was replaced by orgReg                                    }
var endP, hp: Pai;
    removeLast, sequenceEnd, tmpResult, newRegModified, orgRegRead,
      stateChanged, readStateChanged: Boolean;

  function storeBack(p1: pai): boolean;
  { returns true if p1 contains an instruction that stores the contents }
  { of newReg back to orgReg                                            }
  begin
    storeBack :=
      (p1^.typ = ait_instruction) and
      (paicpu(p1)^.opcode = A_MOV) and
      (paicpu(p1)^.oper[0].typ = top_reg) and
      (paicpu(p1)^.oper[0].reg = newReg) and
      (paicpu(p1)^.oper[1].typ = top_reg) and
      (paicpu(p1)^.oper[1].reg = orgReg);
  end;

begin
  ReplaceReg := false;
  tmpResult := true;
  sequenceEnd := false;
  newRegModified := false;
  orgRegRead := false;
  removeLast := false;
  endP := p;
  while tmpResult and not sequenceEnd do
    begin
      tmpResult :=
        getNextInstruction(endP,endP) and
        (endP^.typ = ait_instruction);
      if tmpresult and not assigned(endP^.optInfo) then
        begin
{          hp := new(pai_asm_comment,init(strpnew('next no optinfo')));
          hp^.next := endp;
          hp^.previous := endp^.previous;
          endp^.previous := hp;
          if assigned(hp^.previous) then
            hp^.previous^.next := hp;}
          exit;
        end;
      If tmpResult and
         { don't take into account instructions that will be removed }
         Not (PPaiProp(endP^.optInfo)^.canBeRemoved) then
        begin
          { if the newReg gets stored back to the oldReg, we can change }
          { "mov %oldReg,%newReg; <operations on %newReg>; mov %newReg, }
          { %oldReg" to "<operations on %oldReg>"                       }
          removeLast := storeBack(endP);
          sequenceEnd :=
            { no support for (i)div, mul and imul with hardcoded operands }
            (noHardCodedRegs(paicpu(endP),orgReg,newReg) and
            { if newReg gets loaded with a new value, we can stop   }
            { replacing newReg with oldReg here (possibly keeping   }
            { the original contents of oldReg so we still know them }
            { afterwards)                                           }
             RegLoadedWithNewValue(newReg,true,paicpu(endP)) or
            { we can also stop if we reached the end of the use of }
            { newReg's current contents                            }
             (GetNextInstruction(endp,hp) and
              FindRegDealloc(newReg,hp)));
          { to be able to remove the first and last instruction of  }
          {   movl %reg1, %reg2                                     }
          {   <operations on %reg2> (replacing reg2 with reg1 here) }
          {   movl %reg2, %reg1                                     }
          { %reg2 must not be use afterwards (it can be as the      }
          { result of a peepholeoptimization)                       }
          removeLast := removeLast and sequenceEnd;
          newRegModified :=
            newRegModified or
            (not(regLoadedWithNewValue(newReg,true,paicpu(endP))) and
             RegModifiedByInstruction(newReg,endP));
          orgRegRead := newRegModified and RegReadByInstruction(orgReg,endP);
          sequenceEnd := SequenceEnd and
                         (removeLast or
    { since newReg will be replaced by orgReg, we can't allow that newReg }
    { gets modified if orgReg is still read afterwards (since after       }
    { replacing, this would mean that orgReg first gets modified and then }
    { gets read in the assumption it still contains the unmodified value) }
                         not(newRegModified and orgRegRead)) (* and
    { since newReg will be replaced by orgReg, we can't allow that newReg }
    { gets modified if orgRegCanBeModified = false                        }
    
    { this now gets checked after the loop (JM) }
                         (orgRegCanBeModified or not(newRegModified)) *);
          tmpResult :=
            not(removeLast) and
            not(newRegModified and orgRegRead) and
(*            (orgRegCanBeModified or not(newRegModified)) and *)
            (endP^.typ = ait_instruction) and
            not(paicpu(endP)^.is_jmp) and
            NoHardCodedRegs(paicpu(endP),orgReg,newReg) and
            RegSizesOk(orgReg,newReg,paicpu(endP)) and
            not RegModifiedByInstruction(orgReg,endP);
        end;
    end;
  sequenceEnd := sequenceEnd and
     (removeLast  or
      (orgRegCanBeModified or not(newRegModified))) and
     (not(assigned(endp)) or
      not(endp^.typ = ait_instruction) or
      (noHardCodedRegs(paicpu(endP),orgReg,newReg) and
       RegSizesOk(orgReg,newReg,paicpu(endP)) and
       not(newRegModified and
           (orgReg in PPaiProp(endP^.optInfo)^.usedRegs) and
           not(RegLoadedWithNewValue(orgReg,true,paicpu(endP))))));
  if SequenceEnd then
    begin
{$ifdef replaceregdebug}
      hp := new(pai_asm_comment,init(strpnew(
        'replacing '+att_reg2str[newreg]+' with '+att_reg2str[orgreg]+
        ' from here...')));
      hp^.next := p;
      hp^.previous := p^.previous;
      p^.previous := hp;
      if assigned(hp^.previous) then
        hp^.previous^.next := hp;

      hp := new(pai_asm_comment,init(strpnew(
        'replaced '+att_reg2str[newreg]+' with '+att_reg2str[orgreg]+
        ' till here')));
      hp^.next := endp^.next;
      hp^.previous := endp;
      endp^.next := hp;
      if assigned(hp^.next) then
        hp^.next^.previous := hp;
{$endif replaceregdebug}
      replaceReg := true;
      returnEndP := endP;

      getNextInstruction(p,hp);
      stateChanged := false;
      while hp <> endP do
        begin
          if {not(PPaiProp(hp^.optInfo)^.canBeRemoved) and }
             (hp^.typ = ait_instruction) then
            stateChanged := 
              doReplaceReg(orgReg,newReg,paicpu(hp)) or stateChanged;
            if stateChanged then
              updateStates(orgReg,newReg,hp,true);
          getNextInstruction(hp,hp)
        end;
      if assigned(endp) and (endp^.typ = ait_instruction) then
        readStateChanged :=
          DoReplaceReadReg(orgReg,newReg,paicpu(endP));
      if stateChanged or readStateChanged then
        updateStates(orgReg,newReg,endP,stateChanged);

      if stateChanged or readStateChanged then
        updateState(orgReg,endP);

{ the replacing stops either at the moment that                             }
{  a) the newreg gets loaded with a new value (one not depending on the     }
{     current value of newreg)                                              }
{  b) newreg is completely replaced in this sequence and it's current value }
{     isn't used anymore                                                    }
{ In case b, the newreg was completely replaced by oldreg, so it's contents }
{ are unchanged compared the start of this sequence, so restore them        }
      If removeLast or
         RegLoadedWithNewValue(newReg,true,endP) then
        GetLastInstruction(endP,hp)
      else hp := endP;
      if removeLast or
         (p <> endp) or
         not RegLoadedWithNewValue(newReg,true,endP) then
        RestoreRegContentsTo(newReg,c,p,hp);

{ In both case a and b, it is possible that the new register was modified   }
{ (e.g. an add/sub), so if it was replaced by oldreg in that instruction,   }
{ oldreg's contents have been changed. To take this into account, we simply }
{ set the contents of orgreg to "unknown" after this sequence               }
      if newRegModified then
        ClearRegContentsFrom(orgReg,p,hp);
      if removeLast then
        ppaiprop(endP^.optinfo)^.canBeRemoved := true;
      allocRegBetween(asml,orgReg,p,endP);

    end
{$ifdef replaceregdebug}
     else
       begin
         hp := new(pai_asm_comment,init(strpnew(
           'replacing '+att_reg2str[newreg]+' with '+att_reg2str[orgreg]+
           ' from here...')));
         hp^.previous := p^.previous;
         hp^.next := p;
         p^.previous := hp;
        if assigned(hp^.previous) then
          hp^.previous^.next := hp;

      hp := new(pai_asm_comment,init(strpnew(
        'replacing '+att_reg2str[newreg]+' with '+att_reg2str[orgreg]+
        ' failed here')));
      hp^.next := endp^.next;
      hp^.previous := endp;
      endp^.next := hp;
      if assigned(hp^.next) then
        hp^.next^.previous := hp;
       end;
{$endif replaceregdebug}
End;

Function FindRegWithConst(p: Pai; size: topsize; l: longint; Var Res: TRegister): Boolean;
{Finds a register which contains the constant l}
Var Counter: TRegister;
{$ifdef testing}
    hp: pai;
{$endif testing}
    tmpresult: boolean;
Begin
  Counter := R_NO;
  repeat
     inc(counter);
     tmpresult := (ppaiprop(p^.optInfo)^.regs[counter].typ in
         [con_const,con_noRemoveConst]) and
       (paicpu(PPaiProp(p^.OptInfo)^.Regs[Counter].StartMod)^.opsize = size) and
       (paicpu(PPaiProp(p^.OptInfo)^.Regs[Counter].StartMod)^.oper[0].typ = top_const) and
       (paicpu(PPaiProp(p^.OptInfo)^.Regs[Counter].StartMod)^.oper[0].val = l);
{$ifdef testing}
     if (ppaiprop(p^.optInfo)^.regs[counter].typ in [con_const,con_noRemoveConst]) then
       begin
         hp := new(pai_asm_comment,init(strpnew(
           'checking const load of '+tostr(l)+' here...')));
         hp^.next := PPaiProp(p^.OptInfo)^.Regs[Counter].StartMod;
         hp^.previous := PPaiProp(p^.OptInfo)^.Regs[Counter].StartMod^.previous;
         PPaiProp(p^.OptInfo)^.Regs[Counter].StartMod^.previous := hp;
         if assigned(hp^.previous) then
           hp^.previous^.next := hp;
       end;
{$endif testing}
  until tmpresult or (Counter = R_EDI);
  res := counter;
  FindRegWithConst := tmpResult;
End;

procedure removePrevNotUsedLoad(p: pai; reg: tRegister; check: boolean);
{ If check = true, it means the procedure has to check whether it isn't  }
{ possible that the contents are still used after p (used when removing  }
{ instructions because of a "call"), otherwise this is not necessary     }
{ (e.g. when you have a "mov 8(%ebp),%eax", you can be sure the previous }
{ value of %eax isn't used anymore later on)                             }
var
  hp1: pai;
begin
  if getLastInstruction(p,hp1) then
    with ppaiprop(hp1^.optInfo)^.regs[reg] do
      if (typ in [con_ref,con_invalid]) and
         (nrOfMods = 1) and
         (rState = ppaiprop(startmod^.optInfo)^.regs[reg].rState) and
         (not(check) or
          (not(regInInstruction(reg,p)) and
           (not(reg in usableregs) and
            (startmod^.typ = ait_instruction) and
            ((paicpu(startmod)^.opcode = A_MOV) or
             (paicpu(startmod)^.opcode = A_MOVZX) or
             (paicpu(startmod)^.opcode = A_MOVSX)) and
            (paicpu(startmod)^.oper[0].typ = top_ref) and
            (paicpu(startmod)^.oper[0].ref^.base = stack_pointer)) or
           not(reg in ppaiprop(hp1^.optInfo)^.usedRegs) or
           findRegDealloc(reg,p))) then
        ppaiprop(startMod^.optInfo)^.canBeRemoved := true;
end;

Procedure DoCSE(AsmL: PAasmOutput; First, Last: Pai);
{marks the instructions that can be removed by RemoveInstructs. They're not
 removed immediately because sometimes an instruction needs to be checked in
 two different sequences}
var cnt, cnt2, cnt3: longint;
    p, hp1, hp2, prevSeq, prevSeq_next: Pai;
    hp3, hp4: pai;
    hp5 : pai;
    RegInfo: TRegInfo;
    RegCounter: TRegister;
Begin
  p := First;
  SkipHead(p);
  First := p;
  While (p <> Last) Do
    Begin
      Case p^.typ Of
        ait_align:
          if not(pai_align(p)^.use_op) then
            SetAlignReg(p);
        ait_instruction:
          Begin
            Case Paicpu(p)^.opcode Of
              A_CALL:
                for regCounter := R_EAX to R_EBX do
                  removePrevNotUsedLoad(p,regCounter,true);
              A_CLD: If GetLastInstruction(p, hp1) And
                        (PPaiProp(hp1^.OptInfo)^.DirFlag = F_NotSet) Then
                       PPaiProp(Pai(p)^.OptInfo)^.CanBeRemoved := True;
              A_MOV, A_MOVZX, A_MOVSX:
                Begin
                  Case Paicpu(p)^.oper[0].typ Of
                    Top_Ref:
                      Begin {destination is always a register in this case}
                        With PPaiProp(p^.OptInfo)^.Regs[Reg32(Paicpu(p)^.oper[1].reg)] Do
                          Begin
                            If (p = StartMod) And
                               GetLastInstruction (p, hp1) And
                               (hp1^.typ <> ait_marker) Then
{so we don't try to check a sequence when p is the first instruction of the block}
                              begin
{$ifdef csdebug}
                               hp5 := new(pai_asm_comment,init(strpnew(
                                 'cse checking '+att_reg2str[Reg32(Paicpu(p)^.oper[1].reg)])));
                               insertLLItem(asml,p,p^.next,hp5);
{$endif csdebug}
                               If CheckSequence(p,prevSeq,Paicpu(p)^.oper[1].reg, Cnt, RegInfo) And
                                  (Cnt > 0) Then
                                 Begin
                                   hp1 := nil;
{ although it's perfectly ok to remove an instruction which doesn't contain }
{ the register that we've just checked (CheckSequence takes care of that),  }
{ the sequence containing this other register should also be completely     }
{ checked and removed, otherwise we may get situations like this:           }
{                                                                           }
{   movl 12(%ebp), %edx                       movl 12(%ebp), %edx           }
{   movl 16(%ebp), %eax                       movl 16(%ebp), %eax           }
{   movl 8(%edx), %edx                        movl 8(%edx), %edx            }
{   movl (%eax), eax                          movl (%eax), eax              }
{   cmpl %eax, %edx                           cmpl %eax, %edx               }
{   jnz  l123           getting converted to  jnz  l123                     }
{   movl 12(%ebp), %edx                       movl 4(%eax), eax             }
{   movl 16(%ebp), %eax                                                     }
{   movl 8(%edx), %edx                                                      }
{   movl 4(%eax), eax                                                       }
                                   hp2 := p;
                                   Cnt2 := 1;
                                   While Cnt2 <= Cnt Do
                                     Begin
                                       If Not(RegInInstruction(Paicpu(hp2)^.oper[1].reg, p)) then
                                         begin
                                           if ((p^.typ = ait_instruction) And
                                               ((paicpu(p)^.OpCode = A_MOV)  or
                                                (paicpu(p)^.opcode = A_MOVZX) or
                                                (paicpu(p)^.opcode = A_MOVSX)) And
                                               (paicpu(p)^.Oper[0].typ in
                                                 [top_const,top_ref,top_symbol])) and
                                               (paicpu(p)^.oper[1].typ = top_reg) then
                                             begin
                                               regCounter := reg32(paicpu(p)^.oper[1].reg);
                                               if (regCounter in reginfo.regsStillUsedAfterSeq) then
                                                 begin
                                                   if (hp1 = nil) then
                                                     hp1 := reginfo.lastReload[regCounter];
                                                 end
{$ifndef noremove}
                                               else
                                                 begin
                                                   hp5 := p;
                                                   for cnt3 := ppaiprop(p^.optinfo)^.regs[regCounter].nrofmods downto 1 do
                                                     begin
                                                       if regModifiedByInstruction(regCounter,hp5) then
                                                         PPaiProp(hp5^.OptInfo)^.CanBeRemoved := True;
                                                       getNextInstruction(hp5,hp5);
                                                     end;
                                                 end
{$endif noremove}
                                             end
                                         end
{$ifndef noremove}
                                       else
                                         PPaiProp(p^.OptInfo)^.CanBeRemoved := True
{$endif noremove}
                                       ; Inc(Cnt2);
                                       GetNextInstruction(p, p);
                                     End;
 {hp4 is used to get the contents of the registers before the sequence}
                                   GetLastInstruction(hp2, hp4);

                                   getNextInstruction(prevSeq,prevSeq_next);
{$IfDef CSDebug}
              For RegCounter := R_EAX To R_EDI Do
                If (RegCounter in RegInfo.RegsLoadedForRef) Then
                  Begin
           hp5 := new(pai_asm_comment,init(strpnew('New: '+att_reg2str[RegCounter]+', Old: '+
                                                  att_reg2str[RegInfo.New2OldReg[RegCounter]])));
           InsertLLItem(AsmL, Pai(hp2^.previous), hp2, hp5);
                  End;
{$EndIf CSDebug}
 { If some registers were different in the old and the new sequence, move }
 { the contents of those old registers to the new ones                    }
                                   For RegCounter := R_EAX To R_EDI Do
                                     If Not(RegCounter in [R_ESP,procinfo^.framepointer]) And
                                        (RegInfo.New2OldReg[RegCounter] <> R_NO) Then
                                       Begin
                                         AllocRegBetween(AsmL,RegInfo.New2OldReg[RegCounter],
                                           PPaiProp(prevSeq^.OptInfo)^.Regs[RegInfo.New2OldReg[RegCounter]].StartMod,prevSeq_next);
                                         if hp4 <> prevSeq then
                                           begin
                                             if assigned(reginfo.lastReload[regCounter]) then
                                               getLastInstruction(reginfo.lastReload[regCounter],hp3)
                                             else hp3 := hp4;
                                             if prevSeq <> hp3 then
                                               clearRegContentsFrom(regCounter,prevSeq_next,
                                                 hp3);
                                             allocRegBetween(asmL,regCounter,prevSeq,hp3);
                                           end;
                                         If Not(RegCounter In RegInfo.RegsLoadedForRef) And
                                                        {old reg                new reg}
                                            (RegInfo.New2OldReg[RegCounter] <> RegCounter) Then
                                           Begin
                                             getLastInstruction(p,hp3);
                                             If (hp4 <> prevSeq) or
                                                not(regCounter in usableRegs + [R_EDI,R_ESI]) or
                                                not ReplaceReg(asmL,RegInfo.New2OldReg[RegCounter],
                                                      regCounter,hp3,
                                                      PPaiProp(PrevSeq^.optInfo)^.Regs[regCounter],true,hp5) then
                                               begin
                                                 hp3 := New(Pai_Marker,Init(NoPropInfoEnd));
                                                 InsertLLItem(AsmL, prevSeq, Pai(prevSeq^.next), hp3);
                                                 hp3 := New(Paicpu,Op_Reg_Reg(A_MOV, S_L,
                                                                         {old reg          new reg}
                                                       RegInfo.New2OldReg[RegCounter], RegCounter));
                                                 InsertLLItem(AsmL, prevSeq, Pai(prevSeq^.next), hp3);
                                                 hp3 := New(Pai_Marker,Init(NoPropInfoStart));
                                                 InsertLLItem(AsmL, prevSeq, Pai(prevSeq^.next), hp3);
                                                 { adjusts states in previous instruction so that it will  }
                                                 { definitely be different from the previous or next state }
                                                 incstate(ppaiprop(prevSeq_next^.optinfo)^.
                                                   regs[RegInfo.New2OldReg[RegCounter]].rstate,20);
                                                 incstate(ppaiprop(prevSeq_next^.optinfo)^.
                                                   regs[regCounter].wstate,20);
                                                 updateState(RegInfo.New2OldReg[RegCounter],
                                                   prevSeq_next);
                                               end
                                           End
                                         Else
{   imagine the following code:                                            }
{        normal                    wrong optimized                         }
{    movl 8(%ebp), %eax           movl 8(%ebp), %eax                       }
{    movl (%eax), %eax            movl (%eax), %eax                        }
{    cmpl 8(%ebp), %eax           cmpl 8(%ebp), %eax                       }
{    jne l1                       jne l1                                   }
{    movl 8(%ebp), %eax                                                    }
{    movl (%eax), %edi            movl %eax, %edi                          }
{    movl %edi, -4(%ebp)          movl %edi, -4(%ebp)                      }
{    movl 8(%ebp), %eax                                                    }
{    pushl 70(%eax)               pushl 70(%eax)                           }
{                                                                          }
{   The error is that at the moment that the last instruction is executed, }
{   %eax doesn't contain 8(%ebp) anymore. Solution: the contents of        }
{   registers that are completely removed from a sequence (= registers in  }
{   RegLoadedForRef, have to be changed to their contents from before the  }
{   sequence.                                                              }
                                         If RegCounter in RegInfo.RegsLoadedForRef Then
                                           Begin
                                             hp3 := hp2;
                                             { cnt still holds the number of instructions }
                                             { of the sequence, so go to the end of it    }
                                             for cnt2 := 1 to pred(cnt) Do
                                               getNextInstruction(hp3,hp3);
                                             { hp4 = instruction prior to start of sequence }
                                             restoreRegContentsTo(regCounter,
                                               PPaiProp(hp4^.OptInfo)^.Regs[RegCounter],
                                               hp2,hp3);
                                           End;
                                       End;
                                   If hp1 <> nil Then
                                     p := hp1;
                                   Continue;
                                 End
                               Else
                                 If (PPaiProp(p^.OptInfo)^.
                                      regs[reg32(paicpu(p)^.oper[1].reg)].typ
                                        in [con_ref,con_noRemoveRef]) and
                                    (PPaiProp(p^.OptInfo)^.CanBeRemoved) Then
                                   if (cnt > 0) then
                                     begin
                                       hp2 := p;
                                       Cnt2 := 1;
                                       While Cnt2 <= Cnt Do
                                         Begin
                                           If RegInInstruction(Paicpu(hp2)^.oper[1].reg, p) Then
                                             PPaiProp(p^.OptInfo)^.CanBeRemoved := False;
                                           Inc(Cnt2);
                                           GetNextInstruction(p, p);
                                         End;
                                       Continue;
                                     End
                                   else
                                     begin
                                       { Fix for web bug 972 }
                                       regCounter := Reg32(Paicpu(p)^.oper[1].reg);
                                       cnt := PPaiProp(p^.optInfo)^.Regs[regCounter].nrOfMods;
                                       hp3 := p;
                                       for cnt2 := 1 to cnt do
                                         if not(regModifiedByInstruction(regCounter,hp3) and
                                                not(PPaiProp(hp3^.optInfo)^.canBeRemoved)) then
                                           getNextInstruction(hp3,hp3)
                                         else
                                           break;
                                       getLastInstruction(p,hp4);
                                       RestoreRegContentsTo(regCounter,
                                         PPaiProp(hp4^.optInfo)^.Regs[regCounter],
                                         p,hp3);
                                     end;
                              End;
                          End;
                        if not ppaiprop(p^.optinfo)^.canBeRemoved and
                           not regInRef(reg32(paicpu(p)^.oper[1].reg),
                                        paicpu(p)^.oper[0].ref^) then
                          removePrevNotUsedLoad(p,reg32(paicpu(p)^.oper[1].reg),false);
                      End;
                    top_Reg:
                      { try to replace the new reg with the old reg }
                      if not(PPaiProp(p^.optInfo)^.canBeRemoved) and
                         { only remove if we're not storing something in a regvar }
                         (paicpu(p)^.oper[1].reg in (usableregs+[R_EDI])) and
                         (paicpu(p)^.opcode = A_MOV) and
                         getLastInstruction(p,hp4) then
                        begin
                          case paicpu(p)^.oper[1].typ of
                            top_Reg:
                              { we only have to start replacing from the instruction after the mov, }
                              { but replacereg only starts with getnextinstruction(p,p)             }
                              if ReplaceReg(asmL,paicpu(p)^.oper[0].reg,
                                   paicpu(p)^.oper[1].reg,p,
                                   PPaiProp(hp4^.optInfo)^.Regs[paicpu(p)^.oper[1].reg],false,hp1) then
                                begin
                                    PPaiProp(p^.optInfo)^.canBeRemoved := true;
                                    allocRegBetween(asmL,paicpu(p)^.oper[0].reg,
                                    PPaiProp(p^.optInfo)^.regs[paicpu(p)^.oper[0].reg].startMod,
                                    hp1);
                                end;
                          end
                        end;
                    top_symbol,Top_Const:
                      Begin
                        Case Paicpu(p)^.oper[1].typ Of
                          Top_Reg:
                            Begin
                              regCounter := Reg32(Paicpu(p)^.oper[1].reg);
                              If GetLastInstruction(p, hp1) Then
                                With PPaiProp(hp1^.OptInfo)^.Regs[regCounter] Do
                                  if (typ in [con_const,con_noRemoveConst]) and
                                     (paicpu(startMod)^.opsize >= paicpu(p)^.opsize) and
                                     opsequal(paicpu(StartMod)^.oper[0],paicpu(p)^.oper[0]) Then
                                    begin
                                      PPaiProp(p^.OptInfo)^.CanBeRemoved := True;
                                      allocRegBetween(asmL,regCounter,startMod,p);
                                    end;
                            End;
                          Top_Ref:
                            if (paicpu(p)^.oper[0].typ = top_const) and
                               getLastInstruction(p,hp1) and
                               findRegWithConst(hp1,paicpu(p)^.opsize,paicpu(p)^.oper[0].val,regCounter) then
                              begin
                                paicpu(p)^.loadreg(0,regCounter);
                                allocRegBetween(AsmL,reg32(regCounter),
                                  PPaiProp(hp1^.optinfo)^.regs[regCounter].startMod,p);
                              end;
                        End;
                      End;
                  End;
                End;
              A_STD: If GetLastInstruction(p, hp1) And
                        (PPaiProp(hp1^.OptInfo)^.DirFlag = F_Set) Then
                        PPaiProp(Pai(p)^.OptInfo)^.CanBeRemoved := True;
            End
          End;
      End;
      GetNextInstruction(p, p);
    End;
End;

Procedure RemoveInstructs(AsmL: PAasmOutput; First, Last: Pai);
{ Removes the marked instructions and disposes the PPaiProps of the other }
{ instructions                                                            }
Var p, hp1: Pai;
begin
  p := First;
  While (p <> Last) Do
    Begin
      If (p^.typ = ait_marker) and
         (pai_marker(p)^.kind in [noPropInfoStart,noPropInfoEnd]) then
        begin
          hp1 := pai(p^.next);
          asmL^.remove(p);
          dispose(p,done);
          p := hp1
        end
      else
{$ifndef noinstremove}
        if assigned(p^.optInfo) and
              PPaiProp(p^.optInfo)^.canBeRemoved then
          begin
{$IfDef TP}
            Dispose(PPaiProp(p^.OptInfo));
{$EndIf}
            hp1 := pai(p^.next);
            AsmL^.Remove(p);
            Dispose(p, Done);
            p := hp1;
          End
        Else
{$endif noinstremove}
          Begin
{$IfDef TP}
            if assigned(p^.optInfo) then
              Dispose(PPaiProp(p^.OptInfo));
{$EndIf TP}
            p^.OptInfo := nil;
            p := pai(p^.next);;
          End;
    End;
{$IfNDef TP}
    FreeMem(PaiPropBlock, NrOfPaiObjs*(((SizeOf(TPaiProp)+3)div 4)*4))
{$EndIf TP}
End;

Procedure CSE(AsmL: PAasmOutput; First, Last: Pai);
Begin
  DoCSE(AsmL, First, Last);
  RemoveInstructs(AsmL, First, Last);
End;

End.

{
  $Log$
  Revision 1.9  2000-09-22 15:01:59  jonas
    * fixed some bugs in the previous improvements: in some cases, esi was
      still being replaced before a conditional jump (the code that
      detected conditional jumps sometimes skipped over them)

  Revision 1.8  2000/09/20 15:00:58  jonas
    + much improved CSE: the CSE now searches further back for sequences it
      can reuse. After I've also implemented register renaming, the effect
      should be even better (afaik web bug 1088 will then even be optimized
      properly). I don't know about the slow down factor this adds. Maybe
      a new optimization level should be introduced?

  Revision 1.7  2000/08/25 19:40:45  jonas
    * refined previous fix a bit, some instructions weren't being removed
      while they could (merged from fixes branch)
    * made checksequence a bit faster

  Revision 1.6  2000/08/23 12:55:10  jonas
    * fix for web bug 1112 and a bit of clean up in csopt386 (merged from
      fixes branch)

  Revision 1.5  2000/08/04 20:08:03  jonas
    * improved detection of range of instructions which use a register
      (merged from fixes branch)

  Revision 1.4  2000/07/21 15:19:54  jonas
    * daopt386: changes to getnextinstruction/getlastinstruction so they
      ignore labels who have is_addr set
    + daopt386/csopt386: remove loads of registers which are overwritten
       before their contents are used (especially usefull for removing superfluous
      maybe_loadesi outputs and push/pops transformed by below optimization
    + popt386: transform pop/pop/pop/.../push/push/push to sequences of
      'movl x(%esp),%reg' (only active when compiling a go32v2 compiler
      currently because I don't know whether it's safe to do this under Win32/
      Linux (because of problems we had when using esp as frame pointer on
      those os'es)

  Revision 1.3  2000/07/14 05:11:48  michael
  + Patch to 1.1

  Revision 1.2  2000/07/13 11:32:39  michael
  + removed logs

}
