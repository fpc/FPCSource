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

{$i defines.inc}

Interface

Uses aasm, cpuinfo, cpubase, cpuasm;

function CSE(asmL: TAAsmoutput; first, last: Tai; pass: longint): boolean;

function doReplaceReg(hp: Taicpu; newReg, orgReg: tregister): boolean;
function changeOp(var o: toper; newReg, orgReg: tregister): boolean;
function storeBack(p1: Tai; orgReg, newReg: tregister): boolean;
function NoHardCodedRegs(p: Taicpu; orgReg, newReg: TRegister): boolean;
function RegSizesOK(oldReg,newReg: TRegister; p: Taicpu): boolean;

Implementation

Uses
  {$ifdef replaceregdebug}cutils,{$endif}
  globtype, verbose, cgbase, globals, daopt386, rgobj, rropt386;

{
Function TaiInSequence(P: Tai; Const Seq: TContent): Boolean;
Var P1: Tai;
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
      p1 := Tai(p1.Next);
    End;
  TaiInSequence := TmpResult;
End;
}

function modifiesConflictingMemLocation(p1: Tai; reg: tregister; c: tregContent;
   var regsStillValid: tregset): boolean;
var
  p, hp: Taicpu;
  tmpRef: treference;
  regCounter: tregister;
  opCount: byte;
  dummy: boolean;
begin
  modifiesConflictingMemLocation := false;
  if p1.typ <> ait_instruction then
    exit;
  p := Taicpu(p1);
  case p.opcode of
    A_MOV,A_MOVSX,A_MOVZX:
      if p.oper[1].typ = top_ref then
        for regCounter := R_EAX to R_EDI do
          begin
            if writeToMemDestroysContents(reg32(p.oper[0].reg),p.oper[1].ref^,
                 regCounter,c[regCounter],dummy) then
              begin
                exclude(regsStillValid,regCounter);
                modifiesConflictingMemLocation := not(reg in regsStillValid);
              end;
          end
       else
{         if is_reg_var[reg32(p.oper[1].reg)] then }
          for regCounter := R_EAX to R_EDI do
            begin
              if writeDestroysContents(p.oper[1],regCounter,c[regCounter]) then
                begin
                  exclude(regsStillValid,regCounter);
                  modifiesConflictingMemLocation := not(reg in regsStillValid);
                end
            end;
    A_DIV, A_IDIV, A_MUL, A_IMUL:
      begin
        if (p.ops = 1) then
          begin
            if rg.is_reg_var[R_EDX] and
               (not getNextInstruction(p,hp) or
                not((hp.typ = ait_instruction) and
                    (hp.opcode = A_MOV) and
                    (hp.oper[0].typ = top_reg) and
                    (reg32(hp.oper[0].reg) = R_EDX) and
                    getNextInstruction(hp,hp) and
                    (hp.typ = ait_instruction) and
                    (hp.opcode = A_POP) and
                    (hp.oper[0].reg = R_EDX))) then
              for regCounter := R_EAX to R_EDI do
                if writeToRegDestroysContents(R_EDX,regCounter,c[regCounter]) then
                  begin
                    exclude(regsStillValid,R_EDX);
                    modifiesConflictingMemLocation := not(reg in regsStillValid);
                  end
          end
        else
          { only possible for imul }
          { last operand is always destination }
          if rg.is_reg_var[reg32(p.oper[p.ops-1].reg)] then
            for regCounter := R_EAX to R_EDI do
              begin
                if writeDestroysContents(p.oper[p.ops-1],regCounter,c[regCounter]) then
                  begin
                    exclude(regsStillValid,regCounter);
                    modifiesConflictingMemLocation := not(reg in regsStillValid);
                  end
              end
      end;
    else
      for opCount := 1 to MaxCh do
        case InsProp[p.opcode].Ch[opCount] of
          Ch_MOp1,CH_WOp1,CH_RWOp1:
{             if (p.oper[0].typ = top_ref) or }
{                ((p.oper[0].typ = top_reg) and }
{                 is_reg_var[reg32(p.oper[0].reg)]) then }
              for regCounter := R_EAX to R_EDI do
                if writeDestroysContents(p.oper[0],regCounter,c[regCounter]) then
                  begin
                    exclude(regsStillValid,regCounter);
                    modifiesConflictingMemLocation := not(reg in regsStillValid);
                  end;
          Ch_MOp2,CH_WOp2,CH_RWOp2:
{             if (p.oper[1].typ = top_ref) or }
{                ((p.oper[1].typ = top_reg) and }
{                 is_reg_var[reg32(p.oper[1].reg)]) then }
              for regCounter := R_EAX to R_EDI do
                if writeDestroysContents(p.oper[1],regCounter,c[regCounter]) then
                  begin
                    exclude(regsStillValid,regCounter);
                    modifiesConflictingMemLocation := not(reg in regsStillValid);
                  end;
          Ch_MOp3,CH_WOp3,CH_RWOp3:
{             if (p.oper[2].typ = top_ref) or }
{                ((p.oper[2].typ = top_reg) and }
{                 is_reg_var[reg32(p.oper[2].reg)]) then }
              for regCounter := R_EAX to R_EDI do
                if writeDestroysContents(p.oper[2],regCounter,c[regCounter]) then
                  begin
                    exclude(regsStillValid,regCounter);
                    modifiesConflictingMemLocation := not(reg in regsStillValid);
                  end;
          Ch_WMemEDI:
            begin
              fillchar(tmpref,sizeof(tmpref),0);
              tmpRef.base := R_EDI;
              tmpRef.index := R_EDI;
              for regCounter := R_EAX to R_EDI do
                if writeToMemDestroysContents(R_NO,tmpRef,regCounter,c[regCounter],dummy) then
                  begin
                    exclude(regsStillValid,regCounter);
                    modifiesConflictingMemLocation := not(reg in regsStillValid);
                 end;
            end;
        end;
  end;
end;


function isSimpleMemLoc(const ref: treference): boolean;
begin
  isSimpleMemLoc :=
    (ref.index = R_NO) and
    not(ref.base in (rg.usableregsint+[R_EDI]));
end;

{checks whether the current instruction sequence (starting with p) and the
 one between StartMod and EndMod of Reg are the same. If so, the number of
 instructions that match is stored in Found and true is returned, otherwise
 Found holds the number of instructions between StartMod and EndMod and false
 is returned}
Function CheckSequence(p: Tai; var prev: Tai; Reg: TRegister; Var Found: Longint;
           Var RegInfo: TRegInfo; findPrevSeqs: boolean): Boolean;

var
  regsNotRead, regsStillValid : tregset;
  checkingPrevSequences,
  passedFlagsModifyingInstr,
  passedJump                  : boolean;

  function getPrevSequence(p: Tai; reg: tregister; currentPrev: Tai; var newPrev: Tai): tregister;

  const
    current_reg: tregister = R_NO;

    function stillValid(p: Tai): boolean;
    begin
      stillValid :=
        (p.typ = ait_instruction) and
        (Taicpu(p).opcode <> a_jmp) and
        (pTaiprop(p.optinfo)^.regs[reg].wstate =
           pTaiprop(currentPrev.optinfo)^.regs[reg].wstate) and
       { in case destroyreg is called with doIncState = false }
        (pTaiprop(p.optinfo)^.regs[reg].typ =
           pTaiprop(currentPrev.optinfo)^.regs[reg].typ) and
        (reg in (regsNotRead * regsStillValid));
      passedJump :=
        (p.typ = ait_instruction) and
        (Taicpu(p).is_jmp);
      passedFlagsModifyingInstr :=
        instrWritesFlags(currentPrev);
    end;

    function findChangedRegister(p: Tai): tregister;
    var
      regCounter: tregister;
    begin
      for regCounter := succ(current_reg) to R_EDI do
        with pTaiprop(p.optinfo)^.regs[regCounter] do
        if ((startmod <>
              pTaiprop(currentPrev.optinfo)^.regs[regCounter].startmod)  or
            (nrOfMods <>
              pTaiprop(currentPrev.optinfo)^.regs[regCounter].nrOfMods)) and
           (pTaiprop(p.optinfo)^.regs[regCounter].typ in
             [con_ref,con_noRemoveRef]) then
          begin
            findChangedRegister := regCounter;
            current_reg := regCounter;
            exit;
          end;
      current_reg := R_NO;
      findChangedRegister := R_NO;
    end;

  var
    hp, prevFound: Tai;
    tmpResult, regCounter: tregister;
  begin
    if not(current_reg in [R_NO,R_EDI]) then
      begin
        tmpResult := findChangedRegister(currentPrev);
        if tmpResult <> R_NO then
          begin
            getPrevSequence := tmpResult;
            exit;
          end;
      end;

    getPrevSequence := R_NO;
    passedJump := passedJump or
      ((currentPrev.typ = ait_instruction) and
       (Taicpu(currentPrev).is_jmp));
    passedFlagsModifyingInstr := instrWritesFlags(currentPrev);

    if (passedJump and not(reg in (rg.usableregsint+[R_EDI]))) or
       not getLastInstruction(currentPrev,hp) then
      exit;

    prevFound := currentPrev;
    tmpResult := R_NO;

    while (tmpResult = R_NO) and
          stillValid(hp) and
          (pTaiprop(prevFound.optinfo)^.canBeRemoved or
           not(modifiesConflictingMemLocation(prevFound,reg,
             pTaiprop(p.optinfo)^.regs,regsStillValid))) do
      begin
        { only update the regsread for the instructions we already passed }
        if not(pTaiprop(prevFound.optinfo)^.canBeRemoved) then
          for regCounter := R_EAX to R_EDI do
            if regReadByInstruction(regCounter,prevFound) then
              exclude(regsNotRead,regCounter);

        { in case getPreviousInstruction fails and sets hp to nil in the }
        { next iteration                                                 }
        prevFound := hp;
        if not(pTaiprop(hp.optinfo)^.canBeRemoved) then
          tmpResult := findChangedRegister(hp);
        if { do not load the self pointer or a regvar before a (conditional)  }
           { jump with a new value, since if the jump is taken, the old value }
           { is (probably) still necessary                                    }
           (passedJump and not(reg in (rg.usableregsint+[R_EDI]))) or
           not getLastInstruction(hp,hp) then
          break;
      end;
    getPrevSequence := tmpResult;
    if tmpResult <> R_NO then
      newPrev := prevFound;
  end;


  function getNextRegToTest(var prev: Tai; currentReg: tregister): tregister;
  begin
    if not checkingPrevSequences then
      begin
        Repeat
          Inc(currentReg);
        Until (currentReg > R_EDI) or
              (pTaiprop(prev.optInfo)^.regs[currentReg].typ
                 in [con_ref,con_noRemoveRef]);
        if currentReg > R_EDI then
          begin
            if (Taicpu(p).oper[0].typ <> top_ref) or
               isSimpleMemLoc(Taicpu(p).oper[0].ref^) then
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
      if findPrevSeqs then
        getNextRegToTest :=
          getPrevSequence(p,reg,prev,prev)
      else
        getNextRegToTest := R_NO;
  end;

Var hp2, hp3{, EndMod},highPrev, orgPrev: Tai;
    {Cnt,} OldNrOfMods: Longint;
    startRegInfo, OrgRegInfo, HighRegInfo: TRegInfo;
    regModified: array[R_NO..R_EDI] of boolean;
    HighFound, OrgRegFound: Byte;
    RegCounter, regCounter2, tmpreg, base, index: TRegister;
    OrgRegResult: Boolean;
    TmpResult, flagResultsNeeded: Boolean;
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

  checkingPrevSequences := false;
  passedFlagsModifyingInstr := false;
  flagResultsNeeded := false;
  regsNotRead := [R_EAX,R_EBX,R_ECX,R_EDX,R_ESP,R_EBP,R_EDI,R_ESI];
  regsStillValid := regsNotRead;
  GetLastInstruction(p, prev);
  regCounter := getNextRegToTest(prev,R_NO);
  While (RegCounter <> R_NO) Do
    Begin
      fillchar(regModified,sizeof(regModified),0);
      regInfo := startRegInfo;
      Found := 0;
      hp2 := PTaiProp(prev.OptInfo)^.Regs[RegCounter].StartMod;
      If (prev <> PTaiProp(prev.OptInfo)^.Regs[RegCounter].StartMod)
        Then OldNrOfMods := PTaiProp(prev.OptInfo)^.Regs[RegCounter].NrOfMods
        Else OldNrOfMods := 1;
      hp3 := p;
      While (Found <> OldNrOfMods) And
                                  { old  new }
             InstructionsEquivalent(hp2, hp3, RegInfo) Do
        Begin
            if not checkingPrevSequences and
               (hp3.typ = ait_instruction) and
               ((Taicpu(hp3).opcode = A_MOV) or
                (Taicpu(hp3).opcode = A_MOVZX) or
                (Taicpu(hp3).opcode = A_LEA) or
                (Taicpu(hp3).opcode = A_MOVSX)) and
               (Taicpu(hp3).oper[1].typ = top_reg) and
               not(regInOp(Taicpu(hp3).oper[1].reg,
                     Taicpu(hp3).oper[0])) then
            begin
              tmpreg := reg32(Taicpu(hp3).oper[1].reg);
              regInfo.lastReload[tmpreg] := hp3;
              case Taicpu(hp3).oper[0].typ of
                top_ref:
                  begin
                    base := reg32(Taicpu(hp3).oper[0].ref^.base);
                    index := reg32(Taicpu(hp3).oper[0].ref^.index);
                    if (found <> 0) and
                       ((base = R_NO) or
                        regModified[base] or
                        (base = procinfo^.framepointer) or
                        (assigned(procinfo^._class) and (base = R_ESI))) and
                       ((index = R_NO) or
                        regModified[index] or
                        (assigned(procinfo^._class) and (index = R_ESI))) and
                        not(regInRef(tmpReg,Taicpu(hp3).oper[0].ref^)) then
                      with pTaiprop(hp3.optinfo)^.regs[tmpreg] do
                        if nrOfMods > (oldNrOfMods - found) then
                          oldNrOfMods := found + nrOfMods;
                  end;
                top_reg:
                  if regModified[reg32(Taicpu(hp3).oper[0].reg)] then
                    with pTaiprop(hp3.optinfo)^.regs[tmpreg] do
                      if nrOfMods > (oldNrOfMods - found) then
                        oldNrOfMods := found + nrOfMods;
              end;
            end;
          for regCounter2 := R_EAX to R_EDI do
            regModified[regCounter2] := regModified[regCounter2] or
              regModifiedByInstruction(regCounter2,hp3);
          if flagResultsNeeded then
            flagResultsNeeded := not instrReadsFlags(hp3);
          if not flagResultsNeeded then
            flagResultsNeeded := pTaiprop(hp3.optinfo)^.FlagsUsed;
          GetNextInstruction(hp2, hp2);
          GetNextInstruction(hp3, hp3);
          Inc(Found);
        End;

      for regCounter2 := R_EAX to R_EDI do
        if (regInfo.new2OldReg[regCounter2] <> R_NO) and
           (regCounter2 in PTaiProp(hp3.optInfo)^.usedRegs) and
           not regLoadedWithNewValue(regCounter2,false,hp3) then
          include(regInfo.regsStillUsedAfterSeq,regCounter2);

      if checkingPrevSequences then
        begin
          for regCounter2 := R_EAX to R_EDI do
            if not(regInfo.new2OldReg[regCounter2] in [R_NO,regCounter2]) and
               (not(regCounter2 in (regsNotRead * regsStillValid)) or
               not(regInfo.new2OldReg[regCounter2] in regsStillValid)) then
              begin
                found := 0;
                break;
              end;
           if passedFlagsModifyingInstr and flagResultsNeeded then
              found := 0;
        end;
      If (Found <> OldNrOfMods) or
 { the following is to avoid problems with rangecheck code (see testcse2) }
         (assigned(hp3) and
          ((reg in regInfo.regsLoadedForRef) and
           (reg in PTaiProp(hp3.optInfo)^.usedRegs) and
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
            Found := PTaiProp(Tai(p).OptInfo)^.Regs[Reg].NrOfMods
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

Procedure SetAlignReg(p: Tai);
Const alignSearch = 12;
var regsUsable: TRegSet;
    prevInstrCount, nextInstrCount: Longint;
    prevState, nextWState,nextRState: Array[R_EAX..R_EDI] of byte;
    regCounter, lastRemoved: TRegister;
    prev, next: Tai;
{$ifdef alignregdebug}
    temp: Tai;
{$endif alignregdebug}
begin
  regsUsable := [R_EAX,R_ECX,R_EDX,R_EBX,{R_ESP,R_EBP,}R_ESI,R_EDI];
  for regCounter := R_EAX to R_EDI do
    begin
      prevState[regCounter] := PTaiProp(p.optInfo)^.Regs[regCounter].wState;
      nextWState[regCounter] := PTaiProp(p.optInfo)^.Regs[regCounter].wState;
      nextRState[regCounter] := PTaiProp(p.optInfo)^.Regs[regCounter].rState;
    end;
  getLastInstruction(p,prev);
  getNextInstruction(p,next);
  lastRemoved := Tai_align(p).reg;
  nextInstrCount := 0;
  prevInstrCount := 0;
  while ((assigned(prev) and
          assigned(prev.optInfo) and
          (prevInstrCount < alignSearch)) or
         (assigned(next) and
          assigned(next.optInfo) and
          (nextInstrCount < alignSearch))) And
        (regsUsable <> []) do
    begin
{$ifdef alignregdebug}
      if assigned(prev) then
        begin
          temp := Tai_asm_comment.Create(strpnew('got here'));
          temp.next := prev.next;
          temp.previous := prev;
          prev.next := temp;
          if assigned(temp.next) then
            temp.next.previous := temp;
        end;
{$endif alignregdebug}
      if assigned(prev) and assigned(prev.optinfo) and
         (prevInstrCount < alignSearch) then
        begin
          if (prev.typ = ait_instruction) And
             (insProp[TaiCpu(prev).opcode].ch[1] <> Ch_ALL) and
             (TaiCpu(prev).opcode <> A_JMP) then
            begin
              inc(prevInstrCount);
              for regCounter := R_EAX to R_EDI do
                begin
                  if (regCounter in regsUsable) And
                     (PTaiProp(prev.optInfo)^.Regs[regCounter].wState <>
                       prevState[regCounter]) then
                    begin
                      lastRemoved := regCounter;
                      exclude(regsUsable,regCounter);
{$ifdef alignregdebug}
                      temp := Tai_asm_comment.Create(strpnew(
                                att_reg2str[regCounter]+' removed')));
                      temp.next := prev.next;
                      temp.previous := prev;
                      prev.next := temp;
                      if assigned(temp.next) then
                        temp.next.previous := temp;
                      if regsUsable = [] then
                        begin
                          temp := Tai_asm_comment.Create(strpnew(
                                    'regsUsable empty here')));
                          temp.next := prev.next;
                          temp.previous := prev;
                          prev.next := temp;
                          if assigned(temp.next) then
                            temp.next.previous := temp;
                        end;
{$endif alignregdebug}
                    end;
                  prevState[regCounter] :=
                    PTaiProp(prev.optInfo)^.Regs[regCounter].wState;
                end;
              getLastInstruction(prev,prev);
            end
          else
            If GetLastInstruction(prev,prev) and
               assigned(prev.optinfo) then
              for regCounter := R_EAX to R_EDI do
                prevState[regCounter] :=
                  PTaiProp(prev.optInfo)^.Regs[regCounter].wState
        end;
      if assigned(next) and assigned(next.optInfo) and
         (nextInstrCount < alignSearch) then
        begin
          if (next.typ = ait_instruction) and
             (insProp[TaiCpu(next).opcode].ch[1] <> Ch_ALL) and
             (TaiCpu(next).opcode <> A_JMP) then
            begin
              inc(nextInstrCount);
              for regCounter := R_EAX to R_EDI do
                begin
                  if (regCounter in regsUsable) And
                     ((PTaiProp(next.optInfo)^.Regs[regCounter].wState <>
                       nextWState[regCounter]) or
                      (PTaiProp(next.optInfo)^.Regs[regCounter].rState <>
                       nextRState[regCounter])) Then
                    begin
                      lastRemoved := regCounter;
                      exclude(regsUsable,regCounter);
{$ifdef alignregdebug}
                      temp := Tai_asm_comment.Create(strpnew(
                                att_reg2str[regCounter]+' removed')));
                      temp.next := next.next;
                      temp.previous := next;
                      next.next := temp;
                      if assigned(temp.next) then
                        temp.next.previous := temp;
                      if regsUsable = [] then
                        begin
                          temp := Tai_asm_comment.Create(strpnew(
                                    'regsUsable empty here')));
                          temp.next := next.next;
                          temp.previous := next;
                          next.next := temp;
                          if assigned(temp.next) then
                            temp.next.previous := temp;
                        end;
{$endif alignregdebug}
                    end;
                  nextWState[regCounter] :=
                    PTaiProp(next.optInfo)^.Regs[regCounter].wState;
                  nextRState[regCounter] :=
                    PTaiProp(next.optInfo)^.Regs[regCounter].rState;
                end
            end
          else
            for regCounter := R_EAX to R_EDI do
              begin
                nextWState[regCounter] :=
                  PTaiProp(next.optInfo)^.Regs[regCounter].wState;
                nextRState[regCounter] :=
                  PTaiProp(next.optInfo)^.Regs[regCounter].rState;
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
  next := Tai_asm_comment.Create(strpnew(att_reg2str[lastRemoved]+
               ' chosen as alignment register')));
  next.next := p.next;
  next.previous := p;
  p.next := next;
  if assigned(next.next) then
    next.next.previous := next;
{$endif alignregdebug}
  Tai_align(p).reg := lastRemoved;
End;

procedure clearmemwrites(p: tai; reg: tregister);
var
  beginmemwrite: tai;
begin
  beginmemwrite := pTaiprop(p.optinfo)^.regs[reg].memwrite;
  repeat
    pTaiprop(p.optinfo)^.regs[reg].memwrite := nil;
  until not getnextinstruction(p,p) or
        (pTaiprop(p.optinfo)^.regs[reg].memwrite <> beginmemwrite);
end;

Procedure ClearRegContentsFrom(reg: TRegister; p, endP: Tai);
{ first clears the contents of reg from p till endP. Then the contents are }
{ cleared until the first instruction that changes reg                     }
var
{$ifdef replaceregdebug}
    hp: Tai;
    l: longint;
{$endif replaceregdebug}
    regcounter: tregister;
    oldStartmod: Tai;
begin
{$ifdef replaceregdebug}
  l := random(1000);
  hp := Tai_asm_comment.Create(strpnew(
          'cleared '+att_reg2str[reg]+' from here... '+tostr(l))));
  hp.next := p;
  hp.previous := p.previous;
  p.previous := hp;
  if assigned(hp.previous) then
    hp.previous^.next := hp;
{$endif replaceregdebug}
  PTaiProp(p.optInfo)^.Regs[reg].typ := con_unknown;
  While (p <> endP) Do
    Begin
      for regcounter := R_EAX to R_EDI do
        if (regcounter <> reg) and
           assigned(pTaiprop(p.optinfo)^.regs[reg].memwrite) and
           reginref(regcounter,pTaiprop(p.optinfo)^.regs[reg].memwrite.oper[1].ref^) then
          clearmemwrites(p,regcounter);
      with PTaiProp(p.optInfo)^.Regs[reg] do
        begin
          typ := con_unknown;
          memwrite := nil;
        end;
      getNextInstruction(p,p);
    end;
  oldStartmod := PTaiProp(p.optInfo)^.Regs[reg].startmod;
  repeat
    with PTaiProp(p.optInfo)^.Regs[reg] do
      begin
        typ := con_unknown;
        memwrite := nil;
      end;
  until not getNextInstruction(p,p) or
        (PTaiProp(p.optInfo)^.Regs[reg].startmod <> oldStartmod);
{$ifdef replaceregdebug}
  if assigned(p) then
    begin
      hp := Tai_asm_comment.Create(strpnew(
        'cleared '+att_reg2str[reg]+' till here... '+tostr(l))));
      hp.next := p;
      hp.previous := p.previous;
      p.previous := hp;
      if assigned(hp.previous) then
        hp.previous^.next := hp;
    end;
{$endif replaceregdebug}
end;

Procedure RestoreRegContentsTo(reg: TRegister; const c: TContent; p, endP: Tai);
var
{$ifdef replaceregdebug}
    hp: Tai;
    l: longint;
{$endif replaceregdebug}
    tmpState: byte;
begin
{$ifdef replaceregdebug}
  l := random(1000);
  hp := Tai_asm_comment.Create(strpnew(
          'restored '+att_reg2str[reg]+' with data from here... '+tostr(l))));
  hp.next := p;
  hp.previous := p.previous;
  p.previous := hp;
  if assigned(hp.previous) then
    hp.previous^.next := hp;
{$endif replaceregdebug}
{  PTaiProp(p.optInfo)^.Regs[reg] := c;}
  While (p <> endP) Do
    Begin
      PTaiProp(p.optInfo)^.Regs[reg] := c;
      getNextInstruction(p,p);
    end;
  tmpState := PTaiProp(p.optInfo)^.Regs[reg].wState;
  repeat
    PTaiProp(p.optInfo)^.Regs[reg] := c;
  until not getNextInstruction(p,p) or
        (PTaiProp(p.optInfo)^.Regs[reg].wState <> tmpState) or
        (p.typ = ait_label);
  if p.typ = ait_label then
    clearRegContentsFrom(reg,p,p);
{$ifdef replaceregdebug}
  if assigned(p) then
    begin
      hp := Tai_asm_comment.Create(strpnew(
        'restored '+att_reg2str[reg]+' till here... '+tostr(l))));
      hp.next := p;
      hp.previous := p.previous;
      p.previous := hp;
      if assigned(hp.previous) then
        hp.previous^.next := hp;
    end;
{$endif replaceregdebug}
end;

function NoHardCodedRegs(p: Taicpu; orgReg, newReg: TRegister): boolean;
var chCount: byte;
begin
  case p.opcode of
    A_IMUL: noHardCodedRegs := p.ops <> 1;
    A_SHL,A_SHR,A_SHLD,A_SHRD: noHardCodedRegs :=
      (p.oper[0].typ <> top_reg) or
      ((orgReg <> R_ECX) and (newReg <> R_ECX));
    else
      begin
        NoHardCodedRegs := true;
        with InsProp[p.opcode] do
          for chCount := 1 to MaxCh do
            if Ch[chCount] in ([Ch_REAX..Ch_MEDI,Ch_WMemEDI,Ch_All]-[Ch_RESP,Ch_WESP,Ch_RWESP]) then
              begin
                NoHardCodedRegs := false;
                break
              end;
      end;
  end;
end;

function ChangeReg(var Reg: TRegister; newReg, orgReg: TRegister): boolean;
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

function changeOp(var o: toper; newReg, orgReg: tregister): boolean;
var
  tmpresult: boolean;
begin
  changeOp := false;
  case o.typ of
    top_reg: changeOp := changeReg(o.reg,newReg,orgReg);
    top_ref:
      begin
        tmpresult := changeReg(o.ref^.base,newReg,orgReg);
        changeop := changeReg(o.ref^.index,newReg,orgReg) or tmpresult;
      end;
  end;
end;

procedure updateStates(orgReg,newReg: tregister; hp: Tai; writeStateToo: boolean);
var
  prev: Tai;
  newOrgRegRState, newOrgRegWState: byte;
begin
  if getLastInstruction(hp,prev) then
    with pTaiprop(prev.optinfo)^ do
      begin
{$ifopt r+}
{$define rangeon}
{$r-}
{$endif}
        newOrgRegRState := regs[orgReg].rState +
          pTaiprop(hp.optinfo)^.regs[newReg].rState - regs[newReg].rstate;
        if writeStateToo then
          newOrgRegWState := regs[orgReg].wState +
            pTaiprop(hp.optinfo)^.regs[newReg].wState - regs[newReg].wstate;
{$ifdef rangeon}
{$undef rangeon}
{$r+}
{$endif}
      end
  else
    with pTaiprop(hp.optinfo)^.regs[newReg] do
      begin
        newOrgRegRState := rState;
        if writeStateToo then
          newOrgRegWState := wState;
      end;
  with pTaiprop(hp.optinfo)^.regs[orgReg] do
    begin
      rState := newOrgRegRState;
      if writeStateToo then
        wState := newOrgRegwState;
    end;
end;

function doReplaceReg(hp: Taicpu; newReg, orgReg: tregister): boolean;
var
  opCount: longint;
  tmpResult: boolean;
begin
  for opCount := 0 to hp.ops-1 do
    tmpResult :=
      changeOp(hp.oper[opCount],newReg,orgReg) or tmpResult;
  doReplaceReg := tmpResult;
end;

function RegSizesOK(oldReg,newReg: TRegister; p: Taicpu): boolean;
{ oldreg and newreg must be 32bit components }
var opCount: byte;
begin
  RegSizesOK := true;
  { if only one of them is a general purpose register ... }
  if (IsGP32reg(oldReg) xor IsGP32Reg(newReg)) then
    begin
      for opCount := 0 to 2 do
        if (p.oper[opCount].typ = top_reg) and
           (p.oper[opCount].reg in [R_AL..R_DH]) then
          begin
            RegSizesOK := false;
            break
          end
    end;
end;

function doReplaceReadReg(p: Taicpu; newReg,orgReg: tregister): boolean;
var opCount: byte;
begin
  doReplaceReadReg := false;
  { handle special case }
  case p.opcode of
    A_IMUL:
      begin
        case p.ops of
          1: internalerror(1301001);
          2,3:
            begin
              if changeOp(p.oper[0],newReg,orgReg) then
                begin
{                  updateStates(orgReg,newReg,p,false);}
                  doReplaceReadReg := true;
                end;
             if p.ops = 3 then
                if changeOp(p.oper[1],newReg,orgReg) then
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
          if p.oper[opCount].typ = top_ref then
            if changeOp(p.oper[opCount],newReg,orgReg) then
              begin
{                updateStates(orgReg,newReg,p,false);}
                doReplaceReadReg := true;
              end;
        for opCount := 1 to MaxCh do
          case InsProp[p.opcode].Ch[opCount] of
            Ch_ROp1:
              if p.oper[0].typ = top_reg then
                if changeReg(p.oper[0].reg,newReg,orgReg) then
                  begin
{                    updateStates(orgReg,newReg,p,false);}
                    doReplaceReadReg := true;
                  end;
            Ch_ROp2:
              if p.oper[1].typ = top_reg then
                if changeReg(p.oper[1].reg,newReg,orgReg) then
                  begin
{                    updateStates(orgReg,newReg,p,false);}
                    doReplaceReadReg := true;
                  end;
            Ch_ROp3:
              if p.oper[2].typ = top_reg then
                if changeReg(p.oper[2].reg,newReg,orgReg) then
                  begin
{                    updateStates(orgReg,newReg,p,false);}
                    doReplaceReadReg := true;
                  end;
          end;
      end;
  end;
end;


procedure updateState(reg: tregister; p: Tai);
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
  with pTaiprop(p.optinfo)^.regs[reg] do
    begin
      newRState := rState;
      newWState := wState;
    end;
  if not GetNextInstruction(p,p) then
    exit;
  { get the old read/write states from the next instruction, to know }
  { when we can stop updating                                        }
  with pTaiprop(p.optinfo)^.regs[reg] do
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
    with pTaiprop(p.optinfo)^.regs[reg] do
      begin
        if doRState then
          rState := newRState;
        if doWState then
          wState := newWState;
      end;
    if not getNextInstruction(p,p) then
      break;
    with pTaiprop(p.optinfo)^.regs[reg] do
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


function storeBack(p1: Tai; orgReg, newReg: tregister): boolean;
{ returns true if p1 contains an instruction that stores the contents }
{ of newReg back to orgReg                                            }
begin
  storeBack :=
    (p1.typ = ait_instruction) and
    (Taicpu(p1).opcode = A_MOV) and
    (Taicpu(p1).oper[0].typ = top_reg) and
    (Taicpu(p1).oper[0].reg = newReg) and
    (Taicpu(p1).oper[1].typ = top_reg) and
    (Taicpu(p1).oper[1].reg = orgReg);
end;


function ReplaceReg(asmL: TAAsmOutput; orgReg, newReg: TRegister; p: Tai;
           const c: TContent; orgRegCanBeModified: Boolean;
           var returnEndP: Tai): Boolean;
{ Tries to replace orgreg with newreg in all instructions coming after p }
{ until orgreg gets loaded with a new value. Returns true if successful, }
{ false otherwise. If successful, the contents of newReg are set to c,   }
{ which should hold the contents of newReg before the current sequence   }
{ started                                                                }
{ if the function returns true, returnEndP holds the last instruction    }
{ where newReg was replaced by orgReg                                    }
var endP, hp: Tai;
    removeLast, sequenceEnd, tmpResult, newRegModified, orgRegRead,
      stateChanged, readStateChanged: Boolean;


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
        (endp.typ = ait_instruction) and
        not(Taicpu(endp).is_jmp);
      if tmpresult and not assigned(endp.optInfo) then
        begin
{          hp := Tai_asm_comment.Create(strpnew('next no optinfo'));
          hp.next := endp;
          hp.previous := endp.previous;
          endp.previous := hp;
          if assigned(hp.previous) then
            hp.previous^.next := hp;}
          exit;
        end;
      If tmpResult and
         { don't take into account instructions that will be removed }
         Not (PTaiProp(endp.optInfo)^.canBeRemoved) then
        begin
          { if the newReg gets stored back to the oldReg, we can change }
          { "mov %oldReg,%newReg; <operations on %newReg>; mov %newReg, }
          { %oldReg" to "<operations on %oldReg>"                       }
          removeLast := storeBack(endP, orgReg, newReg);
          sequenceEnd :=
            { no support for (i)div, mul and imul with hardcoded operands }
            (noHardCodedRegs(Taicpu(endP),orgReg,newReg) and
            { if newReg gets loaded with a new value, we can stop   }
            { replacing newReg with oldReg here (possibly keeping   }
            { the original contents of oldReg so we still know them }
            { afterwards)                                           }
             RegLoadedWithNewValue(newReg,true,Taicpu(endP)) or
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
            (not(regLoadedWithNewValue(newReg,true,Taicpu(endP))) and
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
(*          already check at the top
            (endp.typ = ait_instruction) and  *)
            NoHardCodedRegs(Taicpu(endP),orgReg,newReg) and
            RegSizesOk(orgReg,newReg,Taicpu(endP)) and
            not RegModifiedByInstruction(orgReg,endP);
        end;
    end;
  sequenceEnd := sequenceEnd and
     (removeLast  or
      (orgRegCanBeModified or not(newRegModified))) and
     (not(assigned(endp)) or
      not(endp.typ = ait_instruction) or
      (noHardCodedRegs(Taicpu(endP),orgReg,newReg) and
       RegSizesOk(orgReg,newReg,Taicpu(endP)) and
       not(newRegModified and
           (orgReg in PTaiProp(endp.optInfo)^.usedRegs) and
           not(RegLoadedWithNewValue(orgReg,true,Taicpu(endP))))));
  if SequenceEnd then
    begin
{$ifdef replaceregdebug}
      hp := Tai_asm_comment.Create(strpnew(
        'replacing '+att_reg2str[newreg]+' with '+att_reg2str[orgreg]+
        ' from here...')));
      hp.next := p;
      hp.previous := p.previous;
      p.previous := hp;
      if assigned(hp.previous) then
        hp.previous^.next := hp;

      hp := Tai_asm_comment.Create(strpnew(
        'replaced '+att_reg2str[newreg]+' with '+att_reg2str[orgreg]+
        ' till here')));
      hp.next := endp.next;
      hp.previous := endp;
      endp.next := hp;
      if assigned(hp.next) then
        hp.next.previous := hp;
{$endif replaceregdebug}
      replaceReg := true;
      returnEndP := endP;

      getNextInstruction(p,hp);
      stateChanged := false;
      while hp <> endP do
        begin
          if {not(PTaiProp(hp.optInfo)^.canBeRemoved) and }
             (hp.typ = ait_instruction) then
            stateChanged :=
              doReplaceReg(Taicpu(hp),newReg,orgReg) or stateChanged;
            if stateChanged then
              updateStates(orgReg,newReg,hp,true);
          getNextInstruction(hp,hp)
        end;
      if assigned(endp) and (endp.typ = ait_instruction) then
        readStateChanged :=
          DoReplaceReadReg(Taicpu(endP),newReg,orgReg);
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
        pTaiprop(endp.optinfo)^.canBeRemoved := true;
      allocRegBetween(asml,orgReg,p,endP);

    end
{$ifdef replaceregdebug}
     else
       begin
         hp := Tai_asm_comment.Create(strpnew(
           'replacing '+att_reg2str[newreg]+' with '+att_reg2str[orgreg]+
           ' from here...')));
         hp.previous := p.previous;
         hp.next := p;
         p.previous := hp;
        if assigned(hp.previous) then
          hp.previous^.next := hp;

      hp := Tai_asm_comment.Create(strpnew(
        'replacing '+att_reg2str[newreg]+' with '+att_reg2str[orgreg]+
        ' failed here')));
      hp.next := endp.next;
      hp.previous := endp;
      endp.next := hp;
      if assigned(hp.next) then
        hp.next.previous := hp;
       end;
{$endif replaceregdebug}
End;

Function FindRegWithConst(p: Tai; size: topsize; l: aword; Var Res: TRegister): Boolean;
{Finds a register which contains the constant l}
Var Counter: TRegister;
{$ifdef testing}
    hp: Tai;
{$endif testing}
    tmpresult: boolean;
Begin
  Counter := R_NO;
  repeat
     inc(counter);
     tmpresult := (pTaiprop(p.optInfo)^.regs[counter].typ in
         [con_const,con_noRemoveConst]) and
       (Taicpu(PTaiProp(p.OptInfo)^.Regs[Counter].StartMod).opsize = size) and
       (Taicpu(PTaiProp(p.OptInfo)^.Regs[Counter].StartMod).oper[0].typ = top_const) and
       (Taicpu(PTaiProp(p.OptInfo)^.Regs[Counter].StartMod).oper[0].val = l);
{$ifdef testing}
     if (pTaiprop(p.optInfo)^.regs[counter].typ in [con_const,con_noRemoveConst]) then
       begin
         hp := Tai_asm_comment.Create(strpnew(
           'checking const load of '+tostr(l)+' here...')));
         hp.next := PTaiProp(p.OptInfo)^.Regs[Counter].StartMod;
         hp.previous := PTaiProp(p.OptInfo)^.Regs[Counter].StartMod^.previous;
         PTaiProp(p.OptInfo)^.Regs[Counter].StartMod^.previous := hp;
         if assigned(hp.previous) then
           hp.previous^.next := hp;
       end;
{$endif testing}
  until tmpresult or (Counter = R_EDI);
  if tmpResult then
    res := Taicpu(PTaiProp(p.OptInfo)^.Regs[Counter].StartMod).oper[1].reg;
  FindRegWithConst := tmpResult;
End;

procedure removePrevNotUsedLoad(p: Tai; reg: tRegister; check: boolean);
{ If check = true, it means the procedure has to check whether it isn't  }
{ possible that the contents are still used after p (used when removing  }
{ instructions because of a "call"), otherwise this is not necessary     }
{ (e.g. when you have a "mov 8(%ebp),%eax", you can be sure the previous }
{ value of %eax isn't used anymore later on)                             }
var
  hp1: Tai;
begin
  if getLastInstruction(p,hp1) then
    with pTaiprop(hp1.optInfo)^.regs[reg] do
      if (typ in [con_ref,con_invalid,con_const]) and
         (nrOfMods = 1) and
         (rState = pTaiprop(startmod.optInfo)^.regs[reg].rState) and
         (not(check) or
          (not(regInInstruction(reg,p)) and
           (not(reg in rg.usableregsint) and
            (startmod.typ = ait_instruction) and
            ((Taicpu(startmod).opcode = A_MOV) or
             (Taicpu(startmod).opcode = A_MOVZX) or
             (Taicpu(startmod).opcode = A_MOVSX) or
             (Taicpu(startmod).opcode = A_LEA)) and
            (Taicpu(startmod).oper[0].typ = top_ref) and
            (Taicpu(startmod).oper[0].ref^.base = stack_pointer)) or
           not(reg in pTaiprop(hp1.optInfo)^.usedRegs) or
           findRegDealloc(reg,p))) then
        pTaiprop(startMod.optInfo)^.canBeRemoved := true;
end;

function is_mov_for_div(p: Taicpu): boolean;
begin
  is_mov_for_div :=
    (p.opcode = A_MOV) and
    (p.oper[0].typ = top_const) and
    (p.oper[1].typ = top_reg) and
    (p.oper[1].reg = R_EDX) and
    getNextInstruction(p,p) and
    (p.typ = ait_instruction) and
    ((p.opcode = A_DIV) or
     (p.opcode = A_IDIV));
end;

function memtoreg(const t: Taicpu; const ref: treference; var startp: tai): tregister;
var
  hp: tai;
  p: pTaiprop;
  regcounter: tregister;
  optimizable: boolean;
begin
  if not getlastinstruction(t,hp) or
     not issimplememloc(ref) then
    begin
      memtoreg := R_NO;
      exit;
    end;
  p := pTaiprop(hp.optinfo);
  optimizable := false;
  for regcounter := R_EAX to R_EDI do
    begin
      if (assigned(p^.regs[regcounter].memwrite) and
         refsequal(ref,p^.regs[regcounter].memwrite.oper[1].ref^)) then
        begin
          optimizable := true;
          hp := p^.regs[regcounter].memwrite;
        end
      else if ((p^.regs[regcounter].typ in [CON_REF,CON_NOREMOVEREF]) and
             (p^.regs[regcounter].nrofmods = 1) and
             ((Taicpu(p^.regs[regcounter].startmod).opcode = A_MOV) or
              (Taicpu(p^.regs[regcounter].startmod).opcode = A_MOVZX) or
              (Taicpu(p^.regs[regcounter].startmod).opcode = A_MOVSX)) and
             (taicpu(p^.regs[regcounter].startmod).oper[0].typ = top_ref) and
             refsequal(ref,taicpu(p^.regs[regcounter].startmod).oper[0].ref^)) then
        begin
          optimizable := true;
          hp := p^.regs[regcounter].startmod;
        end;
      if optimizable then
          if ((t.opsize <> S_B) or
              (regcounter <> R_EDI)) and
             sizescompatible(Taicpu(hp).opsize,t.opsize) then
            begin
              case t.opsize of
                S_B,S_BW,S_BL:
                  memtoreg := reg32toreg8(regcounter);
                S_W,S_WL:
                  memtoreg := reg32toreg16(regcounter);
                S_L:
                  memtoreg := regcounter;
              end;
              startp := hp;
              exit;
            end;
        end;
  memtoreg := R_NO;
end;

procedure removeLocalStores(const t1: tai);
{var
  p: tai;
  regcount: tregister; }
begin
{
  for regcount := LoGPReg to HiGPReg do
    if assigned(pTaiProp(t1.optinfo)^.regs[regcount].memwrite) and
       (taicpu(pTaiProp(t1.optinfo)^.regs[regcount].memwrite).oper[1].ref^.base
         = procinfo^.framepointer) then
      begin
        pTaiProp(pTaiProp(t1.optinfo)^.regs[regcount].memwrite.optinfo)^.canberemoved := true;
        clearmemwrites(pTaiProp(t1.optinfo)^.regs[regcount].memwrite,regcount);
      end;
}
end;

procedure DoCSE(AsmL: TAAsmOutput; First, Last: Tai; findPrevSeqs, doSubOpts: boolean);
{marks the instructions that can be removed by RemoveInstructs. They're not
 removed immediately because sometimes an instruction needs to be checked in
 two different sequences}
var cnt, cnt2, {cnt3,} orgNrOfMods: longint;
    p, hp1, hp2, prevSeq, prevSeq_next: Tai;
    hp3, hp4: Tai;
    hp5 : Tai;
    RegInfo: TRegInfo;
    RegCounter: TRegister;
Begin
  p := First;
  SkipHead(p);
  While (p <> Last) Do
    Begin
      Case p.typ Of
        ait_align:
          if not(Tai_align(p).use_op) then
            SetAlignReg(p);
        ait_instruction:
          Begin
            Case Taicpu(p).opcode Of
              A_CALL:
                for regCounter := R_EAX to R_EBX do
                  removePrevNotUsedLoad(p,regCounter,true);
              A_CLD: If GetLastInstruction(p, hp1) And
                        (PTaiProp(hp1.OptInfo)^.DirFlag = F_NotSet) Then
                       PTaiProp(Tai(p).OptInfo)^.CanBeRemoved := True;
              A_LEA, A_MOV, A_MOVZX, A_MOVSX:
                Begin
                  hp2 := p;
                  Case Taicpu(p).oper[0].typ Of
                    top_ref, top_reg:
                     if (Taicpu(p).oper[1].typ = top_reg) then
                       Begin
                        With PTaiProp(p.OptInfo)^.Regs[Reg32(Taicpu(p).oper[1].reg)] Do
                          Begin
                            if (startmod = p) then
                              orgNrOfMods := nrOfMods
                            else
                              orgNrOfMods := 0;
                            If (p = StartMod) And
                               GetLastInstruction (p, hp1) And
                               not(hp1.typ in [ait_marker,ait_label]) then
{so we don't try to check a sequence when p is the first instruction of the block}
                              begin
{$ifdef csdebug}
                               hp5 := Tai_asm_comment.Create(strpnew(
                                 'cse checking '+att_reg2str[Reg32(Taicpu(p).oper[1].reg)])));
                               insertLLItem(asml,p,p.next,hp5);
{$endif csdebug}
                               If CheckSequence(p,prevSeq,Taicpu(p).oper[1].reg, Cnt, RegInfo, findPrevSeqs) And
                                  (Cnt > 0) Then
                                 Begin
(*
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
*)

{ not anymore: if the start of a new sequence is found while checking (e.g. }
{ above that of eax while checking edx, this new sequence is automatically  }
{ also checked                                                              }
                                   Cnt2 := 1;
                                   While Cnt2 <= Cnt Do
                                     Begin
(*
                                       If not(regInInstruction(Taicpu(hp2).oper[1].reg, p)) and
                                          not(pTaiprop(p.optinfo)^.canBeRemoved) then
                                         begin
                                           if (p.typ = ait_instruction) And
                                              ((Taicpu(p).OpCode = A_MOV)  or
                                               (Taicpu(p).opcode = A_MOVZX) or
                                               (Taicpu(p).opcode = A_MOVSX)) And
                                              (Taicpu(p).oper[1].typ = top_reg) then
                                             if not is_mov_for_div(Taicpu(p)) then
                                               begin
                                                 regCounter := reg32(Taicpu(p).oper[1].reg);
                                                 if (regCounter in reginfo.regsStillUsedAfterSeq) then
                                                   begin
                                                    if (hp1 = nil) then
                                                      hp1 := reginfo.lastReload[regCounter];
                                                   end
{$ifndef noremove}
                                                 else
                                                   begin
                                                     hp5 := p;
                                                     for cnt3 := pTaiprop(p.optinfo)^.regs[regCounter].nrofmods downto 1 do
                                                       begin
                                                         if regModifiedByInstruction(regCounter,hp5) then
                                                           PTaiProp(hp5.OptInfo)^.CanBeRemoved := True;
                                                         getNextInstruction(hp5,hp5);
                                                       end;
                                                   end
{$endif noremove}
                                               end
{$ifndef noremove}
                                             else
                                               PTaiProp(p.OptInfo)^.CanBeRemoved := True
{$endif noremove}
                                         end
*)
{$ifndef noremove}
(*                                       else *)
                                         PTaiProp(p.OptInfo)^.CanBeRemoved := True
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
           hp5 := Tai_asm_comment.Create(strpnew('New: '+att_reg2str[RegCounter]+', Old: '+
                                                  att_reg2str[RegInfo.New2OldReg[RegCounter]])));
           InsertLLItem(AsmL, Tai(hp2.previous), hp2, hp5);
                  End;
{$EndIf CSDebug}
 { If some registers were different in the old and the new sequence, move }
 { the contents of those old registers to the new ones                    }
                                   For RegCounter := R_EAX To R_EDI Do
                                     If Not(RegCounter in [R_ESP,procinfo^.framepointer]) And
                                        (RegInfo.New2OldReg[RegCounter] <> R_NO) Then
                                       Begin
                                         AllocRegBetween(AsmL,RegInfo.New2OldReg[RegCounter],
                                           PTaiProp(prevSeq.OptInfo)^.Regs[RegInfo.New2OldReg[RegCounter]].StartMod,hp2);
                                         if hp4 <> prevSeq then
                                           begin
                                             if assigned(reginfo.lastReload[regCounter]) then
                                               getLastInstruction(reginfo.lastReload[regCounter],hp3)
                                             else if assigned(reginfo.lastReload[regInfo.New2OldReg[regCounter]]) then
                                               getLastInstruction(reginfo.lastReload[regInfo.new2OldReg[regCounter]],hp3)
                                             else hp3 := hp4;
                                             clearRegContentsFrom(regCounter,prevSeq_next,hp3);
                                             getnextInstruction(hp3,hp3);
                                             allocRegBetween(asmL,regCounter,prevSeq,hp3);
                                           end;
                                         If Not(RegCounter In RegInfo.RegsLoadedForRef) And
                                                        {old reg                new reg}
                                            (RegInfo.New2OldReg[RegCounter] <> RegCounter) Then
                                           Begin
                                             getLastInstruction(p,hp3);
                                             If (hp4 <> prevSeq) or
                                                not(regCounter in rg.usableregsint + [R_EDI,R_ESI]) or
                                                not ReplaceReg(asmL,RegInfo.New2OldReg[RegCounter],
                                                      regCounter,hp3,
                                                      PTaiProp(PrevSeq.optInfo)^.Regs[regCounter],true,hp5) then
                                               begin
                                                 hp3 := Tai_Marker.Create(NoPropInfoStart);
                                                 InsertLLItem(AsmL, prevSeq_next.previous,Tai(prevSeq_next), hp3);
                                                 hp5 := Taicpu.Op_Reg_Reg(A_MOV, S_L,
                                                                         {old reg          new reg}
                                                       RegInfo.New2OldReg[RegCounter], RegCounter);
                                                 new(pTaiprop(hp5.optinfo));
                                                 pTaiprop(hp5.optinfo)^ := pTaiprop(prevSeq_next.optinfo)^;
                                                 pTaiprop(hp5.optinfo)^.canBeRemoved := false;
                                                 InsertLLItem(AsmL, prevSeq_next.previous, Tai(prevSeq_next), hp5);
                                                 hp3 := Tai_Marker.Create(NoPropInfoEnd);
                                                 InsertLLItem(AsmL, prevSeq_next.previous, Tai(prevSeq_next), hp3);
                                                 { adjusts states in previous instruction so that it will  }
                                                 { definitely be different from the previous or next state }
                                                 incstate(pTaiprop(hp5.optinfo)^.
                                                   regs[RegInfo.New2OldReg[RegCounter]].rstate,20);
                                                 incstate(pTaiprop(hp5.optinfo)^.
                                                   regs[regCounter].wstate,20);
                                                 updateState(RegInfo.New2OldReg[RegCounter],hp5);
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
                                               PTaiProp(hp4.OptInfo)^.Regs[RegCounter],
                                               hp2,hp3);
                                           End;
                                       End;
(*
                                   If hp1 <> nil Then
                                     p := hp1;
*)
                                   Continue;
                                 End
(*
                               Else
                                 If (PTaiProp(p.OptInfo)^.
                                      regs[reg32(Taicpu(p).oper[1].reg)].typ
                                        in [con_ref,con_noRemoveRef]) and
                                    (PTaiProp(p.OptInfo)^.CanBeRemoved) Then
                                   if (cnt > 0) then
                                     begin
                                       p := hp2;
                                       Cnt2 := 1;
                                       While Cnt2 <= Cnt Do
                                         Begin
                                           If RegInInstruction(Taicpu(hp2).oper[1].reg, p) Then
                                             PTaiProp(p.OptInfo)^.CanBeRemoved := False;
                                           Inc(Cnt2);
                                           GetNextInstruction(p, p);
                                         End;
                                       Continue;
                                     End
                                   else
                                     begin
                                       { Fix for web bug 972 }
                                       regCounter := Reg32(Taicpu(p).oper[1].reg);
                                       cnt := PTaiProp(p.optInfo)^.Regs[regCounter].nrOfMods;
                                       hp3 := p;
                                       for cnt2 := 1 to cnt do
                                         if not(regModifiedByInstruction(regCounter,hp3) and
                                                not(PTaiProp(hp3.optInfo)^.canBeRemoved)) then
                                           getNextInstruction(hp3,hp3)
                                         else
                                           break;
                                       getLastInstruction(p,hp4);
                                       RestoreRegContentsTo(regCounter,
                                         PTaiProp(hp4.optInfo)^.Regs[regCounter],
                                         p,hp3);
                                     end;
*)
                              End;
                          End;
                      { try to replace the new reg with the old reg }
                      if not(PTaiProp(p.optInfo)^.canBeRemoved) then
                        if (Taicpu(p).oper[0].typ = top_reg) and
                           (Taicpu(p).oper[1].typ = top_reg) and
                           { only remove if we're not storing something in a regvar }
                           (Taicpu(p).oper[1].reg in (rg.usableregsint+[R_EDI])) and
                           (Taicpu(p).opcode = A_MOV) and
                           getLastInstruction(p,hp4) and
                          { we only have to start replacing from the instruction after the mov, }
                          { but replacereg only starts with getnextinstruction(p,p)             }
                            replaceReg(asmL,Taicpu(p).oper[0].reg,
                              Taicpu(p).oper[1].reg,p,
                              pTaiprop(hp4.optInfo)^.regs[Taicpu(p).oper[1].reg],false,hp1) then
                          begin
                            pTaiprop(p.optInfo)^.canBeRemoved := true;
                            allocRegBetween(asmL,Taicpu(p).oper[0].reg,
                              pTaiProp(p.optInfo)^.regs[Taicpu(p).oper[0].reg].startMod,hp1);
                          end
                        else
                          begin
                            if (Taicpu(p).oper[1].typ = top_reg) and
                               not regInOp(Taicpu(p).oper[1].reg,Taicpu(p).oper[0]) then
                             removePrevNotUsedLoad(p,reg32(Taicpu(p).oper[1].reg),false);
                             if doSubOpts and
                                (Taicpu(p).opcode <> A_LEA) and
                                (Taicpu(p).oper[0].typ = top_ref) then
                              begin
                                regcounter :=
                                  memtoreg(taicpu(p),
                                  Taicpu(p).oper[0].ref^,hp5);
                                if regcounter <> R_NO then
                                  if (taicpu(p).opcode = A_MOV) and
                                     (taicpu(p).oper[1].typ = top_reg) and
                                     (taicpu(p).oper[1].reg = regcounter) then
                                    begin
                                      pTaiProp(p.optinfo)^.canberemoved := true;
                                      allocregbetween(asml,reg32(regcounter),hp5,p);
                                    end
                                  else
                                    begin
                                      Taicpu(p).loadreg(0,regcounter);
                                      regcounter := reg32(regcounter);
                                      allocregbetween(asml,regcounter,hp5,p);
                                      incstate(pTaiProp(p.optinfo)^.regs[regcounter].rstate,1);
                                      updatestate(regcounter,p);
                                    end;
                              end;
                          end;
                        { at first, only try optimizations of large blocks, because doing }
                        { doing smaller ones may prevent bigger ones from completing in   }
                        { in the next pass                                                }
                        if not doSubOpts and (orgNrOfMods <> 0) then
                          begin
                            p := hp2;
                            for cnt := 1 to pred(orgNrOfMods) do
                              getNextInstruction(p,p);
                          end;
                      End;
                    top_symbol,Top_Const:
                      Begin
                        Case Taicpu(p).oper[1].typ Of
                          Top_Reg:
                            Begin
                              regCounter := Reg32(Taicpu(p).oper[1].reg);
                              If GetLastInstruction(p, hp1) Then
                                With PTaiProp(hp1.OptInfo)^.Regs[regCounter] Do
                                  if (typ in [con_const,con_noRemoveConst]) and
                                     (Taicpu(startMod).opsize >= Taicpu(p).opsize) and
                                     opsequal(Taicpu(StartMod).oper[0],Taicpu(p).oper[0]) Then
                                    begin
                                      PTaiProp(p.OptInfo)^.CanBeRemoved := True;
                                      allocRegBetween(asmL,regCounter,startMod,p);
                                    end
                                  else
                                    removePrevNotUsedLoad(p,reg32(Taicpu(p).oper[1].reg),false);

                            End;
                          Top_Ref:
                            if (Taicpu(p).oper[0].typ = top_const) and
                               getLastInstruction(p,hp1) and
                               findRegWithConst(hp1,Taicpu(p).opsize,Taicpu(p).oper[0].val,regCounter) then
                              begin
                                Taicpu(p).loadreg(0,regCounter);
                                allocRegBetween(AsmL,reg32(regCounter),
                                  PTaiProp(hp1.optinfo)^.regs[reg32(regCounter)].startMod,p);
                              end;
                        End;
                      End;
                  End;

                End;
              A_LEAVE:
                begin
                  if getlastinstruction(p,hp1) then
                    removeLocalStores(hp1);
                end;
              A_STD: If GetLastInstruction(p, hp1) And
                        (PTaiProp(hp1.OptInfo)^.DirFlag = F_Set) Then
                        PTaiProp(Tai(p).OptInfo)^.CanBeRemoved := True;
              else
                begin
                  for cnt := 1 to maxch do
                    begin
                      case InsProp[taicpu(p).opcode].Ch[cnt] of
                        Ch_ROp1:
                          if (taicpu(p).oper[0].typ = top_ref) and
                             ((taicpu(p).opcode < A_F2XM1) or
                              ((taicpu(p).opcode > A_IN) and
                               (taicpu(p).opcode < A_OUT)) or
                              (taicpu(p).opcode = A_PUSH) or
                              ((taicpu(p).opcode >= A_RCL) and
                               (taicpu(p).opcode <= A_XOR))) then
                            begin
                              regcounter :=
                                memtoreg(taicpu(p),
                                Taicpu(p).oper[0].ref^,hp5);
                              if regcounter <> R_NO then
                                begin
                                  Taicpu(p).loadreg(0,regcounter);
                                  regcounter := reg32(regcounter);
                                  allocregbetween(asml,regcounter,hp5,p);
                                  incstate(pTaiProp(p.optinfo)^.regs[regcounter].rstate,1);
                                  updatestate(regcounter,p);
                                end;
                            end;
                        Ch_MOp1:
                          if Not(CS_LittleSize in aktglobalswitches) And
                             (taicpu(p).oper[0].typ = top_ref) then
                            begin
                              regcounter :=
                                memtoreg(taicpu(p),
                                Taicpu(p).oper[0].ref^,hp5);
                              if (regcounter <> R_NO) (* and
                                 (not getNextInstruction(p,hp1) or
                                  (RegLoadedWithNewValue(reg32(regcounter),false,hp1) or
                                   FindRegDealloc(reg32(regcounter),hp1))) *) then
                                begin
                                  hp1 := Tai_Marker.Create(NoPropInfoEnd);
                                  insertllitem(asml,p,p.next,hp1);
                                  hp1 := taicpu.op_reg_ref(A_MOV,
                                    regsize(regcounter),regcounter,
                                    taicpu(p).oper[0].ref^);
                                  new(pTaiprop(hp1.optinfo));
                                  pTaiProp(hp1.optinfo)^ := pTaiProp(p.optinfo)^;
                                  insertllitem(asml,p,p.next,hp1);
                                  incstate(pTaiProp(hp1.optinfo)^.regs[reg32(regcounter)].rstate,1);
                                  updatestate(reg32(regcounter),hp1);
                                  hp1 := Tai_Marker.Create(NoPropInfoStart);
                                  insertllitem(asml,p,p.next,hp1);
                                  Taicpu(p).loadreg(0,regcounter);
                                  regcounter := reg32(regcounter);
                                  allocregbetween(asml,regcounter,hp5,
                                    tai(p.next.next));
                                end;
                            end;
                        Ch_ROp2:
                          if ((taicpu(p).opcode = A_CMP) or
                              (taicpu(p).opcode = A_TEST)) and
                             (taicpu(p).oper[1].typ = top_ref) then
                            begin
                              regcounter :=
                                memtoreg(taicpu(p),
                                Taicpu(p).oper[1].ref^,hp5);
                              if regcounter <> R_NO then
                                begin
                                  Taicpu(p).loadreg(1,regcounter);
                                  regcounter := reg32(regcounter);
                                  allocregbetween(asml,regcounter,hp5,p);
                                  incstate(pTaiProp(p.optinfo)^.regs[regcounter].rstate,1);
                                  updatestate(regcounter,p);
                                end;
                            end;
                        Ch_MOp2:
                          if not(cs_littlesize in aktglobalswitches) and
                             (taicpu(p).oper[1].typ = top_ref) and
                             ((taicpu(p).opcode < A_BT) or
                              ((taicpu(p).opcode > A_IN) and
                               (taicpu(p).opcode < A_OUT)) or
                              (taicpu(p).opcode = A_PUSH) or
                              ((taicpu(p).opcode >= A_RCL) and
                               (taicpu(p).opcode <= A_XOR))) then
                            begin
                              regcounter :=
                                memtoreg(taicpu(p),
                                Taicpu(p).oper[1].ref^,hp5);
                              if (regcounter <> R_NO) (* and
                                 (not getNextInstruction(p,hp1) or
                                  (RegLoadedWithNewValue(reg32(regcounter),false,hp1) or
                                   FindRegDealloc(reg32(regcounter),hp1))) *) then
                                begin
                                  hp1 := Tai_Marker.Create(NoPropInfoEnd);
                                  insertllitem(asml,p,p.next,hp1);
                                  hp1 := taicpu.op_reg_ref(A_MOV,
                                    regsize(regcounter),regcounter,
                                    taicpu(p).oper[1].ref^);
                                  new(pTaiprop(hp1.optinfo));
                                  pTaiProp(hp1.optinfo)^ := pTaiProp(p.optinfo)^;
                                  insertllitem(asml,p,p.next,hp1);
                                  incstate(pTaiProp(hp1.optinfo)^.regs[reg32(regcounter)].rstate,1);
                                  updatestate(reg32(regcounter),hp1);
                                  hp1 := Tai_Marker.Create(NoPropInfoStart);
                                  insertllitem(asml,p,p.next,hp1);
                                  Taicpu(p).loadreg(1,regcounter);
                                  regcounter := reg32(regcounter);
                                  allocregbetween(asml,regcounter,hp5,
                                    tai(p.next.next));
                                end;
                            end;
                      end;
                    end;
                end;
            End
          End;
      End;
      GetNextInstruction(p, p);
    End;
End;

function removeInstructs(asmL: TAAsmoutput; first, last: Tai): boolean;
{ Removes the marked instructions and disposes the PTaiProps of the other }
{ instructions                                                            }
Var
  p, hp1: Tai;
  nopropinfolevel: longint;
begin
  removeInstructs := false;
  p := First;
  nopropinfolevel := 0;
  While (p <> Last) Do
    Begin
      If (p.typ = ait_marker) and
         (Tai_marker(p).kind = noPropInfoStart) then
        begin
          hp1 := Tai(p.next);
          asmL.remove(p);
          p.free;
          nopropinfolevel := 1;
          while (nopropinfolevel <> 0) do
            begin
              p := Tai(hp1.next);
{$ifndef noinstremove}
              { allocregbetween can insert new ait_regalloc objects }
              { without optinfo                                     }
              if (hp1.typ = ait_marker) then
                begin
                  case Tai_marker(hp1).kind of
                    { they can be nested! }
                    noPropInfoStart: inc(nopropinfolevel);
                    noPropInfoEnd: dec(nopropinfolevel);
                    else
                      begin
                        hp1 := p;
                        continue;
                      end;
                  end;
                  asmL.remove(hp1);
                  hp1.free;
                end
              else if assigned(hp1.optinfo) then
                if pTaiprop(hp1.optinfo)^.canBeRemoved then
                  begin
                    dispose(pTaiprop(hp1.optinfo));
                    hp1.optinfo := nil;
                    asmL.remove(hp1);
                    hp1.free;
                  end
                else
{$endif noinstremove}
                  begin
                    dispose(pTaiprop(hp1.optinfo));
                    hp1.optinfo := nil;
                  end;
              hp1 := p;
            end;
        end
      else
{$ifndef noinstremove}
        if assigned(p.optInfo) and
              PTaiProp(p.optInfo)^.canBeRemoved then
          begin
            hp1 := Tai(p.next);
            AsmL.Remove(p);
            p.free;
            p := hp1;
            removeInstructs := true;
          End
        Else
{$endif noinstremove}
          Begin
            p.OptInfo := nil;
            p := Tai(p.next);;
          End;
    End;
    FreeMem(TaiPropBlock, NrOfTaiObjs*SizeOf(TTaiProp))
End;

function CSE(AsmL: TAAsmOutput; First, Last: Tai; pass: longint): boolean;
Begin
  DoCSE(AsmL, First, Last, not(cs_slowoptimize in aktglobalswitches) or (pass >= 2),
        not(cs_slowoptimize in aktglobalswitches) or (pass >= 1));
 { register renaming }
  if not(cs_slowoptimize in aktglobalswitches) or (pass > 0) then
    doRenaming(asmL, first, last);
  cse := removeInstructs(asmL, first, last);
End;

End.

{
  $Log$
  Revision 1.26  2002-04-02 17:11:34  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.25  2002/03/31 20:26:38  jonas
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

  Revision 1.24  2002/03/04 19:10:12  peter
    * removed compiler warnings

  Revision 1.23  2001/12/04 15:58:13  jonas
    * unnecessary loads of constants are now also remove by
      removePrevNotUsedLoad()

  Revision 1.22  2001/11/30 16:35:02  jonas
    * added missing allocregbetween() call for a memtoreg() optimization

  Revision 1.21  2001/10/27 10:20:43  jonas
    + replace mem accesses to locations to which a reg was stored recently with that reg

  Revision 1.20  2001/10/14 11:50:21  jonas
    + also replace mem references in modify operands with regs

  Revision 1.19  2001/10/12 13:58:05  jonas
    + memory references are now replaced by register reads in "regular"
      instructions (e.g. "addl ref1,%eax" will be replaced by "addl %ebx,%eax"
      if %ebx contains ref1). Previously only complete load sequences were
      optimized away, but not such small accesses in other instructions than
      mov/movzx/movsx

  Revision 1.18  2001/09/04 14:01:03  jonas
    * commented out some inactive code in csopt386
    + small improvement: lea is now handled the same as mov/zx/sx

  Revision 1.17  2001/08/29 14:07:43  jonas
    * the optimizer now keeps track of flags register usage. This fixes some
      optimizer bugs with int64 calculations (because of the carry flag usage)
    * fixed another bug which caused wrong optimizations with complex
      array expressions

  Revision 1.16  2001/08/26 13:36:55  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.15  2001/04/06 16:24:38  jonas
    * fixed bug due to short boolean evaluation

  Revision 1.14  2001/04/02 21:20:36  peter
    * resulttype rewrite

  Revision 1.13  2001/01/10 08:52:40  michael
  + Patch from jonas so 1.0.2 can be used to cycle

  Revision 1.12  2001/01/07 15:51:17  jonas
    * fixed crashing bug to due previous changes

  Revision 1.11  2001/01/06 23:35:05  jonas
    * fixed webbug 1323

  Revision 1.10  2000/12/25 00:07:31  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.9  2000/12/05 09:33:42  jonas
    * when searching for constants in registers, the returned register
      sometimes didn't have the same size as the requested size

  Revision 1.8  2000/11/29 00:30:43  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.7  2000/11/28 16:32:11  jonas
    + support for optimizing simple sequences with div/idiv/mul opcodes

  Revision 1.6  2000/11/14 12:17:34  jonas
    * fixed some bugs in checksequence

  Revision 1.5  2000/11/09 12:34:44  jonas
    * fixed range check error

  Revision 1.4  2000/11/03 17:53:24  jonas
    * some small improvements

  Revision 1.3  2000/11/01 22:53:30  jonas
    * register contents were not cleared if there was only 1 instruction

      between de previous sequence and the current one

  Revision 1.2  2000/10/24 10:40:53  jonas
    + register renaming ("fixes" bug1088)
    * changed command line options meanings for optimizer:
        O2 now means peepholopts, CSE and register renaming in 1 pass
        O3 is the same, but repeated until no further optimizations are
          possible or until 5 passes have been done (to avoid endless loops)
    * changed aopt386 so it does this looping
    * added some procedures from csopt386 to the interface because they're
      used by rropt386 as well
    * some changes to csopt386 and daopt386 so that newly added instructions
      by the CSE get optimizer info (they were simply skipped previously),
      this fixes some bugs

  Revision 1.1  2000/10/15 09:47:43  peter
    * moved to i386/

  Revision 1.14  2000/09/30 13:07:23  jonas
    * fixed support for -Or with new features of CSE

  Revision 1.13  2000/09/29 23:14:45  jonas
    * search much further back for CSE sequences (non-conflicting stores are
      now passed)
    * remove more unnecessary loads of registers (especially the self pointer)

  Revision 1.12  2000/09/26 11:49:41  jonas
    * writes to register variables and to the self pointer now also count as
      memore writes

  Revision 1.11  2000/09/25 09:50:29  jonas
    - removed TP conditional code

  Revision 1.10  2000/09/24 15:06:14  peter
    * use defines.inc

  Revision 1.9  2000/09/22 15:01:59  jonas
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
      maybe_loadself outputs and push/pops transformed by below optimization
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
