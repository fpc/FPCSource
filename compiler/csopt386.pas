{
    $Id$
    Copyright (c) 1998-2000 by Jonas Maebe

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

{$ifdef newOptimizations}
{$define foropt}
{$define replacereg}
{$define arithopt}
{$define foldarithops}
{$endif newOptimizations}

Interface

Uses aasm;

{Procedure CSOpt386(First, Last: Pai);}
Procedure CSE(AsmL: PAasmOutput; First, Last: Pai);

Implementation

Uses
  CObjects, verbose, hcodegen, globals,cpubase,cpuasm,DAOpt386, tgeni386;

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

Function CheckSequence(p: Pai; Reg: TRegister; Var Found: Longint; Var RegInfo: TRegInfo): Boolean;
{checks whether the current instruction sequence (starting with p) and the
 one between StartMod and EndMod of Reg are the same. If so, the number of
 instructions that match is stored in Found and true is returned, otherwise
 Found holds the number of instructions between StartMod and EndMod and false
 is returned}
Var hp2, hp3{, EndMod}: Pai;
    PrevNonRemovablePai: Pai;
    Cnt, OldNrOfMods: Longint;
    OrgRegInfo, HighRegInfo: TRegInfo;
    HighFound, OrgRegFound: Byte;
    RegCounter: TRegister;
    OrgRegResult: Boolean;
    TmpResult: Boolean;
    TmpState: Byte;
Begin {CheckSequence}
  Reg := Reg32(Reg);
  TmpResult := False;
  FillChar(OrgRegInfo, SizeOf(OrgRegInfo), 0);
  OrgRegFound := 0;
  HighFound := 0;
  OrgRegResult := False;
  RegCounter := R_EAX;
  GetLastInstruction(p, PrevNonRemovablePai);
  While (RegCounter <= R_EDI) And
        (PPaiProp(PrevNonRemovablePai^.OptInfo)^.Regs[RegCounter].Typ <> Con_Ref) Do
    Inc(RegCounter);
  While (RegCounter <= R_EDI) Do
    Begin
      FillChar(RegInfo, SizeOf(RegInfo), 0);
      RegInfo.NewRegsEncountered := [procinfo^.FramePointer, R_ESP];
      RegInfo.OldRegsEncountered := RegInfo.NewRegsEncountered;
      RegInfo.New2OldReg[procinfo^.FramePointer] := procinfo^.FramePointer;
      RegInfo.New2OldReg[R_ESP] := R_ESP;
      Found := 0;
      hp2 := PPaiProp(PrevNonRemovablePai^.OptInfo)^.Regs[RegCounter].StartMod;
      If (PrevNonRemovablePai <> PPaiProp(PrevNonRemovablePai^.OptInfo)^.Regs[RegCounter].StartMod)
        Then OldNrOfMods := PPaiProp(PrevNonRemovablePai^.OptInfo)^.Regs[RegCounter].NrOfMods
        Else OldNrOfMods := 1;
      hp3 := p;
      While (Found <> OldNrOfMods) And
                                  { old  new }
             InstructionsEquivalent(hp2, hp3, RegInfo) Do
        Begin
          GetNextInstruction(hp2, hp2);
          GetNextInstruction(hp3, hp3);
          Inc(Found)
        End;
      If (Found <> OldNrOfMods) Then
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
            HighFound := Found;
            HighRegInfo := RegInfo;
          End;
      If (RegCounter = Reg) Then
        Begin
          OrgRegFound := Found;
          OrgRegResult := TmpResult;
          OrgRegInfo := RegInfo
        End;
      Repeat
        Inc(RegCounter);
      Until (RegCounter > R_EDI) or
            ((PPaiProp(PrevNonRemovablePai^.OptInfo)^.Regs[RegCounter].Typ = Con_Ref) {And
             ((Regcounter = Reg) Or
              Not(PaiInSequence(p, PPaiProp(PrevNonRemovablePai^.OptInfo)^.Regs[RegCounter]))) }
            );
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
        Found := OrgRegFound;
        RegInfo := OrgRegInfo;
      End;
{ sometimes, registers in RegsLoadedForRef (which normally aren't/shouldn't }
{ be used anymore after the sequence, are still used nevertheless (when     }
{ range checking is on for instance, because this is not "normal" generated }
{ code, but more or less manually inserted)                                 }
{$ifndef fpc}
  If TmpResult Then
{$else fpc}
  If CheckSequence And (Found > 0) Then
{$endif fpc}
    For RegCounter := R_EAX to R_EDI Do
      If (RegCounter in RegInfo.RegsLoadedForRef) And
         (RegInfo.New2OldReg[RegCounter] <> RegCounter) Then
        Begin
          OldNrOfMods := PPaiProp(PrevNonRemovablePai^.OptInfo)^.
            Regs[RegInfo.New2OldReg[RegCounter]].NrOfMods;
          hp2 := p;
          For Cnt := 1 to Pred(OldNrOfMods) Do
            GetNextInstruction(hp2, hp2);
{ hp2 now containts the last instruction of the sequence }
{ get the writestate at this point of the register in TmpState }
          TmpState := PPaiProp(hp2^.OptInfo)^.Regs[RegCounter].WState;
{ now, even though reg is in RegsLoadedForRef, sometimes it's still used  }
{ afterwards. It is not if either it is not in usedregs anymore after the }
{ sequence, or if it is loaded with a new value right after the sequence  }
          If GetNextInstruction(hp2, hp2) and
             (TmpState = PPaiProp(hp2^.OptInfo)^.Regs[RegCounter].WState) And
             (RegCounter in PPaiProp(hp2^.OptInfo)^.UsedRegs) Then
{ it is still used, so remove it from RegsLoadedForRef }
            Begin
{$ifdef regrefdebug}
              hp3 := new(pai_asm_comment,init(strpnew(att_reg2str[regcounter]+
                         ' removed from regsloadedforref')));
              hp3^.fileinfo := hp2^.fileinfo;
              hp3^.next := hp2^.next;
              hp3^.previous := hp2;
              hp2^.next := hp3;
              If assigned(hp3^.next) then
                Pai(hp3^.next)^.previous := hp3;
{$endif regrefdebug}
              Exclude(RegInfo.RegsLoadedForRef,RegCounter);
            End;
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

{$ifdef replacereg}
function FindRegDealloc(reg: tregister; p: pai): boolean;
{ assumes reg is a 32bit register }
begin
  findregdealloc := false;
  while assigned(p^.previous) and
        ((Pai(p^.previous)^.typ in (skipinstr+[ait_align])) or
         ((Pai(p^.previous)^.typ = ait_label) and
          not(Pai_Label(p^.previous)^.l^.is_used))) do
    begin
      p := pai(p^.previous);
      if (p^.typ = ait_regalloc) and
         (pairegalloc(p)^.reg = reg) then
        begin
          findregdealloc := not(pairegalloc(p)^.allocation);
          break;
        end;
    end
end;

Procedure RestoreRegContentsTo(reg: TRegister; const c: TContent; p, endP: pai);
var
{$ifdef replaceregdebug}
    hp: pai;
{$endif replaceregdebug}
    tmpState: byte;
begin
{$ifdef replaceregdebug}
  hp := new(pai_asm_comment,init(strpnew(
          'restored '+att_reg2str[reg]+' with data from here...')));
  hp^.next := p;
  hp^.previous := p^.previous;
  p^.previous := hp;
  if assigned(hp^.previous) then
    hp^.previous^.next := hp;
{$endif replaceregdebug}
  PPaiProp(p^.optInfo)^.Regs[reg] := c;
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
        'restored '+att_reg2str[reg]+' till here...')));
      hp^.next := p;
      hp^.previous := p^.previous;
      p^.previous := hp;
      if assigned(hp^.previous) then
        hp^.previous^.next := hp;
    end;
{$endif replaceregdebug}
end;

Procedure ClearRegContentsFrom(reg: TRegister; p, endP: pai);
{ first clears the contents of reg from p till endP. Then the contents are }
{ cleared until the first instruction that changes reg                     }
var
{$ifdef replaceregdebug}
    hp: pai;
{$endif replaceregdebug}
    tmpState: byte;
begin
  PPaiProp(p^.optInfo)^.Regs[reg].typ := con_unknown;
  While (p <> endP) Do
    Begin
      PPaiProp(p^.optInfo)^.Regs[reg].typ := con_unknown;
      getNextInstruction(p,p);
    end;
  tmpState := PPaiProp(p^.optInfo)^.Regs[reg].wState;
  repeat
    PPaiProp(p^.optInfo)^.Regs[reg].typ := con_unknown;
  until not getNextInstruction(p,p) or
        (PPaiProp(p^.optInfo)^.Regs[reg].wState <> tmpState);
{$ifdef replaceregdebug}
  if assigned(p) then
    begin
      hp := new(pai_asm_comment,init(strpnew(
        'cleared '+att_reg2str[reg]+' till here...')));
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

Procedure ChangeReg(var Reg: TRegister; orgReg, newReg: TRegister);
begin
  if reg = newReg then
    reg := orgReg
  else if reg = regtoreg8(newReg) then
         reg := regtoreg8(orgReg)
  else if reg = regtoreg16(newReg) then
         reg := regtoreg16(orgReg);
end;

procedure changeOp(var o: toper; orgReg, newReg: tregister);
begin
  case o.typ of
    top_reg: changeReg(o.reg,orgReg,newReg);
    top_ref:
      begin
        changeReg(o.ref^.base,orgReg,newReg);
        changeReg(o.ref^.index,orgReg,newReg);
      end;
  end;
end;

Procedure DoReplaceReg(orgReg,newReg: tregister; hp: paicpu);
var opCount: byte;
begin
  for opCount := 0 to 2 do
    changeOp(hp^.oper[opCount],orgReg,newReg)
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

function RegReadByInstruction(reg: TRegister; hp: pai): boolean;
{ assumes hp doesn't modify registers implicitely (like div) }
{ and that reg is a 32bit register                           }
var p: paicpu;
    opCount: byte;
begin
  RegReadByInstruction := false;
  p := paicpu(hp);
  if hp^.typ <> ait_instruction then
    exit;
  case p^.opcode of
    A_IMUL:
      case p^.ops of
        1: regReadByInstruction := (reg = R_EAX) or reginOp(reg,p^.oper[0]);
        2,3:
          regReadByInstruction := regInOp(reg,p^.oper[0]) or
            regInOp(reg,p^.oper[1]);
      end;
{    A_IDIV,A_DIV,A_IMUL:
      begin
        regReadByInstruction :=
          regInOp(reg,p^.oper[0]) or
          (((p^.opcode = A_IDIV) or
            (p^.opcode = A_DIV)) and
           (reg = R_EAX));
      end;}
    else
      begin
        for opCount := 0 to 2 do
          if (p^.oper[opCount].typ = top_ref) and
             RegInRef(reg,p^.oper[opCount].ref^) then
            begin
              RegReadByInstruction := true;
              exit
            end;
        for opCount := 1 to MaxCh do
          case InsProp[p^.opcode].Ch[opCount] of
            Ch_RWOp1,Ch_ROp1{$ifdef arithopt},Ch_MOp1{$endif}:
              if (p^.oper[0].typ = top_reg) and
                 (reg32(p^.oper[0].reg) = reg) then
                begin
                  RegReadByInstruction := true;
                  exit
                end;
            Ch_RWOp2,Ch_ROp2{$ifdef arithopt},Ch_MOp2{$endif}:
              if (p^.oper[1].typ = top_reg) and
                 (reg32(p^.oper[1].reg) = reg) then
                begin
                  RegReadByInstruction := true;
                  exit
                end;
            Ch_RWOp3,Ch_ROp3{$ifdef arithopt},Ch_MOp3{$endif}:
              if (p^.oper[2].typ = top_reg) and
                 (reg32(p^.oper[2].reg) = reg) then
                begin
                  RegReadByInstruction := true;
                  exit
                end;
          end;
      end;
  end;
end;

procedure DoReplaceReadReg(orgReg,newReg: tregister; p: paicpu);
var opCount: byte;
begin
  { handle special case }
  case p^.opcode of
    A_IMUL:
      begin
        case p^.ops of
          1: internalerror(1301001);
          2,3:
            begin
              changeOp(p^.oper[0],orgReg,newReg);
              if p^.ops = 3 then
                changeOp(p^.oper[1],orgReg,newReg);
            end;
        end;
      end;
    A_DIV,A_IDIV,A_MUL: internalerror(1301002);
    else
      begin
        for opCount := 0 to 2 do
          if p^.oper[opCount].typ = top_ref then
            changeOp(p^.oper[opCount],orgReg,newReg);
        for opCount := 1 to MaxCh do
          case InsProp[p^.opcode].Ch[opCount] of
            Ch_ROp1:
              if p^.oper[0].typ = top_reg then
                ChangeReg(p^.oper[0].reg,orgReg,newReg);
            Ch_ROp2:
              if p^.oper[1].typ = top_reg then
                ChangeReg(p^.oper[1].reg,orgReg,newReg);
            Ch_ROp3:
              if p^.oper[2].typ = top_reg then
                ChangeReg(p^.oper[2].reg,orgReg,newReg);
          end;
      end;
  end;
end;

function ReplaceReg(asmL: PaasmOutput; orgReg, newReg: TRegister; p: pai;
           const c: TContent; orgRegCanBeModified: Boolean;
           var returnEndP: pai): Boolean;
{ Tries to replace orgreg with newreg in all instructions coming after p }
{ until orgreg gets loaded with a new value. Returns true if successful, }
{ false otherwise. If successful, the contents of newReg are set to c,   }
{ which should hold the contents of newReg before the current sequence   }
{ started                                                                }
{ if the functino returns true, returnEndP holds the lat instruction     }
{ where newReg was replaced by orgReg                                    }
var endP, hp: Pai;
    removeLast, sequenceEnd, tmpResult, newRegModified, orgRegRead: Boolean;

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
      while hp <> endP do
        begin
          if not(PPaiProp(hp^.optInfo)^.canBeRemoved) and
             (hp^.typ = ait_instruction) then
            DoReplaceReg(orgReg,newReg,paicpu(hp));
          GetNextInstruction(hp,hp)
        end;
      if assigned(endp) and (endp^.typ = ait_instruction) then
        DoReplaceReadReg(orgReg,newReg,paicpu(endP));
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
        RestoreRegContentsTo(newReg, c ,p, hp);
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
{$endif replacereg}

{$ifdef arithopt}
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
     tmpresult := (PPaiProp(p^.OptInfo)^.Regs[Counter].Typ = Con_Const) and
       (paicpu(PPaiProp(p^.OptInfo)^.Regs[Counter].StartMod)^.opsize = size) and
       (paicpu(PPaiProp(p^.OptInfo)^.Regs[Counter].StartMod)^.oper[0].typ = top_const) and
       (paicpu(PPaiProp(p^.OptInfo)^.Regs[Counter].StartMod)^.oper[0].val = l);
{$ifdef testing}
     if (PPaiProp(p^.OptInfo)^.Regs[Counter].Typ = Con_Const) then
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
{$endif arithopt}

Procedure DoCSE(AsmL: PAasmOutput; First, Last: Pai);
{marks the instructions that can be removed by RemoveInstructs. They're not
 removed immediately because sometimes an instruction needs to be checked in
 two different sequences}
Var Cnt, Cnt2: Longint;
    p, hp1, hp2: Pai;
    hp3, hp4: pai;
{$ifdef replacereg}
    hp5 : pai;
{$else}
  {$ifdef csdebug}
    hp5 : pai;
  {$endif}
{$endif}
    RegInfo: TRegInfo;
    RegCounter: TRegister;
    TmpState: Byte;
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
                               (hp1^.typ <> ait_marker)
                              Then
{so we don't try to check a sequence when p is the first instruction of the block}
                               If CheckSequence(p, Paicpu(p)^.oper[1].reg, Cnt, RegInfo) And
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
                                       If (hp1 = nil) And
                                          Not(RegInInstruction(Paicpu(hp2)^.oper[1].reg, p) Or
                                              RegInInstruction(Reg32(Paicpu(hp2)^.oper[1].reg), p)) And
                                          Not((p^.typ = ait_instruction) And
                                              (paicpu(p)^.OpCode = A_MOV) And
                                              (paicpu(p)^.Oper[0].typ = top_ref) And
                                              (PPaiProp(p^.OptInfo)^.Regs[Reg32(paicpu(p)^.Oper[1].reg)].NrOfMods
                                                 <= (Cnt - Cnt2 + 1)))
                                         Then hp1 := p;
{$ifndef noremove}
                                       PPaiProp(p^.OptInfo)^.CanBeRemoved := True;
{$endif noremove}
                                       Inc(Cnt2);
                                       GetNextInstruction(p, p);
                                     End;
                                   hp3 := New(Pai_Marker,Init(NoPropInfoStart));
                                   InsertLLItem(AsmL, Pai(hp2^.Previous), hp2, hp3);
 {hp4 is used to get the contents of the registers before the sequence}
                                   GetLastInstruction(hp2, hp4);
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
                                           PPaiProp(hp4^.OptInfo)^.Regs[RegInfo.New2OldReg[RegCounter]].StartMod,hp2);
                                         If Not(RegCounter In RegInfo.RegsLoadedForRef) And
                                                        {old reg                new reg}
                                            (RegInfo.New2OldReg[RegCounter] <> RegCounter) Then
                                           Begin
{$ifdef replacereg}
                                             getLastInstruction(p,hp3);
                                             If not(regCounter in usableRegs + [R_EDI,R_ESI]) or
                                                not ReplaceReg(asmL,RegInfo.New2OldReg[RegCounter],
                                                      regCounter,hp3,
                                                      PPaiProp(hp4^.optInfo)^.Regs[regCounter],true,hp5) then
                                               begin
{$endif replacereg}
                                                 hp3 := New(Paicpu,Op_Reg_Reg(A_MOV, S_L,
                                                                         {old reg          new reg}
                                                       RegInfo.New2OldReg[RegCounter], RegCounter));
                                                 InsertLLItem(AsmL, Pai(hp2^.previous), hp2, hp3);
{$ifdef replacereg}
                                               end
{$endif replacereg}
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
{     movl %edi, -4(%ebp)          movl %edi, -4(%ebp)                      }
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
{load Cnt2 with the total number of instructions of this sequence}
                                           Cnt2 := PPaiProp(hp4^.OptInfo)^.
                                                   Regs[RegInfo.New2OldReg[RegCounter]].NrOfMods;

                                           hp3 := hp2;
                                           For Cnt := 1 to Pred(Cnt2) Do
                                             GetNextInstruction(hp3, hp3);
                                           TmpState := PPaiProp(hp3^.OptInfo)^.Regs[RegCounter].WState;
                                           GetNextInstruction(hp3, hp3);
{$ifdef csdebug}
         Writeln('Cnt2: ',Cnt2);
         hp5 := new(pai_asm_comment,init(strpnew('starting here...')));
         InsertLLItem(AsmL, Pai(hp2^.previous), hp2, hp5);
{$endif csdebug}
                                               hp3 := hp2;
{first change the contents of the register inside the sequence}
                                               For Cnt := 1 to Cnt2 Do
                                                 Begin
{save the WState of the last pai object of the sequence for later use}
                                                   TmpState := PPaiProp(hp3^.OptInfo)^.Regs[RegCounter].WState;
{$ifdef csdebug}
         hp5 := new(pai_asm_comment,init(strpnew('WState for '+att_reg2str[Regcounter]+': '
                                                  +tostr(tmpstate))));
         InsertLLItem(AsmL, hp3, pai(hp3^.next), hp5);
{$endif csdebug}
                                                   PPaiProp(hp3^.OptInfo)^.Regs[RegCounter] :=
                                                     PPaiProp(hp4^.OptInfo)^.Regs[RegCounter];
                                                   GetNextInstruction(hp3, hp3);
                                                 End;
{here, hp3 = p = Pai object right after the sequence, TmpState = WState of
 RegCounter at the last Pai object of the sequence}
                                               GetLastInstruction(hp3, hp3);
                                               While GetNextInstruction(hp3, hp3) And
                                                     (PPaiProp(hp3^.OptInfo)^.Regs[RegCounter].WState
                                                      = TmpState) Do
{$ifdef csdebug}
    begin
         hp5 := new(pai_asm_comment,init(strpnew('WState for '+att_reg2str[Regcounter]+': '+
                                                  tostr(PPaiProp(hp3^.OptInfo)^.Regs[RegCounter].WState))));
         InsertLLItem(AsmL, hp3, pai(hp3^.next), hp5);
{$endif csdebug}
                                                 PPaiProp(hp3^.OptInfo)^.Regs[RegCounter] :=
                                                   PPaiProp(hp4^.OptInfo)^.Regs[RegCounter];
{$ifdef csdebug}
    end;
{$endif csdebug}
{$ifdef csdebug}
         hp5 := new(pai_asm_comment,init(strpnew('stopping here...')));
         InsertLLItem(AsmL, hp3, pai(hp3^.next), hp5);
{$endif csdebug}
                                          End;
                                       End;
                                   hp3 := New(Pai_Marker,Init(NoPropInfoEnd));
                                   InsertLLItem(AsmL, Pai(hp2^.Previous), hp2, hp3);
                                   If hp1 <> nil Then p := hp1;
                                   Continue;
                                 End
                               Else
                                 If (Cnt > 0) And
                                    (PPaiProp(p^.OptInfo)^.
                                      Regs[Reg32(Paicpu(p)^.oper[1].reg)].Typ = Con_Ref) And
                                    (PPaiProp(p^.OptInfo)^.CanBeRemoved) Then
                                   Begin
                                     hp2 := p;
                                     Cnt2 := 1;
                                     While Cnt2 <= Cnt Do
                                       Begin
                                         If RegInInstruction(Paicpu(hp2)^.oper[1].reg, p) Or
                                            RegInInstruction(Reg32(Paicpu(hp2)^.oper[1].reg), p) Then
                                           PPaiProp(p^.OptInfo)^.CanBeRemoved := False;
                                         Inc(Cnt2);
                                         GetNextInstruction(p, p);
                                       End;
                                     Continue;
                                   End;
                          End;
                      End;
{$ifdef replacereg}
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
{$endif replacereg}
                    top_symbol,Top_Const:
                      Begin
                        Case Paicpu(p)^.oper[1].typ Of
                          Top_Reg:
                            Begin
                              regCounter := Reg32(Paicpu(p)^.oper[1].reg);
                              If GetLastInstruction(p, hp1) Then
                                With PPaiProp(hp1^.OptInfo)^.Regs[regCounter] Do
                                  If (Typ = Con_Const) And
                                     (paicpu(startMod)^.opsize >= paicpu(p)^.opsize) and
                                     opsequal(paicpu(StartMod)^.oper[0],paicpu(p)^.oper[0]) Then
                                    begin
                                      PPaiProp(p^.OptInfo)^.CanBeRemoved := True;
                                      allocRegBetween(asmL,regCounter,startMod,p);
                                    end;
                            End;
{$ifdef arithopt}
                          Top_Ref:
                            if (paicpu(p)^.oper[0].typ = top_const) and
                               getLastInstruction(p,hp1) and
                               findRegWithConst(hp1,paicpu(p)^.opsize,paicpu(p)^.oper[0].val,regCounter) then
                              begin
                                paicpu(p)^.loadreg(0,regCounter);
                                allocRegBetween(AsmL,reg32(regCounter),
                                  PPaiProp(hp1^.optinfo)^.regs[regCounter].startMod,p);
                              end;
{$endif arithopt}
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
 Revision 1.56  2000-03-25 19:05:47  jonas
   * fixed some things for -Or. Make cycle now works with -OG2p3r if
     you use -Aas. There still a bug in popt386.pas that causes a
     problem with the binary writer, but I haven't found it yet

 Revision 1.55  2000/03/24 15:54:49  jonas
   * fix for -dnewoptimizations and -Or (never remove stores to regvars)
     but make cycle with -OG2p3r still fails :(

 Revision 1.54  2000/02/24 18:41:38  peter
   * removed warnings/notes

 Revision 1.53  2000/02/19 13:50:29  jonas
   * fixed bug in -dnewoptizations (showed itself  only if -Or was
     used as well I think)

 Revision 1.52  2000/02/17 07:46:49  jonas
   * -dreplacereg no logner tries to optimize "movl %reg1,%reg1" (which are
     always marked as CanBeRemoved)
   + some comments in -dreplacereg code
   * small fix which could cause crash when optimizer is compiler with -dTP

 Revision 1.51  2000/02/12 19:28:56  jonas
   * fix for imul optimization in popt386 (exclude top_ref as first
     argument)
   * in csopt386: change "mov reg1,reg2; <several operations on reg2>;
     mov reg2,reg1" to "<several operations on reg1>" (-dnewopt...)

 Revision 1.50  2000/02/12 14:10:14  jonas
   + change "mov reg1,reg2;imul x,reg2" to "imul x,reg1,reg2" in popt386
     (-dnewoptimizations)
   * shl(d) and shr(d) are considered to have a hardcoded register if
     they use cl as shift count (since you can't replace them with
     another register) in csopt386 (also for -dnewoptimizations)

 Revision 1.49  2000/02/12 10:54:18  jonas
   * fixed edi allocation in allocRegBetween
   * fixed bug I introduced yesterday, added comment to prevent it from
     happening again in the future

 Revision 1.48  2000/02/11 23:50:03  jonas
   * fixed crashing bug under Dos with -dnewoptimizations (found it,
     John!). Don't understand why it didn't crash under Linux :(

 Revision 1.47  2000/02/10 16:04:43  jonas
   * fixed stupid typo!

 Revision 1.46  2000/02/10 15:07:41  jonas
   * fixed small bug introduced with my previous fix

 Revision 1.45  2000/02/10 14:57:13  jonas
   * fixed bug due to lack of support for top_symbol operands

 Revision 1.44  2000/02/09 13:22:51  peter
   * log truncated

 Revision 1.43  2000/02/04 13:52:17  jonas
   * better support for regvars (still needs a move of the call to the optimize
   procedure to a place where resetusableregisters is not yet called to work)
   * small regallocation fixes for -dnewoptimizations

 Revision 1.42  2000/01/28 15:15:31  jonas
    * moved skipinstr from daopt386 to aasm
    * fixed crashing bug with -dreplacereg in csopt386.pas

 Revision 1.41  2000/01/23 11:11:37  michael
 + Fixes from Jonas.

 Revision 1.40  2000/01/22 16:10:06  jonas
   + all code generator generated "mov reg1,reg2" instructions are now
     attempted to be removed using the replacereg code
     (-dnewoptimizations)
   * small fixes to -dreplacereg code

 Revision 1.39  2000/01/13 13:07:05  jonas
   * released -dalignreg
   * some small fixes to -dnewOptimizations helper procedures

 Revision 1.38  2000/01/07 01:14:23  peter
   * updated copyright to 2000

 Revision 1.37  2000/01/03 17:11:17  jonas
   * fixed bug with -dreplacereg

 Revision 1.36  1999/12/05 16:48:43  jonas
   * CSE of constant loading in regs works properly again
   + if a constant is stored into memory using "mov const, ref" and
     there is a reg that contains this const, it is changed into
     "mov reg, ref"

 Revision 1.35  1999/12/02 11:26:41  peter
   * newoptimizations define added

 Revision 1.34  1999/11/21 13:09:41  jonas
   * fixed some missed optimizations because 8bit regs were not always
     taken into account

 Revision 1.33  1999/11/20 11:37:03  jonas
   * make cycle works with -dreplacereg (register renaming)! I have not
     tested it yet together with -darithopt, but I don't expect problems

 Revision 1.32  1999/11/14 11:26:53  jonas
   + basic register renaming (not yet working completely, between
     -dreplacereg/-dreplaceregdebug)

 Revision 1.31  1999/11/06 16:21:57  jonas
   + search optimial register to use in alignment code (compile with
     -dalignreg, -dalignregdebug to see chosen register in
     assembler code). Still needs support in ag386bin.

 Revision 1.30  1999/11/06 14:34:20  peter
   * truncated log to 20 revs

 Revision 1.29  1999/11/05 16:01:46  jonas
   + first implementation of choosing least used register for alignment code
      (not yet working, between ifdef alignreg)

 Revision 1.28  1999/10/11 11:11:31  jonas
   * fixed bug which sometimes caused a crash when optimizing blocks of code with
     assembler blocks (didn't notice before because of lack of zero page protection
     under Win9x :( )

 Revision 1.27  1999/10/01 13:51:40  jonas
   * CSE now updates the RegAlloc's

 Revision 1.26  1999/09/30 14:43:13  jonas
   * fixed small efficiency which caused some missed optimizations (saves 1
     assembler instruction on the whole compiler/RTL source tree! :)

 Revision 1.25  1999/09/27 23:44:50  peter
   * procinfo is now a pointer
   * support for result setting in sub procedure

 Revision 1.24  1999/08/25 11:59:58  jonas
   * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

}
