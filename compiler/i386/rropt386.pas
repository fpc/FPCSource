{
    $Id$
    Copyright (c) 1998-2000 by Jonas Maebe, member of the Free Pascal
      development team

    This unit contains register renaming functionality

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
Unit rrOpt386;

{$i defines.inc}

Interface

Uses aasm;

procedure doRenaming(asml: paasmoutput; first, last: pai);

Implementation

Uses
  {$ifdef replaceregdebug}cutils,{$endif}
  verbose,globals,cpubase,cpuasm,daopt386,csopt386,tgcpu;

function canBeFirstSwitch(p: paicpu; reg: tregister): boolean;
{ checks whether an operation on reg can be switched to another reg without an }
{ additional mov, e.g. "addl $4,%reg1" can be changed to "leal 4(%reg1),%reg2" }
begin
  canBeFirstSwitch := false;
  case p^.opcode of
    A_MOV,A_MOVZX,A_MOVSX,A_LEA:
      canBeFirstSwitch :=
        (p^.oper[1].typ = top_reg) and
        (reg32(p^.oper[1].reg) = reg);
    A_IMUL:
      canBeFirstSwitch :=
        (p^.ops >= 2) and
        (reg32(p^.oper[p^.ops-1].reg) = reg) and
        (p^.oper[0].typ <> top_ref);
    A_INC,A_DEC,A_SUB,A_ADD:
      canBeFirstSwitch :=
        (p^.oper[1].typ = top_reg) and
        (p^.opsize = S_L) and
        (reg32(p^.oper[1].reg) = reg) and
        (p^.oper[0].typ <> top_ref) and
        ((p^.opcode <> A_SUB) or
         (p^.oper[0].typ = top_const));
    A_SHL:
      canBeFirstSwitch :=
        (p^.opsize = S_L) and
        (p^.oper[1].typ = top_reg) and
        (p^.oper[1].reg = reg) and
        (p^.oper[0].typ = top_const) and
        (p^.oper[0].val in [1,2,3]);
  end;
end;

procedure switchReg(var reg: tregister; reg1, reg2: tregister);
begin
  if reg = reg1 then
    reg := reg2
  else if reg = reg2 then
    reg := reg1
  else if reg = regtoreg8(reg1) then
         reg := regtoreg8(reg2)
  else if reg = regtoreg8(reg2) then
         reg := regtoreg8(reg1)
  else if reg = regtoreg16(reg1) then
         reg := regtoreg16(reg2)
  else if reg = regtoreg16(reg2) then
         reg := regtoreg16(reg1)
end;


procedure switchOp(var op: toper; reg1, reg2: tregister);
begin
  case op.typ of
    top_reg:
      switchReg(op.reg,reg1,reg2);
    top_ref:
      begin
        switchReg(op.ref^.base,reg1,reg2);
        switchReg(op.ref^.index,reg1,reg2);
      end;
  end;
end;

procedure doSwitchReg(hp: paicpu; reg1,reg2: tregister);
var
  opCount: longint;
begin
  for opCount := 0 to hp^.ops-1 do
    switchOp(hp^.oper[opCount],reg1,reg2);
end;


procedure doFirstSwitch(p: paicpu; reg1, reg2: tregister);
var
  tmpRef: treference;
begin
  case p^.opcode of
    A_MOV,A_MOVZX,A_MOVSX,A_LEA:
       begin
         changeOp(p^.oper[1],reg1,reg2);
         changeOp(p^.oper[0],reg2,reg1);
       end;
    A_IMUL:
      begin
        p^.ops := 3;
        p^.loadreg(2,p^.oper[1].reg);
        changeOp(p^.oper[2],reg1,reg2);
      end;
    A_INC,A_DEC:
      begin
        reset_reference(tmpref);
        tmpref.base := reg1;
        case p^.opcode of
          A_INC:
            tmpref.offset := 1;
          A_DEC:
            tmpref.offset := -1;
        end;
        p^.ops := 2;
        p^.opcode := A_LEA;
        p^.loadreg(1,reg2);
        p^.loadref(0,newreference(tmpref));
      end;
    A_SUB,A_ADD:
      begin
        reset_reference(tmpref);
        tmpref.base := reg1;
        case p^.oper[0].typ of
          top_const:
            begin
              tmpref.offset := p^.oper[0].val;
              if p^.opcode = A_SUB then
                tmpref.offset := - tmpRef.offset;
            end;
          top_symbol:
            tmpref.symbol := p^.oper[0].sym;
          top_reg:
            begin
              tmpref.index := p^.oper[0].reg;
              tmpref.scalefactor := 1;
            end;
          else internalerror(200010031);
        end;
        p^.opcode := A_LEA;
        p^.loadref(0,newreference(tmpref));
        p^.loadreg(1,reg2);
      end;
    A_SHL:
      begin
        reset_reference(tmpref);
        tmpref.base := reg1;
        tmpref.scalefactor := 1 shl p^.oper[0].val;
        p^.opcode := A_LEA;
        p^.loadref(0,newreference(tmpref));
        p^.loadreg(1,reg2);
      end;
    else internalerror(200010032);
  end;
end;


function switchRegs(asml: paasmoutput; reg1, reg2: tregister; start: pai): Boolean;
{ change movl  %reg1,%reg2 ... bla ... to ... bla with reg1 and reg2 switched }
var
  endP, hp: pai;
  switchDone, switchLast, tmpResult, sequenceEnd, reg1Modified, reg2Modified: boolean;
  reg1StillUsed, reg2StillUsed, isInstruction: boolean;
begin
  switchRegs := false;
  tmpResult := true;
  sequenceEnd := false;
  reg1Modified := false;
  reg2Modified := false;
  endP := start;
  while tmpResult and not sequenceEnd do
    begin
      tmpResult :=
        getNextInstruction(endP,endP);
      If tmpResult and
         not ppaiprop(endP^.optinfo)^.canBeRemoved then
        begin
          { if the newReg gets stored back to the oldReg, we can change }
          { "mov %oldReg,%newReg; <operations on %newReg>; mov %newReg, }
          { %oldReg" to "<operations on %oldReg>"                       }
          switchLast := storeBack(endP,reg1,reg2);
          reg1StillUsed := reg1 in ppaiprop(endP^.optinfo)^.usedregs;
          reg2StillUsed := reg2 in ppaiprop(endP^.optinfo)^.usedregs;
          isInstruction := endP^.typ = ait_instruction;
          sequenceEnd :=
            switchLast or
            { if both registers are released right before an instruction }
            { that contains hardcoded regs, it's ok too                  }
            (not reg1StillUsed and not reg2StillUsed) or
            { no support for (i)div, mul and imul with hardcoded operands }
            (((not isInstruction) or
              noHardCodedRegs(paicpu(endP),reg1,reg2)) and
             (not reg1StillUsed or
              (isInstruction and findRegDealloc(reg1,endP) and
               regLoadedWithNewValue(reg1,false,paicpu(endP)))) and
             (not reg2StillUsed or
              (isInstruction and findRegDealloc(reg2,endP) and
               regLoadedWithNewValue(reg2,false,paicpu(endP)))));

          { we can't switch reg1 and reg2 in something like }
          {   movl  %reg1,%reg2                             }
          {   movl  (%reg2),%reg2                           }
          {   movl  4(%reg1),%reg1                          }
          if reg2Modified and not(reg1Modified) and
             regReadByInstruction(reg1,endP) then
            begin
              tmpResult := false;
              break
            end;

          if not reg1Modified then
            begin
              reg1Modified := regModifiedByInstruction(reg1,endP);
              if reg1Modified and not canBeFirstSwitch(paicpu(endP),reg1) then
                begin
                  tmpResult := false;
                  break;
                end;
            end;
          if not reg2Modified then
            reg2Modified := regModifiedByInstruction(reg2,endP);

          if sequenceEnd then
            break;

          tmpResult :=
            (endP^.typ <> ait_label) and
            ((not isInstruction) or
             (NoHardCodedRegs(paicpu(endP),reg1,reg2) and
               RegSizesOk(reg1,reg2,paicpu(endP))));
        end;
    end;

  if tmpResult and sequenceEnd then
    begin
      switchRegs := true;
      reg1Modified := false;
      reg2Modified := false;
      getNextInstruction(start,hp);
      while hp <> endP do
        begin
          if (not ppaiprop(hp^.optinfo)^.canberemoved) and
             (hp^.typ = ait_instruction) then
            begin
              switchDone := false;
              if not reg1Modified then
                begin
                  reg1Modified := regModifiedByInstruction(reg1,hp);
                  if reg1Modified then
                    begin
                      doFirstSwitch(paicpu(hp),reg1,reg2);
                      switchDone := true;
                    end;
                end;
              if not switchDone then
                if reg1Modified then
                  doSwitchReg(paicpu(hp),reg1,reg2)
                else
                  doReplaceReg(paicpu(hp),reg2,reg1);
            end;
          getNextInstruction(hp,hp);
        end;
      if switchLast then
        doSwitchReg(paicpu(hp),reg1,reg2)
      else getLastInstruction(hp,hp);
      allocRegBetween(asmL,reg1,start,hp);
      allocRegBetween(asmL,reg2,start,hp);
    end;
end;

procedure doRenaming(asml: paasmoutput; first, last: pai);
var
  p: pai;
begin
  p := First;
  SkipHead(p);
  while p <> last do
    begin
      case p^.typ of
        ait_instruction:
          begin
            case paicpu(p)^.opcode of
              A_MOV:
                begin
                  if not(ppaiprop(p^.optinfo)^.canBeRemoved) and
                     (paicpu(p)^.oper[0].typ = top_reg) and
                     (paicpu(p)^.oper[1].typ = top_reg) and
                     (paicpu(p)^.opsize = S_L) and
                     (paicpu(p)^.oper[0].reg in (usableregs+[R_EDI])) and
                     (paicpu(p)^.oper[1].reg in (usableregs+[R_EDI])) then
                    if switchRegs(asml,paicpu(p)^.oper[0].reg,
                         paicpu(p)^.oper[1].reg,p) then
                      begin
{                        getnextinstruction(p,hp);
                        asmL^.remove(p);
                        dispose(p,done);
                        p := hp;
                        continue }
                        ppaiprop(p^.optinfo)^.canBeRemoved := true;
                      end;
                end;
            end;
          end;
      end;
      getNextInstruction(p,p);
    end;
end;


End.

{
  $Log$
  Revision 1.3  2000-11-29 00:30:51  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.2  2000/11/22 16:30:04  jonas
    * fixed bug where "imul mem32,reg,reg" could be generated

  Revision 1.1  2000/10/24 10:40:54  jonas
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


}
