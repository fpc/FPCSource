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

procedure doRenaming(asml: TAAsmoutput; first, last: Tai);

Implementation

Uses
  {$ifdef replaceregdebug}cutils,{$endif}
  verbose,globals,cpubase,cpuasm,daopt386,csopt386,cginfo,rgobj;

function canBeFirstSwitch(p: Taicpu; reg: tregister): boolean;
{ checks whether an operation on reg can be switched to another reg without an }
{ additional mov, e.g. "addl $4,%reg1" can be changed to "leal 4(%reg1),%reg2" }
begin
  canBeFirstSwitch := false;
  case p.opcode of
    A_MOV,A_MOVZX,A_MOVSX,A_LEA:
      canBeFirstSwitch :=
        (p.oper[1].typ = top_reg) and
        (reg32(p.oper[1].reg) = reg);
    A_IMUL:
      canBeFirstSwitch :=
        (p.ops >= 2) and
        (reg32(p.oper[p.ops-1].reg) = reg) and
        (p.oper[0].typ <> top_ref) and
        (not pTaiprop(p.optinfo)^.FlagsUsed);
    A_INC,A_DEC,A_SUB,A_ADD:
      canBeFirstSwitch :=
        (p.oper[1].typ = top_reg) and
        (p.opsize = S_L) and
        (reg32(p.oper[1].reg) = reg) and
        (p.oper[0].typ <> top_ref) and
        ((p.opcode <> A_SUB) or
         (p.oper[0].typ = top_const)) and
        (not pTaiprop(p.optinfo)^.FlagsUsed);
    A_SHL:
      canBeFirstSwitch :=
        (p.opsize = S_L) and
        (p.oper[1].typ = top_reg) and
        (p.oper[1].reg = reg) and
        (p.oper[0].typ = top_const) and
        (p.oper[0].val in [1,2,3]) and
        (not pTaiprop(p.optinfo)^.FlagsUsed);
  end;
end;

procedure switchReg(var reg: tregister; reg1, reg2: tregister);
begin
  if reg = reg1 then
    reg := reg2
  else if reg = reg2 then
    reg := reg1
  else if (reg in regset8bit) then
    begin
      if (reg = Changeregsize(reg1,S_B)) then
        reg := Changeregsize(reg2,S_B)
      else if reg = Changeregsize(reg2,S_B) then
        reg := Changeregsize(reg1,S_B);
    end
  else if (reg in regset16bit) then
    begin
      if reg = Changeregsize(reg1,S_W) then
        reg := Changeregsize(reg2,S_W)
      else if reg = Changeregsize(reg2,S_W) then
        reg := Changeregsize(reg1,S_W);
    end;
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

procedure doSwitchReg(hp: Taicpu; reg1,reg2: tregister);
var
  opCount: longint;
begin
  for opCount := 0 to hp.ops-1 do
    switchOp(hp.oper[opCount],reg1,reg2);
end;


procedure doFirstSwitch(p: Taicpu; reg1, reg2: tregister);
var
  tmpRef: treference;
begin
  case p.opcode of
    A_MOV,A_MOVZX,A_MOVSX,A_LEA:
       begin
         changeOp(p.oper[1],reg1,reg2);
         changeOp(p.oper[0],reg2,reg1);
       end;
    A_IMUL:
      begin
        p.ops := 3;
        p.loadreg(2,p.oper[1].reg);
        changeOp(p.oper[2],reg1,reg2);
      end;
    A_INC,A_DEC:
      begin
        reference_reset(tmpref);
        tmpref.base := reg1;
        case p.opcode of
          A_INC:
            tmpref.offset := 1;
          A_DEC:
            tmpref.offset := -1;
        end;
        p.ops := 2;
        p.opcode := A_LEA;
        p.loadreg(1,reg2);
        p.loadref(0,tmpref);
      end;
    A_SUB,A_ADD:
      begin
        reference_reset(tmpref);
        tmpref.base := reg1;
        case p.oper[0].typ of
          top_const:
            begin
              tmpref.offset := p.oper[0].val;
              if p.opcode = A_SUB then
                tmpref.offset := - tmpRef.offset;
            end;
          top_symbol:
            tmpref.symbol := p.oper[0].sym;
          top_reg:
            begin
              tmpref.index := p.oper[0].reg;
              tmpref.scalefactor := 1;
            end;
          else internalerror(200010031);
        end;
        p.opcode := A_LEA;
        p.loadref(0,tmpref);
        p.loadreg(1,reg2);
      end;
    A_SHL:
      begin
        reference_reset(tmpref);
        tmpref.index := reg1;
        tmpref.scalefactor := 1 shl p.oper[0].val;
        p.opcode := A_LEA;
        p.loadref(0,tmpref);
        p.loadreg(1,reg2);
      end;
    else internalerror(200010032);
  end;
end;


function switchRegs(asml: TAAsmoutput; reg1, reg2: tregister; start: Tai): Boolean;
{ change movl  %reg1,%reg2 ... bla ... to ... bla with reg1 and reg2 switched }
var
  endP, hp, lastreg1,lastreg2: Tai;
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
         not pTaiprop(endp.optinfo)^.canBeRemoved then
        begin
          { if the newReg gets stored back to the oldReg, we can change }
          { "mov %oldReg,%newReg; <operations on %newReg>; mov %newReg, }
          { %oldReg" to "<operations on %oldReg>"                       }
          switchLast := storeBack(endP,reg1,reg2);
          reg1StillUsed := reg1 in pTaiprop(endp.optinfo)^.usedregs;
          reg2StillUsed := reg2 in pTaiprop(endp.optinfo)^.usedregs;
          isInstruction := endp.typ = ait_instruction;
          sequenceEnd :=
            switchLast or
            { if both registers are released right before an instruction }
            { that contains hardcoded regs, it's ok too                  }
            (not reg1StillUsed and not reg2StillUsed) or
            { no support for (i)div, mul and imul with hardcoded operands }
            (((not isInstruction) or
              noHardCodedRegs(Taicpu(endP),reg1,reg2)) and
             (not reg1StillUsed or
              (isInstruction and findRegDealloc(reg1,endP) and
               regLoadedWithNewValue(reg1,false,Taicpu(endP)))) and
             (not reg2StillUsed or
              (isInstruction and findRegDealloc(reg2,endP) and
               regLoadedWithNewValue(reg2,false,Taicpu(endP)))));

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
              if reg1Modified and not canBeFirstSwitch(Taicpu(endP),reg1) then
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
            (endp.typ <> ait_label) and
            ((not isInstruction) or
             (NoHardCodedRegs(Taicpu(endP),reg1,reg2) and
              RegSizesOk(reg1,reg2,Taicpu(endP)) and
              (Taicpu(endp).opcode <> A_JMP)));
        end;
    end;

  if tmpResult and sequenceEnd then
    begin
      switchRegs := true;
      reg1Modified := false;
      reg2Modified := false;
      lastreg1 := start;
      lastreg2 := start;
      getNextInstruction(start,hp);
      while hp <> endP do
        begin
          if (not pTaiprop(hp.optinfo)^.canberemoved) and
             (hp.typ = ait_instruction) then
            begin
              switchDone := false;
              if not reg1Modified then
                begin
                  reg1Modified := regModifiedByInstruction(reg1,hp);
                  if reg1Modified then
                    begin
                      doFirstSwitch(Taicpu(hp),reg1,reg2);
                      switchDone := true;
                    end;
                end;
              if not switchDone then
                if reg1Modified then
                  doSwitchReg(Taicpu(hp),reg1,reg2)
                else
                  doReplaceReg(Taicpu(hp),reg2,reg1);
            end;
          if regininstruction(reg1,hp) then
             lastreg1 := hp;
          if regininstruction(reg2,hp) then
             lastreg2 := hp;
          getNextInstruction(hp,hp);
        end;
      if switchLast then
        doSwitchReg(Taicpu(hp),reg1,reg2)
      else getLastInstruction(hp,hp);
      allocRegBetween(asmL,reg1,start,lastreg1);
      allocRegBetween(asmL,reg2,start,lastreg2);
    end;
end;

procedure doRenaming(asml: TAAsmoutput; first, last: Tai);
var
  p: Tai;
begin
  p := First;
  SkipHead(p);
  while p <> last do
    begin
      case p.typ of
        ait_instruction:
          begin
            case Taicpu(p).opcode of
              A_MOV:
                begin
                  if not(pTaiprop(p.optinfo)^.canBeRemoved) and
                     (Taicpu(p).oper[0].typ = top_reg) and
                     (Taicpu(p).oper[1].typ = top_reg) and
                     (Taicpu(p).opsize = S_L) and
                     (Taicpu(p).oper[0].reg in (rg.usableregsint+[R_EDI])) and
                     (Taicpu(p).oper[1].reg in (rg.usableregsint+[R_EDI])) then
                    if switchRegs(asml,Taicpu(p).oper[0].reg,
                         Taicpu(p).oper[1].reg,p) then
                      begin
{                        getnextinstruction(p,hp);
                        asmL^.remove(p);
                        dispose(p,done);
                        p := hp;
                        continue }
                        pTaiprop(p.optinfo)^.canBeRemoved := true;
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
  Revision 1.11  2002-04-15 19:44:22  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.10  2002/04/02 17:11:39  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.9  2002/03/31 20:26:41  jonas
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

  Revision 1.8  2001/10/12 13:55:03  jonas
    * finer granularity for allocation of reused/replaced registers

  Revision 1.7  2001/08/29 14:07:43  jonas
    * the optimizer now keeps track of flags register usage. This fixes some
      optimizer bugs with int64 calculations (because of the carry flag usage)
    * fixed another bug which caused wrong optimizations with complex
      array expressions

  Revision 1.6  2001/01/06 23:35:06  jonas
    * fixed webbug 1323

  Revision 1.5  2000/12/25 00:07:34  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.4  2000/12/05 09:32:47  jonas
    * fixed bug where "shl $1,%reg" was changed to "leal (%reg),%reg2"
      instread of to "leal (,%reg,2),%reg2"

  Revision 1.3  2000/11/29 00:30:51  florian
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
