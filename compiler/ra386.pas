{
    $Id$
    Copyright (c) 1998-2000 by Carl Eric Codere and Peter Vreman

    Handles the common i386 assembler reader routines

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
Unit Ra386;
interface

uses
  aasm,cpubase,RAUtils;

{ Parser helpers }
function is_prefix(t:tasmop):boolean;
function is_override(t:tasmop):boolean;
Function CheckPrefix(prefixop,op:tasmop): Boolean;
Function CheckOverride(overrideop,op:tasmop): Boolean;
Procedure FWaitWarning;

type
  P386Operand=^T386Operand;
  T386Operand=object(TOperand)
  end;

  P386Instruction=^T386Instruction;
  T386Instruction=object(TInstruction)
    { Operand sizes }
    procedure AddReferenceSizes;
    procedure SetInstructionOpsize;
    procedure CheckOperandSizes;
    { opcode adding }
    procedure ConcatInstruction(p : paasmoutput);virtual;
  end;


implementation

uses
  globtype,systems,globals,verbose,cpuasm;


{*****************************************************************************
                              Parser Helpers
*****************************************************************************}

function is_prefix(t:tasmop):boolean;
var
  i : longint;
Begin
  is_prefix:=false;
  for i:=1 to AsmPrefixes do
   if t=AsmPrefix[i-1] then
    begin
      is_prefix:=true;
      exit;
    end;
end;


function is_override(t:tasmop):boolean;
var
  i : longint;
Begin
  is_override:=false;
  for i:=1 to AsmOverrides do
   if t=AsmOverride[i-1] then
    begin
      is_override:=true;
      exit;
    end;
end;


Function CheckPrefix(prefixop,op:tasmop): Boolean;
{ Checks if the prefix is valid with the following opcode }
{ return false if not, otherwise true                          }
Begin
  CheckPrefix := TRUE;
(*  Case prefix of
    A_REP,A_REPNE,A_REPE:
      Case opcode Of
        A_SCASB,A_SCASW,A_SCASD,
        A_INS,A_OUTS,A_MOVS,A_CMPS,A_LODS,A_STOS:;
        Else
          Begin
            CheckPrefix := FALSE;
            exit;
          end;
      end; { case }
    A_LOCK:
      Case opcode Of
        A_BT,A_BTS,A_BTR,A_BTC,A_XCHG,A_ADD,A_OR,A_ADC,A_SBB,A_AND,A_SUB,
        A_XOR,A_NOT,A_NEG,A_INC,A_DEC:;
        Else
          Begin
            CheckPrefix := FALSE;
            Exit;
          end;
      end; { case }
    A_NONE: exit; { no prefix here }
    else
      CheckPrefix := FALSE;
   end; { end case } *)
end;


Function CheckOverride(overrideop,op:tasmop): Boolean;
{ Check if the override is valid, and if so then }
{ update the instr variable accordingly.         }
Begin
  CheckOverride := true;
{     Case instr.getinstruction of
    A_MOVS,A_XLAT,A_CMPS:
      Begin
        CheckOverride := TRUE;
        Message(assem_e_segment_override_not_supported);
      end
  end }
end;


Procedure FWaitWarning;
begin
  if (target_info.target=target_i386_GO32V2) and (cs_fp_emulation in aktmoduleswitches) then
   Message(asmr_w_fwait_emu_prob);
end;


{*****************************************************************************
                              T386Instruction
*****************************************************************************}

procedure T386Instruction.AddReferenceSizes;
{ this will add the sizes for references like [esi] which do not
  have the size set yet, it will take only the size if the other
  operand is a register }
var
  operand2,i : longint;
  s : pasmsymbol;
  so : longint;
begin
  for i:=1to ops do
   if (operands[i]^.size=S_NO) then
    begin
      case operands[i]^.Opr.Typ of
        OPR_REFERENCE :
          begin
            if i=2 then
             operand2:=1
            else
             operand2:=2;
            { Only allow register as operand to take the size from }
            if operands[operand2]^.opr.typ=OPR_REGISTER then
             operands[i]^.size:=operands[operand2]^.size
            else
             begin
               { if no register then take the opsize (which is available with ATT) }
               operands[i]^.size:=opsize;
             end;
          end;
        OPR_SYMBOL :
          begin
            { Fix lea which need a reference }
            if opcode=A_LEA then
             begin
               s:=operands[i]^.opr.symbol;
               so:=operands[i]^.opr.symofs;
               operands[i]^.opr.typ:=OPR_REFERENCE;
               reset_reference(operands[i]^.opr.ref);
               operands[i]^.opr.ref.symbol:=s;
               operands[i]^.opr.ref.offset:=so;
             end;
            operands[i]^.size:=S_L;
          end;
      end;
    end;
end;


procedure T386Instruction.SetInstructionOpsize;
begin
  if opsize<>S_NO then
   exit;
  case ops of
    0 : ;
    1 :
      opsize:=operands[1]^.size;
    2 :
      begin
        case opcode of
          A_MOVZX,A_MOVSX :
            begin
              case operands[1]^.size of
                S_W :
                  case operands[2]^.size of
                    S_L :
                      opsize:=S_WL;
                  end;
                S_B :
                  case operands[2]^.size of
                    S_W :
                      opsize:=S_BW;
                    S_L :
                      opsize:=S_BL;
                  end;
              end;
            end;
          A_OUT :
            opsize:=operands[1]^.size;
          else
            opsize:=operands[2]^.size;
        end;
      end;
    3 :
      opsize:=operands[3]^.size;
  end;
end;


procedure T386Instruction.CheckOperandSizes;
var
  sizeerr : boolean;
  i : longint;
begin
  { Check only the most common opcodes here, the others are done in
    the assembler pass }
  case opcode of
    A_PUSH,A_DEC,A_INC,A_NOT,A_NEG,
    A_CMP,A_MOV,
    A_ADD,A_SUB,A_ADC,A_SBB,
    A_AND,A_OR,A_TEST,A_XOR: ;
  else
    exit;
  end;
  { Handle the BW,BL,WL separatly }
  sizeerr:=false;
  if opsize in [S_BW,S_BL,S_WL] then
   begin
     if ops<>2 then
      sizeerr:=true
     else
      begin
        case opsize of
          S_BW :
            sizeerr:=(operands[1]^.size<>S_B) or (operands[2]^.size<>S_W);
          S_BL :
            sizeerr:=(operands[1]^.size<>S_B) or (operands[2]^.size<>S_L);
          S_WL :
            sizeerr:=(operands[1]^.size<>S_W) or (operands[2]^.size<>S_L);
        end;
      end;
   end
  else
   begin
     for i:=1to ops do
      begin
        if (operands[i]^.opr.typ<>OPR_CONSTANT) and
           (operands[i]^.size in [S_B,S_W,S_L]) and
           (operands[i]^.size<>opsize) then
         sizeerr:=true;
      end;
   end;
  if sizeerr then
   begin
     { if range checks are on then generate an error }
     if (cs_compilesystem in aktmoduleswitches) or
        not (cs_check_range in aktlocalswitches) then
       Message(asmr_w_size_suffix_and_dest_dont_match)
     else
       Message(asmr_e_size_suffix_and_dest_dont_match);
   end;
end;


{*****************************************************************************
                              opcode Adding
*****************************************************************************}

procedure T386Instruction.ConcatInstruction(p : paasmoutput);
var
  siz  : topsize;
  i    : longint;
  ai   : paicpu;
begin
{ Get Opsize }
  if (opsize<>S_NO) or (Ops=0) then
   siz:=opsize
  else
   begin
     if (Ops=2) and (operands[1]^.opr.typ=OPR_REGISTER) then
      siz:=operands[1]^.size
     else
      siz:=operands[Ops]^.size;
   end;

  ai:=new(paicpu,op_none(opcode,siz));
  ai^.Ops:=Ops;
  for i:=1to Ops do
   begin
     case operands[i]^.opr.typ of
       OPR_CONSTANT :
         ai^.loadconst(i-1,operands[i]^.opr.val);
       OPR_REGISTER:
         ai^.loadreg(i-1,operands[i]^.opr.reg);
       OPR_SYMBOL:
         ai^.loadsymbol(i-1,operands[i]^.opr.symbol,operands[i]^.opr.symofs);
       OPR_REFERENCE:
         ai^.loadref(i-1,newreference(operands[i]^.opr.ref));
     end;
   end;

 { Condition ? }
  if condition<>C_None then
   ai^.SetCondition(condition);

 { Concat the opcode or give an error }
  if assigned(ai) then
   p^.concat(ai)
  else
   Message(asmr_e_invalid_opcode_and_operand);
end;

end.
{
  $Log$
  Revision 1.12  2000-02-09 13:23:01  peter
    * log truncated

  Revision 1.11  2000/01/07 01:14:34  peter
    * updated copyright to 2000

  Revision 1.10  1999/12/12 12:59:34  peter
    * only check suffixsize for byte,word,long

  Revision 1.9  1999/08/25 12:00:05  jonas
    * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

  Revision 1.8  1999/08/04 00:23:23  florian
    * renamed i386asm and i386base to cpuasm and cpubase

}
