{
    $Id$
    Copyright (c) 1997-98 by Carl Eric Codere

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
  aasm,
  i386base,
  RAUtils;

{ Parser helpers }
function is_prefix(t:tasmop):boolean;
function is_override(t:tasmop):boolean;
Function CheckPrefix(prefixop,op:tasmop): Boolean;
Function CheckOverride(overrideop,op:tasmop): Boolean;
Procedure InitAsmRef(var instr: TInstruction;operandnum:byte);

{ Operand sizes }
procedure AddReferenceSizes(var instr:TInstruction);
procedure SetInstructionOpsize(var instr:TInstruction);
procedure CheckOperandSizes(var instr:TInstruction);

{ opcode adding }
procedure ConcatInstruction(p : paasmoutput;var instr:TInstruction);


implementation

uses
  globtype,globals,verbose,
  i386asm;


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


Procedure InitAsmRef(var instr: TInstruction;operandnum:byte);
{*********************************************************************}
{  Description: This routine first check if the opcode is of     }
{  type OPR_NONE, or OPR_REFERENCE , if not it gives out an error.    }
{  If the operandtype = OPR_NONE or <> OPR_REFERENCE then it sets up  }
{  the operand type to OPR_REFERENCE, as well as setting up the ref   }
{  to point to the default segment.                                   }
{*********************************************************************}
Begin
  With instr do
  Begin
     case operands[operandnum].operandtype of
       OPR_REFERENCE: exit;
       OPR_NONE: ;
     else
       Message(asmr_e_invalid_operand_type);
     end;
     operands[operandnum].operandtype := OPR_REFERENCE;
     operands[operandnum].ref.segment := R_DEFAULT_SEG;
  end;
end;


{*****************************************************************************
                                Operand Sizes
*****************************************************************************}

procedure AddReferenceSizes(var instr:TInstruction);
{ this will add the sizes for references like [esi] which do not
  have the size set yet, it will take only the size if the other
  operand is a register }
var
  operand2,i : longint;
  s : pasmsymbol;
  so : longint;
begin
  with instr do
   begin
     for i:=1to ops do
      if (operands[i].size=S_NO) then
       begin
         case operands[i].operandtype of
           OPR_REFERENCE :
             begin
               if i=2 then
                operand2:=1
               else
                operand2:=2;
               { Only allow register as operand to take the size from }
               if operands[operand2].operandtype=OPR_REGISTER then
                operands[i].size:=operands[operand2].size
               else
                begin
                  { if no register then take the opsize (which is available with ATT) }
                  operands[i].size:=opsize;
                end;
             end;
           OPR_SYMBOL :
             begin
               { Fix lea which need a reference }
               if opcode=A_LEA then
                begin
                  s:=operands[i].symbol;
                  so:=operands[i].symofs;
                  operands[i].operandtype:=OPR_REFERENCE;
                  reset_reference(operands[i].ref);
                  operands[i].ref.symbol:=s;
                  operands[i].ref.offset:=so;
                end;
               operands[i].size:=S_L;
             end;
         end;
       end;
   end;
end;


procedure SetInstructionOpsize(var instr:TInstruction);
begin
  with instr do
   begin
     if opsize<>S_NO then
      exit;
     case ops of
       0 : ;
       1 :
         opsize:=operands[1].size;
       2 :
         begin
           case opcode of
             A_MOVZX,A_MOVSX :
               begin
                 case operands[1].size of
                   S_W :
                     case operands[2].size of
                       S_L :
                         opsize:=S_WL;
                     end;
                   S_B :
                     case operands[2].size of
                       S_W :
                         opsize:=S_BW;
                       S_L :
                         opsize:=S_BL;
                     end;
                 end;
               end;
             A_IN,A_OUT :
               opsize:=operands[1].size;
             else
               opsize:=operands[2].size;
           end;
         end;
       3 :
         opsize:=operands[3].size;
     end;
   end;
end;


procedure CheckOperandSizes(var instr:TInstruction);
var
  sizeerr : boolean;
  i : longint;
begin
  with instr do
   begin
     { don't check labeled instructions }
     if labeled then
      exit;
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
               sizeerr:=(operands[1].size<>S_B) or (operands[2].size<>S_W);
             S_BL :
               sizeerr:=(operands[1].size<>S_B) or (operands[2].size<>S_L);
             S_WL :
               sizeerr:=(operands[1].size<>S_W) or (operands[2].size<>S_L);
           end;
         end;
      end
     else
      begin
        for i:=1to ops do
         begin
           if (operands[i].operandtype<>OPR_CONSTANT) and
              (operands[i].size<>opsize) then
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
end;


{*****************************************************************************
                              opcode Adding
*****************************************************************************}

procedure ConcatInstruction(p : paasmoutput;var instr:TInstruction);
var
  siz  : topsize;
  i    : longint;
  hlab : plabel;
  ai   : pai386;
begin
  with instr do
   begin
   { Handle a labeled opcode first to see if it needs conversion }
     if labeled then
      begin
        { check if it's a jmp or call to a label, then issue a pai386_labeled }
        if (Ops=1) then
         begin
           case opcode of
             A_CALL,A_JMP,A_Jcc,A_JCXZ, A_JECXZ,
             A_LOOP, A_LOOPE, A_LOOPNE, A_LOOPNZ, A_LOOPZ :
               begin
                 p^.concat(new(pai386_labeled,op_cond_lab(opcode,condition,operands[1].hl)));
                 exit;
               end;
           end;
         end;
        { convert all labinstr to references }
        for i:=1to Ops do
         if operands[i].operandtype=OPR_LABINSTR then
          begin
            hlab:=operands[i].hl;
            operands[i].operandtype:=OPR_REFERENCE;
            reset_reference(operands[i].ref);
            operands[i].ref.symbol:=newasmsymbol(lab2str(hlab));
          end;
      end;

    { Get Opsize }
      if (opsize<>S_NO) or (Ops=0) then
       siz:=opsize
      else
       begin
         if (Ops=2) and (instr.operands[1].operandtype=OPR_REGISTER) then
          siz:=operands[1].size
         else
          siz:=operands[Ops].size;
       end;

     ai:=new(pai386,op_none(opcode,siz));
     ai^.Ops:=Ops;
     for i:=1to Ops do
      begin
        case instr.operands[i].operandtype of
          OPR_CONSTANT :
            ai^.loadconst(i-1,instr.operands[i].val);
          OPR_REGISTER:
            ai^.loadreg(i-1,instr.operands[i].reg);
          OPR_SYMBOL:
            ai^.loadsymbol(i-1,instr.operands[i].symbol,instr.operands[i].symofs);
          OPR_REFERENCE:
            ai^.loadref(i-1,newreference(instr.operands[i].ref));
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
end;

end.
{
  $Log$
  Revision 1.3  1999-05-05 22:21:59  peter
    * updated messages

  Revision 1.2  1999/05/02 14:24:26  peter
    * translate opr_symbol to reference for lea

  Revision 1.1  1999/05/01 13:24:40  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.7  1999/04/26 23:26:16  peter
    * redesigned record offset parsing to support nested records
    * normal compiler uses the redesigned createvarinstr()

  Revision 1.6  1999/04/14 09:07:44  peter
    * asm reader improvements

  Revision 1.5  1999/03/29 16:05:52  peter
    * optimizer working for ag386bin

  Revision 1.4  1999/03/26 00:01:16  peter
    * first things for optimizer (compiles but cycle crashes)

  Revision 1.3  1999/03/06 17:24:25  peter
    * rewritten intel parser a lot, especially reference reading
    * size checking added for asm parsers

  Revision 1.2  1999/03/02 02:56:29  peter
    + stabs support for binary writers
    * more fixes and missing updates from the previous commit :(

  Revision 1.1  1999/03/01 15:46:26  peter
    * ag386bin finally make cycles correct
    * prefixes are now also normal opcodes

}
