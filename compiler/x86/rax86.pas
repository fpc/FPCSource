{
    $Id$
    Copyright (c) 1998-2002 by Carl Eric Codere and Peter Vreman

    Handles the common x86 assembler reader routines

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
{
  Contains the common x86 (i386 and x86-64) assembler reader routines.
}
unit rax86;

{$i fpcdefs.inc}

interface

uses
  aasmbase,aasmtai,aasmcpu,
  cpubase,rautils,cclasses;

{ Parser helpers }
function is_prefix(t:tasmop):boolean;
function is_override(t:tasmop):boolean;
Function CheckPrefix(prefixop,op:tasmop): Boolean;
Function CheckOverride(overrideop,op:tasmop): Boolean;
Procedure FWaitWarning;

type
  Tx86Operand=class(TOperand)
    opsize  : topsize;
    Procedure SetSize(_size:longint;force:boolean);override;
    Procedure SetCorrectSize(opcode:tasmop);override;
  end;

  Tx86Instruction=class(TInstruction)
    OpOrder : TOperandOrder;
    opsize  : topsize;
    constructor Create(optype : tcoperand);override;
    { Operand sizes }
    procedure AddReferenceSizes;
    procedure SetInstructionOpsize;
    procedure CheckOperandSizes;
    procedure CheckNonCommutativeOpcodes;
    procedure SwapOperands;
    { opcode adding }
    function ConcatInstruction(p : taasmoutput) : tai;override;
  end;

const
  AsmPrefixes = 6;
  AsmPrefix : array[0..AsmPrefixes-1] of TasmOP =(
    A_LOCK,A_REP,A_REPE,A_REPNE,A_REPNZ,A_REPZ
  );

  AsmOverrides = 6;
  AsmOverride : array[0..AsmOverrides-1] of TasmOP =(
    A_SEGCS,A_SEGES,A_SEGDS,A_SEGFS,A_SEGGS,A_SEGSS
  );

  CondAsmOps=3;
  CondAsmOp:array[0..CondAsmOps-1] of TasmOp=(
    A_CMOVcc, A_Jcc, A_SETcc
  );
  CondAsmOpStr:array[0..CondAsmOps-1] of string[4]=(
    'CMOV','J','SET'
  );

implementation

uses
  globtype,globals,systems,verbose,
  cpuinfo,cgbase,
  itcpugas,cgx86;

{$define ATTOP}
{$define INTELOP}

{$ifdef NORA386INT}
  {$ifdef NOAG386NSM}
    {$ifdef NOAG386INT}
      {$undef INTELOP}
    {$endif}
  {$endif}
{$endif}

{$ifdef NORA386ATT}
  {$ifdef NOAG386ATT}
    {$undef ATTOP}
  {$endif}
{$endif}



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
  if (target_info.system=system_i386_GO32V2) and (cs_fp_emulation in aktmoduleswitches) then
   Message(asmr_w_fwait_emu_prob);
end;

{*****************************************************************************
                              TX86Operand
*****************************************************************************}

Procedure Tx86Operand.SetSize(_size:longint;force:boolean);
begin
  inherited SetSize(_size,force);
  { OS_64 will be set to S_L and be fixed later
    in SetCorrectSize }
  opsize:=TCGSize2Opsize[size];
end;


Procedure Tx86Operand.SetCorrectSize(opcode:tasmop);
begin
  if gas_needsuffix[opcode]=attsufFPU then
    begin
     case size of
       OS_32 : opsize:=S_FS;
       OS_64 : opsize:=S_FL;
     end;
    end
  else if gas_needsuffix[opcode]=attsufFPUint then
    begin
      case size of
        OS_16 : opsize:=S_IS;
        OS_32 : opsize:=S_IL;
        OS_64 : opsize:=S_IQ;
      end;
    end;
end;


{*****************************************************************************
                              T386Instruction
*****************************************************************************}

constructor Tx86Instruction.Create(optype : tcoperand);
begin
  inherited Create(optype);
  Opsize:=S_NO;
end;


procedure Tx86Instruction.SwapOperands;
begin
  Inherited SwapOperands;
  { mark the correct order }
  if OpOrder=op_intel then
    OpOrder:=op_att
  else
    OpOrder:=op_intel;
end;


procedure Tx86Instruction.AddReferenceSizes;
{ this will add the sizes for references like [esi] which do not
  have the size set yet, it will take only the size if the other
  operand is a register }
var
  operand2,i : longint;
  s : tasmsymbol;
  so : aint;
begin
  for i:=1 to ops do
    begin
      operands[i].SetCorrectSize(opcode);
      if tx86operand(operands[i]).opsize=S_NO then
        begin
          case operands[i].Opr.Typ of
            OPR_LOCAL,
            OPR_REFERENCE :
              begin
                if i=2 then
                 operand2:=1
                else
                 operand2:=2;
                if operand2<ops then
                 begin
                   { Only allow register as operand to take the size from }
                   if operands[operand2].opr.typ=OPR_REGISTER then
                     begin
                       if ((opcode<>A_MOVD) and
                           (opcode<>A_CVTSI2SS)) then
                         tx86operand(operands[i]).opsize:=tx86operand(operands[operand2]).opsize;
                     end
                   else
                    begin
                      { if no register then take the opsize (which is available with ATT),
                        if not availble then give an error }
                      if opsize<>S_NO then
                        tx86operand(operands[i]).opsize:=opsize
                      else
                       begin
                         if (m_delphi in aktmodeswitches) then
                           Message(asmr_w_unable_to_determine_reference_size_using_dword)
                         else
                           Message(asmr_e_unable_to_determine_reference_size);
                         { recovery }
                         tx86operand(operands[i]).opsize:=S_L;
                       end;
                    end;
                 end
                else
                 begin
                   if opsize<>S_NO then
                     tx86operand(operands[i]).opsize:=opsize
                 end;
              end;
            OPR_SYMBOL :
              begin
                { Fix lea which need a reference }
                if opcode=A_LEA then
                 begin
                   s:=operands[i].opr.symbol;
                   so:=operands[i].opr.symofs;
                   operands[i].opr.typ:=OPR_REFERENCE;
                   Fillchar(operands[i].opr.ref,sizeof(treference),0);
                   operands[i].opr.ref.symbol:=s;
                   operands[i].opr.ref.offset:=so;
                 end;
{$ifdef x86_64}
                tx86operand(operands[i]).opsize:=S_Q;
{$else x86_64}
                tx86operand(operands[i]).opsize:=S_L;
{$endif x86_64}
              end;
          end;
        end;
    end;
end;


procedure Tx86Instruction.SetInstructionOpsize;
begin
  if opsize<>S_NO then
   exit;
  if (OpOrder=op_intel) then
    SwapOperands;
  case ops of
    0 : ;
    1 :
      begin
        { "push es" must be stored as a long PM }
        if ((opcode=A_PUSH) or
            (opcode=A_POP)) and
           (operands[1].opr.typ=OPR_REGISTER) and
           is_segment_reg(operands[1].opr.reg) then
          opsize:=S_L
        else
          opsize:=tx86operand(operands[1]).opsize;
      end;
    2 :
      begin
        case opcode of
          A_MOVZX,A_MOVSX :
            begin
              case tx86operand(operands[1]).opsize of
                S_W :
                  case tx86operand(operands[2]).opsize of
                    S_L :
                      opsize:=S_WL;
                  end;
                S_B :
                  case tx86operand(operands[2]).opsize of
                    S_W :
                      opsize:=S_BW;
                    S_L :
                      opsize:=S_BL;
                  end;
              end;
            end;
          A_MOVD : { movd is a move from a mmx register to a
                     32 bit register or memory, so no opsize is correct here PM }
            exit;
          A_OUT :
            opsize:=tx86operand(operands[1]).opsize;
          else
            opsize:=tx86operand(operands[2]).opsize;
        end;
      end;
    3 :
      opsize:=tx86operand(operands[3]).opsize;
  end;
end;


procedure Tx86Instruction.CheckOperandSizes;
var
  sizeerr : boolean;
  i : longint;
begin
  { Check only the most common opcodes here, the others are done in
    the assembler pass }
  case opcode of
    A_PUSH,A_POP,A_DEC,A_INC,A_NOT,A_NEG,
    A_CMP,A_MOV,
    A_ADD,A_SUB,A_ADC,A_SBB,
    A_AND,A_OR,A_TEST,A_XOR: ;
  else
    exit;
  end;
  { Handle the BW,BL,WL separatly }
  sizeerr:=false;
  { special push/pop selector case }
  if ((opcode=A_PUSH) or
      (opcode=A_POP)) and
     (operands[1].opr.typ=OPR_REGISTER) and
     is_segment_reg(operands[1].opr.reg) then
    exit;
  if opsize in [S_BW,S_BL,S_WL] then
   begin
     if ops<>2 then
      sizeerr:=true
     else
      begin
        case opsize of
          S_BW :
            sizeerr:=(tx86operand(operands[1]).opsize<>S_B) or (tx86operand(operands[2]).opsize<>S_W);
          S_BL :
            sizeerr:=(tx86operand(operands[1]).opsize<>S_B) or (tx86operand(operands[2]).opsize<>S_L);
          S_WL :
            sizeerr:=(tx86operand(operands[1]).opsize<>S_W) or (tx86operand(operands[2]).opsize<>S_L);
        end;
      end;
   end
  else
   begin
     for i:=1 to ops do
      begin
        if (operands[i].opr.typ<>OPR_CONSTANT) and
           (tx86operand(operands[i]).opsize in [S_B,S_W,S_L]) and
           (tx86operand(operands[i]).opsize<>opsize) then
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


{ This check must be done with the operand in ATT order
  i.e.after swapping in the intel reader
  but before swapping in the NASM and TASM writers PM }
procedure Tx86Instruction.CheckNonCommutativeOpcodes;
begin
  if (OpOrder=op_intel) then
    SwapOperands;
  if (
      (ops=2) and
      (operands[1].opr.typ=OPR_REGISTER) and
      (operands[2].opr.typ=OPR_REGISTER) and
      { if the first is ST and the second is also a register
        it is necessarily ST1 .. ST7 }
      ((operands[1].opr.reg=NR_ST) or
       (operands[1].opr.reg=NR_ST0))
     ) or
     (ops=0) then
      if opcode=A_FSUBR then
        opcode:=A_FSUB
      else if opcode=A_FSUB then
        opcode:=A_FSUBR
      else if opcode=A_FDIVR then
        opcode:=A_FDIV
      else if opcode=A_FDIV then
        opcode:=A_FDIVR
      else if opcode=A_FSUBRP then
        opcode:=A_FSUBP
      else if opcode=A_FSUBP then
        opcode:=A_FSUBRP
      else if opcode=A_FDIVRP then
        opcode:=A_FDIVP
      else if opcode=A_FDIVP then
        opcode:=A_FDIVRP;
  if  (
       (ops=1) and
       (operands[1].opr.typ=OPR_REGISTER) and
       (getregtype(operands[1].opr.reg)=R_FPUREGISTER) and
       (operands[1].opr.reg<>NR_ST) and
       (operands[1].opr.reg<>NR_ST0)
      ) then
      if opcode=A_FSUBRP then
        opcode:=A_FSUBP
      else if opcode=A_FSUBP then
        opcode:=A_FSUBRP
      else if opcode=A_FDIVRP then
        opcode:=A_FDIVP
      else if opcode=A_FDIVP then
        opcode:=A_FDIVRP;
end;

{*****************************************************************************
                              opcode Adding
*****************************************************************************}

function Tx86Instruction.ConcatInstruction(p : taasmoutput) : tai;
var
  siz  : topsize;
  i,asize : longint;
  ai   : taicpu;
begin
  if (OpOrder=op_intel) then
    SwapOperands;

{ Get Opsize }
  if (opsize<>S_NO) or (Ops=0) then
   siz:=opsize
  else
   begin
     if (Ops=2) and (operands[1].opr.typ=OPR_REGISTER) then
      siz:=tx86operand(operands[1]).opsize
     else
      siz:=tx86operand(operands[Ops]).opsize;
     { MOVD should be of size S_LQ or S_QL, but these do not exist PM }
     if (ops=2) and
        (tx86operand(operands[1]).opsize<>S_NO) and
        (tx86operand(operands[2]).opsize<>S_NO) and
        (tx86operand(operands[1]).opsize<>tx86operand(operands[2]).opsize) then
       siz:=S_NO;
   end;

   if ((opcode=A_MOVD)or
       (opcode=A_CVTSI2SS)) and
      ((tx86operand(operands[1]).opsize=S_NO) or
       (tx86operand(operands[2]).opsize=S_NO)) then
     siz:=S_NO;
   { NASM does not support FADD without args
     as alias of FADDP
     and GNU AS interprets FADD without operand differently
     for version 2.9.1 and 2.9.5 !! }
   if (ops=0) and
      ((opcode=A_FADD) or
       (opcode=A_FMUL) or
       (opcode=A_FSUB) or
       (opcode=A_FSUBR) or
       (opcode=A_FDIV) or
       (opcode=A_FDIVR)) then
     begin
       if opcode=A_FADD then
         opcode:=A_FADDP
       else if opcode=A_FMUL then
         opcode:=A_FMULP
       else if opcode=A_FSUB then
         opcode:=A_FSUBP
       else if opcode=A_FSUBR then
         opcode:=A_FSUBRP
       else if opcode=A_FDIV then
         opcode:=A_FDIVP
       else if opcode=A_FDIVR then
         opcode:=A_FDIVRP;
{$ifdef ATTOP}
       message1(asmr_w_fadd_to_faddp,gas_op2str[opcode]);
{$else}
  {$ifdef INTELOP}
       message1(asmr_w_fadd_to_faddp,std_op2str[opcode]);
  {$else}
       message1(asmr_w_fadd_to_faddp,'fXX');
  {$endif INTELOP}
{$endif ATTOP}
     end;

   { GNU AS interprets FDIV without operand differently
     for version 2.9.1 and 2.10
     we add explicit args to it !! }
  if (ops=0) and
     ((opcode=A_FSUBP) or
      (opcode=A_FSUBRP) or
      (opcode=A_FDIVP) or
      (opcode=A_FDIVRP) or
      (opcode=A_FSUB) or
      (opcode=A_FSUBR) or
      (opcode=A_FADD) or
      (opcode=A_FADDP) or
      (opcode=A_FDIV) or
      (opcode=A_FDIVR)) then
     begin
{$ifdef ATTOP}
       message1(asmr_w_adding_explicit_args_fXX,gas_op2str[opcode]);
{$else}
  {$ifdef INTELOP}
       message1(asmr_w_adding_explicit_args_fXX,std_op2str[opcode]);
  {$else}
       message1(asmr_w_adding_explicit_args_fXX,'fXX');
  {$endif INTELOP}
{$endif ATTOP}
       ops:=2;
       operands[1].opr.typ:=OPR_REGISTER;
       operands[2].opr.typ:=OPR_REGISTER;
       operands[1].opr.reg:=NR_ST0;
       operands[2].opr.reg:=NR_ST1;
     end;
  if (ops=1) and
     (
      (operands[1].opr.typ=OPR_REGISTER) and
      (getregtype(operands[1].opr.reg)=R_FPUREGISTER) and
      (operands[1].opr.reg<>NR_ST) and
      (operands[1].opr.reg<>NR_ST0)
     ) and
     (
      (opcode=A_FSUBP) or
      (opcode=A_FSUBRP) or
      (opcode=A_FDIVP) or
      (opcode=A_FDIVRP) or
      (opcode=A_FADDP) or
      (opcode=A_FMULP)
     ) then
     begin
{$ifdef ATTOP}
       message1(asmr_w_adding_explicit_first_arg_fXX,gas_op2str[opcode]);
{$else}
  {$ifdef INTELOP}
       message1(asmr_w_adding_explicit_first_arg_fXX,std_op2str[opcode]);
  {$else}
       message1(asmr_w_adding_explicit_first_arg_fXX,'fXX');
  {$endif INTELOP}
{$endif ATTOP}
       ops:=2;
       operands[2].opr.typ:=OPR_REGISTER;
       operands[2].opr.reg:=operands[1].opr.reg;
       operands[1].opr.reg:=NR_ST0;
     end;

  if (ops=1) and
     (
      (operands[1].opr.typ=OPR_REGISTER) and
      (getregtype(operands[1].opr.reg)=R_FPUREGISTER) and
      (operands[1].opr.reg<>NR_ST) and
      (operands[1].opr.reg<>NR_ST0)
     ) and
     (
      (opcode=A_FSUB) or
      (opcode=A_FSUBR) or
      (opcode=A_FDIV) or
      (opcode=A_FDIVR) or
      (opcode=A_FADD) or
      (opcode=A_FMUL)
     ) then
     begin
{$ifdef ATTOP}
       message1(asmr_w_adding_explicit_second_arg_fXX,gas_op2str[opcode]);
{$else}
  {$ifdef INTELOP}
       message1(asmr_w_adding_explicit_second_arg_fXX,std_op2str[opcode]);
  {$else}
       message1(asmr_w_adding_explicit_second_arg_fXX,'fXX');
  {$endif INTELOP}
{$endif ATTOP}
       ops:=2;
       operands[2].opr.typ:=OPR_REGISTER;
       operands[2].opr.reg:=NR_ST0;
     end;

   { I tried to convince Linus Torvalds to add
     code to support ENTER instruction
     (when raising a stack page fault)
     but he replied that ENTER is a bad instruction and
     Linux does not need to support it
     So I think its at least a good idea to add a warning
     if someone uses this in assembler code
     FPC itself does not use it at all PM }
   if (opcode=A_ENTER) and
      (target_info.system in [system_i386_linux,system_i386_FreeBSD]) then
     Message(asmr_w_enter_not_supported_by_linux);

  ai:=taicpu.op_none(opcode,siz);
  ai.SetOperandOrder(OpOrder);
  ai.Ops:=Ops;
  ai.Allocate_oper(Ops);
  for i:=1 to Ops do
   begin
     case operands[i].opr.typ of
       OPR_CONSTANT :
         ai.loadconst(i-1,operands[i].opr.val);
       OPR_REGISTER:
         ai.loadreg(i-1,operands[i].opr.reg);
       OPR_SYMBOL:
         ai.loadsymbol(i-1,operands[i].opr.symbol,operands[i].opr.symofs);
       OPR_LOCAL :
         ai.loadlocal(i-1,operands[i].opr.localsym,operands[i].opr.localsymofs,operands[i].opr.localindexreg,
                      operands[i].opr.localscale,operands[i].opr.localgetoffset);
       OPR_REFERENCE:
         begin
           ai.loadref(i-1,operands[i].opr.ref);
           if operands[i].size<>OS_NO then
             begin
               asize:=0;
               case operands[i].size of
                   OS_8,OS_S8 :
                     asize:=OT_BITS8;
                   OS_16,OS_S16 :
                     asize:=OT_BITS16;
                   OS_32,OS_S32,OS_F32 :
                     asize:=OT_BITS32;
                   OS_64,OS_S64:
                     begin
                       { Only FPU operations know about 64bit values, for all
                         integer operations it is seen as 32bit }
                       if gas_needsuffix[opcode] in [attsufFPU,attsufFPUint] then
                         asize:=OT_BITS64
                       else
                         asize:=OT_BITS32;
                     end;
                   OS_F64,OS_C64 :
                     asize:=OT_BITS64;
                   OS_F80 :
                     asize:=OT_BITS80;
                 end;
               if asize<>0 then
                 ai.oper[i-1]^.ot:=(ai.oper[i-1]^.ot and not OT_SIZE_MASK) or asize;
             end;
         end;
     end;
   end;

  if (opcode=A_CALL) and (opsize=S_FAR) then
    opcode:=A_LCALL;
  if (opcode=A_JMP) and (opsize=S_FAR) then
    opcode:=A_LJMP;
  if (opcode=A_LCALL) or (opcode=A_LJMP) then
    opsize:=S_FAR;
 { Condition ? }
  if condition<>C_None then
   ai.SetCondition(condition);

 { Concat the opcode or give an error }
  if assigned(ai) then
   begin
     { Check the instruction if it's valid }
{$ifndef NOAG386BIN}
{$ifndef x86_64}
     ai.CheckIfValid;
{$endif x86_64}
{$endif NOAG386BIN}
     p.concat(ai);
   end
  else
   Message(asmr_e_invalid_opcode_and_operand);
  result:=ai;
end;

end.
{
  $Log$
  Revision 1.18  2004-06-16 20:07:11  florian
    * dwarf branch merged

  Revision 1.17.2.3  2004/05/01 23:36:47  peter
    * assembler reader 64bit fixes

  Revision 1.17.2.2  2004/05/01 16:02:10  peter
    * POINTER_SIZE replaced with sizeof(aint)
    * aint,aword,tconst*int moved to globtype

  Revision 1.17.2.1  2004/04/27 18:18:26  peter
    * aword -> aint

  Revision 1.17  2004/01/22 16:29:11  peter
    * give warning that DWORD is used as size in delphi mode when no
      size was specified

  Revision 1.16  2004/01/14 23:39:05  florian
    * another bunch of x86-64 fixes mainly calling convention and
      assembler reader related

  Revision 1.15  2003/11/17 23:23:47  florian
    + first part of arm assembler reader

  Revision 1.14  2003/11/12 16:05:40  florian
    * assembler readers OOPed
    + typed currency constants
    + typed 128 bit float constants if the CPU supports it

  Revision 1.13  2003/10/30 19:59:00  peter
    * support scalefactor for opr_local
    * support reference with opr_local set, fixes tw2631

  Revision 1.12  2003/10/29 15:40:20  peter
    * support indexing and offset retrieval for locals

  Revision 1.11  2003/10/21 15:15:36  peter
    * taicpu_abstract.oper[] changed to pointers

  Revision 1.10  2003/10/01 20:34:51  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.9  2003/09/23 17:56:06  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.8  2003/09/03 15:55:02  peter
    * NEWRA branch merged

  Revision 1.7.2.4  2003/08/31 16:18:05  peter
    * more fixes

  Revision 1.7.2.3  2003/08/29 17:29:00  peter
    * next batch of updates

  Revision 1.7.2.2  2003/08/28 18:35:08  peter
    * tregister changed to cardinal

  Revision 1.7.2.1  2003/08/27 21:06:34  peter
    * more updates

  Revision 1.7  2003/08/26 12:42:45  peter
    * fix wrong registers in reference

  Revision 1.6  2003/06/20 12:57:15  pierre
   * fix a bug preventing correct reading of intel 'mov [edi],al'

  Revision 1.5  2003/06/07 10:23:50  peter
    * 32bit operands need ofcourse 32bit size

  Revision 1.4  2003/05/31 16:22:28  peter
    * fixed opsize and operand size setting for 64bit values

  Revision 1.3  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.2  2003/05/22 21:33:31  peter
    * removed some unit dependencies

  Revision 1.1  2003/04/30 15:45:35  florian
    * merged more x86-64/i386 code

  Revision 1.30  2003/04/25 12:04:31  florian
    * merged agx64att and ag386att to x86/agx86att

  Revision 1.29  2003/02/19 22:00:16  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.28  2003/02/03 22:47:14  daniel
    - Removed reg_2_opsize array

  Revision 1.27  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.26  2002/11/15 01:58:58  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.25  2002/10/31 13:28:32  pierre
   * correct last wrong fix for tw2158

  Revision 1.24  2002/10/30 17:10:00  pierre
   * merge of fix for tw2158 bug

  Revision 1.23  2002/07/26 21:15:44  florian
    * rewrote the system handling

  Revision 1.22  2002/07/01 18:46:34  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.21  2002/05/18 13:34:25  peter
    * readded missing revisions

  Revision 1.20  2002/05/16 19:46:52  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.18  2002/05/12 16:53:18  peter
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

  Revision 1.17  2002/04/15 19:12:09  carl
  + target_info.size_of_pointer -> sizeof(aint)
  + some cleanup of unused types/variables
  * move several constants from cpubase to their specific units
    (where they are used)
  + att_Reg2str -> gas_reg2str
  + int_reg2str -> std_reg2str

  Revision 1.16  2002/04/04 19:06:13  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.15  2002/04/02 17:11:39  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.14  2002/01/24 18:25:53  peter
   * implicit result variable generation for assembler routines
   * removed m_tp modeswitch, use m_tp7 or not(m_fpc) instead

}
