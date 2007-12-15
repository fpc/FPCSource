{
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
  aasmbase,aasmtai,aasmdata,aasmcpu,
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
    Function CheckOperand: boolean; override;
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
    function ConcatInstruction(p : TAsmList) : tai;override;
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
  procinfo,
  cpuinfo,cgbase,cgutils,
  itcpugas,cgx86;


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
  if (target_info.system=system_i386_GO32V2) and (cs_fp_emulation in current_settings.moduleswitches) then
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

Function Tx86Operand.CheckOperand: boolean;

begin
  result:=true;
  if (opr.typ=OPR_Reference) then
    begin
      if not hasvar then
        begin
          if (getsupreg(opr.ref.base)=RS_EBP) and (opr.ref.offset>0) then
            begin
              if current_procinfo.procdef.proccalloption=pocall_register then
                message(asmr_w_no_direct_ebp_for_parameter)
              else
                message(asmr_w_direct_ebp_for_parameter_regcall);
            end
          else if (getsupreg(opr.ref.base)=RS_EBP) and (opr.ref.offset<0) then
            message(asmr_w_direct_ebp_neg_offset)
          else if (getsupreg(opr.ref.base)=RS_ESP) and (opr.ref.offset<0) then
            message(asmr_w_direct_esp_neg_offset);
        end;
      if (cs_create_pic in current_settings.moduleswitches) and
         assigned(opr.ref.symbol) and
         not assigned(opr.ref.relsymbol) and
         (opr.ref.refaddr<>addr_pic) then
        begin
          message(asmr_e_need_pic_ref);
          result:=false;
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
{$ifdef x86_64}
          if (opcode=A_MOVQ) and
             (ops=2) and
             (operands[1].opr.typ=OPR_CONSTANT) then
             opsize:=S_Q
          else
{$endif x86_64}
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
                           if (m_delphi in current_settings.modeswitches) then
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
              if tx86operand(operands[1]).opsize=S_NO then
                begin
                  tx86operand(operands[1]).opsize:=S_B;
                  if (m_delphi in current_settings.modeswitches) then
                    Message(asmr_w_unable_to_determine_reference_size_using_byte)
                  else
                    Message(asmr_e_unable_to_determine_reference_size);
                end;
              case tx86operand(operands[1]).opsize of
                S_W :
                  case tx86operand(operands[2]).opsize of
                    S_L :
                      opsize:=S_WL;
                  end;
                S_B :
                  begin
                    case tx86operand(operands[2]).opsize of
                      S_W :
                        opsize:=S_BW;
                      S_L :
                        opsize:=S_BL;
                    end;
                  end;
              end;
            end;
          A_MOVD : { movd is a move from a mmx register to a
                     32 bit register or memory, so no opsize is correct here PM }
            exit;
          A_MOVQ :
            opsize:=S_IQ;
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
     if (cs_compilesystem in current_settings.moduleswitches) or
        not (cs_check_range in current_settings.localswitches) then
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

function Tx86Instruction.ConcatInstruction(p : TAsmList) : tai;
var
  siz  : topsize;
  i,asize : longint;
  ai   : taicpu;
begin
  if (OpOrder=op_intel) then
    SwapOperands;

  ai:=nil;
  for i:=1 to Ops do
    if not operands[i].CheckOperand then
      exit;

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
       message1(asmr_w_fadd_to_faddp,std_op2str[opcode]);
     end;

  {It is valid to specify some instructions without operand size.}
  if siz=S_NO then
    begin
      if (ops=1) and (opcode=A_INT) then
        siz:=S_B;
      if (ops=1) and (opcode=A_RET) or (opcode=A_RETN) or (opcode=A_RETF) then
        siz:=S_W;
      if (ops=1) and (opcode=A_PUSH) then
        begin
          {We are a 32 compiler, assume 32-bit by default. This is Delphi
           compatible but bad coding practise.}
          siz:=S_L;
          message(asmr_w_unable_to_determine_reference_size_using_dword);
        end;
      if (opcode=A_JMP) or (opcode=A_JCC) or (opcode=A_CALL) then
        if ops=1 then
          siz:=S_NEAR
        else
          siz:=S_FAR;
    end;

{$ifdef x86_64}
  { Convert movq with at least one general registers or constant to a mov instruction }
  if (opcode=A_MOVQ) and
     (ops=2) and
     (
      (operands[1].opr.typ=OPR_REGISTER) or
      (operands[2].opr.typ=OPR_REGISTER) or
      (operands[1].opr.typ=OPR_CONSTANT)
     ) then
     opcode:=A_MOV;
{$endif x86_64}

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
       message1(asmr_w_adding_explicit_args_fXX,std_op2str[opcode]);
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
       message1(asmr_w_adding_explicit_first_arg_fXX,std_op2str[opcode]);
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
       message1(asmr_w_adding_explicit_second_arg_fXX,std_op2str[opcode]);
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
    case operands[i].opr.typ of
       OPR_CONSTANT :
         ai.loadconst(i-1,operands[i].opr.val);
       OPR_REGISTER:
         ai.loadreg(i-1,operands[i].opr.reg);
       OPR_SYMBOL:
         ai.loadsymbol(i-1,operands[i].opr.symbol,operands[i].opr.symofs);
       OPR_LOCAL :
         with operands[i].opr do
           ai.loadlocal(i-1,localsym,localsymofs,localindexreg,
                        localscale,localgetoffset,localforceref);
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

 { Condition ? }
  if condition<>C_None then
   ai.SetCondition(condition);

 { Concat the opcode or give an error }
  if assigned(ai) then
    p.concat(ai)
  else
   Message(asmr_e_invalid_opcode_and_operand);
  result:=ai;
end;

end.
