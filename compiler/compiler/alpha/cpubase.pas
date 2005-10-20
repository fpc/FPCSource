{
    Copyright (C) 1998-2000 by Florian Klaempfl

    This unit implements an asmlistitem class for the Alpha architecture.

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
  This unit implements an asmlistitem class for the Alpha architecture.
}
unit cpubase;

{$i fpcdefs.inc}

  interface

    uses
       cutils,cclasses,globals,aasmbase,cpuinfo,cginfo;

    type
       { all registers }
       TRegister = (R_NO,  { R_NO is Mandatory, signifies no register }
                    R_0,R_1,R_2,R_3,R_4,R_5,R_6,R_7,R_8,R_9,
                    R_10,R_11,R_12,R_13,R_14,R_15,R_16,R_17,R_18,R_19,
                    R_20,R_21,R_22,R_23,R_24,R_25,R_26,R_27,R_28,R_29,
                    R_30,R_31,
                    R_F0,R_F1,R_F2,R_F3,R_F4,R_F5,R_F6,R_F7,R_F8,R_F9,
                    R_F10,R_F11,R_F12,R_F13,R_F14,R_F15,R_F16,R_F17,R_F18,R_F19,
                    R_F20,R_F21,R_F22,R_F23,R_F24,R_F25,R_F26,R_F27,R_F28,R_F29,
                    R_F30,R_F31);

       tasmop = (A_ADDF,A_ADDG,A_ADDL,A_ADDQ,
                 A_ADDS,A_ADDT,A_AMASK,A_AND,A_BEQ,A_BGE,
                 A_BGT,A_BIC,A_BIS,A_BLBC,A_BLBS,A_BLE,
                 A_BLT,A_BNE,A_BR,A_BSR,A_CALL_PAL,A_CMOVEQ,
                 A_CMOVGE,A_CMOVGT,A_CMOVLBC,A_CMOVLBS,A_CMOVLE,A_CMOVLT,
                 A_CMOVNE,A_CMPBGE,A_CMPEQ,A_CMPGEQ,A_CMPGLE,A_CMPGLT,
                 A_CMPLE,A_CMPLT,A_CMPTEQ,A_CMPTLE,A_CMPTLT,A_CMPTUN,
                 A_CMPULE,A_CMPULT,A_CPYS,A_CPYSE,A_CPYSN,A_CTLZ,
                 A_CTPOP,A_CTTZ,A_CVTDG,A_CVTGD,A_CVTGF,A_CVTGQ,
                 A_CVTLQ,A_CVTQF,A_CVTQG,A_CVTQL,A_CVTQS,A_CVTQT,
                 A_CVTST,A_CVTTQ,A_CVTTS,A_DIVF,A_DIVG,A_DIVS,
                 A_DIVT,A_ECB,A_EQV,A_EXCB,A_EXTBL,A_EXTLH,
                 A_EXTLL,A_EXTQH,A_EXTQL,A_EXTWH,A_EXTWL,A_FBEQ,
                 A_FBGE,A_FBGT,A_FBLE,A_FBLT,A_FBNE,A_FCMOVEQ,
                 A_FCMOVGE,A_FCMOVGT,A_FCMOVLE,A_FCMOVLT,A_FCMOVNE,A_FETCH,
                 A_FETCH_M,A_FTOIS,A_FTOIT,A_IMPLVER,A_INSBL,A_INSLH,
                 A_INSLL,A_INSQH,A_INSQL,A_INSWH,A_INSWL,A_ITOFF,
                 A_ITOFS,A_ITOFT,A_JMP,A_JSR,A_JSR_COROUTINE,A_LDA,
                 A_LDAH,A_LDBU,A_LDWU,A_LDF,A_LDG,A_LDL,
                 A_LDL_L,A_LDQ,A_LDQ_L,A_LDQ_U,A_LDS,A_LDT,
                 A_MAXSB8,A_MAXSW4,A_MAXUB8,A_MAXUW4,A_MB,A_MF_FPCR,
                 A_MINSB8,A_MINSW4,A_MINUB8,A_MINUW4,A_MSKBL,A_MSKLH,
                 A_MSKLL,A_MSKQH,A_MSKQL,A_MSKWH,A_MSKWL,A_MT_FPCR,
                 A_MULF,A_MULG,A_MULL,A_MULQ,
                 A_MULS,A_MULT,A_ORNOT,A_PERR,A_PKLB,A_PKWB,
                 A_RC,A_RET,A_RPCC,A_RS,A_S4ADDL,A_S4ADDQ,
                 A_S4SUBL,A_S4SUBQ,A_S8ADDL,A_S8ADDQ,A_S8SUBL,A_S8SUBQ,
                 A_SEXTB,A_SEXTW,A_SLL,A_SQRTF,A_SQRTG,A_SQRTS,
                 A_SQRTT,A_SRA,A_SRL,A_STB,A_STF,A_STG,
                 A_STS,A_STL,A_STL_C,A_STQ,A_STQ_C,A_STQ_U,
                 A_STT,A_STW,A_SUBF,A_SUBG,A_SUBL,
                 A_SUBQ,A_SUBS,A_SUBT,A_TRAPB,A_UMULH,
                 A_UNPKBL,A_UNPKBW,A_WH64,A_WMB,A_XOR,A_ZAP,
                 A_ZAPNOT
                 { Psuedo code understood by the gnu assembler }
                 ,A_LDGP);

    const
       firstop = low(tasmop);
       lastop  = high(tasmop);

       std_reg2str : array[tregister] of string[4] = (
         '',
         '','','','','','','','','','',
         '','','','','','','','','','',
         '','','','','','','','','','',
         '','',
         '','','','','','','','','','',
         '','','','','','','','','','',
         '','','','','','','','','','',
         '',''
       );


    type
       TAsmCond =
        (
         C_None,C_A,C_AE,C_B,C_BE,C_C,C_E,C_G,C_GE,C_L,C_LE,C_NA,C_NAE,
         C_NB,C_NBE,C_NC,C_NE,C_NG,C_NGE,C_NL,C_NLE,C_NO,C_NP,C_NS,C_NZ,C_O,C_P,
         C_PE,C_PO,C_S,C_Z
        );

        TRegisterset = Set of TRegister;

        tregister64 = tregister;

    Const
       Firstreg = R_0;
       LastReg = R_F31;


{*****************************************************************************
                          Default generic sizes
*****************************************************************************}

       { Defines the default address size for a processor, }
       OS_ADDR = OS_64;
       { the natural int size for a processor,             }
       OS_INT = OS_64;
       { the maximum float size for a processor,           }
       OS_FLOAT = OS_F80;
       { the size of a vector register for a processor     }
       OS_VECTOR = OS_M64;

       stack_pointer_reg = R_30;
       frame_pointer_reg = R_15;
       self_pointer_reg = R_16;
       accumulator   = R_0;
  {the return_result_reg, is used inside the called function to store its return
  value when that is a scalar value otherwise a pointer to the address of the
  result is placed inside it}
        return_result_reg               =       accumulator;

  {the function_result_reg contains the function result after a call to a scalar
  function othewise it contains a pointer to the returned result}
        function_result_reg     =       accumulator;
       fpu_result_reg = R_F0;
       global_pointer = R_29;
       return_pointer = R_26;
       { it is used to pass the offset to the destructor helper routine }
       vmt_offset_reg = R_1;

     { low and high of the available maximum width integer general purpose }
     { registers                                                           }
       LoGPReg = R_0;
       HiGPReg = R_31;

       { low and high of every possible width general purpose register (same as
         above on most architctures apart from the 80x86)                       }
       LoReg = R_0;
       HiReg = R_31;

       { Constant defining possibly all registers which might require saving }
       ALL_REGISTERS = [firstreg..lastreg];

       general_registers = [R_0..R_31];

       availabletempregsint = [R_0..R_14,R_16..R_25,R_28];
       availabletempregsfpu = [R_F0..R_F30];
       availabletempregsmm  = [];

       intregs = [R_0..R_31];
       usableregsint = [];
       c_countusableregsint = 26;

       maxfpuregs = 32;
       fpuregs = [R_F0..R_F31];
       usableregsfpu = [];
       c_countusableregsfpu = 31;

       mmregs = [];
       usableregsmm = [];
       c_countusableregsmm  = 0;

       max_operands = 4;

       registers_saved_on_cdecl = [R_9..R_14,R_F2..R_F9];

       firstsaveintreg = R_NO;
       lastsaveintreg  = R_NO;
       firstsavefpureg = R_NO;
       lastsavefpureg  = R_NO;
       firstsavemmreg  = R_NO;
       lastsavemmreg   = R_NO;
       maxvarregs = 6;

       varregs : Array [1..maxvarregs] of Tregister =
                 (R_9,R_10,R_11,R_12,R_13,R_14);

       maxfpuvarregs = 8;

       { Registers which are defined as scratch and no need to save across
         routine calls or in assembler blocks.
       }
       max_scratch_regs = 2;
       scratch_regs : array[1..max_scratch_regs] of tregister = (R_1,R_2);

{*****************************************************************************
                               GDB Information
*****************************************************************************}

       {  Register indexes for stabs information, when some
         parameters or variables are stored in registers.
       }
       stab_regindex : array[tregister] of shortint =
          (0,
           0,0,0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,0,0,
           0,0,
           0,0,0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,0,0,
           0,0
          );

{*****************************************************************************
                                   Flags
*****************************************************************************}
       type
       { The Alpha doesn't have flags but some generic code depends on this type. }
       TResFlags = (F_NO);


       { reference record }
       pparareference = ^tparareference;
       tparareference = packed record
          index       : tregister;
          offset      : longint;
       end;

       trefoptions=(ref_none,ref_parafixup,ref_localfixup,ref_selffixup);

       TReference = record
         offset : aword;
         symbol : tasmsymbol;
         base : tregister;
         { The index isn't used by the alpha port, but some generic code depends on it }
         index : tregister;
         is_immediate : boolean;
         offsetfixup : word; {needed for inline}
         options     : trefoptions;
         { the boundary to which the reference is surely aligned }
         alignment : byte;
       end;
       PReference = ^TReference;

       TLoc=(
              LOC_INVALID,      { added for tracking problems}
              LOC_CONSTANT,     { constant value }
              LOC_JUMP,         { boolean results only, jump to false or true label }
              LOC_FLAGS,        { boolean results only, flags are set }
              LOC_CREFERENCE,   { in memory constant value reference (cannot change) }
              LOC_REFERENCE,    { in memory value }
              LOC_REGISTER,     { in a processor register }
              LOC_CREGISTER,    { Constant register which shouldn't be modified }
              LOC_FPUREGISTER,  { FPU stack }
              LOC_CFPUREGISTER, { if it is a FPU register variable on the fpu stack }
              LOC_SSEREGISTER,
              LOC_CSSEREGISTER,
              LOC_CMMREGISTER,
              LOC_MMREGISTER
            );

      { tparamlocation describes where a parameter for a procedure is stored.
        References are given from the caller's point of view. The usual
        TLocation isn't used, because contains a lot of unnessary fields.
      }
      tparalocation = packed record
         size : TCGSize;
         loc  : TLoc;
         sp_fixup : longint;
         case TLoc of
            LOC_REFERENCE : (reference : tparareference);
            { segment in reference at the same place as in loc_register }
            LOC_REGISTER,LOC_CREGISTER : (
              case longint of
                1 : (register,register64.reghi : tregister);
                { overlay a register64.reglo }
                2 : (register64.reglo : tregister);
                { overlay a 64 Bit register type }
                3 : (reg64 : tregister64);
                4 : (register64 : tregister64);
              );
      end;

      tlocation = packed record
         loc  : TLoc;
         size : TCGSize;
         case TLoc of
            LOC_CONSTANT : (
              case longint of
                1 : (value : AWord);
                { can't do this, this layout depends on the host cpu. Use }
                { lo(valueqword)/hi(valueqword) instead (JM)              }
                { 2 : (valuelow, valuehigh:AWord);                        }
                { overlay a complete 64 Bit value }
                3 : (valueqword : qword);
              );
            LOC_CREFERENCE,
            LOC_REFERENCE : (reference : treference);
            { segment in reference at the same place as in loc_register }
            LOC_REGISTER,LOC_CREGISTER : (
              case longint of
                1 : (register,register64.reghi,segment : tregister);
                { overlay a register64.reglo }
                2 : (register64.reglo : tregister);
                { overlay a 64 Bit register type }
                3 : (reg64 : tregister64);
                4 : (register64 : tregister64);
              );
      end;

{*****************************************************************************
                                Operands
*****************************************************************************}


        { Types of operand }
        toptype=(top_none,top_reg,top_ref,top_const,top_symbol);

        toper=record
          ot  : longint;
          case typ : toptype of
           top_none   : ();
           top_reg    : (reg:tregister);
           top_ref    : (ref:preference);
           top_const  : (val:longint);
           top_symbol : (sym:tasmsymbol;symofs:longint);
        end;

   const
      { Registers which must be saved when calling a routine declared as
        cppdecl, cdecl, stdcall, safecall, palmossyscall. The registers
        saved should be the ones as defined in the target ABI and / or GCC.

        This value can be deduced from the CALLED_USED_REGISTERS array in the
        GCC source.
      }
      std_saved_registers = [];
      { Required parameter alignment when calling a routine declared as
        stdcall and cdecl. The alignment value should be the one defined
        by GCC or the target ABI.

        The value of this constant is equal to the constant
        PARM_BOUNDARY / BITS_PER_UNIT in the GCC source.
      }
      std_param_align = 8;

      { offsets for the integer and floating point registers }
      INT_REG = 0;
      FLOAT_REG = 32;

      { operator qualifiers }
      OQ_CHOPPED_ROUNDING            = $01;  { /C }
      OQ_ROUNDING_MODE_DYNAMIC       = $02;  { /D }
      OQ_ROUND_TOWARD_MINUS_INFINITY = $04;  { /M }
      OQ_INEXACT_RESULT_ENABLE        = $08; { /I }
      OQ_SOFTWARE_COMPLETION_ENABLE  = $10;  { /S }
      OQ_FLOATING_UNDERFLOW_ENABLE   = $20;  { /U }
      OQ_INTEGER_OVERFLOW_ENABLE     = $40;  { /V }


{*****************************************************************************
                   Opcode propeties (needed for optimizer)
*****************************************************************************}

{$ifndef NOOPT}
Type
{What an instruction can change}
  TInsChange = (Ch_None);
{$endif}


{ resets all values of ref to defaults }
procedure reset_reference(var ref : treference);
{ set mostly used values of a new reference }
function new_reference(base : tregister;offset : longint) : preference;
function newreference(const r : treference) : preference;
procedure disposereference(var r : preference);

function reg2str(r : tregister) : string;

{*****************************************************************************
                                  Init/Done
*****************************************************************************}

  procedure InitCpu;
  procedure DoneCpu;

implementation

uses
   verbose;

function reg2str(r : tregister) : string;

  begin
     if r in [R_0..R_31] then
       reg2str:='R'+tostr(longint(r)-longint(R_0))
     else if r in [R_F0..R_F31] then
       reg2str:='F'+tostr(longint(r)-longint(R_F0))
     else internalerror(38991);
  end;

procedure reset_reference(var ref : treference);
begin
  FillChar(ref,sizeof(treference),0);
end;


function new_reference(base : tregister;offset : longint) : preference;
var
  r : preference;
begin
  new(r);
  FillChar(r^,sizeof(treference),0);
  r^.offset:=offset;
  r^.alignment:=8;
  new_reference:=r;
end;

function newreference(const r : treference) : preference;

var
   p : preference;
begin
   new(p);
   p^:=r;
   newreference:=p;
end;

procedure disposereference(var r : preference);

begin
  dispose(r);
  r:=Nil;
end;

{*****************************************************************************
                                  Init/Done
*****************************************************************************}

  procedure InitCpu;
    begin
    end;

  procedure DoneCpu;
    begin
    end;

end.
