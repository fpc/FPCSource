{
    $Id$
    Copyright (c) 1999 by Florian Klaempfl

    Contains the base types for the PowerPC

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
unit cpubase;
interface
{$ifdef TP}
  {$L-,Y-}
{$endif}

uses
  strings,cobjects,aasm;

const
{ Size of the instruction table converted by nasmconv.pas }
  instabentries = 1103;
  maxinfolen    = 7;

{ By default we want everything }
{$define ATTOP}
{$define ATTREG}
{$define INTELOP}
{$define ITTABLE}

{ For TP we can't use asmdebug due the table sizes }
{$ifndef TP}
  {$define ASMDEBUG}
{$endif}

{ We Don't need the intel style opcodes if we don't have a intel
  reader or generator }
{$ifndef ASMDEBUG}
{$ifdef NORA386INT}
  {$ifdef NOAG386NSM}
    {$ifdef NOAG386INT}
      {$undef INTELOP}
    {$endif}
  {$endif}
{$endif}
{$endif}

{ We Don't need the AT&T style opcodes if we don't have a AT&T
  reader or generator }
{$ifdef NORA386ATT}
  {$ifdef NOAG386ATT}
    {$undef ATTOP}
    {$ifdef NOAG386DIR}
       {$undef ATTREG}
    {$endif}
  {$endif}
{$endif}

type
  TAsmOp=(A_None,
    { normal opcodes }
    a_add, a_add_, a_addo, a_addo_, a_addc, a_addc_, a_addco, a_addco_,
    a_adde, a_adde_, a_addeo, a_addeo_, a_addi, a_addic, a_addic_, a_addis,
    a_addme, a_addme_, a_addmeo, a_addmeo_, a_addze, a_addze_, a_addzeo,
    a_addzeo_, a_and, a_and_, a_andc, a_andc_, a_andi_, a_andis_, a_b,
    a_ba, a_bl, a_bla, a_bc, a_bca, a_bcl, a_bcla, a_bcctr, a_bcctrl, a_bclr,
    a_bclrl, a_cmp, a_cmpi, a_cmpl, a_cmpli, a_cntlzw, a_cntlzw_, a_crand,
    a_crandc, a_creqv, a_crnand, a_crnor, a_cror, a_crorc, a_crxor, a_dcba,
    a_dcbf, a_dcbi, a_dcbst, a_dcbt, a_divw, a_divw_, a_divwo, a_divwo_,
    a_divwu, a_divwu_, a_divwuo, a_divwuo_, a_eciwx, a_ecowx, a_eieio, a_eqv,
    a_eqv_, a_extsb, a_extsb_, a_extsh, a_extsh_, a_fabs, a_fabs_, a_fadd,
    a_fadd_, a_fadds, a_fadds_, a_fcompo, a_fcmpu, a_fctiw, a_fctw_, a_fctwz,
    a_fctwz_, a_fdiv, a_fdiv_, a_fdivs, a_fdivs_, a_fmadd, a_fmadd_, a_fmadds,
    a_fmadds_, a_fmr, a_fmsub, a_fmsub_, a_fmsubs, a_fmsubs_, a_fmul, a_fmul_,
    a_fmuls, a_fmuls_, a_fnabs, a_fnabs_, a_fneg, a_fneg_, a_fnmadd,
    a_fnmadd_, a_fnmadds, a_fnmadds_, a_fnmsub, a_fnmsub_, a_fnmsubs,
    a_fnmsubs_, a_fres, a_fres_, a_frsp, a_frsp_, a_frsqrte, a_frsqrte_,
    a_fsel, a_fsel_, a_fsqrt, a_fsqrt_, a_fsqrts, a_fsqrts_, a_fsub, a_fsub_,
    a_fsubs, a_fsubs_, a_icbi, a_isync, a_lbz, a_lbzu, a_lbzux, a_lbzx,
    a_lfd, a_lfdu, a_lfdux, a_lfdx, a_lfs, a_lfsu, a_lfsux, a_lfsx, a_lha,
    a_lhau, a_lhaux, a_lhax, a_hbrx, a_lhz, a_lhzu, a_lhzux, a_lhzx, a_lmw,
    a_lswi, a_lswx, a_lwarx, a_lwbrx, a_lwz, a_lwzu, a_lwzux, a_lwzx, a_mcrf,
    a_mcrfs, a_lcrxe, a_mfcr, a_mffs, a_maffs_, a_mfmsr, a_mfspr, a_mfsr,
    a_mfsrin, a_mftb, a_mtfcrf, a_a_mtfd0, a_mtfsb1, a_mtfsf, a_mtfsf_,
    a_mtfsfi, a_mtfsfi_, a_mtmsr, a_mtspr, a_mtsr, a_mtsrin, a_mulhw,
    a_mulhw_, a_mulhwu, a_mulhwu_, a_mulli, a_mullh, a_mullw_, a_mullwo,
    a_mullwo_, a_nand, a_nand_, a_neg, a_neg_, a_nego, a_nego_, a_nor, a_nor_,
    a_or, a_or_, a_orc, a_orc_, a_ori, a_oris, a_rfi, a_rlwimi, a_rlwimi_,
    a_rlwinm, a_tlwinm_, a_rlwnm, a_sc, a_slw, a_slw_, a_sraw, a_sraw_,
    a_srawi, a_srawi_,a_srw, a_srw_, a_stb, a_stbu, a_stbux, a_a_stbx, a_stfd,
    a_stfdu, a_stfdux, a_stfdx, a_stfiwx, a_stfs, a_stfsu, a_stfsux, a_stfsx,
    a_sth, a_sthbrx, a_sthu, a_sthux, a_sthx, a_stmw, a_stswi, a_stswx, a_stw,
    a_stwbrx, a_stwx_, a_stwu, a_stwux, a_stwx, a_subf, a_subf_, a_subfo,
    a_subfo_, a_subfc, a_subfc_, a_subfco, a_subfco_, a_subfe, a_subfe_,
    a_subfeo, a_subfeo_, a_subfic, a_subfme, a_subfme_, a_subfmeo, a_subfmeo_,
    a_subfze, a_subfze_, a_subfzeo, a_subfzeo_, a_sync, a_tlbia, a_tlbie,
    a_tlbsync, a_tw, twi, a_xor, a_xor_, a_xori, a_xoris,
    { simplified mnemonics }
    a_subi, a_subis, a_subic, a_subic_, a_sub, a_sub_, a_subo, a_subo_,
    a_subc, a_subc_, a_subco, _subco_, a_cmpwi, a_cmpw, a_cmplwi, a_cmplw,
    a_extlwi, a_extlwi_, a_extrwi, a_extrwi_, a_inslwi, a_inslwi_, a_insrwi,
    a_insrwi_, a_rotlwi, a_rotlwi_, a_rotlw, a_rotlw_, a_slwi, a_slwi_,
    a_srwi, a_srwi_, a_clrlwi, a_clrlwi_, a_clrrwi, a_clrrwi_, a_clrslwi,
    a_clrslwi_, a_blr, a_bctr, a_blrl, a_bctrl, a_crset, a_crclr, a_crmove,
    a_crnot, a_mt {move to special prupose reg}, a_mf {move from special purpose reg},
    nop, a_li, a_la, a_mr, a_not, a_mtcr);

  op2strtable=array[tasmop] of string[8];

const
  firstop = low(tasmop);
  lastop  = high(tasmop);


{*****************************************************************************
                                Conditions
*****************************************************************************}

type
  TAsmCond=(C_None,
    C_LT,C_LE,C_EQ,C_GE,C_GT,C_NL,C_NE,C_NG,C_SO,C_NS,C_UN,C_NU
  );
(*
  TAsmCondBO = (B_T,B_F,B_DNZ,B_DNZT,B_DNZF,B_DZ,B_DZT,B_DZF);
  TasmCondSuffix = (SU_NO,SU_A,SU_LR,SU_CTR,SU_L,SU_LA,SU_LRL,SU_CTRL);

const
  cond2str:array[TAsmCond] of string[2]=('',
    'lt','le','eq','ge','gt','nl','ne','ng','so','ns','un','nu'
  );

  condbo2str:array[TasmCondBO] of String[4] = (
    't','f','dnz','dnzt','dnzf','dz','dzt','dzf'
  );

  condsuffix2str:array[TAsmCondSuffix] of String[4] = (
    '','a','lr','ctr','l','la','lrl','ctrl'
  );

  inverse_cond:array[TAsmCond] of TAsmCond=(C_None,
    C_GE,C_GT,C_NE,C_LT,C_LE,C_LT,C_EQ,C_GT,C_NS,C_SO,C_NU,C_UN
  );

  AllowedCond = Array[TAsmCondBO,TAsmCondSuffix] of Boolean = (
{t}      (
{f}
{dnz}
{dnzt}
{dnzf}
{dz}
{dzt}
{dzf}
const
  CondAsmOps=3;
  CondAsmOp:array[0..CondAsmOps-1] of TasmOp=(
     A_BC, A_TW, A_TWI
  );
*)

{*****************************************************************************
                                  Registers
*****************************************************************************}

type
  { enumeration for registers, don't change the order }
  { it's used by the register size conversions        }
  tregister = (R_NO,
    R_0,R_1,R_2,R_3,R_4,R_5,R_6,R_7, R_9,R_10,R_11,R_12,R_13,R_14,R_15,R_16,
    R_17,R_18,R_19,R_20,R_21,R_22,R_23,R_24,R_25,R_26,R_27,R_28,R_29,R_30,R_31,
    R_F0,R_F1,R_F2,R_F3,R_F4,R_F5,R_F6,R_F7,R_F8,R_F9,R_F10,R_F11,R_F12,
    R_F13,R_F14,R_F15,R_F16,R_F17, R_F18,R_F19,R_F20,R_F21,R_F22, R_F23,R_F24,
    R_F25,R_F26,R_F27,R_F28,R_F29,R_F30,R_F31,
    R_M0,R_M1,R_M2,R_M3,R_M4,R_M5,R_M6,R_M7,R_M8,R_M9,R_M10,R_M11,R_M12,
    R_M13,R_M14,R_M15,R_M16,R_M17,R_M18,R_M19,R_M20,R_M21,R_M22, R_M23,R_M24,
    R_M25,R_M26,R_M27,R_M28,R_M29,R_M30,R_M31,

    R_CR,R_CR0,R_CR1,R_CR2,R_CR3,R_CR4,R_CR5,R_CR6,R_CR7,
    R_XER,R_LR,R_CTR,R_FPSCR
  );

Const
   R_SPR1 = R_XER;
   R_SPR8 = R_LR;
   R_SPR9 = R_CTR;
   R_TOC = R_2;
   CR0 = 0;
   CR1 = 4;
   CR2 = 8;
   CR3 = 12;
   CR4 = 16;
   CR5 = 20;
   CR6 = 24;
   CR7 = 28;
   LT = 0;
   GT = 1;
   EQ = 2;
   SO = 3;
   FX = 4;
   FEX = 5;
   VX = 6;
   OX = 7;


Type
  tregisterset = set of tregister;

  reg2strtable = array[tregister] of string[5];

const
  firstreg = low(tregister);
  lastreg  = high(tregister);

  gnu_reg2str : reg2strtable = ('',
    '0','1','2','3','4','5','6','7', '9','10','11','12','13','14','15','16',
    '17','18','19','20','21','22','23','24','25','26','27','28','29','30','31',
    'F0','F1','F2','F3','F4','F5','F6','F7', 'F8','F9','F10','F11','F12',
    'F13','F14','F15','F16','F17', 'F18','F19','F20','F21','F22', 'F23','F24',
    'F25','F26','F27','F28','F29','F30','F31',
    'M0','M1','M2','M3','M4','M5','M6','M7','M8','M9','M10','M11','M12',
    'M13','M14','M15','M16','M17','M18','M19','M20','M21','M22', 'M23','M24',
    'M25','M26','M27','M28','M29','M30','M31',
    'CR','CR0','CR1','CR2','CR3','CR4','CR5','CR6','CR7',
    'XER','LR','CTR','FPSCR'
  );

  mot_reg2str : reg2strtable = ('',
    'R0','R1','R2','R3','R4','R5','R6','R7', 'R9','R10','R11','R12','R13',
    'R14','R15','R16','R17','R18','R19','R20','R21','R22','R23','R24','R25',
    'R26','R27','R28','R29','R30','R31',
    'F0','F1','F2','F3','F4','F5','F6','F7', 'F8','F9','F10','F11','F12',
    'F13','F14','F15','F16','F17', 'F18','F19','F20','F21','F22', 'F23','F24',
    'F25','F26','F27','F28','F29','F30','F31',
    'M0','M1','M2','M3','M4','M5','M6','M7','M8','M9','M10','M11','M12',
    'M13','M14','M15','M16','M17','M18','M19','M20','M21','M22', 'M23','M24',
    'M25','M26','M27','M28','M29','M30','M31',
    'CR','CR0','CR1','CR2','CR3','CR4','CR5','CR6','CR7',
    'XER','LR','CTR','FPSCR'
  );


{*****************************************************************************
                                   Flags
*****************************************************************************}

type
  TResFlags = (F_LT,F_GT,F_EQ,F_SO,F_FX,F_FEX,F_VX,F_OX);
(*
const
  { arrays for boolean location conversions }
  flag_2_cond : array[TResFlags] of TAsmCond =
     (C_E,C_NE,C_G,C_L,C_GE,C_LE,C_C,C_NC,C_A,C_AE,C_B,C_BE);
*)

{*****************************************************************************
                                Reference
*****************************************************************************}

type
  trefoptions=(ref_none,ref_parafixup,ref_localfixup);

  { immediate/reference record }
  preference = ^treference;
  treference = packed record
     is_immediate : boolean; { is this used as reference or immediate }
     base: tregister;
     offset      : longint;
     symbol      : pasmsymbol;
     offsetfixup : longint;
     options     : trefoptions;
  end;


{*****************************************************************************
                                Operand
*****************************************************************************}

type
  toptype=(top_none,top_reg,top_ref,top_const,top_symbol);

  toper=record
    ot  : longint;
    case typ : toptype of
     top_none   : ();
     top_reg    : (reg:tregister);
     top_ref    : (ref:preference);
     top_const  : (val:longint);
     top_symbol : (sym:pasmsymbol;symofs:longint);
  end;


{*****************************************************************************
                               Generic Location
*****************************************************************************}

type
  TLoc=(
    LOC_INVALID,     { added for tracking problems}
    LOC_REGISTER,    { in a processor register }
    LOC_CREGISTER,    { Constant register which shouldn't be modified }
    LOC_FPUREGISTER, { FPU register }
    LOC_CFPUREGISTER,{ Constant FPU register which shouldn't be modified }
    LOC_MMREGISTER,  { multimedia register }
    LOC_CMMREGISTER, { Constant multimedia reg which shouldn't be modified }
    LOC_MEM,         { in memory }
    LOC_REFERENCE,   { like LOC_MEM, but lvalue }
    LOC_JUMP,        { boolean results only, jump to false or true label }
    LOC_FLAGS        { boolean results only, flags are set }
  );

  plocation = ^tlocation;
  tlocation = packed record
     case loc : tloc of
        LOC_MEM,LOC_REFERENCE : (reference : treference);
        LOC_FPUREGISTER, LOC_CFPUREGISTER : (register: tregister);
        LOC_MMREGISTER, LOC_CMMREGISTER : (register: tregister);
        LOC_JUMP : ();
        LOC_FLAGS : (resflags : tresflags);
        LOC_INVALID : ();

        { segment in reference at the same place as in loc_register }
        LOC_REGISTER,LOC_CREGISTER : (
        case longint of
          1 : (register,registerhigh : tregister);
          { overlay a registerlow }
          2 : (registerlow : tregister);
        );
  end;


{*****************************************************************************
                                 Constants
*****************************************************************************}

{type
  tcpuflags = (cf_registers64);}

const
  availabletempregsint = [R_11..R_30];
  availabletempregsfpu = [R_F14..R_F31];
  availabletempregsmm  = [R_M0..R_M31];

  c_countusableregsint = 21;
  c_countusableregsfpu = 32;
  c_countusableregsmm  = 32;

  maxvarregs = 18;

  varregs : Array [1..maxvarregs] of Tregister =
            (R_13,R_14,R_15,R_16,R_17,R_18,R_19,R_20,R_21,R_22,R_23,R_24,R_25,
             R_26,R_27,R_28,R_29,R_30);

  intregs = [R_0..R_31];
  fpuregs = [R_F0..R_F31];
  mmregs = [R_M0..R_M31];

  registers_saved_on_cdecl = [R_11..R_30];

  { generic register names }
  stack_pointer = R_1;
  frame_pointer = R_31;
  self_pointer  = R_9;
  accumulator   = R_3;
  scratchregister = R_0;

(*  cpuflags : set of tcpuflags = []; *)

  { sizes }
  pointersize   = 4;
  extended_size = 8;

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    { resets all values of ref to defaults }
    procedure reset_reference(var ref : treference);
    { set mostly used values of a new reference }
    function new_reference(base : tregister;offset : longint) : preference;

    function newreference(const r : treference) : preference;
    procedure disposereference(var r : preference);

    function reg2str(r : tregister) : string;

    function is_calljmp(o:tasmop):boolean;


implementation

{$ifdef heaptrc}
  uses
      ppheap;
{$endif heaptrc}

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function reg2str(r : tregister) : string;
      begin
         reg2str:=mot_reg2str[r];
      end;


    function is_calljmp(o:tasmop):boolean;
      begin
        case o of
          A_B,
          A_BA,
          A_BLR,
          A_BCTR,
          A_BC:
            is_calljmp:=true;
          else
            is_calljmp:=false;
        end;
      end;


    procedure disposereference(var r : preference);
      begin
         dispose(r);
         r:=nil;
      end;


    function newreference(const r : treference) : preference;
      var
         p : preference;
      begin
         new(p);
         p^:=r;
         newreference:=p;
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
  r^.base:=base;
  r^.offset:=offset;
  new_reference:=r;
end;

end.
{
  $Log$
  Revision 1.3  1999-08-05 14:58:18  florian
    * some fixes for the floating point registers
    * more things for the new code generator

  Revision 1.2  1999/08/04 12:59:25  jonas
    * all tokes now start with an underscore
    * PowerPC compiles!!

  Revision 1.1  1999/08/03 23:37:53  jonas
    + initial implementation for PowerPC based on the Alpha stuff

}
