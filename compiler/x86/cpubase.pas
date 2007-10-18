{
    Copyright (c) 1998-2002 by Florian Klaempfl and Peter Vreman

    Contains the base types for the i386 and x86-64 architecture

    * This code was inspired by the NASM sources
      The Netwide Assembler is Copyright (c) 1996 Simon Tatham and
      Julian Hall. All rights reserved.

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
{# Base unit for processor information. This unit contains
   enumerations of registers, opcodes, sizes, and other
   such things which are processor specific.
}
unit cpubase;

{$i fpcdefs.inc}

interface

uses
  cutils,cclasses,
  globtype,
  cgbase
  ;


{*****************************************************************************
                                Assembler Opcodes
*****************************************************************************}

    type
{$ifdef x86_64}
      TAsmOp={$i x8664op.inc}
{$else x86_64}
      TAsmOp={$i i386op.inc}
{$endif x86_64}

      { This should define the array of instructions as string }
      op2strtable=array[tasmop] of string[11];

    const
      { First value of opcode enumeration }
      firstop = low(tasmop);
      { Last value of opcode enumeration  }
      lastop  = high(tasmop);

{*****************************************************************************
                                  Registers
*****************************************************************************}

   const
      { Invalid register number }
      RS_INVALID    = $ff;

      { Integer Super registers }
      RS_RAX        = $00;      {EAX}
      RS_RCX        = $01;      {ECX}
      RS_RDX        = $02;      {EDX}
      RS_RBX        = $03;      {EBX}
      RS_RSI        = $04;      {ESI}
      RS_RDI        = $05;      {EDI}
      RS_RBP        = $06;      {EBP}
      RS_RSP        = $07;      {ESP}
      RS_R8         = $08;      {R8}
      RS_R9         = $09;      {R9}
      RS_R10        = $0a;      {R10}
      RS_R11        = $0b;      {R11}
      RS_R12        = $0c;      {R12}
      RS_R13        = $0d;      {R13}
      RS_R14        = $0e;      {R14}
      RS_R15        = $0f;      {R15}
      { create aliases to allow code sharing between x86-64 and i386 }
      RS_EAX        = RS_RAX;
      RS_EBX        = RS_RBX;
      RS_ECX        = RS_RCX;
      RS_EDX        = RS_RDX;
      RS_ESI        = RS_RSI;
      RS_EDI        = RS_RDI;
      RS_EBP        = RS_RBP;
      RS_ESP        = RS_RSP;

      { Number of first imaginary register }
      first_int_imreg     = $10;

      { Float Super registers }
      RS_ST0        = $00;
      RS_ST1        = $01;
      RS_ST2        = $02;
      RS_ST3        = $03;
      RS_ST4        = $04;
      RS_ST5        = $05;
      RS_ST6        = $06;
      RS_ST7        = $07;

      { Number of first imaginary register }
      first_fpu_imreg     = $08;

      { MM Super registers }
      RS_XMM0        = $00;
      RS_XMM1        = $01;
      RS_XMM2        = $02;
      RS_XMM3        = $03;
      RS_XMM4        = $04;
      RS_XMM5        = $05;
      RS_XMM6        = $06;
      RS_XMM7        = $07;
      RS_XMM8        = $08;
      RS_XMM9        = $09;
      RS_XMM10       = $0a;
      RS_XMM11       = $0b;
      RS_XMM12       = $0c;
      RS_XMM13       = $0d;
      RS_XMM14       = $0e;
      RS_XMM15       = $0f;

      { Number of first imaginary register }
{$ifdef x86_64}
      first_mm_imreg     = $10;
{$else x86_64}
      first_mm_imreg     = $08;
{$endif x86_64}

      { The subregister that specifies the entire register and an address }
{$ifdef x86_64}
      { Hammer }
      R_SUBWHOLE    = R_SUBQ;
      R_SUBADDR     = R_SUBQ;
{$else x86_64}
      { i386 }
      R_SUBWHOLE    = R_SUBD;
      R_SUBADDR     = R_SUBD;
{$endif x86_64}

      { Available Registers }
{$ifdef x86_64}
      {$i r8664con.inc}
{$else x86_64}
      {$i r386con.inc}
{$endif x86_64}

    type
      { Number of registers used for indexing in tables }
{$ifdef x86_64}
      tregisterindex=0..{$i r8664nor.inc}-1;
{$else x86_64}
      tregisterindex=0..{$i r386nor.inc}-1;
{$endif x86_64}

    const
{$warning TODO Calculate bsstart}
      regnumber_count_bsstart = 64;

      regnumber_table : array[tregisterindex] of tregister = (
{$ifdef x86_64}
        {$i r8664num.inc}
{$else x86_64}
        {$i r386num.inc}
{$endif x86_64}
      );

      regstabs_table : array[tregisterindex] of shortint = (
{$ifdef x86_64}
        {$i r8664stab.inc}
{$else x86_64}
        {$i r386stab.inc}
{$endif x86_64}
      );

      regdwarf_table : array[tregisterindex] of shortint = (
{$ifdef x86_64}
        {$i r8664dwrf.inc}
{$else x86_64}
        {$i r386dwrf.inc}
{$endif x86_64}
      );

   type
      totherregisterset = set of tregisterindex;


{*****************************************************************************
                                Conditions
*****************************************************************************}

    type
      TAsmCond=(C_None,
        C_A,C_AE,C_B,C_BE,C_C,C_E,C_G,C_GE,C_L,C_LE,C_NA,C_NAE,
        C_NB,C_NBE,C_NC,C_NE,C_NG,C_NGE,C_NL,C_NLE,C_NO,C_NP,
        C_NS,C_NZ,C_O,C_P,C_PE,C_PO,C_S,C_Z
      );

    const
      cond2str:array[TAsmCond] of string[3]=('',
        'a','ae','b','be','c','e','g','ge','l','le','na','nae',
        'nb','nbe','nc','ne','ng','nge','nl','nle','no','np',
        'ns','nz','o','p','pe','po','s','z'
      );

{*****************************************************************************
                                   Flags
*****************************************************************************}

    type
      TResFlags = (F_E,F_NE,F_G,F_L,F_GE,F_LE,F_C,F_NC,
                   F_A,F_AE,F_B,F_BE,
                   F_S,F_NS,F_O,F_NO);


{*****************************************************************************
                                 Constants
*****************************************************************************}

    const
      { declare aliases }
      LOC_SSEREGISTER = LOC_MMREGISTER;
      LOC_CSSEREGISTER = LOC_CMMREGISTER;

      max_operands = 3;
      maxfpuregs = 8;

{*****************************************************************************
                            CPU Dependent Constants
*****************************************************************************}

    {$i cpubase.inc}

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function cgsize2subreg(s:Tcgsize):Tsubregister;
    function reg2opsize(r:Tregister):topsize;
    function reg_cgsize(const reg: tregister): tcgsize;
    function is_calljmp(o:tasmop):boolean;
    procedure inverse_flags(var f: TResFlags);
    function flags_to_cond(const f: TResFlags) : TAsmCond;
    function is_segment_reg(r:tregister):boolean;
    function findreg_by_number(r:Tregister):tregisterindex;
    function std_regnum_search(const s:string):Tregister;
    function std_regname(r:Tregister):string;
    function dwarf_reg(r:tregister):shortint;

    function inverse_cond(const c: TAsmCond): TAsmCond; {$ifdef USEINLINE}inline;{$endif USEINLINE}
    function conditions_equal(const c1, c2: TAsmCond): boolean; {$ifdef USEINLINE}inline;{$endif USEINLINE}

implementation

    uses
      rgbase,verbose;

    const
    {$ifdef x86_64}
      std_regname_table : array[tregisterindex] of string[7] = (
        {$i r8664std.inc}
      );

      regnumber_index : array[tregisterindex] of tregisterindex = (
        {$i r8664rni.inc}
      );
      std_regname_index : array[tregisterindex] of tregisterindex = (
        {$i r8664sri.inc}
      );
    {$else x86_64}
      std_regname_table : array[tregisterindex] of string[7] = (
        {$i r386std.inc}
      );

      regnumber_index : array[tregisterindex] of tregisterindex = (
        {$i r386rni.inc}
      );

      std_regname_index : array[tregisterindex] of tregisterindex = (
        {$i r386sri.inc}
      );
    {$endif x86_64}


{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function cgsize2subreg(s:Tcgsize):Tsubregister;
      begin
        case s of
          OS_8,OS_S8:
            cgsize2subreg:=R_SUBL;
          OS_16,OS_S16:
            cgsize2subreg:=R_SUBW;
          OS_32,OS_S32:
            cgsize2subreg:=R_SUBD;
          OS_64,OS_S64:
            cgsize2subreg:=R_SUBQ;
          OS_M64:
            cgsize2subreg:=R_SUBNONE;
          OS_F32,OS_F64,OS_C64,
          OS_M128,OS_MS128:
            cgsize2subreg:=R_SUBWHOLE;
          else
            internalerror(200301231);
        end;
      end;


    function reg_cgsize(const reg: tregister): tcgsize;
      const subreg2cgsize:array[Tsubregister] of Tcgsize =
            (OS_NO,OS_8,OS_8,OS_16,OS_32,OS_64,OS_NO,OS_NO,OS_NO,OS_F32,OS_F64,OS_M128);
      begin
        case getregtype(reg) of
          R_INTREGISTER :
            reg_cgsize:=subreg2cgsize[getsubreg(reg)];
          R_FPUREGISTER :
            reg_cgsize:=OS_F80;
          R_MMXREGISTER:
            reg_cgsize:=OS_M64;
          R_MMREGISTER:
            reg_cgsize:=subreg2cgsize[getsubreg(reg)];
          R_SPECIALREGISTER :
            case reg of
              NR_CS,NR_DS,NR_ES,NR_SS,NR_FS,NR_GS:
                reg_cgsize:=OS_16
              else
                reg_cgsize:=OS_32
            end
          else
            internalerror(200303181);
          end;
        end;


    function reg2opsize(r:Tregister):topsize;
      const
        subreg2opsize : array[tsubregister] of topsize =
          (S_NO,S_B,S_B,S_W,S_L,S_Q,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO);
      begin
        reg2opsize:=S_L;
        case getregtype(r) of
          R_INTREGISTER :
            reg2opsize:=subreg2opsize[getsubreg(r)];
          R_FPUREGISTER :
            reg2opsize:=S_FL;
          R_MMXREGISTER,
          R_MMREGISTER :
            reg2opsize:=S_MD;
          R_SPECIALREGISTER :
            begin
              case r of
                NR_CS,NR_DS,NR_ES,
                NR_SS,NR_FS,NR_GS :
                  reg2opsize:=S_W;
              end;
            end;
          else
            internalerror(200303181);
        end;
      end;


    function is_calljmp(o:tasmop):boolean;
      begin
        case o of
          A_CALL,
{$ifdef i386}
          A_JCXZ,
{$endif i386}
          A_JECXZ,
{$ifdef x86_64}
          A_JRCXZ,
{$endif x86_64}
          A_JMP,
          A_LOOP,
          A_LOOPE,
          A_LOOPNE,
          A_LOOPNZ,
          A_LOOPZ,
          A_Jcc :
            is_calljmp:=true;
          else
            is_calljmp:=false;
        end;
      end;


    procedure inverse_flags(var f: TResFlags);
      const
        inv_flags: array[TResFlags] of TResFlags =
          (F_NE,F_E,F_LE,F_GE,F_L,F_G,F_NC,F_C,
           F_BE,F_B,F_AE,F_A,
           F_NS,F_S,F_NO,F_O);
      begin
        f:=inv_flags[f];
      end;


    function flags_to_cond(const f: TResFlags) : TAsmCond;
      const
        flags_2_cond : array[TResFlags] of TAsmCond =
          (C_E,C_NE,C_G,C_L,C_GE,C_LE,C_C,C_NC,C_A,C_AE,C_B,C_BE,C_S,C_NS,C_O,C_NO);
      begin
        result := flags_2_cond[f];
      end;


    function is_segment_reg(r:tregister):boolean;
      begin
        result:=false;
        case r of
          NR_CS,NR_DS,NR_ES,
          NR_SS,NR_FS,NR_GS :
            result:=true;
        end;
      end;


    function findreg_by_number(r:Tregister):tregisterindex;
      var
        hr : tregister;
      begin
        { for the name the sub reg doesn't matter }
        hr:=r;
        case getsubreg(hr) of
          R_SUBMMS,R_SUBMMD,R_SUBMMWHOLE:
            setsubreg(hr,R_SUBNONE);
        end;
        result:=findreg_by_number_table(hr,regnumber_index);
      end;


    function std_regnum_search(const s:string):Tregister;
      begin
        result:=regnumber_table[findreg_by_name_table(s,std_regname_table,std_regname_index)];
      end;


    function std_regname(r:Tregister):string;
      var
        p : tregisterindex;
      begin
        if getregtype(r) in [R_MMREGISTER,R_MMXREGISTER] then
          r:=newreg(getregtype(r),getsupreg(r),R_SUBNONE);
        p:=findreg_by_number_table(r,regnumber_index);
        if p<>0 then
          result:=std_regname_table[p]
        else
          result:=generic_regname(r);
      end;


    function inverse_cond(const c: TAsmCond): TAsmCond; {$ifdef USEINLINE}inline;{$endif USEINLINE}
      const
        inverse: array[TAsmCond] of TAsmCond=(C_None,
          C_NA,C_NAE,C_NB,C_NBE,C_NC,C_NE,C_NG,C_NGE,C_NL,C_NLE,C_A,C_AE,
          C_B,C_BE,C_C,C_E,C_G,C_GE,C_L,C_LE,C_O,C_P,
          C_S,C_Z,C_NO,C_NP,C_NP,C_P,C_NS,C_NZ
        );
      begin
        result := inverse[c];
      end;


    function conditions_equal(const c1, c2: TAsmCond): boolean; {$ifdef USEINLINE}inline;{$endif USEINLINE}
      begin
        result := c1 = c2;
      end;


    function dwarf_reg(r:tregister):shortint;
      begin
        result:=regdwarf_table[findreg_by_number(r)];
        if result=-1 then
          internalerror(200603251);
      end;


end.
