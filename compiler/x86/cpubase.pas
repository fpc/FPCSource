{
    $Id$
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
  globtype,globals,
  cpuinfo,
  aasmbase,
  cginfo
{$ifdef delphi}
  ,dmisc
{$endif}
  ;


{*****************************************************************************
                                Assembler Opcodes
*****************************************************************************}

    type
{$ifdef x86_64}
      TAsmOp={$i x86_64op.inc}
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

      { Integer Super register first and last }
      first_int_supreg    = $00;
{$ifdef x86_64}
      last_int_supreg     = $0f;
{$else}
      last_int_supreg     = $07;
{$endif}
      first_int_imreg     = $10;
      last_int_imreg      = $fe;

      { Float Super registers }
      RS_ST0        = $00;
      RS_ST1        = $01;
      RS_ST2        = $02;
      RS_ST3        = $03;
      RS_ST4        = $04;
      RS_ST5        = $05;
      RS_ST6        = $06;
      RS_ST7        = $07;

      { Float Super register first and last }
      first_fpu_supreg    = $00;
      last_fpu_supreg     = $07;
      first_fpu_imreg     = $08;
      last_fpu_imreg      = $fe;

      { MM Super registers }
      RS_MM0        = $00;
      RS_MM1        = $01;
      RS_MM2        = $02;
      RS_MM3        = $03;
      RS_MM4        = $04;
      RS_MM5        = $05;
      RS_MM6        = $06;
      RS_MM7        = $07;
      RS_MM8        = $08;
      RS_MM9        = $09;
      RS_MM10       = $0a;
      RS_MM11       = $0b;
      RS_MM12       = $0c;
      RS_MM13       = $0d;
      RS_MM14       = $0e;
      RS_MM15       = $0f;

      { Float Super register first and last }
      first_mmx_supreg    = $00;
      last_mmx_supreg     = $07;
      first_mmx_imreg     = $08;
      last_mmx_imreg      = $fe;

      { The subregister that specifies the entire register }
{$ifdef x86_64}
      R_SUBWHOLE    = R_SUBQ; {Hammer}
{$else x86_64}
      R_SUBWHOLE    = R_SUBD;  {i386}
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

      regstabs_table : array[tregisterindex] of tregister = (
{$ifdef x86_64}
        {$i r8664stab.inc}
{$else x86_64}
        {$i r386stab.inc}
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

      inverse_cond:array[TAsmCond] of TAsmCond=(C_None,
        C_NA,C_NAE,C_NB,C_NBE,C_NC,C_NE,C_NG,C_NGE,C_NL,C_NLE,C_A,C_AE,
        C_B,C_BE,C_C,C_E,C_G,C_GE,C_L,C_LE,C_O,C_P,
        C_S,C_Z,C_NO,C_NP,C_NP,C_P,C_NS,C_NZ
      );

{*****************************************************************************
                                   Flags
*****************************************************************************}

    type
      TResFlags = (F_E,F_NE,F_G,F_L,F_GE,F_LE,F_C,F_NC,F_A,F_AE,F_B,F_BE);

{*****************************************************************************
                                Reference
*****************************************************************************}

    type
      { reference record }
      preference = ^treference;
      treference = packed record
         segment,
         base,
         index       : tregister;
         scalefactor : byte;
         offset      : longint;
         symbol      : tasmsymbol;
      end;

      { reference record }
      pparareference = ^tparareference;
      tparareference = packed record
         index       : tregister;
         offset      : longint;
      end;

{*****************************************************************************
                               Generic Location
*****************************************************************************}

    type
      { tparamlocation describes where a parameter for a procedure is stored.
        References are given from the caller's point of view. The usual
        TLocation isn't used, because contains a lot of unnessary fields.
      }
      tparalocation = packed record
         size : TCGSize;
         loc  : TCGLoc;
         case TCGLoc of
            LOC_REFERENCE : (reference : tparareference);
            { segment in reference at the same place as in loc_register }
            LOC_REGISTER,LOC_CREGISTER : (
              case longint of
                1 : (register,registerhigh : tregister);
                { overlay a registerlow }
                2 : (registerlow : tregister);
                { overlay a 64 Bit register type }
                3 : (reg64 : tregister64);
                4 : (register64 : tregister64);
              );
            { it's only for better handling }
            LOC_MMXREGISTER,LOC_CMMXREGISTER : (mmxreg : tregister);
      end;

      tlocation = packed record
         loc  : TCGLoc;
         size : TCGSize;
         case TCGLoc of
            LOC_FLAGS : (resflags : tresflags);
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
                1 : (register,registerhigh,segment : tregister);
                { overlay a registerlow }
                2 : (registerlow : tregister);
                { overlay a 64 Bit register type }
                3 : (reg64 : tregister64);
                4 : (register64 : tregister64);
              );
            { it's only for better handling }
            LOC_MMXREGISTER,LOC_CMMXREGISTER : (mmxreg : tregister);
      end;

{*****************************************************************************
                                 Constants
*****************************************************************************}

    const
      { declare aliases }
      LOC_MMREGISTER = LOC_SSEREGISTER;
      LOC_CMMREGISTER = LOC_CSSEREGISTER;

      max_operands = 3;

      { low and high of the available maximum width integer general purpose }
      { registers                                                            }
      LoGPReg = RS_EAX;
      HiGPReg = RS_EDX;

      { Table of registers which can be allocated by the code generator
        internally, when generating the code.
      }
      { legend:                                                                }
      { xxxregs = set of all possibly used registers of that type in the code  }
      {           generator                                                    }
      { usableregsxxx = set of all 32bit components of registers that can be   }
      {           possible allocated to a regvar or using getregisterxxx (this }
      {           excludes registers which can be only used for parameter      }
      {           passing on ABI's that define this)                           }
      { c_countusableregsxxx = amount of registers in the usableregsxxx set    }

//      maxintregs = 4;
//      intregs = [R_EAX..R_BL]-[R_ESI,R_SI];
      { to determine how many registers to use for regvars }
      maxintscratchregs = 1;

      maxfpuregs = 8;
      usableregsfpu = [];
      c_countusableregsfpu = 0;

      usableregsmm = [RS_MM0..RS_MM7];
      c_countusableregsmm  = 8;

{*****************************************************************************
                            CPU Dependent Constants
*****************************************************************************}

    {$i cpubase.inc}

{*****************************************************************************
                                  Helpers
*****************************************************************************}

    function cgsize2subreg(s:Tcgsize):Tsubregister;
    function reg2opsize(r:Tregister):topsize;
    function is_calljmp(o:tasmop):boolean;
    procedure inverse_flags(var f: TResFlags);
    function flags_to_cond(const f: TResFlags) : TAsmCond;
    function is_segment_reg(r:tregister):boolean;
    function findreg_by_number(r:Tregister):tregisterindex;
    function std_regnum_search(const s:string):Tregister;
    function std_regname(r:Tregister):string;


implementation

    uses
      verbose;

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
          else
            internalerror(200301231);
        end;
      end;


    function reg2opsize(r:Tregister):topsize;
      const
        subreg2opsize : array[tsubregister] of topsize =
          (S_NO,S_B,S_B,S_W,S_L,S_D);
      begin
        reg2opsize:=S_L;
        case getregtype(r) of
          R_INTREGISTER :
            reg2opsize:=subreg2opsize[getsubreg(r)];
          R_FPUREGISTER :
            reg2opsize:=S_FL;
          R_MMXREGISTER,
          R_MMREGISTER :
            reg2opsize:=S_D;
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
          A_JCXZ,
          A_JECXZ,
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
          (F_NE,F_E,F_LE,F_GE,F_L,F_G,F_NC,F_C,F_BE,F_B,F_AE,F_A);
      begin
        f:=inv_flags[f];
      end;


    function flags_to_cond(const f: TResFlags) : TAsmCond;
      const
        flags_2_cond : array[TResFlags] of TAsmCond =
          (C_E,C_NE,C_G,C_L,C_GE,C_LE,C_C,C_NC,C_A,C_AE,C_B,C_BE);
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


    function findreg_by_stdname(const s:string):byte;
      var
        i,p : tregisterindex;
      begin
        {Binary search.}
        p:=0;
        i:=regnumber_count_bsstart;
        repeat
          if (p+i<=high(tregisterindex)) and (std_regname_table[std_regname_index[p+i]]<=s) then
            p:=p+i;
          i:=i shr 1;
        until i=0;
        if std_regname_table[std_regname_index[p]]=s then
          result:=std_regname_index[p]
        else
          result:=0;
      end;


    function findreg_by_number(r:Tregister):tregisterindex;
      var
        i,p : tregisterindex;
      begin
        {Binary search.}
        p:=0;
        i:=regnumber_count_bsstart;
        repeat
          if (p+i<=high(tregisterindex)) and (regnumber_table[regnumber_index[p+i]]<=r) then
            p:=p+i;
          i:=i shr 1;
        until i=0;
        if regnumber_table[regnumber_index[p]]=r then
          result:=regnumber_index[p]
        else
          result:=0;
      end;


    function std_regnum_search(const s:string):Tregister;
      begin
        result:=regnumber_table[findreg_by_stdname(s)];
      end;


    function std_regname(r:Tregister):string;
      var
        p : tregisterindex;
      begin
        p:=findreg_by_number(r);
        if p<>0 then
          result:=std_regname_table[p]
        else
          result:=generic_regname(r);
      end;

end.
{
  $Log$
  Revision 1.20  2003-09-25 21:29:23  peter
    * remove sp_fixup

  Revision 1.19  2003/09/24 17:12:36  florian
    * x86-64 adaptions

  Revision 1.18  2003/09/23 17:56:06  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.17  2003/09/07 22:09:35  peter
    * preparations for different default calling conventions
    * various RA fixes

  Revision 1.16  2003/09/04 21:07:03  florian
    * ARM compiler compiles again

  Revision 1.15  2003/09/03 15:55:02  peter
    * NEWRA branch merged

  Revision 1.14  2003/09/03 11:18:37  florian
    * fixed arm concatcopy
    + arm support in the common compiler sources added
    * moved some generic cg code around
    + tfputype added
    * ...

  Revision 1.13.2.8  2003/08/31 19:31:51  daniel
    * FIxed superregister constants

  Revision 1.13.2.7  2003/08/31 16:18:05  peter
    * more fixes

  Revision 1.13.2.6  2003/08/31 15:46:26  peter
    * more updates for tregister

  Revision 1.13.2.5  2003/08/31 13:50:16  daniel
    * Remove sorting and use pregenerated indexes
    * Some work on making things compile

  Revision 1.13.2.4  2003/08/29 17:29:00  peter
    * next batch of updates

  Revision 1.13.2.3  2003/08/28 18:35:08  peter
    * tregister changed to cardinal

  Revision 1.13.2.2  2003/08/27 21:06:34  peter
    * more updates

  Revision 1.13.2.1  2003/08/27 19:55:54  peter
    * first tregister patch

  Revision 1.13  2003/08/20 07:48:04  daniel
    * Made internal assembler use new register coding

  Revision 1.12  2003/08/17 16:59:20  jonas
    * fixed regvars so they work with newra (at least for ppc)
    * fixed some volatile register bugs
    + -dnotranslation option for -dnewra, which causes the registers not to
      be translated from virtual to normal registers. Requires support in
      the assembler writer as well, which is only implemented in aggas/
      agppcgas currently

  Revision 1.11  2003/07/06 21:50:33  jonas
    * fixed ppc compilation problems and changed VOLATILE_REGISTERS for x86
      so that it doesn't include ebp and esp anymore

  Revision 1.10  2003/06/17 16:34:45  jonas
    * lots of newra fixes (need getfuncretparaloc implementation for i386)!
    * renamed all_intregisters to volatile_intregisters and made it
      processor dependent

  Revision 1.9  2003/06/13 21:19:33  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.8  2003/06/12 19:11:34  jonas
    - removed ALL_INTREGISTERS (only the one in rgobj is valid)

  Revision 1.7  2003/06/03 21:11:09  peter
    * cg.a_load_* get a from and to size specifier
    * makeregsize only accepts newregister
    * i386 uses generic tcgnotnode,tcgunaryminus

  Revision 1.6  2003/06/03 13:01:59  daniel
    * Register allocator finished

  Revision 1.5  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.4  2003/04/30 20:53:32  florian
    * error when address of an abstract method is taken
    * fixed some x86-64 problems
    * merged some more x86-64 and i386 code

  Revision 1.3  2002/04/25 20:15:40  florian
    * block nodes within expressions shouldn't release the used registers,
      fixed using a flag till the new rg is ready

  Revision 1.2  2002/04/25 16:12:09  florian
    * fixed more problems with cpubase and x86-64

  Revision 1.1  2003/04/25 11:12:09  florian
    * merged i386/cpubase and x86_64/cpubase to x86/cpubase;
      different stuff went to cpubase.inc

  Revision 1.50  2003/04/25 08:25:26  daniel
    * Ifdefs around a lot of calls to cleartempgen
    * Fixed registers that are allocated but not freed in several nodes
    * Tweak to register allocator to cause less spills
    * 8-bit registers now interfere with esi,edi and ebp
      Compiler can now compile rtl successfully when using new register
      allocator

  Revision 1.49  2003/04/22 23:50:23  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.48  2003/04/22 14:33:38  peter
    * removed some notes/hints

  Revision 1.47  2003/04/22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.46  2003/04/21 19:16:50  peter
    * count address regs separate

  Revision 1.45  2003/03/28 19:16:57  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.44  2003/03/18 18:15:53  peter
    * changed reg2opsize to function

  Revision 1.43  2003/03/08 08:59:07  daniel
    + $define newra will enable new register allocator
    + getregisterint will return imaginary registers with $newra
    + -sr switch added, will skip register allocation so you can see
      the direct output of the code generator before register allocation

  Revision 1.42  2003/02/19 22:00:15  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.41  2003/02/02 19:25:54  carl
    * Several bugfixes for m68k target (register alloc., opcode emission)
    + VIS target
    + Generic add more complete (still not verified)

  Revision 1.40  2003/01/13 18:37:44  daniel
    * Work on register conversion

  Revision 1.39  2003/01/09 20:41:00  daniel
    * Converted some code in cgx86.pas to new register numbering

  Revision 1.38  2003/01/09 15:49:56  daniel
    * Added register conversion

  Revision 1.37  2003/01/08 22:32:36  daniel
    * Added register convesrion procedure

  Revision 1.36  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.35  2003/01/05 13:36:53  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.34  2002/11/17 18:26:16  mazen
  * fixed a compilation bug accmulator-->FUNCTION_RETURN_REG, in definition of return_result_reg

  Revision 1.33  2002/11/17 17:49:08  mazen
  + return_result_reg and FUNCTION_RESULT_REG are now used, in all plateforms, to pass functions result between called function and its caller. See the explanation of each one

  Revision 1.32  2002/10/05 12:43:29  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.31  2002/08/14 18:41:48  jonas
    - remove valuelow/valuehigh fields from tlocation, because they depend
      on the endianess of the host operating system -> difficult to get
      right. Use lo/hi(location.valueqword) instead (remember to use
      valueqword and not value!!)

  Revision 1.30  2002/08/13 21:40:58  florian
    * more fixes for ppc calling conventions

  Revision 1.29  2002/08/12 15:08:41  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.28  2002/08/06 20:55:23  florian
    * first part of ppc calling conventions fix

  Revision 1.27  2002/07/25 18:01:29  carl
    + FPURESULTREG -> FPU_RESULT_REG

  Revision 1.26  2002/07/07 09:52:33  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.25  2002/07/01 18:46:30  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.24  2002/07/01 16:23:55  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.23  2002/05/18 13:34:22  peter
    * readded missing revisions

  Revision 1.22  2002/05/16 19:46:50  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.19  2002/05/12 16:53:16  peter
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

  Revision 1.18  2002/04/21 15:31:40  carl
  - removed some other stuff to their units

  Revision 1.17  2002/04/20 21:37:07  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant
  * removing frame pointer in routines is only available for : i386,m68k and vis targets

  Revision 1.16  2002/04/15 19:53:54  peter
    * fixed conflicts between the last 2 commits

  Revision 1.15  2002/04/15 19:44:20  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.14  2002/04/15 19:12:09  carl
  + target_info.size_of_pointer -> pointer_size
  + some cleanup of unused types/variables
  * move several constants from cpubase to their specific units
    (where they are used)
  + att_Reg2str -> gas_reg2str
  + int_reg2str -> std_reg2str

  Revision 1.13  2002/04/14 16:59:41  carl
  + att_reg2str -> gas_reg2str

  Revision 1.12  2002/04/02 17:11:34  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.11  2002/03/31 20:26:37  jonas
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

  Revision 1.10  2002/03/04 19:10:12  peter
    * removed compiler warnings

}
