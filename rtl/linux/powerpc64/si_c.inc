{*
 * Startup code for programs linked with GNU libc, PowerPC64
 * version.
 *
 * Adapted from the glibc-sources (2.16) in the file
 *
 *     sysdeps/powerpc/powerpc64/elf/start.S
 *
 * Original header follows.
 *}

{* Startup code for programs linked with GNU libc.  PowerPC64 version.
   Copyright (C) 1998,1999,2000,2001,2002,2003,2009
   Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   In addition to the permissions in the GNU Lesser General Public
   License, the Free Software Foundation gives you unlimited
   permission to link the compiled version of this file with other
   programs, and to distribute those programs without any restriction
   coming from the use of this file. (The GNU Lesser General Public
   License restrictions do apply in other respects; for example, they
   cover modification of the file, and distribution when not linked
   into another program.)

   Note that people who make modified versions of this file are not
   obligated to grant this special exception for their modified
   versions; it is their choice whether to do so. The GNU Lesser
   General Public License gives permission to release a modified
   version without this exception; this exception also makes it
   possible to release a modified version which carries forward this
   exception.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  *}

{$goto on}

{$ifndef _CALL_ELF or (_CALL_ELF = 1)}
{*
 * "ptrgl" glue code for calls via pointer. This function
 * sequence loads the data from the function descriptor
 * referenced by R11 into the CTR register (function address),
 * R2 (GOT/TOC pointer), and R11 (the outer frame pointer).
 *
 * On entry, R11 must be set to point to the function descriptor.
 *
 * See also the 64-bit PowerPC ABI specification for more
 * information, chapter 3.5.11 (in v1.7).
 *}
procedure ptrgl;  cdecl; public; assembler; nostackframe;
  asm
    ld	  r0, 0(r11)
    std     r2, 40(r1)
    mtctr   r0
    ld      r2, 8(r11)
    ld      r11, 16(r11)
    bctr
  end;
{$endif _CALL_ELF}


{*
 * Function prolog/epilog helpers, which are part of the 64-bit
 * PowerPC ABI.
 *
 * See also the 64-bit PowerPC ABI specification for more
 * information, chapter 3.5.5, "Register saving and restoring
 * function" (in v1.7).
 *}

{* Each _savegpr0_N routine saves the general registers from rN to r31,
 * inclusive. When the routine is called, r1 must point to the start
 * of the general register save area. R0 must contain the old LR on
 * entry.
 *}
label
  _savegpr0_15,
  _savegpr0_16,
  _savegpr0_17,
  _savegpr0_18,
  _savegpr0_19,
  _savegpr0_20,
  _savegpr0_21,
  _savegpr0_22,
  _savegpr0_23,
  _savegpr0_24,
  _savegpr0_25,
  _savegpr0_26,
  _savegpr0_27,
  _savegpr0_28,
  _savegpr0_29,
  _savegpr0_30,
  _savegpr0_31;

procedure _savegpr0_14; cdecl; public; assembler; nostackframe;
  asm
.globl _savegpr0_15
.globl _savegpr0_16
.globl _savegpr0_17
.globl _savegpr0_18
.globl _savegpr0_19
.globl _savegpr0_20
.globl _savegpr0_21
.globl _savegpr0_22
.globl _savegpr0_23
.globl _savegpr0_24
.globl _savegpr0_25
.globl _savegpr0_26
.globl _savegpr0_27
.globl _savegpr0_28
.globl _savegpr0_29
.globl _savegpr0_30
.globl _savegpr0_31
    std r14,-144(r1)
_savegpr0_15:
    std r15,-136(r1)
_savegpr0_16:
    std r16,-128(r1)
_savegpr0_17:
    std r17,-120(r1)
_savegpr0_18:
    std r18,-112(r1)
_savegpr0_19:
    std r19,-104(r1)
_savegpr0_20:
    std r20,-96(r1)
_savegpr0_21:
    std r21,-88(r1)
_savegpr0_22:
    std r22,-80(r1)
_savegpr0_23:
    std r23,-72(r1)
_savegpr0_24:
    std r24,-64(r1)
_savegpr0_25:
    std r25,-56(r1)
_savegpr0_26:
    std r26,-48(r1)
_savegpr0_27:
    std r27,-40(r1)
_savegpr0_28:
    std r28,-32(r1)
_savegpr0_29:
    std r29,-24(r1)
_savegpr0_30:
    std r30,-16(r1)
_savegpr0_31:
    std r31,-8(r1)
    std r0, 16(r1)
    blr
  end;


label
  _restgpr0_15,
  _restgpr0_16,
  _restgpr0_17,
  _restgpr0_18,
  _restgpr0_19,
  _restgpr0_20,
  _restgpr0_21,
  _restgpr0_22,
  _restgpr0_23,
  _restgpr0_24,
  _restgpr0_25,
  _restgpr0_26,
  _restgpr0_27,
  _restgpr0_28,
  _restgpr0_29,
  _restgpr0_31;

{* Each _restgpr0_N routine restores the general registers from rN to r31,
 * inclusive. When the routine is called, r1 must point to the start
 * of the general register save area.
 *}
procedure _restgpr0_14; cdecl; public; assembler; nostackframe;
  asm
.globl _restgpr0_15
.globl _restgpr0_16
.globl _restgpr0_17
.globl _restgpr0_18
.globl _restgpr0_19
.globl _restgpr0_20
.globl _restgpr0_21
.globl _restgpr0_22
.globl _restgpr0_23
.globl _restgpr0_24
.globl _restgpr0_25
.globl _restgpr0_26
.globl _restgpr0_27
.globl _restgpr0_28
.globl _restgpr0_29
    ld r14,-144(r1)
_restgpr0_15:
    ld r15,-136(r1)
_restgpr0_16:
    ld r16,-128(r1)
_restgpr0_17:
    ld r17,-120(r1)
_restgpr0_18:
    ld r18,-112(r1)
_restgpr0_19:
    ld r19,-104(r1)
_restgpr0_20:
    ld r20,-96(r1)
_restgpr0_21:
    ld r21,-88(r1)
_restgpr0_22:
    ld r22,-80(r1)
_restgpr0_23:
    ld r23,-72(r1)
_restgpr0_24:
    ld r24,-64(r1)
_restgpr0_25:
    ld r25,-56(r1)
_restgpr0_26:
    ld r26,-48(r1)
_restgpr0_27:
    ld r27,-40(r1)
_restgpr0_28:
    ld r28,-32(r1)
_restgpr0_29:
    ld r0, 16(r1)
    ld r29,-24(r1)
    mtlr r0
    ld r30,-16(r1)
    ld r31,-8(r1)
    blr
  end;


procedure _restgpr0_30; cdecl; public; assembler; nostackframe;
  asm
    ld r30,-16(r1)
_restgpr0_31:
    ld r0, 16(r1)
    ld r31,-8(r1)
    mtlr 0
    blr
  end;


{* Each _savegpr1_N routine saves the general registers from rN to r31,
 * inclusive. When the routine is called, r12
 * must point to the start of the general register save area.
 *}

label
  _savegpr1_15,
  _savegpr1_16,
  _savegpr1_17,
  _savegpr1_18,
  _savegpr1_19,
  _savegpr1_20,
  _savegpr1_21,
  _savegpr1_22,
  _savegpr1_23,
  _savegpr1_24,
  _savegpr1_25,
  _savegpr1_26,
  _savegpr1_27,
  _savegpr1_28,
  _savegpr1_29,
  _savegpr1_30,
  _savegpr1_31;

procedure _savegpr1_14; cdecl; public; assembler; nostackframe;
  asm
.globl _savegpr1_15
.globl _savegpr1_16
.globl _savegpr1_17
.globl _savegpr1_18
.globl _savegpr1_19
.globl _savegpr1_20
.globl _savegpr1_21
.globl _savegpr1_22
.globl _savegpr1_23
.globl _savegpr1_24
.globl _savegpr1_25
.globl _savegpr1_26
.globl _savegpr1_27
.globl _savegpr1_28
.globl _savegpr1_29
.globl _savegpr1_30
.globl _savegpr1_31

    std r14,-144(r12)
_savegpr1_15:
    std r15,-136(r12)
_savegpr1_16:
    std r16,-128(r12)
_savegpr1_17:
    std r17,-120(r12)
_savegpr1_18:
    std r18,-112(r12)
_savegpr1_19:
    std r19,-104(r12)
_savegpr1_20:
    std r20,-96(r12)
_savegpr1_21:
    std r21,-88(r12)
_savegpr1_22:
    std r22,-80(r12)
_savegpr1_23:
    std r23,-72(r12)
_savegpr1_24:
    std r24,-64(r12)
_savegpr1_25:
    std r25,-56(r12)
_savegpr1_26:
    std r26,-48(r12)
_savegpr1_27:
    std r27,-40(r12)
_savegpr1_28:
    std r28,-32(r12)
_savegpr1_29:
    std r29,-24(r12)
_savegpr1_30:
    std r30,-16(r12)
_savegpr1_31:
    std r31,-8(r12)
    blr
end;


{* The _restgpr1_N routines restore the general registers from rN to r31.
 * When the routine is called, r12 must point to the start of the general
 * register save area.
 *}
label
  _restgpr1_15,
  _restgpr1_16,
  _restgpr1_17,
  _restgpr1_18,
  _restgpr1_19,
  _restgpr1_20,
  _restgpr1_21,
  _restgpr1_22,
  _restgpr1_23,
  _restgpr1_24,
  _restgpr1_25,
  _restgpr1_26,
  _restgpr1_27,
  _restgpr1_28,
  _restgpr1_29,
  _restgpr1_30,
  _restgpr1_31;

procedure _restgpr1_14; cdecl; public; assembler; nostackframe;
  asm
.globl _restgpr1_15
.globl _restgpr1_16
.globl _restgpr1_17
.globl _restgpr1_18
.globl _restgpr1_19
.globl _restgpr1_20
.globl _restgpr1_21
.globl _restgpr1_22
.globl _restgpr1_23
.globl _restgpr1_24
.globl _restgpr1_25
.globl _restgpr1_26
.globl _restgpr1_27
.globl _restgpr1_28
.globl _restgpr1_29
.globl _restgpr1_30
.globl _restgpr1_31

    ld r14,-144(r12)
_restgpr1_15:
    ld r15,-136(r12)
_restgpr1_16:
    ld r16,-128(r12)
_restgpr1_17:
    ld r17,-120(r12)
_restgpr1_18:
    ld r18,-112(r12)
_restgpr1_19:
    ld r19,-104(r12)
_restgpr1_20:
    ld r20,-96(r12)
_restgpr1_21:
    ld r21,-88(r12)
_restgpr1_22:
    ld r22,-80(r12)
_restgpr1_23:
    ld r23,-72(r12)
_restgpr1_24:
    ld r24,-64(r12)
_restgpr1_25:
    ld r25,-56(r12)
_restgpr1_26:
    ld r26,-48(r12)
_restgpr1_27:
    ld r27,-40(r12)
_restgpr1_28:
    ld r28,-32(r12)
_restgpr1_29:
    ld r29,-24(r12)
_restgpr1_30:
    ld r30,-16(r12)
_restgpr1_31:
    ld r31,-8(r12)
    blr
  end;

{* Each _savefpr_M routine saves the floating point registers from fM to f31,
 * inclusive. When the routine is called, r1 must point to the start of the
 * floating point register save area, and r0 must contain the value of LR on
 * function entry.
 *}
label
  _savefpr_15,
  _savefpr_16,
  _savefpr_17,
  _savefpr_18,
  _savefpr_19,
  _savefpr_20,
  _savefpr_21,
  _savefpr_22,
  _savefpr_23,
  _savefpr_24,
  _savefpr_25,
  _savefpr_26,
  _savefpr_27,
  _savefpr_28,
  _savefpr_29,
  _savefpr_30,
  _savefpr_31;

procedure _savefpr_14; cdecl; public; assembler; nostackframe;
  asm
.globl _savefpr_15
.globl _savefpr_16
.globl _savefpr_17
.globl _savefpr_18
.globl _savefpr_19
.globl _savefpr_20
.globl _savefpr_21
.globl _savefpr_22
.globl _savefpr_23
.globl _savefpr_24
.globl _savefpr_25
.globl _savefpr_26
.globl _savefpr_27
.globl _savefpr_28
.globl _savefpr_29
.globl _savefpr_30
.globl _savefpr_31

    stfd r14,-144(r1)
_savefpr_15:
    stfd r15,-136(r1)
_savefpr_16:
    stfd r16,-128(r1)
_savefpr_17:
    stfd r17,-120(r1)
_savefpr_18:
    stfd r18,-112(r1)
_savefpr_19:
    stfd r19,-104(r1)
_savefpr_20:
    stfd r20,-96(r1)
_savefpr_21:
    stfd r21,-88(r1)
_savefpr_22:
    stfd r22,-80(r1)
_savefpr_23:
    stfd r23,-72(r1)
_savefpr_24:
    stfd r24,-64(r1)
_savefpr_25:
    stfd r25,-56(r1)
_savefpr_26:
    stfd r26,-48(r1)
_savefpr_27:
    stfd r27,-40(r1)
_savefpr_28:
    stfd r28,-32(r1)
_savefpr_29:
    stfd r29,-24(r1)
_savefpr_30:
    stfd r30,-16(r1)
_savefpr_31:
    stfd r31,-8(r1)
    std r0, 16(r1)
    blr
  end;

{* The _restfpr_M routines restore the floating point registers from fM to f31.
 * When the routine is called, r1 must point to the start of the floating point
 * register save area.
 *}
label
  _restfpr_15,
  _restfpr_16,
  _restfpr_17,
  _restfpr_18,
  _restfpr_19,
  _restfpr_20,
  _restfpr_21,
  _restfpr_22,
  _restfpr_23,
  _restfpr_24,
  _restfpr_25,
  _restfpr_26,
  _restfpr_27,
  _restfpr_28,
  _restfpr_29,
  _restfpr_31;

procedure _restfpr_14; cdecl; public; assembler; nostackframe;
  asm
.globl _restfpr_15
.globl _restfpr_16
.globl _restfpr_17
.globl _restfpr_18
.globl _restfpr_19
.globl _restfpr_20
.globl _restfpr_21
.globl _restfpr_22
.globl _restfpr_23
.globl _restfpr_24
.globl _restfpr_25
.globl _restfpr_26
.globl _restfpr_27
.globl _restfpr_28
.globl _restfpr_29

    lfd r14,-144(r1)
_restfpr_15:
    lfd r15,-136(r1)
_restfpr_16:
    lfd r16,-128(r1)
_restfpr_17:
    lfd r17,-120(r1)
_restfpr_18:
    lfd r18,-112(r1)
_restfpr_19:
    lfd r19,-104(r1)
_restfpr_20:
    lfd r20,-96(r1)
_restfpr_21:
    lfd r21,-88(r1)
_restfpr_22:
    lfd r22,-80(r1)
_restfpr_23:
    lfd r23,-72(r1)
_restfpr_24:
    lfd r24,-64(r1)
_restfpr_25:
    lfd r25,-56(r1)
_restfpr_26:
    lfd r26,-48(r1)
_restfpr_27:
    lfd r27,-40(r1)
_restfpr_28:
    lfd r28,-32(r1)
_restfpr_29:
    ld r0, 16(r1)
    lfd r29,-24(r1)
    mtlr r0
    lfd r30,-16(r1)
    lfd r31,-8(r1)
    blr
  end;


procedure _restfpr_30; cdecl; public; assembler; nostackframe;
  asm
.globl _restfpr_31
    lfd r30,-16(r1)
_restfpr_31:
    ld r0, 16(r1)
    lfd r31,-8(r1)
    mtlr r0
    blr
  end;

{* Each _savevr_M routine saves the vector registers from vM to v31, inclusive.
 * When the routine is called, r0 must point to the word just beyound the end
 * of the vector register save area. On return the value of r0 is unchanged
 * while r12 may be modified.
 *}
{* commented out for now, unused
_savevr_20: addi r12,r0,-192
    stvx v20,r12,r0
_savevr_21: addi r12,r0,-176
    stvx v21,r12,r0
_savevr_22: addi r12,r0,-160
    stvx v22,r12,r0
_savevr_23: addi r12,r0,-144
    stvx v23,r12,r0
_savevr_24: addi r12,r0,-128
    stvx v24,r12,r0
_savevr_25: addi r12,r0,-112
    stvx v25,r12,r0
_savevr_26: addi r12,r0,-96
    stvx v26,r12,r0
_savevr_27: addi r12,r0,-80
    stvx v27,r12,r0
_savevr_28: addi r12,r0,-64
    stvx v28,r12,r0
_savevr_29: addi r12,r0,-48
    stvx v29,r12,r0
_savevr_30: addi r12,r0,-32
    stvx v30,r12,r0
_savevr_31: addi r12,r0,-16
    stvx v31,r12,r0
    blr
*}
{* The _restvr_M routines restore the vector registers from vM to v31. When the
 * routine is called, r0 must point to the word just beyound the end of the
 * vector register save area. On return the value of r0 is unchanged while r12
 * may be modified.
 *}
{* commented out for now, unused
_restvr_20: addi r12,r0,-192
    lvx v20,r12,r0
_restvr_21: addi r12,r0,-176
    lvx v21,r12,r0
_restvr_22: addi r12,r0,-160
    lvx v22,r12,r0
_restvr_23: addi r12,r0,-144
    lvx v23,r12,r0
_restvr_24: addi r12,r0,-128
    lvx v24,r12,r0
_restvr_25: addi r12,r0,-112
    lvx v25,r12,r0
_restvr_26: addi r12,r0,-96
    lvx v26,r12,r0
_restvr_27: addi r12,r0,-80
    lvx v27,r12,r0
_restvr_28: addi r12,r0,-64
    lvx v28,r12,r0
_restvr_29: addi r12,r0,-48
    lvx v29,r12,r0
_restvr_30: addi r12,r0,-32
    lvx v30,r12,r0
_restvr_31: addi r12,r0,-16
    lvx v31,r12,r0
    blr
*}

{******************************************************************************
                          Process start/halt
 ******************************************************************************}

procedure __libc_start_main(r3,r4,r5,r6,r7,r8,r9: pointer); cdecl; external;

var
  __fpc_ret: pointer;

procedure save_argc_argv_envp_stkptr_fpcret(argc: longint; argv, envp: ppchar; stkptr: pointer);
  begin
    operatingsystem_parameter_argc:=argc;
    operatingsystem_parameter_argv:=argv;
    operatingsystem_parameter_envp:=envp;
    initialstkptr:=stkptr;
    __fpc_ret:=stkptr;
  end;


procedure _haltproc(exitcode: longint); cdecl; forward;

var
  TOCSTART: pointer; external name '.TOC.';

label
  __data_start,
  data_start;

{*
 * This is our FreePascal main procedure which is called by
 * libc after initializing.
 *}
procedure main_stub(argc: longint; argv, envp: ppchar); cdecl; assembler; nostackframe;
  asm
{$if defined(_CALL_ELF) and (_CALL_ELF = 2)}
.L1:
    addis   r2,r12,(TOCSTART-.L1)@ha
    addi    r2,r2,(TOCSTART-.L1)@l
    .localentry main_stub, . - main_stub
{$endif}
    mflr    r0
    std     r0,16(r1)
    stdu    r1,-128(r1)

    mr      r6, r1
    bl      save_argc_argv_envp_stkptr_fpcret

    bl  PASCALMAIN
    nop

    b   _haltproc
    nop
.section ".data"
.globl  __data_start
__data_start:
data_start:
.text
  end;

procedure ini_dummy;
  begin
  end;

const
  start_addresses: record
    sda_base: pointer;
    main: pointer;
    libc_csu_init: pointer;
    libc_csu_fini: pointer;
  end
  = (sda_base: nil; {* was _SDA_BASE_  but not in 64-bit ABI }
     main: @main_stub;
     { use dummy, it should not be used anymore by glibc }
     libc_csu_init: @ini_dummy;
     { use dummy, it should not be used anymore by glibc }
     libc_csu_fini: @ini_dummy
    );


procedure call_libc_start_main(r3,r4,r5,r6,r7,r8,r9: pointer); cdecl;
  begin
    __libc_start_main(r3,r4,r5,r6,r7,@start_addresses,r9);
    asm
      { not reached }
      { trap }
      .long 0xffe00008
    end;
  end;

{*
 * the real entry point for the program
 *}
procedure _start; cdecl; public; assembler; nostackframe;
  asm
{$if defined(_CALL_ELF) and (_CALL_ELF = 2)}
.L1:
    addis   r2,r12,(TOCSTART-.L1)@ha
    addi    r2,r2,(TOCSTART-.L1)@l
    .localentry _start, . - _start
{$endif}
    mr  r9,r1                   { save the stack pointer }

    { Set up an initial stack frame, and clear the LR.  }
    rldicr  r1,r1,0,59
    li      r0,0
    stdu    r1,-128(r1)
    mtlr    r0
    std     r0,0(r1)

    bl call_libc_start_main
    nop
  end;


procedure _haltproc(exitcode: longint); cdecl; public;
  var
    localret: pointer;
  begin
    localret:=__fpc_ret;
    { return via initial stack }
    asm
      lwa  r3, exitcode
      ld   r1, localret
      addi r1, r1, 128
      ld   r0, 16(r1)
      mtlr r0
      blr
    end;
  end;

