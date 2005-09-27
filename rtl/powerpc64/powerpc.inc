{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2000-2001 by the Free Pascal development team.

    Portions Copyright (c) 2000 by Casey Duncan (casey.duncan@state.co.us)

    Processor dependent implementation for the system unit for
    PowerPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                           PowerPC specific stuff
****************************************************************************}
{
const
  ppc_fpu_overflow     = (1 shl (32-3));
  ppc_fpu_underflow    = (1 shl (32-4));
  ppc_fpu_divbyzero    = (1 shl (32-5));
  ppc_fpu_inexact      = (1 shl (32-6));
  ppc_fpu_invalid_snan = (1 shl (32-7));
}

procedure fpc_enable_ppc_fpu_exceptions;
assembler; nostackframe;
asm
  { clear all "exception happened" flags we care about}
  mtfsfi 0,0
  mtfsfi 1,0
  mtfsfi 2,0
  mtfsfi 3,0
{$ifdef fpc_mtfsb0_corrected}
  mtfsb0 21
  mtfsb0 22
  mtfsb0 23

{$endif fpc_mtfsb0_corrected}

  { enable invalid operations and division by zero exceptions. }
  { No overflow/underflow, since those give some spurious      }
  { exceptions                                                 }
  mtfsfi 6,9
end;


procedure fpc_cpuinit;
begin
  fpc_enable_ppc_fpu_exceptions;
end;


function fpc_get_ppc_fpscr: cardinal;
assembler;
var
  temp: record a,b:longint; end;
asm
  mffs f0
  stfd f0,temp
  lwz  r3,temp.b
  { clear all exception flags }
{
  rlwinm r4,r3,0,16,31
  stw  r4,temp.b
  lfd  f0,temp
  a_mtfsf f0
}
end;


{ note: unused}
{ The following code is never called directly, it's a dummy which holds the
entry points and code to the register save/load subroutines; it is part of the
PPC ABI and used in procedure entry and exit methods.
See the comments in the code for "calling conventions". Directly taken from
the ABI specification. The labels right below are required to shut up the
compiler. }

label
        // _savegpr0_x
        _savegpr0_14, _savegpr0_15, _savegpr0_16, _savegpr0_17, _savegpr0_18, _savegpr0_19,
        _savegpr0_20, _savegpr0_21, _savegpr0_22, _savegpr0_23, _savegpr0_24, _savegpr0_25,
        _savegpr0_26, _savegpr0_27, _savegpr0_28, _savegpr0_29, _savegpr0_30, _savegpr0_31,
        // _restgpr0_x
        _restgpr0_14, _restgpr0_15, _restgpr0_16, _restgpr0_17, _restgpr0_18, _restgpr0_19,
        _restgpr0_20, _restgpr0_21, _restgpr0_22, _restgpr0_23, _restgpr0_24, _restgpr0_25,
        _restgpr0_26, _restgpr0_27, _restgpr0_28, _restgpr0_29, _restgpr0_30, _restgpr0_31,
        // _savegpr1_x
        _savegpr1_14, _savegpr1_15, _savegpr1_16, _savegpr1_17, _savegpr1_18, _savegpr1_19,
        _savegpr1_20, _savegpr1_21, _savegpr1_22, _savegpr1_23, _savegpr1_24, _savegpr1_25,
        _savegpr1_26, _savegpr1_27, _savegpr1_28, _savegpr1_29, _savegpr1_30, _savegpr1_31,
        // _restgpr1_x
        _restgpr1_14, _restgpr1_15, _restgpr1_16, _restgpr1_17, _restgpr1_18, _restgpr1_19,
        _restgpr1_20, _restgpr1_21, _restgpr1_22, _restgpr1_23, _restgpr1_24, _restgpr1_25,
        _restgpr1_26, _restgpr1_27, _restgpr1_28, _restgpr1_29, _restgpr1_30, _restgpr1_31,
        // _savefpr_x
        _savefpr_14, _savefpr_15, _savefpr_16, _savefpr_17, _savefpr_18, _savefpr_19,
        _savefpr_20, _savefpr_21, _savefpr_22, _savefpr_23, _savefpr_24, _savefpr_25,
        _savefpr_26, _savefpr_27, _savefpr_28, _savefpr_29, _savefpr_30, _savefpr_31,
        // _restfpr_x
        _restfpr_14, _restfpr_15, _restfpr_16, _restfpr_17, _restfpr_18, _restfpr_19,
        _restfpr_20, _restfpr_21, _restfpr_22, _restfpr_23, _restfpr_24, _restfpr_25,
        _restfpr_26, _restfpr_27, _restfpr_28, _restfpr_29, _restfpr_30, _restfpr_31,
        // _savevr_x
        _savevr_20, _savevr_21, _savevr_22, _savevr_23, _savevr_24, _savevr_25,
        _savevr_26, _savevr_27, _savevr_28, _savevr_29, _savevr_30, _savevr_31,
        // _restvr_x
        _restvr_20, _restvr_21, _restvr_22, _restvr_23, _restvr_24, _restvr_25,
        _restvr_26, _restvr_27, _restvr_28, _restvr_29, _restvr_30, _restvr_31;


procedure __save_restore_services; assembler; nostackframe;
assembler;
asm
// Each _savegpr0_N routine saves the general registers from rN to r31, inclusive.
// Each routine also saves the LR. When the routine is called, r1 must point to
// the start of the general register save area, and r0 must contain the
// value of LR on function entry.
.globl _savegpr0_14
_savegpr0_14: std r14,-144(r1)
.globl _savegpr0_15
_savegpr0_15: std r15,-136(r1)
.globl _savegpr0_16
_savegpr0_16: std r16,-128(r1)
.globl _savegpr0_17
_savegpr0_17: std r17,-120(r1)
.globl _savegpr0_18
_savegpr0_18: std r18,-112(r1)
.globl _savegpr0_19
_savegpr0_19: std r19,-104(r1)
.globl _savegpr0_20
_savegpr0_20: std r20,-96(r1)
.globl _savegpr0_21
_savegpr0_21: std r21,-88(r1)
.globl _savegpr0_22
_savegpr0_22: std r22,-80(r1)
.globl _savegpr0_23
_savegpr0_23: std r23,-72(r1)
.globl _savegpr0_24
_savegpr0_24: std r24,-64(r1)
.globl _savegpr0_25
_savegpr0_25: std r25,-56(r1)
.globl _savegpr0_26
_savegpr0_26: std r26,-48(r1)
.globl _savegpr0_27
_savegpr0_27: std r27,-40(r1)
.globl _savegpr0_28
_savegpr0_28: std r28,-32(r1)
.globl _savegpr0_29
_savegpr0_29: std r29,-24(r1)
.globl _savegpr0_30
_savegpr0_30: std r30,-16(r1)
.globl _savegpr0_31
_savegpr0_31: std r31,-8(r1)
        std r0, 16(r1)
        blr
// The _restgpr0_N routines restore the general registers from rN to r31, and then
// return to the caller. When the routine is called, r1 must point to the start of
// the general register save area.
.globl _restgpr0_14
_restgpr0_14: ld r14,-144(r1)
.globl _restgpr0_15
_restgpr0_15: ld r15,-136(r1)
.globl _restgpr0_16
_restgpr0_16: ld r16,-128(r1)
.globl _restgpr0_17
_restgpr0_17: ld r17,-120(r1)
.globl _restgpr0_18
_restgpr0_18: ld r18,-112(r1)
.globl _restgpr0_19
_restgpr0_19: ld r19,-104(r1)
.globl _restgpr0_20
_restgpr0_20: ld r20,-96(r1)
.globl _restgpr0_21
_restgpr0_21: ld r21,-88(r1)
.globl _restgpr0_22
_restgpr0_22: ld r22,-80(r1)
.globl _restgpr0_23
_restgpr0_23: ld r23,-72(r1)
.globl _restgpr0_24
_restgpr0_24: ld r24,-64(r1)
.globl _restgpr0_25
_restgpr0_25: ld r25,-56(r1)
.globl _restgpr0_26
_restgpr0_26: ld r26,-48(r1)
.globl _restgpr0_27
_restgpr0_27: ld r27,-40(r1)
.globl _restgpr0_28
_restgpr0_28: ld r28,-32(r1)
.globl _restgpr0_29
_restgpr0_29: ld r0, 16(r1)
        ld r29,-24(r1)
        mtlr r0
        ld r30,-16(r1)
        ld r31,-8(r1)
        blr
.globl _restgpr0_30
_restgpr0_30: ld r30,-16(r1)
.globl _restgpr0_31
_restgpr0_31: ld r0, 16(r1)
        ld r31,-8(r1)
        mtlr r0
        blr
// Each _savegpr1_N routine saves the general registers from rN to r31,
// inclusive. When the routine is called, r12
// must point to the start of the general register save area.
.globl _savegpr1_14
_savegpr1_14: std r14,-144(r12)
.globl _savegpr1_15
_savegpr1_15: std r15,-136(r12)
.globl _savegpr1_16
_savegpr1_16: std r16,-128(r12)
.globl _savegpr1_17
_savegpr1_17: std r17,-120(r12)
.globl _savegpr1_18
_savegpr1_18: std r18,-112(r12)
.globl _savegpr1_19
_savegpr1_19: std r19,-104(r12)
.globl _savegpr1_20
_savegpr1_20: std r20,-96(r12)
.globl _savegpr1_21
_savegpr1_21: std r21,-88(r12)
.globl _savegpr1_22
_savegpr1_22: std r22,-80(r12)
.globl _savegpr1_23
_savegpr1_23: std r23,-72(r12)
.globl _savegpr1_24
_savegpr1_24: std r24,-64(r12)
.globl _savegpr1_25
_savegpr1_25: std r25,-56(r12)
.globl _savegpr1_26
_savegpr1_26: std r26,-48(r12)
.globl _savegpr1_27
_savegpr1_27: std r27,-40(r12)
.globl _savegpr1_28
_savegpr1_28: std r28,-32(r12)
.globl _savegpr1_29
_savegpr1_29: std r29,-24(r12)
.globl _savegpr1_30
_savegpr1_30: std r30,-16(r12)
.globl _savegpr1_31
_savegpr1_31: std r31,-8(r12)
        blr
// The _restgpr1_N routines restore the general registers from rN to r31.
// When the routine is called, r12 must point to the start of the general
// register save area.
.globl _restgpr1_14
_restgpr1_14: ld r14,-144(r12)
.globl _restgpr1_15
_restgpr1_15: ld r15,-136(r12)
.globl _restgpr1_16
_restgpr1_16: ld r16,-128(r12)
.globl _restgpr1_17
_restgpr1_17: ld r17,-120(r12)
.globl _restgpr1_18
_restgpr1_18: ld r18,-112(r12)
.globl _restgpr1_19
_restgpr1_19: ld r19,-104(r12)
.globl _restgpr1_20
_restgpr1_20: ld r20,-96(r12)
.globl _restgpr1_21
_restgpr1_21: ld r21,-88(r12)
.globl _restgpr1_22
_restgpr1_22: ld r22,-80(r12)
.globl _restgpr1_23
_restgpr1_23: ld r23,-72(r12)
.globl _restgpr1_24
_restgpr1_24: ld r24,-64(r12)
.globl _restgpr1_25
_restgpr1_25: ld r25,-56(r12)
.globl _restgpr1_26
_restgpr1_26: ld r26,-48(r12)
.globl _restgpr1_27
_restgpr1_27: ld r27,-40(r12)
.globl _restgpr1_28
_restgpr1_28: ld r28,-32(r12)
.globl _restgpr1_29
_restgpr1_29: ld r29,-24(r12)
.globl _restgpr1_30
_restgpr1_30: ld r30,-16(r12)
.globl _restgpr1_31
_restgpr1_31: ld r31,-8(r12)
        blr

// Each _savefpr_M routine saves the floating point registers from fM to f31,
// inclusive. When the routine is called, r1 must point to the start of the
// floating point register save area, and r0 must contain the value of LR on
// function entry.
_savefpr_14: stfd f14,-144(r1)
_savefpr_15: stfd f15,-136(r1)
_savefpr_16: stfd f16,-128(r1)
_savefpr_17: stfd f17,-120(r1)
_savefpr_18: stfd f18,-112(r1)
_savefpr_19: stfd f19,-104(r1)
_savefpr_20: stfd f20,-96(r1)
_savefpr_21: stfd f21,-88(r1)
_savefpr_22: stfd f22,-80(r1)
_savefpr_23: stfd f23,-72(r1)
_savefpr_24: stfd f24,-64(r1)
_savefpr_25: stfd f25,-56(r1)
_savefpr_26: stfd f26,-48(r1)
_savefpr_27: stfd f27,-40(r1)
_savefpr_28: stfd f28,-32(r1)
_savefpr_29: stfd f29,-24(r1)
_savefpr_30: stfd f30,-16(r1)
_savefpr_31: stfd f31,-8(r1)
        std r0, 16(r1)
        blr
// The _restfpr_M routines restore the floating point registers from fM to f31.
// When the routine is called, r1 must point to the start of the floating point
// register save area.
_restfpr_14: lfd f14,-144(r1)
_restfpr_15: lfd f15,-136(r1)
_restfpr_16: lfd f16,-128(r1)
_restfpr_17: lfd f17,-120(r1)
_restfpr_18: lfd f18,-112(r1)
_restfpr_19: lfd f19,-104(r1)
_restfpr_20: lfd f20,-96(r1)
_restfpr_21: lfd f21,-88(r1)
_restfpr_22: lfd f22,-80(r1)
_restfpr_23: lfd f23,-72(r1)
_restfpr_24: lfd f24,-64(r1)
_restfpr_25: lfd f25,-56(r1)
_restfpr_26: lfd f26,-48(r1)
_restfpr_27: lfd f27,-40(r1)
_restfpr_28: lfd f28,-32(r1)
_restfpr_29: lfd f29,-24(r1)
_restfpr_29: ld r0, 16(r1)
        lfd f29,-24(r1)
        mtlr r0
        lfd f30,-16(r1)
        lfd f31,-8(r1)
        blr
_restfpr_30: lfd f30,-16(r1)
_restfpr_31: ld r0, 16(r1)
        lfd f31,-8(r1)
        mtlr r0
        blr
// Each _savevr_M routine saves the vector registers from vM to v31, inclusive.
// When the routine is called, r0 must point to the word just beyound the end
// of the vector register save area. On return the value of r0 is unchanged
// while r12 may be modified.
(* commented out: GAS does not understand VMX opcodes?
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
*)
// The _restvr_M routines restore the vector registers from vM to v31. When the
// routine is called, r0 must point to the word just beyound the end of the
// vector register save area. On return the value of r0 is unchanged while r12
// may be modified.
(* commented out: GAS does not understand VMX opcodes?
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
*)
end;


{****************************************************************************
                                Move / Fill
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_MOVE}
{$define FPC_SYSTEM_HAS_MOVE}
procedure Move(const source;var dest;count:SizeInt);[public, alias: 'FPC_MOVE'];
type
  bytearray    = array [0..high(sizeint)-1] of byte;
var
  i:longint;
begin
  if count <= 0 then exit;
  Dec(count);
  if @source<@dest then
    begin
      for i:=count downto 0 do
        bytearray(dest)[i]:=bytearray(source)[i];
    end
  else
    begin
      for i:=0 to count do
        bytearray(dest)[i]:=bytearray(source)[i];
    end;
end;
{$endif FPC_SYSTEM_HAS_MOVE}


{$ifndef FPC_SYSTEM_HAS_FILLCHAR}
{$define FPC_SYSTEM_HAS_FILLCHAR}

Procedure FillChar(var x;count:SizeInt;value:byte);
type
  longintarray = array [0..high(sizeint) div 4-1] of longint;
  bytearray    = array [0..high(sizeint)-1] of byte;
var
  i,v : longint;
begin
  if count <= 0 then exit;
  v := 0;
  { aligned? }
  if (PtrUInt(@x) mod sizeof(PtrUInt))<>0 then
    begin
      for i:=0 to count-1 do
        bytearray(x)[i]:=value;
    end
  else
    begin
      v:=(value shl 8) or (value and $FF);
      v:=(v shl 16) or (v and $ffff);
      for i:=0 to (count div 4)-1 do
        longintarray(x)[i]:=v;
      for i:=(count div 4)*4 to count-1 do
        bytearray(x)[i]:=value;
    end;
end;
{$endif FPC_SYSTEM_HAS_FILLCHAR}


{$ifndef FPC_SYSTEM_HAS_FILLDWORD}
{$define FPC_SYSTEM_HAS_FILLDWORD}
procedure filldword(var x;count : SizeInt;value : dword);
assembler; nostackframe;
asm
{       registers:
        r3              x
        r4              count
        r5              value
}
                cmpdi   cr0,r4,0
                mtctr   r4
                subi    r3,r3,4
                ble    .LFillDWordEnd    //if count<=0 Then Exit
.LFillDWordLoop:
                stwu    r5,4(r3)
                bdnz    .LFillDWordLoop
.LFillDWordEnd:
end;
{$endif FPC_SYSTEM_HAS_FILLDWORD}

(*
{$ifndef FPC_SYSTEM_HAS_INDEXBYTE}
{$define FPC_SYSTEM_HAS_INDEXBYTE}
function IndexByte(const buf;len:SizeInt;b:byte):int64; assembler; nostackframe;
{ input: r3 = buf, r4 = len, r5 = b                   }
{ output: r3 = position of b in buf (-1 if not found) }
asm
                {  load the begin of the buffer in the data cache }
                dcbt    0,r3
                cmplwi  r4,0
                mtctr   r4
                subi    r10,r3,1
                mr      r0,r3
                { assume not found }
                li      r3,-1
                ble     .LIndexByteDone
.LIndexByteLoop:
                lbzu    r9,1(r10)
                cmplw   r9,r5
                bdnzf   cr0*4+eq,.LIndexByteLoop
                { r3 still contains -1 here }
                bne     .LIndexByteDone
                sub     r3,r10,r0
.LIndexByteDone:
end;
{$endif FPC_SYSTEM_HAS_INDEXBYTE}
*)
(*
{$ifndef FPC_SYSTEM_HAS_INDEXWORD}
{$define FPC_SYSTEM_HAS_INDEXWORD}
function IndexWord(const buf;len:SizeInt;b:word):int64; assembler; nostackframe;
{ input: r3 = buf, r4 = len, r5 = b                   }
{ output: r3 = position of b in buf (-1 if not found) }
asm
                {  load the begin of the buffer in the data cache }
                dcbt    0,r3
                cmplwi  r4,0
                mtctr   r4
                subi    r10,r3,2
                mr      r0,r3
                { assume not found }
                li      r3,-1
                ble     .LIndexWordDone
.LIndexWordLoop:
                lhzu    r9,2(r10)
                cmplw   r9,r5
                bdnzf   cr0*4+eq,.LIndexWordLoop
                { r3 still contains -1 here }
                bne     .LIndexWordDone
                sub     r3,r10,r0
                srawi   r3,r3,1
.LIndexWordDone:
end;
{$endif FPC_SYSTEM_HAS_INDEXWORD}
*)
(*
{$ifndef FPC_SYSTEM_HAS_INDEXDWORD}
{$define FPC_SYSTEM_HAS_INDEXDWORD}
function IndexDWord(const buf;len:SizeInt;b:DWord):int64; assembler; nostackframe;
{ input: r3 = buf, r4 = len, r5 = b                   }
{ output: r3 = position of b in buf (-1 if not found) }
asm
                {  load the begin of the buffer in the data cache }
                dcbt    0,r3
                cmplwi  r4,0
                mtctr   r4
                subi    r10,r3,4
                mr      r0,r3
                { assume not found }
                li      r3,-1
                ble     .LIndexDWordDone
.LIndexDWordLoop:
                lwzu    r9,4(r10)
                cmplw   r9,r5
                bdnzf   cr0*4+eq, .LIndexDWordLoop
                { r3 still contains -1 here }
                bne     .LIndexDWordDone
                sub     r3,r10,r0
                srawi   r3,r3,2
.LIndexDWordDone:
end;
{$endif FPC_SYSTEM_HAS_INDEXDWORD}
*)
(*
{$ifndef FPC_SYSTEM_HAS_COMPAREBYTE}
{$define FPC_SYSTEM_HAS_COMPAREBYTE}
function CompareByte(const buf1,buf2;len:SizeInt):int64; assembler; nostackframe;
{ input: r3 = buf1, r4 = buf2, r5 = len                           }
{ output: r3 = 0 if equal, < 0 if buf1 < str2, > 0 if buf1 > str2 }
{ note: almost direct copy of strlcomp() from strings.inc         }
asm
        {  load the begin of the first buffer in the data cache }
        dcbt    0,r3
        { use r0 instead of r3 for buf1 since r3 contains result }
        cmplwi  r5,0
        mtctr   r5
        subi    r11,r3,1
        subi    r4,r4,1
        li      r3,0
        ble     .LCompByteDone
.LCompByteLoop:
        { load next chars }
        lbzu    r9,1(r11)
        lbzu    r10,1(r4)
        { calculate difference }
        sub.    r3,r9,r10
        { if chars not equal or at the end, we're ready }
        bdnzt   cr0*4+eq, .LCompByteLoop
.LCompByteDone:
end;
{$endif FPC_SYSTEM_HAS_COMPAREBYTE}
*)
(*
{$ifndef FPC_SYSTEM_HAS_COMPAREWORD}
{$define FPC_SYSTEM_HAS_COMPAREWORD}
function CompareWord(const buf1,buf2;len:SizeInt):int64; assembler; nostackframe;
{ input: r3 = buf1, r4 = buf2, r5 = len                           }
{ output: r3 = 0 if equal, < 0 if buf1 < str2, > 0 if buf1 > str2 }
{ note: almost direct copy of strlcomp() from strings.inc         }
asm
        {  load the begin of the first buffer in the data cache }
        dcbt    0,r3
        { use r0 instead of r3 for buf1 since r3 contains result }
        cmplwi  r5,0
        mtctr   r5
        subi    r11,r3,2
        subi    r4,r4,2
        li      r3,0
        ble     .LCompWordDone
.LCompWordLoop:
        { load next chars }
        lhzu    r9,2(r11)
        lhzu    r10,2(r4)
        { calculate difference }
        sub.    r3,r9,r10
        { if chars not equal or at the end, we're ready }
        bdnzt   cr0*4+eq, .LCompWordLoop
.LCompWordDone:
end;
{$endif FPC_SYSTEM_HAS_COMPAREWORD}
*)
(*
{$ifndef FPC_SYSTEM_HAS_COMPAREDWORD}
{$define FPC_SYSTEM_HAS_COMPAREDWORD}
function CompareDWord(const buf1,buf2;len:SizeInt):int64; assembler; nostackframe;
{ input: r3 = buf1, r4 = buf2, r5 = len                           }
{ output: r3 = 0 if equal, < 0 if buf1 < str2, > 0 if buf1 > str2 }
{ note: almost direct copy of strlcomp() from strings.inc         }
asm
        {  load the begin of the first buffer in the data cache }
        dcbt    0,r3
        { use r0 instead of r3 for buf1 since r3 contains result }
        cmplwi  r5,0
        mtctr   r5
        subi    r11,r3,4
        subi    r4,r4,4
        li      r3,0
        ble     .LCompDWordDone
.LCompDWordLoop:
        { load next chars }
        lwzu    r9,4(r11)
        lwzu    r10,4(r4)
        { calculate difference }
        sub.    r3,r9,r10
        { if chars not equal or at the end, we're ready }
        bdnzt   cr0*4+eq, .LCompDWordLoop
.LCompDWordDone:
end;
{$endif FPC_SYSTEM_HAS_COMPAREDWORD}
*)
(*
{$ifndef FPC_SYSTEM_HAS_INDEXCHAR0}
{$define FPC_SYSTEM_HAS_INDEXCHAR0}
function IndexChar0(const buf;len:SizeInt;b:Char):int64; assembler; nostackframe;
{ input: r3 = buf, r4 = len, r5 = b                         }
{ output: r3 = position of found position (-1 if not found) }
asm
        {  load the begin of the buffer in the data cache }
        dcbt    0,r3
        { length = 0? }
        cmplwi  r4,0
        mtctr   r4
        subi    r9,r3,1
        subi    r0,r3,1
        { assume not found }
        li      r3,-1
        { if yes, do nothing }
        ble     .LIndexChar0Done
.LIndexChar0Loop:
        lbzu    r10,1(r9)
        cmplwi  cr1,r10,0
        cmplw   r10,r5
        beq     cr1,.LIndexChar0Done
        bdnzf   cr0*4+eq, .LIndexChar0Loop
        bne     .LIndexChar0Done
        sub     r3,r9,r0
.LIndexChar0Done:
end;
{$endif FPC_SYSTEM_HAS_INDEXCHAR0}
*)

{****************************************************************************
                                 String
****************************************************************************}
(*
{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_ASSIGN}
{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_ASSIGN}
function fpc_shortstr_to_shortstr(len:longint; const sstr: shortstring): shortstring; [public,alias: 'FPC_SHORTSTR_TO_SHORTSTR']; compilerproc;
assembler; nostackframe;
{ input: r3: pointer to result, r4: len, r5: sstr }
asm
        { load length source }
        lbz     r10,0(r5)
        {  load the begin of the dest buffer in the data cache }
        dcbtst  0,r3

        { put min(length(sstr),len) in r4 }
        subfc   r7,r10,r4     { r0 := r4 - r10                               }
        subfe   r4,r4,r4      { if r3 >= r4 then r3' := 0 else r3' := -1     }
        and     r7,r7,r4      { if r3 >= r4 then r3' := 0 else r3' := r3-r10 }
        add     r4,r10,r7     { if r3 >= r4 then r3' := r10 else r3' := r3   }

        cmplwi  r4,0
        { put length in ctr }
        mtctr   r4
        stb     r4,0(r3)
        beq     .LShortStrCopyDone
.LShortStrCopyLoop:
        lbzu    r0,1(r5)
        stbu    r0,1(r3)
        bdnz    .LShortStrCopyLoop
.LShortStrCopyDone:
end;


{$ifdef interncopy}
procedure fpc_shortstr_assign(len:longint;sstr,dstr:pointer);[public,alias:'FPC_SHORTSTR_ASSIGN'];
{$else}
procedure fpc_shortstr_copy(len:longint;sstr,dstr:pointer);[public,alias:'FPC_SHORTSTR_COPY'];
{$endif}
assembler; nostackframe;
{ input: r3: len, r4: sstr, r5: dstr }
asm
        { load length source }
        lbz     r10,0(r4)
        {  load the begin of the dest buffer in the data cache }
        dcbtst  0,r5

        { put min(length(sstr),len) in r3 }
        subc    r0,r3,r10    { r0 := r3 - r10                               }
        subfe   r3,r3,r3     { if r3 >= r4 then r3' := 0 else r3' := -1     }
        and     r3,r0,r3     { if r3 >= r4 then r3' := 0 else r3' := r3-r10 }
        add     r3,r3,r10    { if r3 >= r4 then r3' := r10 else r3' := r3   }

        cmplwi  r3,0
        { put length in ctr }
        mtctr   r3
        stb     r3,0(r5)
        beq     .LShortStrCopyDone2
.LShortStrCopyLoop2:
        lbzu    r0,1(r4)
        stbu    r0,1(r5)
        bdnz    .LShortStrCopyLoop2
.LShortStrCopyDone2:
end;
{$endif FPC_SYSTEM_HAS_FPC_SHORTSTR_ASSIGN}
*)
(*
{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}
{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}

function fpc_shortstr_concat(const s1, s2: shortstring): shortstring; compilerproc; [public, alias: 'FPC_SHORTSTR_CONCAT'];
{ expects that (r3) contains a pointer to the result r4 to s1, r5 to s2 }
assembler;
asm
      { load length s1 }
      lbz     r6, 0(r4)
      { load length s2 }
      lbz     r10, 0(r5)
      { length 0 for s1? }
      cmplwi  cr7,r6,0
      { length 255 for s1? }
      subfic. r7,r6,255
      { length 0 for s2? }
      cmplwi  cr1,r10,0
      { calculate min(length(s2),255-length(s1)) }
      subc    r8,r7,r10    { r8 := r7 - r10                                }
      cror    4*6+2,4*1+2,4*7+2
      subfe   r7,r7,r7     { if r7 >= r10 then r7' := 0 else r7' := -1     }
      mtctr   r6
      and     r7,r8,r7     { if r7 >= r10 then r7' := 0 else r7' := r7-r10 }
      add     r7,r7,r10    { if r7 >= r10 then r7' := r10 else r7' := r7   }

      mr      r9,r3

      { calculate length of final string }
      add     r8,r7,r6
      stb     r8,0(r3)
      beq     cr7, .Lcopys1loopDone
    .Lcopys1loop:
      lbzu    r0,1(r4)
      stbu    r0,1(r9)
      bdnz    .Lcopys1loop
    .Lcopys1loopDone:
      mtctr   r7
      beq     cr6, .LconcatDone
    .Lcopys2loop:
      lbzu    r0,1(r5)
      stbu    r0,1(r9)
      bdnz    .Lcopys2loop
end;
{$endif FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}
*)
(*
{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_APPEND_SHORTSTR}
{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_APPEND_SHORTSTR}

procedure fpc_shortstr_append_shortstr(var s1: shortstring; const s2: shortstring); compilerproc;
{ expects that results (r3) contains a pointer to the current string s1, r4 }
{ high(s1) and (r5) a pointer to the one that has to be concatenated        }
assembler; nostackframe;
asm
      { load length s1 }
      lbz     r6, 0(r3)
      { load length s2 }
      lbz     r10, 0(r5)
      { length 0? }
      cmplw   cr1,r6,r4
      cmplwi  r10,0

      { calculate min(length(s2),high(result)-length(result)) }
      sub     r9,r4,r6
      subc    r8,r9,r10    { r8 := r9 - r10                                }
      cror    4*7+2,4*0+2,4*1+2
      subfe   r9,r9,r9     { if r9 >= r10 then r9' := 0 else r9' := -1     }
      and     r9,r8,r9     { if r9 >= r10 then r9' := 0 else r9' := r9-r10 }
      add     r9,r9,r10    { if r9 >= r10 then r9' := r10 else r9' := r9   }

      { calculate new length }
      add     r10,r6,r9
      { load value to copy in ctr }
      mtctr   r9
      { store new length }
      stb     r10,0(r3)
      { go to last current character of result }
      add     r3,r6,r3

      { if nothing to do, exit }
      beq    cr7, .LShortStrAppendDone
      { and concatenate }
.LShortStrAppendLoop:
      lbzu    r10,1(r5)
      stbu    r10,1(r3)
      bdnz    .LShortStrAppendLoop
.LShortStrAppendDone:
end;
{$endif FPC_SYSTEM_HAS_FPC_SHORTSTR_APPEND_SHORTSTR}
*)
(*
{$define FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}
function fpc_shortstr_compare(const dstr,sstr:shortstring): longint; [public,alias:'FPC_SHORTSTR_COMPARE']; compilerproc;
assembler;
asm
      { load length sstr }
      lbz     r9,0(r4)
      { load length dstr }
      lbz     r10,0(r3)
      { save their difference for later and      }
      { calculate min(length(sstr),length(dstr)) }
      subfc    r7,r10,r9    { r0 := r9 - r10                               }
      subfe    r9,r9,r9     { if r9 >= r10 then r9' := 0 else r9' := -1    }
      and      r7,r7,r9     { if r9 >= r10 then r9' := 0 else r9' := r9-r8 }
      add      r9,r10,r7    { if r9 >= r10 then r9' := r10 else r9' := r9  }

      { first compare dwords (length/4) }
      srwi.   r5,r9,2
      { keep length mod 4 for the ends }
      rlwinm  r9,r9,0,30,31
      { already check whether length mod 4 = 0 }
      cmplwi  cr1,r9,0
      { so we can load r3 with 0, in case the strings both have length 0 }
      mr      r8,r3
      li      r3, 0
      { length div 4 in ctr for loop }
      mtctr   r5
      { if length < 3, goto byte comparing }
      beq     LShortStrCompare1
      { setup for use of update forms of load/store with dwords }
      subi    r4,r4,3
      subi    r8,r8,3
LShortStrCompare4Loop:
      lwzu    r3,4(r4)
      lwzu    r10,4(r8)
      sub.    r3,r3,r10
      bdnzt   cr0+eq,LShortStrCompare4Loop
      { r3 contains result if we stopped because of "ne" flag }
      bne     LShortStrCompareDone
      { setup for use of update forms of load/store with bytes }
      addi    r4,r4,3
      addi    r8,r8,3
LShortStrCompare1:
      { if comparelen mod 4 = 0, skip this and return the difference in }
      { lengths                                                         }
      beq     cr1,LShortStrCompareLen
      mtctr   r9
LShortStrCompare1Loop:
      lbzu    r3,1(r4)
      lbzu    r10,1(r8)
      sub.    r3,r3,r10
      bdnzt   cr0+eq,LShortStrCompare1Loop
      bne     LShortStrCompareDone
LShortStrCompareLen:
      { also return result in flags, maybe we can use this in the CG }
      mr.     r3,r3
LShortStrCompareDone:
end;
*)


{$ifndef FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}
{$define FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}
function fpc_pchar_to_shortstr(p:pchar):shortstring;[public,alias:'FPC_PCHAR_TO_SHORTSTR']; compilerproc;
assembler; nostackframe;
{$include strpas.inc}
{$endif FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}

(*
{$ifndef FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}
{$define FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}
function fpc_pchar_length(p:pchar):longint;assembler;[public,alias:'FPC_PCHAR_LENGTH']; {$ifdef hascompilerproc} compilerproc; {$endif} nostackframe;
{$include strlen.inc}
{$endif FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}
*)

{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;{$ifdef SYSTEMINLINE}inline;{$endif} nostackframe;
asm
  { all abi's I know use r1 as stack pointer }
  mr r3, r1
end;

{NOTE: On MACOS, 68000 code might call powerpc code, through the MixedMode manager,
(even in the OS in system 9). The pointer to the switching stack frame is
indicated by the first bit set to 1. This is checked below.}

{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer):pointer;assembler;{$ifdef SYSTEMINLINE}inline;{$endif} nostackframe;
asm
   cmpldi  r3,0
   beq     .Lcaller_addr_frame_null
   ld  r3,0(r3)

   cmpldi  r3,0
   beq     .Lcaller_addr_frame_null
   ld r3,16(r3)
.Lcaller_addr_frame_null:
end;


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer):pointer;assembler;{$ifdef SYSTEMINLINE}inline;{$endif} nostackframe;
asm
    cmpldi  r3,0
    beq     .Lcaller_frame_null
    ld  r3,0(r3)
.Lcaller_frame_null:
end;

{$define FPC_SYSTEM_HAS_ABS_LONGINT}
function abs(l:longint):longint; assembler;{$ifdef SYSTEMINLINE}inline;{$endif} nostackframe;
asm
        srawi   r0,r3,31
        add     r3,r0,r3
        xor     r3,r3,r0
end;


{****************************************************************************
                                 Math
****************************************************************************}

{$define FPC_SYSTEM_HAS_ODD_LONGINT}
function odd(l:longint):boolean;assembler;{$ifdef SYSTEMINLINE}inline;{$endif} nostackframe;
asm
        rldicl r3, r3, 0, 63
end;


{$define FPC_SYSTEM_HAS_SQR_LONGINT}
function sqr(l:longint):longint;assembler;{$ifdef SYSTEMINLINE}inline;{$endif} nostackframe;
asm
        mullw   r3,r3,r3
end;

{$define FPC_SYSTEM_HAS_ODD_INT64}
function odd(l:int64):boolean;assembler;{$ifdef SYSTEMINLINE}inline;{$endif} nostackframe;
asm
        rldicl r3, r3, 0, 63
end;


{$define FPC_SYSTEM_HAS_SQR_INT64}
function sqr(l:int64):int64;assembler;{$ifdef SYSTEMINLINE}inline;{$endif} nostackframe;
asm
        mulld   r3,r3,r3
end;


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : Pointer;assembler;{$ifdef SYSTEMINLINE}inline;{$endif} nostackframe;
asm
        mr    r3,r1
end;


{****************************************************************************
                                 Str()
****************************************************************************}

{ int_str: generic implementation is used for now }


{****************************************************************************
                             Multithreading
****************************************************************************}

{ do a thread save inc/dec }


{$define FPC_SYSTEM_HAS_DECLOCKED_LONGINT}
function declocked(var l : longint) : boolean;assembler;nostackframe;
{ input:  address of l in r3                                      }
{ output: boolean indicating whether l is zero after decrementing }
asm
.LDecLockedLoop:
    lwarx    r10,0,r3
    subi    r10,r10,1
    stwcx.  r10,0,r3
    bne-    .LDecLockedLoop
    cntlzd  r3,r10
    srdi    r3,r3,6
end;

{$define FPC_SYSTEM_HAS_INCLOCKED_LONGINT}
procedure inclocked(var l : longint);assembler;nostackframe;
asm
.LIncLockedLoop:

    lwarx   r10,0,r3
    addi    r10,r10,1
    stwcx.  r10,0,r3
    bne-    .LIncLockedLoop
end;


{$define FPC_SYSTEM_HAS_DECLOCKED_INT64}
function declocked(var l : int64) : boolean;assembler;nostackframe;
{ input:  address of l in r3                                      }
{ output: boolean indicating whether l is zero after decrementing }
asm
.LDecLockedLoop:
    ldarx    r10,0,r3
    subi    r10,r10,1
    stdcx.  r10,0,r3
    bne-    .LDecLockedLoop
    cntlzd  r3,r10
    srdi    r3,r3,6
end;

{$define FPC_SYSTEM_HAS_INCLOCKED_INT64}
procedure inclocked(var l : int64);assembler;nostackframe;
asm
.LIncLockedLoop:

    ldarx   r10,0,r3
    addi    r10,r10,1
    stdcx.  r10,0,r3
    bne-    .LIncLockedLoop
end;

