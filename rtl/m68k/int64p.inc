{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by the Free Pascal development team

    This file contains some helper routines for int64 and qword

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifndef FPC_SYSTEM_HAS_SAR_QWORD}
{$define FPC_SYSTEM_HAS_SAR_QWORD}
function fpc_SarInt64(Const AValue : Int64;const Shift : Byte): Int64; [Public,Alias:'FPC_SARINT64']; compilerproc; assembler; nostackframe;
asm
  // d0 = shift
  lea.l   4(sp),a0
  move.l  d2,-(sp)
  move.l  d0,d2
{$ifndef CPUCOLDFIRE}
  and.w   #63,d2
  cmp.w   #32,d2
{$else}
  and.l   #63,d2
  cmp.l   #32,d2
{$endif}
  bge.s   @longshift

  move.l  (a0)+,d0
  move.l  (a0),d1

{$ifdef CPUM68K_HAS_ROLROR}
  cmp.w   #1,d2
  beq.s   @oneshift
{$endif}
{$ifdef CPU68000}
  cmp.w   #16,d2
  beq.s   @sixteenshift
{$endif}

  move.l  d3,a0
  move.l  d4,a1

  move.l  d0,d3
  moveq.l #32,d4
{$ifndef CPUCOLDFIRE}
  sub.w   d2,d4
{$else}
  sub.l   d2,d4
{$endif}
  asr.l   d2,d0
  lsl.l   d4,d3
  lsr.l   d2,d1
  or.l    d3,d1

  move.l  a0,d3
  move.l  a1,d4

  bra.s   @quit

{$ifdef CPU68000}
@sixteenshift:
  move.w  d0,d1
  swap    d1
  swap    d0
  ext.l   d0

  bra.s   @quit
{$endif}

{$ifdef CPUM68K_HAS_ROLROR}
@oneshift:
  asr.l   #1,d0
  roxr.l  #1,d1

  bra.s   @quit
{$endif}

@longshift:
  move.l  (a0),d0
  move.l  d0,d1
  smi     d0
{$if defined(CPU68020) or defined(CPUCOLDFIRE)}
  extb.l  d0
{$else}
  ext.w   d0
  ext.l   d0
{$endif}
  sub.w   #32,d2
  asr.l   d2,d1

@quit:
  move.l (sp)+,d2
end;
{$endif}
