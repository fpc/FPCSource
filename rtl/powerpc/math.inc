{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000 by Jonas Maebe and other members of the
    Free Pascal development team

    Implementation of mathematical Routines (only for real)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


const
  longint_to_real_helper: int64 = $4330000080000000;
  cardinal_to_real_helper: int64 = $4330000000000000;
  int_to_real_factor: double = double(high(cardinal))+1.0;


{****************************************************************************
                       EXTENDED data type routines
 ****************************************************************************}

{$ifndef INTERNCONSTINTF}
    {$define FPC_SYSTEM_HAS_PI}
    function pi : double;[internproc:fpc_in_pi];

    {$define FPC_SYSTEM_HAS_ABS}
    function abs(d : extended) : extended;[internproc:fpc_in_abs_extended];

    {$define FPC_SYSTEM_HAS_SQR}
    function sqr(d : extended) : extended;[internproc:fpc_in_sqr_extended];
{$endif ndef INTERNCONSTINTF}

      const
        factor: double = double(int64(1) shl 32);
        factor2: double = double(int64(1) shl 31);

{$ifndef FPC_SYSTEM_HAS_TRUNC}
    {$define FPC_SYSTEM_HAS_TRUNC}
    function trunc(d : extended) : int64;assembler;{$ifndef INTERNCONSTINTF}[internconst:fpc_in_const_trunc];{$endif}
      { input: d in fr1      }
      { output: result in r3 }
      assembler;
      var
        temp: packed record
            case byte of
              0: (l1,l2: longint);
              1: (d: double);
          end;
      asm
        // store d in temp
        stfd    f1,temp
        // extract sign bit (record in cr0)
        lwz     r3,temp
        rlwinm. r3,r3,1,31,31
        // make d positive
        fabs    f1,f1
        // load 2^32 in f2
        {$ifndef macos}
        lis    r4,factor@ha
        lfd    f2,factor@l(r4)
        {$else}
        lwz    r4,factor(r2)
        lfd    f2,0(r4)
        {$endif}
        // check if value is < 0
        // f3 := d / 2^32;
        fdiv     f3,f1,f2
        // round
        fctiwz   f4,f3
        // store
        stfd     f4,temp
        // and load into r4
        lwz      r3,temp+4
        // convert back to float
        lis      r0,0x4330
        stw      r0,temp
        xoris    r0,r3,0x8000
        stw      r0,temp+4
        {$ifndef macos}
        lis    r4,longint_to_real_helper@ha
        lfd    f0,longint_to_real_helper@l(r4)
        {$else}
        lwz    r4,longint_to_real_helper(r2)
        lfd    f0,0(r4)
        {$endif}
        lfd    f3,temp
        fsub   f3,f3,f0


        // f4 := d "mod" 2^32 ( = d - ((d / 2^32) * 2^32))
        fnmsub   f4,f3,f2,f1

        // now, convert to unsigned 32 bit

        // load 2^31 in f2
        {$ifndef macos}
        lis    r4,factor2@ha
        lfd    f2,factor2@l(r4)
        {$else}
        lwz    r4,factor2(r2)
        lfd    f2,0(r4)
        {$endif}

        // subtract 2^31
        fsub   f3,f4,f2
        // was the value > 2^31?
        fcmpu  cr1,f4,f2
        // use diff if >= 2^31
        fsel   f4,f3,f3,f4

        // next part same as conversion to signed integer word
        fctiwz f4,f4
        stfd   f4,temp
        lwz    r4,temp+4
        // add 2^31 if value was >=2^31
        blt    cr1, .LTruncNoAdd
        xoris  r4,r4,0x8000
.LTruncNoAdd:
        // negate value if it was negative to start with
        beq    cr0,.LTruncPositive
        subfic r4,r4,0
        subfze r3,r3
.LTruncPositive:
      end;
{$endif not FPC_SYSTEM_HAS_TRUNC}


(*
{$ifndef FPC_SYSTEM_HAS_ROUND}
    {$define FPC_SYSTEM_HAS_ROUND}
{$ifdef hascompilerproc}
    function round(d : extended) : int64;{$ifndef INTERNCONSTINTF}[internconst:fpc_in_const_round, external name 'FPC_ROUND'];{$endif}

    function fpc_round(d : extended) : int64;assembler;[public, alias:'FPC_ROUND'];{$ifdef hascompilerproc}compilerproc;{$endif hascompilerproc}
{$else}
    function round(d : extended) : int64;assembler;{$ifndef INTERNCONSTINTF}[internconst:fpc_in_const_round];{$endif}
{$endif hascompilerproc}
      { exactly the same as trunc, except that one fctiwz has become fctiw }
      { input: d in fr1      }
      { output: result in r3 }
      assembler;
      var
        temp: packed record
            case byte of
              0: (l1,l2: longint);
              1: (d: double);
          end;
      asm
        // store d in temp
        stfd    f1, temp
        // extract sign bit (record in cr0)
        lwz     r4,temp
        rlwinm. r4,r4,1,31,31
        // make d positive
        fabs    f1,f1
        // load 2^32 in f2
        {$ifndef macos}
        lis    r4,factor@ha
        lfd    f2,factor@l(r4)
        {$else}
        lwz    r4,factor(r2)
        lfd    f2,0(r4)
        {$endif}
        // check if value is < 0
        // f3 := d / 2^32;
        fdiv     f3,f1,f2
        // round
        fctiwz   f4,f3
        // store
        stfd     f4,temp
        // and load into r4
        lwz      r3,temp+4
        // convert back to float
        lis      r0,0x4330
        stw      r0,temp
        xoris    r0,r3,0x8000
        stw      r0,temp+4
        {$ifndef macos}
        lis    r4,longint_to_real_helper@ha
        lfd    f0,longint_to_real_helper@l(r4)
        {$else}
        lwz    r4,longint_to_real_helper(r2)
        lfd    f0,0(r4)
        {$endif}
        lfd    f3,temp
        fsub   f3,f3,f0


        // f4 := d "mod" 2^32 ( = d - ((d / 2^32) * 2^32))
        fnmsub   f4,f3,f2,f1

        // now, convert to unsigned 32 bit

        // load 2^31 in f2
        {$ifndef macos}
        lis    r4,factor2@ha
        lfd    f2,factor2@l(r4)
        {$else}
        lwz    r4,factor2(r2)
        lfd    f2,0(r4)
        {$endif}

        // subtract 2^31
        fsub   f3,f4,f2
        // was the value > 2^31?
        fcmpu  cr1,f4,f2
        // use diff if >= 2^31
        fsel   f4,f3,f3,f4

        // next part same as conversion to signed integer word
        fctiw  f4,f4
        stfd   f4,temp
        lwz    r4,temp+4
        // add 2^31 if value was >=2^31
        blt    cr1, .LRoundNoAdd
        xoris  r4,r4,0x8000
.LRoundNoAdd:
        // negate value if it was negative to start with
        beq    cr0,.LRoundPositive
        subfic r4,r4,0
        subfze r3,r3
.LRoundPositive:
      end;
{$endif not FPC_SYSTEM_HAS_ROUND}
*)


{****************************************************************************
                         Int to real helpers
 ****************************************************************************}

{$define FPC_SYSTEM_HAS_INT64_TO_DOUBLE}
function fpc_int64_to_double(i: int64): double; compilerproc;
assembler;
{ input: high(i) in r4, low(i) in r3 }
{ output: double(i) in f0            }
var
  temp: packed record
      case byte of
        0: (l1,l2: cardinal);
        1: (d: double);
    end;
asm
           lis    r0,0x4330
           stw    r0,temp
           xoris  r3,r3,0x8000
           stw    r3,temp+4
           {$ifndef macos}
           lis    r3,longint_to_real_helper@ha
           lfd    f1,longint_to_real_helper@l(r3)
           {$else}
           lwz    r3,longint_to_real_helper(r2)
           lfd    f1,0(r3)
           {$endif}
           lfd    f0,temp
           stw    r4,temp+4
           fsub   f0,f0,f1
           {$ifndef macos}
           lis    r4,cardinal_to_real_helper@ha
           lfd    f1,cardinal_to_real_helper@l(r4)
           lis    r4,int_to_real_factor@ha
           lfd    f3,temp
           lfd    f2,int_to_real_factor@l(r4)
           {$else}
           lwz    r4,cardinal_to_real_helper(r2)
           lwz    r3,int_to_real_factor(r2)
           lfd    f3,temp
           lfd    f1,0(r4)
           lfd    f2,0(r3)
           {$endif}
           fsub   f3,f3,f1
           fmadd  f1,f0,f2,f3
end;


{$define FPC_SYSTEM_HAS_QWORD_TO_DOUBLE}
function fpc_qword_to_double(q: qword): double; compilerproc;
assembler;
{ input: high(q) in r4, low(q) in r3 }
{ output: double(q) in f0            }
var
  temp: packed record
      case byte of
        0: (l1,l2: cardinal);
        1: (d: double);
    end;
asm
           lis    r0,0x4330
           stw    r0,temp
           stw    r3,temp+4
           lfd    f0,temp
           {$ifndef macos}
           lis    r3,cardinal_to_real_helper@ha
           lfd    f1,cardinal_to_real_helper@l(r3)
           {$else}
           lwz    r3,longint_to_real_helper(r2)
           lfd    f1,0(r3)
           {$endif}
           stw    r4,temp+4
           fsub   f0,f0,f1
           lfd    f3,temp
           {$ifndef macos}
           lis    r4,int_to_real_factor@ha
           lfd    f2,int_to_real_factor@l(r4)
           {$else}
           lwz    r4,int_to_real_factor(r2)
           lfd    f2,0(r4)
           {$endif}
           fsub   f3,f3,f1
           fmadd  f1,f0,f2,f3
end;


{
  $Log$
  Revision 1.36  2004-11-20 15:49:21  jonas
    * some compilation fixes for powerpc after all the internconst and
      internproc changes, still crashes with internalerror(88) for ppc1
      on real2str.inc(193,39)

  Revision 1.35  2004/11/07 19:09:02  jonas
    * use generic round, the ppc-specific one is buggy

  Revision 1.34  2004/10/09 21:00:46  jonas
    + cgenmath with libc math functions. Faster than the routines in genmath
      and also have full double support (exp() only has support for values in
      the single range in genmath, for example). Used in FPC_USE_LIBC is
      defined
    * several fixes to allow compilation with -dHASINLINE, but internalerrors
      because of missing support for inlining assembler code

  Revision 1.33  2004/02/09 20:21:06  olle
    * fixed global variable access in asm

  Revision 1.32  2003/12/07 19:55:37  jonas
    - reverted previous patch, solved with the new assembler reader
      (which didn't understand the new syntax)

  Revision 1.30  2003/11/15 19:01:27  florian
    * fixed rtl to work with the integrated fpc ppc assembler reader

  Revision 1.29  2003/09/04 16:07:31  florian
    * fixed qword_to_double conversion on powerpc

  Revision 1.28  2003/09/03 14:09:37  florian
    * arm fixes to the common rtl code
    * some generic math code fixed
    * ...

  Revision 1.27  2003/08/08 22:02:05  olle
    * small bugfix macos

  Revision 1.26  2003/06/14 12:41:08  jonas
    * fixed compilation problems (removed unnecessary modified registers
      lists from procedures)

  Revision 1.25  2003/05/31 20:22:06  jonas
    * fixed 64 bit results of trunc and round

  Revision 1.24  2003/05/30 23:56:41  florian
    * fixed parameter passing for int64

  Revision 1.23  2003/05/24 13:39:32  jonas
    * fsqrt is an optional instruction in the ppc architecture and isn't
      implemented by any current ppc afaik, so use the generic sqrt routine
      instead (adapted so it works with compilerproc)

  Revision 1.22  2003/05/16 16:04:33  jonas
    * fixed round() (almost the same as trunc)

  Revision 1.21  2003/05/11 18:09:45  jonas
    * fixed qword and int64 to double conversion

  Revision 1.20  2003/05/02 15:12:19  jonas
    - removed empty ppc-specific frac()
    + added correct generic frac() implementation for doubles (translated
      from glibc code)

  Revision 1.19  2003/04/26 20:36:24  jonas
    * trunc now also supports int64 (no NaN's etc though)

  Revision 1.18  2003/04/26 17:20:16  florian
    * fixed trunc, now it's working at least for longint range

  Revision 1.17  2003/04/23 21:28:21  peter
    * fpc_round added, needed for int64 currency

  Revision 1.16  2003/01/16 11:29:11  olle
    * changed access of globals to be indirect via TOC

  Revision 1.15  2003/01/15 01:09:04  florian
    * changed power(...) prototype to int64

  Revision 1.14  2002/11/28 11:04:16  olle
    * macos: refs to globals in asm adapted to macos

  Revision 1.13  2002/10/21 18:08:28  jonas
    * round has int64 instead of longint result

  Revision 1.12  2002/09/08 13:00:21  jonas
    * made pi an internproc instead of internconst

  Revision 1.11  2002/09/07 16:01:26  peter
    * old logs removed and tabs fixed

  Revision 1.10  2002/08/18 22:11:10  florian
    * fixed remaining assembler errors

  Revision 1.9  2002/08/18 21:37:48  florian
    * several errors in inline assembler fixed

  Revision 1.8  2002/08/10 17:14:36  jonas
    * various fixes, mostly changing the names of the modifies registers to
      upper case since that seems to be required by the compiler

  Revision 1.7  2002/07/31 16:58:12  jonas
    * fixed conversion from int64/qword to double errors

  Revision 1.6  2002/07/29 21:28:17  florian
    * several fixes to get further with linux/ppc system unit compilation

  Revision 1.5  2002/07/28 21:39:29  florian
    * made abs a compiler proc if it is generic

  Revision 1.4  2002/07/28 20:43:49  florian
    * several fixes for linux/powerpc
    * several fixes to MT

}
