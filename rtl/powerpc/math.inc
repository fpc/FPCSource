{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000 by Jonas Maebe and other members of the
    Free Pascal development team

    Implementation of mathamatical Routines (only for real)

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

    {$define FPC_SYSTEM_HAS_PI}
    function pi : double;[internproc:in_pi];

    {$define FPC_SYSTEM_HAS_ABS}
    function abs(d : extended) : extended;[internproc:in_abs_extended];

    {$define FPC_SYSTEM_HAS_SQR}
    function sqr(d : extended) : extended;[internproc:in_sqr_extended];

    {
    function arctan(d : extended) : extended;[internconst:in_arctan_extended];
      begin
        runerror(207);
      end;

    function ln(d : extended) : extended;[internconst:in_ln_extended];
      begin
        runerror(207);
      end;

    function sin(d : extended) : extended;[internconst: in_sin_extended];
      begin
        runerror(207);
      end;

    function cos(d : extended) : extended;[internconst:in_cos_extended];
      begin
        runerror(207);
      end;

    function exp(d : extended) : extended;[internconst:in_const_exp];
      begin
        runerror(207);
      end;

    }

      const
        factor: double = double(int64(1) shl 32);
        factor2: double = double(int64(1) shl 31);

    {$define FPC_SYSTEM_HAS_TRUNC}
    function trunc(d : extended) : int64;assembler;[internconst:in_const_trunc];
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
        lwz     r3,temp
        rlwinm. r3,r3,1,31,31
        // make d positive
        fabs    f1,f1
        // load 2^32 in f2
        {$ifndef macos}
        lis    r4,factor@ha
        lfd    f2,factor@l(r4)
        {$else}
        lwz    r4,factor[TC](r2)
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
        lwz      r3,4+temp
        // convert back to float
        lis      r0,0x4330
        stw      r0,temp
        xoris    r0,r3,0x8000
        stw      r0,4+temp
        {$ifndef macos}
        lis    r4,longint_to_real_helper@ha
        lfd    f0,longint_to_real_helper@l(r4)
        {$else}
        lwz    r4,longint_to_real_helper[TC](r2)
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
        lwz    r4,factor2[TC](r2)
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
        lwz    r4,4+temp
        // add 2^31 if value was >=2^31
        blt    cr1, LTruncNoAdd
        xoris  r4,r4,0x8000
LTruncNoAdd:
        // negate value if it was negative to start with
        beq    cr0,LTruncPositive
        subfic r4,r4,0
        subfze r3,r3
LTruncPositive: 
      end ['R3','R4','F1','F2','F3','F4'];


    {$define FPC_SYSTEM_HAS_ROUND}
{$ifdef hascompilerproc}
    function round(d : extended) : int64;[internconst:in_const_round, external name 'FPC_ROUND'];

    function fpc_round(d : extended) : int64;assembler;[public, alias:'FPC_ROUND'];{$ifdef hascompilerproc}compilerproc;{$endif hascompilerproc}
{$else}
    function round(d : extended) : int64;assembler;[internconst:in_const_round];
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
        lwz    r4,factor[TC](r2)
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
        lwz      r3,4+temp
        // convert back to float
        lis      r0,0x4330
        stw      r0,temp
        xoris    r0,r3,0x8000
        stw      r0,4+temp
        {$ifndef macos}
        lis    r4,longint_to_real_helper@ha
        lfd    f0,longint_to_real_helper@l(r4)
        {$else}
        lwz    r3,longint_to_real_helper[TC](r2)
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
        lwz    r4,factor2[TC](r2)
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
        lwz    r4,4+temp
        // add 2^31 if value was >=2^31
        blt    cr1, LRoundNoAdd
        xoris  r4,r4,0x8000
LRoundNoAdd:
        // negate value if it was negative to start with
        beq    cr0,LRoundPositive
        subfic r4,r4,0
        subfze r3,r3
LRoundPositive: 
      end ['R3','R4','F1','F2','F3','F4'];


   {$define FPC_SYSTEM_HAS_POWER}
   function power(bas,expo : extended) : extended;
     begin
        if bas=0 then
          begin
            if expo<>0 then
              power:=0.0
            else
              HandleError(207);
          end
        else if expo=0 then
         power:=1
        else
        { bas < 0 is not allowed }
         if bas<0 then
          handleerror(207)
         else
          power:=exp(ln(bas)*expo);
     end;


{****************************************************************************
                       Longint data type routines
 ****************************************************************************}

   {$define FPC_SYSTEM_HAS_POWER_INT64}
   function power(bas,expo : int64) : int64;
     begin
        if bas=0 then
          begin
            if expo<>0 then
              power:=0
            else
              HandleError(207);
          end
        else if expo=0 then
         power:=1
        else
         begin
           if bas<0 then
            begin
              if odd(expo) then
                power:=-round(exp(ln(-bas)*expo))
              else
                power:=round(exp(ln(-bas)*expo));
            end
           else
            power:=round(exp(ln(bas)*expo));
         end;
     end;

{****************************************************************************
                    Helper routines to support old TP styled reals
 ****************************************************************************}

    { warning: the following converts a little-endian TP-style real }
    { to a big-endian double. So don't byte-swap the TP real!       }
    {$define FPC_SYSTEM_HAS_REAL2DOUBLE}
    function real2double(r : real48) : double;

      var
         res : array[0..7] of byte;
         exponent : word;

      begin
         { copy mantissa }
         res[6]:=0;
         res[5]:=r[1] shl 5;
         res[4]:=(r[1] shr 3) or (r[2] shl 5);
         res[3]:=(r[2] shr 3) or (r[3] shl 5);
         res[2]:=(r[3] shr 3) or (r[4] shl 5);
         res[1]:=(r[4] shr 3) or (r[5] and $7f) shl 5;
         res[0]:=(r[5] and $7f) shr 3;

         { copy exponent }
         { correct exponent: }
         exponent:=(word(r[0])+(1023-129));
         res[1]:=res[1] or ((exponent and $f) shl 4);
         res[0]:=exponent shr 4;

         { set sign }
         res[0]:=res[0] or (r[5] and $80);
         real2double:=double(res);
      end;


{****************************************************************************
                         Int to real helpers
 ****************************************************************************}

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
           stw    r3,4+temp
           {$ifndef macos}
           lis    r3,longint_to_real_helper@ha
           lfd    f1,longint_to_real_helper@l(r3)
           {$else}
           lwz    r3,longint_to_real_helper[TC](r2)
           lfd    f1,0(r3)
           {$endif}
           lfd    f0,temp
           stw    r4,4+temp
           fsub   f0,f0,f1
           {$ifndef macos}
           lis    r4,cardinal_to_real_helper@ha
           lfd    f1,cardinal_to_real_helper@l(r4)
           lis    r4,int_to_real_factor@ha
           lfd    f3,temp
           lfd    f2,int_to_real_factor@l(r4)
           {$else}
           lwz    r4,cardinal_to_real_helper[TC](r2)
           lwz    r3,int_to_real_factor[TC](r2)
           lfd    f3,temp
           lfd    f1,0(r4)
           lfd    f2,0(r3)
           {$endif}
           fsub   f3,f3,f1
           fmadd  f1,f0,f2,f3
end ['R0','R3','R4','F0','F1','F2','F3'];


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
           stw    r3,4+temp
           lfd    f0,temp
           {$ifndef macos}
           lis    r3,cardinal_to_real_helper@ha
           lfd    f1,cardinal_to_real_helper@l(r3)
           {$else}
           lwz    r3,longint_to_real_helper[TC](r2)
           lfd    f1,0(r3)
           {$endif}
           stw    r4,4+temp
           fsub   f0,f0,f1
           lfd    f3,temp
           {$ifndef macos}
           lis    r4,int_to_real_factor@ha
           lfd    f2,int_to_real_factor@l(r4)
           {$else}
           lwz    r4,int_to_real_factor[TC](r2)
           lfd    f2,0(r4)
           {$endif}
           fsub   f3,f3,f1
           fmadd  f1,f0,f2,f3
end ['R0','R3','R4','F0','F1','F2','F3'];


{
  $Log$
  Revision 1.25  2003-05-31 20:22:06  jonas
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
