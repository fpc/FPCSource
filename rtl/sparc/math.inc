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


{****************************************************************************
                       EXTENDED data type routines
 ****************************************************************************}

    {$define FPC_SYSTEM_HAS_PI}
    function pi : double;[internproc:in_pi];

    {$define FPC_SYSTEM_HAS_ABS}
    function abs(d : extended) : extended;[internproc:in_abs_extended];

    {$define FPC_SYSTEM_HAS_SQR}
    function sqr(d : extended) : extended;[internproc:in_sqr_extended];

    {$define FPC_SYSTEM_HAS_SQRT}
    function sqrt(d : extended) : extended;[internproc:in_sqrt_extended];

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


    function frac(d : extended) : extended;[internconst:in_const_frac];
      begin
        runerror(207);
      end;


    }
    {$define FPC_SYSTEM_HAS_INT}
    {$warning FIX ME}
    function int(d : extended) : extended;[internconst:in_const_int];
      begin
        runerror(207);
      end;


    {$define FPC_SYSTEM_HAS_TRUNC}
    {$warning FIX ME}
    function trunc(d : extended) : int64;{assembler;}[internconst:in_const_trunc];
      { input: d in fr1      }
      { output: result in r3 }
      {assembler;}
      var
        temp: packed record
            case byte of
              0: (l1,l2: longint);
              1: (d: double);
          end;
      begin{asm}
{        fctiwz   f1,f1
        stfd     f1,temp
        lwz      r3,temp
        lwz      r4,4+temp}
      end{ ['R3','F1']};


    {$define FPC_SYSTEM_HAS_ROUND}
    function round(d : extended) : int64;{assembler;}[internconst:in_const_round];
      { input: d in fr1      }
      { output: result in r3 }
      {assembler;}
      var
        temp : packed record
            case byte of
              0: (l1,l2: longint);
              1: (d: double);
          end;
      begin{asm}
  {      fctiw    f1,f1
        stfd     f1,temp
        lwz      r3,temp
        lwz      r4,4+temp}
      end{ ['R3','F1']};


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

   function power(bas,expo : Int64) : Int64;
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

const
  longint_to_real_helper: int64 = $4330000080000000;
  cardinal_to_real_helper: int64 = $430000000000000;
  int_to_real_factor: double = double(high(cardinal))+1.0;

function fpc_int64_to_double(i: int64): double; compilerproc;
{assembler;}
{ input: high(i) in r3, low(i) in r4 }
{ output: double(i) in f0            }
var
  temp: packed record
      case byte of
        0: (l1,l2: cardinal);
        1: (d: double);
    end;
begin{asm}
(*           lis    r0,0x4330
           stw    r0,temp
           xoris  r3,r3,0x8000
           stw    r3,4+temp
           {$ifndef macos}
           lis    r3,longint_to_real_helper@ha
           lfd    f1,longint_to_real_helper@l(r3)
           {$else}
           lfd    f1,longint_to_real_helper(r2)
           {$endif}
           lfd    f0,temp
           stw    r4,4+temp
           fsub   f0,f0,f1
           {$ifndef macos}
           lis    r4,cardinal_to_real_helper@ha
           lfd    f1,cardinal_to_real_helper@l(r4)
           lis    r3,int_to_real_factor@ha
           lfd    f3,temp
           lfd    f2,int_to_real_factor@l(r3)
           {$else}
           lfd    f1,cardinal_to_real_helper(r2)
           lfd    f3,temp
           lfd    f2,int_to_real_factor(r2)
           {$endif}
           fsub   f3,f3,f1
           fmadd  f1,f0,f2,f3*)
end{ ['R0','R3','R4','F0','F1','F2','F3']};


function fpc_qword_to_double(q: qword): double; compilerproc;
{assembler;}
{ input: high(q) in r3, low(q) in r4 }
{ output: double(q) in f0            }
var
  temp: packed record
      case byte of
        0: (l1,l2: cardinal);
        1: (d: double);
    end;
begin{asm}
(*           lis    r0,0x4330
           stw    r0,temp
           stw    r3,4+temp
           lfd    f0,temp
           {$ifndef macos}
           lis    r3,cardinal_to_real_helper@ha
           lfd    f1,cardinal_to_real_helper@l(r3)
           {$else}
           lfd    f1,cardinal_to_real_helper(r2)
           {$endif}
           stw    r4,4+temp
           fsub   f0,f0,f1
           lfd    f3,temp
           {$ifndef macos}
           lis    r3,int_to_real_factor@ha
           lfd    f2,int_to_real_factor@l(r3)
           {$else}
           lfd    f2,int_to_real_factor(r2)
           {$endif}
           fsub   f3,f3,f1
           fmadd  f1,f0,f2,f3*)
end{ ['R0','R3','F0','F1','F2','F3']};


{
  $Log$
  Revision 1.2  2003-01-20 22:21:36  mazen
  * many stuff related to RTL fixed

  Revision 1.1  2002/12/24 21:30:20  mazen
  - some writeln(s) removed in compiler
  + many files added to RTL
  * some errors fixed in RTL

  Revision 1.14  2002/11/28 11:04:16  olle
    * macos: refs to globals in begin{asm} adapted to macos

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
