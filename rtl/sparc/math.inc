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

{$ifdef unused}

{****************************************************************************
                         Int to real helpers
 ****************************************************************************}

const
  longint_to_real_helper: int64 = $4330000080000000;
  cardinal_to_real_helper: int64 = $430000000000000;
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
    { define FPC_SYSTEM_HAS_INT}
    {$warning FIX ME}
    function int(d : extended) : extended;[internconst:in_const_int];
      begin
        runerror(207);
      end;


    { define FPC_SYSTEM_HAS_TRUNC}
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


    { define FPC_SYSTEM_HAS_ROUND}
    function round(d : extended) : int64;[internconst:in_const_round, external name 'FPC_ROUND'];

    function fpc_round(d : extended) : int64;[public, alias:'FPC_ROUND'];compilerproc;
      begin
      end;

{$endif}

{
  $Log$
  Revision 1.8  2004-01-06 21:33:38  peter
    * remove generic functions

  Revision 1.7  2003/09/14 15:02:24  peter
    * remove int64 to double helpers

  Revision 1.6  2003/09/02 17:41:49  peter
    * updated for int64 to double

  Revision 1.5  2003/09/01 20:46:32  peter
    * new dummies

  Revision 1.4  2003/04/23 21:28:21  peter
    * fpc_round added, needed for int64 currency

  Revision 1.3  2003/01/22 20:45:15  mazen
  * making math code in RTL compiling.
  *NB : This does NOT mean necessary that it will generate correct code!

  Revision 1.2  2003/01/20 22:21:36  mazen
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
