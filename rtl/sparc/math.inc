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

{$ifdef INTERNCONSTINTF}

    {$define FPC_SYSTEM_HAS_ABS}
    function fpc_abs_real(d : valreal) : valreal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;

    {$define FPC_SYSTEM_HAS_SQR}
    function fpc_sqr_real(d : valreal) : valreal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;

    {$define FPC_SYSTEM_HAS_SQRT}
    function fpc_sqrt_real(d : valreal) : valreal;compilerproc;
    begin
      { Function is handled internal in the compiler }
      runerror(207);
      result:=0;
    end;

{$else}

    {$define FPC_SYSTEM_HAS_ABS}
    function abs(d : extended) : extended;[internproc:fpc_in_abs_real];

    {$define FPC_SYSTEM_HAS_SQR}
    function sqr(d : extended) : extended;[internproc:fpc_in_sqr_real];

    {$define FPC_SYSTEM_HAS_SQRT}
    function sqrt(d : extended) : extended;[internproc:fpc_in_sqrt_real];

{$endif}

{
  $Log$
  Revision 1.12  2004-11-21 19:11:33  peter
    * fix bootstrapping

  Revision 1.11  2004/11/21 15:35:23  peter
    * float routines all use internproc and compilerproc helpers

  Revision 1.10  2004/10/03 12:41:30  florian
    * made sqrt, sqr and abs internal for the sparc

  Revision 1.9  2004/05/31 14:31:57  peter
    * remove comment warnings

  Revision 1.8  2004/01/06 21:33:38  peter
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
    * macos: refs to globals in beginasm adapted to macos

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
