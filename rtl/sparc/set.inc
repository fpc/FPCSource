{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe, member of the
    Free Pascal development team

    Include file with set operations called by the compiler

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  $Log$
  Revision 1.2  2004-05-30 13:06:38  florian
    * removed useless PowerPC and i386 code

  Revision 1.1  2002/12/24 21:30:20  mazen
  - some writeln(s) removed in compiler
  + many files added to RTL
  * some errors fixed in RTL

  Revision 1.16  2002/10/17 10:14:46  jonas
    * fixed srwi's after cntlzw instructions (should be 5 instead of 31)

  Revision 1.15  2002/09/07 16:01:26  peter
    * old logs removed and tabs fixed

  Revision 1.14  2002/08/18 22:11:10  florian
    * fixed remaining assembler errors

  Revision 1.13  2002/08/18 21:37:48  florian
    * several errors in inline assembler fixed

  Revision 1.12  2002/08/10 17:14:36  jonas
    * various fixes, mostly changing the names of the modifies registers to
      upper case since that seems to be required by the compiler

  Revision 1.11  2002/07/28 20:43:49  florian
    * several fixes for linux/powerpc
    * several fixes to MT

}
