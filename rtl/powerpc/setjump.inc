{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Jonas Maebe and other members of the
    Free Pascal development team

    SetJmp and LongJmp implementation for exception handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ the necessary code can be copied from the linux kernel sources }
function setjmp(var S : jmp_buf) : longint;assembler;[Public, alias : 'FPC_SETJMP'];
  asm
    {$warning FIXME!!!!}
  end;

procedure longjmp(var S : jmp_buf;value : longint);assembler;[Public, alias : 'FPC_LONGJMP'];
  asm
    {$warning FIXME!!!!}
  end;

{
  $Log$
  Revision 1.4  2002-08-31 14:27:40  florian
    + public and alias directive added to *jmp

  Revision 1.3  2002/08/10 17:14:36  jonas
    * various fixes, mostly changing the names of the modifies registers to
      upper case since that seems to be required by the compiler

  Revision 1.2  2002/07/30 17:29:53  florian
    + dummy setjmp and longjmp added
    + dummy implemtation of the destructor helper

  Revision 1.1  2002/07/28 20:43:49  florian
    * several fixes for linux/powerpc
    * several fixes to MT
}