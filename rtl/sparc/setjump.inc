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
  Revision 1.2  2002-11-24 18:19:44  mazen
  + setjmp and longjmp

  Revision 1.1  2002/11/16 20:10:31  florian
    + sparc specific rtl skeleton added
}
