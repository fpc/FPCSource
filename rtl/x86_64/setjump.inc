{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by Florian Klaempfl and other members of the
    Free Pascal development team

    SetJmp and LongJmp implementation for exception handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function setjmp(var S : jmp_buf) : longint;assembler;[Public, alias : 'FPC_SETJMP'];
{$warning FIX ME!!}
  asm
  end;

procedure longjmp(var S : jmp_buf;value : longint);assembler;[Public, alias : 'FPC_LONGJMP'];
{$warning FIX ME!!}
  asm
  end;

{
  $Log$
  Revision 1.1  2003-04-30 22:11:06  florian
    + for a lot of x86-64 dependend files mostly dummies added
}
