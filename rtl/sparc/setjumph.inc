{******************************************************************************
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000-2002 by Jonas Maebe and other members of the
    Free Pascal development team

    SetJmp/Longjmp declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

******************************************************************************}
type
  jmp_buf = packed record
    ProgramCounter,
    StackPointer,
    BasePointer:Pointer;
  end;
  Pjmp_buf = ^jmp_buf;
function setjmp(var S:jmp_buf):longint;
procedure longjmp(var S:jmp_buf;value:longint);
{
  $Log$
  Revision 1.3  2003-01-01 18:24:41  mazen
  * just put register pointers

  Revision 1.2  2002/11/24 18:19:44  mazen
  + setjmp and longjmp

  Revision 1.1  2002/11/16 20:10:31  florian
    + sparc specific rtl skeleton added
}
