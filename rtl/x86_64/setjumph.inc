{
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

 **********************************************************************}

type
   jmp_buf = packed record
     rbx,rbp,r12,r13,r14,r15,rsp,rip : qword;
   end;
   pjmp_buf = ^jmp_buf;

function setjmp(var S : jmp_buf) : longint;
procedure longjmp(var S : jmp_buf;value : longint);

{
  $Log$
  Revision 1.2  2004-04-24 20:13:34  florian
    * fixed x86-64 exception handling

  Revision 1.1  2003/04/30 22:11:06  florian
    + for a lot of x86-64 dependend files mostly dummies added
}
