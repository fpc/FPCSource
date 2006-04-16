{
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
{$ifdef win64}
     rsi,rdi : qword;
{$endif win64}
   end;
   pjmp_buf = ^jmp_buf;

function setjmp(var S : jmp_buf) : longint;
procedure longjmp(var S : jmp_buf;value : longint);

