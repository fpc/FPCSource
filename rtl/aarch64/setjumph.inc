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
   jmp_buf = record
     magic: qword;
     xsp: qword;
     x19: qword;
     x20: qword;
     x21: qword;
     x22: qword;
     x23: qword;
     x24: qword;
     x25: qword;
     x26: qword;
     x27: qword;
     x28: qword;
     x29: qword;
     x30: qword;
     tpidr: qword;
     d8: qword;
     d9: qword;
     d10: qword;
     d11: qword;
     d12: qword;
     d13: qword;
     d14: qword;
     d15: qword;
   end;
   pjmp_buf = ^jmp_buf;

function setjmp(var S : jmp_buf) : longint;[external name 'FPC_SETJMP'];
procedure longjmp(var S : jmp_buf;value : longint);[external name 'FPC_LONGJMP'];

