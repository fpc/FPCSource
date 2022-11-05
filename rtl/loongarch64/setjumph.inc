{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team.

    SetJmp/Longjmp declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
   jmp_buf = packed record
    ra,sp,x,fp,s0,s1,s2,s3,s4,s5,s6,s7,s8,
    fs0,fs1,fs2,fs3,fs4,fs5,fs6,fs7,fs8:qword;
   end;
   pjmp_buf = ^jmp_buf;

function setjmp(var S : jmp_buf) : longint;[external name 'FPC_SETJMP'];
procedure longjmp(var S : jmp_buf;value : longint);[external name 'FPC_LONGJMP'];
