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
    ra,
    x8,x9,x18,x19,x20,x21,
    x22,x23,x24,x25,x26,
    x27,x2: qword;
{$if defined(FPUFD) or defined(FPUD)}
    fcsr,
    f8,f9,f18,f19,f20,f21,
    f22,f23,f24,f25,f26,f27: qword;
{$endif FPUFD or FPUD}
{$if defined(FPUF)}
    fcsr: qword;
    f8,f9,f18,f19,f20,f21,
    f22,f23,f24,f25,f26,f27: longword;
{$endif FPUF}
   end;
   pjmp_buf = ^jmp_buf;

function setjmp(var S : jmp_buf) : longint;[external name 'FPC_SETJMP'];
procedure longjmp(var S : jmp_buf;value : longint);[external name 'FPC_LONGJMP'];


