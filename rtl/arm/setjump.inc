{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team.

    SetJmp and LongJmp implementation for exception handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function setjmp(var S : jmp_buf) : longint;assembler;[Public, alias : 'FPC_SETJMP'];nostackframe;
  label FPC_SETJMP;
  asm
    stmia   r0,{v1-v6, sl, fp, sp, lr}
    mov     r0,#0
    mov     pc,lr
  end;


procedure longjmp(var S : jmp_buf;value : longint);assembler;[Public, alias : 'FPC_LONGJMP'];
  asm
     mov     ip, r0
     movs    r0, r1
     moveq   r0, #1
     ldmia   ip,{v1-v6, sl, fp, sp, pc}
  end;


{
  $Log: setjump.inc,v $
  Revision 1.6  2005/02/14 17:13:21  peter
    * truncate log

  Revision 1.5  2005/01/04 16:22:05  florian
    + added nostackframe directive

}
