{
    $Id$

    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team.

    SetJmp and LongJmp implementation for exception handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function setjmp(var S : jmp_buf) : longint;assembler;[Public, alias : 'FPC_SETJMP'];
  asm
     stmia   r0,{v1-v6, sl, fp, sp, lr}
     (*!!!! fix me ?*)
  end;

procedure longjmp(var S : jmp_buf;value : longint);assembler;[Public, alias : 'FPC_LONGJMP'];
  asm
     mov     ip, r0
     movs    r0, r1
     moveq   r0, #1
     ldmia   ip,{v1-v6, sl, fp, sp, pc}
  end;


{
  $Log$
  Revision 1.2  2003-09-03 14:09:37  florian
    * arm fixes to the common rtl code
    * some generic math code fixed
    * ...

  Revision 1.1  2003/08/21 16:41:54  florian
    * empty dummy files
    + [long|set]jmp implemented
}
