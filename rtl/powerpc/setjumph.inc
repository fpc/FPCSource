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
     r1,r2,lr,r14,r15,r16,r17,r18,r19,r20,r21,r22,r23,r24,r25,r26,r27,r28,r29,r30,r31,cr : dword;
     f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24,f25,f26,f27,f28,f29,f30,f31 : double;
   end;
   pjmp_buf = ^jmp_buf;

function setjmp(var S : jmp_buf) : longint;
procedure longjmp(var S : jmp_buf;value : longint);

{
  $Log$
  Revision 1.6  2003-04-26 18:54:30  florian
    + implemented setjmp record and longjmp

  Revision 1.5  2002/09/15 10:03:04  carl
    * make the record packed (just in case)

  Revision 1.4  2002/08/10 17:14:36  jonas
    * various fixes, mostly changing the names of the modifies registers to
      upper case since that seems to be required by the compiler

  Revision 1.3  2002/07/26 22:46:17  florian
    * interface of system unit for Linux/PowerPC compiles

  Revision 1.2  2002/07/26 22:08:38  florian
    * comments fixed

  Revision 1.1  2002/07/26 22:08:13  florian
    + initial dummy implementation
}
