{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Jonas Maebe and other members of the
    Free Pascal development team

    SetJmp and LongJmp implementation for exception handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ the necessary code can be copied from the linux kernel sources }
function setjmp(var S : jmp_buf) : longint;assembler;[Public, alias : 'FPC_SETJMP'];
  asm
    {$warning FIXME!!!!}
  end;

procedure longjmp(var S : jmp_buf;value : longint);assembler;[Public, alias : 'FPC_LONGJMP'];
  asm
     lwz r1,(r3)
     lwz r2,4(r3)
     lwz r0,8(r3)
     lwz r14,12(r3)
     lfd fp14,88(r3)
     lwz r15,16(r3)
     lfd fp15,96(r3)
     lwz r16,20(r3)
     lfd fp16,104(r3)
     lwz r17,24(r3)
     lfd fp17,112(r3)
     lwz r18,28(r3)
     lfd fp18,120(r3)
     lwz r19,32(r3)
     lfd fp19,128(r3)
     lwz r20,36(r3)
     lfd fp20,136(r3)
     mtlr r0
     lwz r21,40(r3)
     lfd fp21,144(r3)
     lwz r22,44(r3)
     lfd fp22,152(r3)
     lwz r0,84(r3)
     lwz r23,48(r3)
     lfd fp23,160(r3)
     lwz r24,52(r3)
     lfd fp24,168(r3)
     lwz r25,56(r3)
     lfd fp25,176(r3)
     mtcrf 0xFF,r0
     lwz r26,60(r3)
     lfd fp26,184(r3)
     lwz r27,64(r3)
     lfd fp27,192(r3)
     lwz r28,68(r3)
     lfd fp28,200(r3)
     lwz r29,72(r3)
     lfd fp29,208(r3)
     lwz r30,76(r3)
     lfd fp30,216(r3)
     lwz r31,80(r3)
     lfd fp31,224(r3)
     mr r3,r4
     blr
 end;

{
  $Log$
  Revision 1.6  2003-04-26 18:54:30  florian
    + implemented setjmp record and longjmp

  Revision 1.5  2002/09/07 16:01:26  peter
    * old logs removed and tabs fixed

  Revision 1.4  2002/08/31 14:27:40  florian
    + public and alias directive added to *jmp

  Revision 1.3  2002/08/10 17:14:36  jonas
    * various fixes, mostly changing the names of the modifies registers to
      upper case since that seems to be required by the compiler

  Revision 1.2  2002/07/30 17:29:53  florian
    + dummy setjmp and longjmp added
    + dummy implemtation of the destructor helper

  Revision 1.1  2002/07/28 20:43:49  florian
    * several fixes for linux/powerpc
    * several fixes to MT
}
