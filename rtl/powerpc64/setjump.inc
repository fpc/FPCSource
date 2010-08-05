{
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
{
   jmp_buf = packed record
      r1, r2, lr,r14,r15,
     r16,r17,r18,r19,r20,
     r21,r22,r23,r24,r25,
     r26,r27,r28,r29,r30,
     r31,cr : int64;
     // 176
     f14,f15,f16,
     // 200
     f17,f18,f19,f20,f21,
     f22,f23,f24,f25,f26,
     f27,f28,f29,f30,f31 : double;
   end;
   pjmp_buf = ^jmp_buf;}

function fpc_setjmp(var S : jmp_buf) : longint;assembler;[Public, alias : 'FPC_SETJMP']; nostackframe; compilerproc;
  asm
     std     r1,0(r3)    // store r1
     mflr    r0
     std     r2,8(r3)    // store r2
     std     r14,24(r3)  // store r14
     stfd    f14,176(r3) // store f14
     std     r0,16(r3)   // store lr
     std     r15,32(r3)  // store r15
     stfd    f15,184(r3) // store f15
     mfcr    r0
     std     r16,40(r3)  // store r16
     stfd    f16,192(r3) // store f16
     stw     r0,168(r3)  // store cr
     std     r17,48(r3)  // store r17
     stfd    f17,200(r3) // store f17
     std     r18,56(r3)  // ...
     stfd    f18,208(r3)
     std     r19,64(r3)
     stfd    f19,216(r3)
     std     r20,72(r3)
     stfd    f20,224(r3)
     std     r21,80(r3)
     stfd    f21,232(r3)
     std     r22,88(r3)
     stfd    f22,240(r3)
     std     r23,96(r3)
     stfd    f23,248(r3)
     std     r24,104(r3)
     stfd    f24,256(r3)
     std     r25,112(r3)
     stfd    f25,264(r3)
     std     r26,120(r3)
     stfd    f26,272(r3)
     std     r27,128(r3)
     stfd    f27,280(r3)
     std     r28,136(r3)
     stfd    f28,288(r3)
     std     r29,144(r3)
     stfd    f29,296(r3)
     std     r30,152(r3)
     stfd    f30,304(r3)
     std     r31,160(r3)
     stfd    f31,312(r3)
     li      r3,0
  end;

procedure fpc_longjmp(var S : jmp_buf;value : longint);assembler;[Public, alias : 'FPC_LONGJMP']; nostackframe; compilerproc;
  asm
     ld      r1,0(r3)    // load r1
     ld      r2,8(r3)    // load r2
     ld      r0,16(r3)   // load lr
     ld      r14,24(r3)  // load r14
     lfd     f14,176(r3)
     ld      r15,32(r3)  // load r15
     lfd     f15,184(r3)
     ld      r16,40(r3)
     lfd     f16,192(r3)
     ld      r17,48(r3)
     lfd     f17,200(r3)
     ld      r18,56(r3)
     lfd     f18,208(r3)
     ld      r19,64(r3)
     lfd     f19,216(r3)
     ld      r20,72(r3)
     lfd     f20,224(r3)
     mtlr    r0
     ld      r21,80(r3)
     lfd     f21,232(r3)
     ld      r22,88(r3)
     lfd     f22,240(r3)
     lwz     r0,168(r3)
     ld      r23,96(r3)
     lfd     f23,248(r3)
     ld      r24,104(r3)
     lfd     f24,256(r3)
     ld      r25,112(r3)
     lfd     f25,264(r3)
     mtcrf   0xff,r0
     ld      r26,120(r3)
     lfd     f26,272(r3)
     ld      r27,128(r3)
     lfd     f27,280(r3)
     ld      r28,136(r3)
     lfd     f28,288(r3)
     ld      r29,144(r3)
     lfd     f29,296(r3)
     ld      r30,152(r3)
     lfd     f30,304(r3)
     ld      r31,160(r3)
     lfd     f31,312(r3)
     mr r3,r4
    end;

