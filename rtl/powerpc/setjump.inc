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

function fpc_setjmp(var S : jmp_buf) : longint;assembler;[Public, alias : 'FPC_SETJMP']; nostackframe; compilerproc;
  asm
     stw     r1,0(r3)
     mflr    r0
     stw     r2,4(r3)
     stw     r14,12(r3)
{$ifndef FPUNONE}
     stfd    f14,88(r3)
{$endif}
     stw     r0,8(r3)
     stw     r15,16(r3)
{$ifndef FPUNONE}
     stfd    f15,96(r3)
{$endif}
     mfcr    r0
     stw     r16,20(r3)
{$ifndef FPUNONE}
     stfd    f16,104(r3)
{$endif}
     stw     r0,84(r3)
     stw     r17,24(r3)
{$ifndef FPUNONE}
     stfd    f17,112(r3)
{$endif}
     stw     r18,28(r3)
{$ifndef FPUNONE}
     stfd    f18,120(r3)
{$endif}
     stw     r19,32(r3)
{$ifndef FPUNONE}
     stfd    f19,128(r3)
{$endif}
     stw     r20,36(r3)
{$ifndef FPUNONE}
     stfd    f20,136(r3)
{$endif}
     stw     r21,40(r3)
{$ifndef FPUNONE}
     stfd    f21,144(r3)
{$endif}
     stw     r22,44(r3)
{$ifndef FPUNONE}
     stfd    f22,152(r3)
{$endif}
     stw     r23,48(r3)
{$ifndef FPUNONE}
     stfd    f23,160(r3)
{$endif}
     stw     r24,52(r3)
{$ifndef FPUNONE}
     stfd    f24,168(r3)
{$endif}
     stw     r25,56(r3)
{$ifndef FPUNONE}
     stfd    f25,176(r3)
{$endif}
     stw     r26,60(r3)
{$ifndef FPUNONE}
     stfd    f26,184(r3)
{$endif}
     stw     r27,64(r3)
{$ifndef FPUNONE}
     stfd    f27,192(r3)
{$endif}
     stw     r28,68(r3)
{$ifndef FPUNONE}
     stfd    f28,200(r3)
{$endif}
     stw     r29,72(r3)
{$ifndef FPUNONE}
     stfd    f29,208(r3)
{$endif}
     stw     r30,76(r3)
{$ifndef FPUNONE}
     stfd    f30,216(r3)
{$endif}
     stw     r31,80(r3)
{$ifndef FPUNONE}
     stfd    f31,224(r3)
{$endif}
     li      r3,0
  end;

procedure fpc_longjmp(var S : jmp_buf;value : longint);assembler;[Public, alias : 'FPC_LONGJMP']; nostackframe; compilerproc;
  asm
     lwz     r1,0(r3)
     lwz     r2,4(r3)
     lwz     r0,8(r3)
     lwz     r14,12(r3)
{$ifndef FPUNONE}
     lfd     f14,88(r3)
{$endif}
     lwz     r15,16(r3)
{$ifndef FPUNONE}
     lfd     f15,96(r3)
{$endif}
     lwz     r16,20(r3)
{$ifndef FPUNONE}
     lfd     f16,104(r3)
{$endif}
     lwz     r17,24(r3)
{$ifndef FPUNONE}
     lfd     f17,112(r3)
{$endif}
     lwz     r18,28(r3)
{$ifndef FPUNONE}
     lfd     f18,120(r3)
{$endif}
     lwz     r19,32(r3)
{$ifndef FPUNONE}
     lfd     f19,128(r3)
{$endif}
     lwz     r20,36(r3)
{$ifndef FPUNONE}
     lfd     f20,136(r3)
{$endif}
     mtlr    r0
     lwz     r21,40(r3)
{$ifndef FPUNONE}
     lfd     f21,144(r3)
{$endif}
     lwz     r22,44(r3)
{$ifndef FPUNONE}
     lfd     f22,152(r3)
{$endif}
     lwz     r0,84(r3)
     lwz     r23,48(r3)
{$ifndef FPUNONE}
     lfd     f23,160(r3)
{$endif}
     lwz     r24,52(r3)
{$ifndef FPUNONE}
     lfd     f24,168(r3)
{$endif}
     lwz     r25,56(r3)
{$ifndef FPUNONE}
     lfd     f25,176(r3)
{$endif}
     mtcrf   0xff,r0
     lwz     r26,60(r3)
{$ifndef FPUNONE}
     lfd     f26,184(r3)
{$endif}
     lwz     r27,64(r3)
{$ifndef FPUNONE}
     lfd     f27,192(r3)
{$endif}
     lwz     r28,68(r3)
{$ifndef FPUNONE}
     lfd     f28,200(r3)
{$endif}
     lwz     r29,72(r3)
{$ifndef FPUNONE}
     lfd     f29,208(r3)
{$endif}
     lwz     r30,76(r3)
{$ifndef FPUNONE}
     lfd     f30,216(r3)
{$endif}
     lwz     r31,80(r3)
{$ifndef FPUNONE}
     lfd     f31,224(r3)
{$endif}
     mr r3,r4
    end;

