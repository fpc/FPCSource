{
    $Id$

    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team.

    Processor dependent implementation for the system unit for
    ARM

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$asmmode gas}

procedure fpc_cpuinit;
begin
  asm
    rfs r0
    and r0,r0,#0xffe0ffff
    orr r0,r0,#0x00020000
    wfs r0
  end;
end;

{****************************************************************************
                       stack frame related stuff
****************************************************************************}

{$define FPC_SYSTEM_HAS_GET_FRAME}
function get_frame:pointer;assembler;nostackframe;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
  mov    r0,r11
end ['R0'];


{$define FPC_SYSTEM_HAS_GET_CALLER_ADDR}
function get_caller_addr(framebp:pointer):pointer;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
  movs r0,r0
  beq .Lg_a_null
  ldr r0,[r0,#-4]
.Lg_a_null:
end ['R0'];


{$define FPC_SYSTEM_HAS_GET_CALLER_FRAME}
function get_caller_frame(framebp:pointer):pointer;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
  movs r0,r0
  beq .Lgnf_null
  ldr r0,[r0,#-12]
.Lgnf_null:
end ['R0'];


{$define FPC_SYSTEM_HAS_SPTR}
Function Sptr : pointer;assembler;{$ifdef SYSTEMINLINE}inline;{$endif}
asm
  mov    r0,sp
end ['R0'];


{$define FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:longint;value:byte);assembler;nostackframe;
asm
        // less than 0?
        cmp r1,#0
        movlt pc,lr
        mov     r3,r0
        cmp     r1,#8           // at least 8 bytes to do?
        blt     .LFillchar2
        orr r2,r2,r2,lsl #8
        orr r2,r2,r2,lsl #16
.LFillchar0:
        tst     r3,#3           // aligned yet?
        strneb r2,[r3],#1
        subne   r1,r1,#1
        bne     .LFillchar0
        mov     ip,r2
.LFillchar1:
        cmp     r1,#8           // 8 bytes still to do?
        blt     .LFillchar2
        stmia   r3!,{r2,ip}
        sub     r1,r1,#8
        cmp     r1,#8           // 8 bytes still to do?
        blt     .LFillchar2
        stmia   r3!,{r2,ip}
        sub     r1,r1,#8
        cmp     r1,#8           // 8 bytes still to do?
        blt     .LFillchar2
        stmia   r3!,{r2,ip}
        sub     r1,r1,#8
        cmp     r1,#8           // 8 bytes still to do?
        stmgeia r3!,{r2,ip}
        subge   r1,r1,#8
        bge     .LFillchar1
.LFillchar2:
        movs r1,r1              // anything left?
        moveq pc,lr
        rsb     r1,r1,#7
        add     pc,pc,r1,lsl #2
        mov     r0,r0
        strb r2,[r3],#1
        strb r2,[r3],#1
        strb r2,[r3],#1
        strb r2,[r3],#1
        strb r2,[r3],#1
        strb r2,[r3],#1
        strb r2,[r3],#1
        mov pc,lr
end;


{
  $Log$
  Revision 1.11  2005-02-14 17:13:21  peter
    * truncate log

  Revision 1.10  2005/01/05 15:59:02  florian
    + added nostackframe directive to get_frame

  Revision 1.9  2005/01/05 15:21:14  florian
    * fillchar fixed; it's used now

  Revision 1.8  2005/01/04 16:46:38  florian
    + correct setting of FPU exception mask

}
