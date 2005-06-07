{

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


(*
{$define FPC_SYSTEM_HAS_MOVE}
procedure Move(const source;var dest;count:longint);[public, alias: 'FPC_MOVE'];assembler;nostackframe;
asm
	// count <=0 ?
	cmp r2,#0
	movle pc,lr
	// overlap?
	cmp r2,r1
	bls .Lnoverlap
	add r3,r0,r2
	cmp r3,r2
	bls .Lnooverlap
	// overlap, copy backward
.Loverlapped
  subs r2,r2,#1
  ldrb r3,[r0,r2]
  strb r3,[r1,r2]
  bne .Loverlapped
  mov pc,lr
.Lnooverlap:
  // less then 16 bytes to copy?
  cmp r2,#16
  // yes, the forget about the whole optimizations
  // and do a bytewise copy
  blt .Lbyteloop

  // both uneven aligned?
  eor r3,r0,r1
  tst r3,#1

  bne .Ldifferentaligned
  // yes, then align
  // alignment to 4 byte boundries is enough
  ldrb ip,[r0],#1
  sub r2,r2,#1
  stb ip,[r1],#1
  tst r3,#2
  bne .Ldifferentaligned
  ldrh ip,[r0],#2
  sub r2,r2,#2
  sth ip,[r1],#2

.Ldifferentaligned
  // qword aligned?
  orrs r3,r0,r1
  tst r3,#7
  bne .Ldwordloop
.Lqwordloop:
  sub r2,r2,#8
  ldmia r0!,{r3,ip}
  stmia r1!,{r3,ip}



.Lbyteloop:
  ldrb r3,[r0],#1
  strb r3,[r1],#1
  subs r2,r2,#1
  bne .Lbyteloop
  mov pc,lr
end;
*)


