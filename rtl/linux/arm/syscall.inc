{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by Florian Klaempfl,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{No debugging for syslinux include !}
{$IFDEF SYS_LINUX}
  {$UNDEF SYSCALL_DEBUG}
{$ENDIF SYS_LINUX}


{*****************************************************************************
                     --- Main:The System Call Self ---
*****************************************************************************}

{$ifdef FPC_ABI_EABI}

{$ifdef CPUTHUMB}

function FpSysCall(sysnr:TSysParam):TSysResult; assembler; nostackframe; [public,alias:'FPC_SYSCALL0'];
{
  Perform syscall and set errno variable if needed.
}
asm
  push  {r7}
  mov   r7,r0
  // svc   #0x0
  // GNU 2.22 does not like svc #0x0
  .byte 0,0xdf
  mov   r1,#126
  cmn   r0,r1
  pop   {r7}
  bhi   .Lnoexit
  mov   pc,lr
.Lnoexit:
  push  {lr}
  neg   r0,r0
  bl    seterrno
  mov   r0,#0
  mvn   r0,r0
  pop   {pc}
end;


function FpSysCall(sysnr,param1:TSysParam):TSysResult; assembler; nostackframe; [public,alias:'FPC_SYSCALL1'];
{
  Perform syscall and set errno variable if needed.
}
asm
  push  {r7}
  mov   r7,r0
  mov   r0,r1
  // svc   #0x0
  // GNU 2.22 does not like svc #0x0
  .byte 0,0xdf
  mov   r1,#126
  cmn   r0,r1
  pop   {r7}
  bhi   .Lnoexit
  mov   pc,lr
.Lnoexit:
  push  {lr}
  neg   r0,r0
  bl    seterrno
  mov   r0,#0
  mvn   r0,r0
  pop   {pc}
end;


function FpSysCall(sysnr,param1,param2:TSysParam):TSysResult; assembler; nostackframe; [public,alias:'FPC_SYSCALL2'];
{
  Perform syscall and set errno variable if needed.
}
asm
  push  {r7}
  mov   r7,r0
  mov   r0,r1
  mov   r1,r2
  // svc   #0x0
  // GNU 2.22 does not like svc #0x0
  .byte 0,0xdf
  mov   r1,#126
  cmn   r0,r1
  pop   {r7}
  bhi   .Lnoexit
  mov   pc,lr
.Lnoexit:
  push  {lr}
  neg   r0,r0
  bl    seterrno
  mov   r0,#0
  mvn   r0,r0
  pop   {pc}
end;


function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler; nostackframe; [public,alias:'FPC_SYSCALL3'];
{
  Perform syscall and set errno variable if needed.
}
asm
  push  {r7}
  mov   r7,r0
  mov   r0,r1
  mov   r1,r2
  mov   r2,r3
  // svc   #0x0
  // GNU 2.22 does not like svc #0x0
  .byte 0,0xdf
  mov   r1,#126
  cmn   r0,r1
  pop   {r7}
  bhi   .Lnoexit
  mov   pc,lr
.Lnoexit:
  push  {lr}
  neg   r0,r0
  bl    seterrno
  mov   r0,#0
  mvn   r0,r0
  pop   {pc}
end;


function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler; nostackframe; [public,alias:'FPC_SYSCALL4'];
{
  Perform syscall and set errno variable if needed.
}
asm
  push  {r7}
  push  {r0}
  mov   r0,r1
  mov   r1,r2
  mov   r2,r3
  ldr   r3,[r13 ,#8]
  pop   {r7}
  // svc   #0x0
  // GNU 2.22 does not like svc #0x0
  .byte 0,0xdf
  mov   r1,#126
  cmn   r0,r1
  pop   {r7}
  bls   .LExit
  push   {lr}
  neg   r0,r0
  bl    seterrno
  mov   r0,#0
  mvn   r0,r0
  pop   {pc}
.LExit:
  mov   pc,lr
end;

function FpSysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult; assembler; nostackframe; [public,alias:'FPC_SYSCALL5'];
{
  Perform syscall and set errno variable if needed.
}
asm
  push  {r4,r7}
  push  {r0}
  mov   r0,r1
  mov   r1,r2
  mov   r2,r3
  ldr   r3,[r13 ,#12]
  ldr   r4,[r13 ,#16]
  pop   {r7}
  // svc   #0x0
  // GNU 2.22 does not like svc #0x0
  .byte 0,0xdf
  mov   r1,#126
  cmn   r0,r1
  pop   {r4,r7}
  bls   .LExit
  push   {lr}
  neg   r0,r0
  bl    seterrno
  mov   r0,#0
  mvn   r0,r0
  pop   {pc}
.LExit:
  mov   pc,lr
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):TSysResult; assembler; nostackframe; [public,alias:'FPC_SYSCALL6'];
{
  Perform syscall and set errno variable if needed.
}
asm
  push  {r4,r5,r7}
  push  {r0}
  mov   r0,r1
  mov   r1,r2
  mov   r2,r3
  ldr   r3,[r13 ,#16]
  ldr   r4,[r13 ,#20]
  ldr   r5,[r13 ,#24]
  pop   {r7}
  // svc   #0x0
  // GNU 2.22 does not like svc #0x0
  .byte 0,0xdf
  mov   r1,#126
  cmn   r0,r1
  pop   {r4,r5,r7}
  bls   .LExit
  push   {lr}
  neg   r0,r0
  bl    seterrno
  mov   r0,#0
  mvn   r0,r0
  pop   {pc}
.LExit:
  mov   pc,lr
end;


{$else CPUTHUMB}
function FpSysCall(sysnr:TSysParam):TSysResult; assembler; nostackframe; [public,alias:'FPC_SYSCALL0'];
{
  Perform syscall and set errno variable if needed.
}
asm
  str   r7,[sp,#-4]!
  mov   r7,r0
  swi   #0x0
  cmn   r0,#126
  ldr   r7,[sp],#4
  it ls
  movls pc,lr
  stmfd sp!,{lr}
  rsb   r0,r0,#0
  bl    seterrno
  mvn   r0,#0
  ldmfd sp!,{pc}
end;


function FpSysCall(sysnr,param1:TSysParam):TSysResult; assembler; nostackframe; [public,alias:'FPC_SYSCALL1'];
{
  Perform syscall and set errno variable if needed.
}
asm
  str   r7,[sp,#-4]!
  mov   r7,r0
  mov   r0,r1
  swi   #0x0
  cmn   r0,#126
  ldr   r7,[sp],#4
  it ls
  movls pc,lr
  stmfd sp!,{lr}
  rsb   r0,r0,#0
  bl    seterrno
  mvn   r0,#0
  ldmfd sp!,{pc}
end;


function FpSysCall(sysnr,param1,param2:TSysParam):TSysResult; assembler; nostackframe; [public,alias:'FPC_SYSCALL2'];
{
  Perform syscall and set errno variable if needed.
}
asm
  str   r7,[sp,#-4]!
  mov   r7,r0
  mov   r0,r1
  mov   r1,r2
  swi   #0x0
  cmn   r0,#126
  ldr   r7,[sp],#4
  it ls
  movls pc,lr
  stmfd sp!,{lr}
  rsb   r0,r0,#0
  bl    seterrno
  mvn   r0,#0
  ldmfd sp!,{pc}
end;


function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler; nostackframe; [public,alias:'FPC_SYSCALL3'];
{
  Perform syscall and set errno variable if needed.
}
asm
  str   r7,[sp,#-4]!
  mov   r7,r0
  mov   r0,r1
  mov   r1,r2
  mov   r2,r3
  swi   #0x0
  cmn   r0,#126
  ldr   r7,[sp],#4
  it ls
  movls pc,lr
  stmfd sp!,{lr}
  rsb   r0,r0,#0
  bl    seterrno
  mvn   r0,#0
  ldmfd sp!,{pc}
end;


function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler; [public,alias:'FPC_SYSCALL4'];
{
  Perform syscall and set errno variable if needed.
}
asm
  str   r7,[sp,#-4]!
  mov   r7,r0
  mov   r0,r1
  mov   r1,r2
  mov   r2,r3
  ldr   r3,param4
  swi   #0x0
  cmn   r0,#126
  ldr   r7,[sp],#4
  bls   .LExit
  rsb   r0,r0,#0
  bl    seterrno
  mvn   r0,#0
.LExit:
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL5'];
{
  Perform syscall and set errno variable if needed.
}
asm
  stmfd sp!,{r4,r7}
  mov   r7,r0
  mov   r0,r1
  mov   r1,r2
  mov   r2,r3
  ldr   r3,param4
  ldr   r4,param5
  swi   #0x0
  cmn   r0,#126
  ldmfd sp!,{r4,r7}
  bls   .LExit
  rsb   r0,r0,#0
  bl    seterrno
  mvn   r0,#0
.LExit:
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL6'];
{
  Perform syscall and set errno variable if needed.
}
asm
  stmfd sp!,{r4,r5,r7}
  mov   r7,r0
  mov   r0,r1
  mov   r1,r2
  mov   r2,r3
  ldr   r3,param4
  ldr   r4,param5
  ldr   r5,param6
  swi   #0x0
  cmn   r0,#126
  ldmfd sp!,{r4,r5,r7}
  bls   .LExit
  rsb   r0,r0,#0
  bl    seterrno
  mvn   r0,#0
.LExit:
end;

{$endif CPUTHUMB}

{$else FPC_ABI_EABI}

function FpSysCall(sysnr:TSysParam):TSysResult; assembler; nostackframe; [public,alias:'FPC_SYSCALL0'];
{
  Perform syscall and set errno variable if needed.
}
asm
  swi   #0x900071
  cmn   r0,#126
  movls pc,lr
  stmfd sp!,{lr}
  rsb   r0,r0,#0
  bl    seterrno
  mvn   r0,#0
  ldmfd sp!,{pc}
end;

procedure FPC_SYSCALL; external name 'FPC_SYSCALL0';

function FpSysCall(sysnr,param1:TSysParam):TSysResult; assembler; nostackframe; [public,alias:'FPC_SYSCALL1'];
{
  Jump to FPC_SYSCALL0 because code for FPC_SYSCALL0-FPC_SYSCALL3 is the same.
}
asm
  b FPC_SYSCALL
end;


function FpSysCall(sysnr,param1,param2:TSysParam):TSysResult; assembler; nostackframe; [public,alias:'FPC_SYSCALL2'];
{
  Jump to FPC_SYSCALL0 because code for FPC_SYSCALL0-FPC_SYSCALL3 is the same.
}
asm
  b FPC_SYSCALL
end;


function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler; nostackframe; [public,alias:'FPC_SYSCALL3'];
{
  Jump to FPC_SYSCALL0 because code for FPC_SYSCALL0-FPC_SYSCALL3 is the same.
}
asm
  b FPC_SYSCALL
end;


function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler; [public,alias:'FPC_SYSCALL4'];
{
  This function loads some parameters from stack and calls FPC_SYSCALL0.
}
asm
  stmfd sp!,{r4}
  ldr  r4,param4
  bl FPC_SYSCALL
  ldmfd sp!,{r4}
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL5'];
{
  This function loads some parameters from stack and calls FPC_SYSCALL0.
}
asm
  stmfd sp!,{r4,r5}
  ldr  r4,param4
  ldr  r5,param5
  bl FPC_SYSCALL
  ldmfd sp!,{r4,r5}
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL6'];
{
  This function loads some parameters from stack and calls FPC_SYSCALL0.
}
asm
  stmfd sp!,{r4,r5,r6}
  ldr  r4,param4
  ldr  r5,param5
  ldr  r6,param6
  bl FPC_SYSCALL
  ldmfd sp!,{r4,r5,r6}
end;
{$endif FPC_ABI_EABI}
