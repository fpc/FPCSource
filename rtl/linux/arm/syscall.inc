{
    $Id$
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

function FpSysCall(sysnr:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL0'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  swi  #0x900071
  cmn  r0,#126
  bls  .LDone
  ldr  r1,.LErrno
  rsb  r0,r0,#0
  str  r0,[r1]
  mvn  r0,#0
  b    .LDone
.LErrno:
  .word Errno
.LDone:
end;


function FpSysCall(sysnr,param1:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL1'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  swi  #0x900071
  cmn  r0,#126
  bls  .LDone
  ldr  r1,.LErrno
  rsb  r0,r0,#0
  str  r0,[r1]
  mvn  r0,#0
  b    .LDone
.LErrno:
  .word Errno
.LDone:
end;


function FpSysCall(sysnr,param1,param2:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL2'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  swi  #0x900071
  cmn  r0,#126
  bls  .LDone
  ldr  r1,.LErrno
  rsb  r0,r0,#0
  str  r0,[r1]
  mvn  r0,#0
  b    .LDone
.LErrno:
  .word Errno
.LDone:
end;


function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL3'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  swi  #0x900071
  cmn  r0,#126
  bls  .LDone
  ldr  r1,.LErrno
  rsb  r0,r0,#0
  str  r0,[r1]
  mvn  r0,#0
  b    .LDone
.LErrno:
  .word Errno
.LDone:
end;


function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL4'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  stmfd r13!,{r4}
  ldr  r4,param4
  swi  #0x900071
  cmn  r0,#126
  bls  .LDone
  ldr  r1,.LErrno
  rsb  r0,r0,#0
  str  r0,[r1]
  mvn  r0,#0
  b    .LDone
.LErrno:
  .word Errno
.LDone:
  ldmfd r13!,{r4}
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL5'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  stmfd r13!,{r4-r5}
  ldr  r5,param5
  ldr  r4,param4
  swi  #0x900071
  cmn  r0,#126
  bls  .LDone
  ldr  r1,.LErrno
  rsb  r0,r0,#0
  str  r0,[r1]
  mvn  r0,#0
  b    .LDone
.LErrno:
  .word Errno
.LDone:
  ldmfd r13!,{r4-r5}
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL6'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  stmfd r13!,{r4-r6}
  ldr  r6,param6
  ldr  r5,param5
  ldr  r4,param4
  swi  #0x900071
  cmn  r0,#126
  bls  .LDone
  ldr  r1,.LErrno
  rsb  r0,r0,#0
  str  r0,[r1]
  mvn  r0,#0
  b    .LDone
.LErrno:
  .word Errno
.LDone:
  ldmfd r13!,{r4-r6}
end;


{
  $Log$
  Revision 1.5  2004-02-06 23:06:16  florian
    - killed tsyscallregs

  Revision 1.4  2004/01/20 21:01:57  florian
    * fixed setjump
    * fixed syscalls

  Revision 1.3  2003/12/29 19:24:12  florian
    + introduced PtrInt and PtrUInt
    * made strscan 64 bit safe

  Revision 1.2  2003/12/03 17:34:12  florian
    + started to implement arm syscalls

  Revision 1.1  2003/08/28 00:08:29  florian
    * syscall skeleton
}
