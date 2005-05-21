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

function FpSysCall(sysnr:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL0'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  swi  #0x900071
  cmn  r0,#126
  bls  .LDone
  rsb  r4,r0,#0
  ldr  r2,.Lthread_var
  ldr  r2,[r2]
  ldr  r0,.LErrno
  cmp  r2,#0
  bne  .LThread
  str  r4,[r0,#4]
  mvn  r0,#0
  b    .LDone
.LThread:
  mov  pc,r2
  str  r4,[r0]
  mvn  r0,#0
  b    .LDone
.LErrno:
  .word Errno
.Lthread_var:
  .word fpc_threadvar_relocate_proc
.LDone:
end ['r4'];


function FpSysCall(sysnr,param1:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL1'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  swi  #0x900071
  cmn  r0,#126
  bls  .LDone
  rsb  r4,r0,#0
  ldr  r2,.Lthread_var
  ldr  r2,[r2]
  ldr  r0,.LErrno
  cmp  r2,#0
  bne  .LThread
  str  r4,[r0,#4]
  mvn  r0,#0
  b    .LDone
.LThread:
  mov  pc,r2
  str  r4,[r0]
  mvn  r0,#0
  b    .LDone
.LErrno:
  .word Errno
.Lthread_var:
  .word fpc_threadvar_relocate_proc
.LDone:
end ['r4'];


function FpSysCall(sysnr,param1,param2:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL2'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  swi  #0x900071
  cmn  r0,#126
  bls  .LDone
  rsb  r4,r0,#0
  ldr  r2,.Lthread_var
  ldr  r2,[r2]
  ldr  r0,.LErrno
  cmp  r2,#0
  bne  .LThread
  str  r4,[r0,#4]
  mvn  r0,#0
  b    .LDone
.LThread:
  mov  pc,r2
  str  r4,[r0]
  mvn  r0,#0
  b    .LDone
.LErrno:
  .word Errno
.Lthread_var:
  .word fpc_threadvar_relocate_proc
.LDone:
end ['r4'];


function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL3'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  swi  #0x900071
  cmn  r0,#126
  bls  .LDone
  rsb  r4,r0,#0
  ldr  r2,.Lthread_var
  ldr  r2,[r2]
  ldr  r0,.LErrno
  cmp  r2,#0
  bne  .LThread
  str  r4,[r0,#4]
  mvn  r0,#0
  b    .LDone
.LThread:
  mov  pc,r2
  str  r4,[r0]
  mvn  r0,#0
  b    .LDone
.LErrno:
  .word Errno
.Lthread_var:
  .word fpc_threadvar_relocate_proc
.LDone:
end ['r4'];


function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL4'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  ldr  r4,param4
  swi  #0x900071
  cmn  r0,#126
  bls  .LDone
  rsb  r4,r0,#0
  ldr  r2,.Lthread_var
  ldr  r2,[r2]
  ldr  r0,.LErrno
  cmp  r2,#0
  bne  .LThread
  str  r4,[r0,#4]
  mvn  r0,#0
  b    .LDone
.LThread:
  mov  pc,r2
  str  r4,[r0]
  mvn  r0,#0
  b    .LDone
.LErrno:
  .word Errno
.Lthread_var:
  .word fpc_threadvar_relocate_proc
.LDone:
end ['r4'];


function FpSysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL5'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  ldr  r5,param5
  ldr  r4,param4
  swi  #0x900071
  cmn  r0,#126
  bls  .LDone
  rsb  r4,r0,#0
  ldr  r2,.Lthread_var
  ldr  r2,[r2]
  ldr  r0,.LErrno
  cmp  r2,#0
  bne  .LThread
  str  r4,[r0,#4]
  mvn  r0,#0
  b    .LDone
.LThread:
  mov  pc,r2
  str  r4,[r0]
  mvn  r0,#0
  b    .LDone
.LErrno:
  .word Errno
.Lthread_var:
  .word fpc_threadvar_relocate_proc
.LDone:
end ['r4','r5'];


function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL6'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  ldr  r6,param6
  ldr  r5,param5
  ldr  r4,param4
  swi  #0x900071
  cmn  r0,#126
  bls  .LDone
  rsb  r4,r0,#0
  ldr  r2,.Lthread_var
  ldr  r2,[r2]
  ldr  r0,.LErrno
  cmp  r2,#0
  bne  .LThread
  str  r4,[r0,#4]
  mvn  r0,#0
  b    .LDone
.LThread:
  mov  pc,r2
  str  r4,[r0]
  mvn  r0,#0
  b    .LDone
.LErrno:
  .word Errno
.Lthread_var:
  .word fpc_threadvar_relocate_proc
.LDone:
end ['r4','r5','r6'];


{
  $Log: syscall.inc,v $
  Revision 1.7  2005/02/14 17:13:30  peter
    * truncate log

  Revision 1.6  2005/01/04 14:13:58  florian
    * fixed error handling of system calls

}
