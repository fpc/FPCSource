{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
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
var
  temp, retaddress: longint;
asm
  mr  r0,r3
  sc
  bns   .LDone
  lis r10,(fpc_threadvar_relocate_proc)@ha
  lwz   r10,(fpc_threadvar_relocate_proc)@l(r10)
  cmpwi r10,0
  bne   .LThreaded
  lis   r4,(Errno+4)@ha
  stw   r3,(Errno+4)@l(r4)
  b     .LFailed
.LThreaded:
  stw   r3,temp
  mflr  r3
  mtctr r10
  lis   r4,(errno)@ha
  stw   r3,retaddress
  lwz   r3,(errno)@l(r4)
  bctrl
  lwz   r4,temp
  lwz   r5,retaddress
  stw   r4,0(r3)
  mtlr  r5
.LFailed:
  li    r3,-1
.LDone:
end;

function FpSysCall(sysnr,param1:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL1'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp, retaddress: longint;
asm
  mr  r0,r3
  mr  r3,r4
  sc
  bns   .LDone
  lis   r10,(fpc_threadvar_relocate_proc)@ha
  lwz   r10,(fpc_threadvar_relocate_proc)@l(r10)
  cmpwi r10,0
  bne   .LThreaded
  lis   r4,(Errno+4)@ha
  stw   r3,(Errno+4)@l(r4)
  b     .LFailed
.LThreaded:
  stw   r3,temp
  mflr  r3
  mtctr r10
  lis   r4,(errno)@ha
  stw   r3,retaddress
  lwz   r3,(errno)@l(r4)
  bctrl
  lwz   r4,temp
  lwz   r5,retaddress
  stw   r4,0(r3)
  mtlr  r5
.LFailed:
  li    r3,-1
.LDone:
end;


function FpSysCall(sysnr,param1,param2:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL2'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp, retaddress: longint;
asm
  mr  r0,r3
  mr  r3,r4
  mr  r4,r5
  sc
  bns   .LDone
  lis   r10,(fpc_threadvar_relocate_proc)@ha
  lwz   r10,(fpc_threadvar_relocate_proc)@l(r10)
  cmpwi r10,0
  bne   .LThreaded
  lis   r4,(Errno+4)@ha
  stw   r3,(Errno+4)@l(r4)
  b     .LFailed
.LThreaded:
  stw   r3,temp
  mflr  r3
  mtctr r10
  lis   r4,(errno)@ha
  stw   r3,retaddress
  lwz   r3,(errno)@l(r4)
  bctrl
  lwz   r4,temp
  lwz   r5,retaddress
  stw   r4,0(r3)
  mtlr  r5
.LFailed:
  li    r3,-1
.LDone:
end;

function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL3'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp, retaddress: longint;
asm
  mr  r0,r3
  mr  r3,r4
  mr  r4,r5
  mr  r5,r6
  sc
  bns   .LDone
  lis   r10,(fpc_threadvar_relocate_proc)@ha
  lwz   r10,(fpc_threadvar_relocate_proc)@l(r10)
  cmpwi r10,0
  bne   .LThreaded
  lis   r4,(Errno+4)@ha
  stw   r3,(Errno+4)@l(r4)
  b     .LFailed
.LThreaded:
  stw   r3,temp
  mflr  r3
  mtctr r10
  lis   r4,(errno)@ha
  stw   r3,retaddress
  lwz   r3,(errno)@l(r4)
  bctrl
  lwz   r4,temp
  lwz   r5,retaddress
  stw   r4,0(r3)
  mtlr  r5
.LFailed:
  li    r3,-1
.LDone:
end;


function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL4'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp, retaddress: longint;
asm
  mr  r0,r3
  mr  r3,r4
  mr  r4,r5
  mr  r5,r6
  mr  r6,r7
  sc
  bns   .LDone
  lis   r10,(fpc_threadvar_relocate_proc)@ha
  lwz   r10,(fpc_threadvar_relocate_proc)@l(r10)
  cmpwi r10,0
  bne   .LThreaded
  lis   r4,(Errno+4)@ha
  stw   r3,(Errno+4)@l(r4)
  b     .LFailed
.LThreaded:
  stw   r3,temp
  mflr  r3
  mtctr r10
  lis   r4,(errno)@ha
  stw   r3,retaddress
  lwz   r3,(errno)@l(r4)
  bctrl
  lwz   r4,temp
  lwz   r5,retaddress
  stw   r4,0(r3)
  mtlr  r5
.LFailed:
  li    r3,-1
.LDone:
end;

function FpSysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL5'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp, retaddress: longint;
asm
  mr  r0,r3
  mr  r3,r4
  mr  r4,r5
  mr  r5,r6
  mr  r6,r7
  mr  r7,r8
  sc
  bns   .LDone
  lis   r10,(fpc_threadvar_relocate_proc)@ha
  lwz   r10,(fpc_threadvar_relocate_proc)@l(r10)
  cmpwi r10,0
  bne   .LThreaded
  lis   r4,(Errno+4)@ha
  stw   r3,(Errno+4)@l(r4)
  b     .LFailed
.LThreaded:
  stw   r3,temp
  mflr  r3 
  mtctr r10     
  lis   r4,(errno)@ha
  stw   r3,retaddress
  lwz   r3,(errno)@l(r4)
  bctrl
  lwz   r4,temp
  lwz   r5,retaddress
  stw   r4,0(r3)  
  mtlr  r5
.LFailed:
  li    r3,-1   
.LDone:
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL6'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp, retaddress: longint;
asm
  mr  r0,r3
  mr  r3,r4
  mr  r4,r5
  mr  r5,r6
  mr  r6,r7
  mr  r7,r8
  mr  r8,r9
  sc
  bns   .LDone
  lis   r10,(fpc_threadvar_relocate_proc)@ha
  lwz   r10,(fpc_threadvar_relocate_proc)@l(r10)
  cmpwi r10,0
  bne   .LThreaded
  lis   r4,(Errno+4)@ha
  stw   r3,(Errno+4)@l(r4)
  b     .LFailed
.LThreaded:
  stw   r3,temp
  mflr  r3 
  mtctr r10     
  lis   r4,(errno)@ha
  stw   r3,retaddress
  lwz   r3,(errno)@l(r4)
  bctrl
  lwz   r4,temp
  lwz   r5,retaddress
  stw   r4,0(r3)  
  mtlr  r5
.LFailed:
  li    r3,-1   
.LDone:
end;


{
  $Log$
  Revision 1.12  2004-02-06 23:06:16  florian
    - killed tsyscallregs

  Revision 1.11  2003/12/28 20:55:10  jonas
    * fixed result of failed syscalls (no extra neg is needed)
    + support multi-threaded programs

  Revision 1.10  2003/11/29 22:54:32  jonas
    * more ppc fixes, hello world works again under linuxppc

  Revision 1.8  2003/11/15 19:01:27  florian
    * fixed rtl to work with the integrated fpc ppc assembler reader

  Revision 1.7  2003/06/17 16:39:58  jonas
    * fixed old syscall handling for ppc

  Revision 1.6  2003/05/23 21:58:30  jonas
    * fixed storing to errno for single threaded programs

  Revision 1.5  2003/05/11 16:07:55  jonas
    * fixed mmap for non-i386 non-m68k architectures (not sure about
      x86-64)

  Revision 1.4  2003/04/22 17:07:55  florian
    * there where two SYSCALL1 procedures for the powerpc, fixed

  Revision 1.3  2003/01/09 13:38:26  florian
    * syscall stuff fixed

  Revision 1.2  2002/12/22 16:00:28  jonas
    + added syscallh.inc, adapted syscall.inc

  Revision 1.1  2002/11/09 20:32:14  marco
   * powerpc version. Threadsafe errno access not yet done.

  Revision 1.1  2002/10/14 19:39:44  peter
    * syscall moved into seperate include

}

