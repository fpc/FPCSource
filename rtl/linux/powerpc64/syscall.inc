{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Thomas Schatzl,
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


{ This variable is needed in syscall.inc assembler code }
var
  fpc_threadvar_relocate_proc : TRelocateThreadVarHandler; public name 'FPC_THREADVAR_RELOCATE';
{$define THREADVAR_RELOCATED_ALREADY_DEFINED}

{*****************************************************************************
                     --- Main:The System Call Self ---
*****************************************************************************}

function FpSysCall(sysnr:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL0'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp : qword;
  retaddress: ptruint;
asm
  mr r0, r3
  sc
  bns .LDone
  lis r10, (fpc_threadvar_relocate_proc)@highesta
  ori r10, r10, (fpc_threadvar_relocate_proc)@highera
  sldi r10, r10, 32
  oris r10, r10, (fpc_threadvar_relocate_proc)@ha
  ld r10, (fpc_threadvar_relocate_proc)@l(r10)
  
  cmpdi r10, 0
  bne .LThreaded
  lis r4, (Errno+8)@highesta
  ori r4, r4, (Errno+8)@highera
  sldi r4, r4, 32
  oris r4, r4, (Errno+8)@ha
  stw r3, (Errno+8)@l(r4)
  b .LFailed
.LThreaded:
  std r3, temp
  mflr r3
  std r3, retaddress

  // load actual function entry into r9
  ld r9, 0(r10)
  // save old TOC value
  std r2, 40(r1)
  // move function entry into ctr
  mtctr r9
  // load new TOC value
  ld r2, 8(r10)
  // load environment pointer from TOC
  ld r11, 16(r10)
  // load parameters
  lis r4, (errno)@highesta
  ori r4, r4, (errno)@highera
  sldi r4, r4, 32
  oris r4, r4, (errno)@ha
  lwz r3, (errno)@l(r4)
  bctrl
  // restore old TOC value
  ld r2, 40(r1)
  
  ld r4, temp
  ld r5, retaddress
  std r4, 0(r3)
  mtlr r5
.LFailed:
  li r3, -1
.LDone:
end;

function FpSysCall(sysnr,param1:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL1'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp : qword;
  retaddress: ptruint;
asm
  mr r0, r3
  mr r3, r4
  sc
  bns .LDone
  lis r10, (fpc_threadvar_relocate_proc)@highesta
  ori r10, r10, (fpc_threadvar_relocate_proc)@highera
  sldi r10, r10, 32
  oris r10, r10, (fpc_threadvar_relocate_proc)@ha
  ld r10, (fpc_threadvar_relocate_proc)@l(r10)
  
  cmpdi r10, 0
  bne .LThreaded
  lis r4, (Errno+8)@highesta
  ori r4, r4, (Errno+8)@highera
  sldi r4, r4, 32
  oris r4, r4, (Errno+8)@ha
  stw r3, (Errno+8)@l(r4)
  b .LFailed
.LThreaded:
  std r3, temp
  mflr r3

  // load actual function entry into r9
  ld r9, 0(r10)
  // save old TOC value
  std r2, 40(r1)
  // move function entry into ctr
  mtctr r9
  // load new TOC value
  ld r2, 8(r10)
  // load environment pointer from TOC
  ld r11, 16(r10)
  // load parameters
  lis r4, (errno)@highesta
  ori r4, r4, (errno)@highera
  sldi r4, r4, 32
  oris r4, r4, (errno)@ha
  std r3, retaddress
  lwz r3, (errno)@l(r4)
  bctrl
  // restore old TOC value
  ld r2, 40(r1)

  ld r4, temp
  ld r5, retaddress
  std r4, 0(r3)
  mtlr r5
.LFailed:
  li r3, -1
.LDone:
end;

function FpSysCall(sysnr,param1,param2:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL2'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp : qword;
  retaddress: ptruint;
asm
  mr r0, r3
  mr r3, r4
  mr r4, r5
  sc
  bns .LDone
  lis r10, (fpc_threadvar_relocate_proc)@highesta
  ori r10, r10, (fpc_threadvar_relocate_proc)@highera
  sldi r10, r10, 32
  oris r10, r10, (fpc_threadvar_relocate_proc)@ha
  ld r10, (fpc_threadvar_relocate_proc)@l(r10)
  
  cmpdi r10, 0
  bne .LThreaded
  lis r4, (Errno+8)@highesta
  ori r4, r4, (Errno+8)@highera
  sldi r4, r4, 32
  oris r4, r4, (Errno+8)@ha
  stw r3, (Errno+8)@l(r4)
  b .LFailed
.LThreaded:
  std r3, temp
  mflr r3
  // load actual function entry into r9
  ld r9, 0(r10)
  // save old TOC value
  std r2, 40(r1)
  // move function entry into ctr
  mtctr r9
  // load new TOC value
  ld r2, 8(r10)
  // load environment pointer from TOC
  ld r11, 16(r10)
  // load parameters
  lis r4, (errno)@highesta
  ori r4, r4, (errno)@highera
  sldi r4, r4, 32
  oris r4, r4, (errno)@ha
  std r3, retaddress
  lwz r3, (errno)@l(r4)
  bctrl
  // restore old TOC value
  ld r2, 40(r1)

  ld r4, temp
  ld r5, retaddress
  std r4, 0(r3)
  mtlr r5
.LFailed:
  li r3, -1
.LDone:
end;


function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL3'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp : qword;
  retaddress: ptruint;
asm
  mr r0, r3
  mr r3, r4
  mr r4, r5
  mr r5, r6
  sc
  bns .LDone
  lis r10, (fpc_threadvar_relocate_proc)@highesta
  ori r10, r10, (fpc_threadvar_relocate_proc)@highera
  sldi r10, r10, 32
  oris r10, r10, (fpc_threadvar_relocate_proc)@ha
  ld r10, (fpc_threadvar_relocate_proc)@l(r10)
  
  cmpdi r10, 0
  bne .LThreaded
  lis r4, (Errno+8)@highesta
  ori r4, r4, (Errno+8)@highera
  sldi r4, r4, 32
  oris r4, r4, (Errno+8)@ha
  stw r3, (Errno+8)@l(r4)
  b .LFailed
.LThreaded:
  std r3, temp
  mflr r3

  // load actual function entry into r9
  ld r9, 0(r10)
  // save old TOC value
  std r2, 40(r1)
  // move function entry into ctr
  mtctr r9
  // load new TOC value
  ld r2, 8(r10)
  // load environment pointer from TOC
  ld r11, 16(r10)
  // load parameters
  lis r4, (errno)@highesta
  ori r4, r4, (errno)@highera
  sldi r4, r4, 32
  oris r4, r4, (errno)@ha
  std r3, retaddress
  lwz r3, (errno)@l(r4)
  bctrl

  // restore old TOC value
  ld r2, 40(r1)
  ld r4, temp
  ld r5, retaddress
  std r4, 0(r3)
  mtlr r5
.LFailed:
  li r3, -1
.LDone:
end;


function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL4'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp : qword;
  retaddress: ptruint;
asm
  mr r0, r3
  mr r3, r4
  mr r4, r5
  mr r5, r6
  mr r6, r7
  sc
  bns .LDone
  lis r10, (fpc_threadvar_relocate_proc)@highesta
  ori r10, r10, (fpc_threadvar_relocate_proc)@highera
  sldi r10, r10, 32
  oris r10, r10, (fpc_threadvar_relocate_proc)@ha
  ld r10, (fpc_threadvar_relocate_proc)@l(r10)
  
  cmpdi r10, 0
  bne .LThreaded
  lis r4, (Errno+8)@highesta
  ori r4, r4, (Errno+8)@highera
  sldi r4, r4, 32
  oris r4, r4, (Errno+8)@ha
  stw r3, (Errno+8)@l(r4)
  b .LFailed
.LThreaded:
  std r3, temp
  mflr r3

  // load actual function entry into r9
  ld r9, 0(r10)
  // save old TOC value
  std r2, 40(r1)
  // move function entry into ctr
  mtctr r9
  // load new TOC value
  ld r2, 8(r10)
  // load environment pointer from TOC
  ld r11, 16(r10)
  // load parameters
  lis r4, (errno)@highesta
  ori r4, r4, (errno)@highera
  sldi r4, r4, 32
  oris r4, r4, (errno)@ha
  std r3, retaddress
  lwz r3, (errno)@l(r4)
  bctrl

  // restore old TOC value
  ld r2, 40(r1)
  ld r4, temp
  ld r5, retaddress
  std r4, 0(r3)
  mtlr r5
.LFailed:
  li r3, -1
.LDone:
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL5'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp : qword;
  retaddress: ptruint;
asm
  mr r0, r3
  mr r3, r4
  mr r4, r5
  mr r5, r6
  mr r6, r7
  mr r7, r8
  sc
  bns .LDone
  lis r10, (fpc_threadvar_relocate_proc)@highesta
  ori r10, r10, (fpc_threadvar_relocate_proc)@highera
  sldi r10, r10, 32
  oris r10, r10, (fpc_threadvar_relocate_proc)@ha
  ld r10, (fpc_threadvar_relocate_proc)@l(r10)
  
  cmpdi r10, 0
  bne  .LThreaded
  lis r4, (Errno+8)@highesta
  ori r4, r4, (Errno+8)@highera
  sldi r4, r4, 32
  oris r4, r4, (Errno+8)@ha
  stw r3, (Errno+8)@l(r4)
  b .LFailed
.LThreaded:
  std r3, temp
  mflr r3

  // load actual function entry into r9
  ld r9, 0(r10)
  // save old TOC value
  std r2, 40(r1)
  // move function entry into ctr
  mtctr r9
  // load new TOC value
  ld r2, 8(r10)
  // load environment pointer from TOC
  ld r11, 16(r10)
  // load parameters
  lis r4, (errno)@highesta
  ori r4, r4, (errno)@highera
  sldi r4, r4, 32
  oris r4, r4, (errno)@ha
  std r3, retaddress
  lwz r3, (errno)@l(r4)
  bctrl
  // restore old TOC value
  ld r2, 40(r1)

  ld r4, temp
  ld r5, retaddress
  std r4, 0(r3)
  mtlr r5
.LFailed:
  li r3, -1
.LDone:
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL6'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp : qword;
  retaddress: ptruint;
asm
  mr r0, r3
  mr r3, r4
  mr r4, r5
  mr r5, r6
  mr r6, r7
  mr r7, r8
  mr r8, r9
  sc
  bns .LDone
  lis r10, (fpc_threadvar_relocate_proc)@highesta
  ori r10, r10, (fpc_threadvar_relocate_proc)@highera
  sldi r10, r10, 32
  oris r10, r10, (fpc_threadvar_relocate_proc)@ha
  ld r10, (fpc_threadvar_relocate_proc)@l(r10)
  
  cmpdi r10, 0
  bne .LThreaded
  lis r4, (Errno+8)@highesta
  ori r4, r4, (Errno+8)@highera
  sldi r4, r4, 32
  oris r4, r4, (Errno+8)@ha
  stw r3, (Errno+8)@l(r4)
  b .LFailed
.LThreaded:
  std r3, temp
  mflr r3

  // load actual function entry into r9
  ld r9, 0(r10)
  // save old TOC value
  std r2, 40(r1)
  // move function entry into ctr
  mtctr r9
  // load new TOC value
  ld r2, 8(r10)
  // load environment pointer from TOC
  ld r11, 16(r10)
  // load parameters
  lis r4, (errno)@highesta
  ori r4, r4, (errno)@highera
  sldi r4, r4, 32
  oris r4, r4, (errno)@ha
  std r3, retaddress
  lwz r3, (errno)@l(r4)
  bctrl
  // restore old TOC value
  ld r2, 40(r1)

  ld r4, temp
  ld r5, retaddress
  std r4, 0(r3)
  mtlr r5
.LFailed:
  li r3, -1
.LDone:
end;
