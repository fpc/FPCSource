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
asm
  mr  r0,r3
  sc
  blr
  neg   r3, r3
  lis   r4,(Errno+4)@ha
  stw   r3,(Errno+4)@l(r4)
  li    r3,-1
end;

function FpSysCall(sysnr,param1:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL1'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  mr  r0,r3
  mr  r3,r4
  sc
  blr
  neg   r3, r3
  lis   r4,(Errno+4)@ha
  stw   r3,(Errno+4)@l(r4)
  li    r3,-1
end;


function FpSysCall(sysnr,param1,param2:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL2'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  mr  r0,r3
  mr  r3,r4
  mr  r4,r5
  sc
  blr
  neg   r3, r3
  lis   r4,(Errno+4)@ha
  stw   r3,(Errno+4)@l(r4)
  li    r3,-1
end;

function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL3'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  mr  r0,r3
  mr  r3,r4
  mr  r4,r5
  mr  r5,r6
  sc
  blr
  neg   r3, r3
  lis   r4,(Errno+4)@ha
  stw   r3,(Errno+4)@l(r4)
  li    r3,-1
end;


function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL4'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  mr  r0,r3
  mr  r3,r4
  mr  r4,r5
  mr  r5,r6
  mr  r6,r7
  sc
  blr
  neg   r3, r3
  lis   r4,(Errno+4)@ha
  stw   r3,(Errno+4)@l(r4)
  li    r3,-1
end;

function FpSysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL5'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  mr  r0,r3
  mr  r3,r4
  mr  r4,r5
  mr  r5,r6
  mr  r6,r7
  mr  r7,r8
  sc
  blr
  neg   r3, r3
  lis   r4,(Errno+4)@ha
  stw   r3,(Errno+4)@l(r4)
  li    r3,-1
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL6'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  mr  r0,r3
  mr  r3,r4
  mr  r4,r5
  mr  r5,r6
  mr  r6,r7
  mr  r7,r8
  mr  r8,r9
  sc
  blr
  neg   r3, r3
  lis   r4,(Errno+4)@ha
  stw   r3,(Errno+4)@l(r4)
  li    r3,-1
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6,param7:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL7'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  mr  r0,r3
  mr  r3,r4
  mr  r4,r5
  mr  r5,r6
  mr  r6,r7
  mr  r7,r8
  mr  r8,r9
  mr  r9,r10
  sc
  blr
  neg   r3, r3
  lis   r4,(Errno+4)@ha
  stw   r3,(Errno+4)@l(r4)
  li    r3,-1
end;

{
  $Log$
  Revision 1.2  2003-05-25 13:51:58  jonas
    * several fixes, addition of Mach trap numbers (thye are simply syscalls
      with a negative number)

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

