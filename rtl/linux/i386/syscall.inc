{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    The syscalls for the new RTL, moved to platform dependant dir.
    Old linux calling convention is stil kept.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$ASMMODE ATT}

function FpSysCall(sysnr:TSysParam):TSysResult; assembler; oldfpccall;[public,alias:'FPC_SYSCALL0'];

asm
{ load the registers... }
  movl  sysnr,%eax
  int   $0x80
  cmpl  $-4095,%eax
  jb   .LSyscOK
  negl  %eax
{$ifdef REGCALL}
  movl  fpc_threadvar_relocate_proc,%ecx
  testl %ecx,%ecx
  jne   .LThread
  movl  %eax,Errno+4
  jmp   .LNoThread
.LThread:
  movl  %eax,%ebx
  movl Errno,%eax
  call  *%ecx
  movl  %ebx,(%eax)
.LNoThread:
{$else}
  movl  %eax,%edx
  movl  fpc_threadvar_relocate_proc,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  jmp   .LNoThread
.LThread:
  pushl %edx
  pushl Errno
  call  *%eax
  popl  %edx
  movl  %edx,(%eax)
.LNoThread:
{$endif REGCALL}
  movl  $-1,%eax
.LSyscOK:
end;

function FpSysCall(sysnr,param1 : TSysParam):TSysResult; assembler; oldfpccall;[public,alias:'FPC_SYSCALL1'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  int $0x80
  cmpl  $-4095,%eax
  jb   .LSyscOK
  negl  %eax
{$ifdef REGCALL}
  movl  fpc_threadvar_relocate_proc,%ecx
  testl %ecx,%ecx
  jne   .LThread
  movl  %eax,Errno+4
  jmp   .LNoThread
.LThread:
  movl  %eax,%ebx
  movl Errno,%eax
  call  *%ecx
  movl  %ebx,(%eax)
.LNoThread:
{$else}
  movl  %eax,%edx
  movl  fpc_threadvar_relocate_proc,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  jmp   .LNoThread
.LThread:
  pushl %edx
  pushl Errno
  call  *%eax
  popl  %edx
  movl  %edx,(%eax)
.LNoThread:
{$endif REGCALL}
  movl  $-1,%eax
.LSyscOK:
end;

function FpSysCall(sysnr,param1,param2 : TSysParam):TSysResult; assembler; oldfpccall; [public,alias:'FPC_SYSCALL2'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  movl param2,%ecx
  int $0x80
  cmpl  $-4095,%eax
  jb   .LSyscOK
  negl  %eax
{$ifdef REGCALL}
  movl  fpc_threadvar_relocate_proc,%ecx
  testl %ecx,%ecx
  jne   .LThread
  movl  %eax,Errno+4
  jmp   .LNoThread
.LThread:
  movl  %eax,%ebx
  movl Errno,%eax
  call  *%ecx
  movl  %ebx,(%eax)
.LNoThread:
{$else}
  movl  %eax,%edx
  movl  fpc_threadvar_relocate_proc,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  jmp   .LNoThread
.LThread:
  pushl %edx
  pushl Errno
  call  *%eax
  popl  %edx
  movl  %edx,(%eax)
.LNoThread:
{$endif REGCALL}
  movl  $-1,%eax
.LSyscOK:
end;

function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler; oldfpccall; [public,alias:'FPC_SYSCALL3'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  movl param2,%ecx
  movl param3,%edx
  int $0x80
  cmpl  $-4095,%eax
  jb   .LSyscOK
  negl  %eax
{$ifdef REGCALL}
  movl  fpc_threadvar_relocate_proc,%ecx
  testl %ecx,%ecx
  jne   .LThread
  movl  %eax,Errno+4
  jmp   .LNoThread
.LThread:
  movl  %eax,%ebx
  movl Errno,%eax
  call  *%ecx
  movl  %ebx,(%eax)
.LNoThread:
{$else}
  movl  %eax,%edx
  movl  fpc_threadvar_relocate_proc,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  jmp   .LNoThread
.LThread:
  pushl %edx
  pushl Errno
  call  *%eax
  popl  %edx
  movl  %edx,(%eax)
.LNoThread:
{$endif REGCALL}
  movl  $-1,%eax
.LSyscOK:
end;

function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler; oldfpccall; [public,alias:'FPC_SYSCALL4'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  movl param2,%ecx
  movl param3,%edx
  movl param4,%esi
  int $0x80
  cmpl  $-4095,%eax
  jb   .LSyscOK
  negl  %eax
{$ifdef REGCALL}
  movl  fpc_threadvar_relocate_proc,%ecx
  testl %ecx,%ecx
  jne   .LThread
  movl  %eax,Errno+4
  jmp   .LNoThread
.LThread:
  movl  %eax,%ebx
  movl Errno,%eax
  call  *%ecx
  movl  %ebx,(%eax)
.LNoThread:
{$else}
  movl  %eax,%edx
  movl  fpc_threadvar_relocate_proc,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  jmp   .LNoThread
.LThread:
  pushl %edx
  pushl Errno
  call  *%eax
  popl  %edx
  movl  %edx,(%eax)
.LNoThread:
{$endif REGCALL}
  movl  $-1,%eax
.LSyscOK:
end;

function FpSysCall(sysnr,param1,param2,param3,param4,param5 : TSysParam):TSysResult; assembler; oldfpccall;[public,alias:'FPC_SYSCALL5'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  movl param2,%ecx
  movl param3,%edx
  movl param4,%esi
  movl param5,%edi
  int $0x80
  cmpl  $-4095,%eax
  jb   .LSyscOK
  negl  %eax
{$ifdef REGCALL}
  movl  fpc_threadvar_relocate_proc,%ecx
  testl %ecx,%ecx
  jne   .LThread
  movl  %eax,Errno+4
  jmp   .LNoThread
.LThread:
  movl  %eax,%ebx
  movl Errno,%eax
  call  *%ecx
  movl  %ebx,(%eax)
.LNoThread:
{$else}
  movl  %eax,%edx
  movl  fpc_threadvar_relocate_proc,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  jmp   .LNoThread
.LThread:
  pushl %edx
  pushl Errno
  call  *%eax
  popl  %edx
  movl  %edx,(%eax)
.LNoThread:
{$endif REGCALL}
  movl  $-1,%eax
.LSyscOK:
end;

{$ifdef notsupported}
{ Only 5 params are pushed, so it'll not work as expected (PFV) }
function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6 : TSysParam):TSysResult; assembler; oldfpccall;[public,alias:'FPC_SYSCALL6'];

asm
{ load the registers... }
  movl sysnr,%eax
  movl param1,%ebx
  movl param2,%ecx
  movl param3,%edx
  movl param4,%esi
  movl param5,%edi
  int $0x80
  cmpl  $-4095,%eax
  jb   .LSyscOK
  negl  %eax
{$ifdef REGCALL}
  movl  fpc_threadvar_relocate_proc,%ecx
  testl %ecx,%ecx
  jne   .LThread
  movl  %eax,Errno+4
  jmp   .LNoThread
.LThread:
  movl  %eax,%ebx
  movl Errno,%eax
  call  *%ecx
  movl  %ebx,(%eax)
.LNoThread:
{$else}
  movl  %eax,%edx
  movl  fpc_threadvar_relocate_proc,%eax
  testl %eax,%eax
  jne   .LThread
  movl  %edx,Errno+4
  jmp   .LNoThread
.LThread:
  pushl %edx
  pushl Errno
  call  *%eax
  popl  %edx
  movl  %edx,(%eax)
.LNoThread:
{$endif REGCALL}
  movl  $-1,%eax
.LSyscOK:
end;
{$endif notsupported}

{No debugging for syslinux include !}
{$IFDEF SYS_LINUX}
  {$UNDEF SYSCALL_DEBUG}
{$ENDIF SYS_LINUX}


{
  $Log: syscall.inc,v $
  Revision 1.18  2005/03/07 08:27:57  florian
    * applied syscall patch from C Western

  Revision 1.17  2005/02/14 17:13:30  peter
    * truncate log

}

