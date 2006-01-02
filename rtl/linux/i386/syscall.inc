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

Procedure fpc_geteipasebx;[external name 'fpc_geteipasebx'];

function FpSysCall(sysnr:TSysParam):TSysResult; assembler; oldfpccall;[public,alias:'FPC_SYSCALL0'];

asm
{ load the registers... }
  movl  sysnr,%eax
  int   $0x80
  cmpl  $-4095,%eax
  jb   .LSyscOK
  negl  %eax
{$ifdef FPC_PIC}
  call  fpc_geteipasebx
  addl  $_GLOBAL_OFFSET_TABLE_,%ebx
  movl  fpc_threadvar_relocate_proc@GOT(%ebx),%ecx
  movl  (%ecx),%ecx
  movl  Errno@GOT(%ebx),%edi
{$else FPC_PIC}
  leal  Errno,%edi
  movl  fpc_threadvar_relocate_proc,%ecx
{$endif FPC_PIC}
  testl %ecx,%ecx
  jne   .LThread
  movl  %eax,4(%edi)
  jmp   .LNoThread
.LThread:
  movl  %eax,%ebx
  movl  (%edi),%eax
  call  *%ecx
  movl  %ebx,(%eax)
.LNoThread:
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
{$ifdef FPC_PIC}
  call  fpc_geteipasebx
  addl  $_GLOBAL_OFFSET_TABLE_,%ebx
  movl  fpc_threadvar_relocate_proc@GOT(%ebx),%ecx
  movl  (%ecx),%ecx
  movl  Errno@GOT(%ebx),%edi
{$else FPC_PIC}
  leal  Errno,%edi
  movl  fpc_threadvar_relocate_proc,%ecx
{$endif FPC_PIC}
  testl %ecx,%ecx
  jne   .LThread
  movl  %eax,4(%edi)
  jmp   .LNoThread
.LThread:
  movl  %eax,%ebx
  movl  (%edi),%eax
  call  *%ecx
  movl  %ebx,(%eax)
.LNoThread:
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
{$ifdef FPC_PIC}
  call  fpc_geteipasebx
  addl  $_GLOBAL_OFFSET_TABLE_,%ebx
  movl  fpc_threadvar_relocate_proc@GOT(%ebx),%ecx
  movl  (%ecx),%ecx
  movl  Errno@GOT(%ebx),%edi
{$else FPC_PIC}
  leal  Errno,%edi
  movl  fpc_threadvar_relocate_proc,%ecx
{$endif FPC_PIC}
  testl %ecx,%ecx
  jne   .LThread
  movl  %eax,4(%edi)
  jmp   .LNoThread
.LThread:
  movl  %eax,%ebx
  movl (%edi),%eax
  call  *%ecx
  movl  %ebx,(%eax)
.LNoThread:
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
{$ifdef FPC_PIC}
  call  fpc_geteipasebx
  addl  $_GLOBAL_OFFSET_TABLE_,%ebx
  movl  fpc_threadvar_relocate_proc@GOT(%ebx),%ecx
  movl  (%ecx),%ecx
  movl  Errno@GOT(%ebx),%edi
{$else FPC_PIC}
  leal  Errno,%edi
  movl  fpc_threadvar_relocate_proc,%ecx
{$endif FPC_PIC}
  testl %ecx,%ecx
  jne   .LThread
  movl  %eax,4(%edi)
  jmp   .LNoThread
.LThread:
  movl  %eax,%ebx
  movl  (%edi),%eax
  call  *%ecx
  movl  %ebx,(%eax)
.LNoThread:
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
{$ifdef FPC_PIC}
  call  fpc_geteipasebx
  addl  $_GLOBAL_OFFSET_TABLE_,%ebx
  movl  fpc_threadvar_relocate_proc@GOT(%ebx),%ecx
  movl  (%ecx),%ecx
  movl  Errno@GOT(%ebx),%edi
{$else FPC_PIC}
  leal  Errno,%edi
  movl  fpc_threadvar_relocate_proc,%ecx
{$endif FPC_PIC}
  testl %ecx,%ecx
  jne   .LThread
  movl  %eax,4(%edi)
  jmp   .LNoThread
.LThread:
  movl  %eax,%ebx
  movl  (%edi),%eax
  call  *%ecx
  movl  %ebx,(%eax)
.LNoThread:
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
{$ifdef FPC_PIC}
  call  fpc_geteipasebx
  addl  $_GLOBAL_OFFSET_TABLE_,%ebx
  movl  fpc_threadvar_relocate_proc@GOT(%ebx),%ecx
  movl  (%ecx),%ecx
  movl  Errno@GOT(%ebx),%edi
{$else FPC_PIC}
  leal  Errno,%edi
  movl  fpc_threadvar_relocate_proc,%ecx
{$endif FPC_PIC}
  testl %ecx,%ecx
  jne   .LThread
  movl  %eax,4(%edi)
  jmp   .LNoThread
.LThread:
  movl  %eax,%ebx
  movl  (%edi),%eax
  call  *%ecx
  movl  %ebx,(%eax)
.LNoThread:
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
{$ifdef FPC_PIC}
  call  fpc_geteipasebx
  addl  $_GLOBAL_OFFSET_TABLE_,%ebx
  movl  fpc_threadvar_relocate_proc@GOT(%ebx),%ecx
  movl  (%ecx),%ecx
  movl  Errno@GOT(%ebx),%edi
{$else FPC_PIC}
  leal  Errno,%edi
  movl  fpc_threadvar_relocate_proc,%ecx
{$endif FPC_PIC}
  testl %ecx,%ecx
  jne   .LThread
  movl  %eax,4(%edi)
  jmp   .LNoThread
.LThread:
  movl  %eax,%ebx
  movl  (%edi),%eax
  call  *%ecx
  movl  %ebx,(%eax)
.LNoThread:
  movl  $-1,%eax
.LSyscOK:
end;
{$endif notsupported}

{No debugging for syslinux include !}
{$IFDEF SYS_LINUX}
  {$UNDEF SYSCALL_DEBUG}
{$ENDIF SYS_LINUX}
