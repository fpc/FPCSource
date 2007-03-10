{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    The syscalls for the new RTL, moved to platform dependant dir.
    Old linux calling convention is still kept.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ASMMODE ATT}

Procedure fpc_geteipasebx;[external name 'fpc_geteipasebx'];

function FpSysCall(sysnr:TSysParam):TSysResult; assembler; register; [public,alias:'FPC_SYSCALL0'];
{ Var sysnr located in register eax }
asm
//      movl  sysnr,%eax
        int   $0x80
        cmpl  $-4095,%eax
        jb    .LSyscOK
        negl  %eax
        call  seterrno
        movl  $-1,%eax
  .LSyscOK:
end;

function FpSysCall(sysnr,param1 : TSysParam):TSysResult; assembler; register; [public,alias:'FPC_SYSCALL1'];
{ Var sysnr located in register eax
  Var param1 located in register edx }
asm
        movl  %ebx,%ecx
//      movl  sysnr,%eax
        movl  %edx,%ebx
        int   $0x80
        movl  %ecx,%ebx
        cmpl  $-4095,%eax
        jb    .LSyscOK
        negl  %eax
        call  seterrno
        movl  $-1,%eax
  .LSyscOK:
end;

function FpSysCall(sysnr,param1,param2 : TSysParam):TSysResult; assembler; register; [public,alias:'FPC_SYSCALL2'];
{ Var sysnr located in register eax
  Var param1 located in register edx
  Var param2 located in register ecx }
asm
        push  %ebx
//      movl  sysnr,%eax
        movl  %edx,%ebx
//      movl  param2,%ecx
        int   $0x80
        pop   %ebx
        cmpl  $-4095,%eax
        jb    .LSyscOK
        negl  %eax
        call  seterrno
        movl  $-1,%eax
  .LSyscOK:
end;

function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler; register; [public,alias:'FPC_SYSCALL3'];
{ Var sysnr located in register eax
  Var param1 located in register edx
  Var param2 located in register ecx
  Var param3 located at ebp+20 }
asm
        push  %ebx
//      movl  sysnr,%eax
        movl  %edx,%ebx
//      movl  param2,%ecx
        movl  param3,%edx
        int   $0x80
        pop   %ebx
        cmpl  $-4095,%eax
        jb    .LSyscOK
        negl  %eax
        call  seterrno
        movl  $-1,%eax
  .LSyscOK:
end;

function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler; register; [public,alias:'FPC_SYSCALL4'];
{ Var sysnr located in register eax
  Var param1 located in register edx
  Var param2 located in register ecx
  Var param3 located at ebp+20
  Var param4 located at ebp+16 }
asm
        push  %ebx
        push  %esi
//      movl  sysnr,%eax
        movl  %edx,%ebx
//      movl  param2,%ecx
        movl  param3,%edx
        movl  param4,%esi
        int   $0x80
        pop   %esi
        pop   %ebx
        cmpl  $-4095,%eax
        jb    .LSyscOK
        negl  %eax
        call  seterrno
        movl  $-1,%eax
  .LSyscOK:
end;

function FpSysCall(sysnr,param1,param2,param3,param4,param5 : TSysParam):TSysResult; assembler; register; [public,alias:'FPC_SYSCALL5'];
{ Var sysnr located in register eax
  Var param1 located in register edx
  Var param2 located in register ecx
  Var param3 located at ebp+20
  Var param4 located at ebp+16
  Var param5 located at ebp+12 }
asm
        push  %ebx
        push  %esi
        push  %edi
//      movl  sysnr,%eax
        movl  %edx,%ebx
//      movl  param2,%ecx
        movl  param3,%edx
        movl  param4,%esi
        movl  param5,%edi
        int   $0x80
        pop   %edi
        pop   %esi
        pop   %ebx
        cmpl  $-4095,%eax
        jb    .LSyscOK
        negl  %eax
        call  seterrno
        movl  $-1,%eax
  .LSyscOK:
end;

function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6: TSysParam):TSysResult; assembler; register; [public,alias:'FPC_SYSCALL6'];
{ Var sysnr located in register eax
  Var param1 located in register edx
  Var param2 located in register ecx
  Var param3 located at ebp+20
  Var param4 located at ebp+16
  Var param5 located at ebp+12
  Var param6 located at ebp+8 }
asm
        push  %ebx
        push  %esi
        push  %edi
        push  %ebp
//      movl  sysnr,%eax
        movl  %edx,%ebx
//      movl  param2,%ecx
        movl  param3,%edx
        movl  param4,%esi
        movl  param5,%edi
        movl  param6,%ebp
        int   $0x80
        pop   %ebp
        pop   %edi
        pop   %esi
        pop   %ebx
        cmpl  $-4095,%eax
        jb    .LSyscOK
        negl  %eax
        call  seterrno
        movl  $-1,%eax
  .LSyscOK:
end;

{No debugging for syslinux include !}
{$IFDEF SYS_LINUX}
  {$UNDEF SYSCALL_DEBUG}
{$ENDIF SYS_LINUX}
