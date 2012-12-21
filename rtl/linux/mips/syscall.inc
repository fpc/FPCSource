{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2009 by Michael Van Canneyt and David Zhang

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


{$define FPC_SYSTEM_HAS_FPFORK}
{
  behaviour of result of fork on sparc/linux is different than on other
  linux flavours
}
function Fpfork : pid_t;  [public, alias : 'FPC_SYSC_FORK'];assembler;
asm
  li  $2,4002
  syscall
  nop
  beq $7,$0,.LDone
  nop
  move  $a0,$2
  jal   SetErrno
  nop
  li    $2,-1
.LDone:
end;


{*****************************************************************************
                     --- Main:The System Call Self ---
*****************************************************************************}

function FpSysCall(sysnr:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL0'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  move  $v0,$a0
  syscall
  nop
  beq $7,$0,.LDone
  nop
  move  $a0,$2
  jal   SetErrno
  nop
  li    $2,-1
.LDone:
end;


function FpSysCall(sysnr,param1:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL1'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  move  $v0,$a0
  move  $a0,$a1
  syscall
  nop
  beq $7,$0,.LDone
  nop
  move  $a0,$2
  jal   SetErrno
  nop
  li    $2,-1
.LDone:
end;


function FpSysCall(sysnr,param1,param2:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL2'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  move  $v0,$a0
  move  $a0,$a1
  move  $a1,$a2
  syscall
  nop
  beq $7,$0,.LDone
  nop
  move  $a0,$2
  jal   SetErrno
  nop
  li    $2,-1
.LDone:
end;


function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL3'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  move  $v0,$a0
  move  $a0,$a1
  move  $a1,$a2
  move  $a2,$a3
  syscall
  nop
  beq $7,$0,.LDone
  nop
  move  $a0,$2
  jal   SetErrno
  nop
  li    $2,-1
.LDone:
end;


function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL4'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  move  $v0,$a0
  move  $a0,$a1
  move  $a1,$a2
  move  $a2,$a3
  lw    $a3,param4
  syscall
  nop
  beq $7,$0,.LDone
  nop
  move  $a0,$2
  jal   SetErrno
  nop
  li    $2,-1
.LDone:
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL5'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  move  $v0,$a0
  move  $a0,$a1
  move  $a1,$a2
  move  $a2,$a3
  lw	$a3,param4
  lw    $t0,param5
  sw    $t0,16($sp)

  syscall
  nop
  beq $7,$0,.LDone
  nop
  move  $a0,$2
  jal   SetErrno
  nop
  li    $2,-1
.LDone:

end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL6'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
asm
  move  $v0,$a0
  move  $a0,$a1
  move  $a1,$a2
  move  $a2,$a3
  lw	$a3,param4
  lw    $t0,param5
  sw    $t0,16($sp)
  lw    $t0,param6
  sw    $t0,20($sp)
  syscall
  nop
  beq $7,$0,.LDone
  nop
  move  $a0,$2
  jal   SetErrno
  nop
  li    $2,-1
.LDone:
end;
