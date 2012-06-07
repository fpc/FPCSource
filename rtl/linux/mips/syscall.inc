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
var
  temp: longint;
asm
  li  $2,4002
  syscall
  nop
  beq $7,$0,.LDone
  nop
  lui   $8,%hi(fpc_threadvar_relocate_proc)
  addiu   $8,%lo(fpc_threadvar_relocate_proc)
  lw   $8,0($8)
  bne  $8,$0,.LThreaded
  nop
  lui   $4,%hi(Errno+4)
  addiu   $4,%lo(Errno+4)
  sw    $2,0($4)
  b     .LFailed
  nop
.LThreaded:
  sw   $2,temp
  lui   $4,%hi(errno)
  addiu   $4,$4,%lo(errno)
  jal   $8
  nop
  lw  $8,-4($fp)
  sw  $8,0($2)
.LFailed:
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
var
  temp: longint;
asm
  move  $v0,$a0
  syscall
  nop
  beq $7,$0,.LDone
  nop
  lui   $8,%hi(fpc_threadvar_relocate_proc)
  addiu   $8,%lo(fpc_threadvar_relocate_proc)
  lw   $8,0($8)
  bne  $8,$0,.LThreaded
  nop
  lui   $4,%hi(Errno+4)
  addiu   $4,%lo(Errno+4)
  sw    $2,0($4)
  b     .LFailed
  nop
.LThreaded:
  sw   $2,temp
  lui   $4,%hi(errno)
  addiu   $4,$4,%lo(errno)
  jal   $8
  nop
  lw  $8,temp
  sw  $8,0($2)
.LFailed:
  li    $2,-1
.LDone:
end;


function FpSysCall(sysnr,param1:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL1'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp: longint;
asm
  move  $v0,$a0
  move  $a0,$a1
  syscall
  nop
  beq $7,$0,.LDone
  nop
  lui   $8,%hi(fpc_threadvar_relocate_proc)
  addiu   $8,%lo(fpc_threadvar_relocate_proc)
  lw   $8,0($8)
  bne  $8,$0,.LThreaded
  nop
  lui   $4,%hi(Errno+4)
  addiu   $4,%lo(Errno+4)
  sw    $2,0($4)
  b     .LFailed
  nop
.LThreaded:
  sw   $2,temp
  lui   $4,%hi(errno)
  addiu   $4,$4,%lo(errno)
  jal   $8
  nop
  lw  $8,temp
  sw  $8,0($2)
.LFailed:
  li    $2,-1
.LDone:
end;


function FpSysCall(sysnr,param1,param2:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL2'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp: longint;
asm
  move  $v0,$a0
  move  $a0,$a1
  move  $a1,$a2
  syscall
  nop
  beq $7,$0,.LDone
  nop
  lui   $8,%hi(fpc_threadvar_relocate_proc)
  addiu   $8,%lo(fpc_threadvar_relocate_proc)
  lw   $8,0($8)
  bne  $8,$0,.LThreaded
  nop
  lui   $4,%hi(Errno+4)
  addiu   $4,%lo(Errno+4)
  sw    $2,0($4)
  b     .LFailed
  nop
.LThreaded:
  sw   $2,temp
  lui   $4,%hi(errno)
  addiu   $4,$4,%lo(errno)
  jal   $8
  nop
  lw  $8,temp
  sw  $8,0($2)
.LFailed:
  li    $2,-1
.LDone:
end;


function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL3'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp: longint;
asm
  move  $v0,$a0
  move  $a0,$a1
  move  $a1,$a2
  move  $a2,$a3
  syscall
  nop
  beq $7,$0,.LDone
  nop
  lui   $8,%hi(fpc_threadvar_relocate_proc)
  addiu   $8,%lo(fpc_threadvar_relocate_proc)
  lw   $8,0($8)
  bne  $8,$0,.LThreaded
  nop
  lui   $4,%hi(Errno+4)
  addiu   $4,%lo(Errno+4)
  sw    $2,0($4)
  b     .LFailed
  nop
.LThreaded:
  sw   $2,temp
  lui   $4,%hi(errno)
  addiu   $4,$4,%lo(errno)
  jal   $8
  nop
  lw  $8,temp
  sw  $8,0($2)
.LFailed:
  li    $2,-1
.LDone:
end;


function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL4'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp: longint;
asm
  move  $v0,$a0
  move  $a0,$a1
  move  $a1,$a2
  move  $a2,$a3
  //lw    $a3,16($sp)
  move  $a3,$8 //register passing is wrong, hack for now
  syscall
  nop
  beq $7,$0,.LDone
  nop
  lui   $8,%hi(fpc_threadvar_relocate_proc)
  addiu   $8,%lo(fpc_threadvar_relocate_proc)
  lw   $8,0($8)
  bne  $8,$0,.LThreaded
  nop
  lui   $4,%hi(Errno+4)
  addiu   $4,%lo(Errno+4)
  sw    $2,0($4)
  b     .LFailed
  nop
.LThreaded:
  sw   $2,temp
  lui   $4,%hi(errno)
  addiu   $4,$4,%lo(errno)
  jal   $8
  nop
  lw  $8,temp
  sw  $8,0($2)
.LFailed:
  li    $2,-1
.LDone:
end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL5'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp: longint;
asm
  move  $v0,$a0
  move  $a0,$a1
  move  $a1,$a2
  move  $a2,$a3
  move  $a3, $8

  sw    $9, 16($sp)

  syscall
  nop
  beq $7,$0,.LDone
  nop
  lui   $8,%hi(fpc_threadvar_relocate_proc)
  addiu   $8,%lo(fpc_threadvar_relocate_proc)
  lw   $8,0($8)
  bne  $8,$0,.LThreaded
  nop
  lui   $4,%hi(Errno+4)
  addiu   $4,%lo(Errno+4)
  sw    $2,0($4)
  b     .LFailed
  nop
.LThreaded:
  sw   $2,temp
  lui   $4,%hi(errno)
  addiu   $4,$4,%lo(errno)
  jal   $8
  nop
  lw  $8,temp
  sw  $8,0($2)
.LFailed:
  li    $2,-1
.LDone:

end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL6'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp: longint;
asm
{ All params are already stored on stack at that point }
  lw  $v0,sysnr
  lw  $a0,param1
  lw  $a1,param2
  lw  $a2,param3
  lw  $a3,param4

  lw   $at,param5
  sw   $at,16($sp)
  lw    $at,param6
  sw    $at,20($sp)
  syscall
  nop
  beq $7,$0,.LDone
  nop
  lui   $8,%hi(fpc_threadvar_relocate_proc)
  addiu   $8,%lo(fpc_threadvar_relocate_proc)
  lw   $8,0($8)
  bne  $8,$0,.LThreaded
  nop
  lui   $4,%hi(Errno+4)
  addiu   $4,%lo(Errno+4)
  sw    $2,0($4)
  b     .LFailed
  nop
.LThreaded:
  sw   $2,temp
  lui   $4,%hi(errno)
  addiu   $4,$4,%lo(errno)
  jal   $8
  nop
  lw  $8,temp
  sw  $8,0($2)
.LFailed:
  li    $2,-1
.LDone:
end;
