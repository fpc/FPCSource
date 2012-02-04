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
  sw  $4,0($23)
  sw  $5,-4($23)
  sw  $6,-8($23)
  sw  $7,-12($23)
  sw  $8,-16($23)
  sw  $9,-20($23)
  sw  $10,-24($23)
  sw  $11,-28($23)
  sw  $12,-32($23)
  sw  $13,-36($23)
  sw  $14,-40($23)
  addiu  $23,$23,-44

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

  addiu  $23,$23,44
  lw  $4,0($23)
  lw  $5,-4($23)
  lw  $6,-8($23)
  lw  $7,-12($23)
  lw  $8,-16($23)
  lw  $9,-20($23)
  lw  $10,-24($23)
  lw  $11,-28($23)
  lw  $12,-32($23)
  lw  $13,-36($23)
  lw  $14,-40($23)

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
  sw  $4,0($23)
  sw  $5,-4($23)
  sw  $6,-8($23)
  sw  $7,-12($23)
  sw  $8,-16($23)
  sw  $9,-20($23)
  sw  $10,-24($23)
  sw  $11,-28($23)
  sw  $12,-32($23)
  sw  $13,-36($23)
  sw  $14,-40($23)
  addiu  $23,$23,-44

  move  $2,$4
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

  addiu  $23,$23,44
  lw  $4,0($23)
  lw  $5,-4($23)
  lw  $6,-8($23)
  lw  $7,-12($23)
  lw  $8,-16($23)
  lw  $9,-20($23)
  lw  $10,-24($23)
  lw  $11,-28($23)
  lw  $12,-32($23)
  lw  $13,-36($23)
  lw  $14,-40($23)

end;


function FpSysCall(sysnr,param1:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL1'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp: longint;
asm
  sw  $4,0($23)
  sw  $5,-4($23)
  sw  $6,-8($23)
  sw  $7,-12($23)
  sw  $8,-16($23)
  sw  $9,-20($23)
  sw  $10,-24($23)
  sw  $11,-28($23)
  sw  $12,-32($23)
  sw  $13,-36($23)
  sw  $14,-40($23)
  addiu  $23,$23,-44


  move  $2,$4
  move  $4,$5
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

  addiu  $23,$23,44
  lw  $4,0($23)
  lw  $5,-4($23)
  lw  $6,-8($23)
  lw  $7,-12($23)
  lw  $8,-16($23)
  lw  $9,-20($23)
  lw  $10,-24($23)
  lw  $11,-28($23)
  lw  $12,-32($23)
  lw  $13,-36($23)
  lw  $14,-40($23)

end;


function FpSysCall(sysnr,param1,param2:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL2'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp: longint;
asm
  sw  $4,0($23)
  sw  $5,-4($23)
  sw  $6,-8($23)
  sw  $7,-12($23)
  sw  $8,-16($23)
  sw  $9,-20($23)
  sw  $10,-24($23)
  sw  $11,-28($23)
  sw  $12,-32($23)
  sw  $13,-36($23)
  sw  $14,-40($23)
  addiu  $23,$23,-44


  move  $2,$4
  move  $4,$5
  move  $5,$6
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

  addiu  $23,$23,44
  lw  $4,0($23)
  lw  $5,-4($23)
  lw  $6,-8($23)
  lw  $7,-12($23)
  lw  $8,-16($23)
  lw  $9,-20($23)
  lw  $10,-24($23)
  lw  $11,-28($23)
  lw  $12,-32($23)
  lw  $13,-36($23)
  lw  $14,-40($23)

end;


function FpSysCall(sysnr,param1,param2,param3:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL3'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp: longint;
asm
  sw  $4,0($23)
  sw  $5,-4($23)
  sw  $6,-8($23)
  sw  $7,-12($23)
  sw  $8,-16($23)
  sw  $9,-20($23)
  sw  $10,-24($23)
  sw  $11,-28($23)
  sw  $12,-32($23)
  sw  $13,-36($23)
  sw  $14,-40($23)
  addiu  $23,$23,-44


  move  $2,$4
  move  $4,$5
  move  $5,$6
  move  $6,$7
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

  addiu  $23,$23,44
  lw  $4,0($23)
  lw  $5,-4($23)
  lw  $6,-8($23)
  lw  $7,-12($23)
  lw  $8,-16($23)
  lw  $9,-20($23)
  lw  $10,-24($23)
  lw  $11,-28($23)
  lw  $12,-32($23)
  lw  $13,-36($23)
  lw  $14,-40($23)

end;


function FpSysCall(sysnr,param1,param2,param3,param4:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL4'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp: longint;
asm
  sw  $4,0($23)
  sw  $5,-4($23)
  sw  $6,-8($23)
  sw  $7,-12($23)
  sw  $8,-16($23)
  sw  $9,-20($23)
  sw  $10,-24($23)
  sw  $11,-28($23)
  sw  $12,-32($23)
  sw  $13,-36($23)
  sw  $14,-40($23)
  addiu  $23,$23,-44


  move  $2,$4
  move  $4,$5
  move  $5,$6
  move  $6,$7
  move  $7,$8
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

  addiu  $23,$23,44
  lw  $4,0($23)
  lw  $5,-4($23)
  lw  $6,-8($23)
  lw  $7,-12($23)
  lw  $8,-16($23)
  lw  $9,-20($23)
  lw  $10,-24($23)
  lw  $11,-28($23)
  lw  $12,-32($23)
  lw  $13,-36($23)
  lw  $14,-40($23)

end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL5'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp: longint;
asm
  sw  $4,0($23)
  sw  $5,-4($23)
  sw  $6,-8($23)
  sw  $7,-12($23)
  sw  $8,-16($23)
  sw  $9,-20($23)
  sw  $10,-24($23)
  sw  $11,-28($23)
  sw  $12,-32($23)
  sw  $13,-36($23)
  sw  $14,-40($23)
  addiu  $23,$23,-44


  move  $2,$4
  move  $4,$5
  move  $5,$6
  move  $6,$7
  move  $7,$8
  move  $8,$9
  subu  $29,32
  sw    $9, 16($29)

  syscall
  nop
  addiu $29,32

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

  addiu  $23,$23,44
  lw  $4,0($23)
  lw  $5,-4($23)
  lw  $6,-8($23)
  lw  $7,-12($23)
  lw  $8,-16($23)
  lw  $9,-20($23)
  lw  $10,-24($23)
  lw  $11,-28($23)
  lw  $12,-32($23)
  lw  $13,-36($23)
  lw  $14,-40($23)

end;


function FpSysCall(sysnr,param1,param2,param3,param4,param5,param6:TSysParam):TSysResult; assembler;[public,alias:'FPC_SYSCALL6'];
{
  This function puts the registers in place, does the call, and then
  copies back the registers as they are after the SysCall.
}
var
  temp: longint;
asm
  sw  $4,0($23)
  sw  $5,-4($23)
  sw  $6,-8($23)
  sw  $7,-12($23)
  sw  $8,-16($23)
  sw  $9,-20($23)
  sw  $10,-24($23)
  sw  $11,-28($23)
  sw  $12,-32($23)
  sw  $13,-36($23)
  sw  $14,-40($23)
  addiu  $23,$23,-44


  move  $2,$4
  move  $4,$5
  move  $5,$6
  move  $6,$7
  move  $7,$8
  move  $8,$9
  lw  $9,0($fp)

  subu  $29,32
  sw    $8, 16($29)
  sw    $9, 20($29)
  syscall
  nop
  addiu $29,32


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

  addiu  $23,$23,44
  lw  $4,0($23)
  lw  $5,-4($23)
  lw  $6,-8($23)
  lw  $7,-12($23)
  lw  $8,-16($23)
  lw  $9,-20($23)
  lw  $10,-24($23)
  lw  $11,-28($23)
  lw  $12,-32($23)
  lw  $13,-36($23)
  lw  $14,-40($23)

end;
