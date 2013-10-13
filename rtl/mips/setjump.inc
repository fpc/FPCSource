{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Jonas Maebe and David Zhang 
    
    SetJmp and LongJmp implementation for exception handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure fpc_longjmp(var s : jmp_buf;value:longint);assembler;nostackframe;[Public,alias:'FPC_LONGJMP'];compilerproc;
  asm
    lw    $31,jmp_buf.ra($a0)
    lw    $sp,jmp_buf.sp($a0)
    lw    $s0,jmp_buf.s0($a0)
    lw    $s1,jmp_buf.s1($a0)
    lw    $s2,jmp_buf.s2($a0)
    lw    $s3,jmp_buf.s3($a0)
    lw    $s4,jmp_buf.s4($a0)
    lw    $s5,jmp_buf.s5($a0)
    lw    $s6,jmp_buf.s6($a0)
    lw    $s7,jmp_buf.s7($a0)
    lw    $fp,jmp_buf.fp($a0)
    lw    $gp,jmp_buf.gp($a0)
    lw    $v0,jmp_buf._fcsr($a0)
    ctc1  $v0,$31
    lwc1  $f20,jmp_buf.f20($a0)
    lwc1  $f21,jmp_buf.f21($a0)
    lwc1  $f22,jmp_buf.f22($a0)
    lwc1  $f23,jmp_buf.f23($a0)
    lwc1  $f24,jmp_buf.f24($a0)
    lwc1  $f25,jmp_buf.f25($a0)
    lwc1  $f26,jmp_buf.f26($a0)
    lwc1  $f27,jmp_buf.f27($a0)
    lwc1  $f28,jmp_buf.f28($a0)
    lwc1  $f29,jmp_buf.f29($a0)
    lwc1  $f30,jmp_buf.f30($a0)
    lwc1  $f31,jmp_buf.f31($a0)
    
    // Put value into register $2 as in setjmp
    move  $v0,$a1
    bne   $a1,$zero,.L1
    nop
    addiu $v0,$zero,1
.L1:
  end;


function fpc_setjmp(var S:jmp_buf):longint;assembler;nostackframe;[Public,alias:'FPC_SETJMP'];compilerproc;
  asm
    sw    $31,jmp_buf.ra($a0)
    sw    $sp,jmp_buf.sp($a0)
    sw    $s0,jmp_buf.s0($a0)
    sw    $s1,jmp_buf.s1($a0)
    sw    $s2,jmp_buf.s2($a0)
    sw    $s3,jmp_buf.s3($a0)
    sw    $s4,jmp_buf.s4($a0)
    sw    $s5,jmp_buf.s5($a0)
    sw    $s6,jmp_buf.s6($a0)
    sw    $s7,jmp_buf.s7($a0)
    sw    $fp,jmp_buf.fp($a0)
    sw    $gp,jmp_buf.gp($a0)
    cfc1  $v0,$31
    sw    $v0,jmp_buf._fcsr($a0)
    swc1  $f20,jmp_buf.f20($a0)
    swc1  $f21,jmp_buf.f21($a0)
    swc1  $f22,jmp_buf.f22($a0)
    swc1  $f23,jmp_buf.f23($a0)
    swc1  $f24,jmp_buf.f24($a0)
    swc1  $f25,jmp_buf.f25($a0)
    swc1  $f26,jmp_buf.f26($a0)
    swc1  $f27,jmp_buf.f27($a0)
    swc1  $f28,jmp_buf.f28($a0)
    swc1  $f29,jmp_buf.f29($a0)
    swc1  $f30,jmp_buf.f30($a0)
    swc1  $f31,jmp_buf.f31($a0)
    move  $v0,$zero
  end;
