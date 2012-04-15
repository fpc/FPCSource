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
    lw $31,0($4)    // PC
    lw $sp,4($4)    // SP
    lw $16,8($4)    // S0,$16
    lw $17,12($4)   // S1,$16
    lw $18,16($4)   // S2,$16
    lw $19,20($4)   // S3,$16
    lw $20,24($4)   // S4,$16
    lw $21,28($4)   // S5,$16
    lw $22,32($4)   // S6,$16
    lw $23,36($4)   // S7,$16
    lw $fp,40($4)   // FP
    lw $gp,44($4)   // GP

    j $31
    nop
  end;


function fpc_setjmp(var S:jmp_buf):longint;assembler;nostackframe;[Public,alias:'FPC_SETJMP'];compilerproc;
  asm
    sw $31,0($4)        // PC
    sw $sp,4($4)        // SP
    sw $16,8($4)        // S0,$16
    sw $17,12($4)       // S1,$16
    sw $18,16($4)       // S2,$16
    sw $19,20($4)       // S3,$16
    sw $20,24($4)       // S4,$16
    sw $21,28($4)       // S5,$16
    sw $22,32($4)       // S6,$16
    sw $23,36($4)       // S7,$16
    sw $fp,40($4)       // FP
    sw $gp,44($4)       // GP

    move $2,$0
  end;
