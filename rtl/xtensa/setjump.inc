{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team.

    SetJmp and LongJmp implementation for exception handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$ifdef fpc_abi_call0}
function fpc_setjmp(var S : jmp_buf) : longint;assembler;[Public, alias : 'FPC_SETJMP']; compilerproc; nostackframe;
  asm
    s32i.n a0,S.a0
    s32i.n a1,S.a1
    s32i.n a8,S.a8
    s32i.n a12,S.a12
    s32i.n a13,S.a13
    s32i.n a14,S.a14
    s32i.n a15,S.a15
    movi.n a2,0
  end;


procedure fpc_longjmp(var S : jmp_buf;value : longint);assembler;[Public, alias : 'FPC_LONGJMP']; compilerproc; nostackframe;
  asm
    l32i.n a0,S.a0
    l32i.n a1,S.a1
    l32i.n a8,S.a8
    l32i.n a12,S.a12
    l32i.n a13,S.a13
    l32i.n a14,S.a14
    l32i.n a15,S.a15
    movi.n a2,1
    movnez a2,value,value
  end;
{$elseif defined(freertos) and defined(fpc_abi_windowed)}

function fpc_setjmp(var S : jmp_buf) : longint;assembler;[Public, alias : 'FPC_SETJMP']; compilerproc; nostackframe;
  asm
    j.l setjmp,a15
  end;


procedure fpc_longjmp(var S : jmp_buf;value : longint);assembler;[Public, alias : 'FPC_LONGJMP']; compilerproc; nostackframe;
  asm
    j.l longjmp,a15
  end;

{$else}

function fpc_setjmp(var S : jmp_buf) : longint;assembler;[Public, alias : 'FPC_SETJMP']; compilerproc; nostackframe;
  asm
    entry a1,16
    movi.n a2,0
  end;


procedure fpc_longjmp(var S : jmp_buf;value : longint);assembler;[Public, alias : 'FPC_LONGJMP']; compilerproc; nostackframe;
  asm
    entry a1,16
    movi.n a2,1
    movnez a2,value,value
  end;
{$endif}

