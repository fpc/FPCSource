{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Michael Van Canneyt, Peter Vreman,
    & Daniel Mantione, members of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{******************************************************************************
                          Process start/halt
 ******************************************************************************}

var
  dlexitproc : pointer;

function fpc_getgot : pointer; [external name 'FPC_GETGOT'];

procedure _FPC_proc_start; assembler; nostackframe; public name '_start';
  asm
    { first stack frame }
    mov	%g0, %fp
    { space for arguments }
    sub	%sp, 6*8, %sp
{$ifdef FPC_PIC}
    sethi %hi(_GLOBAL_OFFSET_TABLE_-8),%l7
    or %l7,%lo(_GLOBAL_OFFSET_TABLE_-4),%l7
    call FPC_GETGOT
    nop
{$endif FPC_PIC}
    { extract argc }
    ldx	[%sp+STACK_BIAS+22*8], %o2
    sethi %hi(operatingsystem_parameter_argc),%o1
    or	%o1,%lo(operatingsystem_parameter_argc),%o1
{$ifdef FPC_PIC}
    ldx	[%o1+%l7],%o1
{$endif FPC_PIC}
    st	%o2, [%o1]
    { extract argv }
    add	%sp, STACK_BIAS+23*8, %o0
    sethi %hi(operatingsystem_parameter_argv),%o1
    or	%o1,%lo(operatingsystem_parameter_argv),%o1
{$ifdef FPC_PIC}
    ldx [%o1+%l7], %o1
{$endif FPC_PIC}
    stx	%o0, [%o1]

    { envp=argv+(argc+1)*8 }
    inc     %o2
    sllx     %o2, 3, %o2
    add	%o2, %o0, %o2
    sethi %hi(operatingsystem_parameter_envp),%o1
    or	%o1,%lo(operatingsystem_parameter_envp),%o1
{$ifdef FPC_PIC}
    ldx	[%o1+%l7], %o1
{$endif FPC_PIC}
    stx	%o2, [%o1]

    { Save initial stackpointer }
    sethi %hi(initialstkptr),%o1
    or	%o1,%lo(initialstkptr),%o1
{$ifdef FPC_PIC}
    ldx	[%o1+%l7], %o1
{$endif FPC_PIC}
    stx	%sp, [%o1]

    { Call main program }
    call	PASCALMAIN
    nop

    { we shuld never return here }
    unimp
  end;


procedure _FPC_dynamic_proc_start; assembler; nostackframe; public name '_dynamic_start';
  asm
  end;


procedure _FPC_proc_haltproc(e:longint); cdecl; public name '_haltproc';
  begin
    if assigned(dlexitproc) then
      TProcedure(dlexitproc);
    { try to exit_group }
    while true do
      asm
        ld      e,%o0
        mov     188, %g1
        ta      0x6d
      end;
  end;
