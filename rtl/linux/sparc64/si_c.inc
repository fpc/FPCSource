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

var
  libc_environ: pchar; external name '__environ';
  libc_fpu_control: word; external name '__fpu_control';
  libc_init_proc: procedure; external name '_init';
  libc_fini_proc: procedure; external name '_fini';

procedure libc_atexit; external name '__libc_atexit';
procedure libc_exit(ec : longint); external name '__libc_exit';
procedure libc_init; external name '__libc_init';
procedure libc_setfpucw; external name '__setfpucw';
procedure libc_start_main; external name '__libc_start_main';

function fpc_getgot : pointer; [external name 'FPC_GETGOT'];

{******************************************************************************
                          C library start/halt
 ******************************************************************************}

procedure _FPC_libc_start; assembler; nostackframe; public name '_start';
  asm
    { first stack frame }
    mov	%g0, %fp
    { space for arguments }
    sub	%sp, 6*8, %sp
{$ifdef FPC_PIC}
    call FPC_GETGOT
    nop
{$endif FPC_PIC}
    { extract argc }
    ldx	[%sp+STACK_BIAS+22*8], %o2
{$ifdef FPC_PIC}
{$else FPC_PIC}
    sethi	%hi(operatingsystem_parameter_argc),%o1
    or	%o1,%lo(operatingsystem_parameter_argc),%o1
{$endif FPC_PIC}
    st	%o2, [%o1]
    { extract argv }
{$ifdef FPC_PIC}
{$else FPC_PIC}
    add	%sp, STACK_BIAS+23*8, %o0
    sethi	%hi(operatingsystem_parameter_argv),%o1
    or	%o1,%lo(operatingsystem_parameter_argv),%o1
{$endif FPC_PIC}
    stx	%o0, [%o1]

    { envp=argv+(argc+1)*8 }
    inc     %o2
    sllx     %o2, 3, %o2
    add	%o2, %o0, %o2
{$ifdef FPC_PIC}
{$else FPC_PIC}
    sethi	%hi(operatingsystem_parameter_envp),%o1
    or	%o1,%lo(operatingsystem_parameter_envp),%o1
{$endif FPC_PIC}
    stx	%o2, [%o1]

    { Save initial stackpointer }
{$ifdef FPC_PIC}
{$else FPC_PIC}
    sethi	%hi(initialstkptr),%o1
    or	%o1,%lo(initialstkptr),%o1
{$endif FPC_PIC}
    stx	%sp, [%o1]

    { prepare parameters to call __libc_start_main }
    ldx	[%sp+STACK_BIAS+22*8], %o1
    add	%sp, STACK_BIAS+23*8, %o0

{$ifdef FPC_PIC}
{$else FPC_PIC}
    sethi   %hi(PASCALMAIN), %o0
    or      %o0, %lo(PASCALMAIN), %o0
{$endif FPC_PIC}

{$ifdef FPC_PIC}
{$else FPC_PIC}
    sethi   %hi(libc_init_proc), %o3
    or      %o3, %lo(libc_init_proc), %o3
{$endif FPC_PIC}

{$ifdef FPC_PIC}
{$else FPC_PIC}
    sethi   %hi(libc_fini_proc), %o4
    or      %o4, %lo(libc_fini_proc), %o4
{$endif FPC_PIC}

    { shared library termination function }
    mov     %g1, %o5

    call    libc_start_main
    nop

    { we shuld never return here }
    unimp
  end;


procedure _FPC_libc_haltproc(e: longint); cdecl; public name '_haltproc';
  begin
    { try to exit_group }
    while true do
      asm
        ld     e,%o0
        mov     188, %g1
        ta      0x6d
      end;
  end;

