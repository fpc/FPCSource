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

const
  BIAS = 2047;

function fpc_getgot : pointer; [external name 'FPC_GETGOT'];

{******************************************************************************
                          C library start/halt
 ******************************************************************************}

procedure _FPC_libc_start; assembler; nostackframe; public name '_start';
  asm
  end;

procedure _FPC_libc_haltproc(e: longint); cdecl; public name '_haltproc';
  begin
    libc_exit(e);
    { try to exit_group }
    while true do
      asm
        ld     e,%o0
        mov     188, %g1
        ta      0x6d
      end;
  end;

