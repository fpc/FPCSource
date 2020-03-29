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
  gmon_etext: pointer; external name '_etext';
  gmon_monstarted: longint = 0;

procedure gmon_monstartup; external name 'monstartup';
procedure gmon_mcleanup; external name '_mcleanup';

procedure libc_atexit; external name '__libc_atexit';
procedure libc_exit(ec : longint); external name '__libc_exit';
procedure libc_init; external name '__libc_init';
procedure libc_setfpucw; external name '__setfpucw';
procedure libc_start_main; external name '__libc_start_main';

{******************************************************************************
                       Process + profiling start/halt
 ******************************************************************************}

procedure _FPC_proc_gprof_start; assembler; nostackframe; public name '_start';
  asm
  end;

