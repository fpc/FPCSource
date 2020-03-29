{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by Free Pascal development team

    This file implements parts of the startup code for OpenBSD
    shared object (.so) libraries.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$asmmode gas}

{$ifdef VER3_0}
procedure _init; cdecl; external name '_init';
{$else VER3_0}
procedure _init; cdecl; weakexternal name '_init';
{$endif VER3_0}

procedure _FPC_shared_lib_start; cdecl; public name 'FPC_LIB_START';
  begin
    { todo: figure out if there's any way to obtain these in OpenBSD shared libraries }
    environ:=nil;
    operatingsystem_parameter_envp:=nil;
    operatingsystem_parameter_argc:=0;
    operatingsystem_parameter_argv:=nil;
    if Assigned(@_init) then
      _init;
    PascalMain;
  end;

{ this routine is only called when the halt() routine of the RTL embedded in
  the shared library is called }
procedure _FPC_shared_lib_haltproc; cdecl; assembler; nostackframe; public name '_haltproc';
  asm
    movq    $1,%rax
    movl    operatingsystem_result(%rip),%edi
    syscall
    jmp     _FPC_shared_lib_haltproc
  end;
