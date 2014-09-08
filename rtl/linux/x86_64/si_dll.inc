{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by Florian Klaempfl
    members of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY;without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 
 **********************************************************************}


procedure PASCALMAIN; external name 'PASCALMAIN';

procedure _FPC_shared_lib_start(argc:dword; argv,envp:pointer); cdecl; public name 'FPC_SHARED_LIB_START'; public name '_start';
begin
  operatingsystem_parameter_argc:=argc;
  operatingsystem_parameter_argv:=argv;
  operatingsystem_parameter_envp:=envp;
  InitialStkPtr:=get_frame;
  PASCALMAIN;
end;

{ this routine is only called when the halt() routine of the RTL embedded in
  the shared library is called }
procedure _FPC_shared_lib_haltproc(e:longint); cdecl; assembler; nostackframe; public name '_haltproc';
asm
        movl    e,%edi
        movl    $231,%eax                 { exit_group call }
        syscall
        jmp     _FPC_shared_lib_haltproc
end;

{ Define a symbol for the first piece of initialized data.  }
(*      .data
        .globl __data_start
__data_start:
        .long 0
        .weak data_start
        data_start = __data_start *)


(* We need this stuff to make gdb behave itself, otherwise
   gdb will chokes with SIGILL when trying to debug apps.

Makes ld choke:
        .section ".note.ABI-tag", "a"
        .align 4
        .long 1f - 0f
        .long 3f - 2f
        .long  1
0:      .asciz "GNU"
1:      .align 4
2:      .long 0
        .long 2,4,0
3:      .align 4
*)

