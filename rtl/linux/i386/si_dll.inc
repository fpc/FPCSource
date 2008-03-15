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
{$goto on}
{
 Linux ELF startup code for Free Pascal


 Stack layout at program start:

         nil
         envn
         ....
         ....           ENVIRONMENT VARIABLES
         env1
         env0
         nil
         argn
         ....
         ....           COMMAND LINE OPTIONS
         arg1
         arg0
         argc <--- esp
}

procedure PASCALMAIN; external name 'PASCALMAIN';

{******************************************************************************
                        Shared library start/halt
 ******************************************************************************}
{$asmmode ATT}

procedure _FPC_shared_lib_start(argc : dword;argv,envp : pointer); cdecl; public name '_FPC_SHARED_LIB_START_LOCAL'; public name '_start';
begin
  { we've to discuss about the use of this ;) }
  asm
    { Save initial stackpointer }
    movl    %esp,initialstkptr
  end;

  operatingsystem_parameter_argc:=argc;    { Copy the argument count      }
  operatingsystem_parameter_argv:=argv;    { Copy the argument pointer    }
  operatingsystem_parameter_envp:=envp;    { Copy the environment pointer }

  IsLibrary:=true;

  PASCALMAIN;
end;

{$ifndef VER2_0}

{ this hack is needed so we can make the reference below to _FPC_shared_lib_start }
{ local in compiler/systems/t_linux.pas                                           }
procedure _FPC_SHARED_LIB_START_LOCAL(argc : dword;argv,envp : pointer); cdecl; external;

procedure initdummy; assembler; nostackframe;
label
  FPC_LIB_START;
asm
.init
  .align 16
  .globl FPC_LIB_START
//    .type FPC_LIB_START,@function
FPC_LIB_START:
{$ifdef FPC_PIC}
  jmp	_FPC_SHARED_LIB_START_LOCAL@PLT
{$else FPC_PIC}
  jmp	_FPC_SHARED_LIB_START_LOCAL
{$endif FPC_PIC}
.text
end;
{$endif VER_2_0}

procedure _FPC_shared_lib_haltproc; assembler; nostackframe; public name '_haltproc';
asm
{$ifdef FPC_PIC}
  call    fpc_geteipasebx
  addl    $_GLOBAL_OFFSET_TABLE_,%ebx
{$endif}
.Lhaltproc:
  xorl    %eax,%eax
  incl    %eax                    { eax=1, exit call }
{$ifdef FPC_PIC}
  pushl   %ebx
  movl    ExitCode@GOT(%ebx),%ebx
{$if sizeof(ExitCode)=2}
  movzwl  (%ebx),%ebx
{$else}
  mov     (%ebx),%ebx
{$endif}
{$endif}
  int     $0x80
  jmp     .Lhaltproc
  popl    %ebx
end;

