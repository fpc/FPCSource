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

procedure _FPC_shared_lib_start; assembler; nostackframe; public name '_start';
asm
  pushl   %ebp
  movl    %esp,%ebp

{$ifdef FPC_PIC}
  call    fpc_geteipasebx
  addl    $_GLOBAL_OFFSET_TABLE_,%ebx
{$endif}

  movl    8(%ebp),%eax
  movl    12(%ebp),%ecx
  movl    16(%ebp),%edx

  movl    %eax,operatingsystem_parameter_argc    { Copy the argument count      }
  movl    %ecx,operatingsystem_parameter_argv    { Copy the argument pointer    }
  movl    %edx,operatingsystem_parameter_envp    { Copy the environment pointer }

{$ifdef FPC_PIC}
  movl    ISLIBRARY@GOT(%ebx),%eax
  movb    $1,(%eax)
{$else}
  movb    $1,ISLIBRARY
{$endif}  

  { Save initial stackpointer }
  movl    %esp,initialstkptr

  call    PASCALMAIN

  leave
  ret
end;

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
  movzwl  (%ebx),%ebx
{$endif}  
  int     $0x80
  jmp     .Lhaltproc
  popl    %ebx
end;

