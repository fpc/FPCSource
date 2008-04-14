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
                          Process start/halt
 ******************************************************************************}
{$asmmode ATT}

var
  dlexitproc: pointer;

procedure _FPC_proc_start; assembler; nostackframe; public name '_start';
asm
  { First locate the start of the environment variables }
  popl    %ecx                    { Get argc in ecx }
  movl    %esp,%ebx               { Esp now points to the arguments }
  leal    4(%esp,%ecx,4),%eax     { The start of the environment is: esp+4*eax+4 }
  andl    $0xfffffff8,%esp        { Align stack }

  movl    %eax,operatingsystem_parameter_envp
  movl    %ecx,operatingsystem_parameter_argc
  movl    %ebx,operatingsystem_parameter_argv

  movl    %edx, dlexitproc

  fninit                           { initialize fpu }
  fwait
  fldcw   Default8087CW

  { Initialize gs for thread local storage }
  // movw    %ds,%ax 
  // movw    %ax,%gs

  { Save initial stackpointer }
  movl    %esp,initialstkptr

  xorl    %ebp,%ebp
  call    PASCALMAIN
end;

procedure _FPC_proc_haltproc; assembler; nostackframe; public name '_haltproc';
asm
.Lhaltproc:
  movl    dlexitproc,%eax
  testl   %eax,%eax
  je      .Lnodlexitproc
  call    *%eax
.Lnodlexitproc:
  movl    syscall_nr_exit_group,%eax
{$if sizeof(ExitCode)=2}  
  movzwl  ExitCode,%ebx
{$else}
  mov     ExitCode,%ebx
{$endif}
  int     $0x80
  movl    syscall_nr_exit,%eax
{$if sizeof(ExitCode)=2}  
  movzwl  ExitCode,%ebx
{$else}
  mov     ExitCode,%ebx
{$endif}
  int     $0x80
  jmp     .Lhaltproc
end;

