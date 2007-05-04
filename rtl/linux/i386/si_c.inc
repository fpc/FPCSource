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

var
  libc_environ: pchar; external name '__environ';
  libc_fpu_control: word; external name '__fpu_control';
  libc_init_proc: procedure; external name '_init';
  libc_fini_proc: procedure; external name '_fini';

procedure libc_atexit; external name '__libc_atexit';
procedure libc_exit; external name '__libc_exit';
procedure libc_init; external name '__libc_init';
procedure libc_setfpucw; external name '__setfpucw';
procedure libc_start_main; external name '__libc_start_main';

procedure PASCALMAIN; external name 'PASCALMAIN';

{******************************************************************************
                          C library start/halt
 ******************************************************************************}
{$asmmode ATT}

procedure _FPC_libc_start; assembler; nostackframe; public name '_start';
asm
  { First locate the start of the environment variables }
  popl    %ecx                    { Get argc in ecx }
  movl    %esp,%ebx               { Esp now points to the arguments }
  leal    4(%esp,%ecx,4),%eax     { The start of the environment is: esp+4*eax+8 }
  andl    $0xfffffff8,%esp        { Align stack }

  movl    %eax,operatingsystem_parameter_envp    { Move the environment pointer }
  movl    %ecx,operatingsystem_parameter_argc    { Move the argument counter    }
  movl    %ebx,operatingsystem_parameter_argv    { Move the argument pointer    }

  movl    %eax,libc_environ          { libc environ }

  pushl   %eax
  pushl   %ebx
  pushl   %ecx

  call    libc_init             { init libc }
  movzwl  libc_fpu_control,%eax
  pushl   %eax
  call    libc_setfpucw
  popl    %eax
  pushl   $libc_fini_proc
  call    libc_atexit
  popl    %eax
  call    libc_init_proc

  popl    %eax
  popl    %eax

  { Save initial stackpointer }
  movl    %esp,initialstkptr

  xorl    %ebp,%ebp
  call    PASCALMAIN              { start the program }
end;

procedure _FPC_libc_haltproc; assembler; nostackframe; public name '_haltproc';
asm
.Lhaltproc:
{$if sizeof(ExitCode)=2}
  movzwl  ExitCode,%ebx
{$else}
  mov     ExitCode,%ebx
{$endif}
  pushl   %ebx
  call    libc_exit
  xorl    %eax,%eax
  incl    %eax                    { eax=1, exit call }
  popl    %ebx
  int     $0x80
  jmp     .Lhaltproc
end;

