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
  gmon_etext: pointer; external name '_etext';
  gmon_monstarted: longint = 0;

procedure gmon_monstartup; external name 'monstartup';
procedure gmon_mcleanup; external name '_mcleanup';

procedure libc_atexit; external name '__libc_atexit';
procedure libc_exit; external name '__libc_exit';
procedure libc_init; external name '__libc_init';
procedure libc_setfpucw; external name '__setfpucw';
procedure libc_start_main; external name '__libc_start_main';

procedure PASCALMAIN; external name 'PASCALMAIN';

{******************************************************************************
                       Process + profiling start/halt
 ******************************************************************************}
{$asmmode ATT}

procedure _FPC_proc_gprof_start; assembler; nostackframe; public name '_start';
asm
  { First locate the start of the environment variables }
  popl    %ecx
  movl    %esp,%ebx               { Points to the arguments }
  movl    %ecx,%eax
  incl    %eax
  shll    $2,%eax
  addl    %esp,%eax
  andl    $0xfffffff8,%esp        { Align stack }

  movl    %eax,operatingsystem_parameter_envp    { Move the environment pointer }
  movl    %ecx,operatingsystem_parameter_argc    { Move the argument counter    }
  movl    %ebx,operatingsystem_parameter_argv    { Move the argument pointer    }

  finit                           { initialize fpu }
  fwait
  fldcw   Default8087CW 

  pushl   $gmon_etext                 { Initialize gmon }
  pushl   $_FPC_proc_gprof_start
  call    gmon_monstartup
  addl    $8,%esp
  pushl   $gmon_mcleanup
  call    libc_atexit
  addl    $4,%esp

  { Save initial stackpointer }
  movl    %esp,initialstkptr

  xorl    %ebp,%ebp
  call    PASCALMAIN
end;

procedure _FPC_proc_gprof_haltproc; assembler; nostackframe; public name '_haltproc';
asm
.Lhaltproc:
 {$ifdef FPC_PIC}
  call    fpc_geteipasebx
  addl    $_GLOBAL_OFFSET_TABLE_,%ebx
  movl    ExitCode@GOT(%ebx),%ebx
 {$if sizeof(ExitCode)=2}
  movzwl  (%ebx),%ebx
 {$else}
  mov     (%ebx),%ebx
 {$endif}
{$else FPC_PIC}
 {$if sizeof(ExitCode)=2}
  movzwl  ExitCode,%ebx
 {$else}
  mov     ExitCode,%ebx
 {$endif}
{$endif FPC_PIC}

  pushl   %ebx
  call    libc_exit     { call libc exit, this will  write the gmon.out }
  movl    syscall_nr_exit_group,%eax
  popl    %ebx
  int     $0x80
  jmp     .Lhaltproc
end;

