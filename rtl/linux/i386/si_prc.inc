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

function fpc_geteipasebx : pointer; compilerproc; nostackframe; assembler;
asm  
  movl (%esp),%ebx  
  ret  
end;

{******************************************************************************
                          Process start/halt
 ******************************************************************************}
{$asmmode att}

var
  dlexitproc: pointer;

procedure _FPC_proc_start; assembler; nostackframe; public name '_start';
asm
  { First locate the start of the environment variables }

  popl    %ecx                    { Get argc in ecx }

  movl    %esp,%ebx               { Esp now points to the arguments }
  leal    4(%esp,%ecx,4),%eax     { The start of the environment is: esp+4*eax+4 }
  andl    $0xfffffff8,%esp        { Align stack }

  {$ifdef FPC_PIC}
        pushl %ebx
        pushl %ecx
	
	call fpc_geteipasebx
        addl  $_GLOBAL_OFFSET_TABLE_,%ebx

	movl  dlexitproc@GOT(%ebx),%ecx
	movl  %edx,(%ecx)

	movl  operatingsystem_parameter_envp@GOT(%ebx),%ecx
	movl  %eax,(%ecx)

	movl  operatingsystem_parameter_argc@GOT(%ebx),%edx
	popl  %ecx
	movl  %ecx,(%edx)

	movl  operatingsystem_parameter_argv@GOT(%ebx),%edx
	popl  %ebx
	movl  %ebx,(%edx)
  {$else FPC_PIC}
  	movl  %edx, dlexitproc
  	movl  %eax,operatingsystem_parameter_envp
  	movl  %ecx,operatingsystem_parameter_argc
  	movl  %ebx,operatingsystem_parameter_argv
  {$endif FPC_PIC}

  { Initialize FPU }
  call SysResetFPU

  { Save initial stackpointer }
  {$ifdef FPC_PIC}
        pushl %ebx
        call  fpc_geteipasebx
        addl  $_GLOBAL_OFFSET_TABLE_,%ebx
	movl  initialstkptr@GOT(%ebx),%ebx
  	movl  %esp,(%ebx)
	popl  %ebx
  {$else FPC_PIC}
  	movl    %esp,initialstkptr
  {$endif FPC_PIC}

  xorl    %ebp,%ebp
  call    PASCALMAIN
end;

procedure _FPC_proc_haltproc; assembler; nostackframe; public name '_haltproc';
asm

.Lhaltproc:
  {$ifdef FPC_PIC}
        call  fpc_geteipasebx
        addl  $_GLOBAL_OFFSET_TABLE_,%ebx
	movl  dlexitproc@GOT(%ebx),%eax
	movl  (%eax),%eax
  {$else FPC_PIC}
  	movl    dlexitproc,%eax
  {$endif FPC_PIC}
  testl   %eax,%eax
  je      .Lnodlexitproc
  call    *%eax
.Lnodlexitproc:
  movl    syscall_nr_exit_group,%eax

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

  int     $0x80
  movl    syscall_nr_exit,%eax

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

  int     $0x80
  jmp     .Lhaltproc
end;

