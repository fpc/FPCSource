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
  libc21_fpc_ret, libc21_fpc_ret_ebx, libc21_fpc_ret_ebp: ptrint; { return address to libc }

procedure libc_exit; external name '__libc_exit';
procedure libc_init; external name '__libc_init';
procedure libc_setfpucw; external name '__setfpucw';
procedure libc_start_main; external name '__libc_start_main';

procedure PASCALMAIN; external name 'PASCALMAIN';

{******************************************************************************
                         glibc 2.1 library start/halt
 ******************************************************************************}
{$asmmode ATT}

procedure _FPC_libc21_start; assembler; nostackframe; public name '_start';
asm
  { First locate the start of the environment variables }

  popl    %esi
  movl    %eax,%edi

  movl    %esp,%ebx               { Points to the arguments }
  movl    %esi,%eax
  incl    %eax
  shll    $2,%eax
  addl    %esp,%eax
  andl    $0xfffffff8,%esp        { Align stack }

  movl    %eax,operatingsystem_parameter_envp    { Move the environment pointer }
  movl    %esi,operatingsystem_parameter_argc    { Move the argument counter    }
  movl    %ebx,operatingsystem_parameter_argv    { Move the argument pointer    }

  xorl    %ebp,%ebp
  pushl   %edi
  pushl   %esp
  pushl   %edx
  pushl   $.Lfini_dummy
  pushl   $.Linit_dummy
  pushl   %ebx
  pushl   %esi
  pushl   $.Lmain
  call    libc_start_main
.Linit_dummy:
.Lfini_dummy:
  ret

{ fake main routine which will be run from libc }
.Lmain:
  { save return address }
  popl    %eax
  movl    %eax,libc21_fpc_ret
  movl    %ebx,libc21_fpc_ret_ebx
  movl    %ebp,libc21_fpc_ret_ebp
  pushl   %eax

  { Save initial stackpointer }
  movl    %esp,initialstkptr

  { start the program }
  xorl    %ebp,%ebp
  call    PASCALMAIN
  hlt
end;

procedure _FPC_libc21_haltproc; assembler; nostackframe; public name '_haltproc';
asm
{$if sizeof(ExitCode)=2}
  movzwl  ExitCode,%eax
{$else}
  mov     ExitCode,%eax
{$endif}

  movl    libc21_fpc_ret,%edx         { return to libc }
  movl    libc21_fpc_ret_ebp,%ebp
  movl    libc21_fpc_ret_ebx,%ebx
  push    %edx
  ret
end;

