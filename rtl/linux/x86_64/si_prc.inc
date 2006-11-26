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


   %rdx		Contains a function pointer to be registered with `atexit'.
		This is how the dynamic linker arranges to have DT_FINI
		functions called for shared libraries that have been loaded
		before this code runs.

   %rsp		The stack contains the arguments and environment:
		0(%rsp)			argc
		8(%rsp)			argv[0]
		...
		(8*argc)(%rsp)		NULL
		(8*(argc+1))(%rsp)	envp[0]
		...
					NULL
}

procedure PASCALMAIN; external name 'PASCALMAIN';

{******************************************************************************
                          Process start/halt
 ******************************************************************************}

procedure _FPC_proc_start; assembler; nostackframe; public name '_start';
  asm
    popq     %rsi                                  { Pop the argument count.  }
    movq     %rsi,operatingsystem_parameter_argc
    movq     %rsp,operatingsystem_parameter_argv   { argv starts just at the current stack top.  }
    leaq     8(,%rsi,8),%rax
    addq     %rsp,%rax
    movq     %rax,operatingsystem_parameter_envp
    andq     $0xfffffffffffffff0,%rsp                             { Align the stack to a 16 byte boundary to follow the ABI.  }

    { Save initial stackpointer }
    movq    %rsp,__stkptr

    xorq    %rbp, %rbp
    call    PASCALMAIN
  end;


procedure _FPC_proc_haltproc; assembler; nostackframe; public name '_haltproc';
  asm
  .Lhaltproc:
    movl    $231,%eax                             { exit_group call }
    movzwl    operatingsystem_result,%edi
    syscall
    jmp     .Lhaltproc

    { We need this stuff to make gdb behave itself, otherwise
      gdb will chokes with SIGILL when trying to debug apps.
    }
    .section ".note.ABI-tag", "a"
    .align 4
    .long 1f - 0f
    .long 3f - 2f
    .long  1
0:  .asciz "GNU"
1:  .align 4
2:  .long 0
    .long 2,4,0
3:  .align 4

    .section	.note.GNU-stack,"",@progbits
  end;

