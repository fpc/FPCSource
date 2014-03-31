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
  _etext: pointer; external name '_etext';

  fpc_ret,fpc_ret_rbp : pointer;

procedure libc_atexit; external name '__libc_atexit';
procedure libc_exit; external name '__libc_exit';
procedure libc_init; external name '__libc_init';
procedure libc_setfpucw; external name '__setfpucw';
procedure libc_start_main; external name '__libc_start_main';

procedure gmon_monstartup; external name 'monstartup';
procedure gmon_mcleanup; external name '_mcleanup';

procedure PASCALMAIN; external name 'PASCALMAIN';
procedure main_stub; forward;

procedure ini_dummy;
  begin
  end;

{******************************************************************************
                          C library start/halt
 ******************************************************************************}

procedure _FPC_libc_start; assembler; nostackframe; public name '_start';
  asm
    { Clear the frame pointer.  The ABI suggests this be done, to mark
     the outermost frame obviously.  }
    xorq %rbp, %rbp

    { Extract the arguments as encoded on the stack and set up
       the arguments for __libc_start_main (int (*main) (int, char **, char **),
    	   int argc, char *argv,
    	   void (*init) (void), void (*fini) (void),
    	   void (*rtld_fini) (void), void *stack_end).
       The arguments are passed via registers and on the stack:
    main:		%rdi
    argc:		%rsi
    argv:		%rdx
    init:		%rcx
    fini:		%r8
    rtld_fini:	%r9
    stack_end:      stack.  }

    movq %rdx, %r9          { Address of the shared library termination
                             function.  }
    popq %rsi               { Pop the argument count.  }
    movq %rsp, %rdx         { argv starts just at the current stack top.  }

    movq operatingsystem_parameter_argc@GOTPCREL(%rip),%rax
    movq %rsi,(%rax)
    movq operatingsystem_parameter_argv@GOTPCREL(%rip),%rax
    movq %rsp,(%rax)
    leaq 8(,%rsi,8),%rax
    addq %rsp,%rax
    movq operatingsystem_parameter_envp@GOTPCREL(%rip),%rcx
    movq %rax,(%rcx)

    { Align the stack to a 16 byte boundary to follow the ABI.  }
    andq  $0xfffffffffffffff0, %rsp

    pushq %rax              { Push garbage because we push 8 more bytes.  }

    { Provide the highest stack address to the user code (for stacks
     which grow downwards).  }
    pushq %rsp

    { Pass address of our own entry points to .fini and .init.  }
    movq $ini_dummy, %r8
    movq $ini_dummy, %rcx

    movq $main_stub, %rdi

    { Call the user's main function, and exit with its value.
     But let the libc call main.    }
    call libc_start_main@PLT

    hlt                     { Crash if somehow `exit' does return.  }

    { We need this stuff to make gdb behave itself, otherwise
      gdb will chokes with SIGILL when trying to debug apps.
    }
(*
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
*)
  end;

procedure main_stub; assembler; nostackframe;
  asm
    { save return address }
    popq    %rax

    // stack alignment
    pushq	%rax

    movq    %rax,fpc_ret(%rip)
    movq    %rbp,fpc_ret_rbp(%rip)
    pushq   %rax

    { Initialize gmon }
    movq    _etext@GOTPCREL(%rip),%rsi
    movq    _FPC_libc_start@GOTPCREL(%rip),%rdi
    call    gmon_monstartup@PLT

    movq    gmon_mcleanup@GOTPCREL(%rip),%rdi
    call    libc_atexit@PLT
    { Save initial stackpointer }
    movq    initialstkptr(%rip),%rax
    movq    %rsp,(%rax)

    { start the program }
    xorq    %rbp,%rbp
    call    PASCALMAIN@PLT
    hlt
  end;

procedure _FPC_libc_haltproc; assembler; nostackframe; public name '_haltproc';
  asm
    movq      ExitCode@GOTPCREL(%rip),%rax
    movzwq    (%rax),%rax { load and save exitcode }

    movq fpc_ret(%rip),%rdx         { return to libc }
    movq fpc_ret_rbp(%rip),%rbp
    pushq %rdx
  end;
