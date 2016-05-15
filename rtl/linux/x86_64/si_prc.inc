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

{$asmmode gas}

{$L abitag.o}

procedure PASCALMAIN; external name 'PASCALMAIN';

{******************************************************************************
                          Process start/halt
 ******************************************************************************}

var
  dlexitproc: pointer;

procedure _FPC_proc_start; assembler; nostackframe; public name '_start';
  asm
    popq     %rsi                                  { Pop the argument count.  }
    movq     operatingsystem_parameter_argc@GOTPCREL(%rip),%rax
    movq     %rsi,(%rax)
    movq     operatingsystem_parameter_argv@GOTPCREL(%rip),%rax
    movq     %rsp,(%rax)                           { argv starts just at the current stack top.  }
    leaq     8(,%rsi,8),%rax
    addq     %rsp,%rax
    movq     operatingsystem_parameter_envp@GOTPCREL(%rip),%rsi
    movq     %rax,(%rsi)
    andq     $0xfffffffffffffff0,%rsp                             { Align the stack to a 16 byte boundary to follow the ABI.  }

    { Save initial stackpointer }
    movq    initialstkptr@GOTPCREL(%rip),%rax
    movq    %rsp,(%rax)

    xorq    %rbp, %rbp
    call    PASCALMAIN
  end;

procedure _FPC_dynamic_proc_start; assembler; nostackframe; public name '_dynamic_start';
  asm
    movq  dlexitproc@GOTPCREL(%rip),%rax
    movq %rdx,(%rax)
    jmp _FPC_proc_start
  end;

procedure _FPC_proc_haltproc(e: longint); assembler; public name '_haltproc';
  var
    code: longint;
  asm
    movl    %edi,code
    movq    dlexitproc@GOTPCREL(%rip),%rax
    movq    (%rax),%rdx
    testq   %rdx,%rdx
    jz      .Lhaltproc

    call *%rdx

  .Lhaltproc:
    movl    $231,%eax                             { exit_group call }
    movl    code,%edi
    syscall
    jmp     .Lhaltproc
  end;

