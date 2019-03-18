{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by Free Pascal development team

    This file implements parts of the startup code for OpenBSD
    shared object (.so) libraries.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$asmmode gas}

procedure _init; external name '_init';

procedure _FPC_proc___start; forward;

procedure _FPC_proc_start; assembler; nostackframe; public name '_start'; public name '__start';
  asm
    movq    %rbx,%r9
    movq    %rcx,%r8
    movq    %rdx,%rcx
    movq    (%rsp),%rdi
    leaq    16(%rsp,%rdi,8),%rdx
    leaq    8(%rsp),%rsi
    subq    $8,%rsp
    andq    $0xFFFFFFFFFFFFFFF0,%rsp
    addq    $8,%rsp
    jmp     _FPC_proc___start
  end;

procedure _FPC_proc_haltproc; forward;
function _strrchr(str: PChar; character: LongInt): PChar; forward;

procedure _FPC_proc___start; assembler; nostackframe; public name '___start';
  asm
    pushq   %rbp
    movq    %rsp, %rbp
    subq    $64, %rsp
    movl    %edi, -20(%rbp)
    movq    %rsi, -32(%rbp)
    movq    %rdx, -40(%rbp)  { environment }
    movq    %rcx, -48(%rbp)
    movq    %r8, -56(%rbp)
    movq    %r9, -64(%rbp)
    movq    environ@GOTPCREL(%rip), %rdx
    movq    -40(%rbp), %rax
    movq    %rax, (%rdx)
    movq    operatingsystem_parameter_envp@GOTPCREL(%rip), %rdx
    movq    -40(%rbp), %rax
    movq    %rax, (%rdx)

    movl    -20(%rbp), %eax
    movslq  %eax,%rdx
    movq    operatingsystem_parameter_argc@GOTPCREL(%rip), %rax
    movq    %rdx, (%rax)
    movq    operatingsystem_parameter_argv@GOTPCREL(%rip), %rdx
    movq    -32(%rbp), %rax
    movq    %rax, (%rdx)
    movq    -32(%rbp), %rax
    movq    (%rax), %rax
    movq    %rax, -8(%rbp)
    cmpq    $0, -8(%rbp)
    je      .L2
    movq    -8(%rbp), %rdi
    movl    $47, %esi
    call    _strrchr
    movq    %rax, %rdx
    movq    __progname@GOTPCREL(%rip), %rax
    movq    %rdx, (%rax)
    movq    __progname@GOTPCREL(%rip), %rax
    movq    (%rax), %rax
    testq   %rax, %rax
    jne     .L4
    movq    __progname@GOTPCREL(%rip), %rdx
    movq    -8(%rbp), %rax
    movq    %rax, (%rdx)
    jmp     .L6
.L4:
    movq    __progname@GOTPCREL(%rip), %rax
    movq    (%rax), %rax
    leaq    1(%rax), %rdx
    movq    __progname@GOTPCREL(%rip), %rax
    movq    %rdx, (%rax)
.L6:
    movq    __progname_storage@GOTPCREL(%rip), %rax
    movq    %rax, -16(%rbp)
    jmp     .L7
.L8:
    movq    __progname@GOTPCREL(%rip), %rax
    movq    (%rax), %rcx
    movzbl  (%rcx), %edx
    movq    -16(%rbp), %rax
    movb    %dl, (%rax)
    addq    $1, -16(%rbp)
    leaq    1(%rcx), %rdx
    movq    __progname@GOTPCREL(%rip), %rax
    movq    %rdx, (%rax)
.L7:
    movq    __progname@GOTPCREL(%rip), %rax
    movq    (%rax), %rax
    movzbl  (%rax), %eax
    testb   %al, %al
    je      .L9
    movq    __progname_storage@GOTPCREL(%rip), %rax
    leaq    255(%rax), %rax
    cmpq    %rax, -16(%rbp)
    jb      .L8
.L9:
    movq    -16(%rbp), %rax
    movb    $0, (%rax)
    movq    __progname@GOTPCREL(%rip), %rdx
    movq    __progname_storage@GOTPCREL(%rip), %rax
    movq    %rax, (%rdx)
.L2:
    movl    $0, %eax
    call    _init@PLT
    movq    environ@GOTPCREL(%rip), %rax
    movq    (%rax), %rdx
    movq    -32(%rbp), %rsi
    movl    -20(%rbp), %edi
    movl    $0, %eax
    call    PASCALMAIN@PLT
    // movl    %eax, %edi
    // call    exit
    jmp     _FPC_proc_haltproc@PLT
  end;

procedure _FPC_proc_haltproc; assembler; nostackframe; public name '_haltproc';
  asm
    movq    $1,%rax
    movl    operatingsystem_result(%rip),%ebx
    pushq   %rbx
    call    .Lactualsyscall
    addq    $8,%rsp
    jmp     _FPC_proc_haltproc

.Lactualsyscall:
    int     $0x80
    jb      .LErrorcode
    xor     %rbx,%rbx
    ret
.LErrorcode:
    movq    %rax,%rbx
    movq    $-1,%rax
  end;

function _strrchr(str: PChar; character: LongInt): PChar; public name '_strrchr';
  begin
    _strrchr:=nil;
    repeat
      if str^=Chr(character) then
        _strrchr:=str;
      if str^<>#0 then
        Inc(str);
    until str^=#0;
  end;

