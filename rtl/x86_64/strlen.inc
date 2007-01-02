{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Processor specific implementation of strlen

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  Implemented using the code from  glibc: libc/sysdeps/x86_64/strlen.S Version 1.2
}
{$ifdef win64}
var
  rdi : qword;
{$endif win64}
asm
        { win64 has different calling conventions }
{$ifdef win64}
        movq %rdi,rdi
        movq %rcx, %rdi                { Duplicate source pointer. }
{$else win64}
        movq %rdi, %rcx                { Duplicate source pointer. }
{$endif win64}
        andl $7, %ecx                  { mask alignment bits }
        movq %rdi, %rax                { duplicate destination.  }
        jz .LFPC_STRLEN_1               { aligned => start loop }

        neg %ecx                       { We need to align to 8 bytes.  }
        addl $8,%ecx
        { Search the first bytes directly.  }
.LFPC_STRLEN_0:
        cmpb $0x0,(%rax)                { is byte NUL? }
        je .LFPC_STRLEN_2                { yes => return }
        incq %rax                       { increment pointer }
        decl %ecx
        jnz .LFPC_STRLEN_0

.LFPC_STRLEN_1:
         movq $0xfefefefefefefeff,%r8  { Save magic.  }

        .p2align 4                     { Align loop.  }
.LFPC_STRLEN_4:                          { Main Loop is unrolled 4 times.  }
        { First unroll.  }
        movq (%rax), %rcx              { get double word (= 8 bytes) in question }
        addq $8,%rax                   { adjust pointer for next word }
        movq %r8, %rdx                 { magic value }
        addq %rcx, %rdx                { add the magic value to the word.  We get
                                         carry bits reported for each byte which
                                         is *not* 0 }
        jnc .LFPC_STRLEN_3               { highest byte is NUL => return pointer }
        xorq %rcx, %rdx                { (word+magic)^word }
        orq %r8, %rdx                  { set all non-carry bits }
        incq %rdx                      { add 1: if one carry bit was *not* set
                                         the addition will not result in 0.  }
        jnz .LFPC_STRLEN_3               { found NUL => return pointer }

        { Second unroll.  }
        movq (%rax), %rcx        { get double word (= 8 bytes) in question }
        addq $8,%rax                { adjust pointer for next word }
        movq %r8, %rdx                { magic value }
        addq %rcx, %rdx                { add the magic value to the word.  We get
                                   carry bits reported for each byte which
                                   is *not* 0 }
        jnc .LFPC_STRLEN_3                        { highest byte is NUL => return pointer }
        xorq %rcx, %rdx                { (word+magic)^word }
        orq %r8, %rdx                { set all non-carry bits }
        incq %rdx                { add 1: if one carry bit was *not* set
                                   the addition will not result in 0.  }
        jnz .LFPC_STRLEN_3                        { found NUL => return pointer }

        { Third unroll.  }
        movq (%rax), %rcx        { get double word (= 8 bytes) in question }
        addq $8,%rax                { adjust pointer for next word }
        movq %r8, %rdx                { magic value }
        addq %rcx, %rdx                { add the magic value to the word.  We get
                                   carry bits reported for each byte which
                                   is *not* 0 }
        jnc .LFPC_STRLEN_3                        { highest byte is NUL => return pointer }
        xorq %rcx, %rdx                { (word+magic)^word }
        orq %r8, %rdx                { set all non-carry bits }
        incq %rdx                { add 1: if one carry bit was *not* set
                                   the addition will not result in 0.  }
        jnz .LFPC_STRLEN_3                        { found NUL => return pointer }

        { Fourth unroll.  }
        movq (%rax), %rcx        { get double word (= 8 bytes) in question }
        addq $8,%rax                { adjust pointer for next word }
        movq %r8, %rdx                { magic value }
        addq %rcx, %rdx                { add the magic value to the word.  We get
                                   carry bits reported for each byte which
                                   is *not* 0 }
        jnc .LFPC_STRLEN_3                        { highest byte is NUL => return pointer }
        xorq %rcx, %rdx                { (word+magic)^word }
        orq %r8, %rdx                { set all non-carry bits }
        incq %rdx                { add 1: if one carry bit was *not* set
                                   the addition will not result in 0.  }
        jz .LFPC_STRLEN_4                        { no NUL found => continue loop }

        .p2align 4                { Align, it's a jump target.  }
.LFPC_STRLEN_3:
        subq $8,%rax                { correct pointer increment.  }

        testb %cl, %cl                { is first byte NUL? }
        jz .LFPC_STRLEN_2                        { yes => return }
        incq %rax                { increment pointer }

        testb %ch, %ch                { is second byte NUL? }
        jz .LFPC_STRLEN_2                        { yes => return }
        incq %rax                { increment pointer }

        testl $0x00ff0000, %ecx { is third byte NUL? }
        jz .LFPC_STRLEN_2                        { yes => return pointer }
        incq %rax                { increment pointer }

        testl $0xff000000, %ecx { is fourth byte NUL? }
        jz .LFPC_STRLEN_2                        { yes => return pointer }
        incq %rax                { increment pointer }

        shrq $32, %rcx                { look at other half.  }

        testb %cl, %cl                { is first byte NUL? }
        jz .LFPC_STRLEN_2                        { yes => return }
        incq %rax                { increment pointer }

        testb %ch, %ch                { is second byte NUL? }
        jz .LFPC_STRLEN_2                        { yes => return }
        incq %rax                { increment pointer }

        testl $0xff0000, %ecx        { is third byte NUL? }
        jz .LFPC_STRLEN_2                        { yes => return pointer }
        incq %rax                { increment pointer }
.LFPC_STRLEN_2:
        subq %rdi, %rax                { compute difference to string start }
{$ifdef win64}
        movq rdi,%rdi
{$endif win64}
end;


