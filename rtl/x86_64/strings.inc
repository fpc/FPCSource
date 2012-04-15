{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by Florian Klaempfl, member of the
    Free Pascal development team

    Processor dependent part of strings.pp, that can be shared with
    sysutils unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ASMMODE GAS}

{$ifndef FPC_UNIT_HAS_STRCOMP}
{$define FPC_UNIT_HAS_STRCOMP}
function StrComp(Str1, Str2: PChar): SizeInt;assembler;nostackframe;
asm
{$ifndef win64}
        movq   %rsi,%rdx
        movq   %rdi,%rcx
{$endif win64}
        subq   %rcx,%rdx
.balign 16
.Lloop:                          { unrolled 4 times }
        movb   (%rcx),%al
        cmpb   (%rdx,%rcx),%al
        jne    .Ldiff
        testb  %al,%al
        jz    .Leq
        movb   1(%rcx),%al
        cmpb   1(%rdx,%rcx),%al
        jne    .Ldiff
        testb  %al,%al
        jz     .Leq
        movb   2(%rcx),%al
        cmpb   2(%rdx,%rcx),%al
        jne    .Ldiff
        testb  %al,%al
        jz     .Leq
        movb   3(%rcx),%al
        add    $4,%rcx
        cmpb   -1(%rdx,%rcx),%al
        jne    .Ldiff
        testb  %al,%al
        jnz    .Lloop
.Leq:
        xorq   %rax,%rax
        jmp    .Lexit

.Ldiff:
        sbbq   %rax,%rax        { -1 if CF was set, 0 otherwise }
        orb    $1,%al           { 0 becomes 1, -1 remains unchanged }
.Lexit:
end;
{$endif FPC_UNIT_HAS_STRCOMP}
