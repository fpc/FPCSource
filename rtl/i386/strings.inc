{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Processor dependent part of strings.pp, that can be shared with
    sysutils unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ASMMODE ATT}

{$define FPC_UNIT_HAS_STRCOPY}
function strcopy(dest,source : pchar) : pchar;assembler;
asm
        movl    source,%edi
        testl   %edi,%edi
        jz      .LStrCopyDone
        leal    3(%edi),%ecx
        andl    $-4,%ecx
        movl    source,%esi
        subl    %edi,%ecx
        movl    dest,%edi
        jz      .LStrCopyAligned
.LStrCopyAlignLoop:
        movb    (%esi),%al
        incl    %edi
        incl    %esi
        testb   %al,%al
        movb    %al,-1(%edi)
        jz      .LStrCopyDone
        decl    %ecx
        jnz     .LStrCopyAlignLoop
        .balign  16
.LStrCopyAligned:
        movl    (%esi),%eax
        movl    %eax,%edx
        leal    0x0fefefeff(%eax),%ecx
        notl    %edx
        addl    $4,%esi
        andl    %edx,%ecx
        andl    $0x080808080,%ecx
        jnz     .LStrCopyEndFound
        movl    %eax,(%edi)
        addl    $4,%edi
        jmp     .LStrCopyAligned
.LStrCopyEndFound:
        testl   $0x0ff,%eax
        jz      .LStrCopyByte
        testl   $0x0ff00,%eax
        jz      .LStrCopyWord
        testl   $0x0ff0000,%eax
        jz      .LStrCopy3Bytes
        movl    %eax,(%edi)
        jmp     .LStrCopyDone
.LStrCopy3Bytes:
        xorb     %dl,%dl
        movw     %ax,(%edi)
        movb     %dl,2(%edi)
        jmp     .LStrCopyDone
.LStrCopyWord:
        movw    %ax,(%edi)
        jmp     .LStrCopyDone
.LStrCopyByte:
        movb    %al,(%edi)
.LStrCopyDone:
        movl    dest,%eax
end ['EAX','EDX','ECX','ESI','EDI'];


{$define FPC_UNIT_HAS_STRECOPY}
function strecopy(dest,source : pchar) : pchar;assembler;
asm
        cld
        movl    source,%edi
        movl    $0xffffffff,%ecx
        xorl    %eax,%eax
        repne
        scasb
        not     %ecx
        movl    dest,%edi
        movl    source,%esi
        movl    %ecx,%eax
        shrl    $2,%ecx
        rep
        movsl
        movl    %eax,%ecx
        andl    $3,%ecx
        rep
        movsb
        movl    dest,%eax
        decl    %edi
        movl    %edi,%eax
end ['EAX','ECX','ESI','EDI'];


{$define FPC_UNIT_HAS_STRLCOPY}
function strlcopy(dest,source : pchar;maxlen : longint) : pchar;assembler;
asm
        movl    source,%esi
        movl    maxlen,%ecx
        movl    dest,%edi
        orl     %ecx,%ecx
        jz      .LSTRLCOPY2
        cld
.LSTRLCOPY1:
        lodsb
        stosb
        decl    %ecx            // Lower maximum
        jz      .LSTRLCOPY2     // 0 reached ends
        orb     %al,%al
        jnz     .LSTRLCOPY1
        jmp     .LSTRLCOPY3
.LSTRLCOPY2:
        xorb    %al,%al         // If cutted
        stosb                   // add a #0
.LSTRLCOPY3:
        movl    dest,%eax
end ['EAX','ECX','ESI','EDI'];


{$define FPC_UNIT_HAS_STRLEN}
function strlen(p : pchar) : longint;assembler;
{$i strlen.inc}


{$define FPC_UNIT_HAS_STREND}
function strend(p : pchar) : pchar;assembler;
asm
        cld
        xorl    %eax,%eax
        movl    p,%edi
        orl     %edi,%edi
        jz      .LStrEndNil
        movl    $0xffffffff,%ecx
        xorl    %eax,%eax
        repne
        scasb
        movl    %edi,%eax
        decl    %eax
.LStrEndNil:
end ['EDI','ECX','EAX'];



{$define FPC_UNIT_HAS_STRCOMP}
function strcomp(str1,str2 : pchar) : longint;assembler;
asm
        movl    str2,%edi
        movl    $0xffffffff,%ecx
        cld
        xorl    %eax,%eax
        repne
        scasb
        not     %ecx
        movl    str2,%edi
        movl    str1,%esi
        repe
        cmpsb
        movb    -1(%esi),%al
        movzbl  -1(%edi),%ecx
        subl    %ecx,%eax
end ['EAX','ECX','ESI','EDI'];



{$define FPC_UNIT_HAS_STRLCOMP}
function strlcomp(str1,str2 : pchar;l : longint) : longint;assembler;
asm
        movl    str2,%edi
        movl    $0xffffffff,%ecx
        cld
        xorl    %eax,%eax
        repne
        scasb
        not     %ecx
        cmpl    l,%ecx
        jl      .LSTRLCOMP1
        movl    l,%ecx
.LSTRLCOMP1:
        movl    str2,%edi
        movl    str1,%esi
        repe
        cmpsb
        movb    -1(%esi),%al
        movzbl  -1(%edi),%ecx
        subl    %ecx,%eax
end ['EAX','ECX','ESI','EDI'];



{$define FPC_UNIT_HAS_STRICOMP}
function stricomp(str1,str2 : pchar) : longint;assembler;
asm
        movl    str2,%edi
        movl    $0xffffffff,%ecx
        cld
        xorl    %eax,%eax
        repne
        scasb
        not     %ecx
        movl    str2,%edi
        movl    str1,%esi
.LSTRICOMP2:
        repe
        cmpsb
        jz      .LSTRICOMP3     // If last reached then exit
        movzbl  -1(%esi),%eax
        movzbl  -1(%edi),%ebx
        cmpb    $97,%al
        jb      .LSTRICOMP1
        cmpb    $122,%al
        ja      .LSTRICOMP1
        subb    $0x20,%al
.LSTRICOMP1:
        cmpb    $97,%bl
        jb      .LSTRICOMP4
        cmpb    $122,%bl
        ja      .LSTRICOMP4
        subb    $0x20,%bl
.LSTRICOMP4:
        subl    %ebx,%eax
        jz      .LSTRICOMP2     // If still equal, compare again
.LSTRICOMP3:
end ['EAX','EBX','ECX','ESI','EDI'];



{$define FPC_UNIT_HAS_STRLICOMP}
function strlicomp(str1,str2 : pchar;l : longint) : longint;assembler;
asm
        movl    str2,%edi
        movl    $0xffffffff,%ecx
        cld
        xorl    %eax,%eax
        repne
        scasb
        not     %ecx
        cmpl    l,%ecx
        jl      .LSTRLICOMP5
        movl    l,%ecx
.LSTRLICOMP5:
        movl    str2,%edi
        movl    str1,%esi
.LSTRLICOMP2:
        repe
        cmpsb
        jz      .LSTRLICOMP3    // If last reached, exit
        movzbl  -1(%esi),%eax
        movzbl  -1(%edi),%ebx
        cmpb    $97,%al
        jb      .LSTRLICOMP1
        cmpb    $122,%al
        ja      .LSTRLICOMP1
        subb    $0x20,%al
.LSTRLICOMP1:
        cmpb    $97,%bl
        jb      .LSTRLICOMP4
        cmpb    $122,%bl
        ja      .LSTRLICOMP4
        subb    $0x20,%bl
.LSTRLICOMP4:
        subl    %ebx,%eax
        jz      .LSTRLICOMP2
.LSTRLICOMP3:
end ['EAX','EBX','ECX','ESI','EDI'];



{$define FPC_UNIT_HAS_STRSCAN}
function strscan(p : pchar;c : char) : pchar;assembler;
asm
        movl    p,%eax
        xorl    %ecx,%ecx
        testl   %eax,%eax
        jz      .LSTRSCAN
// align
        movb    c,%cl
        leal    3(%eax),%esi
        andl    $-4,%esi
        movl    p,%edi
        subl    %eax,%esi
        jz      .LSTRSCANALIGNED
        xorl    %eax,%eax
.LSTRSCANALIGNLOOP:
        movb    (%edi),%al
// at .LSTRSCANFOUND, one is substracted from edi to calculate the position,
// so add 1 here already (not after .LSTRSCAN, because then the test/jz and
// cmp/je can't be paired)
        incl    %edi
        testb   %al,%al
        jz      .LSTRSCAN
        cmpb    %cl,%al
        je      .LSTRSCANFOUND
        decl    %esi
        jnz     .LSTRSCANALIGNLOOP
.LSTRSCANALIGNED:
// fill ecx with cccc
        movl     %ecx,%eax
        shll     $8,%eax
        orl      %eax,%ecx
        movl     %ecx,%eax
        shll     $16,%eax
        orl      %eax,%ecx
        .balign  16
.LSTRSCANLOOP:
// load new 4 bytes
        movl     (%edi),%edx
// in eax, we will check if "c" appear in the loaded dword
        movl     %edx,%eax
// esi will be used to calculate the mask
        movl     %edx,%esi
        notl     %esi
// in edx we will check for the end of the string
        addl     $0x0fefefeff,%edx
        xorl     %ecx,%eax
        andl     $0x080808080,%esi
        addl     $4,%edi
        andl     %esi,%edx
        movl     %eax,%esi
        notl     %esi
        jnz      .LSTRSCANLONGCHECK
        addl     $0x0fefefeff,%eax
        andl     $0x080808080,%esi
        andl     %esi,%eax
        jz       .LSTRSCANLOOP

// the position in %eax where the char was found is now $80, so keep on
// shifting 8 bits out of %eax until we find a non-zero bit.
// first char
        shrl    $8,%eax
        jc      .LSTRSCANFOUND1
// second char
        shrl    $8,%eax
        jc      .LSTRSCANFOUND2
// third char
        shrl    $8,%eax
        jc      .LSTRSCANFOUND3
// fourth char
        jmp     .LSTRSCANFOUND
.LSTRSCANLONGCHECK:
// there's a null somewhere, but we still have to check whether there isn't
// a 'c' before it.
        addl     $0x0fefefeff,%eax
        andl     $0x080808080,%esi
        andl     %esi,%eax
// Now, in eax we have $80 on the positions where there were c-chars and in
// edx we have $80 on the positions where there were #0's. On all other
// positions, there is now #0
// first char
        shrl    $8,%eax
        jc      .LSTRSCANFOUND1
        shrl    $8,%edx
        jc      .LSTRSCANNOTFOUND
// second char
        shrl    $8,%eax
        jc      .LSTRSCANFOUND2
        shrl    $8,%edx
        jc      .LSTRSCANNOTFOUND
// third char
        shrl    $8,%eax
        jc      .LSTRSCANFOUND3
        shrl    $8,%edx
        jc      .LSTRSCANNOTFOUND
// we know the fourth char is now #0 (since we only jump to the long check if
// there is a #0 char somewhere), but it's possible c = #0, and than we have
// to return the end of the string and not nil!
        shrl    $8,%eax
        jc      .LSTRSCANFOUND
        jmp     .LSTRSCANNOTFOUND
.LSTRSCANFOUND3:
        leal   -2(%edi),%eax
        jmp     .LSTRSCAN
.LSTRSCANFOUND2:
        leal   -3(%edi),%eax
        jmp     .LSTRSCAN
.LSTRSCANFOUND1:
        leal    -4(%edi),%eax
        jmp     .LSTRSCAN
.LSTRSCANFOUND:
        leal    -1(%edi),%eax
        jmp     .LSTRSCAN
.LSTRSCANNOTFOUND:
        xorl    %eax,%eax
.LSTRSCAN:
end ['EAX','ECX','ESI','EDI','EDX'];


{$define FPC_UNIT_HAS_STRRSCAN}
function strrscan(p : pchar;c : char) : pchar;assembler;
asm
        xorl    %eax,%eax
        movl    p,%edi
        orl     %edi,%edi
        jz      .LSTRRSCAN
        movl    $0xffffffff,%ecx
        cld
        xorb    %al,%al
        repne
        scasb
        not     %ecx
        movb    c,%al
        movl    p,%edi
        addl    %ecx,%edi
        decl    %edi
        std
        repne
        scasb
        cld
        movl    $0,%eax
        jnz     .LSTRRSCAN
        movl    %edi,%eax
        incl    %eax
.LSTRRSCAN:
end ['EAX','ECX','EDI'];


{$define FPC_UNIT_HAS_STRUPPER}
function strupper(p : pchar) : pchar;assembler;
asm
        movl    p,%esi
        orl     %esi,%esi
        jz      .LStrUpperNil
        movl    %esi,%edi
.LSTRUPPER1:
        lodsb
        cmpb    $97,%al
        jb      .LSTRUPPER3
        cmpb    $122,%al
        ja      .LSTRUPPER3
        subb    $0x20,%al
.LSTRUPPER3:
        stosb
        orb     %al,%al
        jnz     .LSTRUPPER1
.LStrUpperNil:
        movl    p,%eax
end ['EAX','ESI','EDI'];


{$define FPC_UNIT_HAS_STRLOWER}
function strlower(p : pchar) : pchar;assembler;
asm
        movl    p,%esi
        orl     %esi,%esi
        jz      .LStrLowerNil
        movl    %esi,%edi
.LSTRLOWER1:
        lodsb
        cmpb    $65,%al
        jb      .LSTRLOWER3
        cmpb    $90,%al
        ja      .LSTRLOWER3
        addb    $0x20,%al
.LSTRLOWER3:
        stosb
        orb     %al,%al
        jnz     .LSTRLOWER1
.LStrLowerNil:
        movl    p,%eax
end ['EAX','ESI','EDI'];

{
  $Log$
  Revision 1.8  2003-04-30 16:36:39  florian
    + support for generic pchar routines added
    + some basic rtl stuff for x86-64 added

  Revision 1.7  2002/09/07 16:01:19  peter
    * old logs removed and tabs fixed
}
