{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Processor specific implementation of strpas

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
begin
asm
        movl    p,%esi
        movl    $1,%ecx
        testl   %esi,%esi
        movl    %esi,%eax
        jz      .LStrPasDone
        movl    __RESULT,%edi
        leal    3(%esi),%edx
        andl    $-4,%edx
        // skip length byte
        incl    %edi
        subl    %esi,%edx
        jz      .LStrPasAligned
        // align source to multiple of 4 (not dest, because we can't read past
        // the end of the source, since that may be past the end of the heap
        // -> sigsegv!!)
.LStrPasAlignLoop:
        movb    (%esi),%al
        incl    %esi
        testb   %al,%al
        jz      .LStrPasDone
        incl    %edi
        incb    %cl
        decb    %dl
        movb    %al,-1(%edi)
        jne     .LStrPasAlignLoop
        .balign  16
.LStrPasAligned:
        movl    (%esi),%ebx
        addl    $4,%edi
        leal    0x0fefefeff(%ebx),%eax
        movl    %ebx,%edx
        addl    $4,%esi
        notl    %edx
        andl    %edx,%eax
        addl    $4,%ecx
        andl    $0x080808080,%eax
        movl    %ebx,-4(%edi)
        jnz     .LStrPasEndFound
        cmpl    $252,%ecx
        ja      .LStrPasPreEndLoop
        jmp     .LStrPasAligned
.LStrPasEndFound:
        subl    $4,%ecx
        // this won't overwrite data since the result = 255 char string
        // and we never process more than the first 255 chars of p
        shrl    $8,%eax
        jc      .LStrPasDone
        incl    %ecx
        shrl    $8,%eax
        jc      .LStrPasDone
        incl    %ecx
        shrl    $8,%eax
        jc      .LStrPasDone
        incl    %ecx
        jmp     .LStrPasDone
.LStrPasPreEndLoop:
        testb   %cl,%cl
        jz      .LStrPasDone
        movl    (%esi),%eax
.LStrPasEndLoop:
        testb   %al,%al
        jz      .LStrPasDone
        movb    %al,(%edi)
        shrl    $8,%eax
        incl    %edi
        incb    %cl
        jnz     .LStrPasEndLoop
.LStrPasDone:
        movl    __RESULT,%edi
        addb    $255,%cl
        movb    %cl,(%edi)
end ['EAX','EBX','ECX','EDX','ESI','EDI'];
end;


{
  $Log$
  Revision 1.2  2002-09-07 16:01:19  peter
    * old logs removed and tabs fixed

}
