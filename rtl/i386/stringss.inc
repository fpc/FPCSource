{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Processor dependent part of strings.pp, not shared with
    sysutils unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function strpas(p : pchar) : string;
begin
asm
        movl    p,%esi
        movl    __RESULT,%edi
        leal    3(%esi),%edx
        movl    $1,%ecx
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

function strpcopy(d : pchar;const s : string) : pchar;assembler;
asm
        pushl   %esi            // Save ESI
        cld
        movl    s,%esi          // Load Source adress
        movl    d,%edi          // load destination address
        movzbl  (%esi),%ecx        // load length in ECX
        incl    %esi
        rep
        movsb
        movb    $0,(%edi)
        movl    d,%eax       // return value to EAX
        popl    %esi
end ['EDI','EAX','ECX'];

{
  $Log$
  Revision 1.5  2001-03-04 17:31:35  jonas
    * fixed all implementations of strpas

  Revision 1.4  2001/03/04 12:42:18  jonas
    * fixed strpas (was limited to 254 chars) and made it overall slightly faster

  Revision 1.3  2001/02/10 16:08:46  jonas
    * fixed non-working alignment code

  Revision 1.2  2000/07/13 11:33:42  michael
  + removed logs

}
