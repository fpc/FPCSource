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
        movl    %esi,%edx
        movl    $1,%ecx
        andl    $0x0fffffff8,%esi
        // skip length byte
        incl    %edi
        subl    %esi,%edx
        jz      .LStrPasAligned
        movl    p,%esi
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
        movl    (%esi),%eax
        addl    $4,%esi
        // this won't overwrite data since the result = 255 char string
        // and we never process more than the first 255 chars of p
        movl    %eax,(%edi)
        testl   $0x0ff,%eax
        jz      .LStrPasDone
        incl    %ecx
        testl   $0x0ff00,%eax
        jz      .LStrPasDone
        incl    %ecx
        testl   $0x0ff0000,%eax
        jz      .LStrPasDone
        incl    %ecx
        testl   $0x0ff000000,%eax
        jz      .LStrPasDone
        incl    %ecx
        addl    $4,%edi
        cmpl    $252,%ecx
        jbe     .LStrPasAligned
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
end ['EAX','ECX','EDX','ESI','EDI'];
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
  Revision 1.1  2000-07-13 06:30:43  michael
  + Initial import

  Revision 1.15  2000/07/01 10:52:12  jonas
    * fixed reading past end-of-heap again (correctly this time I hope)

  Revision 1.14  2000/06/30 12:20:20  jonas
    * strpas is again slightly slower, but won't crash anymore if a pchar
      is passed to it that starts less than 4 bbytes from the heap end

  Revision 1.13  2000/06/12 19:53:32  peter
    * change .align to .balign

  Revision 1.12  2000/06/12 13:17:56  jonas
    * fixed typo :(

  Revision 1.11  2000/06/12 08:33:26  jonas
    * new fixed and faster strpas (previous version only returned the first
      254 chars when the pchar was aligned on a 4 byte boundary and was >=
      255 chars)

  Revision 1.10  2000/03/28 11:14:33  jonas
    * added missing register that is destroyed by strecopy
    + some destroyed register lists for procedures that didn't have one yet

  Revision 1.9  2000/02/09 16:59:29  peter
    * truncated log

  Revision 1.8  2000/01/11 22:56:57  pierre
    * wrong change for StrPas function corrected

  Revision 1.7  2000/01/11 21:12:15  marco
   * direct params to internal asm.

  Revision 1.6  2000/01/07 16:41:33  daniel
    * copyright 2000

}
