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
{$ifndef NEWATT}
  strpasopt:='';
{$endif}
asm
{$ifdef NEWATT}
        movl    __RESULT,%edi
{$else}
        movl    8(%ebp),%edi
{$endif}
        movl    p,%esi
        // at the end, add 255 to cl to get the string length (byte(1+255) = 0)
        movb    $1,%cl
        // skip length byte -> align dest to multiple of 4
        movl    (%esi),%eax
        testl   $0x0ff,%eax
        jz      .LStrPasDone
        // we only need the first 3 chars currently
        shll    $8,%eax
        incb    %cl
        addl    $3,%esi
        // Store everything already, since the temp string = 255 chars anyway
        // The length byte will contain zero this way, but it will be
        // overwritten at the end, so it doesn't matter       
        movl    %eax,(%edi)
        // test the second char (we shifted left 8 bits)
        testl   $0x0ff0000,%eax
        jz      .LStrPasDone
        // for pairing, add 4 to edi here already
        addl    $4,%edi
        incb    %cl
        // test the third char (we shifted left 8 bits)
        testl   $0x0ff000000,%eax
        jz      .LStrPasDone
        incb    %cl
        .align  16
.LStrPasLoop:
        movl    (%esi),%eax
        addl    $4,%esi
        // this won't overwrite data since the result = 255 char string
        movl    %eax,(%edi)
        testl   $0x0ff,%eax
        jz      .LStrPasDone
        testl   $0x0ff00,%eax
        jz      .LStrPasByte
        testl   $0x0ff0000,%eax
        jz      .LStrPasWord
        testl   $0x0ff000000,%eax
        jz      .LStrPas3Bytes
        addl    $4,%edi
        addb    $4,%cl
        // since cl = 4 at the start of the loop, it will always count
        // upto exactly 0
        jnz     .LStrPasLoop
        jmp     .LStrPasDone
.LStrPas3Bytes:
        addb     $3,%cl
        jmp     .LStrPasDone
.LStrPasWord:
        addb    $2,%cl
        jmp     .LStrPasDone
.LStrPasByte:
        incb    %cl
.LStrPasDone:
{$ifdef NEWATT}
        movl    __RESULT,%edi
{$else}
        movl    8(%ebp),%edi
{$endif}
        addb    $255,%cl
        movb    %cl,(%edi)
end ['EAX','ECX','ESI','EDI'];
end;


function strpcopy(d : pchar;const s : string) : pchar;assembler;
asm
        pushl   %esi            // Save ESI
        cld
        movl    d,%edi          // load destination address
        movl    s,%esi          // Load Source adress
        lodsb                   // load length in ECX
        movzbl  %al,%ecx
        rep
        movsb
        xorb    %al,%al         // Set #0
        stosb
        movl    d,%eax       // return value to EAX
        popl    %esi
end ['EDI','EAX','ECX'];

{
  $Log$
  Revision 1.11  2000-06-12 08:33:26  jonas
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
