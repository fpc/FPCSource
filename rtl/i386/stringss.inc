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
  strpas:='';
{$endif}
  asm
        cld
        movl    p,%edi
        movl    $0xff,%ecx
        xorl    %eax,%eax
        movl    %edi,%esi
        repne
        scasb
        movl    %ecx,%eax
{$ifdef NEWATT}
        movl    __RESULT,%edi
{$else}
        movl    8(%ebp),%edi
{$endif}
        notb    %al
        decl    %eax
        stosb
        cmpl    $7,%eax
        jl      .LStrPas2
        movl    %edi,%ecx       // Align on 32bits
        negl    %ecx
        andl    $3,%ecx
        subl    %ecx,%eax
        rep
        movsb
        movl    %eax,%ecx
        andl    $3,%eax
        shrl    $2,%ecx
        rep
        movsl
.LStrPas2:
        movl    %eax,%ecx
        rep
        movsb
  end ['ECX','EAX','ESI','EDI'];
end;


function strpcopy(d : pchar;const s : string) : pchar;assembler;
asm
        pushl   %esi            // Save ESI
        cld
        movl    d,%edi          // load destination address
        movl    s,%esi          // Load Source adress
        movl    %edi,%ebx       // Set return value
        lodsb                   // load length in ECX
        movzbl  %al,%ecx
        rep
        movsb
        xorb    %al,%al         // Set #0
        stosb
        movl    %ebx,%eax       // return value to EAX
        popl    %esi
end ['EDI','ESI','EBX','EAX','ECX'];

{
  $Log$
  Revision 1.9  2000-02-09 16:59:29  peter
    * truncated log

  Revision 1.8  2000/01/11 22:56:57  pierre
    * wrong change for StrPas function corrected

  Revision 1.7  2000/01/11 21:12:15  marco
   * direct params to internal asm.

  Revision 1.6  2000/01/07 16:41:33  daniel
    * copyright 2000

}
