{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by the Free Pascal development team

    Processor dependent part of strings.pp, not shared with 
    sysutils unit.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ASMMODE DIRECT}
    function strpas(p : pchar) : string;
    begin
      asm
        cld
        movl    12(%ebp),%edi
        movl    $0xff,%ecx
        xorl    %eax,%eax
        movl    %edi,%esi
        repne
        scasb
        movl    %ecx,%eax

        movl    8(%ebp),%edi
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
{$ASMMODE ATT}

    function strpcopy(d : pchar;const s : string) : pchar;

      begin
         asm
            pushl %esi          // Save ESI
            cld
            movl 8(%ebp),%edi   // load destination address
            movl 12(%ebp),%esi   // Load Source adress
            movl %edi,%ebx      // Set return value
            lodsb               // load length in ECX
            movzbl %al,%ecx
            rep
            movsb
            xorb %al,%al        // Set #0
            stosb
            movl %ebx,%eax      // return value to EAX
            popl %esi
            leave               // ... and ready
            ret $8
         end ['EDI','ESI','EBX','EAX','ECX'];
      end;

{
  $Log$
  Revision 1.2  1999-02-25 10:07:02  michael
  + Added header and log

}