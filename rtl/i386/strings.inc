{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999 by the Free Pascal development team

    Processor dependent part of strings.pp, that can be shared with 
    sysutils unit.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ASMMODE ATT}

    function strcopy(dest,source : pchar) : pchar;

      begin
         asm
            cld
            movl 12(%ebp),%edi
            movl $0xffffffff,%ecx
            xorb %al,%al
            repne
            scasb
            not %ecx
            movl 8(%ebp),%edi
            movl 12(%ebp),%esi
            movl %ecx,%eax
            shrl $2,%ecx
            rep
            movsl
            movl %eax,%ecx
            andl $3,%ecx
            rep
            movsb
            movl 8(%ebp),%eax
            leave
            ret $8
         end;
      end;

    function strecopy(dest,source : pchar) : pchar;

      begin
         asm
            cld
            movl 12(%ebp),%edi
            movl $0xffffffff,%ecx
            xorl %eax,%eax
            repne
            scasb
            not %ecx
            movl 8(%ebp),%edi
            movl 12(%ebp),%esi
            movl %ecx,%eax
            shrl $2,%ecx
            rep
            movsl
            movl %eax,%ecx
            andl $3,%ecx
            rep
            movsb
            movl 8(%ebp),%eax
            decl %edi
            movl %edi,%eax
            leave
            ret $8
         end ['EAX','ESI','EDI'];
      end;

    function strlcopy(dest,source : pchar;maxlen : longint) : pchar;

      begin
         asm
            movl 8(%ebp),%edi
            movl 12(%ebp),%esi
            movl 16(%ebp),%ecx
            cld
         .LSTRLCOPY1:
            lodsb
            stosb
            decl %ecx           // Lower maximum
            jz .LSTRLCOPY2      // 0 reached ends
            orb %al,%al
            jnz .LSTRLCOPY1
            movl 8(%ebp),%eax
            leave
            ret $12
        .LSTRLCOPY2:

            xorb %al,%al        // If cutted
            stosb               // add a #0
            movl 8(%ebp),%eax
            leave
            ret $12
         end ['EAX','ECX','ESI','EDI'];
      end;

    function strlen(p : pchar) : longint;
    begin
      asm
        cld
        movl    8(%ebp),%edi
        movl    $0xffffffff,%ecx
        xorl    %eax,%eax
        repne
        scasb
        movl    $0xfffffffe,%eax
        subl    %ecx,%eax
        leave
        ret     $4
      end ['EDI','ECX','EAX'];
    end;

    function strend(p : pchar) : pchar;

      begin
         asm
            cld
            movl 8(%ebp),%edi
            movl $0xffffffff,%ecx
            xorl %eax,%eax
            repne
            scasb
            movl %edi,%eax
            decl %eax
            leave
            ret $4
         end ['EDI','ECX','EAX'];
      end;

    function strcomp(str1,str2 : pchar) : longint;

      begin
         asm
            // Find terminating zero
            movl 12(%ebp),%edi
            movl $0xffffffff,%ecx
            cld
            xorl %eax,%eax
            repne
            scasb
            not %ecx
            movl 12(%ebp),%edi
            movl 8(%ebp),%esi
            repe
            cmpsb
            movb -1(%esi),%al
            movzbl -1(%edi),%ecx
            subl %ecx,%eax
            leave
            ret $8
         end ['EAX','ECX','ESI','EDI'];
      end;

    function strlcomp(str1,str2 : pchar;l : longint) : longint;

      begin
         asm
            // Find terminating zero
            movl 12(%ebp),%edi
            movl $0xffffffff,%ecx
            cld
            xorl %eax,%eax
            repne
            scasb
            not %ecx
            cmpl 16(%ebp),%ecx
            jl .LSTRLCOMP1
            movl 16(%ebp),%ecx
        .LSTRLCOMP1:
            movl 12(%ebp),%edi
            movl 8(%ebp),%esi
            repe
            cmpsb
            movb -1(%esi),%al
            movzbl -1(%edi),%ecx
            subl %ecx,%eax
            leave
            ret $12
         end ['EAX','ECX','ESI','EDI'];
      end;

    function stricomp(str1,str2 : pchar) : longint;

      begin
         asm
            // Find terminating zero
            movl 12(%ebp),%edi
            movl $0xffffffff,%ecx
            cld
            xorl %eax,%eax
            repne
            scasb
            not %ecx
            movl 12(%ebp),%edi
            movl 8(%ebp),%esi
       .LSTRICOMP2:
            repe
            cmpsb
            jz .LSTRICOMP3      // If last reached then exit
            movzbl -1(%esi),%eax
            movzbl -1(%edi),%ebx
            cmpb $97,%al
            jb .LSTRICOMP1
            cmpb $122,%al
            ja .LSTRICOMP1
            subb $0x20,%al
        .LSTRICOMP1:
            cmpb $97,%bl
            jb .LSTRICOMP4
            cmpb $122,%bl
            ja .LSTRICOMP4
            subb $0x20,%bl
       .LSTRICOMP4:
            subl %ebx,%eax
            jz .LSTRICOMP2      // If still equal, compare again
       .LSTRICOMP3:
            leave
            ret $8
         end ['EAX','ECX','ESI','EDI'];
      end;

    function strlicomp(str1,str2 : pchar;l : longint) : longint;

      begin
         asm
            // Search terminating zero
            movl 12(%ebp),%edi
            movl $0xffffffff,%ecx
            cld
            xorl %eax,%eax
            repne
            scasb
            not %ecx
            cmpl 16(%ebp),%ecx
            jl .LSTRLICOMP5
            movl 16(%ebp),%ecx
       .LSTRLICOMP5:
            movl 12(%ebp),%edi
            movl 8(%ebp),%esi
       .LSTRLICOMP2:
            repe
            cmpsb
            jz .LSTRLICOMP3     // If last reached, exit
            movzbl -1(%esi),%eax
            movzbl -1(%edi),%ebx
            cmpb $97,%al
            jb .LSTRLICOMP1
            cmpb $122,%al
            ja .LSTRLICOMP1
            subb $0x20,%al
        .LSTRLICOMP1:
            cmpb $97,%bl
            jb .LSTRLICOMP4
            cmpb $122,%bl
            ja .LSTRLICOMP4
            subb $0x20,%bl
       .LSTRLICOMP4:
            subl %ebx,%eax
            jz .LSTRLICOMP2

       .LSTRLICOMP3:
            leave
            ret $12
         end ['EAX','ECX','ESI','EDI'];
      end;

    function strscan(p : pchar;c : char) : pchar;

      begin
         asm
            movl 8(%ebp),%edi
            movl $0xffffffff,%ecx
            cld
            xorb %al,%al
            repne
            scasb
            not %ecx
            movb 12(%ebp),%al
            movl 8(%ebp),%edi
            repne
            scasb
            movl $0,%eax
            jnz .LSTRSCAN
            movl %edi,%eax
            decl %eax
        .LSTRSCAN:
            leave
            ret $6
         end;
      end;

    function strrscan(p : pchar;c : char) : pchar;

      begin
         asm
            movl 8(%ebp),%edi
            movl $0xffffffff,%ecx
            cld
            xorb %al,%al
            repne
            scasb
            not %ecx
            movb 12(%ebp),%al
            movl 8(%ebp),%edi
            addl %ecx,%edi
            decl %edi
            std
            repne
            scasb
            movl $0,%eax

            jnz .LSTRRSCAN
            movl %edi,%eax
            incl %eax
        .LSTRRSCAN:
            leave
            ret $6
         end;
      end;

    function strupper(p : pchar) : pchar;

      begin
         asm
            movl 8(%ebp),%esi
            movl %esi,%edi
         .LSTRUPPER1:
            lodsb
            cmpb $97,%al
            jb .LSTRUPPER3
            cmpb $122,%al
            ja .LSTRUPPER3
            subb $0x20,%al
         .LSTRUPPER3:
            stosb
            orb %al,%al
            jnz .LSTRUPPER1
            movl 8(%ebp),%eax
            leave
            ret $4
         end;
      end;

    function strlower(p : pchar) : pchar;

      begin
         asm
            movl 8(%ebp),%esi
            movl %esi,%edi
         .LSTRLOWER1:
            lodsb
            cmpb $65,%al
            jb .LSTRLOWER3
            cmpb $90,%al
            ja .LSTRLOWER3
            addb $0x20,%al
         .LSTRLOWER3:
            stosb
            orb %al,%al
            jnz .LSTRLOWER1
            movl 8(%ebp),%eax
            leave
            ret $4
         end;
      end;
{
  $Log$
  Revision 1.2  1999-02-25 10:07:01  michael
  + Added header and log

}