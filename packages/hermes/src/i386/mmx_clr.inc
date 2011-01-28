{
    MMX surface clear routines for HERMES
    Copyright (c) 1998 Christian Nentwich (c.nentwich@cs.ucl.ac.uk)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,and
    to copy and distribute the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module, the terms
    and conditions of the license of that module. An independent module is a
    module which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{$ASMMODE att}

{
 --------------------------------------------------------------------------
 HermesClearInterface (ebp+..)
   0: char8 *dest
   4: int32 value
   8: unsigned int width (already checked to be >0!)
  12: unsigned int height (already checked to be >0!)
  16: int add
}
procedure ClearMMX_32(hci: PHermesClearInterface); cdecl; assembler;
asm
        pushl %ebp
        movl 8(%ebp),%ebp

        movl 4(%ebp),%eax       // pixel value
        movd 4(%ebp),%mm0

        movl 12(%ebp),%edx      // height
        movq %mm0,%mm1

        psllq $32,%mm0
        movl (%ebp),%edi        // destination

        por %mm1,%mm0
.L_y:
        movl 8(%ebp),%ecx

        movl %ecx,%ebx

        shrl $1,%ecx
        jz .L_last

.L_x:
        movq %mm0,(%edi)
        addl $8,%edi

        decl %ecx
        jnz .L_x


.L_last:
        testl $1,%ebx
        jz .L_endline

        movl %eax,(%edi)
        addl $4,%edi

.L_endline:

        addl 16(%ebp),%edi

        decl %edx
        jnz .L_y

        emms
        popl %ebp
end;

procedure ClearMMX_24(hci: PHermesClearInterface); cdecl; assembler;
asm
end;

procedure ClearMMX_16(hci: PHermesClearInterface); cdecl; assembler;
asm
        pushl %ebp
        movl 8(%ebp),%ebp

        movl 4(%ebp),%eax       // pixel value
        movl 4(%ebp),%ebx

        movl 12(%ebp),%edx      // height
        movl (%ebp),%edi        // destination

        shll $16,%eax           // Duplicate pixel value
        andl $0x0ffff,%ebx

        orl %ebx,%eax

        movd %eax,%mm0
        movd %eax,%mm1

        psllq $32,%mm0

        por %mm1,%mm0
.L_y:
        movl 8(%ebp),%ecx

        testl $3,%edi           // Check if destination is aligned mod 4
        jz .L_aligned

        movw %ax,(%edi)         // otherwise write one pixel
        addl $2,%edi

        decl %ecx
        jz .L_endline

.L_aligned:
        movl %ecx,%ebx
        shrl $2,%ecx

        jz .L_last

.L_x:
        movq %mm0,(%edi)
        addl $8,%edi

        decl %ecx
        jnz .L_x

.L_last:
        andl $3,%ebx
        jz .L_endline

        movw %ax,(%edi)         // Write trailing pixels
        addl $2,%edi
        decl %ebx
        jz .L_endline

        movw %ax,(%edi)
        addl $2,%edi
        decl %ebx
        jz .L_endline

        movw %ax,(%edi)
        addl $2,%edi
        decl %ebx
        jnz .L_endline

.L_endline:
        addl 16(%ebp),%edi

        decl %edx
        jnz .L_y

        emms
        popl %ebp
end;


{
 Clear8_x86 isnt optimised fully yet as it seems to be a tiny bit slower
 than the C routine
}
procedure ClearMMX_8(hci: PHermesClearInterface); cdecl; assembler;
asm
        pushl %ebp
        movl 8(%ebp),%ebp

        movl 4(%ebp),%eax       // pixel value
        movl 4(%ebp),%ebx

        movl 12(%ebp),%edx      // height
        andl $0x0ff,%ebx

        shll $8,%eax            // Put the byte pixel value in all four bytes
        movl (%ebp),%edi        // destination

        movb %bl,%al
        movb %bl,%bh

        shll $16,%eax

        movb %bh,%ah
        movb %bl,%al

        movd %eax,%mm0
        movd %eax,%mm1

        psllq $32,%mm0

        por %mm1,%mm0

.L_y:
        movl 8(%ebp),%ecx

        testl $3,%edi           // Align mod 4
        jz .L_aligned

        movl %edi,%ebx

        andl $3,%ebx

        movb %al,(%edi)         // Unrolled (copy & paste), align and jump
        incl %edi               // if finished, faster than a loop...
        decl %ecx
        jz .L_endline
        decl %ebx
        jz .L_aligned

        movb %al,(%edi)         // Second pixel
        incl %edi
        decl %ecx
        jz .L_endline
        decl %ebx
        jz .L_aligned

        movb %al,(%edi)         // Third pixel
        incl %edi
        decl %ecx
        jz .L_endline
        decl %ebx
        jz .L_aligned

.L_aligned:
        movl %ecx,%ebx          // Store ecx for later

        shrl $3,%ecx            // We write 8 pixels at once
        jz .L_last

.L_x:
        movq %mm0,(%edi)
        addl $8,%edi

        decl %ecx
        jnz .L_x

.L_last:
        movl %ebx,%ecx          // Clean up trailing pixels

        andl $7,%ecx            // Could be up to 7 left
        jz .L_endline

        testb $0b100,%cl        // If theres less than four jump
        jz .L_lessthanfour

        movl %eax,(%edi)        // Otherwise write a dword
        addl $4,%edi

        subl $4,%ecx

.L_lessthanfour:
        rep
 stosb              // Clean up the very rest

.L_endline:
        addl 16(%ebp),%edi

        decl %edx
        jnz .L_y

        emms
        popl %ebp
end;
