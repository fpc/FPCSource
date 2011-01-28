{
    x86 surface clear routines for HERMES
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

    (04/10/99)    Modified ClearX86_8             <Mikko.Tiihonen@hut.fi>
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
procedure ClearX86_32(hci: PHermesClearInterface); cdecl; assembler;
asm
        pushl %ebp

        movl 8(%ebp),%ebp

        movl (%ebp),%edi        // destination
        movl 4(%ebp),%eax       // pixel value

        movl 12(%ebp),%edx      // height
.balign 4
.L_y:
        movl 8(%ebp),%ecx
        rep
 stosl

        addl 16(%ebp),%edi

        decl %edx
        jnz .L_y

        popl %ebp
end;

procedure ClearX86_24(hci: PHermesClearInterface); cdecl; assembler;
asm
end;

procedure ClearX86_16(hci: PHermesClearInterface); cdecl; assembler;
asm
        pushl %ebp

        movl 8(%ebp),%ebp

        movl (%ebp),%edi        // destination
        movl 4(%ebp),%eax       // pixel value

        movl 12(%ebp),%edx      // height
        movl %eax,%ebx

        shll $16,%eax           // Duplicate pixel value
        andl $0x0ffff,%ebx

        orl %ebx,%eax
.L_y:
        movl 8(%ebp),%ecx

        testl $3,%edi           // Check if destination is aligned mod 4
        jz .L_aligned

        movw %ax,(%edi)         // otherwise write one pixel
        addl $2,%edi

        decl %ecx
        jz .L_endline

.L_aligned:
        shrl $1,%ecx

rep
 stosl

        jnc .L_endline

        movw %ax,(%edi)
        addl $2,%edi

.L_endline:
        addl 16(%ebp),%edi

        decl %edx
        jnz .L_y

        popl %ebp
end;

procedure ClearX86_8(hci: PHermesClearInterface); cdecl; nostackframe; assembler;
asm
        pushl %ebp
        movl %esp,%ebp

        movl 8(%ebp),%ebp

        movl 4(%ebp),%eax       // pixel value
        movl 12(%ebp),%edx      // height

        movb %al,%ah
        movl (%ebp),%edi        // destination

        movl %eax,%ecx

        shll $16,%eax           // Put the byte pixel value in all four bytes
        andl $0x0ffff,%ecx      // of eax

        movl 8(%ebp),%ebx
        orl %ecx,%eax

        cmpl $5,%ebx            // removes need for extra checks later
        jbe .L_short_y

.balign 4
.L_y:
        testl $3,%edi
        jz .L_aligned

        movl %edi,%ecx
        negl %ecx
        andl $3,%ecx

        subl %ecx,%ebx

        rep
 stosb

.L_aligned:
        movl %ebx,%ecx

        shrl $2,%ecx
        andl $3,%ebx

        rep
 stosl

        movl %ebx,%ecx
        rep
 stosb

        addl 16(%ebp),%edi

        decl %edx
        movl 8(%ebp),%ebx
        jnz .L_y

        popl %ebp
        ret

// Short loop
.balign 4
.L_short_y:
        movl %ebx,%ecx

        rep
 stosb
        addl 16(%ebp),%edi

        decl %edx
        jnz .L_short_y

        popl %ebp
end;

{
 ClearX86_8 version 2,
 Im not sure wheather this is faster or not...
 too many jumps could confuse the CPU branch guessing
}
procedure ClearX86_8_2(hci: PHermesClearInterface); cdecl; nostackframe; assembler;
asm
        pushl %ebp
        movl %esp,%ebp

        movl 8(%ebp),%ebp

        movl 4(%ebp),%eax       // pixel value
        movl 12(%ebp),%edx      // height

        movb %al,%ah
        movl (%ebp),%edi        // destination

        movl %eax,%ecx

        shll $16,%eax           // Put the byte pixel value in all four bytes
        andl $0x0ffff,%ecx      // of eax

        movl 8(%ebp),%ebx
        orl %ecx,%eax

        cmpl $5,%ebx            // removes need for extra checks in main loop
        jbe .L_short_y


.balign 4
.L_y:
        testl $3,%edi
        jz .L_aligned

        movl %edi,%ecx
        negl %ecx
        andl $3,%ecx

        movb %al,(%edi)
        subl %ecx,%ebx

        incl %edi

        decl %ecx
        jz .L_aligned

        movb %al,(%edi)
        incl %edi
        decl %ecx
        jz .L_aligned

        movb %al,(%edi)
        incl %edi

.L_aligned:
        movl %ebx,%ecx

        shrl $2,%ecx
        andl $3,%ebx

        rep
 stosl

        jz .L_endline
                // ebx

        movb %al,(%edi)
                // Write remaining (1,2 or 3) pixels
        incl %edi
        decl %ebx
        jz .L_endline

        movb %al,(%edi)
        incl %edi
        decl %ebx
        jz .L_endline

        movb %al,(%edi)
        incl %edi
        decl %ebx
        jz .L_endline

        movb %al,(%edi)
        incl %edi

.L_endline:
        addl 16(%ebp),%edi

        decl %edx
        movl 8(%ebp),%ebx
        jnz .L_y

        popl %ebp
        ret

// Short loop
.balign 4
.L_short_y:
        movl %ebx,%ecx

        rep
 stosb
        addl 16(%ebp),%edi

        decl %edx
        jnz .L_short_y

        popl %ebp
end;
