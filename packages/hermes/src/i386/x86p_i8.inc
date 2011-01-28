{
    x86 format converters for HERMES
    Copyright (c) 1998 Christian Nentwich (c.nentwich@cs.ucl.ac.uk)
    Some routines are (c) Glenn Fiedler (ptc@gaffer.org), used with permission

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



{ Convert_*
 Paramters:
   ESI = source
   EDI = dest
   ECX = amount (NOT 0!!! (the ConvertX86 routine checks for that though))
 Destroys:
   EAX, EBX, EDX
}

procedure ConvertX86pI8_32(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
        xorl %ebx,%ebx
        movl 36(%ebp),%edx
.L1:
        movb (%esi),%bl
        incl %esi

        movl (%edx,%ebx,4),%eax

        movl %eax,(%edi)
        addl $4,%edi

        decl %ecx
        jnz .L1

        jmp _X86RETURN
end;


procedure ConvertX86pI8_24(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
        movl 36(%ebp),%ebx

        xorl %edx,%edx

        // check short
        cmpl $32,%ecx
        ja .L3


.L1:  // short loop
        movb (%esi),%dl
        movl (%ebx,%edx,4),%eax
        movb %al,(%edi)  // blue
        movb %ah,1(%edi) // green
        shrl $16,%eax
        movb %al,2(%edi) // red
        incl %esi
        addl $3,%edi
        decl %ecx
        jnz .L1
.L2:
        jmp _X86RETURN

.L3:  // head
        movl %edi,%eax
        andl $0b11,%eax
        jz .L4
        movb (%esi),%dl
        movl (%ebx,%edx,4),%eax
        movb %al,(%edi)  // blue
        movb %ah,1(%edi) // green
        shrl $16,%eax
        movb %al,2(%edi) // red
        incl %esi
        addl $3,%edi
        decl %ecx
        jmp .L3

.L4:    // save ebp
        pushl %ebp
        movl %ebx,%ebp

        // save count
        pushl %ecx

        // unroll 4 times
        shrl $2,%ecx

.L5:    pushl %ecx                      // save ecx
        movb (%esi),%dl                 // index to "A"

        movl (%ebp,%edx,4),%eax         // eax = [xx][A2][A1][A0]
        shll $8,%eax                    // eax = [A2][A1][A0][xx]

        movb 1(%esi),%dl                // index to "B"

        movb (%ebp,%edx,4),%al          // eax = [A2][A1][A0][B0]
        rorl $8,%eax                    // eax = [B0][A2][A1][A0] (done)
        movl %eax,(%edi)

        movl (%ebp,%edx,4),%eax         // eax = [xx][B2][B1][B0]
        shll $8,%eax                    // eax = [B2][B1][B0][xx]

        movb 3(%esi),%dl                // index to "D"

        movl (%ebp,%edx,4),%ecx         // ecx = [xx][D2][D1][D0]
        shll $8,%ecx                    // ecx = [D2][D1][D0][xx]

        movb 2(%esi),%dl                // index to "C"

        movb 1(%ebp,%edx,4),%ah         // eax = [B2][B1][C1][xx]
        movb (%ebp,%edx,4),%al          // eax = [B2][B1][C1][C0]
        rorl $16,%eax                   // eax = [C1][C0][B2][B1] (done)

        movb 2(%ebp,%edx,4),%cl         // ecx = [D2][D1][D0][C2] (done)

        movl %eax,4(%edi)
        movl %ecx,8(%edi)

        addl $4,%esi
        addl $3*4,%edi

        popl %ecx                       // restore ecx

        decl %ecx
        jnz .L5

        // tail
        popl %ecx
        andl $0b11,%ecx
        jz .L7

.L6:
        movb (%esi),%dl
        movl (%ebx,%edx,4),%eax
        movb %al,(%edi)  // blue
        movb %ah,1(%edi) // green
        shrl $16,%eax
        movb %al,2(%edi) // red
        incl %esi
        addl $3,%edi
        decl %ecx
        jnz .L6

.L7:    popl %ebp
        jmp _X86RETURN
end;

procedure ConvertX86pI8_16(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
        xorl %ebx,%ebx
        movl 36(%ebp),%edx

        testl $3,%edi
        jz .Laligned

        movb (%esi),%bl

        movl (%edx,%ebx,4),%eax
        incl %esi

        movw %ax,(%edi)
        addl $2,%edi

        decl %ecx
        jz .LOut

.Laligned:
        pushl %ecx

        xorl %eax,%eax
        xorl %ebx,%ebx

        shrl $1,%ecx
        jz .Last_pixel
.balign 8
.Ly:
        movb 1(%esi),%bl
        movb (%esi),%al

        movl (%edx,%ebx,4),%ebx
        addl $2,%esi

        shll $16,%ebx
        movl (%edx,%eax,4),%eax

        orl %ebx,%eax
        xorl %ebx,%ebx

        movl %eax,(%edi)
        addl $4,%edi

        xorl %eax,%eax
        decl %ecx
        jnz .Ly

.Last_pixel:
        popl %ecx

        testb $1,%cl
        jz .LOut

        movb (%esi),%bl

        movl (%edx,%ebx,4),%eax
        incl %esi

        movw %ax,(%edi)
        addl $2,%edi

.LOut:
        jmp _X86RETURN
end;
