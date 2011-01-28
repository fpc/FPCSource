{
    x86 format converters for HERMES
    Some routines Copyright (c) 1998 Christian Nentwich (c.nentwich@cs.ucl.ac.uk)
    Most routines are (c) Glenn Fiedler (ptc@gaffer.org), used with permission

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



{ _Convert_*
 Paramters:
   ESI = source
   EDI = dest
   ECX = amount (NOT 0!!! (the _ConvertX86 routine checks for that though))
 Destroys:
   EAX, EBX, EDX
}

procedure ConvertX86p32_32BGR888(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
    // check short
    cmpl $32,%ecx
    ja .L3

.L1:  // short loop
    movl (%esi),%edx
    bswapl %edx
    rorl $8,%edx
    movl %edx,(%edi)
    addl $4,%esi
    addl $4,%edi
    decl %ecx
    jnz .L1
.L2:
    jmp _X86RETURN

.L3:  // save ebp
    pushl %ebp

    // unroll four times
    movl %ecx,%ebp
    shrl $2,%ebp

    // save count
    pushl %ecx

.L4:    movl (%esi),%eax
        movl 4(%esi),%ebx

        bswapl %eax

        bswapl %ebx

        rorl $8,%eax
        movl 8(%esi),%ecx

        rorl $8,%ebx
        movl 12(%esi),%edx

        bswapl %ecx

        bswapl %edx

        rorl $8,%ecx
        movl %eax,(%edi)

        rorl $8,%edx
        movl %ebx,4(%edi)

        movl %ecx,8(%edi)
        movl %edx,12(%edi)

        addl $16,%esi
        addl $16,%edi

        decl %ebp
        jnz .L4

    // check tail
    popl %ecx
    andl $0b11,%ecx
    jz .L6

.L5:  // tail loop
    movl (%esi),%edx
    bswapl %edx
    rorl $8,%edx
    movl %edx,(%edi)
    addl $4,%esi
    addl $4,%edi
    decl %ecx
    jnz .L5

.L6: popl %ebp
    jmp _X86RETURN
end;

procedure ConvertX86p32_32RGBA888(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
    // check short
    cmpl $32,%ecx
    ja .L3

.L1:  // short loop
    movl (%esi),%edx
    roll $8,%edx
    movl %edx,(%edi)
    addl $4,%esi
    addl $4,%edi
    decl %ecx
    jnz .L1
.L2:
    jmp _X86RETURN

.L3:  // save ebp
    pushl %ebp

    // unroll four times
    movl %ecx,%ebp
    shrl $2,%ebp

    // save count
    pushl %ecx

.L4:    movl (%esi),%eax
        movl 4(%esi),%ebx

        roll $8,%eax
        movl 8(%esi),%ecx

        roll $8,%ebx
        movl 12(%esi),%edx

        roll $8,%ecx
        movl %eax,(%edi)

        roll $8,%edx
        movl %ebx,4(%edi)

        movl %ecx,8(%edi)
        movl %edx,12(%edi)

        addl $16,%esi
        addl $16,%edi

        decl %ebp
        jnz .L4

    // check tail
    popl %ecx
    andl $0b11,%ecx
    jz .L6

.L5:  // tail loop
    movl (%esi),%edx
    roll $8,%edx
    movl %edx,(%edi)
    addl $4,%esi
    addl $4,%edi
    decl %ecx
    jnz .L5

.L6: popl %ebp
    jmp _X86RETURN
end;


procedure ConvertX86p32_32BGRA888(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
    // check short
    cmpl $32,%ecx
    ja .L3

.L1:  // short loop
    movl (%esi),%edx
    bswapl %edx
    movl %edx,(%edi)
    addl $4,%esi
    addl $4,%edi
    decl %ecx
    jnz .L1
.L2:
    jmp _X86RETURN

.L3:  // save ebp
    pushl %ebp

    // unroll four times
    movl %ecx,%ebp
    shrl $2,%ebp

    // save count
    pushl %ecx

.L4:    movl (%esi),%eax
        movl 4(%esi),%ebx

        movl 8(%esi),%ecx
        movl 12(%esi),%edx

        bswapl %eax

        bswapl %ebx

        bswapl %ecx

        bswapl %edx

        movl %eax,(%edi)
        movl %ebx,4(%edi)

        movl %ecx,8(%edi)
        movl %edx,12(%edi)

        addl $16,%esi
        addl $16,%edi

        decl %ebp
        jnz .L4

    // check tail
    popl %ecx
    andl $0b11,%ecx
    jz .L6

.L5:  // tail loop
    movl (%esi),%edx
    bswapl %edx
    movl %edx,(%edi)
    addl $4,%esi
    addl $4,%edi
    decl %ecx
    jnz .L5

.L6: popl %ebp
    jmp _X86RETURN
end;



// 32 bit RGB 888 to 24 BIT RGB 888
procedure ConvertX86p32_24RGB888(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
        // check short
        cmpl $32,%ecx
        ja .L3

.L1:  // short loop
        movb (%esi),%al
        movb 1(%esi),%bl
        movb 2(%esi),%dl
        movb %al,(%edi)
        movb %bl,1(%edi)
        movb %dl,2(%edi)
        addl $4,%esi
        addl $3,%edi
        decl %ecx
        jnz .L1
.L2:
        jmp _X86RETURN

.L3:  //        head
        movl %edi,%edx
        andl $0b11,%edx
        jz .L4
        movb (%esi),%al
        movb 1(%esi),%bl
        movb 2(%esi),%dl
        movb %al,(%edi)
        movb %bl,1(%edi)
        movb %dl,2(%edi)
        addl $4,%esi
        addl $3,%edi
        decl %ecx
        jmp .L3

.L4:  // unroll 4 times
        pushl %ebp
        movl %ecx,%ebp
        shrl $2,%ebp

    // save count
        pushl %ecx

.L5:    movl (%esi),%eax                // first dword            eax = [A][R][G][B]
        movl 4(%esi),%ebx               // second dword           ebx = [a][r][g][b]

        shll $8,%eax                    //                        eax = [R][G][B][.]
        movl 12(%esi),%ecx              // third dword            ecx = [a][r][g][b]

        shll $8,%ebx                    //                        ebx = [r][g][b][.]
        movb 4(%esi),%al                //                        eax = [R][G][B][b]

        rorl $8,%eax                    //                        eax = [b][R][G][B] (done)
        movb 8+1(%esi),%bh              //                        ebx = [r][g][G][.]

        movl %eax,(%edi)
        addl $3*4,%edi

        shll $8,%ecx                    //                        ecx = [r][g][b][.]
        movb 8+0(%esi),%bl              //                        ebx = [r][g][G][B]

        roll $16,%ebx                   //                        ebx = [G][B][r][g] (done)
        movb 8+2(%esi),%cl              //                        ecx = [r][g][b][R] (done)

        movl %ebx,4-3*4(%edi)
        addl $4*4,%esi

        movl %ecx,8-3*4(%edi)
        decl %ebp

        jnz .L5

    // check tail
        popl %ecx
        andl $0b11,%ecx
        jz .L7

.L6:  // tail loop
        movb (%esi),%al
        movb 1(%esi),%bl
        movb 2(%esi),%dl
        movb %al,(%edi)
        movb %bl,1(%edi)
        movb %dl,2(%edi)
        addl $4,%esi
        addl $3,%edi
        decl %ecx
        jnz .L6

.L7:    popl %ebp
        jmp _X86RETURN
end;



// 32 bit RGB 888 to 24 bit BGR 888
procedure ConvertX86p32_24BGR888(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
        // check short
        cmpl $32,%ecx
        ja .L3


.L1:  // short loop
        movb (%esi),%dl
        movb 1(%esi),%bl
        movb 2(%esi),%al
        movb %al,(%edi)
        movb %bl,1(%edi)
        movb %dl,2(%edi)
        addl $4,%esi
        addl $3,%edi
        decl %ecx
        jnz .L1
.L2:
        jmp _X86RETURN

.L3:  // head
        movl %edi,%edx
        andl $0b11,%edx
        jz .L4
        movb (%esi),%dl
        movb 1(%esi),%bl
        movb 2(%esi),%al
        movb %al,(%edi)
        movb %bl,1(%edi)
        movb %dl,2(%edi)
        addl $4,%esi
        addl $3,%edi
        decl %ecx
        jmp .L3

.L4:  // unroll 4 times
        pushl %ebp
        movl %ecx,%ebp
        shrl $2,%ebp

        // save count
        pushl %ecx

.L5:
        movl (%esi),%eax                // first dword            eax = [A][R][G][B]
        movl 4(%esi),%ebx               // second dword           ebx = [a][r][g][b]

        bswapl %eax                     //                        eax = [B][G][R][A]

        bswapl %ebx                     //                        ebx = [b][g][r][a]

        movb 4+2(%esi),%al              //                        eax = [B][G][R][r]
        movb 4+4+1(%esi),%bh            //                        ebx = [b][g][G][a]

        rorl $8,%eax                    //                        eax = [r][B][G][R] (done)
        movb 4+4+2(%esi),%bl            //                        ebx = [b][g][G][R]

        rorl $16,%ebx                   //                        ebx = [G][R][b][g] (done)
        movl %eax,(%edi)

        movl %ebx,4(%edi)
        movl 12(%esi),%ecx              // third dword            ecx = [a][r][g][b]

        bswapl %ecx                     //                        ecx = [b][g][r][a]

        movb 8(%esi),%cl                //                        ecx = [b][g][r][B] (done)
        addl $4*4,%esi

        movl %ecx,8(%edi)
        addl $3*4,%edi

        decl %ebp
        jnz .L5

        // check tail
        popl %ecx
        andl $0b11,%ecx
        jz .L7

.L6:  // tail loop
        movb (%esi),%dl
        movb 1(%esi),%bl
        movb 2(%esi),%al
        movb %al,(%edi)
        movb %bl,1(%edi)
        movb %dl,2(%edi)
        addl $4,%esi
        addl $3,%edi
        decl %ecx
        jnz .L6

.L7:
        popl %ebp
        jmp _X86RETURN
end;



// 32 bit RGB 888 to 16 BIT RGB 565
procedure ConvertX86p32_16RGB565(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
        // check short
        cmpl $16,%ecx
        ja .L3

.L1:  // short loop
        movb (%esi),%bl   // blue
        movb 1(%esi),%al  // green
        movb 2(%esi),%ah  // red
        shrb $3,%ah
        andb $0b11111100,%al
        shll $3,%eax
        shrb $3,%bl
        addb %bl,%al
        movb %al,(%edi)
        movb %ah,1(%edi)
        addl $4,%esi
        addl $2,%edi
        decl %ecx
        jnz .L1

.L2:     // End of short loop
        jmp _X86RETURN


.L3:  // head
        movl %edi,%ebx
        andl $0b11,%ebx
        jz .L4

        movb (%esi),%bl   // blue
        movb 1(%esi),%al  // green
        movb 2(%esi),%ah  // red
        shrb $3,%ah
        andb $0b11111100,%al
        shll $3,%eax
        shrb $3,%bl
        addb %bl,%al
        movb %al,(%edi)
        movb %ah,1(%edi)
        addl $4,%esi
        addl $2,%edi
        decl %ecx

.L4:
    // save count
        pushl %ecx

    // unroll twice
        shrl $1,%ecx

    // point arrays to end
        leal (%esi,%ecx,8),%esi
        leal (%edi,%ecx,4),%edi

    // negative counter
        negl %ecx
        jmp .L6

.L5:
        movl %eax,-4(%edi,%ecx,4)
.balign 8
.L6:
        movl (%esi,%ecx,8),%eax

        shrb $2,%ah
        movl 4(%esi,%ecx,8),%ebx

        shrl $3,%eax
        movl 4(%esi,%ecx,8),%edx

        shrb $2,%bh
        movb 2(%esi,%ecx,8),%dl

        shll $13,%ebx
        andl $0x000007FF,%eax

        shll $8,%edx
        andl $0x07FF0000,%ebx

        andl $0x0F800F800,%edx
        addl %ebx,%eax

        addl %edx,%eax
        incl %ecx

        jnz .L5

        movl %eax,-4(%edi,%ecx,4)

    // tail
        popl %ecx
        testb $1,%cl
        jz .L7

        movb (%esi),%bl   // blue
        movb 1(%esi),%al  // green
        movb 2(%esi),%ah  // red
        shrb $3,%ah
        andb $0b11111100,%al
        shll $3,%eax
        shrb $3,%bl
        addb %bl,%al
        movb %al,(%edi)
        movb %ah,1(%edi)
        addl $4,%esi
        addl $2,%edi

.L7:
        jmp _X86RETURN
end;



// 32 bit RGB 888 to 16 BIT BGR 565

procedure ConvertX86p32_16BGR565(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
        // check short
        cmpl $16,%ecx
        ja .L3

.L1:  // short loop
        movb (%esi),%ah   // blue
        movb 1(%esi),%al  // green
        movb 2(%esi),%bl  // red
        shrb $3,%ah
        andb $0b11111100,%al
        shll $3,%eax
        shrb $3,%bl
        addb %bl,%al
        movb %al,(%edi)
        movb %ah,1(%edi)
        addl $4,%esi
        addl $2,%edi
        decl %ecx
        jnz .L1
.L2:
        jmp _X86RETURN

.L3:  // head
        movl %edi,%ebx
        andl $0b11,%ebx
        jz .L4
        movb (%esi),%ah   // blue
        movb 1(%esi),%al  // green
        movb 2(%esi),%bl  // red
        shrb $3,%ah
        andb $0b11111100,%al
        shll $3,%eax
        shrb $3,%bl
        addb %bl,%al
        movb %al,(%edi)
        movb %ah,1(%edi)
        addl $4,%esi
        addl $2,%edi
        decl %ecx

.L4:  // save count
        pushl %ecx

        // unroll twice
        shrl $1,%ecx

        // point arrays to end
        leal (%esi,%ecx,8),%esi
        leal (%edi,%ecx,4),%edi

        // negative count
        negl %ecx
        jmp .L6

.L5:
        movl %eax,-4(%edi,%ecx,4)
.L6:
        movl 4(%esi,%ecx,8),%edx

        movb 4(%esi,%ecx,8),%bh
        movb (%esi,%ecx,8),%ah

        shrb $3,%bh
        movb 1(%esi,%ecx,8),%al

        shrb $3,%ah
        movb 5(%esi,%ecx,8),%bl

        shll $3,%eax
        movb 2(%esi,%ecx,8),%dl

        shll $19,%ebx
        andl $0x0000FFE0,%eax

        shrl $3,%edx
        andl $0x0FFE00000,%ebx

        andl $0x001F001F,%edx
        addl %ebx,%eax

        addl %edx,%eax
        incl %ecx

        jnz .L5

        movl %eax,-4(%edi,%ecx,4)

        // tail
        popl %ecx
        andl $1,%ecx
        jz .L7
        movb (%esi),%ah   // blue
        movb 1(%esi),%al  // green
        movb 2(%esi),%bl  // red
        shrb $3,%ah
        andb $0b11111100,%al
        shll $3,%eax
        shrb $3,%bl
        addb %bl,%al
        movb %al,(%edi)
        movb %ah,1(%edi)
        addl $4,%esi
        addl $2,%edi

.L7:
        jmp _X86RETURN
end;



// 32 BIT RGB TO 16 BIT RGB 555
procedure ConvertX86p32_16RGB555(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
        // check short
        cmpl $16,%ecx
        ja .L3

.L1:  // short loop
        movb (%esi),%bl   // blue
        movb 1(%esi),%al  // green
        movb 2(%esi),%ah  // red
        shrb $3,%ah
        andb $0b11111000,%al
        shll $2,%eax
        shrb $3,%bl
        addb %bl,%al
        movb %al,(%edi)
        movb %ah,1(%edi)
        addl $4,%esi
        addl $2,%edi
        decl %ecx
        jnz .L1
.L2:
        jmp _X86RETURN

.L3:  // head
        movl %edi,%ebx
        andl $0b11,%ebx
        jz .L4
        movb (%esi),%bl   // blue
        movb 1(%esi),%al  // green
        movb 2(%esi),%ah  // red
        shrb $3,%ah
        andb $0b11111000,%al
        shll $2,%eax
        shrb $3,%bl
        addb %bl,%al
        movb %al,(%edi)
        movb %ah,1(%edi)
        addl $4,%esi
        addl $2,%edi
        decl %ecx

.L4:  // save count
        pushl %ecx

        // unroll twice
        shrl $1,%ecx

        // point arrays to end
        leal (%esi,%ecx,8),%esi
        leal (%edi,%ecx,4),%edi

        // negative counter
        negl %ecx
        jmp .L6

.L5:
        movl %eax,-4(%edi,%ecx,4)
.L6:
        movl (%esi,%ecx,8),%eax

        shrb $3,%ah
        movl 4(%esi,%ecx,8),%ebx

        shrl $3,%eax
        movl 4(%esi,%ecx,8),%edx

        shrb $3,%bh
        movb 2(%esi,%ecx,8),%dl

        shll $13,%ebx
        andl $0x000007FF,%eax

        shll $7,%edx
        andl $0x07FF0000,%ebx

        andl $0x07C007C00,%edx
        addl %ebx,%eax

        addl %edx,%eax
        incl %ecx

        jnz .L5

        movl %eax,-4(%edi,%ecx,4)

        // tail
        popl %ecx
        andl $1,%ecx
        jz .L7
        movb (%esi),%bl   // blue
        movb 1(%esi),%al  // green
        movb 2(%esi),%ah  // red
        shrb $3,%ah
        andb $0b11111000,%al
        shll $2,%eax
        shrb $3,%bl
        addb %bl,%al
        movb %al,(%edi)
        movb %ah,1(%edi)
        addl $4,%esi
        addl $2,%edi

.L7:
        jmp _X86RETURN
end;



// 32 BIT RGB TO 16 BIT BGR 555
procedure ConvertX86p32_16BGR555(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
        // check short
        cmpl $16,%ecx
        ja .L3


.L1:  // short loop
        movb (%esi),%ah   // blue
        movb 1(%esi),%al  // green
        movb 2(%esi),%bl  // red
        shrb $3,%ah
        andb $0b11111000,%al
        shll $2,%eax
        shrb $3,%bl
        addb %bl,%al
        movb %al,(%edi)
        movb %ah,1(%edi)
        addl $4,%esi
        addl $2,%edi
        decl %ecx
        jnz .L1
.L2:
        jmp _X86RETURN

.L3:  // head
        movl %edi,%ebx
        andl $0b11,%ebx
        jz .L4
        movb (%esi),%ah   // blue
        movb 1(%esi),%al  // green
        movb 2(%esi),%bl  // red
        shrb $3,%ah
        andb $0b11111000,%al
        shll $2,%eax
        shrb $3,%bl
        addb %bl,%al
        movb %al,(%edi)
        movb %ah,1(%edi)
        addl $4,%esi
        addl $2,%edi
        decl %ecx

.L4:  // save count
        pushl %ecx

        // unroll twice
        shrl $1,%ecx

        // point arrays to end
        leal (%esi,%ecx,8),%esi
        leal (%edi,%ecx,4),%edi

        // negative counter
        negl %ecx
        jmp .L6

.L5:
        movl %eax,-4(%edi,%ecx,4)
.L6:
        movl 4(%esi,%ecx,8),%edx

        movb 4(%esi,%ecx,8),%bh
        movb (%esi,%ecx,8),%ah

        shrb $3,%bh
        movb 1(%esi,%ecx,8),%al

        shrb $3,%ah
        movb 5(%esi,%ecx,8),%bl

        shll $2,%eax
        movb 2(%esi,%ecx,8),%dl

        shll $18,%ebx
        andl $0x00007FE0,%eax

        shrl $3,%edx
        andl $0x07FE00000,%ebx

        andl $0x001F001F,%edx
        addl %ebx,%eax

        addl %edx,%eax
        incl %ecx

        jnz .L5

        movl %eax,-4(%edi,%ecx,4)

        // tail
        popl %ecx
        andl $1,%ecx
        jz .L7
        movb (%esi),%ah   // blue
        movb 1(%esi),%al  // green
        movb 2(%esi),%bl  // red
        shrb $3,%ah
        andb $0b11111000,%al
        shll $2,%eax
        shrb $3,%bl
        addb %bl,%al
        movb %al,(%edi)
        movb %ah,1(%edi)
        addl $4,%esi
        addl $2,%edi

.L7:
        jmp _X86RETURN
end;




// FROM 32 BIT RGB to 8 BIT RGB (rrrgggbbb)
// This routine writes FOUR pixels at once (dword) and then, if they exist
// the trailing three pixels
procedure ConvertX86p32_8RGB332(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
.L_ALIGNED:
        pushl %ecx

        shrl $2,%ecx            // We will draw 4 pixels at once
        jnz .L1

        jmp .L2 // short jump out of range :(

.L1:
        movl (%esi),%eax        // first pair of pixels
        movl 4(%esi),%edx

        shrb $6,%dl
        movl %eax,%ebx

        shrb $6,%al
        andb $0x0e0,%ah

        shrl $16,%ebx
        andb $0x0e0,%dh

        shrb $3,%ah
        andb $0x0e0,%bl

        shrb $3,%dh

        orb %bl,%al

        movl %edx,%ebx
        orb %ah,%al

        shrl $16,%ebx
        orb %dh,%dl

        andb $0x0e0,%bl

        orb %bl,%dl

        movb %dl,%ah



        movl 8(%esi),%ebx       // second pair of pixels

        movl %ebx,%edx
        andb $0x0e0,%bh

        shrb $6,%bl
        andl $0x0e00000,%edx

        shrl $16,%edx

        shrb $3,%bh

        rorl $16,%eax
        orb %dl,%bl

        movl 12(%esi),%edx
        orb %bh,%bl

        movb %bl,%al

        movl %edx,%ebx
        andb $0x0e0,%dh

        shrb $6,%dl
        andl $0x0e00000,%ebx

        shrb $3,%dh
        movb %dl,%ah

        shrl $16,%ebx
        orb %dh,%ah

        orb %bl,%ah

        roll $16,%eax
        addl $16,%esi

        movl %eax,(%edi)
        addl $4,%edi

        decl %ecx
        jz .L2 // L1 out of range for short jump :(

        jmp .L1
.L2:

        popl %ecx
        andl $3,%ecx            // mask out number of pixels to draw

        jz .L4 // Nothing to do anymore

.L3:
        movl (%esi),%eax        // single pixel conversion for trailing pixels

        movl %eax,%ebx

        shrb $6,%al
        andb $0x0e0,%ah

        shrl $16,%ebx

        shrb $3,%ah
        andb $0x0e0,%bl

        orb %ah,%al
        orb %bl,%al

        movb %al,(%edi)

        incl %edi
        addl $4,%esi

        decl %ecx
        jnz .L3

.L4:
        jmp _X86RETURN
end;
