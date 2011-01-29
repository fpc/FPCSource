{
    x86 format converters for HERMES
    Copyright (c) 1998 Glenn Fielder (gaffer@gaffer.org)
    Routines adjusted for Hermes by Christian Nentwich (c.nentwich@cs.ucl.ac.uk)
    Used with permission.

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


procedure ConvertX86p16_32RGB888(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
    // check short
    cmpl $32,%ecx
    ja .L3


    // short loop
    xorl %ebx,%ebx
.L1: movb (%esi),%bl              // ebx = lower byte pixel 1
    movl ConvertX86p16_32RGB888_LUT_X86(,%ebx,8),%eax   // eax = ARGB8888 of lower byte pixel 1
    movb 1(%esi),%bl                                     // ebx = upper byte pixel 1
    movl ConvertX86p16_32RGB888_LUT_X86+4(,%ebx,8),%edx // edx = ARGB8888 of upper byte pixel 1
    addl %edx,%eax
    movl %eax,(%edi)
    addl $2,%esi
    addl $4,%edi
    decl %ecx
    jnz .L1
.L2:
    jmp _X86RETURN


.L3:  // save ebp
    pushl %ebp

    // save count
    pushl %ecx

    // unroll twice
    movl %ecx,%ebp
    shrl $1,%ebp

    // point arrays to end
    leal (%esi,%ebp,4),%esi
    leal (%edi,%ebp,8),%edi

    // negative counter
    negl %ebp

    // clear
    xorl %ebx,%ebx
    xorl %ecx,%ecx

    // prestep
    movb (%esi,%ebp,4),%cl
    movb 1(%esi,%ebp,4),%bl

.L4:    movl ConvertX86p16_32RGB888_LUT_X86(,%ecx,8),%edx
        movb 2(%esi,%ebp,4),%cl

        movl ConvertX86p16_32RGB888_LUT_X86+4(,%ebx,8),%eax
        movb 3(%esi,%ebp,4),%bl

        addl %edx,%eax
        movl ConvertX86p16_32RGB888_LUT_X86(,%ecx,8),%edx

        movl %eax,(%edi,%ebp,8)
        movl ConvertX86p16_32RGB888_LUT_X86+4(,%ebx,8),%eax

        addl %edx,%eax
        movb 4(%esi,%ebp,4),%cl

        movl %eax,4(%edi,%ebp,8)
        movb 5(%esi,%ebp,4),%bl

        incl %ebp
        jnz .L4

    // tail
    popl %ecx
    andl $1,%ecx
    jz .L6
    xorl %ebx,%ebx
    movb (%esi),%bl                                      // ebx = lower byte pixel 1
    movl ConvertX86p16_32RGB888_LUT_X86(,%ebx,8),%eax   // eax = ARGB8888 of lower byte pixel 1
    movb 1(%esi),%bl                                     // ebx = upper byte pixel 1
    movl ConvertX86p16_32RGB888_LUT_X86+4(,%ebx,8),%edx // edx = ARGB8888 of upper byte pixel 1
    addl %edx,%eax
    movl %eax,(%edi)
    addl $2,%esi
    addl $4,%edi

.L6: popl %ebp
    jmp _X86RETURN
end;





procedure ConvertX86p16_32BGR888(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
    // check short
    cmpl $32,%ecx
    ja .L3


    // short loop
    xorl %ebx,%ebx
.L1: movb (%esi),%bl                                     // ebx = lower byte pixel 1
    movl ConvertX86p16_32BGR888_LUT_X86(,%ebx,8),%eax    // eax = ABGR8888 of lower byte pixel 1
    movb 1(%esi),%bl                                     // ebx = upper byte pixel 1
    movl ConvertX86p16_32BGR888_LUT_X86+4(,%ebx,8),%edx  // edx = ABGR8888 of upper byte pixel 1
    addl %edx,%eax
    movl %eax,(%edi)
    addl $2,%esi
    addl $4,%edi
    decl %ecx
    jnz .L1
.L2:
    jmp _X86RETURN

.L3:  // save ebp
    pushl %ebp

    // save count
    pushl %ecx

    // unroll twice
    movl %ecx,%ebp
    shrl $1,%ebp

    // point arrays to end
    leal (%esi,%ebp,4),%esi
    leal (%edi,%ebp,8),%edi

    // negative counter
    negl %ebp

    // clear
    xorl %ebx,%ebx
    xorl %ecx,%ecx

    // prestep
    movb (%esi,%ebp,4),%cl
    movb 1(%esi,%ebp,4),%bl

.L4:    movl ConvertX86p16_32BGR888_LUT_X86(,%ecx,8),%edx
        movb 2(%esi,%ebp,4),%cl

        movl ConvertX86p16_32BGR888_LUT_X86+4(,%ebx,8),%eax
        movb 3(%esi,%ebp,4),%bl

        addl %edx,%eax
        movl ConvertX86p16_32BGR888_LUT_X86(,%ecx,8),%edx

        movl %eax,(%edi,%ebp,8)
        movl ConvertX86p16_32BGR888_LUT_X86+4(,%ebx,8),%eax

        addl %edx,%eax
        movb 4(%esi,%ebp,4),%cl

        movl %eax,4(%edi,%ebp,8)
        movb 5(%esi,%ebp,4),%bl

        incl %ebp
        jnz .L4

    // tail
    popl %ecx
    andl $1,%ecx
    jz .L6
    xorl %ebx,%ebx
    movb (%esi),%bl                                      // ebx = lower byte pixel 1
    movl ConvertX86p16_32BGR888_LUT_X86(,%ebx,8),%eax    // eax = ABGR8888 of lower byte pixel 1
    movb 1(%esi),%bl                                     // ebx = upper byte pixel 1
    movl ConvertX86p16_32BGR888_LUT_X86+4(,%ebx,8),%edx  // edx = ABGR8888 of upper byte pixel 1
    addl %edx,%eax
    movl %eax,(%edi)
    addl $2,%esi
    addl $4,%edi

.L6: popl %ebp
    jmp _X86RETURN
end;



procedure ConvertX86p16_32RGBA888(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
    // check short
    cmpl $32,%ecx
    ja .L3


    // short loop
    xorl %ebx,%ebx
.L1: movb (%esi),%bl                                      // ebx = lower byte pixel 1
    movl ConvertX86p16_32RGBA888_LUT_X86(,%ebx,8),%eax   // eax = RGBA8888 of lower byte pixel 1
    movb 1(%esi),%bl                                      // ebx = upper byte pixel 1
    movl ConvertX86p16_32RGBA888_LUT_X86+4(,%ebx,8),%edx // edx = RGBA8888 of upper byte pixel 1
    addl %edx,%eax
    movl %eax,(%edi)
    addl $2,%esi
    addl $4,%edi
    decl %ecx
    jnz .L1
.L2:
    jmp _X86RETURN

.L3:  // save ebp
    pushl %ebp

    // save count
    pushl %ecx

    // unroll twice
    movl %ecx,%ebp
    shrl $1,%ebp

    // point arrays to end
    leal (%esi,%ebp,4),%esi
    leal (%edi,%ebp,8),%edi

    // negative counter
    negl %ebp

    // clear
    xorl %ebx,%ebx
    xorl %ecx,%ecx

    // prestep
    movb (%esi,%ebp,4),%cl
    movb 1(%esi,%ebp,4),%bl

.L4:    movl ConvertX86p16_32RGBA888_LUT_X86(,%ecx,8),%edx
        movb 2(%esi,%ebp,4),%cl

        movl ConvertX86p16_32RGBA888_LUT_X86+4(,%ebx,8),%eax
        movb 3(%esi,%ebp,4),%bl

        addl %edx,%eax
        movl ConvertX86p16_32RGBA888_LUT_X86(,%ecx,8),%edx

        movl %eax,(%edi,%ebp,8)
        movl ConvertX86p16_32RGBA888_LUT_X86+4(,%ebx,8),%eax

        addl %edx,%eax
        movb 4(%esi,%ebp,4),%cl

        movl %eax,4(%edi,%ebp,8)
        movb 5(%esi,%ebp,4),%bl

        incl %ebp
        jnz .L4

    // tail
    popl %ecx
    andl $1,%ecx
    jz .L6
    xorl %ebx,%ebx
    movb (%esi),%bl                                       // ebx = lower byte pixel 1
    movl ConvertX86p16_32RGBA888_LUT_X86(,%ebx,8),%eax   // eax = RGBA8888 of lower byte pixel 1
    movb 1(%esi),%bl                                      // ebx = upper byte pixel 1
    movl ConvertX86p16_32RGBA888_LUT_X86+4(,%ebx,8),%edx // edx = RGBA8888 of upper byte pixel 1
    addl %edx,%eax
    movl %eax,(%edi)
    addl $2,%esi
    addl $4,%edi

.L6: popl %ebp
    jmp _X86RETURN
end;




procedure ConvertX86p16_32BGRA888(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
    // check short
    cmpl $32,%ecx
    ja .L3

    // short loop
    xorl %ebx,%ebx
.L1: movb (%esi),%bl                                      // ebx = lower byte pixel 1
    movl ConvertX86p16_32BGRA888_LUT_X86(,%ebx,8),%eax    // eax = BGRA8888 of lower byte pixel 1
    movb 1(%esi),%bl                                      // ebx = upper byte pixel 1
    movl ConvertX86p16_32BGRA888_LUT_X86+4(,%ebx,8),%edx  // edx = BGRA8888 of upper byte pixel 1
    addl %edx,%eax
    movl %eax,(%edi)
    addl $2,%esi
    addl $4,%edi
    decl %ecx
    jnz .L1
.L2:
    jmp _X86RETURN

.L3:  // save ebp
    pushl %ebp

    // save count
    pushl %ecx

    // unroll twice
    movl %ecx,%ebp
    shrl $1,%ebp

    // point arrays to end
    leal (%esi,%ebp,4),%esi
    leal (%edi,%ebp,8),%edi

    // negative counter
    negl %ebp

    // clear
    xorl %ebx,%ebx
    xorl %ecx,%ecx

    // prestep
    movb (%esi,%ebp,4),%cl
    movb 1(%esi,%ebp,4),%bl

.L4:    movl ConvertX86p16_32BGRA888_LUT_X86(,%ecx,8),%edx
        movb 2(%esi,%ebp,4),%cl

        movl ConvertX86p16_32BGRA888_LUT_X86+4(,%ebx,8),%eax
        movb 3(%esi,%ebp,4),%bl

        addl %edx,%eax
        movl ConvertX86p16_32BGRA888_LUT_X86(,%ecx,8),%edx

        movl %eax,(%edi,%ebp,8)
        movl ConvertX86p16_32BGRA888_LUT_X86+4(,%ebx,8),%eax

        addl %edx,%eax
        movb 4(%esi,%ebp,4),%cl

        movl %eax,4(%edi,%ebp,8)
        movb 5(%esi,%ebp,4),%bl

        incl %ebp
        jnz .L4

    // tail
    popl %ecx
    andl $1,%ecx
    jz .L6
    xorl %ebx,%ebx
    movb (%esi),%bl                                       // ebx = lower byte pixel 1
    movl ConvertX86p16_32BGRA888_LUT_X86(,%ebx,8),%eax   // eax = BGRA8888 of lower byte pixel 1
    movb 1(%esi),%bl                                      // ebx = upper byte pixel 1
    movl ConvertX86p16_32BGRA888_LUT_X86+4(,%ebx,8),%edx // edx = BGRA8888 of upper byte pixel 1
    addl %edx,%eax
    movl %eax,(%edi)
    addl $2,%esi
    addl $4,%edi

.L6: popl %ebp
    jmp _X86RETURN
end;




procedure ConvertX86p16_24RGB888(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
    // check short
    cmpl $32,%ecx
    ja .L3


    // short loop
    xorl %edx,%edx
.L1: movb (%esi),%dl
    movl ConvertX86p16_32RGB888_LUT_X86(,%edx,8),%eax     // eax = ARGB8888 of lower byte
    movb 1(%esi),%dl
    movl ConvertX86p16_32RGB888_LUT_X86+4(,%edx,8),%ebx   // ebx = ARGB8888 of upper byte
    addl %ebx,%eax                                        // eax = ARGB8888 pixel
    movb %al,(%edi)
    movb %ah,1(%edi)
    shrl $16,%eax
    movb %al,2(%edi)
    addl $2,%esi
    addl $3,%edi
    decl %ecx
    jnz .L1
.L2: jmp _X86RETURN


.L3:  // clear edx
    xorl %edx,%edx

.L4:  // head
    movl %edi,%eax
    andl $0b11,%eax
    jz .L5
    movb (%esi),%dl
    movl ConvertX86p16_32RGB888_LUT_X86(,%edx,8),%eax     // eax = ARGB8888 of lower byte
    movb 1(%esi),%dl
    movl ConvertX86p16_32RGB888_LUT_X86+4(,%edx,8),%ebx   // ebx = ARGB8888 of upper byte
    addl %ebx,%eax                                        // eax = ARGB8888 pixel
    movb %al,(%edi)
    movb %ah,1(%edi)
    shrl $16,%eax
    movb %al,2(%edi)
    addl $2,%esi
    addl $3,%edi
    decl %ecx
    jmp .L4

.L5:  // unroll 4 times
    pushl %ebp
    movl %ecx,%ebp
    shrl $2,%ebp

    // clear ebx
    xorl %ebx,%ebx

    // save count
    pushl %ecx

    // prestep
    movb (%esi),%bl                                     // ebx = lower byte pixel 1
    movb 1(%esi),%dl                                    // edx = upper byte pixel 1

.L6:    movl ConvertX86p16_32RGB888_LUT_X86(,%ebx,8),%eax   // eax = ARGB8888 of lower byte pixel 1
        movb 2(%esi),%bl                                    // ebx = lower byte pixel 2

        movl ConvertX86p16_32RGB888_LUT_X86+4(,%edx,8),%ecx    // ecx = ARGB8888 of upper byte pixel 1
        movb 3(%esi),%dl                                    // edx = upper byte pixel 2

        pushl %ebp                                          // save ebp
        addl %ecx,%eax                                      // eax = ARGB8888 of pixel 1

        movl ConvertX86p16_32RGB888_LUT_X86(,%ebx,8),%ebp      // ebp = ARGB8888 of lower byte pixel 2
        movl ConvertX86p16_32RGB888_LUT_X86+4(,%edx,8),%ecx    // ecx = ARGB8888 of upper byte pixel 2

        movb 4(%esi),%bl                                    // ebx = lower byte pixel 3
        addl %ebp,%ecx                                      // ecx = ARGB8888 of pixel 2

        shll $24,%ebp                                       // ebp = [b][0][0][0] of pixel 2
        movb 5(%esi),%dl                                    // edx = upper byte pixel 3

        shrl $8,%ecx                                        // ecx = [0][0][r][g] pixel 2
        addl %ebp,%eax                                      // eax = [b2][r1][g1][b1] (done)

        movl %eax,(%edi)                                    // store dword 1
        movl ConvertX86p16_32RGB888_LUT_X86+4(,%edx,8),%eax    // eax = ARGB8888 of upper byte pixel 3

        movl ConvertX86p16_32RGB888_LUT_X86(,%ebx,8),%ebp      // ebp = ARGB8888 of lower byte pixel 3
        movb 6(%esi),%bl                                    // ebx = lower byte pixel 4

        addl %eax,%ebp                                      // ebp = ARGB8888 of pixel 3
        movb 7(%esi),%dl                                    // edx = upper byte pixel 4

        shll $16,%ebp                                       // ebp = [g][b][0][0] pixel 3

        shrl $16,%eax                                       //  al = red component of pixel 3
        addl %ecx,%ebp                                      // ebp = [g3][b3][r2][g2] (done)

        movl %ebp,4(%edi)                                   // store dword 2
        movl ConvertX86p16_32RGB888_LUT_X86(,%ebx,8),%ecx      // ebx = ARGB8888 of lower byte pixel 4

        movl ConvertX86p16_32RGB888_LUT_X86+4(,%edx,8),%ebp    // ebp = ARGB8888 of upper byte pixel 4
        movb 4*2+0(%esi),%bl                                // ebx = lower byte pixel 1

        addl %ebp,%ecx                                      // ecx = ARGB8888 of pixel 4
        movb 4*2+1(%esi),%dl                                // edx = upper byte pixel 1

        shll $8,%ecx                                        // ecx = [r][g][b][0]
        popl %ebp                                           // restore ebp

        movb %al,%cl                                        // ecx = [r4][g4][b4][r3] (done)
        addl $4*2,%esi

        movl %ecx,8(%edi)                                   // store dword 3
        addl $3*4,%edi

        decl %ebp
        jz .L7

        jmp .L6

.L7:  // check tail
    popl %ecx
    andl $0b11,%ecx
    jz .L9

.L8:  // tail
    movb (%esi),%dl
    movl ConvertX86p16_32RGB888_LUT_X86(,%edx,8),%eax    // eax = ARGB8888 of lower byte
    movb 1(%esi),%dl
    movl ConvertX86p16_32RGB888_LUT_X86+4(,%edx,8),%ebx  // ebx = ARGB8888 of upper byte
    addl %ebx,%eax                                    // eax = ARGB8888 pixel
    movb %al,(%edi)
    movb %ah,1(%edi)
    shrl $16,%eax
    movb %al,2(%edi)
    addl $2,%esi
    addl $3,%edi
    decl %ecx
    jnz .L8

.L9: popl %ebp
    jmp _X86RETURN
end;




procedure ConvertX86p16_24BGR888(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
    // check short
    cmpl $32,%ecx
    ja .L3


    // short loop
    xorl %edx,%edx
.L1: movb (%esi),%dl
    movl ConvertX86p16_32BGR888_LUT_X86(,%edx,8),%eax    // eax = ABGR8888 of lower byte
    movb 1(%esi),%dl
    movl ConvertX86p16_32BGR888_LUT_X86+4(,%edx,8),%ebx  // ebx = ABGR8888 of upper byte
    addl %ebx,%eax                                    // eax = ABGR8888 pixel
    movb %al,(%edi)
    movb %ah,1(%edi)
    shrl $16,%eax
    movb %al,2(%edi)
    addl $2,%esi
    addl $3,%edi
    decl %ecx
    jnz .L1
.L2:
    jmp _X86RETURN


.L3:  // clear edx
    xorl %edx,%edx

.L4:  // head
    movl %edi,%eax
    andl $0b11,%eax
    jz .L5
    movb (%esi),%dl
    movl ConvertX86p16_32BGR888_LUT_X86(,%edx,8),%eax    // eax = ABGR8888 of lower byte
    movb 1(%esi),%dl
    movl ConvertX86p16_32BGR888_LUT_X86+4(,%edx,8),%ebx  // ebx = ABGR8888 of upper byte
    addl %ebx,%eax                                    // eax = ABGR8888 pixel
    movb %al,(%edi)
    movb %ah,1(%edi)
    shrl $16,%eax
    movb %al,2(%edi)
    addl $2,%esi
    addl $3,%edi
    decl %ecx
    jmp .L4

.L5:  // unroll 4 times
    pushl %ebp
    movl %ecx,%ebp
    shrl $2,%ebp

    // clear ebx
    xorl %ebx,%ebx

    // save count
    pushl %ecx

    // prestep
    movb (%esi),%bl                                     // ebx = lower byte pixel 1
    movb 1(%esi),%dl                                    // edx = upper byte pixel 1

.L6:    movl ConvertX86p16_32BGR888_LUT_X86(,%ebx,8),%eax // eax = ABGR8888 of lower byte pixel 1
        movb 2(%esi),%bl                                    // ebx = lower byte pixel 2

        movl ConvertX86p16_32BGR888_LUT_X86+4(,%edx,8),%ecx    // ecx = ABGR8888 of upper byte pixel 1
        movb 3(%esi),%dl                                    // edx = upper byte pixel 2

        pushl %ebp                                          // save ebp
        addl %ecx,%eax                                      // eax = ABGR8888 of pixel 1

        movl ConvertX86p16_32BGR888_LUT_X86(,%ebx,8),%ecx      // ecx = ABGR8888 of lower byte pixel 2
        movl ConvertX86p16_32BGR888_LUT_X86+4(,%edx,8),%ebp    // ebp = ABGR8888 of upper byte pixel 2

        movb 4(%esi),%bl                                    // ebx = lower byte pixel 3
        addl %ebp,%ecx                                      // ecx = ABGR8888 of pixel 2

        shll $24,%ebp                                       // ebp = [r][0][0][0] of pixel 2
        movb 5(%esi),%dl                                    // edx = upper byte pixel 3

        shrl $8,%ecx                                        // ecx = [0][0][b][g] pixel 2
        addl %ebp,%eax                                      // eax = [r2][b1][g1][r1] (done)

        movl %eax,(%edi)                                    // store dword 1
        movl ConvertX86p16_32BGR888_LUT_X86+4(,%edx,8),%ebp    // ebp = ABGR8888 of upper byte pixel 3

        movl ConvertX86p16_32BGR888_LUT_X86(,%ebx,8),%eax      // eax = ABGR8888 of lower byte pixel 3
        movb 6(%esi),%bl                                    // ebx = lower byte pixel 4

        addl %eax,%ebp                                      // ebp = ABGR8888 of pixel 3
        movb 7(%esi),%dl                                    // edx = upper byte pixel 4

        shll $16,%ebp                                       // ebp = [g][r][0][0] pixel 3

        shrl $16,%eax                                       //  al = blue component of pixel 3
        addl %ecx,%ebp                                      // ebp = [g3][r3][b2][g2] (done)

        movl %ebp,4(%edi)                                   // store dword 2
        movl ConvertX86p16_32BGR888_LUT_X86(,%ebx,8),%ecx      // ebx = ABGR8888 of lower byte pixel 4

        movl ConvertX86p16_32BGR888_LUT_X86+4(,%edx,8),%ebp    // ebp = ABGR8888 of upper byte pixel 4
        movb 4*2+0(%esi),%bl                                // ebx = lower byte pixel 1

        addl %ebp,%ecx                                      // ecx = ABGR8888 of pixel 4
        movb 4*2+1(%esi),%dl                                // edx = upper byte pixel 1

        shll $8,%ecx                                        // ecx = [b][g][r][0]
        popl %ebp                                           // restore ebp

        movb %al,%cl                                        // ecx = [b4][g4][r4][b3] (done)
        addl $4*2,%esi

        movl %ecx,8(%edi)                                   // store dword 3
        addl $3*4,%edi

        decl %ebp
        jz .L7

        jmp .L6

.L7:  // check tail
    popl %ecx
    andl $0b11,%ecx
    jz .L9

.L8:  // tail
    movb (%esi),%dl
    movl ConvertX86p16_32BGR888_LUT_X86(,%edx,8),%eax    // eax = ABGR8888 of lower byte
    movb 1(%esi),%dl
    movl ConvertX86p16_32BGR888_LUT_X86+4(,%edx,8),%ebx  // ebx = ABGR8888 of upper byte
    addl %ebx,%eax                                    // eax = ABGR8888 pixel
    movb %al,(%edi)
    movb %ah,1(%edi)
    shrl $16,%eax
    movb %al,2(%edi)
    addl $2,%esi
    addl $3,%edi
    decl %ecx
    jnz .L8

.L9: popl %ebp
    jmp _X86RETURN
end;



procedure ConvertX86p16_16BGR565(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
    // check short
    cmpl $16,%ecx
    ja .L3


.L1:  // short loop
    movb (%esi),%al
    movb 1(%esi),%ah
    movl %eax,%ebx
    movl %eax,%edx
    shrl $11,%eax
    andl $0b11111,%eax
    andl $0b11111100000,%ebx
    shll $11,%edx
    addl %ebx,%eax
    addl %edx,%eax
    movb %al,(%edi)
    movb %ah,1(%edi)
    addl $2,%esi
    addl $2,%edi
    decl %ecx
    jnz .L1
.L2:
    jmp _X86RETURN

.L3:  // head
    movl %edi,%eax
    andl $0b11,%eax
    jz .L4
    movb (%esi),%al
    movb 1(%esi),%ah
    movl %eax,%ebx
    movl %eax,%edx
    shrl $11,%eax
    andl $0b11111,%eax
    andl $0b11111100000,%ebx
    shll $11,%edx
    addl %ebx,%eax
    addl %edx,%eax
    movb %al,(%edi)
    movb %ah,1(%edi)
    addl $2,%esi
    addl $2,%edi
    decl %ecx

.L4:  // save count
    pushl %ecx

    // unroll twice
    shrl $1,%ecx

    // point arrays to end
    leal (%esi,%ecx,4),%esi
    leal (%edi,%ecx,4),%edi

    // negative counter
    negl %ecx
    jmp .L6

.L5:    movl %eax,-4(%edi,%ecx,4)
.L6:    movl (%esi,%ecx,4),%eax

        movl (%esi,%ecx,4),%ebx
        andl $0x07E007E0,%eax

        movl (%esi,%ecx,4),%edx
        andl $0x0F800F800,%ebx

        shrl $11,%ebx
        andl $0x001F001F,%edx

        shll $11,%edx
        addl %ebx,%eax

        addl %edx,%eax
        incl %ecx

        jnz .L5

    movl %eax,-4(%edi,%ecx,4)

    // tail
    popl %ecx
    andl $1,%ecx
    jz .L7
    movb (%esi),%al
    movb 1(%esi),%ah
    movl %eax,%ebx
    movl %eax,%edx
    shrl $11,%eax
    andl $0b11111,%eax
    andl $0b11111100000,%ebx
    shll $11,%edx
    addl %ebx,%eax
    addl %edx,%eax
    movb %al,(%edi)
    movb %ah,1(%edi)
    addl $2,%esi
    addl $2,%edi

.L7:
    jmp _X86RETURN
end;




procedure ConvertX86p16_16RGB555(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
    // check short
    cmpl $32,%ecx
    ja .L3


.L1:  // short loop
    movb (%esi),%al
    movb 1(%esi),%ah
    movl %eax,%ebx
    shrl $1,%ebx
    andl $0b0111111111100000,%ebx
    andl $0b0000000000011111,%eax
    addl %ebx,%eax
    movb %al,(%edi)
    movb %ah,1(%edi)
    addl $2,%esi
    addl $2,%edi
    decl %ecx
    jnz .L1
.L2:
    jmp _X86RETURN

.L3:  // head
    movl %edi,%eax
    andl $0b11,%eax
    jz .L4
    movb (%esi),%al
    movb 1(%esi),%ah
    movl %eax,%ebx
    shrl $1,%ebx
    andl $0b0111111111100000,%ebx
    andl $0b0000000000011111,%eax
    addl %ebx,%eax
    movb %al,(%edi)
    movb %ah,1(%edi)
    addl $2,%esi
    addl $2,%edi
    decl %ecx

.L4:  // save ebp
    pushl %ebp

    // save count
    pushl %ecx

    // unroll four times
    shrl $2,%ecx

    // point arrays to end
    leal (%esi,%ecx,8),%esi
    leal (%edi,%ecx,8),%edi

    // negative counter
    xorl %ebp,%ebp
    subl %ecx,%ebp

.L5:    movl (%esi,%ebp,8),%eax // agi?
        movl 4(%esi,%ebp,8),%ecx

        movl %eax,%ebx
        movl %ecx,%edx

        andl $0x0FFC0FFC0,%eax
        andl $0x0FFC0FFC0,%ecx

        shrl $1,%eax
        andl $0x001F001F,%ebx

        shrl $1,%ecx
        andl $0x001F001F,%edx

        addl %ebx,%eax
        addl %edx,%ecx

        movl %eax,(%edi,%ebp,8)
        movl %ecx,4(%edi,%ebp,8)

        incl %ebp
        jnz .L5

    // tail
    popl %ecx
.L6: andl $0b11,%ecx
    jz .L7
    movb (%esi),%al
    movb 1(%esi),%ah
    movl %eax,%ebx
    shrl $1,%ebx
    andl $0b0111111111100000,%ebx
    andl $0b0000000000011111,%eax
    addl %ebx,%eax
    movb %al,(%edi)
    movb %ah,1(%edi)
    addl $2,%esi
    addl $2,%edi
    decl %ecx
    jmp .L6

.L7: popl %ebp
    jmp _X86RETURN
end;





procedure ConvertX86p16_16BGR555(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
    // check short
    cmpl $16,%ecx
    ja .L3


.L1:  // short loop
    movb (%esi),%al
    movb 1(%esi),%ah
    movl %eax,%ebx
    movl %eax,%edx
    shrl $11,%eax
    andl $0b11111,%eax
    shrl $1,%ebx
    andl $0b1111100000,%ebx
    shll $10,%edx
    andl $0b0111110000000000,%edx
    addl %ebx,%eax
    addl %edx,%eax
    movb %al,(%edi)
    movb %ah,1(%edi)
    addl $2,%esi
    addl $2,%edi
    decl %ecx
    jnz .L1
.L2:
    jmp _X86RETURN

.L3:  // head
    movl %edi,%eax
    andl $0b11,%eax
    jz .L4
    movb (%esi),%al
    movb 1(%esi),%ah
    movl %eax,%ebx
    movl %eax,%edx
    shrl $11,%eax
    andl $0b11111,%eax
    shrl $1,%ebx
    andl $0b1111100000,%ebx
    shll $10,%edx
    andl $0b0111110000000000,%edx
    addl %ebx,%eax
    addl %edx,%eax
    movb %al,(%edi)
    movb %ah,1(%edi)
    addl $2,%esi
    addl $2,%edi
    decl %ecx

.L4:  // save count
    pushl %ecx

    // unroll twice
    shrl $1,%ecx

    // point arrays to end
    leal (%esi,%ecx,4),%esi
    leal (%edi,%ecx,4),%edi

    // negative counter
    negl %ecx
    jmp .L6

.L5:    movl %eax,-4(%edi,%ecx,4)
.L6:    movl (%esi,%ecx,4),%eax

        shrl $1,%eax
        movl (%esi,%ecx,4),%ebx

        andl $0x03E003E0,%eax
        movl (%esi,%ecx,4),%edx

        andl $0x0F800F800,%ebx

        shrl $11,%ebx
        andl $0x001F001F,%edx

        shll $10,%edx
        addl %ebx,%eax

        addl %edx,%eax
        incl %ecx

        jnz .L5

    movl %eax,-4(%edi,%ecx,4)

    // tail
    popl %ecx
    andl $1,%ecx
    jz .L7
    movb (%esi),%al
    movb 1(%esi),%ah
    movl %eax,%ebx
    movl %eax,%edx
    shrl $11,%eax
    andl $0b11111,%eax
    shrl $1,%ebx
    andl $0b1111100000,%ebx
    shll $10,%edx
    andl $0b0111110000000000,%edx
    addl %ebx,%eax
    addl %edx,%eax
    movb %al,(%edi)
    movb %ah,1(%edi)
    addl $2,%esi
    addl $2,%edi

.L7:
    jmp _X86RETURN
end;





procedure ConvertX86p16_8RGB332(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
    // check short
    cmpl $16,%ecx
    ja .L3


.L1:  // short loop
    movb (%esi),%al
    movb 1(%esi),%ah
    movl %eax,%ebx
    movl %eax,%edx
    andl $0b11000,%eax          // blue
    shrl $3,%eax
    andl $0b11100000000,%ebx    // green
    shrl $6,%ebx
    andl $0b1110000000000000,%edx // red
    shrl $8,%edx
    addl %ebx,%eax
    addl %edx,%eax
    movb %al,(%edi)
    addl $2,%esi
    incl %edi
    decl %ecx
    jnz .L1
.L2:
    jmp _X86RETURN

.L3: movl %edi,%eax
    andl $0b11,%eax
    jz .L4
    movb (%esi),%al
    movb 1(%esi),%ah
    movl %eax,%ebx
    movl %eax,%edx
    andl $0b11000,%eax          // blue
    shrl $3,%eax
    andl $0b11100000000,%ebx    // green
    shrl $6,%ebx
    andl $0b1110000000000000,%edx // red
    shrl $8,%edx
    addl %ebx,%eax
    addl %edx,%eax
    movb %al,(%edi)
    addl $2,%esi
    incl %edi
    decl %ecx
    jmp .L3

.L4:  // save ebp
    pushl %ebp

    // save count
    pushl %ecx

    // unroll 4 times
    shrl $2,%ecx

    // prestep
    movb (%esi),%dl
    movb 1(%esi),%bl
    movb 2(%esi),%dh

.L5: shll $16,%edx
        movb 3(%esi),%bh

        shll $16,%ebx
        movb 4(%esi),%dl

        movb 6(%esi),%dh
        movb 5(%esi),%bl

        andl $0b00011000000110000001100000011000,%edx
        movb 7(%esi),%bh

        rorl $16+3,%edx
        movl %ebx,%eax                                  // setup eax for reds

        andl $0b00000111000001110000011100000111,%ebx
        andl $0b11100000111000001110000011100000,%eax   // reds

        rorl $16-2,%ebx
        addl $8,%esi

        rorl $16,%eax
        addl $4,%edi

        addl %ebx,%eax
        movb 1(%esi),%bl                                // greens

        addl %edx,%eax
        movb (%esi),%dl                                 // blues

        movl %eax,-4(%edi)
        movb 2(%esi),%dh

        decl %ecx
        jnz .L5

    // check tail
    popl %ecx
    andl $0b11,%ecx
    jz .L7

.L6:  // tail
    movb (%esi),%al
    movb 1(%esi),%ah
    movl %eax,%ebx
    movl %eax,%edx
    andl $0b11000,%eax          // blue
    shrl $3,%eax
    andl $0b11100000000,%ebx    // green
    shrl $6,%ebx
    andl $0b1110000000000000,%edx // red
    shrl $8,%edx
    addl %ebx,%eax
    addl %edx,%eax
    movb %al,(%edi)
    addl $2,%esi
    incl %edi
    decl %ecx
    jnz .L6

.L7: popl %ebp
    jmp _X86RETURN
end;
