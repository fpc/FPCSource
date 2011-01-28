{
    MMX format converters for HERMES
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

    COPYRIGHT NOTICE

    This file partly contains code that is (c) Intel Corporation, specifically
    the mode detection routine, and the converter to 15 bit (8 pixel
    conversion routine from the mmx programming tutorial pages).
}

// Constants for conversion routines
const
  //mmx32_rgb555_rb: QWord = $00f800f800f800f8;
  mmx32_rgb555_add: QWord = $2000000820000008;
  //mmx32_rgb555_g: QWord = $0000f8000000f800;

{## Gone for now, it didnt draw correctly AND was slower than the x86 routine
_ConvertMMXp32_16RGB565:

        jmp _MMXRETURN}

procedure ConvertMMXp32_16RGB555(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
        movq mmx32_rgb555_add,%mm7
        movq mmx32_rgb555_g,%mm6

        movl %ecx,%edx                     // Save ecx

        andl $0x0fffffff8,%ecx             // clear lower three bits
        jnz .L_OK
        jmp .L2

.L_OK:

        movq 8(%esi),%mm2

        movq (%esi),%mm0
        movq %mm2,%mm3

        pand mmx32_rgb555_rb,%mm3
        movq %mm0,%mm1

        pand mmx32_rgb555_rb,%mm1
        pmaddwd %mm7,%mm3

        pmaddwd %mm7,%mm1
        pand %mm6,%mm2

.L1:
        movq 24(%esi),%mm4
        pand %mm6,%mm0

        movq 16(%esi),%mm5
        por %mm2,%mm3

        psrld $6,%mm3
        por %mm0,%mm1

        movq %mm4,%mm0
        psrld $6,%mm1

        pand mmx32_rgb555_rb,%mm0
        packssdw %mm3,%mm1

        movq %mm5,%mm3
        pmaddwd %mm7,%mm0

        pand mmx32_rgb555_rb,%mm3
        pand %mm6,%mm4

        movq %mm1,(%edi)
        pmaddwd %mm7,%mm3

        addl $32,%esi
        por %mm0,%mm4

        pand %mm6,%mm5
        psrld $6,%mm4

        movq 8(%esi),%mm2
        por %mm3,%mm5

        movq (%esi),%mm0
        psrld $6,%mm5

        movq %mm2,%mm3
        movq %mm0,%mm1

        pand mmx32_rgb555_rb,%mm3
        packssdw %mm4,%mm5

        pand mmx32_rgb555_rb,%mm1
        pand %mm6,%mm2

        movq %mm5,8(%edi)
        pmaddwd %mm7,%mm3

        pmaddwd %mm7,%mm1
        addl $16,%edi

        subl $8,%ecx
        jz .L2
        jmp .L1


.L2:
        movl %edx,%ecx

        andl $7,%ecx
        jz .L4

.L3:
        movl (%esi),%ebx
        addl $4,%esi

        movl %ebx,%eax
        movl %ebx,%edx

        shrl $3,%eax
        shrl $6,%edx

        andl $0b0000000000011111,%eax
        andl $0b0000001111100000,%edx

        shrl $9,%ebx

        orl %edx,%eax

        andl $0b0111110000000000,%ebx

        orl %ebx,%eax

        movw %ax,(%edi)
        addl $2,%edi

        decl %ecx
        jnz .L3

.L4:
        jmp _MMXRETURN
end;
