{
    x86 format converters for HERMES
    Some routines Copyright (c) 1998 Christian Nentwich (c.nentwich@cs.ucl.ac.uk)
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



{ _Convert*_S
 Paramters:
   ESI = source
   EDI = dest
   ECX = amount (NOT 0!!! (the _ConvertX86 routine checks for that though))
   EDX = x increment
 Destroys:
   EAX, EBX, ECX, EDX
}

procedure ConvertX86p32_16RGB565_S(CONVERT_PARAMETERS); cdecl; nostackframe; assembler;
asm
        pushl %ebp
        pushl %edx              // increment now at [esp+4]!

        movl $0,%ebp

        pushl %ecx
        shrl $1,%ecx
        jnz .L_ok
        jmp .L_final

.L_ok:

.balign 8
.Lx:
        movl %ebp,%eax

        shrl $14,%eax
        addl 4(%esp),%ebp

        movl %ebp,%edx
        andl $0x0fffffffc,%eax

        shrl $14,%edx
        movl (%esi,%eax),%ebx  // ebx = pixel one

        andl $0x0fffffffc,%edx
        andl $0x0f8fcf8,%ebx

        shrb $2,%bh
        movl (%esi,%edx),%eax  // eax = pixel two

        andl $0x0f8fcf8,%eax
        addl 4(%esp),%ebp

        shrb $2,%ah             // eax & ebx= rrrrr000|00gggggg|bbbbb000

        movl %eax,%edx

        shrw $3,%dx
        andl $0x0f80000,%eax

        shrl $8,%eax

        orl %edx,%eax

        shll $16,%eax
        movl %ebx,%edx

        shrw $3,%dx
        andl $0x0f80000,%ebx

        shrl $8,%ebx
        orb %dh,%ah

        orb %bh,%ah
        orb %dl,%al

        movl %eax,(%edi)
        addl $4,%edi

        decl %ecx
        jnz .Lx

.L_final:
        popl %ecx
        andl $1,%ecx
        jz .L_out

        shrl $14,%ebp           // trailing pixel

        andl $0x0fffffffc,%ebp

        movl (%esi,%ebp),%eax
        movl (%esi,%ebp),%ebx

        shrl $8,%ebx
        andl $0x0fcf8,%eax

        shrb $2,%ah
        andl $0x0f800,%ebx

        shrl $3,%eax

        orl %ebx,%eax

        movw %ax,(%edi)
        addl $2,%edi

.L_out:

        popl %edx
        popl %ebp
        jmp _X86RETURN_S
end;
