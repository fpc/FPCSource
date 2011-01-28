{
    x86 format converters for HERMES
    Some routines Copyright (c) 1998 Christian Nentwich (c.nentwich@cs.ucl.ac.uk)

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

label
  _x86return, _x86return_S;

var
  cpu_flags: DWord = 0;

{ _ConvertX86:
 [ESP+8] ConverterInfo*
 --------------------------------------------------------------------------

 ConvertX86Stretch 'abuses' the following info structure fields:
      - d_pitch for the y increment
      - s_add for the x increment
 because they're unused anyway and this is thread safe.. (it's a per
 converter handle structure)
}
procedure ConvertX86Stretch(hci: PHermesConverterInterface); cdecl; assembler;
label
  endconvert_S;
asm
        pushl %ebp

        movl 8(%ebp),%ebp

        movl 8(%ebp),%eax  // s_height
        sall $16,%eax
        cltd
        idivl 24(%ebp)  // d_height
        movl %eax,44(%ebp)  // d_pitch

        movl 4(%ebp),%eax  // s_width
        sall $16,%eax
        cltd
        idivl 20(%ebp)  // d_width
        movl %eax,12(%ebp)  // s_add

        movl $0,8(%ebp)  // s_height

        movl 16(%ebp),%edi  // d_pixels
        movl 0(%ebp),%esi  // s_pixels

        movl 12(%ebp),%edx  // s_add
        movl 20(%ebp),%ecx  // d_width
        jmp *32(%ebp)  // conv_func

.balign 8
_x86return_S:

        decl 24(%ebp)  // d_height
        jz endconvert_S

        movl 8(%ebp),%eax  // s_height
        addl 28(%ebp),%edi  // d_add

        addl 44(%ebp),%eax  // d_pitch

        movl %eax,%ebx

        shrl $16,%eax

        mull 40(%ebp)  // s_pitch
        andl $0x0ffff,%ebx

        movl %ebx,8(%ebp)  // s_height
        addl %eax,%esi

        movl 12(%ebp),%edx  // s_add
        movl 20(%ebp),%ecx  // d_width

        jmp *32(%ebp)  // conv_func

endconvert_S:

        popl %ebp
end;

procedure ConvertX86(hci: PHermesConverterInterface); cdecl; assembler;
label
  endconvert;
asm
        pushl %ebp

        movl 8(%ebp),%ebp

        movl 0(%ebp),%esi  // s_pixels
        movl 20(%ebp),%ecx  // d_width
        movl 16(%ebp),%edi  // d_pixels

        jmp *32(%ebp)

.balign 8
_x86return:
        decl 8(%ebp)  // s_height
        jz endconvert

        movl 20(%ebp),%ecx  // d_width
        addl 12(%ebp),%esi  // s_add
        addl 28(%ebp),%edi  // d_add

        jmp *32(%ebp)


endconvert:
        popl %ebp
end;

// Hermes_X86_CPU returns the CPUID flags in eax
function Hermes_X86_CPU: Integer; cdecl; assembler;
asm
        pushfl
        popl %eax

        movl %eax,%ecx

        xorl $0x040000,%eax
        pushl %eax

        popfl
        pushfl

        popl %eax
        xorl %ecx,%eax
        jz .L1   // Processor is 386

        pushl %ecx
        popfl

        movl %ecx,%eax
        xorl $0x200000,%eax

        pushl %eax
        popfl
        pushfl

        popl %eax
        xorl %ecx,%eax
        je .L1

        pusha

        movl $1,%eax
        cpuid

        movl %edx,cpu_flags

        popa

        movl cpu_flags,%eax

.L1:
end;
