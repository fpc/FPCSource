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

label
  _MMXRETURN;

{ _ConvertMMX:
 [ESP+8] ConverterInfo*
 --------------------------------------------------------------------------
 ConverterInfo (ebp+..)
   0: void *s_pixels
   4: int s_width
   8: int s_height
  12: int s_add
  16: void *d_pixels
  20: int d_width
  24: int d_height
  28: int d_add
  32: void (*converter_function)()
  36: int32 *lookup
}
procedure ConvertMMX(hci: PHermesConverterInterface); cdecl; assembler;
label
  y_loop, endconvert;
asm
        movl 8(%ebp),%eax

        cmpl $0,4(%eax)
        je endconvert

        pushl %ebp
        movl %eax,%ebp

        movl (%ebp),%esi
        movl 16(%ebp),%edi

y_loop:
        movl 4(%ebp),%ecx

        jmp *32(%ebp)

_MMXRETURN:
        addl 12(%ebp),%esi
        addl 28(%ebp),%edi

        decl 8(%ebp)
        jnz y_loop


        popl %ebp

endconvert:
        emms
end;
