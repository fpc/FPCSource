{
    Free Pascal version of the Hermes pixel conversion library.
    Copyright (C) 2012, 2013  Nikolay Nikolov (nickysn@users.sourceforge.net)

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

{$ASMMODE intel}

{ -------------------------------------------------------------------------

                             NORMAL CONVERTERS

  ------------------------------------------------------------------------- }

procedure ConvertX86_64_index8_32(iface: PHermesConverterInterface); cdecl;
label
  preloop_start, preloop_skip, loop_start, loop_pre_remainder, loop_remainder, done;
var
  i: Integer;
  s_pixel: Uint8;
  d_pixel: Uint32;
  source, dest: PUint8;
  lookup: PUint32;
  s_width: int64;
begin
  source := iface^.s_pixels;
  dest := iface^.d_pixels;
  lookup := iface^.lookup;
  s_width := iface^.s_width;
  if s_width <= 0 then
    exit;
  repeat
{    for i := 0 to iface^.s_width - 1 do
    begin}
{      s_pixel := source^;
      d_pixel := iface^.lookup[s_pixel];
      PUint32(dest)^ := d_pixel or $FF;
      Inc(source);
      Inc(dest, 4);}
      asm
        mov rsi, [source]
        mov rdi, [dest]
        mov rbx, [lookup]
        mov rcx, [s_width]

preloop_start:
        test rdi, 15
        jz preloop_skip

        movzx rax, byte [rsi]
        mov edx, dword [rbx + rax * 4]
        movnti [rdi], edx
        inc rsi
        add rdi, 4
        sub rcx, 1
        jz done
        jmp preloop_start

preloop_skip:
        mov r8, rcx
        and r8, 3
        shr rcx, 2
        test rcx, rcx
        jz loop_pre_remainder

        align 16
loop_start:
        movzx rax, byte [rsi]
        movzx r9, byte [rsi + 1]
        movzx r10, byte [rsi + 2]
        movzx r11, byte [rsi + 3]

        movd xmm0, dword [rbx + rax * 4]
        movd xmm1, dword [rbx + r9 * 4]
        movd xmm2, dword [rbx + r10 * 4]
        movd xmm3, dword [rbx + r11 * 4]
        punpckldq xmm0, xmm1
        punpckldq xmm2, xmm3
        punpcklqdq xmm0, xmm2

        movntdq [rdi], xmm0
        add rsi, 4
        add rdi, 16
        sub ecx, 1
        jnz loop_start

loop_pre_remainder:
        mov rcx, r8
        test rcx, rcx
        jz done
loop_remainder:
        movzx rax, byte [rsi]
        mov edx, dword [rbx + rax * 4]
        movnti [rdi], edx
        inc rsi
        add rdi, 4
        sub ecx, 1
        jnz loop_remainder

done:
        mov [source], rsi
        mov [dest], rdi
      end;
{    end;}
    Inc(source, iface^.s_add);
    Inc(dest, iface^.d_add);
    Dec(iface^.s_height);
  until iface^.s_height = 0;
  asm
    sfence
  end;
end;
