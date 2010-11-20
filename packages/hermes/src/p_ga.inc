{
    Free Pascal port of the Hermes C library.
    Copyright (C) 2001-2003  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C version by Christian Nentwich (c.nentwich@cs.ucl.ac.uk)

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

{
   Generic format conversion routines for the HERMES library
   Copyright (c) 1998 Christian Nentwich (c.nentwich@cs.ucl.ac.uk)
   This source code is licensed under the GNU LGPL

   Please refer to the file COPYING.LIB contained in the distribution for
   licensing conditions
}

procedure ConvertP_Generic32_A_Generic32_A(iface: PHermesConverterInterface); cdecl;
var
  s_pixel, r, g, b, a: Uint32;
  count: DWord;
  source, dest: PUint8;
begin
  source := iface^.s_pixels; dest := iface^.d_pixels;
  repeat
    count := iface^.s_width;
    repeat
      s_pixel := PUint32(source)^;

      r := ((s_pixel shr iface^.info.r_right) shl iface^.info.r_left) and
        iface^.mask_r;
      g := ((s_pixel shr iface^.info.g_right) shl iface^.info.g_left) and
        iface^.mask_g;
      b := ((s_pixel shr iface^.info.b_right) shl iface^.info.b_left) and
        iface^.mask_b;
      a := ((s_pixel shr iface^.info.a_right) shl iface^.info.a_left) and
        iface^.mask_a;

      PUint32(dest)^ := r or g or b or a;

      Inc(source, 4);
      Inc(dest, 4);
      Dec(count);
    until count = 0;

    Inc(source, iface^.s_add);
    Inc(dest, iface^.d_add);
    Dec(iface^.s_height);
  until iface^.s_height = 0;
end;

procedure ConvertP_Generic32_A_Generic24_A(iface: PHermesConverterInterface); cdecl;
var
  s_pixel, r, g, b, a: Uint32;
  d_ptr: PUint8;
  count: DWord;
  source, dest: PUint8;
begin
  d_ptr := PUint8(@s_pixel) + (R_32 - R_24);
  source := iface^.s_pixels; dest := iface^.d_pixels;
  repeat
    count := iface^.s_width;
    repeat
      s_pixel := PUint32(source)^;

      r := ((s_pixel shr iface^.info.r_right) shl iface^.info.r_left) and
        iface^.mask_r;
      g := ((s_pixel shr iface^.info.g_right) shl iface^.info.g_left) and
        iface^.mask_g;
      b := ((s_pixel shr iface^.info.b_right) shl iface^.info.b_left) and
        iface^.mask_b;
      a := ((s_pixel shr iface^.info.a_right) shl iface^.info.a_left) and
        iface^.mask_a;

      s_pixel := r or g or b or a;

      (dest+0)^ := (d_ptr+0)^;
      (dest+1)^ := (d_ptr+1)^;
      (dest+2)^ := (d_ptr+2)^;

      Inc(source, 4);
      Inc(dest, 3);
      Dec(count);
    until count = 0;

    Inc(source, iface^.s_add);
    Inc(dest, iface^.d_add);
    Dec(iface^.s_height);
  until iface^.s_height = 0;
end;

procedure ConvertP_Generic32_A_Generic16_A(iface: PHermesConverterInterface); cdecl;
var
  s_pixel, r, g, b, a: Uint32;
  count: DWord;
  source, dest: PUint8;
begin
  source := iface^.s_pixels; dest := iface^.d_pixels;
  repeat
    count := iface^.s_width shr 1;

    if count <> 0 then
      repeat
        r := (((PUint32(source)^) shr iface^.info.r_right) shl iface^.info.r_left) and
              iface^.mask_r;
        g := (((PUint32(source)^) shr iface^.info.g_right) shl iface^.info.g_left) and
              iface^.mask_g;
        b := (((PUint32(source)^) shr iface^.info.b_right) shl iface^.info.b_left) and
              iface^.mask_b;
        a := (((PUint32(source)^) shr iface^.info.a_right) shl iface^.info.a_left) and
              iface^.mask_a;

        s_pixel := (r or g or b or a) shl DWORD_SMALLINT0_SHL;

        r := ((((PUint32(source)+1)^) shr iface^.info.r_right) shl iface^.info.r_left) and
              iface^.mask_r;
        g := ((((PUint32(source)+1)^) shr iface^.info.g_right) shl iface^.info.g_left) and
              iface^.mask_g;
        b := ((((PUint32(source)+1)^) shr iface^.info.b_right) shl iface^.info.b_left) and
              iface^.mask_b;
        a := ((((PUint32(source)+1)^) shr iface^.info.a_right) shl iface^.info.a_left) and
              iface^.mask_a;

        s_pixel := s_pixel or ((r or g or b or a) shl DWORD_SMALLINT1_SHL);

        PUint32(dest)^ := s_pixel;

        Inc(source, 8);
        Inc(dest, 4);
        Dec(count);
      until count = 0;


    { Trailing pixel }

    if (iface^.s_width and 1) <> 0 then
    begin
      r := (((PUint32(source)^) shr iface^.info.r_right) shl iface^.info.r_left) and
            iface^.mask_r;
      g := (((PUint32(source)^) shr iface^.info.g_right) shl iface^.info.g_left) and
            iface^.mask_g;
      b := (((PUint32(source)^) shr iface^.info.b_right) shl iface^.info.b_left) and
            iface^.mask_b;
      a := (((PUint32(source)^) shr iface^.info.a_right) shl iface^.info.a_left) and
            iface^.mask_a;

      PUint16(dest)^ := r or g or b or a;
      Inc(dest, 2);
      Inc(source, 4);
    end;

    Inc(source, iface^.s_add);
    Inc(dest, iface^.d_add);
    Dec(iface^.s_height);
  until iface^.s_height = 0;
end;

procedure ConvertP_Generic32_A_Generic8_A(iface: PHermesConverterInterface); cdecl;
var
  s_pixel, r, g, b, a: Uint32;
  count: DWord;
  source, dest: PUint8;
begin
  source := iface^.s_pixels; dest := iface^.d_pixels;
  repeat
    count := iface^.s_width;
    repeat
      s_pixel := PUint32(source)^;

      r := ((s_pixel shr iface^.info.r_right) shl iface^.info.r_left) and
        iface^.mask_r;
      g := ((s_pixel shr iface^.info.g_right) shl iface^.info.g_left) and
        iface^.mask_g;
      b := ((s_pixel shr iface^.info.b_right) shl iface^.info.b_left) and
        iface^.mask_b;
      a := ((s_pixel shr iface^.info.a_right) shl iface^.info.a_left) and
        iface^.mask_a;

      dest^ := r or g or b or a;

      Inc(source, 4);
      Inc(dest);
      Dec(count);
    until count = 0;

    Inc(source, iface^.s_add);
    Inc(dest, iface^.d_add);
    Dec(iface^.s_height);
  until iface^.s_height = 0;
end;

procedure ConvertP_Generic24_A_Generic32_A(iface: PHermesConverterInterface); cdecl;
var
  s_pixel, r, g, b, a: Uint32;
  count: DWord;
  source, dest: PUint8;
begin
  source := iface^.s_pixels; dest := iface^.d_pixels;
  repeat
    count := iface^.s_width;
    repeat
      s_pixel := ((PUint32(source+R_24)^) shl 16) or
                 ((PUint32(source+G_24)^) shl 8) or
                 (PUint32(source+B_24)^);

      r := ((s_pixel shr iface^.info.r_right) shl iface^.info.r_left) and
        iface^.mask_r;
      g := ((s_pixel shr iface^.info.g_right) shl iface^.info.g_left) and
        iface^.mask_g;
      b := ((s_pixel shr iface^.info.b_right) shl iface^.info.b_left) and
        iface^.mask_b;
      a := ((s_pixel shr iface^.info.a_right) shl iface^.info.a_left) and
        iface^.mask_a;

      PUint32(dest)^ := r or g or b or a;

      Inc(source, 3);
      Inc(dest, 4);
      Dec(count);
    until count = 0;

    Inc(source, iface^.s_add);
    Inc(dest, iface^.d_add);
    Dec(iface^.s_height);
  until iface^.s_height = 0;
end;

procedure ConvertP_Generic24_A_Generic24_A(iface: PHermesConverterInterface); cdecl;
var
  s_pixel, r, g, b, a: Uint32;
  d_ptr: PUint8;
  count: DWord;
  source, dest: PUint8;
begin
  d_ptr := PUint8(@s_pixel) + (R_32 - R_24);
  source := iface^.s_pixels; dest := iface^.d_pixels;
  repeat
    count := iface^.s_width;
    repeat
      s_pixel := ((PUint32(source+R_24)^) shl 16) or
                 ((PUint32(source+G_24)^) shl 8) or
                 (PUint32(source+B_24)^);

      r := ((s_pixel shl iface^.info.r_left) shr iface^.info.r_right) and
        iface^.mask_r;
      g := ((s_pixel shl iface^.info.g_left) shr iface^.info.g_right) and
        iface^.mask_g;
      b := ((s_pixel shl iface^.info.b_left) shr iface^.info.b_right) and
        iface^.mask_b;
      a := ((s_pixel shl iface^.info.a_left) shr iface^.info.a_right) and
        iface^.mask_a;

      s_pixel := r or g or b or a;

      (dest + 0)^ := (d_ptr + 0)^;
      (dest + 1)^ := (d_ptr + 1)^;
      (dest + 2)^ := (d_ptr + 2)^;

      Inc(source, 3);
      Inc(dest, 3);
      Dec(count);
    until count = 0;

    Inc(source, iface^.s_add);
    Inc(dest, iface^.d_add);
    Dec(iface^.s_height);
  until iface^.s_height = 0;
end;

procedure ConvertP_Generic24_A_Generic16_A(iface: PHermesConverterInterface); cdecl;
var
  s_pixel, r, g, b, a: Uint32;
  count: DWord;
  source, dest: PUint8;
begin
  source := iface^.s_pixels; dest := iface^.d_pixels;
  repeat
    count := iface^.s_width;
    repeat
      s_pixel := ((PUint32(source+R_24)^) shl 16) or
                 ((PUint32(source+G_24)^) shl 8) or
                 (PUint32(source+B_24)^);

      r := ((s_pixel shr iface^.info.r_right) shl iface^.info.r_left) and
        iface^.mask_r;
      g := ((s_pixel shr iface^.info.g_right) shl iface^.info.g_left) and
        iface^.mask_g;
      b := ((s_pixel shr iface^.info.b_right) shl iface^.info.b_left) and
        iface^.mask_b;
      a := ((s_pixel shr iface^.info.a_right) shl iface^.info.a_left) and
        iface^.mask_a;

      PUint16(dest)^ := r or g or b or a;

      Inc(source, 3);
      Inc(dest, 2);
      Dec(count);
    until count = 0;

    Inc(source, iface^.s_add);
    Inc(dest, iface^.d_add);
    Dec(iface^.s_height);
  until iface^.s_height = 0;
end;

procedure ConvertP_Generic24_A_Generic8_A(iface: PHermesConverterInterface); cdecl;
var
  s_pixel, r, g, b, a: Uint32;
  count: DWord;
  source, dest: PUint8;
begin
  source := iface^.s_pixels; dest := iface^.d_pixels;
  repeat
    count := iface^.s_width;
    repeat
      s_pixel := ((PUint32(source+R_24)^) shl 16) or
                 ((PUint32(source+G_24)^) shl 8) or
                 (PUint32(source+B_24)^);

      r := ((s_pixel shr iface^.info.r_right) shl iface^.info.r_left) and
        iface^.mask_r;
      g := ((s_pixel shr iface^.info.g_right) shl iface^.info.g_left) and
        iface^.mask_g;
      b := ((s_pixel shr iface^.info.b_right) shl iface^.info.b_left) and
        iface^.mask_b;
      a := ((s_pixel shr iface^.info.a_right) shl iface^.info.a_left) and
        iface^.mask_a;

      dest^ := r or g or b or a;

      Inc(source, 3);
      Inc(dest);
      Dec(count);
    until count = 0;

    Inc(source, iface^.s_add);
    Inc(dest, iface^.d_add);
    Dec(iface^.s_height);
  until iface^.s_height = 0;
end;

procedure ConvertP_Generic16_A_Generic32_A(iface: PHermesConverterInterface); cdecl;
var
  s_pixel, r, g, b, a: Uint32;
  count: DWord;
  source, dest: PUint8;
begin
  source := iface^.s_pixels; dest := iface^.d_pixels;
  repeat
    count := iface^.s_width;
    repeat
      s_pixel := PUint16(source)^;

      r := ((s_pixel shr iface^.info.r_right) shl iface^.info.r_left) and
        iface^.mask_r;
      g := ((s_pixel shr iface^.info.g_right) shl iface^.info.g_left) and
        iface^.mask_g;
      b := ((s_pixel shr iface^.info.b_right) shl iface^.info.b_left) and
        iface^.mask_b;
      a := ((s_pixel shr iface^.info.a_right) shl iface^.info.a_left) and
        iface^.mask_a;

      PUint32(dest)^ := r or g or b or a;

      Inc(source, 2);
      Inc(dest, 4);
      Dec(count);
    until count = 0;

    Inc(source, iface^.s_add);
    Inc(dest, iface^.d_add);
    Dec(iface^.s_height);
  until iface^.s_height = 0;
end;

procedure ConvertP_Generic16_A_Generic24_A(iface: PHermesConverterInterface); cdecl;
var
  s_pixel, r, g, b, a: Uint32;
  d_ptr: PUint8;
  count: DWord;
  source, dest: PUint8;
begin
  d_ptr := PUint8(@s_pixel) + (R_32 - R_24);
  source := iface^.s_pixels; dest := iface^.d_pixels;
  repeat
    count := iface^.s_width;
    repeat
      s_pixel := PUint16(source)^;

      r := ((s_pixel shr iface^.info.r_right) shl iface^.info.r_left) and
        iface^.mask_r;
      g := ((s_pixel shr iface^.info.g_right) shl iface^.info.g_left) and
        iface^.mask_g;
      b := ((s_pixel shr iface^.info.b_right) shl iface^.info.b_left) and
        iface^.mask_b;
      a := ((s_pixel shr iface^.info.a_right) shl iface^.info.a_left) and
        iface^.mask_a;

      s_pixel := r or g or b or a;

      (dest + 0)^ := (d_ptr + 0)^;
      (dest + 1)^ := (d_ptr + 1)^;
      (dest + 2)^ := (d_ptr + 2)^;

      Inc(source, 2);
      Inc(dest, 3);
      Dec(count);
    until count = 0;

    Inc(source, iface^.s_add);
    Inc(dest, iface^.d_add);
    Dec(iface^.s_height);
  until iface^.s_height = 0;
end;

procedure ConvertP_Generic16_A_Generic16_A(iface: PHermesConverterInterface); cdecl;
var
  s_pixel, r, g, b, a: Uint32;
  count: DWord;
  source, dest: PUint8;
begin
  source := iface^.s_pixels; dest := iface^.d_pixels;
  repeat
    count := iface^.s_width;
    repeat
      s_pixel := PUint16(source)^;

      r := ((s_pixel shr iface^.info.r_right) shl iface^.info.r_left) and
        iface^.mask_r;
      g := ((s_pixel shr iface^.info.g_right) shl iface^.info.g_left) and
        iface^.mask_g;
      b := ((s_pixel shr iface^.info.b_right) shl iface^.info.b_left) and
        iface^.mask_b;
      a := ((s_pixel shr iface^.info.a_right) shl iface^.info.a_left) and
        iface^.mask_a;

      PUint16(dest)^ := r or g or b or a;

      Inc(source, 2);
      Inc(dest, 2);
      Dec(count);
    until count = 0;

    Inc(source, iface^.s_add);
    Inc(dest, iface^.d_add);
    Dec(iface^.s_height);
  until iface^.s_height = 0;
end;

procedure ConvertP_Generic16_A_Generic8_A(iface: PHermesConverterInterface); cdecl;
var
  s_pixel, r, g, b, a: Uint32;
  count: DWord;
  source, dest: PUint8;
begin
  source := iface^.s_pixels; dest := iface^.d_pixels;
  repeat
    count := iface^.s_width;
    repeat
      s_pixel := PUint16(source)^;

      r := ((s_pixel shr iface^.info.r_right) shl iface^.info.r_left) and
        iface^.mask_r;
      g := ((s_pixel shr iface^.info.g_right) shl iface^.info.g_left) and
        iface^.mask_g;
      b := ((s_pixel shr iface^.info.b_right) shl iface^.info.b_left) and
        iface^.mask_b;
      a := ((s_pixel shr iface^.info.a_right) shl iface^.info.a_left) and
        iface^.mask_a;

      dest^ := r or g or b or a;

      Inc(source, 2);
      Inc(dest);
      Dec(count);
    until count = 0;

    Inc(source, iface^.s_add);
    Inc(dest, iface^.d_add);
    Dec(iface^.s_height);
  until iface^.s_height = 0;
end;

{ -------------------------------------------------------------------------

                             STRETCH CONVERTERS

  ------------------------------------------------------------------------- }


procedure ConvertP_Generic32_A_Generic32_A_S(iface: PHermesConverterInterface); cdecl;
var
  s_pixel, r, g, b, a: Uint32;
  count: DWord;
  source, dest: PUint8;
  dx, dy, x, y: DWord;
begin
  source := iface^.s_pixels;
  dest := iface^.d_pixels;
  dy := (iface^.s_height shl 16) div iface^.d_height;
  dx := (iface^.s_width shl 16) div iface^.d_width;
  y := 0;
  repeat
    count := iface^.d_width;
    x := 0;
    repeat
      s_pixel := (PUint32(source)+(x shr 16))^;

      r := ((s_pixel shr iface^.info.r_right) shl iface^.info.r_left) and
        iface^.mask_r;
      g := ((s_pixel shr iface^.info.g_right) shl iface^.info.g_left) and
        iface^.mask_g;
      b := ((s_pixel shr iface^.info.b_right) shl iface^.info.b_left) and
        iface^.mask_b;
      a := ((s_pixel shr iface^.info.a_right) shl iface^.info.a_left) and
        iface^.mask_a;

      PUint32(dest)^ := r or g or b or a;

      Inc(x, dx);
      Inc(dest, 4);
      Dec(count);
    until count = 0;

    Inc(dest, iface^.d_add);

    Inc(y, dy);
    Inc(source, (y shr 16)*DWord(iface^.s_pitch));
    y := y and $ffff;

    Dec(iface^.d_height);
  until iface^.d_height = 0;
end;

procedure ConvertP_Generic32_A_Generic16_A_S(iface: PHermesConverterInterface); cdecl;
var
  s_pixel, r, g, b, a: Uint32;
  count: DWord;
  source, dest: PUint8;
  dx, dy, x, y: DWord;
begin
  source := iface^.s_pixels;
  dest := iface^.d_pixels;
  dy := (iface^.s_height shl 16) div iface^.d_height;
  dx := (iface^.s_width shl 16) div iface^.d_width;
  y := 0;
  repeat
    count := iface^.d_width;
    x := 0;
    repeat
      s_pixel := (PUint32(source)+(x shr 16))^;

      r := ((s_pixel shr iface^.info.r_right) shl iface^.info.r_left) and
        iface^.mask_r;
      g := ((s_pixel shr iface^.info.g_right) shl iface^.info.g_left) and
        iface^.mask_g;
      b := ((s_pixel shr iface^.info.b_right) shl iface^.info.b_left) and
        iface^.mask_b;
      a := ((s_pixel shr iface^.info.a_right) shl iface^.info.a_left) and
        iface^.mask_a;

      PUint16(dest)^ := r or g or b or a;

      Inc(x, dx);
      Inc(dest, 2);
      Dec(count);
    until count = 0;

    Inc(dest, iface^.d_add);

    Inc(y, dy);
    Inc(source, (y shr 16)*DWord(iface^.s_pitch));
    y := y and $ffff;

    Dec(iface^.d_height);
  until iface^.d_height = 0;
end;
