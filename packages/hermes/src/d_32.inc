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
   32 bit to * dithered converters for the HERMES library
   Copyright (c) 1998 Christian Nentwich (c.nentwich@cs.ucl.ac.uk)
   This source code is licensed under the GNU LGPL

   Please refer to the file COPYING.LIB contained in the distribution for
   licensing conditions
}

procedure ConvertP_32rgb888_16rgb565_dither(iface: PHermesConverterInterface); cdecl;
var
  source, dest: PUint8;
  d_pixel: Uint32;
  y, count: LongInt;
begin
  y := 0;
  source := iface^.s_pixels;
  dest := iface^.d_pixels;
  while y < iface^.d_height do
  begin
    { Get counter for this scanline }
    count := iface^.d_width;

    { Check first pixel alignment, correct if necessary }
    if (PtrUInt(iface^.d_pixels) and 3) <> 0 then
    begin
      PUint16(dest)^ :=
        DitherTab_r565_44[count and 3, y and 3, (PUint32(source)^ shr 16) and $ff] or
        DitherTab_g565_44[count and 3, y and 3, (PUint32(source)^ shr 8) and $ff] or
        DitherTab_b565_44[count and 3, y and 3, PUint32(source)^ and $ff];

      Inc(source, 4);
      Inc(dest, 2);
      Dec(count);
    end;

    { Two pixels at a time loop }
    while count > 1 do
    begin
      d_pixel := (
        DitherTab_r565_44[count and 3, y and 3, (PUint32(source)^ shr 16) and $ff] or
        DitherTab_g565_44[count and 3, y and 3, (PUint32(source)^ shr 8) and $ff] or
        DitherTab_b565_44[count and 3, y and 3, PUint32(source)^ and $ff]) shl DWORD_SMALLINT0_SHL;

      Inc(source, 4);
      Dec(count);

      d_pixel := d_pixel or ((
        DitherTab_r565_44[count and 3, y and 3, (PUint32(source)^ shr 16) and $ff] or
        DitherTab_g565_44[count and 3, y and 3, (PUint32(source)^ shr 8) and $ff] or
        DitherTab_b565_44[count and 3, y and 3, PUint32(source)^ and $ff]) shl DWORD_SMALLINT1_SHL);

      Dec(count);
      Inc(source, 4);

      PUint32(dest)^ := d_pixel;

      Inc(dest, 4);
    end;

    { Convert the odd trailing pixel }
    if (iface^.d_width and 1) <> 0 then
    begin
      PUint16(dest)^ :=
        DitherTab_r565_44[count and 3, y and 3, (PUint32(source)^ shr 16) and $ff] or
        DitherTab_g565_44[count and 3, y and 3, (PUint32(source)^ shr 8) and $ff] or
        DitherTab_b565_44[count and 3, y and 3, PUint32(source)^ and $ff];

      Inc(source, 4);
      Inc(dest, 2);
    end;

    Inc(source, iface^.s_add);
    Inc(dest, iface^.d_add);

    Inc(y);
  end;
end;

procedure ConvertP_32rgb888_8rgb332_dither(iface: PHermesConverterInterface); cdecl;
var
  source, dest: PUint8;
  d_pixel: Uint32;
  y, count: LongInt;
begin
  y := 0;
  source := iface^.s_pixels;
  dest := iface^.d_pixels;

  while y < iface^.d_height do
  begin
    { Get counter for this scanline }
    count := iface^.d_width;


    { TODO: alignment loop }


    { Convert 4 pixels at a time }
    while count > 3 do
    begin
      d_pixel := (
        DitherTab_r332_44[count and 3, y and 3, (PUint32(source)^ shr 16) and $ff] or
        DitherTab_g332_44[count and 3, y and 3, (PUint32(source)^ shr 8) and $ff] or
        DitherTab_b332_44[count and 3, y and 3, PUint32(source)^ and $ff]) shl DWORD_BYTE0_SHL;
      Dec(count);
      Inc(source, 4);

      d_pixel := d_pixel or ((
        DitherTab_r332_44[count and 3, y and 3, (PUint32(source)^ shr 16) and $ff] or
        DitherTab_g332_44[count and 3, y and 3, (PUint32(source)^ shr 8) and $ff] or
        DitherTab_b332_44[count and 3, y and 3, PUint32(source)^ and $ff]) shl DWORD_BYTE1_SHL);
      Dec(count);
      Inc(source, 4);

      d_pixel := d_pixel or ((
        DitherTab_r332_44[count and 3, y and 3, (PUint32(source)^ shr 16) and $ff] or
        DitherTab_g332_44[count and 3, y and 3, (PUint32(source)^ shr 8) and $ff] or
        DitherTab_b332_44[count and 3, y and 3, PUint32(source)^ and $ff]) shl DWORD_BYTE2_SHL);
      Dec(count);
      Inc(source, 4);

      d_pixel := d_pixel or ((
        DitherTab_r332_44[count and 3, y and 3, (PUint32(source)^ shr 16) and $ff] or
        DitherTab_g332_44[count and 3, y and 3, (PUint32(source)^ shr 8) and $ff] or
        DitherTab_b332_44[count and 3, y and 3, PUint32(source)^ and $ff]) shl DWORD_BYTE3_SHL);
      Dec(count);
      Inc(source, 4);

      PUint32(dest)^ := d_pixel;
      Inc(dest, 4);
    end;

    { Write trailing pixels }
    while count <> 0 do
    begin
      Dec(count);
      dest^ :=
        DitherTab_r332_44[count and 3, y and 3, (PUint32(source)^ shr 16) and $ff] or
        DitherTab_g332_44[count and 3, y and 3, (PUint32(source)^ shr 8) and $ff] or
        DitherTab_b332_44[count and 3, y and 3, PUint32(source)^ and $ff];

      Inc(source, 4);
      Inc(dest);
    end;

    Inc(source, iface^.s_add);
    Inc(dest, iface^.d_add);

    Inc(y);
  end;
end;
