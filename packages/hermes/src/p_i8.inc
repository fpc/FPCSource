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

   Generic C converter (from 8 bit indexed) for the HERMES library
   Copyright (c) 1998 Christian Nentwich (c.nentwich@cs.ucl.ac.uk)
   This source code is licensed under the GNU LGPL

   Please refer to the file COPYING.LIB contained in the distribution for
   licensing conditions
}

{ -------------------------------------------------------------------------

                             NORMAL CONVERTERS

  ------------------------------------------------------------------------- }

procedure ConvertP_index8_32(iface: PHermesConverterInterface); cdecl;
var
  i: Integer;
  s_pixel: Uint8;
  d_pixel: Uint32;
  source, dest: PUint8;
begin
  source := iface^.s_pixels;
  dest := iface^.d_pixels;
  repeat
    for i := 0 to iface^.s_width - 1 do
    begin
      s_pixel := source^;
      d_pixel := iface^.lookup[s_pixel];
      PUint32(dest)^ := d_pixel;
      Inc(source);
      Inc(dest, 4);
    end;
    Inc(source, iface^.s_add);
    Inc(dest, iface^.d_add);
    Dec(iface^.s_height);
  until iface^.s_height = 0;
end;

procedure ConvertP_index8_24(iface: PHermesConverterInterface); cdecl;
var
  count: Integer;
  s_pixel, s_pixel2, d_pixel: Uint32;
  d_ptr, source, dest: PUint8;
begin
  d_ptr := PUint8(@d_pixel) + (R_32 - R_24);
  source := iface^.s_pixels;
  dest := iface^.d_pixels;
  repeat
    count := iface^.d_width shr 2;
    while count <> 0 do
    begin
      Dec(count);

      s_pixel := iface^.lookup[source^]; Inc(source);
      s_pixel2 := iface^.lookup[source^]; Inc(source);

      {$IFDEF FPC_LITTLE_ENDIAN}
      s_pixel := s_pixel or (s_pixel2 shl 24);
      {$ELSE FPC_LITTLE_ENDIAN}
      s_pixel := (s_pixel shl 8) or (s_pixel2 shr 16);
      {$ENDIF FPC_LITTLE_ENDIAN}
      PUint32(dest)^ := s_pixel;

      s_pixel := iface^.lookup[source^]; Inc(source);
      {$IFDEF FPC_LITTLE_ENDIAN}
      s_pixel2 := (s_pixel2 shr 8) or (s_pixel shl 16);
      {$ELSE FPC_LITTLE_ENDIAN}
      s_pixel2 := (s_pixel2 shl 16) or (s_pixel shr 8);
      {$ENDIF FPC_LITTLE_ENDIAN}
      PUint32(dest + 4)^ := s_pixel2;

      s_pixel2 := iface^.lookup[source^]; Inc(source);
      {$IFDEF FPC_LITTLE_ENDIAN}
      s_pixel := (s_pixel shr 16) or (s_pixel2 shl 8);
      {$ELSE FPC_LITTLE_ENDIAN}
      s_pixel := (s_pixel shl 24) or s_pixel2;
      {$ENDIF FPC_LITTLE_ENDIAN}
      PUint32(dest + 8)^ := s_pixel;

      Inc(dest, 12);
    end;

    count := iface^.d_width and $3;
    while count <> 0 do
    begin
      Dec(count);
      d_pixel := iface^.lookup[source^]; Inc(source);

      (dest + 0)^ := (d_ptr + 0)^;
      (dest + 1)^ := (d_ptr + 1)^;
      (dest + 2)^ := (d_ptr + 2)^;

      Inc(dest, 3);
    end;
    Inc(source, iface^.s_add);
    Inc(dest, iface^.d_add);
    Dec(iface^.d_height);
  until iface^.d_height = 0;
end;

procedure ConvertP_index8_16(iface: PHermesConverterInterface); cdecl;
var
  source, dest: PUint8;
  count, c: DWord;
begin
  source := iface^.s_pixels;
  dest := iface^.d_pixels;
  repeat
    count := iface^.s_width;
    if (PtrUInt(dest) and $3) <> 0 then
    begin
      PUint16(dest)^ := iface^.lookup[source^]; Inc(source);
      Inc(dest, 2);
      Dec(count);
    end;
    c := count shr 1;
    if c <> 0 then
      repeat
        PUint32(dest)^ := (iface^.lookup[source^] shl DWORD_SMALLINT0_SHL) or
                        (iface^.lookup[(source + 1)^] shl DWORD_SMALLINT1_SHL);
        Inc(dest, 4);
        Inc(source, 2);
        Dec(c);
      until c = 0;
    if (count and 1) <> 0 then
    begin
      PUint16(dest)^ := iface^.lookup[source^];
      Inc(source);
      Inc(dest, 2);
    end;
    Inc(source, iface^.s_add);
    Inc(dest, iface^.d_add);
    Dec(iface^.s_height);
  until iface^.s_height = 0;
end;

procedure ConvertP_index8_8(iface: PHermesConverterInterface); cdecl;
var
  source, dest: PUint8;
  count, c: DWord;
begin
  source := iface^.s_pixels;
  dest := iface^.d_pixels;
  repeat
    count := iface^.s_width;
    if (PtrUInt(dest) and $3) <> 0 then
    begin
      dest^ := iface^.lookup[source^]; Inc(source);
      Inc(dest);
      Dec(count);
    end;
    c := count shr 2;
    if c <> 0 then
      repeat
        PUint32(dest)^ := (iface^.lookup[source^] shl DWORD_BYTE0_SHL) or
                        (iface^.lookup[(source + 1)^] shl DWORD_BYTE1_SHL) or
                        (iface^.lookup[(source + 2)^] shl DWORD_BYTE2_SHL) or
                        (iface^.lookup[(source + 3)^] shl DWORD_BYTE3_SHL);
        Inc(dest, 4);
        Inc(source, 4);
        Dec(c);
      until c = 0;
    count := count and $03;
    while count > 0 do
    begin
      dest^ := iface^.lookup[source^]; Inc(source);
      Inc(dest);
      Dec(count);
    end;
    Inc(source, iface^.s_add);
    Inc(dest, iface^.d_add);
    Dec(iface^.s_height);
  until iface^.s_height = 0;
end;

{ -------------------------------------------------------------------------

                             STRETCH CONVERTERS

  ------------------------------------------------------------------------- }

procedure ConvertP_index8_32_S(iface: PHermesConverterInterface); cdecl;
var
  x, y, count: DWord;
  dx, dy: DWord;
  source: PUint8;
begin
  y := 0;
  dy := (iface^.s_height shl 16) div iface^.d_height;
  dx := (iface^.s_width shl 16) div iface^.d_width;
  source := iface^.s_pixels;
  repeat
    count := iface^.d_width;
    x := 0;

    repeat
      PUint32(iface^.d_pixels)^ := iface^.lookup[(source + (x shr 16))^];
      Inc(x, dx);
      Inc(iface^.d_pixels, 4);
      Dec(count);
    until count = 0;

    { Go to next destination row }
    Inc(iface^.d_pixels, iface^.d_add);

    { Calculate amount of rows to move in source surface }
    Inc(y, dy);

    Inc(source, (y shr 16) * DWord(iface^.s_pitch));
    y := y and $ffff;
    Dec(iface^.d_height);
  until iface^.d_height = 0;
end;

procedure ConvertP_index8_24_S(iface: PHermesConverterInterface); cdecl;
var
  x, y, count: DWord;
  dx, dy: DWord;
  d_ptr, source, dest: PUint8;
  s_pixel, s_pixel2, d_pixel: Uint32;
begin
  d_ptr := PUint8(@d_pixel) + (R_32 - R_24);
  y := 0;
  dy := (iface^.s_height shl 16) div iface^.d_height;
  dx := (iface^.s_width shl 16) div iface^.d_width;
  source := iface^.s_pixels;
  dest := iface^.d_pixels;
  repeat
    x := 0;
    count := iface^.d_width shr 2;
    while count <> 0 do
    begin
      Dec(count);

      s_pixel := iface^.lookup[(source + (x shr 16))^]; Inc(x, dx);
      s_pixel2 := iface^.lookup[(source + (x shr 16))^]; Inc(x, dx);

      {$IFDEF FPC_LITTLE_ENDIAN}
      s_pixel := s_pixel or (s_pixel2 shl 24);
      {$ELSE FPC_LITTLE_ENDIAN}
      s_pixel := (s_pixel shl 8) or (s_pixel2 shr 16);
      {$ENDIF FPC_LITTLE_ENDIAN}
      PUint32(dest)^ := s_pixel;

      s_pixel := iface^.lookup[(source + (x shr 16))^]; Inc(x, dx);
      {$IFDEF FPC_LITTLE_ENDIAN}
      s_pixel2 := (s_pixel2 shr 8) or (s_pixel shl 16);
      {$ELSE FPC_LITTLE_ENDIAN}
      s_pixel2 := (s_pixel2 shl 16) or (s_pixel shr 8);
      {$ENDIF FPC_LITTLE_ENDIAN}
      PUint32(dest + 4)^ := s_pixel2;

      s_pixel2 := iface^.lookup[(source + (x shr 16))^]; Inc(x, dx);
      {$IFDEF FPC_LITTLE_ENDIAN}
      s_pixel := (s_pixel shr 16) or (s_pixel2 shl 8);
      {$ELSE FPC_LITTLE_ENDIAN}
      s_pixel := (s_pixel shl 24) or s_pixel2;
      {$ENDIF FPC_LITTLE_ENDIAN}
      PUint32(dest + 8)^ := s_pixel;

      Inc(dest, 12);
    end;

    count := iface^.d_width and $3;
    while count <> 0 do
    begin
      Dec(count);
      d_pixel := iface^.lookup[(source + (x shr 16))^]; Inc(x, dx);

      (dest + 0)^ := (d_ptr + 0)^;
      (dest + 1)^ := (d_ptr + 1)^;
      (dest + 2)^ := (d_ptr + 2)^;

      Inc(dest, 3);
    end;

    { Go to next destination row }
{    Inc(iface^.d_pixels, iface^.d_add);}
    Inc(dest, iface^.d_add);

    { Calculate amount of rows to move in source surface }
    Inc(y, dy);

    Inc(source, (y shr 16) * DWord(iface^.s_pitch));
    y := y and $ffff;
    Dec(iface^.d_height);
  until iface^.d_height = 0;
end;

{ Quick hack of a index 8 to 16 stretch converter }
procedure ConvertP_index8_16_S(iface: PHermesConverterInterface); cdecl;
var
  x, y, count: DWord;
  dx, dy: DWord;
  source, dest: PUint8;
begin
  y := 0;
  dy := (iface^.s_height shl 16) div iface^.d_height;
  dx := (iface^.s_width shl 16) div iface^.d_width;
  source := iface^.s_pixels;
  dest := iface^.d_pixels;
  repeat
    { do a two pixel at a time loop }

    count := iface^.d_width shr 1;
    x := 0;

    while count <> 0 do
    begin
      Dec(count);
      PUint32(dest)^ := (iface^.lookup[(source + (x shr 16))^] shl DWORD_SMALLINT0_SHL) or
                      (iface^.lookup[(source + ((x + dx) shr 16))^] shl DWORD_SMALLINT1_SHL);
      Inc(x, dx); Inc(x, dx);
      Inc(dest, 4);
    end;

    { Clean up remaining pixel if odd width }
    if (iface^.d_width and 1) <> 0 then
    begin
      PUint16(dest)^ := iface^.lookup[(source + (x shr 16))^];
      Inc(dest, 2);
    end;

    { Go to next destination row }
    Inc(dest, iface^.d_add);

    { Calculate amount of rows to move in source surface }
    Inc(y, dy);

    Inc(source, (y shr 16) * DWord(iface^.s_pitch));
    y := y and $ffff;
    Dec(iface^.d_height);
  until iface^.d_height = 0;
end;

procedure ConvertP_index8_8_S(iface: PHermesConverterInterface); cdecl;
var
  x, y, count, c: DWord;
  dx, dy: DWord;
  source, dest: PUint8;
begin
  y := 0;
  dy := (iface^.s_height shl 16) div iface^.d_height;
  dx := (iface^.s_width shl 16) div iface^.d_width;
  source := iface^.s_pixels;
  dest := iface^.d_pixels;
  repeat
    { do a four pixel at a time loop }

    count := iface^.d_width;
    x := 0;

    while ((PtrUInt(dest) and 3) <> 0) and (count > 0) do
    begin
      Dec(count);
      dest^ := iface^.lookup[(source + (x shr 16))^];
      Inc(x, dx);
      Inc(dest);
    end;

    c := count shr 2;
    count := count and 3;

    while c <> 0 do
    begin
      Dec(c);
      PUint32(dest)^ := (iface^.lookup[(source + (x shr 16))^] shl DWORD_BYTE0_SHL) or
                      (iface^.lookup[(source + ((x + dx) shr 16))^] shl DWORD_BYTE1_SHL) or
                      (iface^.lookup[(source + ((x + (dx shl 1)) shr 16))^] shl DWORD_BYTE2_SHL) or
                      (iface^.lookup[(source + ((x + dx + (dx shl 1)) shr 16))^] shl DWORD_BYTE3_SHL);
      Inc(x, dx shl 2);
      Inc(dest, 4);
    end;

    while count > 0 do
    begin
      Dec(count);
      dest^ := iface^.lookup[(source + (x shr 16))^];
      Inc(dest);
    end;

    { Go to next destination row }
    Inc(dest, iface^.d_add);

    { Calculate amount of rows to move in source surface }
    Inc(y, dy);

    Inc(source, (y shr 16) * DWord(iface^.s_pitch));
    y := y and $ffff;
    Dec(iface^.d_height);
  until iface^.d_height = 0;
end;
