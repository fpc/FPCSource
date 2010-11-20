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
   16 bit to * converters for the HERMES library
   Copyright (c) 1998 Christian Nentwich (c.nentwich@cs.ucl.ac.uk)
   This source code is licensed under the GNU LGPL

   Please refer to the file COPYING.LIB contained in the distribution for
   licensing conditions
}

{ TO 32 BIT RGB }
procedure ConvertP_16rgb565_32rgb888(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_pixel: Uint32;
begin
  repeat
    d_pixel := PUint16(source)^;

    d_pixel := ((d_pixel and $f800) shl 8) or ((d_pixel and $7e0) shl 5) or
               ((d_pixel and $1f) shl 3) or $030103;

    PUint32(dest)^ := d_pixel;

    Inc(source, 2);
    Inc(dest, 4);
    Dec(count);
  until count = 0;
end;

{ TO 32 BIT BGR }
procedure ConvertP_16rgb565_32bgr888(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_pixel: Uint32;
begin
  repeat
    d_pixel := PUint16(source)^;

    d_pixel := ((d_pixel and $f800) shr 8) or ((d_pixel and $7e0) shl 5) or
               ((d_pixel and $1f) shl 19) or $030103;

    PUint32(dest)^ := d_pixel;

    Inc(source, 2);
    Inc(dest, 4);
    Dec(count);
  until count = 0;
end;

{ TO 32 BIT RGBA }
procedure ConvertP_16rgb565_32rgba888(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_pixel: Uint32;
begin
  repeat
    d_pixel := PUint16(source)^;

    d_pixel := ((d_pixel and $f800) shl 8) or ((d_pixel and $7e0) shl 5) or
               ((d_pixel and $1f) shl 3) or $030103;

    PUint32(dest)^ := (d_pixel shl 8) or $ff;

    Inc(source, 2);
    Inc(dest, 4);
    Dec(count);
  until count = 0;
end;

{ TO 32 BIT BGRA }
procedure ConvertP_16rgb565_32bgra888(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_pixel: Uint32;
begin
  repeat
    d_pixel := PUint16(source)^;

    d_pixel := ((d_pixel and $f800) shr 8) or ((d_pixel and $7e0) shl 5) or
               ((d_pixel and $1f) shl 19) or $030103;

    PUint32(dest)^ := (d_pixel shl 8) or $ff;

    Inc(source, 2);
    Inc(dest, 4);
    Dec(count);
  until count = 0;
end;

{ TO 24 BIT RGB }
procedure ConvertP_16rgb565_24rgb888(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_pixel: Uint32;
  d_ptr: PUint8;
begin
  d_ptr := PUint8(@d_pixel) + (R_32 - R_24);
  repeat
    d_pixel := PUint16(source)^;

    d_pixel := ((d_pixel and $f800) shl 8) or ((d_pixel and $7e0) shl 5) or
               ((d_pixel and $1f) shl 3) or $030103;

    (dest + 0)^ := (d_ptr + 0)^;
    (dest + 1)^ := (d_ptr + 1)^;
    (dest + 2)^ := (d_ptr + 2)^;

    Inc(source, 2);
    Inc(dest, 3);
    Dec(count);
  until count = 0;
end;

{ TO 24 BIT BGR }
procedure ConvertP_16rgb565_24bgr888(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_pixel: Uint32;
  d_ptr: PUint8;
begin
  d_ptr := PUint8(@d_pixel) + (R_32 - R_24);
  repeat
    d_pixel := PUint16(source)^;

    d_pixel := ((d_pixel and $f800) shl 8) or ((d_pixel and $7e0) shl 5) or
               ((d_pixel and $1f) shl 3) or $030103;

    { Red and blue are swapped here }
    (dest + 0)^ := (d_ptr + 2)^;
    (dest + 1)^ := (d_ptr + 1)^;
    (dest + 2)^ := (d_ptr + 0)^;

    Inc(source, 2);
    Inc(dest, 3);
    Dec(count);
  until count = 0;
end;

{ TO 16 BIT BGR 565 }
procedure ConvertP_16rgb565_16bgr565(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  i: DWord;
  s_pixel: Uint32;
begin
  { if we are not aligned to a dword, try and convert a single pixel }
  if (PtrUInt(dest) and $3) <> 0 then
  begin
    s_pixel := PUint16(source)^;

    { Swap around R and B, leave G unchanged }
    s_pixel := (s_pixel shr 11) or (s_pixel and $7e0) or
               ((s_pixel shl 11) and $f800);

    PUint16(dest)^ := s_pixel;

    Dec(count);
    Inc(dest, 2); Inc(source, 2);
  end;

  { Now copy blocks of dwords }
  for i := 1 to count shr 1 do
  begin
    s_pixel := PUint32(source)^;

    { Leave G unchanged, shift R to the right and B to the left }
    s_pixel := (s_pixel and $07e007e0) or ((s_pixel and $f800f800) shr 11) or
               ((s_pixel and $001f001f) shl 11);

    PUint32(dest)^ := s_pixel;
    Inc(source, 4); Inc(dest, 4);
  end;

  if (count and 1) <> 0 then
  begin
    s_pixel := PUint16(source)^;

    { Swap around R and B, leave G unchanged }
    s_pixel := (s_pixel shr 11) or (s_pixel and $7e0) or
               ((s_pixel shl 11) and $f800);

    PUint16(dest)^ := s_pixel;
  end;
end;

{ TO 16 BIT RGB 555 }
procedure ConvertP_16rgb565_16rgb555(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  i: DWord;
  s_pixel: Uint32;
begin
  { if we are not aligned to a dword, try and convert a single pixel }
  if (PtrUInt(dest) and $3) <> 0 then
  begin
    s_pixel := PUint16(source)^;

    { Leave blue untouched, mask red and shift by one, mask green and shift
      by one }
    s_pixel := (s_pixel and $1f) or ((s_pixel and $f800) shr 1) or
               ((s_pixel and $7c0) shr 1);

    PUint16(dest)^ := s_pixel;

    Dec(count);
    Inc(dest, 2); Inc(source, 2);
  end;

  { Now copy blocks of dwords }
  for i := 1 to count shr 1 do
  begin
    s_pixel := PUint32(source)^;

    { Leave blue untouched, mask red and shift by one, mask green and shift
      by one }
    s_pixel := (s_pixel and $001f001f) or ((s_pixel and $f800f800) shr 1) or
               ((s_pixel and $07c007c0) shr 1);

    PUint32(dest)^ := s_pixel;
    Inc(source, 4); Inc(dest, 4);
  end;

  if (count and 1) <> 0 then
  begin
    s_pixel := PUint16(source)^;

    { Leave blue untouched, mask red and shift by one, mask green and shift
      by one }
    s_pixel := (s_pixel and $1f) or ((s_pixel and $f800) shr 1) or
               ((s_pixel and $7c0) shr 1);

    PUint16(dest)^ := s_pixel;
  end;
end;

{ TO 16 BIT BGR 555 }
procedure ConvertP_16rgb565_16bgr555(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  i: DWord;
  s_pixel: Uint32;
begin
  { if we are not aligned to a dword, try and convert a single pixel }
  if (PtrUInt(dest) and $3) <> 0 then
  begin
    s_pixel := PUint16(source)^;

    { Shift red right by 11, mask green and shift right one, shift blue
      left ten }
    s_pixel := ((s_pixel and $f800) shr 11) or ((s_pixel and $7c0) shr 1) or
               ((s_pixel and $1f) shl 10);

    PUint16(dest)^ := s_pixel;

    Dec(count);
    Inc(dest, 2); Inc(source, 2);
  end;

  { Now copy blocks of dwords }
  for i := 1 to count shr 1 do
  begin
    s_pixel := PUint32(source)^;

    { Shift red right by 11, mask green and shift right one, shift blue
      left ten }
    s_pixel := ((s_pixel and $f800f800) shr 11) or
               ((s_pixel and $07c007c0) shr 1) or
               ((s_pixel and $001f001f) shl 10);

    PUint32(dest)^ := s_pixel;
    Inc(source, 4); Inc(dest, 4);
  end;

  if (count and 1) <> 0 then
  begin
    s_pixel := PUint16(source)^;

    { Shift red right by 11, mask green and shift right one, shift blue
      left ten }
    s_pixel := ((s_pixel and $f800) shr 11) or ((s_pixel and $7c0) shr 1) or
               ((s_pixel and $1f) shl 10);

    PUint16(dest)^ := s_pixel;
  end;
end;

{ TO 8 BIT RGB 332 }
procedure ConvertP_16rgb565_8rgb332(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  s_block, d_block: Uint32;
  i: DWord;
begin
  { Align to dword first }
  while (PtrUInt(dest) and $3) <> 0 do
  begin
    s_block := PUint16(source)^;

    s_block := ((s_block and $e000) shr 8) or ((s_block and $0700) shr 6) or
               ((s_block and $18) shr 3);

    dest^ := s_block;

    Dec(count);
    if count = 0 then
      exit;

    Inc(source, 2);
    Inc(dest);
  end;

  { Write blocks of four pixels }
  for i := 1 to count shr 2 do
  begin
    { Read and process first two pixels }
    s_block := PUint32(source)^;

    d_block := ((s_block and $e000e000) shr 8) or
               ((s_block and $07000700) shr 6) or
               ((s_block and $00180018) shr 3);
    d_block := (d_block and $ff) or ((d_block and $ff0000) shr 8);

    { and the second two }
    s_block := (PUint32(source)+1)^;

    s_block := ((s_block and $e000e000) shr 8) or
               ((s_block and $07000700) shr 6) or
               ((s_block and $00180018) shr 3);
    s_block := (s_block and $ff) or ((s_block and $ff0000) shr 8);

    { Put it all in one dword and write it }
    d_block := (d_block shl DWORD_SMALLINT0_SHL) or (s_block shl DWORD_SMALLINT1_SHL);

    PUint32(dest)^ := d_block;
    Inc(source, 8);
    Inc(dest, 4);
  end;

  { Clean up remaining pixels }
  count := count and 3;
  while count > 0 do
  begin
    Dec(count);
    s_block := PUint16(source)^;

    dest^ := ((s_block and $e000) shr 8) or ((s_block and $0700) shr 6) or
             ((s_block and $18) shr 3);
    Inc(dest);
    Inc(source, 2);
  end;
end;

{ -------------------------------------------------------------------------

                          STRETCH CONVERTERS

  ------------------------------------------------------------------------- }


procedure ConvertP_16rgb565_32rgb888_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x: DWord;
  p: Uint32;
begin
  x := 0;
  repeat
    p := (((PUint16(source) + (x shr 16))^ and $f800) shl 8) or
         (((PUint16(source) + (x shr 16))^ and $7e0) shl 5) or
         (((PUint16(source) + (x shr 16))^ and $1f) shl 3) or $30103;

    Inc(x, inc_source);

    PUint32(dest)^ := p;

    Inc(dest, 4);
    Dec(count);
  until count = 0;
end;

procedure ConvertP_16rgb565_32bgr888_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_pixel: Uint32;
  x: DWord;
begin
  x := 0;

  repeat
    d_pixel := (PUint16(source) + (x shr 16))^;

    d_pixel := ((d_pixel and $f800) shr 8) or ((d_pixel and $7e0) shl 5) or
               ((d_pixel and $1f) shl 19) or $30103;

    PUint32(dest)^ := d_pixel;

    Inc(dest, 4);
    Inc(x, inc_source);
    Dec(count);
  until count = 0;
end;

procedure ConvertP_16rgb565_32rgba888_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x: DWord;
  p: Uint32;
begin
  x := 0;
  repeat
    p := (((PUint16(source) + (x shr 16))^ and $f800) shl (8+8)) or
         (((PUint16(source) + (x shr 16))^ and $7e0) shl (5+8)) or
         (((PUint16(source) + (x shr 16))^ and $1f) shl (3+8)) or $30103ff;

    Inc(x, inc_source);

    PUint32(dest)^ := p;

    Inc(dest, 4);
    Dec(count);
  until count = 0;
end;

procedure ConvertP_16rgb565_32bgra888_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_pixel: Uint32;
  x: DWord;
begin
  x := 0;

  repeat
    d_pixel := (PUint16(source) + (x shr 16))^;

    d_pixel := ((d_pixel and $f800) {Shr 8}) or ((d_pixel and $7e0) shl (5+8)) or
               ((d_pixel and $1f) shl (19+8)) or $30103ff;

    PUint32(dest)^ := d_pixel;

    Inc(dest, 4);
    Inc(x, inc_source);
    Dec(count);
  until count = 0;
end;

procedure ConvertP_16rgb565_24rgb888_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x: DWord;
  p1, p2, p3, p4: DWord;
  c: DWord;
begin
  x := 0;
  while (PtrUInt(dest) and 3) <> 0 do
  begin
    p1 := (((PUint16(source) + (x shr 16))^ and $f800) shl 8) or
          (((PUint16(source) + (x shr 16))^ and $7e0) shl 5) or
          (((PUint16(source) + (x shr 16))^ and $1f) shl 3) or $30103;
    (dest + B_24)^ := p1 and $FF;
    (dest + G_24)^ := (p1 shr 8) and $FF;
    (dest + R_24)^ := p1 shr 16;

    Inc(dest, 3);
    Inc(x, inc_source);
    Dec(count);
    if count = 0 then
      exit;
  end;

  c := count shr 2;
  while c > 0 do
  begin
    p1 := (PUint16(source) + (x shr 16))^;
    p2 := (PUint16(source) + ((x + inc_source) shr 16))^;
    p3 := (PUint16(source) + ((x + 2*inc_source) shr 16))^;
    p4 := (PUint16(source) + ((x + 3*inc_source) shr 16))^;

    {$IFDEF FPC_LITTLE_ENDIAN}
    PUint32(dest + 0)^ := ((p2 and $001F) shl 27) or ((p1 and $F800) shl 8) or ((p1 and $07E0) shl 5) or ((p1 and $001F) shl 3) or $03030103;
    PUint32(dest + 4)^ := ((p3 and $07E0) shl 21) or ((p3 and $001F) shl 19) or (p2 and $F800) or ((p2 and $07E0) shr 3) or $01030301;
    PUint32(dest + 8)^ := ((p4 and $F800) shl 16) or ((p4 and $07E0) shl 13) or ((p4 and $001F) shl 11) or ((p3 and $F800) shr 8) or $03010303;
    {$ELSE FPC_LITTLE_ENDIAN}
    PUint32(dest + 0)^ := ((p1 and $F800) shl 16) or ((p1 and $07E0) shl 13) or ((p1 and $001F) shl 11) or ((p2 and $F800) shr 8) or $03010303;
    PUint32(dest + 4)^ := ((p2 and $07E0) shl 21) or ((p2 and $001F) shl 19) or (p3 and $F800) or ((p3 and $07E0) shr 3) or $01030301;
    PUint32(dest + 8)^ := ((p3 and $001F) shl 27) or ((p4 and $F800) shl 8) or ((p4 and $07E0) shl 5) or ((p4 and $001F) shl 3) or $03030103;
    {$ENDIF FPC_LITTLE_ENDIAN}

    Inc(x, 4*inc_source);
    Inc(dest, 12);
    Dec(c);
  end;

  count := count and 3;
  while count > 0 do
  begin
    p1 := (((PUint16(source) + (x shr 16))^ and $f800) shl 8) or
          (((PUint16(source) + (x shr 16))^ and $7e0) shl 5) or
          (((PUint16(source) + (x shr 16))^ and $1f) shl 3) or $30103;
    (dest + B_24)^ := p1 and $FF;
    (dest + G_24)^ := (p1 shr 8) and $FF;
    (dest + R_24)^ := p1 shr 16;

    Inc(dest, 3);
    Inc(x, inc_source);
    Dec(count);
  end;
end;

procedure ConvertP_16rgb565_24bgr888_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x: DWord;
  p1, p2, p3, p4: DWord;
  c: DWord;
begin
  x := 0;
  while (PtrUInt(dest) and 3) <> 0 do
  begin
    p1 := (((PUint16(source) + (x shr 16))^ and $f800) shr 8) or
          (((PUint16(source) + (x shr 16))^ and $7e0) shl 5) or
          (((PUint16(source) + (x shr 16))^ and $1f) shl 19) or $30103;
    (dest + B_24)^ := p1 and $FF;
    (dest + G_24)^ := (p1 shr 8) and $FF;
    (dest + R_24)^ := p1 shr 16;

    Inc(dest, 3);
    Inc(x, inc_source);
    Dec(count);
    if count = 0 then
      exit;
  end;

  c := count shr 2;
  while c > 0 do
  begin
    p1 := (PUint16(source) + (x shr 16))^;
    p2 := (PUint16(source) + ((x + inc_source) shr 16))^;
    p3 := (PUint16(source) + ((x + 2*inc_source) shr 16))^;
    p4 := (PUint16(source) + ((x + 3*inc_source) shr 16))^;

    {$IFDEF FPC_LITTLE_ENDIAN}
    PUint32(dest + 0)^ := ((p2 and $F800) shl 16) or ((p1 and $001F) shl 19) or ((p1 and $07E0) shl 5) or ((p1 and $F800) shr 8) or $03030103;
    PUint32(dest + 4)^ := ((p3 and $07E0) shl 21) or ((p3 and $F800) shl 8) or ((p2 and $001F) shl 11) or ((p2 and $07E0) shr 3) or $01030301;
    PUint32(dest + 8)^ := ((p4 and $001F) shl 27) or ((p4 and $07E0) shl 13) or (p4 and $F800) or ((p3 and $001F) shl 3) or $03010303;
    {$ELSE FPC_LITTLE_ENDIAN}
    PUint32(dest + 0)^ := ((p1 and $001F) shl 27) or ((p1 and $07E0) shl 13) or (p1 and $F800) or ((p2 and $001F) shl 3) or $03010303;
    PUint32(dest + 4)^ := ((p2 and $07E0) shl 21) or ((p2 and $F800) shl 8) or ((p3 and $001F) shl 11) or ((p3 and $07E0) shr 3) or $01030301;
    PUint32(dest + 8)^ := ((p3 and $F800) shl 16) or ((p4 and $001F) shl 19) or ((p4 and $07E0) shl 5) or ((p4 and $F800) shr 8) or $03030103;
    {$ENDIF FPC_LITTLE_ENDIAN}

    Inc(x, 4*inc_source);
    Inc(dest, 12);
    Dec(c);
  end;

  count := count and 3;
  while count > 0 do
  begin
    p1 := (((PUint16(source) + (x shr 16))^ and $f800) shr 8) or
          (((PUint16(source) + (x shr 16))^ and $7e0) shl 5) or
          (((PUint16(source) + (x shr 16))^ and $1f) shl 19) or $30103;
    (dest + B_24)^ := p1 and $FF;
    (dest + G_24)^ := (p1 shr 8) and $FF;
    (dest + R_24)^ := p1 shr 16;

    Inc(dest, 3);
    Inc(x, inc_source);
    Dec(count);
  end;
end;

procedure ConvertP_16rgb565_16bgr565_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x, c: DWord;
  p: Uint32;
begin
  x := 0;
  { if we are not aligned to a dword, try and convert a single pixel }
  if (PtrUInt(dest) and $3) <> 0 then
  begin
    { Swap r and b, leave g untouched }
    PUint16(dest)^ := ((PUint16(source) + (x shr 16))^ shr 11) or
                       ((PUint16(source) + (x shr 16))^ and $7e0) or
                       (((PUint16(source) + (x shr 16))^ shl 11) and $f800);
    Inc(x, inc_source);
    Inc(dest, 2);
    Dec(count);
  end;

  c := count shr 1;
  while c <> 0 do
  begin
    Dec(c);

    { Swap r and b, leave g untouched }
    p := (((PUint16(source) + (x shr 16))^ shr 11) or
          ((PUint16(source) + (x shr 16))^ and $7e0) or
          (((PUint16(source) + (x shr 16))^ shl 11) and $f800)) shl DWORD_SMALLINT0_SHL;
    Inc(x, inc_source);

    p := p or ((((PUint16(source) + (x shr 16))^ shr 11) or
                ((PUint16(source) + (x shr 16))^ and $7e0) or
                (((PUint16(source) + (x shr 16))^ shl 11) and $f800)) shl DWORD_SMALLINT1_SHL);
    Inc(x, inc_source);

    PUint32(dest)^ := p;
    Inc(dest, 4);
  end;

  if (count and 1) <> 0 then
    PUint16(dest)^ := ((PUint16(source) + (x shr 16))^ shr 11) or
                       ((PUint16(source) + (x shr 16))^ and $7e0) or
                       (((PUint16(source) + (x shr 16))^ shl 11) and $f800);
end;

procedure ConvertP_16rgb565_16rgb555_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x, c: DWord;
  p: Uint32;
begin
  x := 0;
  { if we are not aligned to a dword, try and convert a single pixel }
  if (PtrUInt(dest) and $3) <> 0 then
  begin
    PUint16(dest)^ := (((PUint16(source) + (x shr 16))^ and $f800) shr 1) or
                       (((PUint16(source) + (x shr 16))^ and $7c0) shr 1) or
                       ((PUint16(source) + (x shr 16))^ and $1f);
    Inc(dest, 2);
    Inc(x, inc_source);
    Dec(count);
  end;

  c := count shr 1;
  while c <> 0 do
  begin
    Dec(c);

    { Leave blue untouched, mask red and shift by one, mask green and shift
      by one }
    p := ((((PUint16(source) + (x shr 16))^ and $f800) shr 1) or
          (((PUint16(source) + (x shr 16))^ and $7c0) shr 1) or
          ((PUint16(source) + (x shr 16))^ and $1f)) shl DWORD_SMALLINT0_SHL;
    Inc(x, inc_source);

    p := p or (((((PUint16(source) + (x shr 16))^ and $f800) shr 1) or
                (((PUint16(source) + (x shr 16))^ and $7c0) shr 1) or
                ((PUint16(source) + (x shr 16))^ and $1f)) shl DWORD_SMALLINT1_SHL);
    Inc(x, inc_source);

    PUint32(dest)^ := p;
    Inc(dest, 4);
  end;

  if (count and 1) <> 0 then
    PUint16(dest)^ := (((PUint16(source) + (x shr 16))^ and $f800) shr 1) or
                       (((PUint16(source) + (x shr 16))^ and $7c0) shr 1) or
                       ((PUint16(source) + (x shr 16))^ and $1f);
end;

procedure ConvertP_16rgb565_16bgr555_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x, c: DWord;
  p: Uint32;
begin
  x := 0;
  { if we are not aligned to a dword, try and convert a single pixel }
  if (PtrUInt(dest) and $3) <> 0 then
  begin
    PUint16(dest)^ := (((PUint16(source) + (x shr 16))^ and $f800) shr 11) or
                       (((PUint16(source) + (x shr 16))^ and $7c0) shr 1) or
                       (((PUint16(source) + (x shr 16))^ and $1f) shl 10);
    Inc(dest, 2);
    Inc(x, inc_source);
    Dec(count);
  end;

  c := count shr 1;
  while c <> 0 do
  begin
    Dec(c);

    p := ((((PUint16(source) + (x shr 16))^ and $f800) shr 11) or
          (((PUint16(source) + (x shr 16))^ and $7c0) shr 1) or
          (((PUint16(source) + (x shr 16))^ and $1f) shl 10)) shl DWORD_SMALLINT0_SHL;
    Inc(x, inc_source);

    p := p or (((((PUint16(source) + (x shr 16))^ and $f800) shr 11) or
                (((PUint16(source) + (x shr 16))^ and $7c0) shr 1) or
                (((PUint16(source) + (x shr 16))^ and $1f) shl 10)) shl DWORD_SMALLINT1_SHL);
    Inc(x, inc_source);

    PUint32(dest)^ := p;
    Inc(dest, 4);
  end;

  if (count and 1) <> 0 then
    PUint16(dest)^ := (((PUint16(source) + (x shr 16))^ and $f800) shr 11) or
                       (((PUint16(source) + (x shr 16))^ and $7c0) shr 1) or
                       (((PUint16(source) + (x shr 16))^ and $1f) shl 10);
end;

procedure ConvertP_16rgb565_8rgb332_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x, c: DWord;
  p: Uint32;
begin
  x := 0;

  { Write single pixels until the destination address is aligned mod 4 }
  while (PtrUInt(dest) and $3) <> 0 do
  begin
    dest^ := (((PUint16(source) + (x shr 16))^ shr 8) and $e0) or
             (((PUint16(source) + (x shr 16))^ shr 6) and $1c) or
             (((PUint16(source) + (x shr 16))^ shr 3) and $3);
    Inc(x, inc_source);
    Inc(dest);
    Dec(count);
    if count = 0 then
      exit;
  end;


  {* Write blocks of four pixels now }
  c := count shr 2;
  while c <> 0 do
  begin
    Dec(c);
    p := ((((PUint16(source) + (x shr 16))^ shr 8) and $e0) or
          (((PUint16(source) + (x shr 16))^ shr 6) and $1c) or
          (((PUint16(source) + (x shr 16))^ shr 3) and $3)) shl DWORD_BYTE0_SHL;
    Inc(x, inc_source);

    p := p or
      (((((PUint16(source) + (x shr 16))^ shr 8) and $e0) or
        (((PUint16(source) + (x shr 16))^ shr 6) and $1c) or
        (((PUint16(source) + (x shr 16))^ shr 3) and $3)) shl DWORD_BYTE1_SHL);
    Inc(x, inc_source);

    p := p or
      (((((PUint16(source) + (x shr 16))^ shr 8) and $e0) or
        (((PUint16(source) + (x shr 16))^ shr 6) and $1c) or
        (((PUint16(source) + (x shr 16))^ shr 3) and $3)) shl DWORD_BYTE2_SHL);
    Inc(x, inc_source);

    p := p or
      (((((PUint16(source) + (x shr 16))^ shr 8) and $e0) or
        (((PUint16(source) + (x shr 16))^ shr 6) and $1c) or
        (((PUint16(source) + (x shr 16))^ shr 3) and $3)) shl DWORD_BYTE3_SHL);
    Inc(x, inc_source);

    PUint32(dest)^ := p;

    Inc(dest, 4);
  end;

  { Write up to three trailing pixels }
  c := count and $3;
  while c <> 0 do
  begin
    Dec(c);
    dest^ := (((PUint16(source) + (x shr 16))^ shr 8) and $e0) or
             (((PUint16(source) + (x shr 16))^ shr 6) and $1c) or
             (((PUint16(source) + (x shr 16))^ shr 3) and $3);
    Inc(x, inc_source);
    Inc(dest);
  end;
end;
