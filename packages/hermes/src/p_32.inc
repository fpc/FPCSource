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
   32 bit to * converters for the HERMES library
   Copyright (c) 1998 Christian Nentwich (c.nentwich@cs.ucl.ac.uk)
   This source code is licensed under the GNU LGPL

   Please refer to the file COPYING.LIB contained in the distribution for
   licensing conditions
}

{ -------------------------------------------------------------------------

                             NORMAL CONVERTERS

  ------------------------------------------------------------------------- }


{ TO 32 BGR }
procedure ConvertP_32rgb888_32bgr888(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  s_pixel: Uint32;
  s_ptr: PUint8;
  tmp: Uint8;
begin
  s_ptr := @s_pixel;
  repeat
    s_pixel := PUint32(source)^;

    tmp := (s_ptr + R_32)^;
    (s_ptr + R_32)^ := (s_ptr + B_32)^;
    (s_ptr + B_32)^ := tmp;

    PUint32(dest)^ := s_pixel;

    Inc(dest, 4);
    Inc(source, 4);
    Dec(count);
  until count = 0;
end;

{ 32 RGBA }
procedure ConvertP_32rgb888_32rgba888(source, dest: PUint8; count, inc_source: DWord); cdecl;
begin
  repeat
    PUint32(dest)^ := (PUint32(source)^ shl 8) or $ff;

    Inc(dest, 4);
    Inc(source, 4);
    Dec(count);
  until count = 0;
end;

{ 32 BGRA }
procedure ConvertP_32rgb888_32bgra888(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  s_pixel: Uint32;
  s_ptr: PUint8;
  tmp: Uint8;
begin
  s_ptr := @s_pixel;
  repeat
    s_pixel := PUint32(source)^;

    tmp := (s_ptr + R_32)^;
    (s_ptr + R_32)^ := (s_ptr + B_32)^;
    (s_ptr + B_32)^ := tmp;

    PUint32(dest)^ := (s_pixel shl 8) or $ff;

    Inc(dest, 4);
    Inc(source, 4);
    Dec(count);
  until count = 0;
end;

{ TO 24 RGB }
procedure ConvertP_32rgb888_24rgb888(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  s_pixel, s_pixel2: Uint32;
  s_point: PUint8;
  c: DWord;
begin
  s_point := PUint8(@s_pixel) + (R_32 - R_24);

  { Align mod 4 (quite important in this case.. ) }

  while (PtrUInt(dest) and $3) <> 0 do
  begin
    s_pixel := PUint32(source)^;

    (dest + 0)^ := (s_point + 0)^;
    (dest + 1)^ := (s_point + 1)^;
    (dest + 2)^ := (s_point + 2)^;

    Inc(source, 4);
    Inc(dest, 3);

    Dec(count);
    if count = 0 then
      exit;
  end;

  { Main loop. }

  c := count shr 2;
  while c <> 0 do
  begin
    Dec(c);
    s_pixel := PUint32(source)^;
    s_pixel2 := (PUint32(source) + 1)^;

    {$IFDEF FPC_LITTLE_ENDIAN}
    s_pixel := (s_pixel and $ffffff) or (s_pixel2 shl 24);
    {$ELSE FPC_LITTLE_ENDIAN}
    s_pixel := (s_pixel shl 8) or ((s_pixel2 shr 16) and $FF);
    {$ENDIF FPC_LITTLE_ENDIAN}
    PUint32(dest)^ := s_pixel;

    s_pixel := (PUint32(source) + 2)^;
    {$IFDEF FPC_LITTLE_ENDIAN}
    s_pixel2 := ((s_pixel2 shr 8) and $ffff) or (s_pixel shl 16);
    {$ELSE FPC_LITTLE_ENDIAN}
    s_pixel2 := (s_pixel2 shl 16) or ((s_pixel shr 8) and $FFFF);
    {$ENDIF FPC_LITTLE_ENDIAN}
    (PUint32(dest) + 1)^ := s_pixel2;

    s_pixel2 := (PUint32(source) + 3)^;
    {$IFDEF FPC_LITTLE_ENDIAN}
    s_pixel := ((s_pixel shr 16) and $ff) or (s_pixel2 shl 8);
    {$ELSE FPC_LITTLE_ENDIAN}
    s_pixel := (s_pixel shl 24) or (s_pixel2 and $FFFFFF);
    {$ENDIF FPC_LITTLE_ENDIAN}
    (PUint32(dest) + 2)^ := s_pixel;

    Inc(source, 16);
    Inc(dest, 12);
  end;

  { Convert trailing pixels }

  count := count and $3;
  while count <> 0 do
  begin
    Dec(count);
    s_pixel := PUint32(source)^;

    (dest + 0)^ := (s_point + 0)^;
    (dest + 1)^ := (s_point + 1)^;
    (dest + 2)^ := (s_point + 2)^;

    Inc(source, 4);
    Inc(dest, 3);
  end;
end;

{ TO 24 BGR }
procedure ConvertP_32rgb888_24bgr888(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  s_pixel: Uint32;
  s_point: PUint8;
begin
  s_point := PUint8(@s_pixel) + (R_32 - R_24);
  repeat
    s_pixel := PUint32(source)^;

    { Note that R and B are swapped }
    (dest + 0)^ := (s_point + 2)^;
    (dest + 1)^ := (s_point + 1)^;
    (dest + 2)^ := (s_point + 0)^;

    Inc(source, 4);
    Inc(dest, 3);
    Dec(count);
  until count = 0;
end;

{ TO 16 RGB 565 }
procedure ConvertP_32rgb888_16rgb565(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  i: DWord;
  s_pixel: Uint32;
  source_32, dest_32: PUint32;
begin
  dest_32 := PUint32(dest);
  source_32 := PUint32(source);

  { if the current pixel isn't dword aligned, try write one pixel first }
  if (PtrUInt(dest_32) and $3) <> 0 then
  begin
    s_pixel := ((source_32^ shr 8) and $f800) or
               ((source_32^ shr 5) and $7e0) or
               ((source_32^ shr 3) and $1f);

    PUint16(dest_32)^ := s_pixel;

    dest_32 := PUint32(dest + 2);
    Inc(source_32);

    Dec(count);
  end;

  { Write blocks of two pixels }

  for i := 1 to count shr 1 do
  begin
    {This horrible construct is actually faster than loading into a variable}
    {$IFDEF FPC_LITTLE_ENDIAN}
    dest_32^ := ((source_32^ shr 8) and $f800) or
                ((source_32^ shr 5) and $7e0) or
                ((source_32^ shr 3) and $1f) or
                (((source_32 + 1)^ shl 8) and $f8000000) or
                (((source_32 + 1)^ shl 11) and $7e00000) or
                (((source_32 + 1)^ shl 13) and $1f0000);
    {$ELSE FPC_LITTLE_ENDIAN}
    dest_32^ := ((source_32^ shl 8) and $f8000000) or
                ((source_32^ shl 11) and $7e00000) or
                ((source_32^ shl 13) and $1f0000) or
                (((source_32 + 1)^ shr 8) and $f800) or
                (((source_32 + 1)^ shr 5) and $7e0) or
                (((source_32 + 1)^ shr 3) and $1f);
    {$ENDIF FPC_LITTLE_ENDIAN}

    Inc(dest_32);
    Inc(source_32, 2);
  end;

  { Eventually, write a single odd pixel that might be left }
  if (count and 1) <> 0 then
  begin
    s_pixel := source_32^;
    PUint16(dest_32)^ := ((s_pixel shr 8) and $f800) or
                          ((s_pixel shr 5) and $7e0) or
                          ((s_pixel shr 3) and $1f);
  end;
end;

{ TO 16 BGR 565 }
procedure ConvertP_32rgb888_16bgr565(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  i: DWord;
  r, g, b: Uint32;
  s_pixel, d_pixelblock: Uint32;
  d_pixel: Uint16;
begin
  { if the current pixel isn't dword aligned, try write one pixel first }
  if (PtrUInt(dest) and $3) <> 0 then
  begin
    s_pixel := PUint32(source)^;

    r := (s_pixel shr 19) and $1f;
    g := (s_pixel shr 5) and $7e0;
    b := (s_pixel shl 8) and $f800;

    d_pixel := r or g or b;

    PUint16(dest)^ := d_pixel;

    Inc(source, 4);
    Inc(dest, 2);
    Dec(count);
  end;

  { Write blocks of two pixels }
  for i := 1 to count shr 1 do
  begin
    s_pixel := PUint32(source)^;

    d_pixelblock := (((s_pixel shr 19) and $1f) or
                     ((s_pixel shr 5) and $7e0) or
                     ((s_pixel shl 8) and $f800)) shl DWORD_SMALLINT0_SHL;

    s_pixel := (PUint32(source) + 1)^;

    d_pixelblock := d_pixelblock or
                  ((((s_pixel shr 19) and $1f) or
                    ((s_pixel shr 5) and $7e0) or
                    ((s_pixel shl 8) and $f800)) shl DWORD_SMALLINT1_SHL);

    PUint32(dest)^ := d_pixelblock;
    Inc(source, 8);
    Inc(dest, 4);
  end;

  { Eventually, write a single odd pixel that might be left }
  if (count and 1) <> 0 then
  begin
    s_pixel := PUint32(source)^;

    r := (s_pixel shr 19) and $1f;
    g := (s_pixel shr 5) and $7e0;
    b := (s_pixel shl 8) and $f800;

    d_pixel := r or g or b;

    PUint16(dest)^ := d_pixel;
  end;
end;

{ TO 16 RGB 555 }
procedure ConvertP_32rgb888_16rgb555(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  r, g, b: Uint32;
  s_pixel, d_pixelblock: Uint32;
  d_pixel: Uint16;
  i: DWord;
begin
  if (PtrUInt(dest) and $3) <> 0 then
  begin
    s_pixel := PUint32(source)^;

    r := (s_pixel shr 9) and $7c00;
    g := (s_pixel shr 6) and $3e0;
    b := (s_pixel shr 3) and $1f;

    d_pixel := r or g or b;

    PUint16(dest)^ := d_pixel;

    Inc(source, 4);
    Inc(dest, 2);
    Dec(count);
  end;

  for i := 1 to count shr 1 do
  begin
    s_pixel := PUint32(source)^;

    d_pixelblock := (((s_pixel shr 9) and $7c00) or
                     ((s_pixel shr 6) and $3e0) or
                     ((s_pixel shr 3) and $1f)) shl DWORD_SMALLINT0_SHL;

    s_pixel := (PUint32(source) + 1)^;

    d_pixelblock := d_pixelblock or
                  ((((s_pixel shr 9) and $7c00) or
                    ((s_pixel shr 6) and $3e0) or
                    ((s_pixel shr 3) and $1f)) shl DWORD_SMALLINT1_SHL);

    PUint32(dest)^ := d_pixelblock;
    Inc(source, 8);
    Inc(dest, 4);
  end;

  if (count and 1) <> 0 then
  begin
    s_pixel := PUint32(source)^;

    r := (s_pixel shr 9) and $7c00;
    g := (s_pixel shr 6) and $3e0;
    b := (s_pixel shr 3) and $1f;

    d_pixel := r or g or b;

    PUint16(dest)^ := d_pixel;
  end;
end;

{ TO 16 BGR 555 }
procedure ConvertP_32rgb888_16bgr555(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  r, g, b: Uint32;
  s_pixel, d_pixelblock: Uint32;
  d_pixel: Uint16;
  i: DWord;
begin
  if (PtrUInt(dest) and $3) <> 0 then
  begin
    s_pixel := PUint32(source)^;

    r := (s_pixel shr 19) and $1f;
    g := (s_pixel shr 6) and $3e0;
    b := (s_pixel shl 7) and $7c00;

    d_pixel := r or g or b;

    PUint16(dest)^ := d_pixel;

    Inc(source, 4);
    Inc(dest, 2);
    Dec(count);
  end;

  for i := 1 to count shr 1 do
  begin
    s_pixel := PUint32(source)^;

    d_pixelblock := (((s_pixel shr 19) and $1f) or
                     ((s_pixel shr 6) and $3e0) or
                     ((s_pixel shl 7) and $7c00)) shl DWORD_SMALLINT0_SHL;

    s_pixel := (PUint32(source) + 1)^;

    d_pixelblock := d_pixelblock or
                  ((((s_pixel shr 19) and $1f) or
                    ((s_pixel shr 6) and $3e0) or
                    ((s_pixel shl 7) and $7c00)) shl DWORD_SMALLINT1_SHL);

    PUint32(dest)^ := d_pixelblock;
    Inc(source, 8);
    Inc(dest, 4);
  end;

  if (count and 1) <> 0 then
  begin
    s_pixel := PUint32(source)^;

    r := (s_pixel shr 19) and $1f;
    g := (s_pixel shr 6) and $3e0;
    b := (s_pixel shl 7) and $7c00;

    d_pixel := r or g or b;

    PUint16(dest)^ := d_pixel;
  end;
end;

{ TO 8 RGB 332 }
procedure ConvertP_32rgb888_8rgb332(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  i: DWord;
  s_pixel, d_block: Uint32;
  d_pixel: Uint8;
begin
  { Process single pixels until we are dword aligned }
  while (PtrUInt(dest) and $3) <> 0 do
  begin
    s_pixel := PUint32(source)^;

    d_pixel := ((s_pixel shr 16) and $e0) or
               ((s_pixel shr 11) and $1c) or
               ((s_pixel shr 6) and $3);

    dest^ := d_pixel;

    Dec(count);
    if count = 0 then
      exit;
    Inc(dest);
    Inc(source, 4);
  end;

  { Now process blocks of four pixels }
  for i := 1 to count shr 2 do
  begin
    s_pixel := PUint32(source)^;
    d_block := (((s_pixel shr 16) and $e0) or
                ((s_pixel shr 11) and $1c) or
                ((s_pixel shr 6) and $3)) shl DWORD_BYTE0_SHL;

    s_pixel := (PUint32(source) + 1)^;
    d_block := ((((s_pixel shr 16) and $e0) or
                 ((s_pixel shr 11) and $1c) or
                 ((s_pixel shr 6) and $3)) shl DWORD_BYTE1_SHL) or d_block;

    s_pixel := (PUint32(source) + 2)^;
    d_block := ((((s_pixel shr 16) and $e0) or
                 ((s_pixel shr 11) and $1c) or
                 ((s_pixel shr 6) and $3)) shl DWORD_BYTE2_SHL) or d_block;

    s_pixel := (PUint32(source) + 3)^;
    d_block := ((((s_pixel shr 16) and $e0) or
                 ((s_pixel shr 11) and $1c) or
                 ((s_pixel shr 6) and $3)) shl DWORD_BYTE3_SHL) or d_block;

    PUint32(dest)^ := d_block;
    Inc(source, 16);
    Inc(dest, 4);
  end;

  { Write all possibly remaining pixel }
  count := count and $3;
  while count <> 0 do
  begin
    Dec(count);
    s_pixel := PUint32(source)^;

    dest^ := ((s_pixel shr 16) and $e0) or
             ((s_pixel shr 11) and $1c) or
             ((s_pixel shr 6) and $3);

    Inc(dest);
    Inc(source, 4);
  end;
end;

{ -------------------------------------------------------------------------

                             STRETCH CONVERTERS

  ------------------------------------------------------------------------- }

procedure ConvertP_32rgb888_32bgr888_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x: DWord;
  s_pixel: Uint32;
  s_ptr: PUint8;
  tmp: Uint8;
begin
  x := 0;
  s_ptr := @s_pixel;
  repeat
    s_pixel := (PUint32(source)+(x shr 16))^;

    tmp := (s_ptr + R_32)^;
    (s_ptr + R_32)^ := (s_ptr + B_32)^;
    (s_ptr + B_32)^ := tmp;

    PUint32(dest)^ := s_pixel;

    Inc(dest, 4);
    Inc(x, inc_source);
    Dec(count);
  until count = 0;
end;

procedure ConvertP_32rgb888_32rgba888_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x: DWord;
begin
  x := 0;
  repeat
    PUint32(dest)^ := ((PUint32(source) + (x shr 16))^ shl 8) or $ff;

    Inc(dest, 4);
    Inc(x, inc_source);
    Dec(count);
  until count = 0;
end;

procedure ConvertP_32rgb888_32bgra888_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x: DWord;
  s_pixel: Uint32;
  s_ptr: PUint8;
  tmp: Uint8;
begin
  x := 0;
  s_ptr := @s_pixel;
  repeat
    s_pixel := (PUint32(source)+(x shr 16))^;

    tmp := (s_ptr + R_32)^;
    (s_ptr + R_32)^ := (s_ptr + B_32)^;
    (s_ptr + B_32)^ := tmp;

    PUint32(dest)^ := (s_pixel shl 8) or $ff;

    Inc(dest, 4);
    Inc(x, inc_source);
    Dec(count);
  until count = 0;
end;

procedure ConvertP_32rgb888_24rgb888_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
const
  endian_fixup = R_32 - R_24;
var
  x, c: DWord;
  s1, s2: DWord;
begin
  x := 0;
  while (PtrUInt(dest) and 3) <> 0 do
  begin
    (dest + 0)^ := (source + 0 + endian_fixup)^;
    (dest + 1)^ := (source + 1 + endian_fixup)^;
    (dest + 2)^ := (source + 2 + endian_fixup)^;

    Inc(x, inc_source);
    Inc(source, (x shr 16)*4);
    x := x and $FFFF;

    Inc(dest, 3);
    Dec(count);
    if count = 0 then
      exit;
  end;

  c := count shr 2;
  while c > 0 do
  begin
    {$IFDEF FPC_LITTLE_ENDIAN}
    s1 := (PUint32(source) + ((x + inc_source) shr 16))^ and $FFFFFF;
    PUint32(dest)^ := ((PUint32(source) + (x shr 16))^ and $FFFFFF) or (s1 shl 24);
    s2 := (PUint32(source) + ((x + 2*inc_source) shr 16))^ and $FFFFFF;
    PUint32(dest + 4)^ := (s1 shr 8) or (s2 shl 16);
    PUint32(dest + 8)^ := (s2 shr 16) or ((PUint32(source) + ((x + 3*inc_source) shr 16))^ shl 8);
    {$ELSE FPC_LITTLE_ENDIAN}
    s1 := (PUint32(source) + ((x + inc_source) shr 16))^ and $FFFFFF;
    PUint32(dest)^ := ((PUint32(source) + (x shr 16))^ shl 8) or (s1 shr 16);
    s2 := (PUint32(source) + ((x + 2*inc_source) shr 16))^ and $FFFFFF;
    PUint32(dest + 4)^ := (s1 shl 16) or (s2 shr 8);
    PUint32(dest + 8)^ := (s2 shl 24) or ((PUint32(source) + ((x + 3*inc_source) shr 16))^ and $FFFFFF);
    {$ENDIF FPC_LITTLE_ENDIAN}

    Inc(x, 4*inc_source);

    Inc(dest, 12);
    Dec(c);
  end;
  Inc(source, (x shr 16) * 4);
  x := x and $FFFF;

  count := count and $3;
  while count > 0 do
  begin
    (dest + 0)^ := (source + 0 + endian_fixup)^;
    (dest + 1)^ := (source + 1 + endian_fixup)^;
    (dest + 2)^ := (source + 2 + endian_fixup)^;

    Inc(x, inc_source);
    Inc(source, (x shr 16)*4);
    x := x and $FFFF;

    Inc(dest, 3);
    Dec(count);
  end;
end;

procedure ConvertP_32rgb888_24bgr888_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
const
  endian_fixup = R_32 - R_24;
var
  x, c: DWord;
  s1, s2, s3, s4: DWord;
begin
  x := 0;
  while (PtrUInt(dest) and 3) <> 0 do
  begin
    (dest + 0)^ := (source + 2 + endian_fixup)^;
    (dest + 1)^ := (source + 1 + endian_fixup)^;
    (dest + 2)^ := (source + 0 + endian_fixup)^;

    Inc(x, inc_source);
    Inc(source, (x shr 16)*4);
    x := x and $FFFF;

    Inc(dest, 3);
    Dec(count);
    if count = 0 then
      exit;
  end;

  c := count shr 2;
  while c > 0 do
  begin
    s1 := (PUint32(source) + (x shr 16))^;
    s2 := (PUint32(source) + ((x + inc_source) shr 16))^;
    s3 := (PUint32(source) + ((x + 2*inc_source) shr 16))^;
    s4 := (PUint32(source) + ((x + 3*inc_source) shr 16))^;

    {$IFDEF FPC_LITTLE_ENDIAN}
    PUint32(dest + 0)^ := ((s2 and $FF0000) shl 8) or ((s1 and $FF) shl 16) or (s1 and $FF00) or ((s1 shr 16) and $FF);
    PUint32(dest + 4)^ := ((s3 and $FF00) shl 16) or (s3 and $FF0000) or ((s2 and $FF) shl 8) or ((s2 shr 8) and $FF);
    PUint32(dest + 8)^ := ((s4 and $FF) shl 24) or ((s4 and $FF00) shl 8) or ((s4 shr 8) and $FF00) or (s3 and $FF);
    {$ELSE FPC_LITTLE_ENDIAN}
    PUint32(dest + 0)^ := ((s1 and $FF) shl 24) or ((s1 and $FF00) shl 8) or ((s1 and $FF0000) shr 8) or (s2 and $FF);
    PUint32(dest + 4)^ := ((s2 and $FF00) shl 16) or (s2 and $FF0000) or ((s3 and $FF) shl 8) or ((s3 and $FF00) shr 8);
    PUint32(dest + 8)^ := ((s3 and $FF0000) shl 8) or ((s4 and $FF) shl 16) or (s4 and $FF00) or ((s4 and $FF0000) shr 16);
    {$ENDIF FPC_LITTLE_ENDIAN}

    Inc(x, 4*inc_source);

    Inc(dest, 12);
    Dec(c);
  end;
  Inc(source, (x shr 16) * 4);
  x := x and $FFFF;

  count := count and $3;
  while count > 0 do
  begin
    (dest + 0)^ := (source + 2 + endian_fixup)^;
    (dest + 1)^ := (source + 1 + endian_fixup)^;
    (dest + 2)^ := (source + 0 + endian_fixup)^;

    Inc(x, inc_source);
    Inc(source, (x shr 16)*4);
    x := x and $FFFF;

    Inc(dest, 3);
    Dec(count);
  end;
end;

procedure ConvertP_32rgb888_16rgb565_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x, c: DWord;
  p: Uint32;
begin
  x := 0;
  { Align mod 4 }
  if (PtrUInt(dest) and 3) <> 0 then
  begin
    PUint16(dest)^ := (((PUint32(source) + (x shr 16))^ shr 8) and $f800) or
                       (((PUint32(source) + (x shr 16))^ shr 5) and $7e0) or
                       (((PUint32(source) + (x shr 16))^ shr 3) and $1f);
    Inc(x, inc_source);
    Inc(dest, 2);
    Dec(count);
  end;

  { Try to write 2 pixel blocks }
  c := count shr 1;
  while c <> 0 do
  begin
    Dec(c);
    p := ((((PUint32(source) + (x shr 16))^ shr 8) and $f800) or
          (((PUint32(source) + (x shr 16))^ shr 5) and $7e0) or
          (((PUint32(source) + (x shr 16))^ shr 3) and $1f)) shl DWORD_SMALLINT0_SHL;
    Inc(x, inc_source);

    p := p or
       (((((PUint32(source) + (x shr 16))^ shr 8) and $f800) or
         (((PUint32(source) + (x shr 16))^ shr 5) and $7e0) or
         (((PUint32(source) + (x shr 16))^ shr 3) and $1f)) shl DWORD_SMALLINT1_SHL);
    Inc(x, inc_source);

    PUint32(dest)^ := p;

    Inc(dest, 4);
  end;

  { Write trailing pixel }
  if (count and 1) <> 0 then
    PUint16(dest)^ := (((PUint32(source) + (x shr 16))^ shr 8) and $f800) or
                       (((PUint32(source) + (x shr 16))^ shr 5) and $7e0) or
                       (((PUint32(source) + (x shr 16))^ shr 3) and $1f);
end;

procedure ConvertP_32rgb888_16bgr565_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x, c: DWord;
  p: Uint32;
begin
  x := 0;
  { Align mod 4 }
  if (PtrUInt(dest) and 3) <> 0 then
  begin
    PUint16(dest)^ := (((PUint32(source) + (x shr 16))^ shr 19) and $1f) or
                       (((PUint32(source) + (x shr 16))^ shr 5) and $7e0) or
                       (((PUint32(source) + (x shr 16))^ shl 8) and $f800);
    Inc(x, inc_source);
    Inc(dest, 2);
    Dec(count);
  end;

  { Try to write 2 pixel blocks }
  c := count shr 1;
  while c <> 0 do
  begin
    Dec(c);
    p := ((((PUint32(source) + (x shr 16))^ shr 19) and $1f) or
          (((PUint32(source) + (x shr 16))^ shr 5) and $7e0) or
          (((PUint32(source) + (x shr 16))^ shl 8) and $f800)) shl DWORD_SMALLINT0_SHL;
    Inc(x, inc_source);

    p := p or
       (((((PUint32(source) + (x shr 16))^ shr 19) and $1f) or
         (((PUint32(source) + (x shr 16))^ shr 5) and $7e0) or
         (((PUint32(source) + (x shr 16))^ shl 8) and $f800)) shl DWORD_SMALLINT1_SHL);
    Inc(x, inc_source);

    PUint32(dest)^ := p;

    Inc(dest, 4);
  end;

  { Write trailing pixel }
  if (count and 1) <> 0 then
    PUint16(dest)^ := (((PUint32(source) + (x shr 16))^ shr 19) and $1f) or
                       (((PUint32(source) + (x shr 16))^ shr 5) and $7e0) or
                       (((PUint32(source) + (x shr 16))^ shl 8) and $f800);
end;

procedure ConvertP_32rgb888_16rgb555_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x, c: DWord;
  p: Uint32;
begin
  x := 0;
  { Align mod 4 }
  if (PtrUInt(dest) and 3) <> 0 then
  begin
    PUint16(dest)^ := (((PUint32(source) + (x shr 16))^ shr 9) and $7c00) or
                       (((PUint32(source) + (x shr 16))^ shr 6) and $3e0) or
                       (((PUint32(source) + (x shr 16))^ shr 3) and $1f);
    Inc(x, inc_source);
    Inc(dest, 2);
    Dec(count);
  end;

  { Try to write 2 pixel blocks }
  c := count shr 1;
  while c <> 0 do
  begin
    Dec(c);
    p := ((((PUint32(source) + (x shr 16))^ shr 9) and $7c00) or
          (((PUint32(source) + (x shr 16))^ shr 6) and $3e0) or
          (((PUint32(source) + (x shr 16))^ shr 3) and $1f)) shl DWORD_SMALLINT0_SHL;
    Inc(x, inc_source);

    p := p or
       (((((PUint32(source) + (x shr 16))^ shr 9) and $7c00) or
         (((PUint32(source) + (x shr 16))^ shr 6) and $3e0) or
         (((PUint32(source) + (x shr 16))^ shr 3) and $1f)) shl DWORD_SMALLINT1_SHL);
    Inc(x, inc_source);

    PUint32(dest)^ := p;

    Inc(dest, 4);
  end;

  { Write trailing pixel }
  if (count and 1) <> 0 then
    PUint16(dest)^ := (((PUint32(source) + (x shr 16))^ shr 9) and $7c00) or
                       (((PUint32(source) + (x shr 16))^ shr 6) and $3e0) or
                       (((PUint32(source) + (x shr 16))^ shr 3) and $1f);
end;

procedure ConvertP_32rgb888_16bgr555_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x, c: DWord;
  p: Uint32;
begin
  x := 0;
  { Align mod 4 }
  if (PtrUInt(dest) and 3) <> 0 then
  begin
    PUint16(dest)^ := (((PUint32(source) + (x shr 16))^ shr 19) and $1f) or
                       (((PUint32(source) + (x shr 16))^ shr 6) and $3e0) or
                       (((PUint32(source) + (x shr 16))^ shl 7) and $7c00);
    Inc(x, inc_source);
    Inc(dest, 2);
    Dec(count);
  end;

  { Try to write 2 pixel blocks }
  c := count shr 1;
  while c <> 0 do
  begin
    Dec(c);
    p := ((((PUint32(source) + (x shr 16))^ shr 19) and $1f) or
          (((PUint32(source) + (x shr 16))^ shr 6) and $3e0) or
          (((PUint32(source) + (x shr 16))^ shl 7) and $7c00)) shl DWORD_SMALLINT0_SHL;
    Inc(x, inc_source);

    p := p or
       (((((PUint32(source) + (x shr 16))^ shr 19) and $1f) or
         (((PUint32(source) + (x shr 16))^ shr 6) and $3e0) or
         (((PUint32(source) + (x shr 16))^ shl 7) and $7c00)) shl DWORD_SMALLINT1_SHL);
    Inc(x, inc_source);

    PUint32(dest)^ := p;

    Inc(dest, 4);
  end;

  { Write trailing pixel }
  if (count and 1) <> 0 then
    PUint16(dest)^ := (((PUint32(source) + (x shr 16))^ shr 19) and $1f) or
                       (((PUint32(source) + (x shr 16))^ shr 6) and $3e0) or
                       (((PUint32(source) + (x shr 16))^ shl 7) and $7c00);
end;

procedure ConvertP_32rgb888_8rgb332_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x, c: DWord;
  p: Uint32;
begin
  x := 0;
  { Write single pixels until the destination address is aligned mod 4 }
  while (PtrUInt(dest) and $3) <> 0 do
  begin
    dest^ := (((PUint32(source) + (x shr 16))^ shr 16) and $e0) or
             (((PUint32(source) + (x shr 16))^ shr 11) and $1c) or
             (((PUint32(source) + (x shr 16))^ shr 6) and $3);
    Inc(x, inc_source);
    Inc(dest);
    Dec(count);
    if count = 0 then
      exit;
  end;

  { Write blocks of four pixels now }
  c := count shr 2;
  while c <> 0 do
  begin
    Dec(c);
    p := ((((PUint32(source) + (x shr 16))^ shr 16) and $e0) or
          (((PUint32(source) + (x shr 16))^ shr 11) and $1c) or
          (((PUint32(source) + (x shr 16))^ shr 6) and $3)) shl DWORD_BYTE0_SHL;
    Inc(x, inc_source);

    p := p or
       (((((PUint32(source) + (x shr 16))^ shr 16) and $e0) or
         (((PUint32(source) + (x shr 16))^ shr 11) and $1c) or
         (((PUint32(source) + (x shr 16))^ shr 6) and $3)) shl DWORD_BYTE1_SHL);
    Inc(x, inc_source);

    p := p or
       (((((PUint32(source) + (x shr 16))^ shr 16) and $e0) or
         (((PUint32(source) + (x shr 16))^ shr 11) and $1c) or
         (((PUint32(source) + (x shr 16))^ shr 6) and $3)) shl DWORD_BYTE2_SHL);
    Inc(x, inc_source);

    p := p or
       (((((PUint32(source) + (x shr 16))^ shr 16) and $e0) or
         (((PUint32(source) + (x shr 16))^ shr 11) and $1c) or
         (((PUint32(source) + (x shr 16))^ shr 6) and $3)) shl DWORD_BYTE3_SHL);
    Inc(x, inc_source);

    PUint32(dest)^ := p;
    Inc(dest, 4);
  end;

  { Write up to three trailing pixels }
  c := count and $3;
  while c <> 0 do
  begin
    Dec(c);
    dest^ := (((PUint32(source) + (x shr 16))^ shr 16) and $e0) or
             (((PUint32(source) + (x shr 16))^ shr 11) and $1c) or
             (((PUint32(source) + (x shr 16))^ shr 6) and $3);
    Inc(x, inc_source);
    Inc(dest);
  end;
end;
