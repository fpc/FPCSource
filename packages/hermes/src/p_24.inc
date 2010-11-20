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
   Generic C converter (from 24 bit) for the HERMES library
   Copyright (c) 1998 Christian Nentwich (c.nentwich@cs.ucl.ac.uk)
   This source code is licensed under the GNU LGPL

   Please refer to the file COPYING.LIB contained in the distribution for
   licensing conditions
}

{ -------------------------------------------------------------------------

                             NORMAL CONVERTERS

  ------------------------------------------------------------------------- }
{ FROM 24 BIT ROUTINES }
procedure ConvertP_24rgb888_32rgb888(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_block: Uint32;
begin
  repeat
    d_block := ((source + R_24)^ shl 16) or
               ((source + G_24)^ shl 8) or
               (source + B_24)^;
    PUint32(dest)^ := d_block;
    Inc(dest, 4);
    Inc(source, 3);
    Dec(count);
  until count = 0;
end;

procedure ConvertP_24rgb888_32bgr888(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_block: Uint32;
begin
  repeat
    d_block := (source + R_24)^ or
               ((source + G_24)^ shl 8) or
               ((source + B_24)^ shl 16);
    PUint32(dest)^ := d_block;
    Inc(dest, 4);
    Inc(source, 3);
    Dec(count);
  until count = 0;
end;

procedure ConvertP_24rgb888_32rgba888(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_block: Uint32;
begin
  repeat
    d_block := ((source + R_24)^ shl 24) or
               ((source + G_24)^ shl 16) or
               ((source + B_24)^ shl 8) or $ff;
    PUint32(dest)^ := d_block;
    Inc(dest, 4);
    Inc(source, 3);
    Dec(count);
  until count = 0;
end;

procedure ConvertP_24rgb888_32bgra888(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_block: Uint32;
begin
  repeat
    d_block := ((source + R_24)^ shl 8) or
               ((source + G_24)^ shl 16) or
               ((source + B_24)^ shl 24) or $ff;
    PUint32(dest)^ := d_block;
    Inc(dest, 4);
    Inc(source, 3);
    Dec(count);
  until count = 0;
end;

procedure ConvertP_24rgb888_24bgr888(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_pix1, d_pix2, d_pix3: Uint32;
  c_pix1, c_pix2, c_pix3: PUint8;
  c: DWord;
  tmp: Uint8;
begin
  while ((PtrUInt(dest) and 3) <> 0) and (count > 0) do
  begin
    (dest + 0)^ := (source + 2)^;
    (dest + 1)^ := (source + 1)^;
    (dest + 2)^ := (source + 0)^;
    Dec(count);
    Inc(dest, 3);
    Inc(source, 3);
  end;

  c_pix1 := @d_pix1;
  c_pix2 := @d_pix2;
  c_pix3 := @d_pix3;

  c := count shr 2;
  while c > 0 do
  begin
    d_pix1 := PUint32(source)^;
    d_pix2 := PUint32(source + 4)^;
    d_pix3 := PUint32(source + 8)^;

    { Swap R and B in all three pixels }
    tmp := (c_pix1 + 0)^; (c_pix1 + 0)^ := (c_pix1 + 2)^; (c_pix1 + 2)^ := tmp;
    tmp := (c_pix1 + 3)^; (c_pix1 + 3)^ := (c_pix2 + 1)^; (c_pix2 + 1)^ := tmp;
    tmp := (c_pix2 + 2)^; (c_pix2 + 2)^ := (c_pix3 + 0)^; (c_pix3 + 0)^ := tmp;
    tmp := (c_pix3 + 1)^; (c_pix3 + 1)^ := (c_pix3 + 3)^; (c_pix3 + 3)^ := tmp;

    PUint32(dest)^ := d_pix1;
    PUint32(dest + 4)^ := d_pix2;
    PUint32(dest + 8)^ := d_pix3;
    Inc(dest, 12);
    Inc(source, 12);
    Dec(c);
  end;

  count := count and $3;
  while count > 0 do
  begin
    (dest + 0)^ := (source + 2)^;
    (dest + 1)^ := (source + 1)^;
    (dest + 2)^ := (source + 0)^;
    Dec(count);
    Inc(dest, 3);
    Inc(source, 3);
  end;
end;

procedure ConvertP_24rgb888_16rgb565(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_block: Uint32;
  i: DWord;
begin
  for i := 0 to (count shr 1) - 1 do
  begin
    d_block := ((((source + R_24)^ shl 8) and $f800) or
                (((source + G_24)^ shl 3) and $7e0) or
                (((source + B_24)^ shr 3) and $1f)) shl DWORD_SMALLINT0_SHL;

    d_block := d_block or
             (((((source + R_24 + 3)^ shl 8) and $f800) or
               (((source + G_24 + 3)^ shl 3) and $7e0) or
               (((source + B_24 + 3)^ shr 3) and $1f)) shl DWORD_SMALLINT1_SHL);

    PUint32(dest)^ := d_block;
    Inc(source, 6);
    Inc(dest, 4);
  end;

  if (count and 1) <> 0 then
  begin
    d_block := (((source + R_24)^ shl 8) and $f800) or
               (((source + G_24)^ shl 3) and $7e0) or
               (((source + B_24)^ shr 3) and $1f);

    PUint16(dest)^ := d_block;
  end;
end;

procedure ConvertP_24rgb888_16bgr565(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_block: Uint32;
  i: DWord;
begin
  for i := 0 to (count shr 1) - 1 do
  begin
    d_block := ((((source + R_24)^ shr 3) and $1f) or
                (((source + G_24)^ shl 3) and $7e0) or
                (((source + B_24)^ shl 8) and $f800)) shl DWORD_SMALLINT0_SHL;

    d_block := d_block or
             (((((source + R_24 + 3)^ shr 3) and $1f) or
               (((source + G_24 + 3)^ shl 3) and $7e0) or
               (((source + B_24 + 3)^ shl 8) and $f800)) shl DWORD_SMALLINT1_SHL);

    PUint32(dest)^ := d_block;
    Inc(source, 6);
    Inc(dest, 4);
  end;

  if (count and 1) <> 0 then
  begin
    d_block := (((source + R_24)^ shr 3) and $1f) or
               (((source + G_24)^ shl 3) and $7e0) or
               (((source + B_24)^ shl 8) and $f800);

    PUint16(dest)^ := d_block;
  end;
end;

procedure ConvertP_24rgb888_16rgb555(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_block: Uint32;
  i: DWord;
begin
  for i := 0 to (count shr 1) - 1 do
  begin
    d_block := ((((source + R_24)^ shl 7) and $7c00) or
                (((source + G_24)^ shl 2) and $3e0) or
                (((source + B_24)^ shr 3) and $1f)) shl DWORD_SMALLINT0_SHL;

    d_block := d_block or
             (((((source + R_24 + 3)^ shl 7) and $7c00) or
               (((source + G_24 + 3)^ shl 2) and $3e0) or
               (((source + B_24 + 3)^ shr 3) and $1f)) shl DWORD_SMALLINT1_SHL);

    PUint32(dest)^ := d_block;
    Inc(source, 6);
    Inc(dest, 4);
  end;

  if (count and 1) <> 0 then
  begin
    d_block := (((source + R_24)^ shl 7) and $7c00) or
               (((source + G_24)^ shl 2) and $3e0) or
               (((source + B_24)^ shr 3) and $1f);

    PUint16(dest)^ := d_block;
  end;
end;

procedure ConvertP_24rgb888_16bgr555(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_block: Uint32;
  i: DWord;
begin
  for i := 0 to (count shr 1) - 1 do
  begin
    d_block := ((((source + R_24)^ shr 3) and $1f) or
                (((source + G_24)^ shl 2) and $3e0) or
                (((source + B_24)^ shl 7) and $7c00)) shl DWORD_SMALLINT0_SHL;

    d_block := d_block or
             (((((source + R_24 + 3)^ shr 3) and $1f) or
               (((source + G_24 + 3)^ shl 2) and $3e0) or
               (((source + B_24 + 3)^ shl 7) and $7c00)) shl DWORD_SMALLINT1_SHL);

    PUint32(dest)^ := d_block;
    Inc(source, 6);
    Inc(dest, 4);
  end;

  if (count and 1) <> 0 then
  begin
    d_block := (((source + R_24)^ shr 3) and $1f) or
               (((source + G_24)^ shl 2) and $3e0) or
               (((source + B_24)^ shl 7) and $7c00);

    PUint16(dest)^ := d_block;
  end;
end;

{ optimise me !! }

procedure ConvertP_24rgb888_8rgb332(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  r, g, b: Uint32;
  i: DWord;
begin
  for i := 0 to count - 1 do
  begin
    r := (source + R_24)^ and $e0;
    g := ((source + G_24)^ shr 3) and $1c;
    b := ((source + B_24)^ shr 6) and $3;

    dest^ := r or g or b;
    Inc(source, 3);
    Inc(dest);
  end;
end;

{ -------------------------------------------------------------------------

                             STRETCH CONVERTERS

  ------------------------------------------------------------------------- }
procedure ConvertP_24rgb888_32rgb888_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x: DWord;
begin
  x := 0;
  repeat
    PUint32(dest)^ := ((source + R_24)^ shl 16) or
                     ((source + G_24)^ shl 8) or
                      (source + B_24)^;
    Inc(x, inc_source);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    x := x and $FFFF;
    Inc(dest, 4);
    Dec(count);
  until count = 0;
end;

procedure ConvertP_24rgb888_32bgr888_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x: DWord;
begin
  x := 0;
  repeat
    PUint32(dest)^ :=  (source + R_24)^ or
                     ((source + G_24)^ shl 8) or
                     ((source + B_24)^ shl 16);
    Inc(x, inc_source);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    x := x and $FFFF;
    Inc(dest, 4);
    Dec(count);
  until count = 0;
end;

procedure ConvertP_24rgb888_32rgba888_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x: DWord;
begin
  x := 0;
  repeat
    PUint32(dest)^ := ((((source + R_24)^ shl 16) or
                       ((source + G_24)^ shl 8) or
                        (source + B_24)^) shl 8) or $FF;
    Inc(x, inc_source);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    x := x and $FFFF;
    Inc(dest, 4);
    Dec(count);
  until count = 0;
end;

procedure ConvertP_24rgb888_32bgra888_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x: DWord;
begin
  x := 0;
  repeat
    PUint32(dest)^ :=  (((source + R_24)^ or
                       ((source + G_24)^ shl 8) or
                       ((source + B_24)^ shl 16)) shl 8) or $FF;
    Inc(x, inc_source);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    x := x and $FFFF;
    Inc(dest, 4);
    Dec(count);
  until count = 0;
end;

procedure ConvertP_24rgb888_24bgr888_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x: Uint32;
begin
  x := 0;
  repeat
    (dest + 0)^ := (source + 2)^;
    (dest + 1)^ := (source + 1)^;
    (dest + 2)^ := (source + 0)^;

    Inc(x, inc_source);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    x := x and $FFFF;
    Inc(dest, 3);
    Dec(count);
  until count = 0;
end;

procedure ConvertP_24rgb888_16rgb565_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_block: Uint32;
  x: DWord;
  c: DWord;
begin
  x := 0;
  if (PtrUInt(dest) and 3) <> 0 then
  begin
    d_block := (((source + R_24)^ shl 8) and $f800) or
               (((source + G_24)^ shl 3) and $7e0) or
               (((source + B_24)^ shr 3) and $1f);

    Inc(x, inc_source);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    x := x and $FFFF;

    PUint16(dest)^ := d_block;
    Inc(dest, 2);
    Dec(count);
  end;
  c := count shr 1;
  while c > 0 do
  begin
    d_block := ((((source + R_24)^ shl 8) and $f800) or
                (((source + G_24)^ shl 3) and $7e0) or
                (((source + B_24)^ shr 3) and $1f)) shl DWORD_SMALLINT0_SHL;
    Inc(x, inc_source);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    x := x and $FFFF;

    d_block := d_block or
             (((((source + R_24)^ shl 8) and $f800) or
               (((source + G_24)^ shl 3) and $7e0) or
               (((source + B_24)^ shr 3) and $1f)) shl DWORD_SMALLINT1_SHL);
    Inc(x, inc_source);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    x := x and $FFFF;

    PUint32(dest)^ := d_block;
    Inc(dest, 4);
    Dec(c);
  end;

  if (count and 1) <> 0 then
  begin
    d_block := (((source + R_24)^ shl 8) and $f800) or
               (((source + G_24)^ shl 3) and $7e0) or
               (((source + B_24)^ shr 3) and $1f);

    PUint16(dest)^ := d_block;
  end;
end;

procedure ConvertP_24rgb888_16bgr565_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_block: Uint32;
  x: DWord;
  c: DWord;
begin
  x := 0;
  if (PtrUInt(dest) and 3) <> 0 then
  begin
    d_block := (((source + R_24)^ shr 3) and $1f) or
               (((source + G_24)^ shl 3) and $7e0) or
               (((source + B_24)^ shl 8) and $f800);

    Inc(x, inc_source);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    x := x and $FFFF;

    PUint16(dest)^ := d_block;
    Inc(dest, 2);
    Dec(count);
  end;
  c := count shr 1;
  while c > 0 do
  begin
    d_block := ((((source + R_24)^ shr 3) and $1f) or
                (((source + G_24)^ shl 3) and $7e0) or
                (((source + B_24)^ shl 8) and $f800)) shl DWORD_SMALLINT0_SHL;
    Inc(x, inc_source);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    x := x and $FFFF;

    d_block := d_block or
              (((((source + R_24)^ shr 3) and $1f) or
                (((source + G_24)^ shl 3) and $7e0) or
                (((source + B_24)^ shl 8) and $f800)) shl DWORD_SMALLINT1_SHL);
    Inc(x, inc_source);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    x := x and $FFFF;

    PUint32(dest)^ := d_block;
    Inc(dest, 4);
    Dec(c);
  end;

  if (count and 1) <> 0 then
  begin
    d_block := (((source + R_24)^ shr 3) and $1f) or
               (((source + G_24)^ shl 3) and $7e0) or
               (((source + B_24)^ shl 8) and $f800);

    PUint16(dest)^ := d_block;
  end;
end;

procedure ConvertP_24rgb888_16rgb555_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_block: Uint32;
  x: DWord;
  c: DWord;
begin
  x := 0;
  if (PtrUInt(dest) and 3) <> 0 then
  begin
    d_block := (((source + R_24)^ shl 7) and $7c00) or
               (((source + G_24)^ shl 2) and $3e0) or
               (((source + B_24)^ shr 3) and $1f);

    Inc(x, inc_source);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    x := x and $FFFF;

    PUint16(dest)^ := d_block;
    Inc(dest, 2);
    Dec(count);
  end;
  c := count shr 1;
  while c > 0 do
  begin
    d_block := ((((source + R_24)^ shl 7) and $7c00) or
                (((source + G_24)^ shl 2) and $3e0) or
                (((source + B_24)^ shr 3) and $1f)) shl DWORD_SMALLINT0_SHL;
    Inc(x, inc_source);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    x := x and $FFFF;

    d_block := d_block or
               (((((source + R_24)^ shl 7) and $7c00) or
                (((source + G_24)^ shl 2) and $3e0) or
                (((source + B_24)^ shr 3) and $1f)) shl DWORD_SMALLINT1_SHL);
    Inc(x, inc_source);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    x := x and $FFFF;

    PUint32(dest)^ := d_block;
    Inc(dest, 4);
    Dec(c);
  end;

  if (count and 1) <> 0 then
  begin
    d_block := (((source + R_24)^ shl 7) and $7c00) or
               (((source + G_24)^ shl 2) and $3e0) or
               (((source + B_24)^ shr 3) and $1f);

    PUint16(dest)^ := d_block;
  end;
end;

procedure ConvertP_24rgb888_16bgr555_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  d_block: Uint32;
  x: DWord;
  c: DWord;
begin
  x := 0;
  if (PtrUInt(dest) and 3) <> 0 then
  begin
    d_block := (((source + R_24)^ shr 3) and $1f) or
               (((source + G_24)^ shl 2) and $3e0) or
               (((source + B_24)^ shl 7) and $7c00);

    Inc(x, inc_source);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    x := x and $FFFF;

    PUint16(dest)^ := d_block;
    Inc(dest, 2);
    Dec(count);
  end;
  c := count shr 1;
  while c > 0 do
  begin
    d_block := ((((source + R_24)^ shr 3) and $1f) or
                (((source + G_24)^ shl 2) and $3e0) or
                (((source + B_24)^ shl 7) and $7c00)) shl DWORD_SMALLINT0_SHL;
    Inc(x, inc_source);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    x := x and $FFFF;

    d_block := d_block or
             (((((source + R_24)^ shr 3) and $1f) or
               (((source + G_24)^ shl 2) and $3e0) or
               (((source + B_24)^ shl 7) and $7c00)) shl DWORD_SMALLINT1_SHL);
    Inc(x, inc_source);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    x := x and $FFFF;

    PUint32(dest)^ := d_block;
    Inc(dest, 4);
    Dec(c);
  end;

  if (count and 1) <> 0 then
  begin
    d_block := (((source + R_24)^ shr 3) and $1f) or
               (((source + G_24)^ shl 2) and $3e0) or
               (((source + B_24)^ shl 7) and $7c00);

    PUint16(dest)^ := d_block;
  end;
end;

procedure ConvertP_24rgb888_8rgb332_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  r, g, b: Uint32;
  i: DWord;
  x: DWord;
begin
  x := 0;
  for i := 0 to count - 1 do
  begin
    r := (source + R_24)^ and $e0;
    g := ((source + G_24)^ shr 3) and $1c;
    b := ((source + B_24)^ shr 6) and $3;

    dest^ := r or g or b;
    Inc(x, inc_source);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    Inc(source, x shr 16);
    x := x and $FFFF;
    Inc(dest);
  end;
end;
