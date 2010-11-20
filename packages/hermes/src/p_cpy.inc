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
   C straight copy routines for the HERMES library
   Copyright (c) 1998 Christian Nentwich (c.nentwich@cs.ucl.ac.uk)
   This source code is licensed under the GNU LGPL

   Please refer to the file COPYING.LIB contained in the distribution for
   licensing conditions
}

procedure CopyP_4byte(source, dest: PUint8; count, inc_source: DWord); cdecl;
begin
  Move(source^, dest^, count shl 2);
end;

procedure CopyP_3byte(source, dest: PUint8; count, inc_source: DWord); cdecl;
begin
  Move(source^, dest^, count * 3);
end;

procedure CopyP_2byte(source, dest: PUint8; count, inc_source: DWord); cdecl;
begin
  Move(source^, dest^, count shl 1);
end;

procedure CopyP_1byte(source, dest: PUint8; count, inc_source: DWord); cdecl;
begin
  Move(source^, dest^, count);
end;

procedure CopyP_4byte_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x: DWord;
begin
  x := 0;
  repeat
    PUint32(dest)^ := (PUint32(source)+(x shr 16))^;

    Inc(x, inc_source);
    Inc(dest, 4);
    Dec(count);
  until count = 0;
end;

{ TODO: Optimise }
procedure CopyP_3byte_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x: DWord;
begin
  x := 0;
  repeat
    dest[0] := source[0];
    dest[1] := source[1];
    dest[2] := source[2];

    Inc(x, inc_source);
    Inc(source, 3*(x shr 16));
    x := x and $FFFF;
    Inc(dest, 3);
    Dec(count);
  until count = 0;
end;

procedure CopyP_2byte_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x, c: DWord;
  p: Uint32;
begin
  x := 0;
  { Alignment mod 4 }
  if (PtrUInt(dest) and 3) <> 0 then
  begin
    PUint16(dest)^ := (PUint16(source) + (x shr 16))^;
    Inc(x, inc_source);
    Inc(dest, 2);
    Dec(count);
  end;

  c := count shr 1;

  while c <> 0 do
  begin
    Dec(c);
    { TODO: make fast :) }
    p := (PUint16(source) + (x shr 16))^ shl DWORD_SMALLINT0_SHL; Inc(x, inc_source);
    p := p or ((PUint16(source) + (x shr 16))^ shl DWORD_SMALLINT1_SHL);
    Inc(x, inc_source);

    PUint32(dest)^ := p;
    Inc(dest, 4);
  end;

  if (count and 1) <> 0 then
    PUint16(dest)^ := (PUint16(source) + (x shr 16))^;
end;

procedure CopyP_1byte_S(source, dest: PUint8; count, inc_source: DWord); cdecl;
var
  x, c: DWord;
  p: Uint32;
begin
  x := 0;
  { Alignment mod 4 }
  while (PtrUInt(dest) and 3) <> 0 do
  begin
    dest^ := (source + (x shr 16))^;
    Inc(x, inc_source);
    Inc(dest); Dec(count);
    if count = 0 then
      exit;
  end;

  { Write blocks of four pixels }
  c := count shr 2;
  while c <> 0 do
  begin
    Dec(c);
    p := (source + (x shr 16))^ shl DWORD_BYTE0_SHL; Inc(x, inc_source);
    p := p or ((source + (x shr 16))^ shl DWORD_BYTE1_SHL); Inc(x, inc_source);
    p := p or ((source + (x shr 16))^ shl DWORD_BYTE2_SHL); Inc(x, inc_source);
    p := p or ((source + (x shr 16))^ shl DWORD_BYTE3_SHL); Inc(x, inc_source);

    PUint32(dest)^ := p;
    Inc(dest, 4);
  end;

  { Write up to three trailing pixels }
  c := count and $3;
  while c <> 0 do
  begin
    Dec(c);
    dest^ := (source + (x shr 16))^;
    Inc(x, inc_source);
    Inc(dest);
  end;
end;
