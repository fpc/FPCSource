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
   C surface clearing routines for the HERMES library
   Copyright (c) 1998 Christian Nentwich (c.nentwich@cs.ucl.ac.uk)
   This source code is licensed under the GNU LGPL

   Please refer to the file COPYING.LIB contained in the distribution for
   licensing conditions
}

procedure ClearP_32(iface: PHermesClearInterface); cdecl;
var
  count: DWord;
  value: Uint32;
  dest: PUint8;
begin
  value := iface^.value;
  dest := iface^.dest;
  repeat
    count := iface^.width;
    repeat
      PUint32(dest)^ := value;
      Inc(dest, 4);
      Dec(count);
    until count = 0;
    Inc(dest, iface^.add);
    Dec(iface^.height);
  until iface^.height = 0;
end;

procedure ClearP_24(iface: PHermesClearInterface); cdecl;
var
  p_value: PUint8;
  count: DWord;
  dest: PUint8;
begin
  p_value := PUint8(@iface^.value) + (R_32 - R_24);
  dest := iface^.dest;
  repeat
    count := iface^.width;
    repeat
      (dest + 0)^ := (p_value + 0)^;
      (dest + 1)^ := (p_value + 1)^;
      (dest + 2)^ := (p_value + 2)^;

      Inc(dest, 3);
      Dec(count);
    until count = 0;

    Inc(dest, iface^.add);
    Dec(iface^.height);
  until iface^.height = 0;
end;

procedure ClearP_16(iface: PHermesClearInterface); cdecl;
var
  value32: DWord;
  countshifted, count: DWord;
  dest: PUint8;
begin
  value32 := (iface^.value shl 16) or (iface^.value and $ffff);
  dest := iface^.dest;
  repeat
    count := iface^.width;

    { Align destination }
    if (PtrUInt(dest) and $3) <> 0 then
    begin
      PUint16(dest)^ := iface^.value;
      Inc(dest, 2);
      Dec(count);
    end;

    countshifted := count shr 1;

    while countshifted <> 0 do
    begin
      Dec(countshifted);
      PUint32(dest)^ := value32;
      Inc(dest, 4);
    end;

    if (count and 1) <> 0 then
    begin
      PUint16(dest)^ := iface^.value;
      Inc(dest, 2);
    end;

    Inc(dest, iface^.add);
    Dec(iface^.height);
  until iface^.height = 0;
end;

{$GOTO ON}

procedure ClearP_8(iface: PHermesClearInterface); cdecl;
label
  yloop;
var
  count, shiftcount: DWord;
  value32: Uint32;
  value: Uint8;
  dest: PUint8;
begin
  dest := iface^.dest;

  value := iface^.value and $ff;
  value32 := (value shl 24) or (value shl 16) or (value shl 8) or value;

  repeat
    count := iface^.width;

    while (PtrUInt(dest) and $3) <> 0 do    { Align to dword boundary }
    begin
      dest^ := value;
      Inc(dest);
      Dec(count);
      if count = 0 then
        goto yloop;                { GOTO's are nice ;) }
    end;

    shiftcount := count shr 2;

    while shiftcount <> 0 do
    begin
      Dec(shiftcount);
      PUint32(dest)^ := value32;
      Inc(dest, 4);
    end;

    count := count and $3;
    while count <> 0 do
    begin
      Dec(count);
      dest^ := value;
      Inc(dest);
    end;

yloop:
    Inc(dest, iface^.add);
    Dec(iface^.height);
  until iface^.height = 0;
end;
