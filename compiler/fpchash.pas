{
    Copyright (c) 2000-2002 by Free Pascal Development Team

    Routines to compute hash values

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
Unit fpchash;

{$i fpcdefs.inc}

Interface

Function UpdateCrc32(InitCrc:cardinal;const InBuf;InLen:integer):cardinal;
{ If needed trims the string to maxlen, adding at the end the CRC32 of discarded chars.
  The resulting string is guaranteed to be not longer than maxlen. }
function TrimStrCRC32(const s: ansistring; maxlen: longint): ansistring;

{ calculate string hash using FNV Hash:
  http://www.isthe.com/chongo/tech/comp/fnv/
}
function UpdateFnv64(const InitFnv: uint64; const InBuf; InLen: Integer): uint64;

type
  Base64OfUint64String = string[11];

function Base64Mangle(const x: uint64): Base64OfUint64String;

Implementation

{*****************************************************************************
                                   Crc 32
*****************************************************************************}

var
  Crc32Tbl : array[0..255] of cardinal;

procedure MakeCRC32Tbl;
var
  crc : cardinal;
  i,n : integer;
begin
  for i:=0 to 255 do
   begin
     crc:=i;
     for n:=1 to 8 do
      if (crc and 1)<>0 then
       crc:=(crc shr 1) xor cardinal($edb88320)
      else
       crc:=crc shr 1;
     Crc32Tbl[i]:=crc;
   end;
end;


Function UpdateCrc32(InitCrc:cardinal;const InBuf;InLen:Integer):cardinal;
var
  i : integer;
  p : pchar;
begin
  if Crc32Tbl[1]=0 then
   MakeCrc32Tbl;
  p:=@InBuf;
  result:=not InitCrc;
  for i:=1 to InLen do
   begin
     result:=Crc32Tbl[byte(result) xor byte(p^)] xor (result shr 8);
     inc(p);
   end;
  result:=not result;
end;


function TrimStrCRC32(const s: ansistring; maxlen: longint): ansistring;
var
  crc: DWord;
  len: longint;
begin
  len:=length(s);
  if (len<=maxlen) or (len<12) then
    result:=s
  else
   begin
     dec(maxlen,11);
     crc:=0;
     crc:=UpdateCrc32(crc,s[maxlen+1],len-maxlen);
     result:=copy(s,1,maxlen)+'$CRC'+hexstr(crc,8);
   end;
end;

{ calculate string hash using FNV Hash:
  http://www.isthe.com/chongo/tech/comp/fnv/
}
{$push} {$rangechecks off} {$overflowchecks off}
function UpdateFnv64(const InitFnv: uint64; const InBuf; InLen: Integer): uint64;
const
  M = uint64(1099511628211);
  { Compiler yells at you for overflows in constants, even with disabled range checks,
    so there are precalculated values for unrolled loop: M^2, M^3, M^4. }
  Mp2 = uint64(956575116354345);
  Mp3 = uint64(624165263380053675);
  Mp4 = uint64(11527715348014283921);
var
  pp: pByte;
begin
  result := InitFnv;
  pp := @InBuf;
  while InLen >= 4 do
   begin
     result := (result + pp[0]) * Mp4 + pp[1] * Mp3 + pp[2] * Mp2 + pp[3] * M;
     pp := pp + 4;
     InLen := InLen - 4;
   end;
  while InLen > 0 do
   begin
     result := (result + pp^) * M;
     pp := pp + 1;
     InLen := InLen - 1;
   end;
end;
{$pop}

const
  Base64Chars: array[0 .. 63] of char = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_$';

function Base64Mangle(const x: uint64): Base64OfUint64String;
var
  b64chars: pChar;
begin
  b64chars := pChar(Base64Chars);
  result[0] := #11;
  result[1] := b64chars[x and $3f];
  result[2] := b64chars[uint32(x) shr 6 and $3f];
  result[3] := b64chars[uint32(x) shr 12 and $3f];
  result[4] := b64chars[uint32(x) shr 18 and $3f];
  result[5] := b64chars[uint32(x) shr 24 and $3f];
  result[6] := b64chars[x shr 30 and $3f];
  result[7] := b64chars[x shr 36 and $3f];
  result[8] := b64chars[x shr 42 and $3f];
  result[9] := b64chars[x shr 48 and $3f];
  result[10] := b64chars[x shr 54 and $3f];
  result[11] := b64chars[x shr 60];
end;

end.
