{
    Copyright (c) 2000-2002 by Free Pascal Development Team

    Routines to compute CRC values

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
Unit fpccrc;

{$i fpcdefs.inc}

Interface

Function UpdateCrc32(InitCrc:cardinal;const InBuf;InLen:integer):cardinal;
{ If needed trims the string to maxlen, adding at the end the CRC32 of discarded chars.
  The resulting string is guaranteed to be not longer than maxlen. }
function TrimStrCRC32(const s: ansistring; maxlen: longint): ansistring;

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

end.
