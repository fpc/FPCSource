{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure BinToHex(BinValue, HexValue: PChar; BinBufSize: Integer);
Const
  HexDigits='0123456789ABCDEF';
var
  i : longint;
begin
  for i:=0 to binbufsize-1 do
    begin
    HexValue[0]:=hexdigits[1+((ord(binvalue^) shr 4))];
    HexValue[1]:=hexdigits[1+((ord(binvalue^) and 15))];
    inc(hexvalue,2);
    inc(binvalue);
    end;
end;


function HexToBin(HexValue, BinValue: PChar; BinBufSize: Integer): Integer;
// more complex, have to accept more than bintohex
// A..F    1000001
// a..f    1100001
// 0..9     110000
var i,j,h,l : integer;

begin
  i:=binbufsize;
  while (i>0) do
    begin
    if hexvalue^ IN ['A'..'F','a'..'f'] then
      h:=((ord(hexvalue^)+9) and 15)
    else if hexvalue^ IN ['0'..'9'] then
      h:=((ord(hexvalue^)) and 15)
    else
      break;
    inc(hexvalue);
    if hexvalue^ IN ['A'..'F','a'..'f'] then
      l:=(ord(hexvalue^)+9) and 15
    else if hexvalue^ IN ['0'..'9'] then
      l:=(ord(hexvalue^)) and 15
    else
      break;
    j := l + (h shl 4);
    inc(hexvalue);
    binvalue^:=chr(j);
    inc(binvalue);
    dec(i);
    end;
  result:=binbufsize-i;
end;                  

