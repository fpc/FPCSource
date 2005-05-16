{
    $Id: util.inc,v 1.4 2005/04/14 17:43:53 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Function IntToStr (I : Longint) : String;

begin
  Str(I,Result);
end;

function IsValidIdent(const Ident: string): Boolean;

begin
  Result:=True;
end;


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

{
  $Log: util.inc,v $
  Revision 1.4  2005/04/14 17:43:53  michael
  + Fix for BintoHex and hextobin by Uberto Barbini

  Revision 1.3  2005/02/14 17:13:31  peter
    * truncate log

  Revision 1.2  2005/02/03 20:17:05  florian
    + BinToHex and HexToBin from Marco added

}
