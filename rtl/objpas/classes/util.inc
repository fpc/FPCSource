{
    $Id$
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
      HexValue[0]:=hexdigits[(ord(binvalue^) and 15)];
      HexValue[1]:=hexdigits[(ord(binvalue^) shr 4)];
      inc(hexvalue,2);
      inc(binvalue);		
    end;
end;


function HexToBin(HexValue, BinValue: PChar; BinBufSize: Integer): Integer;
// more complex, have to accept more than bintohex
// A..F    1000001
// a..f    1100001
// 0..9     110000
var
	i,j : integer;
begin
 i:=binbufsize;
 while (i>0) do
   begin
     if hexvalue^ IN ['A'..'F','a'..'f'] then
       j:=(ord(hexvalue^)+9) and 15
     else
       if hexvalue^ IN ['0'..'9'] then
         j:=(ord(hexvalue^)) and 15
     else
       break;
     inc(hexvalue);
     if hexvalue^ IN ['A'..'F','a'..'f'] then
       j:=j+((ord(hexvalue^)+9) and 15) shl 4
     else
       if hexvalue^ IN ['0'..'9'] then
         j:=j+((ord(hexvalue^)) and 15) shl 4
     else
       break;
     inc(hexvalue);
     binvalue^:=chr(j);
     inc(binvalue);
     dec(i);
   end;
  result:=binbufsize-i;
end;

{
  $Log$
  Revision 1.2  2005-02-03 20:17:05  florian
    + BinToHex and HexToBin from Marco added

  Revision 1.1  2003/10/06 21:01:06  peter
    * moved classes unit to rtl

  Revision 1.3  2002/09/07 15:15:26  peter
    * old logs removed and tabs fixed

}
