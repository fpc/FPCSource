{
    *********************************************************************
    $Id$
    Copyright (C) 1997, 1998 Gertjan Schouten

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
    *********************************************************************

    System Utilities For Free Pascal
}

{ string manipulation functions }

function setLength(var s:string; newLength:longint):longint;
begin
if (newLength > 255) then
   newLength := 255;
s[0] := char(newLength);
setLength := ord(s[0]);
end ;

function UpperCase(s: string): string;
var l:longint;
begin
l := Length(S);
while l <> 0 do begin
   if (s[l] in ['a'..'z']) then s[l] := char(byte(s[l]) - 32);
   dec(l);
   end;
uppercase := s;
end;

function LowerCase(s: string): string;
var l:longint;
begin
l := Length(S);
while l <> 0 do begin
   if (s[l] in ['A'..'Z']) then s[l] := char(byte(s[l]) + 32);
   dec(l);
   end;
LowerCase := s;
end;

{!$I ANSI.PPI}

function AnsiUpperCase(s: string):string;
begin
end ;

function AnsiLowerCase(s: string):string;
begin
end ;

function left(s: string;i:Longint): string;
begin
left := copy(s,1,i);
end ;

function right(s: string;i:Longint): string;
begin
right := copy(s,1 + length(s)-i,i);
end ;

function trim(s: string):string;
var i,l:longint;
begin
l := length(s);
while (s[l] = ' ') and (l > 0) do dec(l);
setLength(s, l);
i := 1;
while (s[i] = ' ') and (i <= l) do inc(i);
trim := copy(s, i, l);
end ;

function trimleft(s:string):string;
var i,l:longint;
begin
l := length(s);
i := 1;
while (s[i] = ' ') and (i <= l) do inc(i);
trimleft := copy(s, i, l);
end ;

function trimright(s:string):string;
var l:longint;
begin
l := length(s);
while (s[l] = ' ') and (l > 0) do dec(l);
setLength(s, l);
trimright := s;
end ;

{ Conversion stuff }

function  IntToStr(l:longint):string;
var result:string;
begin
system.str(l,result);
inttostr := result;
end ;

function  StrToInt(s:string):longint;
var result:longint;c:word;
begin
val(s, result, c);
strtoint := result;
end ;

function  IntToHex(Value: longint; Digits: longint): string;
var result:string;i:longint;
begin
result := '        ';
setLength(result, digits);
for i := 0 to digits - 1 do begin
   result[digits - i] := HexDigits[value and 15];
   value := value shr 4;
   end ;
inttohex := result;
end ;


{
  $Log$
  Revision 1.1  1998-04-10 15:17:46  michael
  + Initial implementation; Donated by Gertjan Schouten
    His file was split into several files, to keep it a little bit structured.

}