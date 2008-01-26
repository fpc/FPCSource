{
    Copyright (c) 1999-2000 by Michael Van Canneyt

    Unit to read a disc TOC and get discid for a cddb query.


    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit discid;

{$mode objfpc}

interface

uses cdrom,sysutils;

Function CDDBDiscID(Const CDTOC : Array of TTocEntry; Count : Integer) : integer ;
Function GetCDDBQueryString(Const Tracks : Array of TTocEntry; Count : Integer) : String;

Implementation

Function cddb_sum(N : Integer) : Cardinal;

begin
  Result:=0;
  while (n > 0) do
    begin
    Inc(result,(n mod 10));
    n:=n div 10;
    end;
end;

Function cddbdiscid(Const cdtoc : Array of TTocEntry; Count : Integer) : integer ;

Var
 i,t,n :  cardinal;

begin
  t:=0;
  n:=0;
  i:= 0;
  For I:=0 to count-1 do
    n := n + cddb_sum((cdtoc[i].min * 60) + cdtoc[i].sec);
  t:=((cdtoc[Count].min * 60) + cdtoc[Count].sec) -
     ((cdtoc[0].min * 60) + cdtoc[0].sec);
  Result:=(((n mod $ff) shl 24) or (t shl 8 or count));
end;

Function GetCDDBQueryString(Const Tracks : Array of TTocEntry; Count : Integer) : String;


Var
  I,TheDiscID : Integer;

begin
  TheDiscID:=cddbdiscid(Tracks,Count);
  Result:=Lowercase(HexStr(TheDiscID,8))+' '+intToStr(Count);
  for I:=0 to Count-1 do
     Result:=Result+' '+IntToStr(tracks[i].frame);
  Result:=Result+' '+IntToStr(tracks[Count].frame div 75);
end;

end.
