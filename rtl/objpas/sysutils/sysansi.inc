{
    *********************************************************************
    $Id$
    Copyright (C) 2002 by Florian Klaempfl

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
}

Function AnsiCompareFileName(const S1, S2: string): SizeInt;

begin
  If FileNameCaseSensitive then
    Result:=AnsiCompareStr(S1,S2) // Compare case sensitive
  else
    Result:=AnsiCompareText(S1,S2); // Compare case insensitive. No MBCS yet.
end;

Function SameFileName(const S1, S2: string): Boolean;

begin
  Result:=AnsiCompareFileName(S1,S2)=0;
end;

Function AnsiLowerCaseFileName(const S: string): string;

begin
  Result:=AnsiLowerCase(S); // No locale support or MBCS yet.
end;

Function AnsiUpperCaseFileName(const S: string): string;

begin
  Result:=AnsiUpperCase(S); // No locale support or MBCS yet.
end;

Function AnsiPos(const Substr, S: string): Integer;

begin
  Result:=Pos(Substr,S); // No MBCS yet.
end;

Function AnsiStrPos(Str, SubStr: PChar): PChar;

begin
  Result:=StrPos(Str,Substr);
end;

Function AnsiStrRScan(Str: PChar; Chr: Char): PChar;

begin
  Result:=StrRScan(Str,Chr);
end;

Function AnsiStrScan(Str: PChar; Chr: Char): PChar;

begin
  Result:=StrScan(Str,Chr);
end;

{
  $Log$
  Revision 1.3  2004-06-13 10:49:50  florian
    * fixed some bootstrapping problems as well as some 64 bit stuff

  Revision 1.2  2003/11/26 22:17:42  michael
  + Merged fixbranch fixes, missing in main branch

  Revision 1.1  2003/10/06 21:01:06  peter
    * moved classes unit to rtl

  Revision 1.1  2002/10/07 19:43:24  florian
    + empty prototypes for the AnsiStr* multi byte functions added
}
