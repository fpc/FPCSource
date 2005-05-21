{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993-2001 by Free Pascal team

    A little example of using OS/2 API calls.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program GetCountryInfo;

uses
 OS2Def, DosCalls;

var
 Country: TCountryCode;  (* Country code info (0 = current country) *)
 CtryInfo: TCountryInfo; (* Buffer for country-specific information *)
 Size: longint;         (* Real size of returned data              *)
 W: word;

begin
 WriteLn;
 Size := 0;
 FillChar (Country, SizeOf (Country), 0);
 FillChar (CtryInfo, SizeOf (CtryInfo), 0);
 W := DosQueryCtryInfo (SizeOf (CtryInfo), Country, CtryInfo, Size);
 if (W <> NO_ERROR) then
 begin
  WriteLn ('DosQueryCtryInfo error: return code = ', W);
  Halt (1);
 end;
 WriteLn ('Code of the country is ', CtryInfo.Country,
                             ', current codepage is ', CtryInfo.CodePage, '.');
end.
