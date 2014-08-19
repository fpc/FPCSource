{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Marcus Sackrow

    utility.library functions for AROS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure Amiga2Date(date_amiga: longword; cd: PClockData);
type
  TAmiga2Date = procedure(date_amiga: longword; cd: PClockData; Base: Pointer); cdecl;
var
  Call: TAmiga2Date;
begin
  Call := TAmiga2Date(GetLibAdress(AOS_UtilityBase, 20));
  Call(date_amiga, cd, AOS_UtilityBase);
end;

function Date2Amiga(date: PClockData): longword;
type
  TDate2Amiga = function(date: PClockData; Base: Pointer): longword; cdecl;
var
  Call: TDate2Amiga;
begin
  Call := TDate2Amiga(GetLibAdress(AOS_UtilityBase, 21));
  Date2Amiga := Call(date, AOS_UtilityBase);
end;


