program checkcvs;
{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    A simple filter program which displays what happened on CVS today.

    Without parameters it shows what happened today, if you specify a
    nummeric parameter smaller than 365, CheckCvs searches for entries
    n days back.
    Great to quickly check what changed after an update etc.

    Todo : add getopts and some switches to increase configurability.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Uses Dos;

type
   Array12type = ARRAY [1..12] OF longint;
const
   MonthCumm : Array12type=(0,31,59,90,120,151,181,212,243,273,304,334);

function LeapYr( Year : longint) : boolean;
begin
   LeapYr:=(Year MOD 4 = 0) AND ((Year MOD 100 <> 0) OR (Year MOD 400 = 0));
end;

function DayNr( Day,Month,Year: longint) : longint;
{Modified version. A daynr function that returns daynr since 1-1-1980.
Leapyears ok till 2100.}

VAR
   i : longint;
begin
   i := MonthCumm[Month]+Day;
   if (Month > 2) AND LeapYr( Year ) then
      INC( i );
   INC(I,(Year-1980)*365 + (Year-1976) SHR 2);
   {  - (Year -2000) DIV 100; makes it ok till 2400}
   DayNr:=i;
END ;

{TrimLeft isn't overloaded for pascal string yet.}
procedure LTrim(VAR P : String;Ch:Char);

VAR I,J : longint;

begin
 I:=Length(P);      { Keeping length in local data eases optimalisations}
 if (I>0) then
  begin
   J:=1;
   while (P[J]=Ch) AND (J<=I) DO INC(J);
   if J>1 then
    Delete(P,1,J-1);
   end;
end;

procedure CheckAfile(Name:String;Firstday:longint);
{Outputs filename and relevant CVSLOG entries for all files that have log
entries newer than FirstDay.}

VAR  F               : Text;
     Lines           : longint;
     Found           : boolean;
     S,S2,S3         : String;
     ValidLogEntry   : boolean;
     Day,Month,Year  : longint;
     PosDate         : longint;
     FirstLogEntry   : boolean;

function ReadTwo(Position:longint):longint; INLINE;

begin
 ReadTwo:=(ord(S[Position])-48)*10+(ord(S[Position+1])-48);
end;

begin
 Assign(F,Name);
 Reset(F);
 Lines:=5; Found:=FALSE;
 repeat                         {Valid files have $Id: somewhere
                                       in the first lines}
  ReadLn(F,S);
  LTrim(S,' ');
  if Copy(S,1,4)='$Id:' then
   Found:=TRUE;
  dec(Lines);
 until ((Lines=0) OR Found) OR EOF(F);
 if NOT Found then
  EXIT;
 Found:=FALSE;
 repeat                         {Valid files have $Id: somewhere
                                       in the first lines}
  ReadLn(F,S);
  LTrim(S,' ');
  if Copy(S,1,5)='$Log:' then
   Found:=TRUE;
 until (Found) OR EOF(F);
 if NOT Found then
  EXIT;
 ValidLogEntry:=FALSE;
 FirstLogEntry:=TRUE;
 repeat
  ReadLn(F,S);
  S3:=S;
  LTrim(S3,' ');
  if Copy(S3,1,8)='Revision' then
   begin
    ValidLogEntry:=FALSE;
    S2:=S;
    Delete(S3,1,9);
    S:=S3;
    Lines:=Pos(' ',S);
    if Lines<>0 then
     begin
      Delete(S,1,Lines);
      LTrim(S,' ');
      Year:=ReadTwo(1)*100+ReadTwo(3);
      Month:=ReadTwo(6);
      Day:=ReadTwo(9);
      PosDate:=DayNr(Day,Month,Year);
      if (PosDate>=FirstDay) then
       begin
        ValidLogEntry:=TRUE;
        if FirstLogEntry then
         begin
          FirstLogEntry:=FALSE;
          Writeln('File: ',Name);
         end;
        Writeln(S2);
       end;
     end;
   END
  ELSE
  if ValidLogEntry and (S[1]<>'}') then
   Writeln(S);
  until EOF(F) OR (S[1]='}');
 Close(F);
end;

VAR year, month, mday, wday: word;
    TheDay,Days            : longint;
    S                      : String;
    D                      : SearchRec;

procedure SearchExtension(Pattern:String);

begin
 FindFirst(Pattern,Anyfile-Directory,D);
 while DosError=0 DO
  begin
   CheckAFile(D.Name,TheDay);
   FindNext(D);
  end;
 FindClose(D);
end;


begin
 GetDate(year, month, mday, wday);      {GetDate}
 TheDay:=DayNr(MDay,Month,Year);        {Convert to something linear}

 if ParamCount<>0 then                  {If parameter is nummeric, subtract}
  begin
   Val(ParamStr(1),Days,Year);
   if (Year=0) AND (Days<365) then      {  n days from current date}
    dec(TheDay,Days);
  end;
 SearchExtension('*.pp');               {Scan files in simple FindFirst loop}
 SearchExtension('*.pas');
 SearchExtension('*.inc');
END.

{
  $Log$
  Revision 1.2  2000-01-14 22:06:07  marco
   * removed I hope

  Revision 1.1  2000/01/14 12:02:04  marco
   * Initial version

}
