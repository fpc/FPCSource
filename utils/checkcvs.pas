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

TYPE
   Array12Type = ARRAY [1..12] OF LONGINT;
CONST
   MonthCumm : Array12Type=(0,31,59,90,120,151,181,212,243,273,304,334);

FUNCTION LeapYr( Year : LONGINT) : BOOLEAN;
BEGIN
   LeapYr:=(Year MOD 4 = 0) AND ((Year MOD 100 <> 0) OR (Year MOD 400 = 0));
END;

FUNCTION DayNr( Day,Month,Year: LONGINT) : LONGINT;
{Modified version. A daynr function that returns daynr since 1-1-1980.
Leapyears ok till 2100.}

VAR
   i : LONGINT;
BEGIN
   i := MonthCumm[Month]+Day;
   IF (Month > 2) AND LeapYr( Year ) THEN
      INC( i );
   INC(I,(Year-1980)*365 + (Year-1976) SHR 2);
   {  - (Year -2000) DIV 100; makes it ok till 2400}
   DayNr:=i;
END ;

{TrimLeft isn't overloaded for pascal string yet.}
PROCEDURE LTrim(VAR P : String;Ch:Char);

VAR I,J : LONGINT;

BEGIN
 I:=Length(P);      { Keeping length in local data eases optimalisations}
 IF (I>0) THEN
  BEGIN
   J:=1;
   WHILE (P[J]=Ch) AND (J<=I) DO INC(J);
   IF J>1 THEN
    Delete(P,1,J-1);
   END;
END;

PROCEDURE CheckAfile(Name:String;Firstday:LONGINT);
{Outputs filename and relevant CVSLOG entries for all files that have log
entries newer than FirstDay.}

VAR  F               : Text;
     Lines           : LONGINT;
     Found           : BOOLEAN;
     S,S2            : String;
     ValidLogEntry   : BOOLEAN;
     Day,Month,Year  : LONGINT;
     PosDate         : LONGINT;
     FirstLogEntry   : BOOLEAN;

FUNCTION ReadTwo(Position:LONGINT):LONGINT; INLINE;

BEGIN
 ReadTwo:=(ORD(S[Position])-48)*10+(ORD(S[Position+1])-48);
END;

BEGIN
 Assign(F,Name);
 Reset(F);
 Lines:=5; Found:=FALSE;
 REPEAT                         {Valid files have $Id: somewhere
                                       in the first lines}
  ReadLn(F,S);
  LTrim(S,' ');
  IF Copy(S,1,4)='$Id:' THEN
   Found:=TRUE;
  DEC(Lines);
 UNTIL ((Lines=0) OR Found) OR EOF(F);
 IF NOT Found THEN
  EXIT;
 REPEAT                         {Valid files have $Id: somewhere
                                       in the first lines}
  ReadLn(F,S);
  LTrim(S,' ');
  IF Copy(S,1,4)='$Log:' THEN
   Found:=TRUE;
 UNTIL (Found) OR EOF(F);
 IF NOT Found THEN
  EXIT;
 ValidLogEntry:=FALSE;
 FirstLogEntry:=TRUE;
 REPEAT
  ReadLn(F,S);
  IF Copy(S,3,8)='Revision' THEN
   BEGIN
    ValidLogEntry:=FALSE;
    S2:=S;
    Delete(S,1,11);
    Lines:=Pos(' ',S);
    IF Lines<>0 THEN
     BEGIN
      Delete(S,1,Lines);
      LTrim(S,' ');
      Year:=ReadTwo(1)*100+ReadTwo(3);
      Month:=ReadTwo(6);
      Day:=ReadTwo(9);
      PosDate:=DayNr(Day,Month,Year);
      IF (PosDate>=FirstDay) THEN
       BEGIN
        ValidLogEntry:=TRUE;
        IF FirstLogEntry THEN
         BEGIN
          FirstLogEntry:=FALSE;
          Writeln('File: ',Name);
         END;
        Writeln(S2);
       END;
     END;
   END
  ELSE
  IF ValidLogEntry THEN
   Writeln(S);
  UNTIL EOF(F) OR (S[1]='}');
 Close(F);
END;

VAR year, month, mday, wday: word;
    TheDay,Days            : LONGINT;
    S                      : String;
    D                      : SearchRec;

PROCEDURE SearchExtension(Pattern:String);

BEGIN
 FindFirst(Pattern,Anyfile-Directory,D);
 WHILE DosError=0 DO
  BEGIN
   CheckAFile(D.Name,TheDay);
   FindNext(D);
  END;
 FindClose(D);
END;


BEGIN
 GetDate(year, month, mday, wday);      {GetDate}
 TheDay:=DayNr(MDay,Month,Year);        {Convert to something linear}

 IF ParamCount<>0 THEN                  {If parameter is nummeric, subtract}
  BEGIN
   Val(ParamStr(1),Days,Year);
   IF (Year=0) AND (Days<365) THEN      {  n days from current date}
    Dec(TheDay,Days);
  END;
 SearchExtension('*.pp');               {Scan files in simple FindFirst loop}
 SearchExtension('*.pas');
 SearchExtension('*.inc');
END.

{
  $Log$
  Revision 1.1  2000-01-14 12:02:04  marco
   * Initial version


}
