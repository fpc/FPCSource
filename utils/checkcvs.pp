Program checkcvs;
{   $Id$
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

Type 
   Array12type = ARRAY [1..12] Of longint;

Const 
   MonthCumm : Array12type = (0,31,59,90,120,151,181,212,243,273,304,334);

Function LeapYr( Year : longint) : boolean;
Begin
   LeapYr := (Year Mod 4 = 0) And ((Year Mod 100 <> 0) Or (Year Mod 400 = 0));
End;

Function DayNr( Day,Month,Year: longint) : longint;
{Modified version. A daynr function that returns daynr since 1-1-1980.
Leapyears ok till 2100.}

Var 
   i : longint;
Begin
   i := MonthCumm[Month]+Day;
   If (Month > 2) And LeapYr( Year ) Then
      INC( i );
   INC(I,(Year-1980)*365 + (Year-1976) SHR 2);
   {  - (Year -2000) DIV 100; makes it ok till 2400}
   DayNr := i;
End ;

{TrimLeft isn't overloaded for pascal string yet.}

Procedure LTrim(Var P : String;Ch:Char);

Var I,J : longint;

Begin
  I := Length(P);      { Keeping length in local data eases optimalisations}
  If (I>0) Then
    Begin
      J := 1;
      while (P[J]=Ch) AND (J<=I) Do INC(J);
      If J>1 Then
        Delete(P,1,J-1);
    End;
End;

Procedure CheckAfile(Name:String;Firstday:longint);
{Outputs filename and relevant CVSLOG entries for all files that have log
entries newer than FirstDay.}

Var  F               : Text;
     Lines           : longint;
     Found           : boolean;
     S,S2,S3         : String;
     ValidLogEntry   : boolean;
     Day,Month,Year  : longint;
     PosDate         : longint;
     FirstLogEntry   : boolean;

Function ReadTwo(Position:longint): longint;
INLINE;

Begin
  ReadTwo := (ord(S[Position])-48)*10+(ord(S[Position+1])-48);
End;

Begin
  Assign(F,Name);
  Reset(F);
  Lines := 5;
  Found := FALSE;
  Repeat                         {Valid files have $Id: somewhere
                                       in the first lines}
    ReadLn(F,S);
    LTrim(S,' ');
    If Copy(S,1,4)='$Id:' Then
      Found := TRUE;
    dec(Lines);
  Until ((Lines=0) Or Found) Or EOF(F);
  If Not Found Then
    EXIT;
  Found := FALSE;
  Repeat                         {Valid files have $Id: somewhere
                                       in the first lines}
    ReadLn(F,S);
    LTrim(S,' ');
    If Copy(S,1,5)='$Log:' Then
      Found := TRUE;
  Until (Found) Or EOF(F);
  If Not Found Then
    EXIT;
  ValidLogEntry := FALSE;
  FirstLogEntry := TRUE;
  Repeat
    ReadLn(F,S);
    S3 := S;
    LTrim(S3,' ');
    If Copy(S3,1,8)='Revision' Then
      Begin
        ValidLogEntry := FALSE;
        S2 := S;
        Delete(S3,1,9);
        S := S3;
        Lines := Pos(' ',S);
        If Lines<>0 Then
          Begin
            Delete(S,1,Lines);
            LTrim(S,' ');
            Year := ReadTwo(1)*100+ReadTwo(3);
            Month := ReadTwo(6);
            Day := ReadTwo(9);
            PosDate := DayNr(Day,Month,Year);
            If (PosDate>=FirstDay) Then
              Begin
                ValidLogEntry := TRUE;
                If FirstLogEntry Then
                  Begin
                    FirstLogEntry := FALSE;
                    Writeln('File: ',Name);
                  End;
                Writeln(S2);
              End;
          End;
      End
    Else
      If ValidLogEntry And (S[1]<>'}') Then
        Writeln(S);
  Until EOF(F) Or (S[1]='}');
  Close(F);
End;

Var year, month, mday, wday: word;
    TheDay,Days            : longint;
    D                      : SearchRec;

Procedure SearchExtension(Pattern:String);

Begin
  FindFirst(Pattern,Anyfile-Directory,D);
  while DosError = 0 Do
                   Begin
                     CheckAFile(D.Name,TheDay);
                     FindNext(D);
                   End;
  FindClose(D);
End;


Begin
  GetDate(year, month, mday, wday);      {GetDate}
  TheDay := DayNr(MDay,Month,Year);        {Convert to something linear}

  If ParamCount<>0 Then                  {If parameter is nummeric, subtract}
    Begin
      Val(ParamStr(1),Days,Year);
      If (Year=0) And (Days<365) Then      {  n days from current date}
        dec(TheDay,Days);
    End;
  SearchExtension('*.pp');               {Scan files in simple FindFirst loop}
  SearchExtension('*.pas');
  SearchExtension('*.inc');
End.

{   $Log$
{   Revision 1.1  2000-01-14 22:05:47  marco
{    * Some fixes, rename .pas to .pp, ptop'ed
{
  Revision 1.1  2000/01/14 12:02:04  marco
   * Initial version

}
