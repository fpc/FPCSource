Program checkcvs;
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    A simple filter program which displays what happened on CVS today.

    Without parameters it shows the newest CVS log entry.
    If you specify a nummeric parameter smaller than 365,
    CheckCvs searches for ALL entries n days back.
    Great to quickly check what changed after an update etc.

    Todo : add getopts and some switches to increase configurability.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Uses Dos;

Const bufferlimit=10000;

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

Var NewestBuffer : PChar;       {Buffer containing the "newest" data}
    BufferIndex  : Longint;     {Bytes in buffer}
    NewestDate   : Longint;     {Newest date (the one in NewestBuffer)}
    CheckMode    : boolean;     {Do we search newest, or all msgs since
                                  <parameter> days ago}

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

Procedure AppendLine (S : String);

Begin
 If CheckMode Then
  Begin
   If (Length(S)<>0) AND ((Length(S)+BufferIndex+2)<BufferLimit) Then
    Begin
     Move(S[1],NewestBuffer[BufferIndex],Length(S));
     Inc(BufferIndex,Length(S));
     {$Ifndef Unix}
      NewestBuffer[BufferIndex]:=#13;
      Inc(BufferIndex);
     {$EndIf}
     NewestBuffer[BufferIndex]:=#10;
     Inc(BufferIndex);
    End;
  End
 Else
  Begin
   Writeln(S);
  End;
End;

Function ReadTwo(Position:longint): longint; INLINE;

Begin
  ReadTwo := (ord(S[Position])-48)*10+(ord(S[Position+1])-48);
End;

Begin
  Assign(F,Name);
  Reset(F);
  Lines := 5;
  Found := FALSE;
                                       in the first lines}
    ReadLn(F,S);
    LTrim(S,' ');
      Found := TRUE;
    dec(Lines);
  Until ((Lines=0) Or Found) Or EOF(F);
  If Not Found Then
   BEGIN
    Close(F);
    EXIT;
   END;
  Found := FALSE;
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
            If CheckMode Then
             Begin
              If PosDate>=NewestDate Then
               Begin
                NewestDate:=PosDate;
                BufferIndex:=0;
                ValidLogEntry := TRUE;
                AppendLine('File: '+Name);
                AppendLine(S2);
               End;
             End
            Else
            If (PosDate>=FirstDay) Then
              Begin
                ValidLogEntry := TRUE;
                If FirstLogEntry Then
                  Begin
                    FirstLogEntry := FALSE;
                    AppendLine('File: '+Name);
                  End;
                AppendLine(S2);
              End;
          End;
      End
    Else
      If ValidLogEntry And (S[1]<>'}') Then
       AppendLine(S);
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
  GetMem(NewestBuffer,bufferlimit);
  BufferIndex:=0;
  NewestDate:=0;
  GetDate(year, month, mday, wday);      {GetDate}
  TheDay := DayNr(MDay,Month,Year);        {Convert to something linear}

  If ParamCount<>0 Then                  {If parameter is nummeric, subtract}
    Begin
     CheckMode:=FALSE;
      Val(ParamStr(1),Days,Year);
      If (Year=0) And (Days<365) Then      {  n days from current date}
        dec(TheDay,Days);
    End
   Else
    CheckMode:=True;
  SearchExtension('*.pp');               {Scan files in simple FindFirst loop}
  SearchExtension('*.pas');
  SearchExtension('*.inc');
  If CheckMode AND (BufferIndex<>0) THEN
   Begin
    For Days:=0 TO BufferIndex-1 Do
     Write(NewestBuffer[Days]);
   End;
  FreeMem(NewestBuffer,bufferlimit);
End.
