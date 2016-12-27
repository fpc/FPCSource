{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by Marcus Sackrow and Karoly Balogh
    members of the Free Pascal development team.

    Command line parameter handling for Atari

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{ Generates correct argument array on startup }
procedure GenerateArgs;
var
  ArgVLen: LongInt;

  procedure AllocArg(Idx, Len: LongInt);
  var
    i, OldArgVLen : LongInt;
  begin
    if Idx >= ArgVLen then
    begin
      OldArgVLen := ArgVLen;
      ArgVLen := (Idx + 8) and (not 7);
      SysReAllocMem(Argv, Argvlen * SizeOf(Pointer));
      for i := OldArgVLen to ArgVLen - 1 do
        ArgV[i]:=nil;
    end;
    ArgV[Idx] := SysAllocMem(Succ(Len));
  end;

var
  Count: Word;
  Start: Word;
  Ende: Word;
  LocalIndex: Word;
  i: Integer;
  P : PChar;
  Temp : AnsiString;
  InQuotes: boolean;
begin
  P := Args;
  ArgVLen := 0;

  { Set argv[0] }
  Temp := ParamStr(0);
  AllocArg(0, Length(Temp));
  Move(Temp[1], Argv[0]^, Length(Temp));
  Argv[0][Length(Temp)] := #0;

  InQuotes := False;
  { Handle the other args }
  Count := 0;
  { first index is one }
  LocalIndex := 1;
  while (P[Count] <> #0) do
  begin
    while (p[count]=' ') or (p[count]=#9) or (p[count]=LineEnding) do
      Inc(count);
    if p[count] = '"' then
    begin
      inQuotes := True;
      Inc(Count);
    end;
    start := count;
    if inQuotes then
    begin
      while (p[count]<>#0) and (p[count]<>'"') and (p[count]<>LineEnding) do
      begin
        Inc(Count)
      end;
    end else
    begin
      while (p[count]<>#0) and (p[count]<>' ') and (p[count]<>#9) and (p[count]<>LineEnding) do
        inc(count);
    end;
    ende := count;
    if not inQuotes then
    begin
      while (p[start]=' ') and (Start < Ende) do
        Inc(Start)
    end;
    if (ende-start>0) then
    begin
      allocarg(localindex,ende-start);
      move(p[start],argv[localindex]^,ende-start);
      argv[localindex][ende-start]:=#0;
      if inQuotes and (argv[localindex][(ende-start) - 1] = '"') then
        argv[localindex][(ende-start)-1] := #0;
      inc(localindex);
    end;
    if inQuotes and (p[count] = '"') then
      Inc(Count);
    inQuotes := False;
  end;
  argc:=localindex;
end;


{*****************************************************************************
                             ParamStr
*****************************************************************************}

{ number of args }
function ParamCount: LongInt;
begin
  ParamCount := argc - 1;
end;

{ argument number l }
function ParamStr(l: LongInt): string;
var
  s1: string;
begin
  ParamStr := '';
  if (l > 0) and (l + 1 <= argc) then
    ParamStr := StrPas(argv[l]);
end;
