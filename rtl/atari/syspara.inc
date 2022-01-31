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

(* define this symbol to get ARGV argument passing that's strictly
 * compatible with the Atari standard. If it's not defined, then
 * the startup code won't validate the ARGV= variable by checking
 * the command byte for 127. Note that there are still some
 * applications (gulam is a notable example) that implement only
 * part of the standard and don't set the command byte to 127.
 *)
{$IF 0}
{$DEFINE STRICTLY_COMPATIBLE_WITH_STANDARD }
{$ENDIF}

{ Generates correct argument array on startup }
procedure GenerateArgs;
var
  ArgVLen: LongInt;
  LocalIndex: Word;
  len: Integer;

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

  function scan_argv : boolean;
  var
     hp, start : pchar;
     len: integer;
  begin
   hp:=basepage^.p_env;
   result:=false;
   if (hp=nil) then
      exit;
   LocalIndex := 0;
   while hp^<>#0 do
     begin
       if (hp[0] = 'A') and (hp[1] = 'R') and (hp[2] = 'G') and (hp[3] = 'V') and (hp[4] = '=') then
       begin
         { in any case, terminate environment here }
         hp[0] := #0;
         hp[1] := #0;
         { skip ARGV= string }
         hp := hp + 5;
         if (hp[0] = 'N') and (hp[1] = 'U') and (hp[2] = 'L') and (hp[3] = 'L') and (hp[4] = ':') then
           begin
             { TODO: handle NULL arguments }
           end;
{$ifdef STRICTLY_COMPATIBLE_WITH_STANDARD}
         if (len <> 127) then
           exit;
{$endif}
         { skip ARGV= value }
         while hp^<>#0 do
           inc(hp);
         inc(hp);
         { get arguments }
         while hp^<>#0 do
           begin
             start := hp;
             while hp^<>#0 do
               inc(hp);
             len := hp - start;
             allocarg(localindex,len);
             move(start^,argv[localindex]^,len);
             argv[localindex][len]:=#0;
             inc(localindex);
             inc(hp);
           end;
         argc:=localindex;
         result := true;
         exit;
       end;
       hp := hp + strlen(hp) + 1;
     end;
   end;

var
  Count: Word;
  Start: Word;
  Ende: Word;
  i: Integer;
  P : PChar;
begin
  P := Args;
  ArgVLen := 0;

  { check ARGV usage indicator }
  len := ord(P[0]);
  if scan_argv then
    exit;

  { Set argv[0] }
  AllocArg(0, 0);
  Argv[0][0] := #0;

  { just in case; commandline cannot be longer }
  if len > 127 then
    begin
      argc := 1;
      exit;
    end;

  { Handle the other args }
  p[len + 1] := #0;
  Count := 1;
  { first index is one }
  LocalIndex := 1;
  while (P[Count] <> #0) do
  begin
    while (P[Count] <> #0) and (p[count]<=#32) do
      Inc(count);
    if p[count] = '"' then
    begin
      Inc(Count);
      start := count;
      while (p[count]<>#0) and (p[count]<>'"') and (p[count]>=#32) do
        Inc(Count);
      ende := count;
      if (p[count] = '"') then
        Inc(Count);
    end else
    begin
      start := count;
      while (p[count]<>#0) and (p[count]>#32) do
        inc(count);
      ende := count;
    end;
    if (ende>start) then
    begin
      allocarg(localindex,ende-start);
      move(p[start],argv[localindex]^,ende-start);
      argv[localindex][ende-start]:=#0;
      inc(localindex);
    end;
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
  if (l >= 0) and (l < argc) then
    ParamStr := StrPas(argv[l]);
end;
