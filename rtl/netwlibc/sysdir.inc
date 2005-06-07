{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl and Pavel Ozerski
    member of the Free Pascal development team.

    FPC Pascal system unit for the Win32 API.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}
procedure mkdir(const s : string);[IOCheck];
var S2 : STRING;
    Res: LONGINT;
BEGIN
  S2 := S;
  IF Length (S2) = 255 THEN DEC (BYTE(S2[0]));
  S2 := S2 + #0;
  Res := FpMkdir (@S2[1],S_IRWXU);
  if Res = 0 then
    InOutRes:=0
  else
    SetFileError (Res);
end;

procedure rmdir(const s : string);[IOCheck];
VAR S2 : STRING;
    Res: LONGINT;
BEGIN
  S2 := S;
  IF Length (S2) = 255 THEN DEC (BYTE(S2[0]));
  S2 := S2 + #0;
  Res := FpRmdir (@S2[1]);
  IF Res = 0 THEN
    InOutRes:=0
  ELSE
    SetFileError (Res);
end;

procedure chdir(const s : string);[IOCheck];
VAR S2 : STRING;
    Res: LONGINT;
begin
  S2 := S;
  IF Length (S2) = 255 THEN DEC (BYTE(S2[0]));
  S2 := S2 + #0;
  Res := FpChdir (@S2[1]);
  IF Res = 0 THEN
    InOutRes:=0
  ELSE
    SetFileError (Res);
end;

procedure getdir(drivenr : byte;var dir : shortstring);
var P : array [0..255] of CHAR;
    i : LONGINT;
begin
  P[0] := #0;
  getcwdpath(@P,nil,0);   // getcwd does not return volume, getcwdpath does
  i := libc_strlen (P);
  if i > 0 then
  begin
    Move (P, dir[1], i);
    BYTE(dir[0]) := i;
    For i := 1 to length (dir) do
      if dir[i] = '\' then dir [i] := '/';
    // fix / after volume, the compiler needs that
    // normaly root of a volumes is SERVERNAME/SYS:, change that
    // to SERVERNAME/SYS:/
    i := pos (':',dir);
    if (i > 0) then
      if i = Length (dir) then dir := dir + '/' else
      if dir [i+1] <> '/' then insert ('/',dir,i+1);
  end else
    InOutRes := 1;
end;



