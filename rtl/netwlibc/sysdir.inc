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
Procedure do_MkDir(const s: rawbytestring);
var Res: LONGINT;
BEGIN
  Res := FpMkdir (pchar(s),S_IRWXU);
  if Res = 0 then
    InOutRes:=0
  else
    SetFileError (Res);
end;

procedure do_RmDir(const s: rawbytestring);
var Res: longint;
begin
  Res := FpRmdir (pchar(s));
  if Res = 0 then
    InOutRes:=0
  else
    SetFileError (Res);
end;


procedure do_ChDir(const s: rawbytestring);
var Res: longint;
begin
  Res := FpChdir (s);
  if Res = 0 then
    InOutRes:=0
  else
    SetFileError (Res);
end;


procedure do_getdir(drivenr : byte;var dir : rawbytestring);
var P : array [0..255] of CHAR;
    i : LONGINT;
begin
  P[0] := #0;
  getcwdpath(@P,nil,0);   // getcwd does not return volume, getcwdpath does
  i := libc_strlen (P);
  if i > 0 then
  begin
    SetLength (dir, i);
    Move (P, dir[1], i);
    DoDirSeparators(dir);
    // fix / after volume, the compiler needs that
    // normaly root of a volumes is SERVERNAME/SYS:, change that
    // to SERVERNAME/SYS:/
    i := pos (':',dir);
    if (i > 0) then
      if i = Length (dir) then dir := dir + '/' else
      if dir [i+1] <> '/' then insert ('/',dir,i+1);
    SetCodePage (dir,DefaultFileSystemCodePage,false);
  end else
    InOutRes := 1;
end;



