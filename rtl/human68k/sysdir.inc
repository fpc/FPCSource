{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2020 by Free Pascal development team

    Low level directory functions for Human 68k (Sharp X68000)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{*****************************************************************************
                           Directory Handling
*****************************************************************************}
procedure do_mkdir(const s : rawbytestring);
var
  dosResult: longint;
  ps: rawbytestring;
begin
  ps:=s;
  DoDirSeparators(ps);
  dosResult:=h68kdos_mkdir(PAnsiChar(ps));
  if dosResult < 0 then
    Error2InOutRes(dosResult);
end;


procedure do_rmdir(const s : rawbytestring);
var
  dosResult: longint;
  ps: rawbytestring;
begin
  ps:=s;
  DoDirSeparators(ps);
  if ps='.' then
    begin
      InOutRes:=16;
      exit;
    end;

  dosResult:=h68kdos_rmdir(PAnsiChar(ps));
  if dosResult < 0 then
    Error2InOutRes(dosResult);
end;


procedure do_ChDir(const s: rawbytestring);
begin
end;


procedure do_GetDir (DriveNr: byte; var Dir: RawByteString);
begin
end;
