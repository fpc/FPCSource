{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by Free Pascal development team

    Low level directory functions for Atari TOS

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
  dosResult:=gemdos_dcreate(pchar(ps));
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
  if s='.' then
    begin
      InOutRes:=16;
      exit;
    end;

  dosResult:=gemdos_ddelete(pchar(s));
  if dosResult < 0 then
    Error2InOutRes(dosResult);
end;


procedure do_ChDir(const s: rawbytestring);
var
  ps: rawbytestring;
begin
  ps:=s;
  DoDirSeparators(ps);

  {$WARNING Implement do_chdir}
end;


procedure do_GetDir (DriveNr: byte; var Dir: RawByteString);
begin
  Dir := '';

  {$WARNING Implement do_getdir}

  SetCodePage(Dir,DefaultSystemCodePage,false);
end;
