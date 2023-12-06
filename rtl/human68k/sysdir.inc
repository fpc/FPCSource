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
var
  ps: rawbytestring;
  len: longint;
  curdrive: word;
  newdrive: word;
  dosResult: longint;
begin
  ps:=s;
  DoDirSeparators(ps);
  len:=Length(ps);

  { first, handle drive changes }
  if (len>=2) and (ps[2]=':') then
    begin
      curdrive:=h68kdos_curdrv;
      newdrive:=(ord(ps[1]) and (not 32))-ord('A');
      if (newdrive <> curdrive) then
        begin
          dosResult:=h68kdos_chgdrv(newdrive);
          if dosResult <= newdrive then
            begin
              Error2InOutRes(DOSE_ILGDRV);
              exit;
            end;
        end;

      if len=2 then
        exit;
    end;
  { do normal setpath }
  dosResult:=h68kdos_chdir(PAnsiChar(ps));
  if dosResult < 0 then
    Error2InOutRes(dosResult);
end;


procedure do_GetDir (DriveNr: byte; var Dir: RawByteString);
var
  dosResult: longint;
  pathbuf: array[0..259] of AnsiChar;
begin
  Dir := '';

  dosResult:=h68kdos_curdir(DriveNr,@pathbuf[2]);
  if dosResult < 0 then
    begin
      Error2InOutRes(dosResult);
      exit;
    end;

  if DriveNr = 0 then
    DriveNr := h68kdos_curdrv + 1;

  { return a full path, including drive }
  pathbuf[0]:=AnsiChar(ord('A') + DriveNr - 1);
  pathbuf[1]:=DriveSeparator;

  Dir:=pathbuf;
  SetCodePage(Dir,DefaultSystemCodePage,false);
end;
