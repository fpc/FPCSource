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
  if ps='.' then
    begin
      InOutRes:=16;
      exit;
    end;

  dosResult:=gemdos_ddelete(pchar(ps));
  if dosResult < 0 then
    Error2InOutRes(dosResult);
end;


procedure do_ChDir(const s: rawbytestring);
var
  ps: rawbytestring;
  len: longint;
  drives: dword;
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
      curdrive:=gemdos_dgetdrv;
      newdrive:=(ord(ps[1]) and (not 32))-ord('A');
      if (newdrive <> curdrive) then
        begin
          { verify if the drive we have to set actually exist.
            not doing so may corrupt TOS internal structures, 
            according to docs. (KB) }
          drives:=gemdos_dsetdrv(curdrive);
          if (drives and (1 shl newdrive)) = 0 then
            begin
              InOutRes:=15;
              exit;
            end;
          gemdos_dsetdrv(newdrive);
        end;
      if len=2 then
        exit;
    end;
  { do normal setpath }
  dosResult:=gemdos_dsetpath(pchar(ps));
  if dosResult < 0 then
    Error2InOutRes(dosResult);
end;


procedure do_GetDir (DriveNr: byte; var Dir: RawByteString);
var
  dosResult: longint;
  pathbuf: array[0..259] of char;
begin
  Dir := '';

  dosResult:=gemdos_dgetpath(@pathbuf[2],DriveNr);
  if dosResult < 0 then
    begin
      Error2InOutRes(dosResult);
      exit;
    end;

  if DriveNr = 0 then
    DriveNr := gemdos_dgetdrv + 1;

  { return a full path, including drive }
  pathbuf[0]:=char(ord('A') + DriveNr - 1);
  pathbuf[1]:=DriveSeparator;

  Dir:=pathbuf;
  SetCodePage(Dir,DefaultSystemCodePage,false);
end;
