{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by Free Pascal development team

    Low level directory functions for Nintendo Wii
    Copyright (c) 2011 by Francesco Lombardi
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{*****************************************************************************
                           Directory Handling
*****************************************************************************}
procedure do_mkdir(const s: rawbytestring);
begin
  { TODO: convert callback to use rawbytestring to avoid conversion }
  if FileIODevice.DirIO.DoMkdir <> nil then
    FileIODevice.DirIO.DoMkdir(s);
end;

procedure do_rmdir(const s: rawbytestring);
begin
  { TODO: convert callback to use rawbytestring to avoid conversion }
  if FileIODevice.DirIO.DoRmdir <> nil then
    FileIODevice.DirIO.DoRmdir(s);
end;

procedure do_chdir(const s: rawbytestring);
begin
  { TODO: convert callback to use rawbytestring to avoid conversion }
  if FileIODevice.DirIO.DoChdir <> nil then
    FileIODevice.DirIO.DoChdir(pchar(s));
end;

procedure do_GetDir(DriveNr: byte; var Dir: RawByteString);
var
  TmpDir: ShortString;
begin
  { TODO: convert callback to use rawbytestring to avoid conversion }
  if FileIODevice.DirIO.DoGetdir <> nil then
    begin
      FileIODevice.DirIO.DoGetdir(DriveNr, TmpDir);
      Dir:=TmpDir;
      SetCodePage(Dir,DefaultFileSystemCodePage,false);
    end;
end;



