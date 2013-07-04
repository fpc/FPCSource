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
procedure mkdir(s: pchar; len: sizeuint);[IOCheck, public, alias : 'FPC_SYS_MKDIR'];
begin
  if not assigned(s) or (len=0) or (InOutRes<>0) then exit;
  if FileIODevice.DirIO.DoMkdir <> nil then
    FileIODevice.DirIO.DoMkdir(strpas(s));
end;

procedure rmdir(s: pchar; len: sizeuint);[IOCheck, public, alias : 'FPC_SYS_RMDIR'];
begin
  if not assigned(s) or (len=0) then exit;
  if FileIODevice.DirIO.DoRmdir <> nil then
    FileIODevice.DirIO.DoRmdir(strpas(s));
end;

procedure chdir(s: pchar; len: sizeuint);[IOCheck, public, alias : 'FPC_SYS_CHDIR'];
begin
  if not assigned(s) or (len=0) then exit;
  if FileIODevice.DirIO.DoChdir <> nil then
    FileIODevice.DirIO.DoChdir(strpas(s));
end;

procedure GetDir(DriveNr: byte; var Dir: RawByteString);
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



