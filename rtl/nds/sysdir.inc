{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by Free Pascal development team

    Low level directory functions
    Nintendo DS does not have any drive, so no directory handling is needed.
    Copyright (c) 2006 by Francesco Lombardi
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{*****************************************************************************
                           Directory Handling
*****************************************************************************}
procedure mkdir(const s: string);[IOCheck];
begin
  if FileIODevice.DirIO.DoMkdir <> nil then
    FileIODevice.DirIO.DoMkdir(s);
end;

procedure rmdir(const s: string);[IOCheck];
begin
  if FileIODevice.DirIO.DoRmdir <> nil then
    FileIODevice.DirIO.DoRmdir(s);
end;

procedure chdir(const s: string);[IOCheck];
begin
  if FileIODevice.DirIO.DoChdir <> nil then
    FileIODevice.DirIO.DoChdir(s);
end;

procedure GetDir(DriveNr: byte; var Dir: ShortString);
begin
  if FileIODevice.DirIO.DoGetdir <> nil then
    FileIODevice.DirIO.DoGetdir(DriveNr, Dir);
end;



