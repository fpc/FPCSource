{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005 by Florian Klaempfl and Pavel Ozerski
    member of the Free Pascal development team.

    Low level directory functions for MacOS

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
var
  spec: FSSpec;
  createdDirID: Longint;
  err: OSErr;
  res: Integer;
begin
  { TODO: convert PathArgToFSSpec (and the routines it calls) to rawbytestring }
  res:= PathArgToFSSpec(s, spec);
  if (res = 0) or (res = 2) then
    begin
      err:= FSpDirCreate(spec, smSystemScript, createdDirID);
      OSErr2InOutRes(err);
    end
  else
    InOutRes:=res;
end;

procedure do_rmdir(const s: rawbytestring);

var
  spec: FSSpec;
  err: OSErr;
  res: Integer;

begin
  res:= PathArgToFSSpec(s, spec);

  if (res = 0) then
    begin
      if IsDirectory(spec) then
        begin
          err:= FSpDelete(spec);
          OSErr2InOutRes(err);
        end
      else
        InOutRes:= 20;
    end
  else
    InOutRes:=res;
end;

procedure do_chdir(const s: rawbytestring);
var
  spec, newDirSpec: FSSpec;
  err: OSErr;
  res: Integer;
begin
  res:= PathArgToFSSpec(s, spec);
  if (res = 0) or (res = 2) then
    begin
      { The fictive file x is appended to the directory name to make
        FSMakeFSSpec return a FSSpec to a file in the directory.
        Then by clearing the name, the FSSpec then
        points to the directory. It doesn't matter whether x exists or not.}
      err:= FSMakeFSSpec (spec.vRefNum, spec.parID, ':'+spec.name+':x', newDirSpec);
      if (err = noErr) or (err = fnfErr) then
        begin
          workingDirectorySpec:= newDirSpec;
          workingDirectorySpec.name:='';
          InOutRes:= 0;
        end
      else
        begin
          {E g if the directory doesn't exist.}
          OSErr2InOutRes(err);
        end;
    end
  else
    InOutRes:=res;
end;

procedure do_getDir (DriveNr: byte; var Dir: RawByteString);

var
  pathHandleSize: Longint;

begin
  if FSpGetFullPath(workingDirectorySpec, Dir, false) <> noErr then
    Halt(3);  {exit code 3 according to MPW}

  SetCodePage(Dir,DefaultFileSystemCodePage,false);
end;



