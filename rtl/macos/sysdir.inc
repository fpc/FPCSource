{
    $Id: sysdir.inc,v 1.3 2005/05/14 11:52:37 olle Exp $
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

procedure mkdir(const s:string);[IOCheck];
var
  spec: FSSpec;
  createdDirID: Longint;
  err: OSErr;
  res: Integer;
begin
  If (s='') or (InOutRes <> 0) then
    exit;

  res:= PathArgToFSSpec(s, spec);
  if (res = 0) or (res = 2) then
    begin
      err:= FSpDirCreate(spec, smSystemScript, createdDirID);
      OSErr2InOutRes(err);
    end
  else
    InOutRes:=res;
end;

procedure rmdir(const s:string);[IOCheck];

var
  spec: FSSpec;
  err: OSErr;
  res: Integer;

begin
  If (s='') or (InOutRes <> 0) then
    exit;

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

procedure chdir(const s:string);[IOCheck];
var
  spec, newDirSpec: FSSpec;
  err: OSErr;
  res: Integer;
begin
  if (s='') or (InOutRes <> 0) then
    exit;

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

procedure getDir (DriveNr: byte; var Dir: ShortString);

var
  fullPath: AnsiString;
  pathHandleSize: Longint;

begin
  if FSpGetFullPath(workingDirectorySpec, fullPath, false) <> noErr then
    Halt(3);  {exit code 3 according to MPW}

  if Length(fullPath) <= 255 then {because dir is ShortString}
    InOutRes := 0
  else
    InOutRes := 1; //TODO Exchange to something better

  dir:= fullPath;
end;



{
  $Log: sysdir.inc,v $
  Revision 1.3  2005/05/14 11:52:37  olle
    * Updated header comments

  Revision 1.2  2005/02/14 17:13:30  peter
    * truncate log

  Revision 1.1  2005/02/07 21:30:12  peter
    * system unit updated

  Revision 1.1  2005/02/06 16:57:18  peter
    * threads for go32v2,os,emx,netware

  Revision 1.1  2005/02/06 13:06:20  peter
    * moved file and dir functions to sysfile/sysdir
    * win32 thread in systemunit

}
