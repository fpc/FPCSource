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

type
 TDirFnType=function(name:pointer):longbool;stdcall;

procedure dirfn(afunc : TDirFnType;const s:string);
var
  buffer : array[0..255] of char;
begin
  move(s[1],buffer,length(s));
  buffer[length(s)]:=#0;
  AllowSlash(pchar(@buffer));
  if not aFunc(@buffer) then
    begin
      errno:=GetLastError;
      Errno2InoutRes;
    end;
end;

function CreateDirectoryTrunc(name:pointer):longbool;stdcall;
begin
  CreateDirectoryTrunc:=CreateDirectory(name,nil);
end;

procedure mkdir(const s:string);[IOCHECK];
begin
  If (s='') or (InOutRes <> 0) then
   exit;
  dirfn(TDirFnType(@CreateDirectoryTrunc),s);
end;

procedure rmdir(const s:string);[IOCHECK];
begin
  if (s ='.') then
    InOutRes := 16;
  If (s='') or (InOutRes <> 0) then
   exit;
  dirfn(TDirFnType(@RemoveDirectory),s);
end;

procedure chdir(const s:string);[IOCHECK];
begin
  If (s='') or (InOutRes <> 0) then
   exit;
  dirfn(TDirFnType(@SetCurrentDirectory),s);
  if Inoutres=2 then
   Inoutres:=3;
end;

procedure GetDir (DriveNr: byte; var Dir: ShortString);
const
  Drive:array[0..3]of char=(#0,':',#0,#0);
var
  defaultdrive:boolean;
  DirBuf,SaveBuf:array[0..259] of Char;
begin
  defaultdrive:=drivenr=0;
  if not defaultdrive then
   begin
    byte(Drive[0]):=Drivenr+64;
    GetCurrentDirectory(SizeOf(SaveBuf),SaveBuf);
    if not SetCurrentDirectory(@Drive) then
     begin
      errno := word (GetLastError);
      Errno2InoutRes;
      Dir := char (DriveNr + 64) + ':\';
      SetCurrentDirectory(@SaveBuf);
      Exit;
     end;
   end;
  GetCurrentDirectory(SizeOf(DirBuf),DirBuf);
  if not defaultdrive then
   SetCurrentDirectory(@SaveBuf);
  dir:=strpas(DirBuf);
  if not FileNameCaseSensitive then
   dir:=upcase(dir);
end;

{
  $Log: sysdir.inc,v $
  Revision 1.2  2005/02/14 17:13:32  peter
    * truncate log

  Revision 1.1  2005/02/06 13:06:20  peter
    * moved file and dir functions to sysfile/sysdir
    * win32 thread in systemunit

}
