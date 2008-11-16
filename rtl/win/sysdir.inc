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
 TDirFnType=function(name:PRtlChar):longbool;stdcall;

procedure dirfn(afunc : TDirFnType;const s:RtlString);
var
  buffer : RtlString;
begin
  buffer:=s;
  DoDirSeparators(PRtlChar(buffer));
  if not aFunc(PRtlChar(buffer)) then
    begin
      errno:=GetLastError;
      Errno2InoutRes;
    end;
end;

function CreateDirectoryTrunc(name:PRtlChar):longbool;stdcall;
begin
  CreateDirectoryTrunc:=CreateDirectory(name,nil);
end;

procedure mkdir(const s:RtlString);[IOCHECK];
begin
  If (s='') or (InOutRes <> 0) then
   exit;
  dirfn(TDirFnType(@CreateDirectoryTrunc),s);
end;

procedure rmdir(const s:RtlString);[IOCHECK];
begin
  if (s ='.') then
    InOutRes := 16;
{$ifdef WINCE}
  if (s ='..') then
    InOutRes := 5;
{$endif WINCE}
  If (s='') or (InOutRes <> 0) then
   exit;
  dirfn(TDirFnType(@RemoveDirectory),s);
{$ifdef WINCE}
  if (Inoutres=3) and (Pos(DirectorySeparator, s)<2) then
    Inoutres:=2;
{$endif WINCE}
end;

procedure chdir(const s:RtlString);[IOCHECK];
begin
{$ifndef WINCE}
  If (s='') or (InOutRes <> 0) then
   exit;
  dirfn(TDirFnType(@SetCurrentDirectory),s);
  if Inoutres=2 then
   Inoutres:=3;
{$else WINCE}
  InOutRes:=3;
{$endif WINCE}
end;

procedure GetDir (DriveNr: byte; var Dir: RtlString);
{$ifndef WINCE}
const
  Drive:array[0..3]of RtlChar=(#0,':',#0,#0);
var
  defaultdrive:boolean;
  DirBuf,SaveBuf:array[0..259] of RtlChar;
{$endif WINCE}
begin
{$ifndef WINCE}
  defaultdrive:=drivenr=0;
  if not defaultdrive then
   begin
    Drive[0]:=RtlChar(Drivenr+64);
    GetCurrentDirectory(High(SaveBuf)+1,SaveBuf);
    if not SetCurrentDirectory(@Drive) then
     begin
      errno := word (GetLastError);
      Errno2InoutRes;
      Dir := RtlChar (DriveNr + 64) + ':\';
      SetCurrentDirectory(@SaveBuf);
      Exit;
     end;
   end;
  GetCurrentDirectory(High(SaveBuf)+1,DirBuf);
  if not defaultdrive then
   SetCurrentDirectory(@SaveBuf);
  dir:=DirBuf;
  if not FileNameCaseSensitive then
   dir:=upcase(dir);
{$else WINCE}
  Dir:='\';
{$endif WINCE}
end;
