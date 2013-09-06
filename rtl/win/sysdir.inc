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

function CreateDirectoryTrunc(name:pointer):longbool;stdcall;
begin
  CreateDirectoryTrunc:=CreateDirectoryW(name,nil);
end;

procedure dirfn(afunc : TDirFnType;s:unicodestring);
begin
  DoDirSeparators(s);
  if not aFunc(punicodechar(s)) then
    begin
      errno:=GetLastError;
      Errno2InoutRes;
    end;
end;
Procedure do_MkDir(const s: UnicodeString);
begin
  dirfn(TDirFnType(@CreateDirectoryTrunc),s);
end;

Procedure do_RmDir(const s: UnicodeString);
begin
  if (s ='.') then
    begin
      InOutRes := 16;
      exit;
    end;
  {$ifdef WINCE}
  if (s='..') then
    begin
      InOutRes := 5;
      exit;
    end;
  {$endif WINCE}
  dirfn(TDirFnType(@RemoveDirectoryW),s);
{$ifdef WINCE}
  if (Inoutres=3) and (Pos(DirectorySeparator, s)<2) then
    Inoutres:=2;
{$endif WINCE}
end;

Procedure do_ChDir(const s: UnicodeString);
begin
{$ifndef WINCE}
  dirfn(TDirFnType(@SetCurrentDirectoryW),s);
  if Inoutres=2 then
   Inoutres:=3;
{$else WINCE}
  InOutRes:=3;
{$endif WINCE}
end;

procedure do_GetDir (DriveNr: byte; var Dir: Unicodestring);
{$ifndef WINCE}
var
  Drive:array[0..3]of widechar;
  defaultdrive:boolean;
  savebuf: UnicodeString;
  len : integer;
{$endif WINCE}
begin
{$ifndef WINCE}
  defaultdrive:=drivenr=0;
  if not defaultdrive then
   begin
    Drive[0]:=widechar(Drivenr+64);
    Drive[1]:=':';
    Drive[2]:=#0;
    Drive[3]:=#0;
    len:=GetCurrentDirectoryW(0,nil); // in TChar
    setlength(savebuf,len-1); // -1 because len is #0 inclusive

    GetCurrentDirectoryW(len,punicodechar(SaveBuf)); // in TChar
    if not SetCurrentDirectoryW(@Drive) then
     begin
      errno := word (GetLastError);
      Errno2InoutRes;
      Dir := widechar (DriveNr + 64) + ':\';
      SetCurrentDirectoryW(punicodechar(SaveBuf));
      Exit;
     end;
   end;

  len:=GetCurrentDirectoryW(0,nil);
  setlength(dir,len-1); // -1 because len is #0 inclusive
  GetCurrentDirectoryW(len,punicodechar(dir));
  if not defaultdrive then
    SetCurrentDirectoryW(punicodechar(SaveBuf));
  if not FileNameCasePreserving then
    dir:=upcase(dir);
{$else WINCE}
  Dir:='\';
{$endif WINCE}
end;
