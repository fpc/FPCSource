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

{$ifdef FPC_UNICODE_RTL}
procedure dirfn(afunc : TDirFnType;s:unicodestring);
begin
  DoDirSeparators(s);
  if not aFunc(punicodechar(s)) then
    begin
      errno:=GetLastError;
      Errno2InoutRes;
    end;
end;

function CreateDirectoryTrunc(name:pointer):longbool;stdcall;
begin
  CreateDirectoryTrunc:=CreateDirectory(name,nil);
end;

Procedure do_MkDir(const s: UnicodeString);[IOCheck];
begin
  If (length(s)=0) or (InOutRes <> 0) then
   exit;
  dirfn(TDirFnType(@CreateDirectoryTrunc),s);
end;

Procedure do_MkDir(const s: RawByteString);[IOCheck];
begin
  do_mkdir(UnicodeString(s));
end;

Procedure do_RmDir(const s: UnicodeString);[IOCheck];
begin
  if (s ='.') then
    InOutRes := 16;
  {$ifdef WINCE}
  if (s='..') then
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

Procedure do_RmDir(const s: RawByteString);[IOCheck];
begin
  do_RmDir(UnicodeString(s));
end;

Procedure do_ChDir(const s: UnicodeString);[IOCheck];
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

Procedure do_ChDir(const s: RawByteString);[IOCheck];
begin
  do_ChDir(UnicodeString(s));
end;

procedure do_GetDir (DriveNr: byte; var Dir: Unicodestring);
{$ifndef WINCE}
var
  Drive:array[0..3]of char;
  defaultdrive:boolean;
  DirBuf,SaveBuf:array[0..259] of WideChar;
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
    GetCurrentDirectory(high(SaveBuf)+1,SaveBuf); // in TChar
    if not SetCurrentDirectory(@Drive) then
     begin
      errno := word (GetLastError);
      Errno2InoutRes;
      Dir := widechar (DriveNr + 64) + ':\';
      SetCurrentDirectory(@SaveBuf);
      Exit;
     end;
   end;

  len:=GetCurrentDirectory(0,nil);
  setlength(dir,len-1); // -1 because len is #0 inclusive
  GetCurrentDirectory(len,punicodechar(dir));
  if not defaultdrive then
   SetCurrentDirectory(@SaveBuf);
  if not FileNameCasePreserving then
   dir:=upcase(dir);
    {todo: massive loss of encoding and number of chars}
{$else WINCE}
  Dir:='\';
{$endif WINCE}
end;

procedure do_GetDir (DriveNr: byte; var Dir: RawByteString);

var ldir : Unicodestring;
begin
  do_GetDir(DriveNr,ldir);
  dir:=ToSingleByteFileSystemEncodedFileName(ldir);
end;
{$else}
procedure dirfn(afunc : TDirFnType;s:pchar;len:integer);
begin
  DoDirSeparators(s);
  if not aFunc(s) then
    begin
      errno:=GetLastError;
      Errno2InoutRes;
    end;
end;

function CreateDirectoryTrunc(name:pointer):longbool;stdcall;
begin
  CreateDirectoryTrunc:=CreateDirectory(name,nil);
end;

Procedure do_MkDir(s: pchar;len:sizeuint);[IOCheck, public, alias : 'FPC_SYS_MKDIR'];
begin
  If not assigned(s) or (len=0) or (InOutRes <> 0) then
   exit;
  dirfn(TDirFnType(@CreateDirectoryTrunc),s,len);
end;

Procedure do_RmDir(s: pchar;len:sizeuint);[IOCheck, public, alias : 'FPC_SYS_RMDIR'];

begin
  if (len=1) and (s^ ='.') then
    InOutRes := 16;
  If not assigned(s) or (len=0) or (InOutRes <> 0) then
   exit;
{$ifdef WINCE}
  if (len=2) and (s[0]='.') and (s[1]='.') then
    InOutRes := 5;
{$endif WINCE}
  dirfn(TDirFnType(@RemoveDirectory),s,len);
{$ifdef WINCE}
  if (Inoutres=3) and (Pos(DirectorySeparator, s)<2) then
    Inoutres:=2;
{$endif WINCE}
end;

Procedure do_ChDir(s: pchar;len:sizeuint);[IOCheck, public, alias : 'FPC_SYS_CHDIR'];

begin
{$ifndef WINCE}
  If not assigned(s) or (len=0) or (InOutRes <> 0) then
   exit;
  dirfn(TDirFnType(@SetCurrentDirectory),s,len);
  if Inoutres=2 then
   Inoutres:=3;
{$else WINCE}
  InOutRes:=3;
{$endif WINCE}
end;

procedure GetDir (DriveNr: byte; var Dir: ShortString);
{$ifndef WINCE}
var
  Drive:array[0..3]of char;
  defaultdrive:boolean;
  DirBuf,SaveBuf:array[0..259] of Char;
{$endif WINCE}
begin
{$ifndef WINCE}
  defaultdrive:=drivenr=0;
  if not defaultdrive then
   begin
    byte(Drive[0]):=Drivenr+64;
    Drive[1]:=':';
    Drive[2]:=#0;
    Drive[3]:=#0;
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
  if not FileNameCasePreserving then
   dir:=upcase(dir);
{$else WINCE}
  Dir:='\';
{$endif WINCE}
end;
{$endif}
