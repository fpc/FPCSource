{
   $Id$
   System independent low-level file interface for Win32

   Copyright (c) 1999 by Florian Klaempfl

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.


   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

uses
  Windows;

function OpenFileStr(FName: PChar; Flags: Longint): TFileHandle;
begin
  SetLastError(0);

  OpenFileStr :=
    Windows.CreateFile(FName,
               GENERIC_READ OR GENERIC_WRITE,
               FILE_SHARE_READ OR FILE_SHARE_WRITE,
               nil,
               OPEN_EXISTING,
               FILE_ATTRIBUTE_NORMAL,
               0);

  ErrorCode := GetLastError;
end; { func. OpenFileStr }


function CreateFileStr(FName: PChar): TFileHandle;
begin
  SetLastError(0);

  CreateFileStr :=
    Windows.CreateFile(FName,
               GENERIC_READ OR GENERIC_WRITE,
               FILE_SHARE_READ OR FILE_SHARE_WRITE,
               nil,
               CREATE_ALWAYS,
               FILE_ATTRIBUTE_NORMAL,
               0);

  ErrorCode := GetLastError;
end;


procedure DeleteFileStr(FName: PChar);
begin
   ErrorCode:=0;

  if NOT Windows.DeleteFile(FName) then
     ErrorCode:=GetLastError;
end;


procedure CloseFile(Handle: TFileHandle);

begin
   ErrorCode:=0;
   if NOT CloseHandle(Handle) then
     ErrorCode:=GetLastError;
end;


function SeekFile(Handle: TFileHandle; Pos: TFileInt; SeekType: Word): TFileInt;
var tmp: longint;
begin
   ErrorCode:=0;
   Tmp := SetFilePointer(Handle, Pos, nil, SeekType);

   if tmp =$ffffffff then
     begin
       ErrorCode:=GetLastError;
       SeekFile := 0
     end
       else SeekFile := tmp;
end;


function ReadFile(Handle: TFileHandle; var Buff; Count: CPUWord): CPUWord;
var
  Result : CPUWord;
begin
  ErrorCode:=0;
  if Windows.ReadFile(Handle, Buff, Count, Result, nil) then
    ErrorCode:=GetLastError;
  ReadFile:=result;
end;


function WriteFile(Handle: TFileHandle; var Buff; Count: CPUWord): CPUWord;
var
   Written : CPUWord;
   Size: Longint;
begin
  ErrorCode:=0;
  if Windows.WriteFile(Handle, Buff, Count, Size, nil) then
    ErrorCode:=GetLastError;
  WriteFile:=Written;
end;


procedure FlushFile(Handle: TFileHandle);
begin
  ErrorCode:=0;
  if FlushFileBuffers(Handle) then
    ErrorCode:=GetLastError;
end;


procedure TruncateFile(Handle: TFileHandle);
begin
  ErrorCode:=0;
  SeekFile(Handle, 0, skEnd);
  if not(SetEndOfFile(Handle)) then
    ErrorCode:=GetLastError;
end;


function EndOfFile(Handle: TFileHandle): Boolean;
begin
  ErrorCode:=0;
  EndOfFile := FilePos(Handle) >= FileSize(Handle);
end;


function FilePos(Handle: TFileHandle): TFileInt;
var
  l : TFileInt;
begin
  ErrorCode:=0;
  l:=SetFilePointer(Handle, 0, nil, FILE_CURRENT);
  if l=-1 then
    begin
       l:=0;
       ErrorCode:=GetLastError;
    end;
  FilePos:=l;
end;


function FileSize(Handle: TFileHandle): TFileInt;
var
  aktfilepos : TFileInt;
begin
  SetLastError(0);

  AktFilePos := FilePos(Handle);
  FileSize := SeekFile(Handle, 0, skEnd);
  SeekFile(Handle, aktfilepos, skBeg);
end;

{
  $Log$
  Revision 1.1  2000-07-13 06:29:41  michael
  + Initial import

  Revision 1.2  2000/05/31 07:27:12  jonas
    * fixed formal parameters passed to var arguments problem

  Revision 1.1  2000/01/06 01:20:31  peter
    * moved out of packages/ back to topdir

  Revision 1.1  1999/11/24 23:36:38  peter
    * moved to packages dir

  Revision 1.2  1999/06/21 16:43:52  peter
    * win32 updates from Maarten Bekkers

  Revision 1.1  1999/01/08 14:37:03  florian
    + initial version, not working yet

}
