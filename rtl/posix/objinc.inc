
{
    $Id$
    Copyright (c) 2001 by the Freepascal development team

    Objects unit OS specific implementation

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}

{$i errno.inc}
{$i osposixh.inc}
{$i osposix.inc}
const
     { read/write permission for everyone }
     MODE_OPEN = S_IWUSR OR S_IRUSR OR
                 S_IWGRP OR S_IRGRP OR
                 S_IWOTH OR S_IROTH;


FUNCTION FileOpen (Var FileName: AsciiZ; Mode: Word): THandle;

Var FileMode : cint;

BEGIN
  FileMode:=0;
  if Mode=stCreate then
  Begin
     FileMode:=O_CREAT;
     FileMode:=FileMode or O_RDWR;
  end
  else
   Begin
     Case (Mode and 3) of
      0 : FileMode:=FileMode or O_RDONLY;
      1 : FileMode:=FileMode or O_WRONLY;
      2 : FileMode:=FileMode or O_RDWR;
     end;
   end;
  FileOpen:=sys_open (pchar(@FileName[0]),FileMode,MODE_OPEN);
  if (ErrNo=Sys_EROFS) and ((FileMode and O_RDWR)<>0) then
   begin
     FileMode:=FileMode and not(O_RDWR);
     FileOpen:=sys_open(pchar(@FileName[0]),Filemode,MODE_OPEN);
   end;
  If FileOpen=-1 then
    FileOpen:=0;
  DosStreamError:=Errno;
END;

FUNCTION FileRead (Handle: THandle; Var BufferArea; BufferLength: Sw_Word;
Var BytesMoved: Sw_Word): word;
var result : cint;
BEGIN
  repeat
     result:=Sys_read (Handle,pchar(@BufferArea),BufferLength);
  until errno<>Sys_EINTR;
  if result = -1 then
    BytesMoved := 0
  else
    BytesMoved := result;
  DosStreamError:=Errno;
  FileRead:=Errno;
END;

FUNCTION FileWrite (Handle:  THandle; Var BufferArea; BufferLength: Sw_Word;
Var BytesMoved: Sw_Word): Word;
var result: cint;
BEGIN
  repeat
     result:=Sys_Write (Handle,pchar(@BufferArea),BufferLength);
  until errno<>Sys_EINTR;
  if result = -1 then
    BytesMoved := 0
  else
    BytesMoved := result;
  FileWrite:=Errno;
  DosStreamError:=Errno;
END;

FUNCTION SetFilePos (Handle: THandle; Pos: LongInt; MoveType: Word;
VAR NewPos: LongInt): Word;

var
 whence : cint;
BEGIN
  whence := SEEK_SET;
  case MoveType of
  1 : whence := SEEK_CUR;
  2 : whence := SEEK_END;
  end;
  NewPos:=Sys_LSeek (Handle,Pos,whence);
  SetFilePos:=Errno;
END;

FUNCTION FileClose (Handle: THandle): Word;
BEGIN
  Sys_Close (Handle);
  DosStreamError:=Errno;
  FileClose := Errno;
END;

FUNCTION SetFileSize (Handle: THandle; FileSize: LongInt): Word;
var
 Actual : longint;
BEGIN
   SetFilePos(Handle,FileSize,0,Actual);
   If (Actual = FileSize) Then
    Begin
      if (Sys_FTruncate(Handle,Filesize)=-1) then
         SetFileSize:=103
      else
         SetFileSize:=0;
    end;
END;

{
 $Log$
 Revision 1.3  2002-09-07 16:01:26  peter
   * old logs removed and tabs fixed

 Revision 1.2  2002/08/10 13:42:36  marco
  * Fixes Posix dir copied to devel branch

}
