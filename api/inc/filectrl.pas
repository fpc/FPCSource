{
   $Id$

   System independent low-level file interface

   Copyright (c) 1997 Balazs Scheidler (bazsi@balabit.hu)

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

 ****************************************************************************

  Todo:
   OS/2 versions of TruncateFile, FlushFile

 ****************************************************************************}
unit FileCtrl;

interface
{$I platform.inc}

uses
  ApiComm, FileSys;

{ The following platforms are supported
    OS/2 either 1.x, or 2.x
    Linux
    DOS  16 bit, DPMI, Windows 3.1
  Not supported:
    Win32 (yet)
}

const
{ standard file handles under DOS, under linux only stdin, stdout and strerr
  is defined }
  stdin         = 0;
  stdout        = 1;
  stderr        = 2;
  stdaux        = 3;
  stdprn        = 4;

  { file access constants }
  filRead       = 0;
  filWrite      = 1;
  filReadWrite  = 2;

  { seek constants }
  skBeg         = 0;
  skCur         = 1;
  skEnd         = 2;

  FilePerms: Word = $1A4;  { rw-r--r-- }

type
{$IFDEF BIT_16}
  TFileHandle = Word;
{$ELSE}
  TFileHandle = Longint;
{$ENDIF}

{ System independent calls }
{ All of these functions do what their name imply, set ErrorCode (in Common)
  to the returned error. }

{ Under linux, I'll use FilePerms as permissions, instead of expecting an
  additional parameter }

{$IFDEF PPC_Feature_Overriding}
function OpenFile(FName: PChar; Flags: Longint): TFileHandle;
function CreateFile(FName: PChar): TFileHandle;
procedure DeleteFile(FName: PChar); { should be moved to FileSys }
{$ENDIF}
function OpenFileStr(FName: PChar; Flags: Longint): TFileHandle;
function CreateFileStr(FName: PChar): TFileHandle;
procedure DeleteFileStr(FName: PChar); { should be moved to FileSys }

function OpenFile(FName: TFileName; Flags: Longint): TFileHandle;
function CreateFile(FName: TFileName): TFileHandle;
procedure DeleteFile(FName: TFileName);

procedure CloseFile(Handle: TFileHandle);
function SeekFile(Handle: TFileHandle; Pos: TFileInt; SeekType: Word): TFileInt;
function ReadFile(Handle: TFileHandle; var Buff; Count: CPUWord): CPUWord;
function WriteFile(Handle: TFileHandle; var Buff; Count: CPUWord): CPUWord;
procedure FlushFile(Handle: TFileHandle);
procedure TruncateFile(Handle: TFileHandle);
function EndOfFile(Handle: TFileHandle): Boolean;
function FilePos(Handle: TFileHandle): TFileInt;
function FileSize(Handle: TFileHandle): TFileInt;

procedure CopyFile(F1, F2: TFileHandle; Length: TFileInt);

implementation

{ Include system dependent part }
{$i filectrl.inc}

function OpenFile(FName: TFileName; Flags: Longint): TFileHandle;
begin
  FName := FName + #0;
  OpenFile := OpenFileStr(@FName[1], Flags);
end;

function CreateFile(FName: TFileName): TFileHandle;
begin
  FName := FName+#0;
  CreateFile := CreateFileStr(@FName[1]);
end;

procedure DeleteFile(FName: TFileName);
begin
  FName := FName + #0;
  DeleteFileStr(@FName[1]);
end;

{$IFDEF PPC_Feature_Overriding}
function OpenFile(FName: PChar; Flags: Longint): TFileHandle;
begin
  OpenFile := OpenFileStr(FName, Flags);
end;

function CreateFile(FName: PChar): TFileHandle;
begin
  CreateFile := CreateFileStr(FName);
end;

procedure DeleteFile(FName: PChar);
begin
  DeleteFileStr(FName);
end;
{$ENDIF}


procedure CopyFile(F1, F2: TFileHandle; Length: TFileInt);
var
  Buf: array [0..1023] of Byte;
  Len: Word;
begin
  while (ErrorCode = 0) and (Length <> 0) do begin
    if Length < 1024 then Len := Length else Len := 1024;
    Len := ReadFile(F1, Buf, Len);
    WriteFile(F2, Buf, Len);
    Dec(Length, Len);
  end;
end;

end.
{
  $Log$
  Revision 1.2  2000-02-29 11:43:16  pierre
    Common renamed APIComm to avoid problems with free vision

  Revision 1.1  2000/01/06 01:20:31  peter
    * moved out of packages/ back to topdir

  Revision 1.1  1999/12/23 19:36:47  peter
    * place unitfiles in target dirs

  Revision 1.1  1999/11/24 23:36:37  peter
    * moved to packages dir

  Revision 1.3  1999/04/13 09:29:44  daniel
  * Reverted a terrible mistake

  Revision 1.1  1998/12/04 12:48:24  peter
    * moved some dirs

  Revision 1.6  1998/10/26 11:22:49  peter
    * updates


   Date       Version  Who     Comments
   07/06/97   0.1      bazsi   Initial implementation
                               many of the platforms implemented, but not
                               tested at all
   07/07/97   0.1.1    bazsi   Some changes suggested by Marco Schmidt
                               (TFileInt)
                               Tested under Linux (FPC) and DOS (BP).
   07/12/97   0.1.2    bazsi   Converted to the new error-handling scheme,
                               began adding error codes, but this will be
                               changed (!)
   07/18/97   0.2      bazsi   Error codes moved to common
   07/18/97   0.2.1    bazsi   Corrected some syntactical errors (haven't
                               checked before uploading...)
   07/19/97   0.2.2    bazsi   Overriden versions using Pascal style strings
   07/19/97   0.3      bazsi   EndOfFile, TruncateFile added, FlushFile
                               implemented on Linux, DOS
   07/28/97   0.3.1    bazsi   Corrected some DOS 16 bit bugs (setting ErrorCode)
   08/07/97   0.3.2    bazsi   renamed to .PAS
                               PChar versions are named xxxxStr, overriden
                               versions are provided if PPC_Feature_Overriding is
                               defined (the Str versions are provided in both cases)
   08/24/97   0.3.3    bazsi   FileSys added to uses clause

   04/15/98   0.3.4    Michael Updated Linux implementation.
   05/05/98   0.3.5    mkoeppe Fixed ReadFile, WriteFile return value in Linux.

}