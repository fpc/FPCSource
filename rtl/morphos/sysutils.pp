{
    $Id$

    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 by Karoly Balogh

    Sysutils unit for MorphOS

    Based on Amiga version by Carl Eric Codere, and other
    parts of the RTL

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit sysutils;

interface

{$MODE objfpc}
{ force ansistrings }
{$H+}

{ Include platform independent interface part }
{$i sysutilh.inc}

{ Platform dependent calls }

Procedure AddDisk(const path:string);


implementation

uses dos,sysconst;

{ Include platform independent implementation part }
{$i sysutils.inc}



{****************************************************************************
                              File Functions
****************************************************************************}
{$I-}{ Required for correct usage of these routines }



(* non portable routines *)
Function FileOpen (Const FileName : string; Mode : Integer) : Longint;
Begin
end;

Function FileGetDate (Handle : Longint) : Longint;
begin
end;


Function FileSetDate (Handle,Age : Longint) : Longint;
begin
  // Impossible under unix from FileHandle !!
  FileSetDate:=-1;
end;



Function FileCreate (Const FileName : String) : Longint;
begin
end;

function FileCreate (const FileName: string; Mode: integer): longint;
begin
end;

Function FileRead (Handle : Longint; Var Buffer; Count : longint) : Longint;
begin
end;


Function FileWrite (Handle : Longint; const Buffer; Count : Longint) : Longint;
begin
end;


Function FileSeek (Handle,FOffset,Origin : Longint) : Longint;
begin
end;

function FileSeek (Handle: longint; FOffset, Origin: Int64): Int64;
begin
end;

Procedure FileClose (Handle : Longint);
begin
end;


Function FileTruncate (Handle,Size: Longint) : boolean;
begin
end;
(* end of non portable routines *)

Function FileAge (Const FileName : String): Longint;

var F: file;
    Time: longint;
begin
   Assign(F,FileName);
   dos.GetFTime(F,Time);
   { Warning this is not compatible with standard routines
     since Double are not supported on m68k by default!
   }  
   FileAge:=Time;
end;


Function FileExists (Const FileName : String) : Boolean;
Var
 F: File;
 OldMode : Byte;
Begin
  OldMode := FileMode;
  FileMode := fmOpenRead;
  Assign(F,FileName);
  Reset(F,1);
  FileMode := OldMode;
  If IOResult <> 0 then
    FileExists := FALSE
  else
    Begin
      FileExists := TRUE;
      Close(F);
    end;
end;    

type 
  PDOSSearchRec = ^SearchRec;

Function FindFirst (Const Path : String; Attr : Longint; Var Rslt : TSearchRec) : Longint;
Const
  faSpecial = faHidden or faSysFile or faVolumeID or faDirectory;
var
  p : pDOSSearchRec;
  dosattr: word;
  DT: Datetime;
begin
 dosattr:=0;
 if Attr and faHidden <> 0 then
   dosattr := dosattr or Hidden;
 if Attr and faSysFile <> 0 then
   dosattr := dosattr or SysFile;
 if Attr and favolumeID <> 0 then
   dosattr := dosattr or VolumeID;
 if Attr and faDirectory <> 0 then
   dosattr := dosattr or Directory;
 New(p);
 Rslt.FindHandle :=  THandle(p);
 dos.FindFirst(path,dosattr,p^);
 if DosError <> 0 then
    begin
      FindFirst := -1;
    end
 else
   begin
     Rslt.Name := p^.Name;
     { Not compatible with other platforms! }
     Rslt.Time:=p^.Time;
     Rslt.Attr := p^.Attr;
     Rslt.ExcludeAttr := not p^.Attr;
     Rslt.Size := p^.Size;
     FindFirst := 0;
   end;   
end;


Function FindNext (Var Rslt : TSearchRec) : Longint;
var
 p : pDOSSearchRec;
 DT: Datetime;
begin
  p:= PDOsSearchRec(Rslt.FindHandle);
  if not assigned(p) then
     begin
       FindNext := -1;
       exit;
     end;
  Dos.FindNext(p^);
 if DosError <> 0 then
    begin
      FindNext := -1;
    end
 else
   begin
     Rslt.Name := p^.Name;
     UnpackTime(p^.Time, DT);
     { Warning: Not compatible with other platforms }
     Rslt.time := p^.Time;
     Rslt.Attr := p^.Attr;
     Rslt.ExcludeAttr := not p^.Attr;
     Rslt.Size := p^.Size;
     FindNext := 0;
   end;     
end;

Procedure FindClose (Var F : TSearchrec);
Var
  p : PDOSSearchRec;

begin
  p:=PDOSSearchRec(f.FindHandle); 
  if not assigned(p) then
       exit;
  Dos.FindClose(p^);
  if assigned(p) then
     Dispose(p);
  f.FindHandle := THandle(nil);
end;

Function FileGetAttr (Const FileName : String) : Longint;
var
 F: file;
 attr: word;
begin
 Assign(F,FileName);
 dos.GetFAttr(F,attr);
 if DosError <> 0 then
    FileGetAttr := -1
 else
    FileGetAttr := Attr; 
end;


Function FileSetAttr (Const Filename : String; Attr: longint) : Longint;
var
 F: file;
begin
 Assign(F, FileName);
 Dos.SetFAttr(F, Attr and $ffff);
 FileSetAttr := DosError;
end;


Function DeleteFile (Const FileName : String) : Boolean;
var
 F: File;
begin
 Assign(F,FileName);
 Erase(F);
 DeleteFile := (IOResult = 0);
end;

Function RenameFile (Const OldName, NewName : String) : Boolean;
var
 F: File;
begin
 Assign(F,OldName);
 Rename(F,NewName);
 RenameFile := (IOResult = 0);
end;




{****************************************************************************
                              Disk Functions
****************************************************************************}

{
  The Diskfree and Disksize functions need a file on the specified drive, since this
  is required for the statfs system call.
  These filenames are set in drivestr[0..26], and have been preset to :
   0 - '.'      (default drive - hence current dir is ok.)
   1 - '/fd0/.'  (floppy drive 1 - should be adapted to local system )
   2 - '/fd1/.'  (floppy drive 2 - should be adapted to local system )
   3 - '/'       (C: equivalent of dos is the root partition)
   4..26          (can be set by you're own applications)
  ! Use AddDisk() to Add new drives !
  They both return -1 when a failure occurs.
}
Const
  FixDriveStr : array[0..3] of pchar=(
    '.',
    '/fd0/.',
    '/fd1/.',
    '/.'
    );
var
  Drives   : byte;
  DriveStr : array[4..26] of pchar;

Procedure AddDisk(const path:string);
begin
  if not (DriveStr[Drives]=nil) then
   FreeMem(DriveStr[Drives],StrLen(DriveStr[Drives])+1);
  GetMem(DriveStr[Drives],length(Path)+1);
  StrPCopy(DriveStr[Drives],path);
  inc(Drives);
  if Drives>26 then
   Drives:=4;
end;



Function DiskFree(Drive: Byte): int64;
Begin
  DiskFree := dos.diskFree(Drive);
End;



Function DiskSize(Drive: Byte): int64;
Begin
  DiskSize := dos.DiskSize(Drive);
End;




Function GetCurrentDir : String;
begin
  GetDir (0,Result);
end;


Function SetCurrentDir (Const NewDir : String) : Boolean;
begin
   ChDir(NewDir);
  result := (IOResult = 0);
end;


Function CreateDir (Const NewDir : String) : Boolean;
begin
   MkDir(NewDir);
  result := (IOResult = 0);
end;


Function RemoveDir (Const Dir : String) : Boolean;
begin
   RmDir(Dir);
  result := (IOResult = 0);
end;


Function DirectoryExists(const Directory: string): Boolean;
var
 s: string;
begin
  { Get old directory }
  s:=GetCurrentDir;
  ChDir(Directory);
  DirectoryExists := (IOResult = 0);
  ChDir(s);
end;


{****************************************************************************
                              Misc Functions
****************************************************************************}

procedure Beep;
begin
end;


{****************************************************************************
                              Locale Functions
****************************************************************************}

Procedure GetLocalTime(var SystemTime: TSystemTime);
var
 dayOfWeek: word;
begin
  dos.GetTime(SystemTime.Hour, SystemTime.Minute, SystemTime.Second,SystemTime.Millisecond);
  dos.GetDate(SystemTime.Year, SystemTime.Month, SystemTime.Day, DayOfWeek);
end ;


Procedure InitAnsi;
Var
  i : longint;
begin
  {  Fill table entries 0 to 127  }
  for i := 0 to 96 do
    UpperCaseTable[i] := chr(i);
  for i := 97 to 122 do
    UpperCaseTable[i] := chr(i - 32);
  for i := 123 to 191 do
    UpperCaseTable[i] := chr(i);
  Move (CPISO88591UCT,UpperCaseTable[192],SizeOf(CPISO88591UCT));

  for i := 0 to 64 do
    LowerCaseTable[i] := chr(i);
  for i := 65 to 90 do
    LowerCaseTable[i] := chr(i + 32);
  for i := 91 to 191 do
    LowerCaseTable[i] := chr(i);
  Move (CPISO88591LCT,UpperCaseTable[192],SizeOf(CPISO88591UCT));
end;


Procedure InitInternational;
begin
  InitAnsi;
end;

function SysErrorMessage(ErrorCode: Integer): String;

begin
{  Result:=StrError(ErrorCode);}
end;

{****************************************************************************
                              OS utility functions
****************************************************************************}

Function GetEnvironmentVariable(Const EnvVar : String) : String;

begin
  Result:=Dos.Getenv(shortstring(EnvVar));
end;

function ExecuteProcess (const Path: AnsiString; const ComLine: AnsiString):
                                                                       integer;
var
  CommandLine: AnsiString;
  E: EOSError;

begin
  Dos.Exec (Path, ComLine);
  if DosError <> 0 then begin

    if ComLine = '' then
      CommandLine := Path
    else
      CommandLine := Path + ' ' + ComLine;

    E := EOSError.CreateFmt (SExecuteProcessFailed, [CommandLine, DosError]);
    E.ErrorCode := DosError;
    raise E;
  end;
end;

function ExecuteProcess (const Path: AnsiString;
                                  const ComLine: array of AnsiString): integer;
var 
  CommandLine: AnsiString;
  I: integer;

begin
  Commandline := '';
  for I := 0 to High (ComLine) do
   if Pos (' ', ComLine [I]) <> 0 then
    CommandLine := CommandLine + ' ' + '"' + ComLine [I] + '"'
   else
    CommandLine := CommandLine + ' ' + Comline [I];
  ExecuteProcess := ExecuteProcess (Path, CommandLine);
end;


{****************************************************************************
                              Initialization code
****************************************************************************}

Initialization
  InitExceptions;
  InitInternational;    { Initialize internationalization settings }
Finalization
  DoneExceptions;
end.
{
    $Log$
    Revision 1.1  2004-06-06 00:58:02  karoly
      * initial revision

}
