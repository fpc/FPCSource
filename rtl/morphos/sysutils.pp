{

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


{ * Include MorphOS specific includes * }
{$include execd.inc}
{$include execf.inc}
{$include timerd.inc}
{$include doslibd.inc}
{$include doslibf.inc}
{$include utilf.inc}

{ * Followings are implemented in the system unit! * }
function PathConv(path: shortstring): shortstring; external name 'PATHCONV';
procedure AddToList(var l: Pointer; h: LongInt); external name 'ADDTOLIST';
function RemoveFromList(var l: Pointer; h: LongInt): boolean; external name 'REMOVEFROMLIST';
function CheckInList(var l: Pointer; h: LongInt): pointer; external name 'CHECKINLIST';

var
  MOS_fileList: Pointer; external name 'MOS_FILELIST';


{****************************************************************************
                              File Functions
****************************************************************************}
{$I-}{ Required for correct usage of these routines }


(****** non portable routines ******)

function FileOpen(const FileName: string; Mode: Integer): LongInt;
var
  dosResult: LongInt;
  tmpStr   : array[0..255] of char;
begin
  {$WARNING FIX ME! To do: FileOpen Access Modes}
  tmpStr:=PathConv(FileName)+#0;
  dosResult:=Open(@tmpStr,MODE_OLDFILE);
  if dosResult=0 then
    dosResult:=-1
  else
    AddToList(MOS_fileList,dosResult);

  FileOpen:=dosResult;
end;


function FileGetDate(Handle: LongInt) : LongInt;
begin
end;


function FileSetDate(Handle, Age: LongInt) : LongInt;
begin
  // Impossible under unix from FileHandle !!
  FileSetDate:=-1;
end;


function FileCreate(const FileName: string) : LongInt;
var
  dosResult: LongInt;
  tmpStr   : array[0..255] of char;
begin
 tmpStr:=PathConv(FileName)+#0;
 dosResult:=Open(@tmpStr,MODE_NEWFILE);
 if dosResult=0 then
   dosResult:=-1
 else
   AddToList(MOS_fileList,dosResult);

 FileCreate:=dosResult;
end;


function FileCreate(const FileName: string; Mode: integer): LongInt;
begin
  {$WARNING FIX ME! To do: FileCreate Access Modes}
  FileCreate:=FileCreate(FileName);
end;


function FileRead(Handle: LongInt; var Buffer; Count: LongInt): LongInt;
begin
  FileRead:=-1;
  if (Count<=0) or (Handle<=0) then exit;

  FileRead:=dosRead(Handle,@Buffer,Count);
end;


function FileWrite(Handle: LongInt; const Buffer; Count: LongInt): LongInt;
begin
  FileWrite:=-1;
  if (Count<=0) or (Handle<=0) then exit;

  FileWrite:=dosWrite(Handle,@Buffer,Count);
end;


function FileSeek(Handle, FOffset, Origin: LongInt) : LongInt;
var
  seekMode: LongInt;
begin
  FileSeek:=-1;
  if (Handle<=0) then exit;

  case Origin of
    fsFromBeginning: seekMode:=OFFSET_BEGINNING;
    fsFromCurrent  : seekMode:=OFFSET_CURRENT;
    fsFromEnd      : seekMode:=OFFSET_END;
  end;

  FileSeek:=dosSeek(Handle, FOffset, seekMode);
end;

function FileSeek(Handle: LongInt; FOffset, Origin: Int64): Int64;
begin
  {$WARNING Need to add 64bit call }
  FileSeek:=FileSeek(Handle,LongInt(FOffset),LongInt(Origin));
end;


procedure FileClose(Handle: LongInt);
begin
  if (Handle<=0) then exit;

  dosClose(Handle);
  RemoveFromList(MOS_fileList,Handle);
end;


function FileTruncate(Handle, Size: LongInt): Boolean;
var
  dosResult: LongInt;
begin
  FileTruncate:=False;
  if (Handle<=0) then exit;

  dosResult:=SetFileSize(Handle, Size, OFFSET_BEGINNING);
  if (dosResult<0) then exit;

  FileTruncate:=True;
end;


function DeleteFile(const FileName: string) : Boolean;
var
  tmpStr: array[0..255] of char;
begin
  tmpStr:=PathConv(FileName)+#0;

  DeleteFile:=dosDeleteFile(@tmpStr);
end;


function RenameFile(const OldName, NewName: string): Boolean;
var
  tmpOldName, tmpNewName: array[0..255] of char;
begin
  tmpOldName:=PathConv(OldName)+#0;
  tmpNewName:=PathConv(NewName)+#0;

  RenameFile:=dosRename(tmpOldName, tmpNewName);
end;


(****** end of non portable routines ******)


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

Function FindFirst (Const Path : String; Attr : Longint; Out Rslt : TSearchRec) : Longint;
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


function DirectoryExists(const Directory: string): Boolean;
var
  tmpStr : array[0..255] of Char;
  tmpLock: LongInt;
  FIB    : PFileInfoBlock;
begin
  DirectoryExists:=False;
  If (Directory='') or (InOutRes<>0) then exit;
  tmpStr:=PathConv(Directory)+#0;
  tmpLock:=0;

  tmpLock:=Lock(@tmpStr,SHARED_LOCK);
  if tmpLock=0 then exit;

  FIB:=nil; new(FIB);

  if (Examine(tmpLock,FIB)=True) and (FIB^.fib_DirEntryType>0) then begin
    DirectoryExists:=True;
  end;

  if tmpLock<>0 then Unlock(tmpLock);
  if assigned(FIB) then dispose(FIB);
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
  InitInternationalGeneric; 
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
Function GetEnvironmentVariableCount : Integer;

begin
  // Result:=FPCCountEnvVar(EnvP);
  Result:=Dos.envCount;
end;

Function GetEnvironmentString(Index : Integer) : String;

begin
  // Result:=FPCGetEnvStrFromP(Envp,Index);
  Result:=Dos.EnvStr(Index);
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
