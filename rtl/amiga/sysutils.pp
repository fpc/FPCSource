{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004-2013 by Karoly Balogh

    Sysutils unit for AmigaOS & clones

    Based on Amiga 1.x version by Carl Eric Codere, and other
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
{$MODESWITCH OUT}
{ force ansistrings }
{$H+}

{$DEFINE HAS_SLEEP}
{$DEFINE HAS_OSERROR}

{ used OS file system APIs use ansistring }
{$define SYSUTILS_HAS_ANSISTR_FILEUTIL_IMPL}
{ OS has an ansistring/single byte environment variable API }
{$define SYSUTILS_HAS_ANSISTR_ENVVAR_IMPL}

{ Include platform independent interface part }
{$i sysutilh.inc}

{ Platform dependent calls }

Procedure AddDisk(const path:string);


implementation

uses dos,sysconst;

{$DEFINE FPC_FEXPAND_VOLUMES} (* Full paths begin with drive specification *)
{$DEFINE FPC_FEXPAND_DRIVESEP_IS_ROOT}
{$DEFINE FPC_FEXPAND_NO_DEFAULT_PATHS}

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
function PathConv(path: RawByteString): RawByteString; external name 'PATHCONVRBS';
procedure AddToList(var l: Pointer; h: LongInt); external name 'ADDTOLIST';
function RemoveFromList(var l: Pointer; h: LongInt): boolean; external name 'REMOVEFROMLIST';
function CheckInList(var l: Pointer; h: LongInt): pointer; external name 'CHECKINLIST';

var
  MOS_fileList: Pointer; external name 'AOS_FILELIST';


function dosLock(const name: String;
                 accessmode: Longint) : LongInt;
var
  buffer: array[0..255] of Char;
begin
  move(name[1],buffer,length(name));
  buffer[length(name)]:=#0;
  dosLock:=Lock(buffer,accessmode);
end;


function AmigaFileDateToDateTime(aDate: TDateStamp; out success: boolean): TDateTime;
var
  tmpSecs: DWord;
  tmpDate: TDateTime;
  tmpTime: TDateTime; 
  clockData: TClockData;
begin
  with aDate do
    tmpSecs:=(ds_Days * (24 * 60 * 60)) + (ds_Minute * 60) + (ds_Tick div TICKS_PER_SECOND);

  Amiga2Date(tmpSecs,@clockData);
{$WARNING TODO: implement msec values, if possible}
  with clockData do begin
     success:=TryEncodeDate(year,month,mday,tmpDate) and
              TryEncodeTime(hour,min,sec,0,tmpTime);
  end;

  result:=ComposeDateTime(tmpDate,tmpTime);
end;



{****************************************************************************
                              File Functions
****************************************************************************}
{$I-}{ Required for correct usage of these routines }


(****** non portable routines ******)

function FileOpen(const FileName: rawbytestring; Mode: Integer): LongInt;
var
  SystemFileName: RawByteString;
  dosResult: LongInt;
begin
  SystemFileName:=PathConv(ToSingleByteFileSystemEncodedFileName(FileName));
  {$WARNING FIX ME! To do: FileOpen Access Modes}
  dosResult:=Open(PChar(SystemFileName),MODE_OLDFILE);
  if dosResult=0 then
    dosResult:=-1
  else
    AddToList(MOS_fileList,dosResult);

  FileOpen:=dosResult;
end;


function FileGetDate(Handle: LongInt) : LongInt;
begin
  {$WARNING filegetdate call is dummy}
end;


function FileSetDate(Handle, Age: LongInt) : LongInt;
begin
  // Impossible under unix from FileHandle !!
  FileSetDate:=-1;
end;


function FileCreate(const FileName: RawByteString) : LongInt;
var
  SystemFileName: RawByteString;
  dosResult: LongInt;
begin
 SystemFileName:=PathConv(ToSingleByteFileSystemEncodedFileName(FileName));
 dosResult:=Open(PChar(FileName),MODE_NEWFILE);
 if dosResult=0 then
   dosResult:=-1
 else
   AddToList(MOS_fileList,dosResult);

 FileCreate:=dosResult;
end;


function FileCreate(const FileName: RawByteString; Rights: integer): LongInt;
begin
  {$WARNING FIX ME! To do: FileCreate Access Modes}
  FileCreate:=FileCreate(FileName);
end;

function FileCreate(const FileName: RawByteString; ShareMode: integer; Rights : Integer): LongInt;
begin
  {$WARNING FIX ME! To do: FileCreate Access Modes}
  FileCreate:=FileCreate(FileName);
end;


function FileRead(Handle: LongInt; Out Buffer; Count: LongInt): LongInt;
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

function FileSeek(Handle: LongInt; FOffset: Int64; Origin: Longint): Int64;
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


function FileTruncate(Handle: longint; Size: Int64): Boolean;
var
  dosResult: LongInt;
begin
  FileTruncate:=False;
  if Size > high (longint) then exit;
{$WARNING Possible support for 64-bit FS to be checked!}
  if (Handle<=0) then exit;

  dosResult:=SetFileSize(Handle, Size, OFFSET_BEGINNING);
  if (dosResult<0) then exit;

  FileTruncate:=True;
end;


function DeleteFile(const FileName: RawByteString) : Boolean;
var
  SystemFileName: RawByteString;
begin
  SystemFileName:=PathConv(ToSingleByteFileSystemEncodedFileName(FileName));

  DeleteFile:=dosDeleteFile(PChar(SystemFileName));
end;


function RenameFile(const OldName, NewName: string): Boolean;
var
  OldSystemFileName, NewSystemFileName: RawByteString;
begin
  OldSystemFileName:=PathConv(ToSingleByteFileSystemEncodedFileName(OldName));
  NewSystemFileName:=PathConv(ToSingleByteFileSystemEncodedFileName(NewName));

  RenameFile:=dosRename(PChar(OldSystemFileName), PChar(NewSystemFileName));
end;


(****** end of non portable routines ******)


function FileAge (const FileName : RawByteString): Longint;
var
  tmpName: RawByteString;
  tmpLock: Longint;
  tmpFIB : PFileInfoBlock;
  tmpDateTime: TDateTime;
  validFile: boolean;

begin
  validFile:=false;
  tmpName := PathConv(ToSingleByteFileSystemEncodedFileName(FileName));
  tmpLock := dosLock(tmpName, SHARED_LOCK);

  if (tmpLock <> 0) then begin
    new(tmpFIB);
    if Examine(tmpLock,tmpFIB) then begin
      tmpDateTime:=AmigaFileDateToDateTime(tmpFIB^.fib_Date,validFile);
    end;
    Unlock(tmpLock);
    dispose(tmpFIB);
  end;

  if validFile then
    result:=DateTimeToFileDate(tmpDateTime)
  else
    result:=-1;   
end;


function FileExists (const FileName : RawByteString) : Boolean;
var
  tmpLock: LongInt;
  tmpFIB : PFileInfoBlock;
  SystemFileName: RawByteString;
begin
  result:=false;
  SystemFileName:=PathConv(ToSingleByteFileSystemEncodedFileName(FileName));
  tmpLock := dosLock(PChar(SystemFileName), SHARED_LOCK);

  if (tmpLock <> 0) then begin
    new(tmpFIB);
    if Examine(tmpLock,tmpFIB) and (tmpFIB^.fib_DirEntryType <= 0) then
      result:=true;
    Unlock(tmpLock);
    dispose(tmpFIB);
  end;
end;


Function InternalFindFirst (Const Path : RawByteString; Attr : Longint; out Rslt : TAbstractSearchRec; var Name: RawByteString) : Longint;
var
  tmpStr: RawByteString;
  Anchor: PAnchorPath;
  tmpDateTime: TDateTime;
  validDate: boolean;
begin
  result:=-1; { We emulate Linux/Unix behaviour, and return -1 on errors. }
  tmpStr:=PathConv(ToSingleByteEncodedFileName(path));

  { $1e = faHidden or faSysFile or faVolumeID or faDirectory }
  Rslt.ExcludeAttr := (not Attr) and ($1e);
  Rslt.FindHandle  := 0;

  new(Anchor);
  FillChar(Anchor^,sizeof(TAnchorPath),#0);

  if MatchFirst(pchar(tmpStr),Anchor)<>0 then exit;
  Rslt.FindHandle := longint(Anchor);

  with Anchor^.ap_Info do begin
    Name := fib_FileName;
    SetCodePage(Name,DefaultFileSystemCodePage,false);

    Rslt.Size := fib_Size;
    Rslt.Time := DateTimeToFileDate(AmigaFileDateToDateTime(fib_Date,validDate));
    if not validDate then exit;

    { "128" is Windows "NORMALFILE" attribute. Some buggy code depend on this... :( (KB) }
    Rslt.Attr := 128;

    if fib_DirEntryType > 0 then Rslt.Attr:=Rslt.Attr or faDirectory;
    if ((fib_Protection and FIBF_READ) <> 0) and
       ((fib_Protection and FIBF_WRITE) = 0) then Rslt.Attr:=Rslt.Attr or faReadOnly;

    result:=0; { Return zero if everything went OK }
  end;
end;


Function InternalFindNext (var Rslt : TAbstractSearchRec; var Name : RawByteString) : Longint;
var
  Anchor: PAnchorPath;
  validDate: boolean;
begin
  result:=-1;

  Anchor:=PAnchorPath(Rslt.FindHandle);
  if not assigned(Anchor) then exit;
  if MatchNext(Anchor) <> 0 then exit;

  with Anchor^.ap_Info do begin
    Name := fib_FileName;
    SetCodePage(Name,DefaultFileSystemCodePage,false);
    Rslt.Size := fib_Size;
    Rslt.Time := DateTimeToFileDate(AmigaFileDateToDateTime(fib_Date,validDate));
    if not validDate then exit;

    { "128" is Windows "NORMALFILE" attribute. Some buggy code depend on this... :( (KB) }
    Rslt.Attr := 128;
    if fib_DirEntryType > 0 then Rslt.Attr:=Rslt.Attr or faDirectory;
    if ((fib_Protection and FIBF_READ) <> 0) and
       ((fib_Protection and FIBF_WRITE) = 0) then Rslt.Attr:=Rslt.Attr or faReadOnly;

    result:=0; { Return zero if everything went OK }
  end;
end;


Procedure InternalFindClose(var Handle: THandle);
var
  Anchor: PAnchorPath;
begin
  Anchor:=PAnchorPath(Handle);
  if not assigned(Anchor) then exit;
  MatchEnd(Anchor);
  Dispose(Anchor);
  Handle:=THandle(nil);
end;


(****** end of non portable routines ******)

Function FileGetAttr (Const FileName : RawByteString) : Longint;
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


Function FileSetAttr (Const Filename : RawByteString; Attr: longint) : Longint;
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

function DirectoryExists(const Directory: RawByteString): Boolean;
var
  tmpStr : String;
  tmpLock: LongInt;
  FIB    : PFileInfoBlock;
  SystemFileName: RawByteString;
begin
  result:=false;
  if (Directory='') or (InOutRes<>0) then exit;
  SystemFileName:=PathConv(ToSingleByteFileSystemEncodedFileName(FileName));

  tmpLock:=dosLock(PChar(SystemFileName),SHARED_LOCK);
  if tmpLock=0 then exit;

  FIB:=nil; new(FIB);

  if (Examine(tmpLock,FIB)=True) and (FIB^.fib_DirEntryType>0) then
    result:=True;

  if tmpLock<>0 then Unlock(tmpLock);
  if assigned(FIB) then dispose(FIB);
end;



{****************************************************************************
                              Misc Functions
****************************************************************************}

procedure SysBeep;
begin
// TODO
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
end;


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

function GetLastOSError: Integer;
begin
    result:=-1;
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

Function GetEnvironmentString(Index : Integer) : {$ifdef FPC_RTL_UNICODE}UnicodeString{$else}AnsiString{$endif};

begin
  // Result:=FPCGetEnvStrFromP(Envp,Index);
  Result:=Dos.EnvStr(Index);
end;

function ExecuteProcess (const Path: AnsiString; const ComLine: AnsiString;Flags:TExecuteFlags=[]):
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
                                  const ComLine: array of AnsiString;Flags:TExecuteFlags=[]): integer;
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

procedure Sleep(Milliseconds: cardinal);
begin
  // Amiga dos.library Delay() has precision of 1/50 seconds
  Delay(Milliseconds div 20);
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
