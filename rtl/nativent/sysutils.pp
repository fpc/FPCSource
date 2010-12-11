{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2010 by Sven Barth
    member of the Free Pascal development team

    Sysutils unit for NativeNT

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

uses
  ndk;

{$DEFINE HAS_SLEEP}
{$DEFINE HAS_CREATEGUID}

{ Include platform independent interface part }
{$i sysutilh.inc}

implementation

  uses
    sysconst, ndkutils;

{$DEFINE FPC_NOGENERICANSIROUTINES}

{ Include platform independent implementation part }
{$i sysutils.inc}

{****************************************************************************
                              File Functions
****************************************************************************}

function FileOpen(const FileName : string; Mode : Integer) : THandle;
const
  AccessMode: array[0..2] of ACCESS_MASK  = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of ULONG = (
               0,
               0,
               FILE_SHARE_READ,
               FILE_SHARE_WRITE,
               FILE_SHARE_READ or FILE_SHARE_WRITE);
var
  ntstr: UNICODE_STRING;
  objattr: OBJECT_ATTRIBUTES;
  iostatus: IO_STATUS_BLOCK;
begin
  AnsiStrToNtStr(FileName, ntstr);
  InitializeObjectAttributes(objattr, @ntstr, 0, 0, Nil);
  NtCreateFile(@Result, AccessMode[Mode and 3] or NT_SYNCHRONIZE, @objattr,
    @iostatus, Nil, FILE_ATTRIBUTE_NORMAL, ShareMode[(Mode and $F0) shr 4],
    FILE_OPEN, FILE_NON_DIRECTORY_FILE or FILE_SYNCHRONOUS_IO_NONALERT, Nil, 0);
  FreeNtStr(ntstr);
end;


function FileCreate(const FileName : String) : THandle;
var
  ntstr: UNICODE_STRING;
  objattr: OBJECT_ATTRIBUTES;
  iostatus: IO_STATUS_BLOCK;
  res: NTSTATUS;
begin
  AnsiStrToNTStr(FileName, ntstr);
  InitializeObjectAttributes(objattr, @ntstr, 0, 0, Nil);
  NtCreateFile(@Result, GENERIC_READ or GENERIC_WRITE or NT_SYNCHRONIZE,
    @objattr, @iostatus, Nil, FILE_ATTRIBUTE_NORMAL, 0, FILE_OVERWRITE_IF,
    FILE_NON_DIRECTORY_FILE or FILE_SYNCHRONOUS_IO_NONALERT, Nil, 0);
  FreeNtStr(ntstr);
end;


function FileCreate(const FileName : String; Mode: longint) : THandle;
begin
  FileCreate := FileCreate(FileName);
end;


function FileRead(Handle : THandle; out Buffer; Count : longint) : Longint;
var
  iostatus: IO_STATUS_BLOCK;
  res: NTSTATUS;
begin
  res := NtReadFile(Handle, 0, Nil, Nil, @iostatus, @Buffer, Count, Nil, Nil);

  if res = STATUS_PENDING then begin
    res := NtWaitForSingleObject(Handle, False, Nil);
    if NT_SUCCESS(res) then
      res := iostatus.union1.Status;
  end;

  if NT_SUCCESS(res) then
    Result := LongInt(iostatus.Information)
  else
    Result := -1;
end;


function FileWrite(Handle : THandle; const Buffer; Count : Longint) : Longint;
var
  iostatus: IO_STATUS_BLOCK;
  res: NTSTATUS;
begin
  res := NtWriteFile(Handle, 0, Nil, Nil, @iostatus, @Buffer, Count, Nil,
           Nil);

  if res = STATUS_PENDING then begin
    res := NtWaitForSingleObject(Handle, False, Nil);
    if NT_SUCCESS(res) then
      res := iostatus.union1.Status;
  end;

  if NT_SUCCESS(res) then
    Result := LongInt(iostatus.Information)
  else
    Result := -1;
end;


function FileSeek(Handle : THandle;FOffset,Origin : Longint) : Longint;
begin
  Result := longint(FileSeek(Handle, Int64(FOffset), Origin));
end;


function FileSeek(Handle : THandle; FOffset: Int64; Origin: Longint) : Int64;
const
  ErrorCode = $FFFFFFFFFFFFFFFF;
var
  position: FILE_POSITION_INFORMATION;
  standard: FILE_STANDARD_INFORMATION;
  iostatus: IO_STATUS_BLOCK;
  res: NTSTATUS;
begin
  { determine the new position }
  case Origin of
    fsFromBeginning:
      position.CurrentByteOffset.QuadPart := FOffset;
    fsFromCurrent: begin
      res := NtQueryInformationFile(Handle, @iostatus, @position,
               SizeOf(FILE_POSITION_INFORMATION), FilePositionInformation);
      if res < 0 then begin
        Result := ErrorCode;
        Exit;
      end;
      position.CurrentByteOffset.QuadPart :=
        position.CurrentByteOffset.QuadPart + FOffset;
    end;
    fsFromEnd: begin
      res := NtQueryInformationFile(Handle, @iostatus, @standard,
               SizeOf(FILE_STANDARD_INFORMATION), FileStandardInformation);
      if res < 0 then begin
        Result := ErrorCode;
        Exit;
      end;
      position.CurrentByteOffset.QuadPart := standard.EndOfFile.QuadPart +
                                               FOffset;
    end;
    else begin
      Result := ErrorCode;
      Exit;
    end;
  end;

  { set the new position }
  res := NtSetInformationFile(Handle, @iostatus, @position,
           SizeOf(FILE_POSITION_INFORMATION), FilePositionInformation);
  if res < 0 then
    Result := ErrorCode
  else
    Result := position.CurrentByteOffset.QuadPart;
end;


procedure FileClose(Handle : THandle);
begin
  NtClose(Handle);
end;


function FileTruncate(Handle : THandle;Size: Int64) : boolean;
var
  endoffileinfo: FILE_END_OF_FILE_INFORMATION;
  allocinfo: FILE_ALLOCATION_INFORMATION;
  iostatus: IO_STATUS_BLOCK;
  res: NTSTATUS;
begin
  // based on ReactOS' SetEndOfFile
  endoffileinfo.EndOfFile.QuadPart := Size;
  res := NtSetInformationFile(Handle, @iostatus, @endoffileinfo,
           SizeOf(FILE_END_OF_FILE_INFORMATION), FileEndOfFileInformation);
  if NT_SUCCESS(res) then begin
    allocinfo.AllocationSize.QuadPart := Size;
    res := NtSetInformationFile(handle, @iostatus, @allocinfo,
             SizeOf(FILE_ALLOCATION_INFORMATION), FileAllocationInformation);
    Result := NT_SUCCESS(res);
  end else
    Result := False;
end;

function NTToDosTime(const NtTime: LARGE_INTEGER): LongInt;
var
  userdata: PKUSER_SHARED_DATA;
  local, bias: LARGE_INTEGER;
  fields: TIME_FIELDS;
  zs: LongInt;
begin
  userdata := SharedUserData;
  repeat
    bias.u.HighPart := userdata^.TimeZoneBias.High1Time;
    bias.u.LowPart := userdata^.TimeZoneBias.LowPart;
  until bias.u.HighPart = userdata^.TimeZoneBias.High2Time;

  local.QuadPart := NtTime.QuadPart - bias.QuadPart;

  RtlTimeToTimeFields(@local, @fields);

  { from objpas\datutil.inc\DateTimeToDosDateTime }
  Result := - 1980;
  Result := Result + fields.Year and 127;
  Result := Result shl 4;
  Result := Result + fields.Month;
  Result := Result shl 5;
  Result := Result + fields.Day;
  Result := Result shl 16;
  zs := fields.Hour;
  zs := zs shl 6;
  zs := zs + fields.Minute;
  zs := zs shl 5;
  zs := zs + fields.Second div 2;
  Result := Result + (zs and $ffff);
end;

function DosToNtTime(aDTime: LongInt; var aNtTime: LARGE_INTEGER): Boolean;
var
  fields: TIME_FIELDS;
  local, bias: LARGE_INTEGER;
  userdata: PKUSER_SHARED_DATA;
begin
  { from objpas\datutil.inc\DosDateTimeToDateTime }
  fields.Second := (aDTime and 31) * 2;
  aDTime := aDTime shr 5;
  fields.Minute := aDTime and 63;
  aDTime := aDTime shr 6;
  fields.Hour := aDTime and 31;
  aDTime := aDTime shr 5;
  fields.Day := aDTime and 31;
  aDTime := aDTime shr 5;
  fields.Month := aDTime and 15;
  aDTime := aDTime shr 4;
  fields.Year := aDTime + 1980;

  Result := RtlTimeFieldsToTime(@fields, @local);
  if not Result then
    Exit;

  userdata := SharedUserData;
  repeat
    bias.u.HighPart := userdata^.TimeZoneBias.High1Time;
    bias.u.LowPart := userdata^.TimeZoneBias.LowPart;
  until bias.u.HighPart = userdata^.TimeZoneBias.High2Time;

  aNtTime.QuadPart := local.QuadPart + bias.QuadPart;
end;

function FileAge(const FileName: String): Longint;
begin
  Result := -1;
end;


function FileExists(const FileName: String): Boolean;
var
  ntstr: UNICODE_STRING;
  objattr: OBJECT_ATTRIBUTES;
  res: NTSTATUS;
  iostatus: IO_STATUS_BLOCK;
  h: THandle;
begin
  AnsiStrToNtStr(FileName, ntstr);
  InitializeObjectAttributes(objattr, @ntstr, 0, 0, Nil);
  res := NtOpenFile(@h, 0, @objattr, @iostatus,
           FILE_SHARE_READ or FILE_SHARE_WRITE,
           FILE_NON_DIRECTORY_FILE or FILE_SYNCHRONOUS_IO_NONALERT);
  Result := NT_SUCCESS(res);

  if Result then
    NtClose(h);
  FreeNtStr(ntstr);
end;


function DirectoryExists(const Directory : String) : Boolean;
var
  ntstr: UNICODE_STRING;
  objattr: OBJECT_ATTRIBUTES;
  res: NTSTATUS;
  iostatus: IO_STATUS_BLOCK;
  h: THandle;
begin
  AnsiStrToNtStr(Directory, ntstr);
  InitializeObjectAttributes(objattr, @ntstr, 0, 0, Nil);

  { first test wether this is a object directory }
  res := NtOpenDirectoryObject(@h, 0, @objattr);
  if NT_SUCCESS(res) then
    Result := True
  else begin
    if res = STATUS_OBJECT_TYPE_MISMATCH then begin
      { this is a file object! }
      res := NtOpenFile(@h, 0, @objattr, @iostatus,
        FILE_SHARE_READ or FILE_SHARE_WRITE,
        FILE_DIRECTORY_FILE or FILE_SYNCHRONOUS_IO_NONALERT);
      Result := NT_SUCCESS(res);
    end else
      Result := False;
  end;

  if Result then
    NtClose(h);
  FreeNtStr(ntstr);
end;


function FindMatch(var f: TSearchRec): Longint;
begin
  Result := -1;
end;


function FindFirst(const Path: String; Attr: Longint; out Rslt: TSearchRec): Longint;
begin
  Result := -1;
end;


function FindNext(var Rslt: TSearchRec): Longint;
begin
  Result := -1;
end;


procedure FindClose(var F: TSearchrec);
begin
  { empty }
end;


function FileGetDate(Handle: THandle): Longint;
var
  res: NTSTATUS;
  basic: FILE_BASIC_INFORMATION;
  iostatus: IO_STATUS_BLOCK;
begin
  res := NtQueryInformationFile(Handle, @iostatus, @basic,
           SizeOf(FILE_BASIC_INFORMATION), FileBasicInformation);
  if NT_SUCCESS(res) then
    Result := NtToDosTime(basic.LastWriteTime)
  else
    Result := -1;
end;


function FileSetDate(Handle: THandle;Age: Longint): Longint;
var
  res: NTSTATUS;
  basic: FILE_BASIC_INFORMATION;
  iostatus: IO_STATUS_BLOCK;
begin
  res := NtQueryInformationFile(Handle, @iostatus, @basic,
           SizeOf(FILE_BASIC_INFORMATION), FileBasicInformation);
  if NT_SUCCESS(res) then begin
    if not DosToNtTime(Age, basic.LastWriteTime) then begin
      Result := -1;
      Exit;
    end;

    res := NtSetInformationFile(Handle, @iostatus, @basic,
             SizeOf(FILE_BASIC_INFORMATION), FileBasicInformation);
    if NT_SUCCESS(res) then
      Result := 0
    else
      Result := res;
  end else
    Result := res;
end;


function FileGetAttr(const FileName: String): Longint;
var
  objattr: OBJECT_ATTRIBUTES;
  info: FILE_NETWORK_OPEN_INFORMATION;
  res: NTSTATUS;
  ntstr: UNICODE_STRING;
begin
  AnsiStrToNtStr(FileName, ntstr);
  InitializeObjectAttributes(objattr, @ntstr, 0, 0, Nil);

  res := NtQueryFullAttributesFile(@objattr, @info);
  if NT_SUCCESS(res) then
    Result := info.FileAttributes
  else
    Result := 0;

  FreeNtStr(ntstr);
end;


function FileSetAttr(const Filename: String; Attr: LongInt): Longint;
var
  h: THandle;
  objattr: OBJECT_ATTRIBUTES;
  ntstr: UNICODE_STRING;
  basic: FILE_BASIC_INFORMATION;
  res: NTSTATUS;
  iostatus: IO_STATUS_BLOCK;
begin
  AnsiStrToNtStr(Filename, ntstr);
  InitializeObjectAttributes(objattr, @ntstr, 0, 0, Nil);
  res := NtOpenFile(@h,
           NT_SYNCHRONIZE or FILE_READ_ATTRIBUTES or FILE_WRITE_ATTRIBUTES,
           @objattr, @iostatus,
           FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
           FILE_SYNCHRONOUS_IO_NONALERT);

  FreeNtStr(ntstr);

  if NT_SUCCESS(res) then begin
    res := NtQueryInformationFile(h, @iostatus, @basic,
             SizeOf(FILE_BASIC_INFORMATION), FileBasicInformation);

    if NT_SUCCESS(res) then begin
      basic.FileAttributes := Attr;
      Result := NtSetInformationFile(h, @iostatus, @basic,
                  SizeOf(FILE_BASIC_INFORMATION), FileBasicInformation);
    end;

    NtClose(h);
  end else
    Result := res;
end;


function DeleteFile(const FileName: String): Boolean;
var
  h: THandle;
  objattr: OBJECT_ATTRIBUTES;
  ntstr: UNICODE_STRING;
  dispinfo: FILE_DISPOSITION_INFORMATION;
  res: NTSTATUS;
  iostatus: IO_STATUS_BLOCK;
begin
  AnsiStrToNtStr(Filename, ntstr);
  InitializeObjectAttributes(objattr, @ntstr, 0, 0, Nil);
  res := NtOpenFile(@h, NT_DELETE, @objattr, @iostatus,
           FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
           FILE_NON_DIRECTORY_FILE);

  FreeNtStr(ntstr);

  if NT_SUCCESS(res) then begin
    dispinfo.DeleteFile := True;

    res := NtSetInformationFile(h, @iostatus, @dispinfo,
             SizeOf(FILE_DISPOSITION_INFORMATION), FileDispositionInformation);

    Result := NT_SUCCESS(res);

    NtClose(h);
  end else
    Result := False;
end;


function RenameFile(const OldName, NewName: String): Boolean;
var
  h: THandle;
  objattr: OBJECT_ATTRIBUTES;
  iostatus: IO_STATUS_BLOCK;
  dest, src: UNICODE_STRING;
  renameinfo: PFILE_RENAME_INFORMATION;
  res: LongInt;
begin
  { check whether the destination exists first }
  AnsiStrToNtStr(NewName, dest);
  InitializeObjectAttributes(objattr, @dest, 0, 0, Nil);

  res := NtCreateFile(@h, 0, @objattr, @iostatus, Nil, 0,
           FILE_SHARE_READ or FILE_SHARE_WRITE, FILE_OPEN,
           FILE_NON_DIRECTORY_FILE, Nil, 0);
  if NT_SUCCESS(res) then begin
    { destination already exists => error }
    NtClose(h);
    Result := False;
  end else begin
    AnsiStrToNtStr(OldName, src);
    InitializeObjectAttributes(objattr, @src, 0, 0, Nil);

    res := NtCreateFile(@h,
             GENERIC_ALL or NT_SYNCHRONIZE or FILE_READ_ATTRIBUTES,
             @objattr, @iostatus, Nil, 0, FILE_SHARE_READ or FILE_SHARE_WRITE,
             FILE_OPEN, FILE_OPEN_FOR_BACKUP_INTENT or FILE_OPEN_REMOTE_INSTANCE
             or FILE_NON_DIRECTORY_FILE or FILE_SYNCHRONOUS_IO_NONALERT, Nil,
             0);

    if NT_SUCCESS(res) then begin
      renameinfo := GetMem(SizeOf(FILE_RENAME_INFORMATION) + dest.Length);
      with renameinfo^ do begin
        ReplaceIfExists := False;
        RootDirectory := 0;
        FileNameLength := dest.Length;
        Move(dest.Buffer^, renameinfo^.FileName, dest.Length);
      end;

      res := NtSetInformationFile(h, @iostatus, renameinfo,
               SizeOf(FILE_RENAME_INFORMATION) + dest.Length,
               FileRenameInformation);
      if not NT_SUCCESS(res) then begin
        { this could happen if src and destination reside on different drives,
          so we need to copy the file manually }
        {$message warning 'RenameFile: Implement file copy!'}
        Result := False;
      end else
        Result := True;

      NtClose(h);
    end else
      Result := False;

    FreeNtStr(src);
  end;

  FreeNtStr(dest);
end;


{****************************************************************************
                              Disk Functions
****************************************************************************}

function diskfree(drive: byte): int64;
begin
  { here the mount manager needs to be queried }
  Result := -1;
end;


function disksize(drive: byte): int64;
begin
  { here the mount manager needs to be queried }
  Result := -1;
end;


function GetCurrentDir: String;
begin
  GetDir(0, result);
end;


function SetCurrentDir(const NewDir: String): Boolean;
begin
{$I-}
  ChDir(NewDir);
{$I+}
  Result := IOResult = 0;
end;


function CreateDir(const NewDir: String): Boolean;
begin
{$I-}
  MkDir(NewDir);
{$I+}
  Result := IOResult = 0;
end;


function RemoveDir(const Dir: String): Boolean;
begin
{$I-}
  RmDir(Dir);
{$I+}
  Result := IOResult = 0;
end;


{****************************************************************************
                              Time Functions
****************************************************************************}


procedure GetLocalTime(var SystemTime: TSystemTime);
var
  bias, syst: LARGE_INTEGER;
  fields: TIME_FIELDS;
  userdata: PKUSER_SHARED_DATA;
begin
  // get UTC time
  userdata := SharedUserData;
  repeat
    syst.u.HighPart := userdata^.SystemTime.High1Time;
    syst.u.LowPart := userdata^.SystemTime.LowPart;
  until syst.u.HighPart = userdata^.SystemTime.High2Time;

  // adjust to local time
  repeat
    bias.u.HighPart := userdata^.TimeZoneBias.High1Time;
    bias.u.LowPart := userdata^.TimeZoneBias.LowPart;
  until bias.u.HighPart = userdata^.TimeZoneBias.High2Time;
  syst.QuadPart := syst.QuadPart - bias.QuadPart;

  RtlTimeToTimeFields(@syst, @fields);

  SystemTime.Year := fields.Year;
  SystemTime.Month := fields.Month;
  SystemTime.Day := fields.Day;
  SystemTime.Hour := fields.Hour;
  SystemTime.Minute := fields.Minute;
  SystemTime.Second := fields.Second;
  SystemTime.Millisecond := fields.MilliSeconds;
end;


{****************************************************************************
                              Misc Functions
****************************************************************************}

procedure sysbeep;
begin
  { empty }
end;

procedure InitInternational;
begin
  InitInternationalGeneric;
end;


{****************************************************************************
                           Target Dependent
****************************************************************************}

function SysErrorMessage(ErrorCode: Integer): String;
begin
  Result := 'NT error code: 0x' + IntToHex(ErrorCode, 8);
end;

{****************************************************************************
                              Initialization code
****************************************************************************}

function wstrlen(p: PWideChar): LongInt; external name 'FPC_PWIDECHAR_LENGTH';

function GetEnvironmentVariable(const EnvVar: String): String;
var
   s : string;
   i : longint;
   hp: pwidechar;
   len: sizeint;
begin
   { TODO : test once I know how to execute processes }
   Result:='';
   hp:=PPEB(CurrentPEB)^.ProcessParameters^.Environment;
   while hp^<>#0 do
     begin
        len:=UnicodeToUTF8(Nil, hp, 0);
        SetLength(s,len);
        UnicodeToUTF8(PChar(s), hp, len);
        //s:=strpas(hp);
        i:=pos('=',s);
        if uppercase(copy(s,1,i-1))=upcase(envvar) then
          begin
             Result:=copy(s,i+1,length(s)-i);
             break;
          end;
        { next string entry}
        hp:=hp+wstrlen(hp)+1;
     end;
end;

function GetEnvironmentVariableCount: Integer;
var
  hp : pwidechar;
begin
  Result:=0;
  hp:=PPEB(CurrentPEB)^.ProcessParameters^.Environment;
  If (Hp<>Nil) then
    while hp^<>#0 do
      begin
      Inc(Result);
      hp:=hp+wstrlen(hp)+1;
      end;
end;

function GetEnvironmentString(Index: Integer): String;
var
  hp : pwidechar;
  len: sizeint;
begin
  Result:='';
  hp:=PPEB(CurrentPEB)^.ProcessParameters^.Environment;
  If (Hp<>Nil) then
    begin
    while (hp^<>#0) and (Index>1) do
      begin
      Dec(Index);
      hp:=hp+wstrlen(hp)+1;
      end;
    If (hp^<>#0) then
      begin
        len:=UnicodeToUTF8(Nil, hp, 0);
        SetLength(Result, len);
        UnicodeToUTF8(PChar(Result), hp, len);
      end;
    end;
end;


function ExecuteProcess(const Path: AnsiString; const ComLine: AnsiString;
  Flags: TExecuteFlags = []): Integer;
begin
  { TODO : implement }
  Result := 0;
end;

function ExecuteProcess(const Path: AnsiString;
  const ComLine: Array of AnsiString; Flags:TExecuteFlags = []): Integer;
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
  ExecuteProcess := ExecuteProcess (Path, CommandLine,Flags);
end;

procedure Sleep(Milliseconds: Cardinal);
const
  DelayFactor = 10000;
var
  interval: LARGE_INTEGER;
begin
  interval.QuadPart := - Milliseconds * DelayFactor;
  NtDelayExecution(False, @interval);
end;

{****************************************************************************
                              Initialization code
****************************************************************************}

initialization
  InitExceptions;       { Initialize exceptions. OS independent }
  InitInternational;    { Initialize internationalization settings }
  OnBeep := @SysBeep;
finalization
  DoneExceptions;
end.
