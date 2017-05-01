{
    Sysutils unit for UEFI

    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Olivier Coursière

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
{$modeswitch typehelpers}
{$modeswitch advancedrecords}

{$DEFINE HAS_SLEEP}

{ TODO : check this for UEFI }
{ used OS file system APIs use ansistring }
{$define SYSUTILS_HAS_UNICODESTR_FILEUTIL_IMPL}
{ OS has an ansistring/single byte environment variable API (actually it's
  unicodestring, but that's not yet implemented) }
{$define SYSUTILS_HAS_ANSISTR_ENVVAR_IMPL}

{ Include platform independent interface part }
{$i sysutilh.inc}

implementation

  uses
    sysconst;

{$DEFINE FPC_NOGENERICANSIROUTINES}

{ Include platform independent implementation part }
{$i sysutils.inc}

{****************************************************************************
                              File Functions
****************************************************************************}

function FileOpen(const FileName : UnicodeString; Mode : Integer) : THandle;
begin
end;


function FileCreate(const FileName : UnicodeString) : THandle;
begin
end;


function FileCreate(const FileName : UnicodeString; Rights: longint) : THandle;
begin
end;


function FileCreate(const FileName : UnicodeString; ShareMode : longint; Rights: longint) : THandle;
begin
end;


function FileRead(Handle : THandle; out Buffer; Count : longint) : Longint;
begin
end;


function FileWrite(Handle : THandle; const Buffer; Count : Longint) : Longint;
begin
end;


function FileSeek(Handle : THandle;FOffset,Origin : Longint) : Longint;
begin
end;


function FileSeek(Handle : THandle; FOffset: Int64; Origin: Longint) : Int64;
begin
end;


procedure FileClose(Handle : THandle);
begin
end;


function FileTruncate(Handle : THandle;Size: Int64) : boolean;
begin
end;


function FileAge(const FileName: UnicodeString): Longint;
begin
  { TODO }
  Result := -1;
end;


function FileExists(const FileName: UnicodeString): Boolean;
begin
end;


Procedure InternalFindClose(var Handle: THandle);
begin
end;


Function InternalFindNext (Var Rslt : TAbstractSearchRec; var Name: UnicodeString) : Longint;
begin
end;


Function InternalFindFirst (Const Path : UnicodeString; Attr : Longint; out Rslt : TAbstractSearchRec; var Name : UnicodeString) : Longint;
begin
end;


function DirectoryExists(const Directory : UnicodeString) : Boolean;
begin
end;


function FileGetDate(Handle: THandle): Longint;
begin
end;


function FileSetDate(Handle: THandle;Age: Longint): Longint;
begin
end;


function FileGetAttr(const FileName: UnicodeString): Longint;
begin
end;


function FileSetAttr(const Filename: UnicodeString; Attr: LongInt): Longint;
begin
end;


function DeleteFile(const FileName: UnicodeString): Boolean;
begin
end;


function RenameFile(const OldName, NewName: UnicodeString): Boolean;
begin
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


{****************************************************************************
                              Time Functions
****************************************************************************}


procedure GetLocalTime(var SystemTime: TSystemTime);
begin
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
  Result := 'UEFI error code: 0x' + IntToHex(ErrorCode, 8);
end;

{****************************************************************************
                              Initialization code
****************************************************************************}

function GetEnvironmentVariable(const EnvVar: String): String;
begin
  Result := '';
end;

function GetEnvironmentVariableCount: Integer;
begin
  Result := 0;
end;

function GetEnvironmentString(Index: Integer): {$ifdef FPC_RTL_UNICODE}UnicodeString{$else}AnsiString{$endif};
begin
  Result := '';
end;

function ExecuteProcess(const Path: AnsiString; const ComLine: AnsiString;
  Flags: TExecuteFlags = []): Integer;
begin
  { TODO : implement }
  Result := 0;
end;

function ExecuteProcess(const Path: AnsiString;
  const ComLine: Array of AnsiString; Flags:TExecuteFlags = []): Integer;
begin
  Result := 0;
end;

function ExecuteProcess(const Path: RawByteString; const ComLine: RawByteString;
  Flags: TExecuteFlags = []): Integer;
begin
  { TODO : implement }
  Result := 0;
end;

function ExecuteProcess(const Path: RawByteString;
  const ComLine: Array of RawByteString; Flags:TExecuteFlags = []): Integer;
var
  CommandLine: RawByteString;
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

function ExecuteProcess(const Path: UnicodeString; const ComLine: UnicodeString;
  Flags: TExecuteFlags = []): Integer;
begin
  { TODO : implement }
  Result := 0;
end;

function ExecuteProcess(const Path: UnicodeString;
  const ComLine: Array of UnicodeString; Flags:TExecuteFlags = []): Integer;
var
  CommandLine: UnicodeString;
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
begin
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
