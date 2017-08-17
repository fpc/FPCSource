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
{ $ define SYSUTILS_HAS_ANSISTR_ENVVAR_IMPL}
{$define SYSUTILS_HAS_UNICODESTR_ENVVAR_IMPL}

{ Include platform independent interface part }
{$i sysutilh.inc}

implementation

  uses
    sysconst;

{ $DEFINE FPC_NOGENERICANSIROUTINES}

{ Include platform independent implementation part }
{$i sysutils.inc}

{****************************************************************************
                              File Functions
****************************************************************************}

function FileOpen(const FileName : UnicodeString; Mode : Integer) : THandle;
begin
  Debugger('TODO FileOpen');
end;


function FileCreate(const FileName : UnicodeString) : THandle;
begin
  Debugger('TODO FileCreate');
end;


function FileCreate(const FileName : UnicodeString; Rights: longint) : THandle;
begin
  Debugger('TODO FileCreate');
end;


function FileCreate(const FileName : UnicodeString; ShareMode : longint; Rights: longint) : THandle;
begin
  Debugger('TODO FileCreate');
end;


function FileRead(Handle : THandle; out Buffer; Count : longint) : Longint;
begin
  Debugger('TODO FileRead');
end;


function FileWrite(Handle : THandle; const Buffer; Count : Longint) : Longint;
begin
  Debugger('TODO FileWrite');
end;


function FileSeek(Handle : THandle;FOffset,Origin : Longint) : Longint;
begin
  Debugger('TODO FileSeek');
end;


function FileSeek(Handle : THandle; FOffset: Int64; Origin: Longint) : Int64;
begin
  Debugger('TODO FileSeek');
end;


procedure FileClose(Handle : THandle);
begin
  Debugger('TODO FileClose');
end;


function FileTruncate(Handle : THandle;Size: Int64) : boolean;
begin
  Debugger('TODO FileTruncate');
end;


function FileAge(const FileName: UnicodeString): Longint;
begin
  Debugger('TODO FileAge');
  { TODO }
  Result := -1;
end;


function FileExists(const FileName: UnicodeString): Boolean;
begin
  Debugger('TODO FileExists');
end;


Procedure InternalFindClose(var Handle: THandle);
begin
  Debugger('TODO InternalFindClose');
end;


Function InternalFindNext (Var Rslt : TAbstractSearchRec; var Name: UnicodeString) : Longint;
begin
  Debugger('TODO InternalFindNext');
end;


Function InternalFindFirst (Const Path : UnicodeString; Attr : Longint; out Rslt : TAbstractSearchRec; var Name : UnicodeString) : Longint;
begin
  Debugger('TODO InternalFindFirst');
end;


function DirectoryExists(const Directory : UnicodeString) : Boolean;
begin
  Debugger('TODO DirectoryExist');
end;


function FileGetDate(Handle: THandle): Longint;
begin
  Debugger('TODO FileGetDate : Implementation required');
end;


function FileSetDate(Handle: THandle;Age: Longint): Longint;
begin
  Debugger('TODO FileSetDate : Implementation required');
end;


function FileGetAttr(const FileName: UnicodeString): Longint;
begin
  Debugger('TODO FileGetAttr : Implementation required');
end;


function FileSetAttr(const Filename: UnicodeString; Attr: LongInt): Longint;
begin
  Debugger('TODO FileSetAttr : Implementation required');
end;


function DeleteFile(const FileName: UnicodeString): Boolean;
begin
  Debugger('TODO DeleteFile : Implementation required');
end;


function RenameFile(const OldName, NewName: UnicodeString): Boolean;
begin
  Debugger('TODO RenameFile : Implementation required');
end;


{****************************************************************************
                              Disk Functions
****************************************************************************}

function diskfree(drive: byte): int64;
begin
  Debugger('TODO diskfree : Implementation required');
  { here the mount manager needs to be queried }
  Result := -1;
end;


function disksize(drive: byte): int64;
begin
  Debugger('TODO disksize : Implementation required');
  { here the mount manager needs to be queried }
  Result := -1;
end;


{****************************************************************************
                              Time Functions
****************************************************************************}


procedure GetLocalTime(var SystemTime: TSystemTime);
begin
  Debugger('TODO GetLocalTime Implementation required');
end;


{****************************************************************************
                              Misc Functions
****************************************************************************}

procedure sysbeep;
begin
  Debugger('TODO sysbeep : Implementation required');
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
  Debugger('TODO SysErrorMessage required');
  Result := 'UEFI error code: 0x' + IntToHex(ErrorCode, 8);
end;

{****************************************************************************
                              Initialization code
****************************************************************************}

{$ifdef SYSUTILS_HAS_UNICODESTR_ENVVAR_IMPL}
function GetEnvironmentVariable(const EnvVar: UnicodeString): UnicodeString;
begin
  Debugger('TODO GetEnvironmentVariable');
  Result := '';
end;
{$else}
function GetEnvironmentVariable(const EnvVar: String): String;
begin
  Debugger('TODO GetEnvironmentVariable');
  Result := '';
end;
{$endif}

function GetEnvironmentVariableCount: Integer;
begin
  Debugger('TODO GetEnvironmentVariableCount');
  Result := 0;
end;

function GetEnvironmentString(Index: Integer): {$ifdef FPC_RTL_UNICODE}UnicodeString{$else}AnsiString{$endif};
begin
  Debugger('TODO GetEnvironmentString');
  Result := '';
end;

function ExecuteProcess(const Path: AnsiString; const ComLine: AnsiString;
  Flags: TExecuteFlags = []): Integer;
begin
  Debugger('TODO ExecuteProcess');
  { TODO : implement }
  Result := 0;
end;

function ExecuteProcess(const Path: AnsiString;
  const ComLine: Array of AnsiString; Flags:TExecuteFlags = []): Integer;
begin
  Debugger('TODO ExecuteProcess');
  Result := 0;
end;

function ExecuteProcess(const Path: RawByteString; const ComLine: RawByteString;
  Flags: TExecuteFlags = []): Integer;
begin
  Debugger('TODO ExecuteProcess');
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
  Debugger('TODO ExecuteProcess');
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
  Debugger('TODO Sleep');
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
