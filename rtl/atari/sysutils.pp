{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by Free Pascal development team

    Sysutils unit for Atari

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

{$DEFINE OS_FILESETDATEBYNAME}
{$DEFINE HAS_SLEEP}
{$DEFINE HAS_OSERROR}

{OS has only 1 byte version for ExecuteProcess}
{$define executeprocuni}

{ used OS file system APIs use ansistring }
{$define SYSUTILS_HAS_ANSISTR_FILEUTIL_IMPL}
{ OS has an ansistring/single byte environment variable API }
{$define SYSUTILS_HAS_ANSISTR_ENVVAR_IMPL}

{ Include platform independent interface part }
{$i sysutilh.inc}

{ Platform dependent calls }


implementation

uses
{  dos,} sysconst;

{$DEFINE FPC_FEXPAND_UNC} (* UNC paths are supported *)
{$DEFINE FPC_FEXPAND_DRIVES} (* Full paths begin with drive specification *)

{ Include platform independent implementation part }
{$i sysutils.inc}





{****************************************************************************
                              File Functions
****************************************************************************}
{$I-}{ Required for correct usage of these routines }


(****** non portable routines ******)

function FileOpen(const FileName: rawbytestring; Mode: Integer): THandle;
begin
end;


function FileGetDate(Handle: THandle) : LongInt;
begin
  result:=-1;
end;


function FileSetDate(Handle: THandle; Age: LongInt) : LongInt;
begin
  result:=0;
end;


function FileSetDate(const FileName: RawByteString; Age: LongInt) : LongInt;
begin
  result:=-1;
end;


function FileCreate(const FileName: RawByteString) : THandle;
begin
  result:=-1;
end;

function FileCreate(const FileName: RawByteString; Rights: integer): THandle;
begin
  {$WARNING FIX ME! To do: FileCreate Access Modes}
  FileCreate:=FileCreate(FileName);
end;

function FileCreate(const FileName: RawByteString; ShareMode: integer; Rights : integer): THandle;
begin
  {$WARNING FIX ME! To do: FileCreate Access Modes}
  FileCreate:=FileCreate(FileName);
end;


function FileRead(Handle: THandle; out Buffer; Count: LongInt): LongInt;
begin
  FileRead:=-1;
  if (Count<=0) or (Handle=0) or (Handle=-1) then exit;

end;


function FileWrite(Handle: THandle; const Buffer; Count: LongInt): LongInt;
begin
  FileWrite:=-1;
  if (Count<=0) or (Handle=0) or (Handle=-1) then exit;

end;


function FileSeek(Handle: THandle; FOffset, Origin: LongInt) : LongInt;
begin
  FileSeek:=-1;
end;

function FileSeek(Handle: THandle; FOffset: Int64; Origin: Longint): Int64;
begin
  FileSeek:=FileSeek(Handle,LongInt(FOffset),LongInt(Origin));
end;


procedure FileClose(Handle: THandle);
begin
  if (Handle=0) or (Handle=-1) then exit;

end;


function FileTruncate(Handle: THandle; Size: Int64): Boolean;
begin
  FileTruncate:=False;
end;


function DeleteFile(const FileName: RawByteString) : Boolean;
begin
end;


function RenameFile(const OldName, NewName: RawByteString): Boolean;
begin
end;


(****** end of non portable routines ******)


function FileAge (const FileName : RawByteString): Longint;
begin
  result:=-1;
end;


function FileExists (const FileName : RawByteString) : Boolean;
begin
  result:=false;
end;


Function InternalFindFirst (Const Path : RawByteString; Attr : Longint; out Rslt : TAbstractSearchRec; var Name: RawByteString) : Longint;
begin
  result:=-1; { We emulate Linux/Unix behaviour, and return -1 on errors. }
end;


Function InternalFindNext (var Rslt : TAbstractSearchRec; var Name : RawByteString) : Longint;
begin
  result:=-1;
end;


Procedure InternalFindClose(var Handle: THandle);
begin
end;


(****** end of non portable routines ******)

Function FileGetAttr (Const FileName : RawByteString) : Longint;
begin
  FileGetAttr := -1
end;


Function FileSetAttr (Const Filename : RawByteString; Attr: longint) : Longint;
begin
  FileSetAttr := -1;
end;



{****************************************************************************
                              Disk Functions
****************************************************************************}

// New easier DiskSize()
//
function DiskSize(Drive: AnsiString): Int64;
begin
  DiskSize := -1;
end;

function DiskSize(Drive: Byte): Int64;
begin
  DiskSize := -1;
end;

// New easier DiskFree()
//
function DiskFree(Drive: AnsiString): Int64;
begin
  DiskFree := -1;
end;

function DiskFree(Drive: Byte): Int64;
begin
  DiskFree := -1;
end;

function DirectoryExists(const Directory: RawByteString): Boolean;
begin
  result:=false;
end;



{****************************************************************************
                              Locale Functions
****************************************************************************}

Procedure GetLocalTime(var SystemTime: TSystemTime);
begin
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
  Result:=Format(SUnknownErrorCode,[ErrorCode]);
end;

function GetLastOSError: Integer;
begin
    result:=-1;
end;

{****************************************************************************
                              OS utility functions
****************************************************************************}

function GetPathString: String;
begin
   result := '';
end;

Function GetEnvironmentVariable(Const EnvVar : String) : String;
begin
end;

Function GetEnvironmentVariableCount : Integer;
begin
end;

Function GetEnvironmentString(Index : Integer) : {$ifdef FPC_RTL_UNICODE}UnicodeString{$else}AnsiString{$endif};
begin
end;

function ExecuteProcess (const Path: RawByteString; const ComLine: RawByteString;Flags:TExecuteFlags=[]):
                                                                       integer;
begin
end;

function ExecuteProcess (const Path: RawByteString;
                                  const ComLine: array of RawByteString;Flags:TExecuteFlags=[]): integer;
var
  CommandLine: RawByteString;
  I: integer;

begin
  Commandline := '';
  for I := 0 to High (ComLine) do
   if Pos (' ', ComLine [I]) <> 0 then
    CommandLine := CommandLine + ' ' + '"' + ToSingleByteFileSystemEncodedFileName(ComLine [I]) + '"'
   else
    CommandLine := CommandLine + ' ' + ToSingleByteFileSystemEncodedFileName(Comline [I]);
  ExecuteProcess := ExecuteProcess (Path, CommandLine);
end;

procedure Sleep(Milliseconds: cardinal);
begin
end;


{****************************************************************************
                              Initialization code
****************************************************************************}

Initialization
  InitExceptions;
  InitInternational;    { Initialize internationalization settings }
  OnBeep:=Nil;          { No SysBeep() on Atari for now. }

Finalization
  DoneExceptions;
end.
