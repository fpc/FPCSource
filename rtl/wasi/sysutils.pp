{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2021 by the Free Pascal development team.

    Sysutils unit for The WebAssembly System Interface (WASI).

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$inline on}

unit sysutils;
interface

{$MODE objfpc}
{$MODESWITCH out}
{ force ansistrings }
{$H+}
{$modeswitch typehelpers}
{$modeswitch advancedrecords}

uses
  wasiapi;

{$DEFINE HAS_SLEEP}
{$DEFINE HAS_GETTICKCOUNT64}

{ used OS file system APIs use ansistring }
{$define SYSUTILS_HAS_ANSISTR_FILEUTIL_IMPL}
{ OS has an ansistring/single byte environment variable API }
{$define SYSUTILS_HAS_ANSISTR_ENVVAR_IMPL}

{ Include platform independent interface part }
{$i sysutilh.inc}


implementation

  uses
    sysconst;

{$DEFINE FPC_FEXPAND_UNC} (* UNC paths are supported *)
{$DEFINE FPC_FEXPAND_DRIVES} (* Full paths begin with drive specification *)
{$DEFINE HAS_LOCALTIMEZONEOFFSET}

{$DEFINE executeprocuni} (* Only 1 byte version of ExecuteProcess is provided by the OS *)

{ Include platform independent implementation part }
{$i sysutils.inc}


function GetTickCount64: QWord;
var
  NanoSecsPast: __wasi_timestamp_t;
begin
  if __wasi_clock_time_get(__WASI_CLOCKID_MONOTONIC,1000000,@NanoSecsPast)=__WASI_ERRNO_SUCCESS then
    Result:=NanoSecsPast div 1000000
  else
    Result:=0;
end;


{****************************************************************************
                              File Functions
****************************************************************************}


Function FileOpen (Const FileName : RawByteString; Mode : Integer) : THandle;
Begin
end;


Function FileCreate (Const FileName : RawByteString) : THandle;
begin
end;


Function FileCreate (Const FileName : RawByteString; ShareMode:integer; Rights : integer) : THandle;
begin
end;


Function FileCreate (Const FileName : RawByteString; Rights:integer) : THandle;
begin
end;


Function FileRead (Handle : THandle; Out Buffer; Count : longint) : Longint;
begin
end;


Function FileWrite (Handle : THandle; const Buffer; Count : Longint) : Longint;
begin
end;


Function FileSeek (Handle : THandle; FOffset, Origin : Longint) : Longint;
begin
end;


Function FileSeek (Handle : THandle; FOffset: Int64; Origin: {Integer}Longint) : Int64;
begin
end;


Procedure FileClose (Handle : THandle);
begin
end;


Function FileTruncate (Handle: THandle; Size: Int64) : boolean;
begin
end;


Function FileAge (Const FileName : RawByteString): Int64;
begin
end;


function FileGetSymLinkTarget(const FileName: RawByteString; out SymLinkRec: TRawbyteSymLinkRec): Boolean;
begin
  Result := False;
end;


function FileExists (const FileName: RawByteString; FollowLink : Boolean): boolean;
begin
end;


Function DirectoryExists (Const Directory : RawByteString; FollowLink : Boolean) : Boolean;
begin
end;


Function InternalFindFirst (Const Path : RawByteString; Attr : Longint; out Rslt : TAbstractSearchRec; var Name: RawByteString) : Longint;
begin
  { not yet implemented }
  Result := -1;
end;


Function InternalFindNext (var Rslt : TAbstractSearchRec; var Name : RawByteString) : Longint;
begin
  { not yet implemented }
  Result := -1;
end;


Procedure InternalFindClose(var Handle: THandle);
begin
end;


Function FileGetDate (Handle : THandle) : Int64;
begin
end;


Function FileSetDate (Handle : THandle; Age : Int64) : Longint;
begin
end;


Function FileGetAttr (Const FileName : RawByteString) : Longint;
begin
end;


Function FileSetAttr (Const Filename : RawByteString; Attr: longint) : Longint;
begin
end;


Function DeleteFile (Const FileName : RawByteString) : Boolean;
begin
end;


Function RenameFile (Const OldName, NewName : RawByteString) : Boolean;
begin
end;


{****************************************************************************
                              Disk Functions
****************************************************************************}


function diskfree(drive : byte) : int64;
begin
end;


function disksize(drive : byte) : int64;
begin
end;


{****************************************************************************
                              Time Functions
****************************************************************************}

{$I tzenv.inc}

Procedure GetLocalTime(var SystemTime: TSystemTime);
begin
end ;


{****************************************************************************
                              Misc Functions
****************************************************************************}

procedure sysBeep;
begin
end;


{****************************************************************************
                              Locale Functions
****************************************************************************}


procedure InitAnsi;
begin
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

{****************************************************************************
                              Os utils
****************************************************************************}

Function GetEnvironmentVariable(Const EnvVar : String) : String;
var
  hp : ppchar;
  hs : string;
  eqpos : longint;
begin
  result:='';
  hp:=envp;
  if hp<>nil then
    while assigned(hp^) do
      begin
        hs:=strpas(hp^);
        eqpos:=pos('=',hs);
        if copy(hs,1,eqpos-1)=envvar then
          begin
            result:=copy(hs,eqpos+1,length(hs)-eqpos);
            break;
          end;
        inc(hp);
      end;
end;

Function GetEnvironmentVariableCount : Integer;
var
  p: ppchar;
begin
  result:=0;
  p:=envp;      {defined in system}
  if p<>nil then
    while p^<>nil do
      begin
        inc(result);
        inc(p);
      end;
end;

Function GetEnvironmentString(Index : Integer) : {$ifdef FPC_RTL_UNICODE}UnicodeString{$else}AnsiString{$endif};
Var
  i : longint;
  p : ppchar;
begin
  if (Index <= 0) or (envp=nil) then
    result:=''
  else
    begin
      p:=envp;      {defined in system}
      i:=1;
      while (i<Index) and (p^<>nil) do
        begin
          inc(i);
          inc(p);
        end;
      if p^=nil then
        result:=''
      else
        result:=strpas(p^)
    end;
end;


function ExecuteProcess(Const Path: RawByteString; Const ComLine: RawByteString;Flags:TExecuteFlags=[]):integer;
begin
end;


function ExecuteProcess (const Path: RawByteString;
                                  const ComLine: array of RawByteString;Flags:TExecuteFlags=[]): integer;
begin
end;


{*************************************************************************
                                   Sleep
*************************************************************************}

procedure Sleep (MilliSeconds: Cardinal);
var
  subscription: __wasi_subscription_t;
  event: __wasi_event_t;
  nevents: __wasi_size_t;
begin
  FillChar(subscription,SizeOf(subscription),0);
  subscription.u.tag:=__WASI_EVENTTYPE_CLOCK;
  subscription.u.u.clock.id:=__WASI_CLOCKID_MONOTONIC;
  subscription.u.u.clock.timeout:=MilliSeconds*1000000;
  subscription.u.u.clock.precision:=1000000;
  subscription.u.u.clock.flags:=0;  { timeout value is relative }
  __wasi_poll_oneoff(@subscription,@event,1,@nevents);
end;

{****************************************************************************
                              Initialization code
****************************************************************************}

Initialization
  InitExceptions;       { Initialize exceptions. OS independent }
  InitInternational;    { Initialize internationalization settings }
  OnBeep:=@SysBeep;
  InitTZ;
Finalization
  FreeTerminateProcs;
  DoneExceptions;
end.
