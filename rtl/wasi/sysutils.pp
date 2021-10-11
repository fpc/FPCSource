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
Var
  SystemFileName: RawByteString;
  fs_rights_base: __wasi_rights_t = 0;
  ourfd: __wasi_fd_t;
  res: __wasi_errno_t;
  pr: RawByteString;
  fd: __wasi_fd_t;
Begin
  SystemFileName:=ToSingleByteFileSystemEncodedFileName(FileName);
  case (Mode and (fmOpenRead or fmOpenWrite or fmOpenReadWrite)) of
   fmOpenRead:
     fs_rights_base :=__WASI_RIGHTS_FD_READ or
                      __WASI_RIGHTS_FD_FILESTAT_GET or
                      __WASI_RIGHTS_FD_SEEK or
                      __WASI_RIGHTS_FD_TELL or
                      __WASI_RIGHTS_FD_FDSTAT_SET_FLAGS or
                      __WASI_RIGHTS_FD_ADVISE or
                      __WASI_RIGHTS_POLL_FD_READWRITE;
   fmOpenWrite:
     fs_rights_base :=__WASI_RIGHTS_FD_WRITE or
                      __WASI_RIGHTS_FD_FILESTAT_GET or
                      __WASI_RIGHTS_FD_SEEK or
                      __WASI_RIGHTS_FD_TELL or
                      __WASI_RIGHTS_FD_FDSTAT_SET_FLAGS or
                      __WASI_RIGHTS_FD_ADVISE or
                      __WASI_RIGHTS_POLL_FD_READWRITE or
                      __WASI_RIGHTS_FD_FILESTAT_SET_SIZE or
                      __WASI_RIGHTS_FD_FILESTAT_SET_TIMES or
                      __WASI_RIGHTS_FD_ALLOCATE or
                      __WASI_RIGHTS_FD_DATASYNC or
                      __WASI_RIGHTS_FD_SYNC;
   fmOpenReadWrite:
     fs_rights_base :=__WASI_RIGHTS_FD_READ or
                      __WASI_RIGHTS_FD_WRITE or
                      __WASI_RIGHTS_FD_FILESTAT_GET or
                      __WASI_RIGHTS_FD_SEEK or
                      __WASI_RIGHTS_FD_TELL or
                      __WASI_RIGHTS_FD_FDSTAT_SET_FLAGS or
                      __WASI_RIGHTS_FD_ADVISE or
                      __WASI_RIGHTS_POLL_FD_READWRITE or
                      __WASI_RIGHTS_FD_FILESTAT_SET_SIZE or
                      __WASI_RIGHTS_FD_FILESTAT_SET_TIMES or
                      __WASI_RIGHTS_FD_ALLOCATE or
                      __WASI_RIGHTS_FD_DATASYNC or
                      __WASI_RIGHTS_FD_SYNC;
  end;
  if not ConvertToFdRelativePath(SystemFileName,fd,pr) then
    begin
      result:=-1;
      exit;
    end;
  repeat
    res:=__wasi_path_open(fd,
                          0,
                          PChar(pr),
                          length(pr),
                          0,
                          fs_rights_base,
                          fs_rights_base,
                          0,
                          @ourfd);
  until (res=__WASI_ERRNO_SUCCESS) or (res<>__WASI_ERRNO_INTR);
  If res=__WASI_ERRNO_SUCCESS Then
    Result:=ourfd
  else
    Result:=-1;
end;


Function FileCreate (Const FileName : RawByteString) : THandle;
Const
  fs_rights_base: __wasi_rights_t =
    __WASI_RIGHTS_FD_READ or
    __WASI_RIGHTS_FD_WRITE or
    __WASI_RIGHTS_FD_FILESTAT_GET or
    __WASI_RIGHTS_FD_SEEK or
    __WASI_RIGHTS_FD_TELL or
    __WASI_RIGHTS_FD_FDSTAT_SET_FLAGS or
    __WASI_RIGHTS_FD_ADVISE or
    __WASI_RIGHTS_POLL_FD_READWRITE or
    __WASI_RIGHTS_FD_FILESTAT_SET_SIZE or
    __WASI_RIGHTS_FD_FILESTAT_SET_TIMES or
    __WASI_RIGHTS_FD_ALLOCATE or
    __WASI_RIGHTS_FD_DATASYNC or
    __WASI_RIGHTS_FD_SYNC;
Var
  SystemFileName: RawByteString;
  ourfd: __wasi_fd_t;
  res: __wasi_errno_t;
  pr: RawByteString;
  fd: __wasi_fd_t;
Begin
  SystemFileName:=ToSingleByteFileSystemEncodedFileName(FileName);
  if not ConvertToFdRelativePath(SystemFileName,fd,pr) then
    begin
      result:=-1;
      exit;
    end;
  repeat
    res:=__wasi_path_open(fd,
                          0,
                          PChar(pr),
                          length(pr),
                          __WASI_OFLAGS_CREAT or __WASI_OFLAGS_TRUNC,
                          fs_rights_base,
                          fs_rights_base,
                          0,
                          @ourfd);
  until (res=__WASI_ERRNO_SUCCESS) or (res<>__WASI_ERRNO_INTR);
  If res=__WASI_ERRNO_SUCCESS Then
    Result:=ourfd
  else
    Result:=-1;
end;


Function FileCreate (Const FileName : RawByteString; ShareMode:integer; Rights : integer) : THandle;
begin
  FileCreate:=FileCreate(FileName);
end;


Function FileCreate (Const FileName : RawByteString; Rights:integer) : THandle;
begin
  FileCreate:=FileCreate(FileName);
end;


Function FileRead (Handle : THandle; Out Buffer; Count : longint) : Longint;
var
  our_iov: __wasi_iovec_t;
  our_nread: __wasi_size_t;
  res: __wasi_errno_t;
begin
  repeat
    our_iov.buf:=@Buffer;
    our_iov.buf_len:=Count;
    res:=__wasi_fd_read(Handle,@our_iov,1,@our_nread);
  until (res=__WASI_ERRNO_SUCCESS) or ((res<>__WASI_ERRNO_INTR) and (res<>__WASI_ERRNO_AGAIN));
  if res=__WASI_ERRNO_SUCCESS then
    Result:=our_nread
  else
    Result:=-1;
end;


Function FileWrite (Handle : THandle; const Buffer; Count : Longint) : Longint;
var
  our_iov: __wasi_ciovec_t;
  our_nwritten: longint;
  res: __wasi_errno_t;
begin
  repeat
    our_iov.buf:=@Buffer;
    our_iov.buf_len:=Count;
    res:=__wasi_fd_write(Handle,@our_iov,1,@our_nwritten);
  until (res=__WASI_ERRNO_SUCCESS) or ((res<>__WASI_ERRNO_INTR) and (res<>__WASI_ERRNO_AGAIN));
  if res=__WASI_ERRNO_SUCCESS then
    Result:=our_nwritten
  else
    Result:=-1;
end;


Function FileSeek (Handle : THandle; FOffset, Origin : Longint) : Longint;
begin
  result:=longint(FileSeek(Handle,int64(FOffset),Origin));
end;


Function FileSeek (Handle : THandle; FOffset: Int64; Origin: Longint) : Int64;
var
  res: __wasi_errno_t;
  newoffset: __wasi_filesize_t;
  whence: __wasi_whence_t;
begin
  case Origin of
    fsFromBeginning:
      whence:=__WASI_WHENCE_SET;
    fsFromCurrent:
      whence:=__WASI_WHENCE_CUR;
    fsFromEnd:
      whence:=__WASI_WHENCE_END;
    else
      begin
        Result:=-1;
        exit;
      end;
  end;
  res:=__wasi_fd_seek(Handle,FOffset,whence,@newoffset);
  if res=__WASI_ERRNO_SUCCESS then
    Result:=newoffset
  else
    Result:=-1;
end;


Procedure FileClose (Handle : THandle);
var
  res: __wasi_errno_t;
begin
  repeat
    res:=__wasi_fd_close(Handle);
  until (res=__WASI_ERRNO_SUCCESS) or (res<>__WASI_ERRNO_INTR);
end;


Function FileTruncate (Handle: THandle; Size: Int64) : boolean;
var
  res: __wasi_errno_t;
begin
  Result:=__wasi_fd_filestat_set_size(handle,Size)=__WASI_ERRNO_SUCCESS;
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
var
  fd1,fd2: __wasi_fd_t;
  pr1,pr2: RawByteString;
  res: __wasi_errno_t;
begin
  result:=false;
  if not ConvertToFdRelativePath(OldName,fd1,pr1) then
    exit;
  if not ConvertToFdRelativePath(NewName,fd2,pr2) then
    exit;
  result:=__wasi_path_rename(fd1,PChar(pr1),Length(pr1),fd2,PChar(pr2),Length(pr2))=__WASI_ERRNO_SUCCESS;
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
