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

{$IFNDEF FPC_DOTTEDUNITS}
unit sysutils;
{$ENDIF FPC_DOTTEDUNITS}
interface

{$MODE objfpc}
{$MODESWITCH OUT}
{$IFDEF UNICODERTL}
{$MODESWITCH UNICODESTRINGS}
{$ELSE}
{$H+}
{$ENDIF}
{$modeswitch typehelpers}
{$modeswitch advancedrecords}

{$IFDEF FPC_DOTTEDUNITS}
uses
  WASIApi.WASIApi, WASIApi.WASIUtil;
{$ELSE FPC_DOTTEDUNITS}
uses
  wasiapi, wasiutil;
{$ENDIF FPC_DOTTEDUNITS}

{$DEFINE OS_FILESETDATEBYNAME}
{$DEFINE HAS_SLEEP}
{$DEFINE HAS_GETTICKCOUNT64}

{ used OS file system APIs use ansistring }
{$define SYSUTILS_HAS_ANSISTR_FILEUTIL_IMPL}
{ OS has an ansistring/single byte environment variable API }
{$define SYSUTILS_HAS_ANSISTR_ENVVAR_IMPL}

type
  TWasiFindData = TWasiSearchRec;

{ Include platform independent interface part }
{$i sysutilh.inc}


implementation

{$IFDEF FPC_DOTTEDUNITS}
  uses
    System.SysConst;
{$ELSE FPC_DOTTEDUNITS}
  uses
    sysconst;
{$ENDIF FPC_DOTTEDUNITS}

{$DEFINE FPC_FEXPAND_UNC} (* UNC paths are supported *)
{$DEFINE FPC_FEXPAND_DRIVES} (* Full paths begin with drive specification *)
{$DEFINE HAS_LOCALTIMEZONEOFFSET}

{$DEFINE executeprocuni} (* Only 1 byte version of ExecuteProcess is provided by the OS *)

Function UniversalToEpoch(year,month,day,hour,minute,second:Word):int64;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}WASIApi.{$ENDIF}WasiUtil.UniversalToEpoch(year,month,day,hour,minute,second);
end;

Function LocalToEpoch(year,month,day,hour,minute,second:Word):int64;
begin
  Result:={$IFDEF FPC_DOTTEDUNITS}WASIApi.{$ENDIF}WasiUtil.LocalToEpoch(year,month,day,hour,minute,second);
end;

Procedure EpochToUniversal(epoch:int64;var year,month,day,hour,minute,second:Word);
begin
  {$IFDEF FPC_DOTTEDUNITS}WASIApi.{$ENDIF}WasiUtil.EpochToUniversal(epoch,year,month,day,hour,minute,second);
end;

Procedure EpochToLocal(epoch:int64;var year,month,day,hour,minute,second:Word);
begin
  {$IFDEF FPC_DOTTEDUNITS}WASIApi.{$ENDIF}WasiUtil.EpochToLocal(epoch,year,month,day,hour,minute,second);
end;

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

Function WasiToWinAttr (const FN : RawByteString; fd: __wasi_fd_t; pr: PAnsiChar; pr_len: size_t; Const Info : __wasi_filestat_t) : Longint;
Var
  LinkInfo : __wasi_filestat_t;
  nm : RawByteString;
begin
  Result:=faArchive;
  if Info.filetype=__WASI_FILETYPE_DIRECTORY then
    Result:=Result or faDirectory;
  nm:=ExtractFileName(FN);
  If (Length(nm)>=2) and
     (nm[1]='.') and
     (nm[2]<>'.')  then
    Result:=Result or faHidden;
  If (Info.filetype=__WASI_FILETYPE_BLOCK_DEVICE) or
     (Info.filetype=__WASI_FILETYPE_CHARACTER_DEVICE) or
     (Info.filetype=__WASI_FILETYPE_SOCKET_DGRAM) or
     (Info.filetype=__WASI_FILETYPE_SOCKET_STREAM) then
    Result:=Result or faSysFile;
  if Info.filetype=__WASI_FILETYPE_SYMBOLIC_LINK then
    begin
      Result:=Result or faSymLink;
      // Windows reports if the link points to a directory.
      if (__wasi_path_filestat_get(fd,__WASI_LOOKUPFLAGS_SYMLINK_FOLLOW,pr,pr_len,@LinkInfo)=__WASI_ERRNO_SUCCESS) and
         (LinkInfo.filetype=__WASI_FILETYPE_DIRECTORY) then
        Result := Result or faDirectory;
    end;
end;


Function FileOpen (Const FileName : RawByteString; Mode : Integer) : THandle;
Var
  fs_rights_base: __wasi_rights_t = 0;
  ourfd: __wasi_fd_t;
  res: __wasi_errno_t;
  pr: RawByteString;
  fd: __wasi_fd_t;
Begin
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
  if ConvertToFdRelativePath(FileName,fd,pr)<>0 then
    begin
      result:=-1;
      exit;
    end;
  repeat
    res:=__wasi_path_open(fd,
                          0,
                          PAnsiChar(pr),
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
  ourfd: __wasi_fd_t;
  res: __wasi_errno_t;
  pr: RawByteString;
  fd: __wasi_fd_t;
Begin
  if ConvertToFdRelativePath(FileName,fd,pr)<>0 then
    begin
      result:=-1;
      exit;
    end;
  repeat
    res:=__wasi_path_open(fd,
                          0,
                          PAnsiChar(pr),
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
var
  res: __wasi_errno_t;
  pr: RawByteString;
  fd: __wasi_fd_t;
  Info: __wasi_filestat_t;
begin
  if ConvertToFdRelativePath(FileName,fd,pr)<>0 then
    begin
      result:=-1;
      exit;
    end;
  res:=__wasi_path_filestat_get(fd,0,PAnsiChar(pr),length(pr),@Info);
  if res=__WASI_ERRNO_SUCCESS then
    result:=Info.mtim div 1000000000
  else
    result:=-1;
end;


function FileGetSymLinkTarget(const FileName: RawByteString; out SymLinkRec: TRawbyteSymLinkRec): Boolean;
var
  pr: RawByteString;
  fd: __wasi_fd_t;
  Info: __wasi_filestat_t;
  symlink: RawByteString;
  res: __wasi_errno_t;
begin
  FillChar(SymLinkRec, SizeOf(SymLinkRec), 0);
  result:=false;
  if ConvertToFdRelativePath(FileName,fd,pr)<>0 then
    exit;
  if __wasi_path_filestat_get(fd,0,PAnsiChar(pr),length(pr),@Info)<>__WASI_ERRNO_SUCCESS then
    exit;
  if Info.filetype<>__WASI_FILETYPE_SYMBOLIC_LINK then
    exit;
  if fpc_wasi_path_readlink_ansistring(fd,PAnsiChar(pr),Length(pr),symlink)<>__WASI_ERRNO_SUCCESS then
    exit;
  SymLinkRec.TargetName:=symlink;

  res:=__wasi_path_filestat_get(fd,__WASI_LOOKUPFLAGS_SYMLINK_FOLLOW,PAnsiChar(pr),length(pr),@Info);
  if res<>__WASI_ERRNO_SUCCESS then
    raise EDirectoryNotFoundException.Create('Error ' + IntToStr(res){todo: SysErrorMessage SysErrorMessage(GetLastOSError)});
  SymLinkRec.Attr := WasiToWinAttr(FileName,fd,PAnsiChar(pr),length(pr),Info);
  SymLinkRec.Size := Info.size;
  result:=true;
end;


function FileExists (const FileName: RawByteString; FollowLink : Boolean): boolean;
var
  pr: RawByteString;
  fd: __wasi_fd_t;
  Info: __wasi_filestat_t;
  flags: __wasi_lookupflags_t;
begin
  if FileName='' then
    exit(false);
  if ConvertToFdRelativePath(FileName,fd,pr)<>0 then
    exit(false);
  if FollowLink then
    flags:=__WASI_LOOKUPFLAGS_SYMLINK_FOLLOW
  else
    flags:=0;
  if __wasi_path_filestat_get(fd,flags,PAnsiChar(pr),length(pr),@Info)=__WASI_ERRNO_SUCCESS then
    result:=Info.filetype<>__WASI_FILETYPE_DIRECTORY
  else
    result:=false;
end;


Function DirectoryExists (Const Directory : RawByteString; FollowLink : Boolean) : Boolean;
var
  pr: RawByteString;
  fd: __wasi_fd_t;
  Info: __wasi_filestat_t;
  flags: __wasi_lookupflags_t;
begin
  if Directory='' then
    exit(false);
  if ConvertToFdRelativePath(Directory,fd,pr)<>0 then
    exit(false);
  if FollowLink then
    flags:=__WASI_LOOKUPFLAGS_SYMLINK_FOLLOW
  else
    flags:=0;
  if __wasi_path_filestat_get(fd,flags,PAnsiChar(pr),length(pr),@Info)=__WASI_ERRNO_SUCCESS then
    result:=Info.filetype=__WASI_FILETYPE_DIRECTORY
  else
    result:=false;
end;


Function InternalFindFirst (Const Path : RawByteString; Attr : Longint; out Rslt : TAbstractSearchRec; var Name: RawByteString) : Longint;
var
  derror: longint;
begin
  Result:=-1;
  { this is safe even though Rslt actually contains a refcounted field, because
    it is declared as "out" and hence has already been initialised }
  fillchar(Rslt,sizeof(Rslt),0);
  if Path='' then
    exit;

  derror:=WasiFindFirst(Path, Attr, Rslt.FindData);
  if derror=0 then
    result:=0
  else
    result:=-1;

  Name:=Rslt.FindData.Name;
  Rslt.Attr:=Rslt.FindData.Attr;
  Rslt.Size:=Rslt.FindData.Size;
  Rslt.Time:=Rslt.FindData.Time div 1000000000;
end;


Function InternalFindNext (var Rslt : TAbstractSearchRec; var Name : RawByteString) : Longint;
var
  derror: longint;
begin
  derror:=WasiFindNext(Rslt.FindData);
  if derror=0 then
    result:=0
  else
    result:=-1;

  Name:=Rslt.FindData.Name;
  Rslt.Attr:=Rslt.FindData.Attr;
  Rslt.Size:=Rslt.FindData.Size;
  Rslt.Time:=Rslt.FindData.Time div 1000000000;
end;


Procedure InternalFindClose(var Handle: THandle; var FindData: TFindData);
begin
  WasiFindClose(FindData);
end;


Function FileGetDate (Handle : THandle) : Int64;
var
  res: __wasi_errno_t;
  Info: __wasi_filestat_t;
begin
  res:=__wasi_fd_filestat_get(Handle,@Info);
  if res=__WASI_ERRNO_SUCCESS then
    result:=Info.mtim div 1000000000
  else
    result:=-1;
end;


Function FileSetDate (Handle : THandle; Age : Int64) : Longint;
begin
  if __wasi_fd_filestat_set_times(Handle,Age*1000000000,Age*1000000000,
     __WASI_FSTFLAGS_MTIM or __WASI_FSTFLAGS_ATIM)=__WASI_ERRNO_SUCCESS then
    result:=0
  else
    result:=-1;
end;


Function FileSetDate (Const FileName : RawByteString; Age : Int64) : Longint;
var
  pr: RawByteString;
  fd: __wasi_fd_t;
begin
  if ConvertToFdRelativePath(FileName,fd,pr)<>0 then
    begin
      result:=-1;
      exit;
    end;
  if __wasi_path_filestat_set_times(fd,0,PAnsiChar(pr),length(pr),Age*1000000000,Age*1000000000,
     __WASI_FSTFLAGS_MTIM or __WASI_FSTFLAGS_ATIM)=__WASI_ERRNO_SUCCESS then
    result:=0
  else
    result:=-1;
end;


Function FileGetAttr (Const FileName : RawByteString) : Longint;
var
  pr: RawByteString;
  fd: __wasi_fd_t;
  Info: __wasi_filestat_t;
begin
  if ConvertToFdRelativePath(FileName,fd,pr)<>0 then
    begin
      result:=-1;
      exit;
    end;
  if __wasi_path_filestat_get(fd,0,PAnsiChar(pr),length(pr),@Info)=__WASI_ERRNO_SUCCESS then
    result:=WasiToWinAttr(FileName,fd,PAnsiChar(pr),length(pr),Info)
  else
    result:=-1;
end;


Function FileSetAttr (Const Filename : RawByteString; Attr: longint) : Longint;
begin
  Result:=-1;
end;


Function DeleteFile (Const FileName : RawByteString) : Boolean;
var
  fd: __wasi_fd_t;
  pr: RawByteString;
  res: __wasi_errno_t;
begin
  if ConvertToFdRelativePath(FileName,fd,pr)<>0 then
    begin
      result:=false;
      exit;
    end;
  result:=__wasi_path_unlink_file(fd,PAnsiChar(pr),Length(pr))=__WASI_ERRNO_SUCCESS;
end;


Function RenameFile (Const OldName, NewName : RawByteString) : Boolean;
var
  fd1,fd2: __wasi_fd_t;
  pr1,pr2: RawByteString;
  res: __wasi_errno_t;
begin
  result:=false;
  if ConvertToFdRelativePath(OldName,fd1,pr1)<>0 then
    exit;
  if ConvertToFdRelativePath(NewName,fd2,pr2)<>0 then
    exit;
  result:=__wasi_path_rename(fd1,PAnsiChar(pr1),Length(pr1),fd2,PAnsiChar(pr2),Length(pr2))=__WASI_ERRNO_SUCCESS;
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
var
  NanoSecsPast: __wasi_timestamp_t;
begin
  if __wasi_clock_time_get(__WASI_CLOCKID_REALTIME,1000000,@NanoSecsPast)=__WASI_ERRNO_SUCCESS then
    begin
      EpochToLocal(NanoSecsPast div 1000000000,
        SystemTime.Year,SystemTime.Month,SystemTime.Day,
        SystemTime.Hour,SystemTime.Minute,SystemTime.Second);
      SystemTime.MilliSecond := (NanoSecsPast div 1000000) mod 1000;
      SystemTime.DayOfWeek := DayOfWeek(EncodeDate(SystemTime.Year,SystemTime.Month,SystemTime.Day))-1;
    end
  else
    FillChar(SystemTime,SizeOf(SystemTime),0);
end;


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
  Move (CPISO88591LCT,LowerCaseTable[192],SizeOf(CPISO88591UCT));
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

Function GetEnvironmentVariable(Const EnvVar : AnsiString) : AnsiString;
var
  hp : PPAnsiChar;
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
  p: PPAnsiChar;
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

Function GetEnvironmentString(Index : Integer) : RTLString;
Var
  i : longint;
  p : PPAnsiChar;
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
