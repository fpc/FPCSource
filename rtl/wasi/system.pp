{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2020,2021 by the Free Pascal development team.

    System unit for The WebAssembly System Interface (WASI).

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit system;

interface


{$DEFINE SYSTEM_HAS_FEATURE_MONITOR}
{$define FPC_IS_SYSTEM}
{$ifdef FPC_WASM_THREADS}
  {$define DISABLE_NO_THREAD_MANAGER}
{$else FPC_WASM_THREADS}
  {$define USE_NOTHREADMANAGER}
{$endif FPC_WASM_THREADS}

{$I systemh.inc}

const
  LineEnding = #10;
  LFNSupport = true;
  DirectorySeparator = '/';
  DriveSeparator = '';
  ExtensionSeparator = '.';
  PathSeparator = ':';
  AllowDirectorySeparators : set of AnsiChar = ['\','/'];
  AllowDriveSeparators : set of AnsiChar = [];
{  FileNameCaseSensitive and FileNameCasePreserving are defined below! }
  maxExitCode = 65535;
  MaxPathLen = 4096;
  AllFilesMask = '*';

const
  UnusedHandle    = -1;
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = 2;

  FileNameCaseSensitive : boolean = true;
  FileNameCasePreserving: boolean = true;
  CtrlZMarksEOF: boolean = false; (* #26 not considered as end of file *)

  sLineBreak = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsLF;

var
  argc: longint;
  argv: PPAnsiChar;
  envp: PPAnsiChar;
  ___fpc_wasm_suspender: WasmExternRef; section 'WebAssembly.Global';

function __fpc_get_wasm_suspender: WasmExternRef;
procedure __fpc_set_wasm_suspender(v: WasmExternRef);

property __fpc_wasm_suspender: WasmExternRef read __fpc_get_wasm_suspender write __fpc_set_wasm_suspender;

implementation

{$I wasitypes.inc}
{$I wasiprocs.inc}

function WasiAlloc (aLength : Longint) : Pointer; [public, alias: 'wasiAlloc'];

begin
  Result:=GetMem(aLength);
end;

procedure WasiFree (aMem : pointer); [public, alias: 'wasiFree'];

begin
  FreeMem(aMem);
end;

exports 
  WasiAlloc,WasiFree;

function __fpc_get_wasm_suspender: WasmExternRef;
begin
  result:=___fpc_wasm_suspender;
end;

procedure __fpc_set_wasm_suspender(v: WasmExternRef);
begin
  ___fpc_wasm_suspender:=v;
end;

function ConvertToFdRelativePath(path: RawByteString; out fd: LongInt; out relfd_path: RawByteString): Word; forward;

function fpc_wasi_path_readlink_ansistring(
                 fd: __wasi_fd_t;
                 const path: PAnsiChar;
                 path_len: size_t;
                 out link: rawbytestring): __wasi_errno_t;[Public, Alias : 'FPC_WASI_PATH_READLINK_ANSISTRING'];
const
  InitialBufLen=64;
  MaximumBufLen=16384;
var
  CurrentBufLen: __wasi_size_t;
  BufUsed: __wasi_size_t;
begin
  CurrentBufLen:=InitialBufLen div 2;
  repeat
    CurrentBufLen:=CurrentBufLen*2;
    SetLength(link,CurrentBufLen);
    result:=__wasi_path_readlink(fd,path,path_len,@(link[1]),CurrentBufLen,@BufUsed);
  until (result<>__WASI_ERRNO_SUCCESS) or ((result=__WASI_ERRNO_SUCCESS) and (BufUsed<CurrentBufLen)) or (CurrentBufLen>MaximumBufLen);
  if result=__WASI_ERRNO_SUCCESS then
  begin
    SetLength(link,BufUsed);
    setcodepage(link,DefaultRTLFileSystemCodePage,true);
  end
  else
    link:='';
end;

function HasDriveLetter(const path: rawbytestring): Boolean;
begin
  HasDriveLetter:=(Length(path)>=2) and (UpCase(path[1]) in ['A'..'Z']) and (path[2] = ':');
end;

type
  PPreopenedDir = ^TPreopenedDir;
  TPreopenedDir = record
    dir_name: RawByteString;
    drive_str: RawByteString;
    fd: longint;
  end;
  PCurrentDir = ^TCurrentDir;
  TCurrentDir = record
    dir_name: RawByteString;
    drive_str: RawByteString;
  end;

var
  preopened_dirs_count: longint;
  preopened_dirs: PPreopenedDir;
  drives_count: longint;
  current_dirs: PCurrentDir;
  current_drive: longint;

{$I system.inc}

var
  argv_size,
  argv_buf_size: __wasi_size_t;
  argv_buf: Pointer;
  environc,environ_buf_size,envp_size: __wasi_size_t;
  environ_buf: Pointer;

function GetProcessID: SizeUInt;
begin
end;

Procedure Randomize;
Begin
  __wasi_random_get(@RandSeed,SizeOf(RandSeed));
End;

procedure System_exit;
begin
  if ExitCode>=126 then
    begin
      writeln(stderr,'##WASI-EXITCODE: ',ExitCode,' -> 125##');
      ExitCode:=125;
    end;
  __wasi_proc_exit(ExitCode);
End;

function Do_ConvertToFdRelativePath(path: RawByteString; out fd: LongInt; out relfd_path: RawByteString): Word;
var
  drive_nr,I,pdir_drive,longest_match,chridx: longint;
  IsAbsolutePath: Boolean;
  pdir: RawByteString;
begin
  fd:=0;
  relfd_path:='';

  if HasDriveLetter(path) then
  begin
    drive_nr:=Ord(UpCase(path[1]))-(Ord('A')-1);
    delete(path,1,2);
  end
  else
    drive_nr:=current_drive;
  { path is relative to a current directory? }
  if (path='') or not (path[1] in AllowDirectorySeparators) then
  begin
    { if so, convert to absolute }
    if (drive_nr>=drives_count) or (current_dirs[drive_nr].dir_name='') then
    begin
      Do_ConvertToFdRelativePath:=15;
      exit;
    end;
    if current_dirs[drive_nr].dir_name[Length(current_dirs[drive_nr].dir_name)] in AllowDirectorySeparators then
      path:=current_dirs[drive_nr].dir_name+path
    else
      path:=current_dirs[drive_nr].dir_name+DirectorySeparator+path;
  end;
  { path is now absolute. Try to find it in the preopened dirs array }
  longest_match:=0;
  for I:=0 to preopened_dirs_count-1 do
  begin
    pdir:=preopened_dirs[I].dir_name;
    if preopened_dirs[I].drive_str<>'' then
      pdir_drive:=Ord(UpCase(preopened_dirs[I].drive_str[1]))-(Ord('A')-1)
    else
      pdir_drive:=0;
    if pdir_drive<>drive_nr then
      continue;
    if Length(pdir)>Length(path) then
      continue;
    if Copy(path,1,Length(pdir))<>pdir then
      continue;
    chridx:=Length(pdir)+1;
    if ((Length(pdir)<>1) or ((Length(pdir)=1) and not (pdir[1] in AllowDirectorySeparators))) and
       ((chridx>Length(path)) or not (path[chridx] in AllowDirectorySeparators)) then
      continue;
    if Length(pdir)>longest_match then
    begin
      longest_match:=Length(pdir);
      while (chridx<=Length(path)) and (path[chridx] in AllowDirectorySeparators) do
        Inc(chridx);
      fd:=preopened_dirs[I].fd;
      relfd_path:=Copy(path,chridx,Length(path)-chridx+1);
    end;
  end;
  if longest_match>0 then
    Do_ConvertToFdRelativePath:=0
  else
    Do_ConvertToFdRelativePath:=3;
end;

function ConvertToFdRelativePath(path: RawByteString; out fd: LongInt; out relfd_path: RawByteString): Word;[Public, Alias : 'FPC_WASI_CONVERTTOFDRELATIVEPATH'];
begin
  ConvertToFdRelativePath:=Do_ConvertToFdRelativePath(ToSingleByteFileSystemEncodedFileName(path),fd,relfd_path);
  setcodepage(relfd_path,DefaultRTLFileSystemCodePage,true);
end;

procedure Setup_PreopenedDirs;
var
  fd: __wasi_fd_t;
  prestat: __wasi_prestat_t;
  res: __wasi_errno_t;
  prestat_dir_name: RawByteString;
  drive_nr: longint;
begin
  preopened_dirs_count:=0;
  preopened_dirs:=nil;
  drives_count:=0;
  current_dirs:=nil;
  current_drive:=0;
  fd:=3;
  repeat
    res:=__wasi_fd_prestat_get(fd, @prestat);
    if res=__WASI_ERRNO_SUCCESS then
    begin
      if (prestat.tag=__WASI_PREOPENTYPE_DIR) and (prestat.u.dir.pr_name_len>0) then
      begin
        SetLength(prestat_dir_name,prestat.u.dir.pr_name_len);
        if __wasi_fd_prestat_dir_name(fd,PByte(prestat_dir_name),prestat.u.dir.pr_name_len)=__WASI_ERRNO_SUCCESS then
        begin
          setcodepage(prestat_dir_name,DefaultRTLFileSystemCodePage,true);
          Inc(preopened_dirs_count);
          if preopened_dirs=nil then
            preopened_dirs:=AllocMem(preopened_dirs_count*SizeOf(TPreopenedDir))
          else
            ReAllocMem(preopened_dirs, preopened_dirs_count*SizeOf(TPreopenedDir));
          preopened_dirs[preopened_dirs_count-1].fd:=fd;
          if HasDriveLetter(prestat_dir_name) then
          begin
            drive_nr:=Ord(UpCase(prestat_dir_name[1]))-(Ord('A')-1);
            preopened_dirs[preopened_dirs_count-1].drive_str:=Copy(prestat_dir_name,1,2);
            preopened_dirs[preopened_dirs_count-1].dir_name:=Copy(prestat_dir_name,2,Length(prestat_dir_name)-2);
          end
          else
          begin
            drive_nr:=0;
            preopened_dirs[preopened_dirs_count-1].drive_str:='';
            preopened_dirs[preopened_dirs_count-1].dir_name:=prestat_dir_name;
          end;
          if (drive_nr+1)>drives_count then
          begin
            drives_count:=drive_nr+1;
            if current_dirs=nil then
              current_dirs:=AllocMem(drives_count*SizeOf(TCurrentDir))
            else
              ReAllocMem(current_dirs,drives_count*SizeOf(TCurrentDir));
          end;
          if current_dirs[drive_nr].dir_name='' then
            current_dirs[drive_nr].dir_name:=prestat_dir_name;
        end;
      end;
    end;
    Inc(fd);
  until res<>__WASI_ERRNO_SUCCESS;
  while (current_drive<(drives_count-1)) and (current_dirs[current_drive].dir_name='') do
    Inc(current_drive);
  for drive_nr:=0 to drives_count-1 do
  begin
    if drive_nr>0 then
      current_dirs[drive_nr].drive_str:=Chr(Ord('A')+drive_nr-1)+':';
    if current_dirs[drive_nr].dir_name='' then
      current_dirs[drive_nr].dir_name:=DirectorySeparator;
  end;
end;

procedure Setup_Environment;
begin
  if envp<>nil then
    exit;
  if __wasi_environ_sizes_get(@environc, @environ_buf_size)<>__WASI_ERRNO_SUCCESS then
  begin
    envp:=nil;
    exit;
  end;
  envp_size:=(environc+1)*SizeOf(PAnsiChar);
  GetMem(envp, envp_size);
  GetMem(environ_buf, environ_buf_size);
  envp[environc]:=nil;
  if __wasi_environ_get(Pointer(envp), environ_buf)<>__WASI_ERRNO_SUCCESS then
  begin
    FreeMem(envp, envp_size);
    FreeMem(environ_buf, environ_buf_size);
    envp:=nil;
  end;
end;

procedure setup_arguments;
begin
  if argv<>nil then
    exit;
  if __wasi_args_sizes_get(@argc, @argv_buf_size)<>__WASI_ERRNO_SUCCESS then
  begin
    argc:=0;
    argv:=nil;
    exit;
  end;
  argv_size:=(argc+1)*SizeOf(PAnsiChar);
  GetMem(argv, argv_size);
  GetMem(argv_buf, argv_buf_size);
  if __wasi_args_get(Pointer(argv), argv_buf)<>__WASI_ERRNO_SUCCESS then
  begin
    FreeMem(argv, argv_size);
    FreeMem(argv_buf, argv_buf_size);
    argc:=0;
    argv:=nil;
  end;
end;

Function ParamCount: Longint;
Begin
  if argv=nil then
    setup_arguments;
  paramcount := argc - 1;
End;

function paramstr(l: longint) : shortstring;
begin
  if argv=nil then
    setup_arguments;
  if (l>=0) and (l+1<=argc) then
    paramstr:=strpas(argv[l])
  else
    paramstr:='';
end;

procedure SysInitStdIO;
begin
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(ErrOutput,fmOutput,StdErrorHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
end;

function CheckInitialStkLen(stklen : SizeUInt) : SizeUInt;
begin
end;

begin
  { To be set if this is a GUI or console application }
  IsConsole := TRUE;
{$ifdef FPC_HAS_FEATURE_DYNLIBS}
  { If dynlibs feature is disabled,
    IsLibrary is a constant, which can thus not be set to a value }
  { To be set if this is a library and not a program  }
  IsLibrary := FALSE;
{$endif def FPC_HAS_FEATURE_DYNLIBS}
  { Setup heap }
  InitHeap;
  SysInitExceptions;
  initunicodestringmanager;
  { Setup stdin, stdout and stderr }
  SysInitStdIO;
  { Reset IO Error }
  InOutRes:=0;
{$ifdef FPC_HAS_FEATURE_THREADING}
  InitSystemThreads;
{$endif}
  Setup_Environment;
  Setup_PreopenedDirs;
end.
