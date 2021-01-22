unit system;

interface

{$define FPC_IS_SYSTEM}

{$I systemh.inc}

const
  LineEnding = #10;
  LFNSupport = true;
  DirectorySeparator = '/';
  DriveSeparator = '';
  ExtensionSeparator = '.';
  PathSeparator = ':';
  AllowDirectorySeparators : set of char = ['\','/'];
  AllowDriveSeparators : set of char = [];
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

procedure DebugWrite(const P: PChar);
procedure DebugWriteLn(const P: PChar);
procedure DebugWriteChar(Ch: Char);
procedure DebugWriteHexDigit(d: Byte);
procedure DebugWriteHexByte(b: Byte);
procedure DebugWriteHexWord(w: Word);
procedure DebugWriteHexLongWord(lw: Word);

implementation

type
  P__wasi_size_t = ^__wasi_size_t;
  __wasi_size_t = longint;
  __wasi_fd_t = longint;
  size_t = longint;
  __wasi_errno_t = longint;

  P__wasi_iovec_t = ^__wasi_iovec_t;
  __wasi_iovec_t = record
    buf: PUInt8;
    buf_len: __wasi_size_t;
  end;

  P__wasi_ciovec_t = ^__wasi_ciovec_t;
  __wasi_ciovec_t = record
    buf: pointer;
    buf_len: __wasi_size_t;
  end;

  __wasi_exitcode_t = UInt32;

  __wasi_filetype_t = UInt8;

const
  __WASI_FILETYPE_UNKNOWN          = 0;
  __WASI_FILETYPE_BLOCK_DEVICE     = 1;
  __WASI_FILETYPE_CHARACTER_DEVICE = 2;
  __WASI_FILETYPE_DIRECTORY        = 3;
  __WASI_FILETYPE_REGULAR_FILE     = 4;
  __WASI_FILETYPE_SOCKET_DGRAM     = 5;
  __WASI_FILETYPE_SOCKET_STREAM    = 6;
  __WASI_FILETYPE_SYMBOLIC_LINK    = 7;

type
  __wasi_fdflags_t = UInt16;

const
  __WASI_FDFLAGS_APPEND   = 1;
  __WASI_FDFLAGS_DSYNC    = 2;
  __WASI_FDFLAGS_NONBLOCK = 4;
  __WASI_FDFLAGS_RSYNC    = 8;
  __WASI_FDFLAGS_SYNC     = 16;

type
  __wasi_rights_t = UInt64;

const
  __WASI_RIGHTS_FD_DATASYNC             = 1;
  __WASI_RIGHTS_FD_READ                 = 2;
  __WASI_RIGHTS_FD_SEEK                 = 4;
  __WASI_RIGHTS_FD_FDSTAT_SET_FLAGS     = 8;
  __WASI_RIGHTS_FD_SYNC                 = 16;
  __WASI_RIGHTS_FD_TELL                 = 32;
  __WASI_RIGHTS_FD_WRITE                = 64;
  __WASI_RIGHTS_FD_ADVISE               = 128;
  __WASI_RIGHTS_FD_ALLOCATE             = 256;
  __WASI_RIGHTS_PATH_CREATE_DIRECTORY   = 512;
  __WASI_RIGHTS_PATH_CREATE_FILE        = 1024;
  __WASI_RIGHTS_PATH_LINK_SOURCE        = 2048;
  __WASI_RIGHTS_PATH_LINK_TARGET        = 4096;
  __WASI_RIGHTS_PATH_OPEN               = 8192;
  __WASI_RIGHTS_FD_READDIR              = 16384;
  __WASI_RIGHTS_PATH_READLINK           = 32768;
  __WASI_RIGHTS_PATH_RENAME_SOURCE      = 65536;
  __WASI_RIGHTS_PATH_RENAME_TARGET      = 131072;
  __WASI_RIGHTS_PATH_FILESTAT_GET       = 262144;
  __WASI_RIGHTS_PATH_FILESTAT_SET_SIZE  = 524288;
  __WASI_RIGHTS_PATH_FILESTAT_SET_TIMES = 1048576;
  __WASI_RIGHTS_FD_FILESTAT_GET         = 2097152;
  __WASI_RIGHTS_FD_FILESTAT_SET_SIZE    = 4194304;
  __WASI_RIGHTS_FD_FILESTAT_SET_TIMES   = 8388608;
  __WASI_RIGHTS_PATH_SYMLINK            = 16777216;
  __WASI_RIGHTS_PATH_REMOVE_DIRECTORY   = 33554432;
  __WASI_RIGHTS_PATH_UNLINK_FILE        = 67108864;
  __WASI_RIGHTS_POLL_FD_READWRITE       = 134217728;
  __WASI_RIGHTS_SOCK_SHUTDOWN           = 268435456;

type
  P__wasi_fdstat_t = ^__wasi_fdstat_t;
  __wasi_fdstat_t = record
    fs_filetype: __wasi_filetype_t;
    fs_flags: __wasi_fdflags_t;
    fs_rights_base: __wasi_rights_t;
    fs_rights_inheriting: __wasi_rights_t;
  end;

function fd_write(fd: __wasi_fd_t;
                  iovs: P__wasi_ciovec_t;
                  iovs_len: size_t;
                  nwritten: P__wasi_size_t): __wasi_errno_t; external 'wasi_snapshot_preview1';
function fd_read(fd: __wasi_fd_t;
                 iovs: P__wasi_iovec_t;
                 iovs_len: size_t;
                 nread: P__wasi_size_t): __wasi_errno_t; external 'wasi_snapshot_preview1';
procedure proc_exit(rval: __wasi_exitcode_t); noreturn; external 'wasi_snapshot_preview1';
function fd_fdstat_get(fd: __wasi_fd_t;
                       stat: P__wasi_fdstat_t): __wasi_errno_t; external 'wasi_snapshot_preview1';

{$I system.inc}

function GetProcessID: SizeUInt;
begin
end;

Procedure Randomize;
Begin
End;

procedure System_exit;
begin
  proc_exit(ExitCode);
End;

Function ParamCount: Longint;
Begin
End;

function paramstr(l: longint) : string;
begin
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

procedure DebugWrite(const P: PChar);
var
  our_iov: __wasi_ciovec_t;
  our_nwritten: longint;
begin
  our_iov.buf := P;
  our_iov.buf_len := StrLen(P);
  fd_write(1, @our_iov, 1, @our_nwritten);
end;

procedure DebugWriteLn(const P: PChar);
begin
  DebugWrite(P);
  DebugWriteChar(#10);
end;

procedure DebugWriteChar(Ch: Char);
var
  CharArr: array [0..1] of Char;
begin
  CharArr[0] := Ch;
  CharArr[1] := #0;
  DebugWrite(@CharArr);
end;

procedure DebugWriteHexDigit(d: Byte);
const
  HexDigits: array [0..15] of Char = '0123456789ABCDEF';
begin
  DebugWriteChar(HexDigits[d]);
end;

procedure DebugWriteHexByte(b: Byte);
begin
  DebugWriteHexDigit(b shr 4);
  DebugWriteHexDigit(b and 15);
end;

procedure DebugWriteHexWord(w: Word);
begin
  DebugWriteHexByte(w shr 8);
  DebugWriteHexByte(Byte(w));
end;

procedure DebugWriteHexLongWord(lw: Word);
begin
  DebugWriteHexWord(lw shr 16);
  DebugWriteHexWord(Word(lw));
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
end.
