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
  P__wasi_fd_t = ^__wasi_fd_t;
  __wasi_fd_t = longint;
  size_t = longint;
  __wasi_errno_t = longint;

const
  __WASI_ERRNO_SUCCESS        = 0;
  __WASI_ERRNO_2BIG           = 1;
  __WASI_ERRNO_ACCES          = 2;
  __WASI_ERRNO_ADDRINUSE      = 3;
  __WASI_ERRNO_ADDRNOTAVAIL   = 4;
  __WASI_ERRNO_AFNOSUPPORT    = 5;
  __WASI_ERRNO_AGAIN          = 6;
  __WASI_ERRNO_ALREADY        = 7;
  __WASI_ERRNO_BADF           = 8;
  __WASI_ERRNO_BADMSG         = 9;
  __WASI_ERRNO_BUSY           = 10;
  __WASI_ERRNO_CANCELED       = 11;
  __WASI_ERRNO_CHILD          = 12;
  __WASI_ERRNO_CONNABORTED    = 13;
  __WASI_ERRNO_CONNREFUSED    = 14;
  __WASI_ERRNO_CONNRESET      = 15;
  __WASI_ERRNO_DEADLK         = 16;
  __WASI_ERRNO_DESTADDRREQ    = 17;
  __WASI_ERRNO_DOM            = 18;
  __WASI_ERRNO_DQUOT          = 19;
  __WASI_ERRNO_EXIST          = 20;
  __WASI_ERRNO_FAULT          = 21;
  __WASI_ERRNO_FBIG           = 22;
  __WASI_ERRNO_HOSTUNREACH    = 23;
  __WASI_ERRNO_IDRM           = 24;
  __WASI_ERRNO_ILSEQ          = 25;
  __WASI_ERRNO_INPROGRESS     = 26;
  __WASI_ERRNO_INTR           = 27;
  __WASI_ERRNO_INVAL          = 28;
  __WASI_ERRNO_IO             = 29;
  __WASI_ERRNO_ISCONN         = 30;
  __WASI_ERRNO_ISDIR          = 31;
  __WASI_ERRNO_LOOP           = 32;
  __WASI_ERRNO_MFILE          = 33;
  __WASI_ERRNO_MLINK          = 34;
  __WASI_ERRNO_MSGSIZE        = 35;
  __WASI_ERRNO_MULTIHOP       = 36;
  __WASI_ERRNO_NAMETOOLONG    = 37;
  __WASI_ERRNO_NETDOWN        = 38;
  __WASI_ERRNO_NETRESET       = 39;
  __WASI_ERRNO_NETUNREACH     = 40;
  __WASI_ERRNO_NFILE          = 41;
  __WASI_ERRNO_NOBUFS         = 42;
  __WASI_ERRNO_NODEV          = 43;
  __WASI_ERRNO_NOENT          = 44;
  __WASI_ERRNO_NOEXEC         = 45;
  __WASI_ERRNO_NOLCK          = 46;
  __WASI_ERRNO_NOLINK         = 47;
  __WASI_ERRNO_NOMEM          = 48;
  __WASI_ERRNO_NOMSG          = 49;
  __WASI_ERRNO_NOPROTOOPT     = 50;
  __WASI_ERRNO_NOSPC          = 51;
  __WASI_ERRNO_NOSYS          = 52;
  __WASI_ERRNO_NOTCONN        = 53;
  __WASI_ERRNO_NOTDIR         = 54;
  __WASI_ERRNO_NOTEMPTY       = 55;
  __WASI_ERRNO_NOTRECOVERABLE = 56;
  __WASI_ERRNO_NOTSOCK        = 57;
  __WASI_ERRNO_NOTSUP         = 58;
  __WASI_ERRNO_NOTTY          = 59;
  __WASI_ERRNO_NXIO           = 60;
  __WASI_ERRNO_OVERFLOW       = 61;
  __WASI_ERRNO_OWNERDEAD      = 62;
  __WASI_ERRNO_PERM           = 63;
  __WASI_ERRNO_PIPE           = 64;
  __WASI_ERRNO_PROTO          = 65;
  __WASI_ERRNO_PROTONOSUPPORT = 66;
  __WASI_ERRNO_PROTOTYPE      = 67;
  __WASI_ERRNO_RANGE          = 68;
  __WASI_ERRNO_ROFS           = 69;
  __WASI_ERRNO_SPIPE          = 70;
  __WASI_ERRNO_SRCH           = 71;
  __WASI_ERRNO_STALE          = 72;
  __WASI_ERRNO_TIMEDOUT       = 73;
  __WASI_ERRNO_TXTBSY         = 74;
  __WASI_ERRNO_XDEV           = 75;
  __WASI_ERRNO_NOTCAPABLE     = 76;

type
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

  __wasi_lookupflags_t = UInt32;

const
  __WASI_LOOKUPFLAGS_SYMLINK_FOLLOW = 1;

type
  __wasi_oflags_t = UInt16;

const
  __WASI_OFLAGS_CREAT     = 1;
  __WASI_OFLAGS_DIRECTORY = 2;
  __WASI_OFLAGS_EXCL      = 4;
  __WASI_OFLAGS_TRUNC     = 8;

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
function path_open(fd: __wasi_fd_t;
                   dirflags: __wasi_lookupflags_t;
                   path: PChar;
                   path_len: size_t;
                   oflags: __wasi_oflags_t;
                   fs_rights_base,
                   fs_rights_inherting: __wasi_rights_t;
                   fdflags: __wasi_fdflags_t;
                   opened_fd: P__wasi_fd_t): __wasi_errno_t; external 'wasi_snapshot_preview1';

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
{$ifdef FPC_HAS_FEATURE_THREADING}
  InitSystemThreads;
{$endif}
end.
