unit system;

interface

type
  integer = longint;
  hresult = integer; 
  ttypekind = integer;
  filerec = integer;
  textrec = integer;
  pbyte = ^byte;
  pchar = ^Char;

procedure fpc_lib_exit; compilerproc;
procedure DebugWrite(const P: PChar);
procedure DebugWriteChar(Ch: Char);
procedure DebugWriteHexDigit(d: Byte);
procedure DebugWriteHexByte(b: Byte);

implementation

type
  P__wasi_size_t = ^__wasi_size_t;
  __wasi_size_t = longint;
  __wasi_fd_t = longint;
  size_t = longint;
  __wasi_errno_t = longint;

  P__wasi_ciovec_t = ^__wasi_ciovec_t;
  __wasi_ciovec_t = record
    buf: pointer;
    buf_len: __wasi_size_t;
  end;

function fd_write(fd: __wasi_fd_t;
                  iovs: P__wasi_ciovec_t;
                  iovs_len: size_t;
                  nwritten: P__wasi_size_t): __wasi_errno_t; external 'wasi_unstable';

function StrLen(P: PChar): size_t;
var
  i: size_t;
begin
  i := 0;
  while p[i]<>#0 do
    Inc(i);
  StrLen := i;
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

procedure fpc_lib_exit; compilerproc;
begin
end;

end.
