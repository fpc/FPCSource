uses
  DosCalls, DosCall2;

(*
NAME
       fcntl - manipulate file descriptor

SYNOPSIS
       #include <unistd.h>
       #include <fcntl.h>

       int fcntl(int fd, int cmd);
       int fcntl(int fd, int cmd, long arg);
       int fcntl(int fd, int cmd, struct flock * lock);

DESCRIPTION
       fcntl  performs one of various miscellaneous operations on
       fd.  The operation in question is determined by cmd:

       F_GETLK, F_SETLK and F_SETLKW are used to  manage  discreð
       tionary  file locks.  The third argument lock is a pointer
       to a struct flock (that may be overwritten by this  call).

       F_GETLK
              Return  the  flock  structure that prevents us from
              obtaining the lock, or set the l_type field of  the
              lock to F_UNLCK if there is no obstruction.

       F_SETLK
              The lock is set (when l_type is F_RDLCK or F_WRLCK)
              or cleared (when it is F_UNLCK).  If  the  lock  is
              held by someone else, this call returns -1 and sets
              errno to EACCES or EAGAIN.

       F_SETLKW
              Like F_SETLK, but instead of returning an error  we
              wait for the lock to be released.  If a signal that
              is to be caught is received while fcntl is waiting,
              it is interrupted and (after the signal handler has
              returned) returns immediately (with return value -1
              and errno set to EINTR).

       Using  these  mechanisms,  a  program  can implement fully
       asynchronous I/O without using select(2) or  poll(2)  most
       of the time.

       The  use of O_ASYNC, F_GETOWN, F_SETOWN is specific to BSD
       and Linux.   F_GETSIG  and  F_SETSIG  are  Linux-specific.
       POSIX  has asynchronous I/O and the aio_sigevent structure
       to achieve similar things; these  are  also  available  in
       Linux as part of the GNU C Library (Glibc).

RETURN VALUE
       For  a  successful  call,  the return value depends on the
       operation:

       F_GETFD  Value of flag.

       F_GETFL  Value of flags.

       F_GETOWN Value of descriptor owner.

       F_GETSIG Value of signal sent when read or  write  becomes
                possible,   or   zero   for   traditional   SIGIO
                behaviour.

       All other commands
                Zero.

       On error, -1 is returned, and errno is set  appropriately.

ERRORS
       EACCES   Operation  is  prohibited  by locks held by other
                processes.

       EAGAIN   Operation is prohibited because the file has been
                memory-mapped by another process.

       EBADF    fd is not an open file descriptor.

       EDEADLK  It  was detected that the specified F_SETLKW comð
                mand would cause a deadlock.

       EFAULT   lock is outside your accessible address space.

       EINTR    For F_SETLKW, the command was  interrupted  by  a
                signal.  For F_GETLK and F_SETLK, the command was
                interrupted by  a  signal  before  the  lock  was
                checked  or acquired.  Most likely when locking a
                remote file (e.g.  locking  over  NFS),  but  can
                sometimes happen locally.

       EINVAL   For  F_DUPFD,  arg is negative or is greater than
                the maximum allowable value.  For  F_SETSIG,  arg
                is not an allowable signal number.

       EMFILE   For  F_DUPFD, the process already has the maximum
                number of file descriptors open.

       ENOLCK   Too many segment locks open, lock table is  full,
                or a remote locking protocol failed (e.g. locking
                over NFS).

       EPERM    Attempted to clear the O_APPEND flag  on  a  file
                that has the append-only attribute set.

typedef long  __kernel_off_t;
typedef int   __kernel_pid_t;

struct flock {
        short l_type;
        short l_whence;
        off_t l_start;
        off_t l_len;
        pid_t l_pid;
};

whence:
--------
const
  SEEK_SET        = 0;      { Seek from beginning of file.  }
  SEEK_CUR        = 1;      { Seek from current position.  }
  SEEK_END        = 2;      { Seek from end of file.  }

{ Old BSD names for the same constants; just for compatibility.  }
  L_SET           = SEEK_SET;
  L_INCR          = SEEK_CUR;
  L_XTND          = SEEK_END;
*)

(*
const
   F_RDLCK = 0;
   F_WRLCK = 1;
   F_UNLCK = 2;
   F_EXLCK = 4;
   F_SHLCK = 8;

   LOCK_SH = 1;
   LOCK_EX = 2;
   LOCK_NB = 4;
   LOCK_UN = 8;

   LOCK_MAND = 32;
   LOCK_READ = 64;
   LOCK_WRITE = 128;
   LOCK_RW = 192;

   EACCES = ESysEACCES;
   EAGAIN = ESysEAGAIN;
*)
{$PACKRECORDS 1}
const
  LastErr: cardinal = 0;

type
 TQRecL = record
  L1, L2: longint;
 end;

 TQRecC = record
  C1, C2: cardinal;
 end;

function LockFile (hFile: THandle; dwFileOffsetLow, dwFileOffsetHigh: DWORD;
               nNumberOfBytesToLockLow, nNumberOfBytesToLockHigh: DWORD): BOOL;
var
  FLock, FUnlock: TFileLockL;
  RC: Cardinal;
begin
  FillChar (FUnlock, SizeOf (FUnlock), 0);
  TQRecC (FLock.Offset).C2 := dwFileOffsetHigh;
  TQRecC (FLock.Offset).C1 := dwFileOffsetLow;
  TQRecC (FLock.Range).C2 := nNumberOfBytesToLockHigh;
  TQRecC (FLock.Range).C1 := nNumberOfBytesToLockLow;
  RC := DosSetFileLocksL (hFile, FLock, FUnlock, 50, 0);
  if RC <> 0 then
   begin
    LastErr := RC;
    Result := false;
   end
  else
   Result := true;
end;

function UnlockFile (hFile: THandle; dwFileOffsetLow, dwFileOffsetHigh: DWORD;
           nNumberOfBytesToUnlockLow, nNumberOfBytesToUnlockHigh: DWORD): BOOL;
var
  FLock, FUnlock: TFileLockL;
  RC: Cardinal;
begin
  FillChar (FLock, SizeOf (FLock), 0);
  TQRecC (FUnlock.Offset).C2 := dwFileOffsetHigh;
  TQRecC (FUnlock.Offset).C1 := dwFileOffsetLow;
  TQRecC (FUnlock.Range).C2 := nNumberOfBytesToUnlockHigh;
  TQRecC (FUnlock.Range).C1 := nNumberOfBytesToUnlockLow;
  RC := DosSetFileLocksL (hFile, FLock, FUnlock, 50, 0);
  if RC <> 0 then
   begin
    LastErr := RC;
    Result := false;
   end
  else
   Result := true;
end;

function GetOEMCP: Cardinal;
begin
  Result := DefaultSystemCodePage;
end;

function GetACP: Cardinal;
begin
  Result := DefaultSystemCodePage;
end;

function OemToChar(lpszSrc: PChar; lpszDst: PChar): BOOL;
begin
  if lpszDst <> lpszSrc then
    StrCopy(lpszDst, lpszSrc);
  Result := true;
end;

function CharToOem(lpszSrc: PChar; lpszDst: PChar): BOOL;
begin
  if lpszDst <> lpszSrc then
    StrCopy(lpszDst, lpszSrc);
  Result := true;
end;

function OemToCharBuffA(lpszSrc: PChar; lpszDst: PChar; cchDstLength: DWORD): BOOL;
begin
  if lpszDst <> lpszSrc then
    StrLCopy(lpszDst, lpszSrc, cchDstLength);
  Result := true;
end;

function CharToOemBuffA(lpszSrc: PChar; lpszDst: PChar; cchDstLength: DWORD): BOOL;
begin
  if lpszDst <> lpszSrc then
    StrLCopy(lpszDst, lpszSrc, cchDstLength);
  Result := true;
end;

function MultiByteToWideChar(CodePage: DWORD; dwFlags: DWORD; const lpMultiByteStr: LPCSTR; cchMultiByte: Integer; lpWideCharStr: LPWSTR; cchWideChar: Integer): Integer;
var
  TempA: AnsiString;
  TempW: WideString;
begin
{$WARNING To be rewritten using the UnicodeStringManager functionality!}
  TempA := String(lpMultiByteStr^);
  TempW := TempA;
  Result := Length(TempW);
  System.Move(TempW, lpWideCharStr^, Result);
end;

function WideCharToMultiByte(CodePage: DWORD; dwFlags: DWORD; lpWideCharStr: LPWSTR; cchWideChar: Integer; lpMultiByteStr: LPSTR; cchMultiByte: Integer; lpDefaultChar: LPCSTR; lpUsedDefaultChar: PBOOL): Integer;
var
  TempA: AnsiString;
  TempW: WideString;
begin
{$WARNING To be rewritten using the UnicodeStringManager functionality!}
  TempW := WideString(lpWideCharStr^);
  TempA := TempW;
  Result := Length(TempA);
  System.Move(TempA, lpMultiByteStr^, Result);
end;

function CompareString(Locale: LCID; dwCmpFlags: DWORD; lpString1: PChar; cchCount1: Integer; lpString2: PChar; cchCount2: Integer): Integer;
begin
{$WARNING To be rewritten using the UnicodeStringManager functionality!}
  Result := StrLComp(lpString1, lpString2, cchCount1) + 2;
  if Result > 2 then Result := 3;
  if Result < 2 then Result := 1;
end;

function EnumSystemCodePages(lpCodePageEnumProc: TFNCodepageEnumProc; dwFlags: DWORD): BOOL;
begin
{$WARNING To be rewritten using the LIBUNI.DLL functionality!}
  Result := True;
end;

function EnumSystemLocales(lpLocaleEnumProc: TFNLocaleEnumProc; dwFlags: DWORD): BOOL;
begin
{$WARNING To be rewritten using the LIBUNI.DLL functionality!}
(* http://compgroups.net/comp.os.os2.programmer.misc/how-to-obtain-current-locale-lang/2524034 *)
  Result := True;
end;

function GetUserDefaultLCID: LCID;
begin
{$WARNING To be rewritten using the LIBUNI.DLL functionality!}
(* http://compgroups.net/comp.os.os2.programmer.misc/how-to-obtain-current-locale-lang/2524034 *)
  Result := LANG_ENGLISH or (SUBLANG_ENGLISH_UK shl 10);
end;

function GetLastError: Integer;
begin
  Result := Integer (LastErr);
end;

procedure SetLastError (Value: Integer);
begin
  LastErr := cardinal (Value);
end;
