{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team
    Copyright (c) 2011 by Jonas Maebe

    This file implements all the base types and limits required
    for a minimal POSIX compliant subset required to port the compiler
    to a new OS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{***********************************************************************}
{                       POSIX TYPE DEFINITIONS                          }
{***********************************************************************}

{ Introduced defines
    - 64bitfs   (should be on if libc switches to a 64-bit system.

All three tested systems (PPC,Alpha,2x i386) gave the same POSIX limits,
and all three 32-bit systems returned completely identical types too
(everything 32-bit except dev_t, which is assumed to be a result of devfs
introduction)
}

{$I ctypes.inc}
{$packrecords c}

Type

    dev_t    = {$ifdef cpu64}culong{$else}cuint{$endif};
    TDev     = dev_t;
    pDev     = ^dev_t;

    gid_t    = cuint;         { used for group IDs           }
    TGid     = gid_t;
    pGid     = ^gid_t;
    TIOCtlRequest = cInt;

    ino_t    = {$ifdef cpu64}culong{$else}cuint{$endif};           { used for file serial numbers }
    TIno     = ino_t;
    pIno     = ^ino_t;
    ino64_t  = cuint64;

    mode_t   = cuint;         { used for file attributes     }
    TMode    = mode_t;
    pMode    = ^mode_t;

    nlink_t  = cshort;        { used for link counts         }
    TnLink   = nlink_t;
    pnLink   = ^nlink_t;

    off_t    = clonglong;     { used for file sizes          }
    TOff     = off_t;
    pOff     = ^off_t;

    pid_t    = cint;          { used as process identifier   }
    TPid     = pid_t;
    pPid     = ^pid_t;

    size_t   = culong;         { as definied in the C standard}
    ssize_t  = clong;          { used by function for returning number of bytes }
    clock_t  = cint;
    time_t   = {$ifdef cpu64}clong{$else}cint{$endif};           { used for returning the time  }
    wint_t    = cint;
    TSize     = size_t;
    pSize     = ^size_t;
    psize_t   = pSize;
    TSSize    = ssize_t;
    pSSize    = ^ssize_t;
    TClock    = clock_t;
    pClock    = ^clock_t;
    // TTime    = time_t;    // Not allowed in system unit, -> unixtype

    pTime     = ^time_t;
    ptime_t   = ^time_t;
    clockid_t = clonglong;
    caddr_t   = ^char;

    uint32_t = cuint32;
    int32_t = cint32;
//    caddr32_t = uint32_t;
    daddr32_t = cint;
//    off32_t = cuint;
    ino32_t = cuint;
    blkcnt32_t = cint;
//    fsblkcnt32_t = uint32_t;
//    fsfilcnt32_t = uint32_t;
//    id32_t = int32_t;
//    major32_t = uint32_t;
//    minor32_t = uint32_t;
//    key32_t = int32_t;
//    mode32_t = uint32_t;
//    uid32_t = int32_t;
//    gid32_t = int32_t;
//    nlink32_t = uint32_t;
    dev32_t = cuint;
    pid32_t = cuint;
//    size32_t = uint32_t;
//    ssize32_t = int32_t;
    time32_t = cint;


    uint16_t = cuint16;
//    upad64_t  = qword;
    uintptr_t = ^cuint;
    uint_t    = cuint;

    fsid_t = record
      val: array[0..1] of cuint;
    end;

    fsid64_t = record
      val: array[0..1] of cuint64;
    end;


{$ifdef cpu64}
    wchar_t   = cuint;
{$else cpu64}
    wchar_t   = cushort; { utf-16! }
{$endif cpu64}
    pwchar_t  = ^wchar_t;

    uid_t    = cuint;         { used for user ID type        }
    TUid     = uid_t;
    pUid     = ^uid_t;

    socklen_t= cuint32;
    TSockLen = socklen_t;
    pSockLen = ^socklen_t;

  suseconds_t = cint;
  timeval     = record
                 tv_sec  : time_t;
                 tv_usec : suseconds_t;
                end;
  ptimeval    = ^timeval;
  TTimeVal    = timeval;

  timespec    =  record
                 tv_sec   : time_t;
                 tv_nsec  : clong;
                end;
  ptimespec   = ^timespec;
  TTimeSpec   = timespec;

  fsblkcnt_t = culong;

  TStatfs = record
    f_version : cint;  { version/type of statfs, 0 for now }
    f_type    : cint;  { type of info, zero for now }
    bsize     : culong;  { fundamental file system block size  }
    blocks,            { total data blocks in file system }
    bfree,             { free blocks in fs }
    bavail,            { free blocks avail to non-superuser }
    files,             { total file nodes in file system }
    ffree   : fsblkcnt_t;   { free file nodes in fs }
{$ifdef cpu64}
    fsid    : fsid64_t;{ File system ID }
{$else}
    fsid    : fsid_t;
{$endif}
    vfstype : cint;    { what type of vfs this is }
    fsize   : culong;  { fundamental file system block size }
    vfsnumber : cint;  { vfs indentifier number }
    vfsoff : cint;     { reserved, for vfs specific data offset }
    vfslen : cint;     { reserved, for len of vfs specific data }
    vfsvers : cint;    { reserved, for vers of vfs specific data }
    fstr   : array[0..31] of char; { fname in C header, file system name (usually mount pt.) }
    fpack   : array[0..31] of char; { file system pack name }
    namelen : cint; { maximum component name length for posix }
  end;
  PStatFS=^TStatFS;

  mbstate_t = pointer;
  pmbstate_t = ^mbstate_t;
  

//  clock32_t = int32_t;
  timeval32 = record
{ seconds  }
       tv_sec : int32_t;
{ and microseconds  }
       tv_usec : int32_t;
    end;


const
   { System limits, POSIX value in parentheses, used for buffer and stack allocation }
   { took idefix' values}

    ARG_MAX        = 24576;    { Maximum number of argument size     }
    // depends on file system, must use pathconf() or fpathconf() instead of constant!
    NAME_MAX       = 255;      { Maximum number of bytes in filename }
    PATH_MAX       = 1023;     { Maximum number of bytes in pathname }
    SYS_NMLN       = 32;
{$ifdef cpu64}
    SIG_MAXSIG      = 255;     // highest signal number
{$else}
    SIG_MAXSIG      = 63;      // highest signal number
{$endif}

 { For getting/setting priority }
  Prio_Process = 0;
  Prio_PGrp    = 1;
  Prio_User    = 2;


 type
    pthread_t = cuint;

    pthread_attr_t = pointer;

    pthread_mutexattr_t = pointer;

    pthread_cond_t = record
{$ifdef cpu64}
       __co_word: array[0..5] of clong;
{$else}
       __co_word: array[0..10] of cint;
{$endif}
    end;

    pthread_condattr_t = pointer;

    pthread_key_t = cuint;

    pthread_mutex_t = record
{$ifdef cpu64}
      __mt_word: array[0..7] of clong;
{$else}
      __mt_word: array[0..12] of cint;
{$endif}
   end;

    pthread_rwlock_t = record
 {$ifdef cpu64}
       __rw_word: array[0..9] of clong;
 {$else}
       __rw_word: array[0..51] of cint;
 {$endif}
    end;

    sem_t = cint;
