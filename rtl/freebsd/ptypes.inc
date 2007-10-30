{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

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

{$I ctypes.inc}
{$packrecords c}

type

    dev_t    = cuint32;         { used for device numbers      }
    TDev     = dev_t;
    pDev     = ^dev_t;

    gid_t    = cuint32;         { used for group IDs           }
    TGid     = gid_t;
    pGid     = ^gid_t;
    TIOCtlRequest = cuLong;

    {$ifdef CPU64}
     ino_t    = cuint32;           { used for file serial numbers }
    {$else}	
     ino_t    = clong;           { used for file serial numbers }
    {$endif}
    TIno     = ino_t;
    pIno     = ^ino_t;

    mode_t   = cuint16;         { used for file attributes     }
    TMode    = mode_t;
    pMode    = ^mode_t;

    nlink_t  = cuint16;         { used for link counts         }
    TnLink   = nlink_t;
    pnLink   = ^nlink_t;

    off_t    = cint64;          { used for file sizes          }
    TOff     = off_t;
    pOff     = ^off_t;

    pid_t    = cint32;          { used as process identifier   }
    TPid     = pid_t;
    pPid     = ^pid_t;

{$ifdef CPU64}
    size_t   = cuint64;
{$else}
    size_t   = cuint32;         { as definied in the C standard}
{$endif}
    TSize    = size_t;
    pSize    = ^size_t;
    pSize_t  = ^size_t;

{$ifdef CPU64}
    ssize_t  = cint64;          { used by function for returning number of bytes }
{$else}
    ssize_t  = cint32;          { used by function for returning number of bytes}
{$endif}

    TsSize   = ssize_t;
    psSize   = ^ssize_t;

    uid_t    = cuint32;         { used for user ID type        }
    TUid     = Uid_t;
    pUid     = ^Uid_t;

    wint_t    = cint32;
    wchar_t   = widechar;
    pwchar_t  = ^wchar_t;


{$ifdef CPU64}
    clock_t  = cuint32;         // 32-bit even on Athlon64
{$else}
    clock_t  = culong;
{$endif}
    TClock   = clock_t;
    pClock   = ^clock_t;

    time_t   = clong;           { used for returning the time, clong
                                        is 64-bit on AMD64}
    TTime    = time_t;
    pTime    = ^time_t;
    ptime_t =  ^time_t;

    socklen_t= cuint32;
    TSocklen = socklen_t;
    pSocklen = ^socklen_t;

  timeval  = packed record
    tv_sec,tv_usec:clong;
  end;
  ptimeval = ^timeval;
  TTimeVal = timeval;

  timespec = packed record
    tv_sec   : time_t;                  // should be time_t, bug compability
    tv_nsec  : clong;
  end;
  ptimespec= ^timespec;
  Ttimespec= timespec;

   pthread_t            = pointer;
   pthread_attr_t       = pointer;
   pthread_mutex_t      = {$i pmutext.inc}
   pthread_mutexattr_t  = pointer;
   pthread_cond_t       = pointer;
   pthread_condattr_t   = pointer;
   pthread_key_t        = cint;
   pthread_rwlock_t     = pointer;
   pthread_rwlockattr_t = pointer;

   sem_t                = pointer;
   rlim_t		= int64;
   TRlim		= rlim_t;
  {
     Mutex types (Single UNIX Specification, Version 2, 1997).

     Note that a mutex attribute with one of the following types:

     PTHREAD_MUTEX_NORMAL
     PTHREAD_MUTEX_RECURSIVE
     MUTEX_TYPE_FAST (deprecated)
     MUTEX_TYPE_COUNTING_FAST (deprecated)

       will deviate from POSIX specified semantics.
      }

  pthread_mutextype = (
    { Default POSIX mutex  }
    _PTHREAD_MUTEX_ERRORCHECK := 1,
    { Recursive mutex  }
    _PTHREAD_MUTEX_RECURSIVE := 2,
    { No error checking  }
    _PTHREAD_MUTEX_NORMAL    := 3,
    _MUTEX_TYPE_MAX
  );


Const
     MNAMLEN   = 80;      // slightly machine specific.
     MFSNamLen = 16;	
type
  fsid_t  = array[0..1] of cint;

// Kernel statfs

  TStatfs = packed record
    spare2,            { place holder}
    bsize,             { fundamental block size}
    iosize,            { optimal block size }
    blocks,            {  total blocks}
    bfree,             { blocks free}
    bavail,            { block available for mortal users}
    files,             { Total file nodes}
    ffree          : clong ;    { file nodes free}
    fsid           : fsid_t;
    fowner         : tuid; {mounter uid}
    ftype          : cint;
    fflags         : cint; {copy of mount flags}
    fsyncwrites,
    fasyncwrites   : clong;
    fstypename     : array[0..MFSNamLen-1] of char;
    mountpoint     : array[0..MNAMLEN-1] of char;
    fsyncreads,            { count of sync reads since mount }
    fasyncreads    : clong;
    fspares1       : cshort;
    mnfromname     : array[0..MNAMLEN-1] of char;
    fspares2       : cshort;
    fspare3        : array[0..1] of clong;
  end;
  PStatFS=^TStatFS;

  ITimerVal= Record
              It_Interval,
              It_Value      : TimeVal;
             end;

const
  _PTHREAD_MUTEX_DEFAULT     = _PTHREAD_MUTEX_ERRORCHECK;
  _MUTEX_TYPE_FAST          = _PTHREAD_MUTEX_NORMAL;
  _MUTEX_TYPE_COUNTING_FAST = _PTHREAD_MUTEX_RECURSIVE;

  _PTHREAD_KEYS_MAX              = 256;
  _PTHREAD_STACK_MIN             = 1024;

   { System limits, POSIX value in parentheses, used for buffer and stack allocation }
{$ifdef CPU64}
    ARG_MAX  = 262144;   {4096}  { Maximum number of argument size     }
{$else}
    ARG_MAX  = 65536;   {4096}  { Maximum number of argument size     }
{$endif}

    NAME_MAX = 255;     {14}    { Maximum number of bytes in filename }
    PATH_MAX = 1024;    {255}   { Maximum number of bytes in pathname }
    SYS_NMLN = 32;              {BSD utsname struct limit, kernel mode}

    SIG_MAXSIG      = 128;      // highest signal version
//  wordsinsigset   = 4;               // words in sigset_t


  { For getting/setting priority }
   Prio_Process = 0;
   Prio_PGrp    = 1;
   Prio_User    = 2;

