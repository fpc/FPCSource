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

    dev_t    = cuint;          { used for device numbers      }
    TDev     = dev_t;
    pDev     = ^dev_t;

    kDev_t   = cushort;         // Linux has two different device conventions
    TkDev    = KDev_t;          // kernel and glibc. This is kernel.
    pkDev    = ^kdev_t;

    gid_t    = cuint32;         { used for group IDs           }
    TGid     = gid_t;
    pGid     = ^gid_t;
    TIOCtlRequest = cInt;

    ino_t    = clong;           { used for file serial numbers }
    TIno     = ino_t;
    pIno     = ^ino_t;

    mode_t   = cuint32;         { used for file attributes     }
    TMode    = mode_t;
    pMode    = ^mode_t;

    nlink_t  = cuint32;         { used for link counts         }
    TnLink   = nlink_t;
    pnLink   = ^nlink_t;

{$ifdef cpu64}
    off_t    = cint64;          { used for file sizes          }
{$else}
   {$ifdef 64BitFS}
    off_t    = cint64;
   {$else}
    off_t    = cint;
   {$endif}
{$endif}
    TOff     = off_t;
    pOff     = ^off_t;

    pid_t    = cint32;          { used as process identifier   }
    TPid     = pid_t;
    pPid     = ^pid_t;

{$ifdef cpu64}
    size_t   = cuint64;         { as definied in the C standard}
    ssize_t  = cint64;          { used by function for returning number of bytes }
    clock_t  = cuint64;
    time_t   = cint64;           { used for returning the time  }
{$else}
    size_t   = cuint32;         { as definied in the C standard}
    ssize_t  = cint32;          { used by function for returning number of bytes }
    clock_t  = culong;
    time_t   = clong;           { used for returning the time  }
{$endif}
    wint_t    = cint32;
    TSize     = size_t;
    pSize     = ^size_t;
    psize_t   = pSize;
    TSSize    = ssize_t;
    pSSize    = ^ssize_t;
    TClock    = clock_t;
    pClock    = ^clock_t;
    TTime     = time_t;
    pTime     = ^time_t;
    ptime_t   = ^time_t;
    clockid_t = cint;
    caddr_t   = ^char;

    uint32_t = cuint32;
    int32_t = cint32;
    caddr32_t = uint32_t;
    daddr32_t = int32_t;
    off32_t = int32_t;
    ino32_t = uint32_t;
    blkcnt32_t = int32_t;
    fsblkcnt32_t = uint32_t;
    fsfilcnt32_t = uint32_t;
    id32_t = int32_t;
    major32_t = uint32_t;
    minor32_t = uint32_t;
    key32_t = int32_t;
    mode32_t = uint32_t;
    uid32_t = int32_t;
    gid32_t = int32_t;
    nlink32_t = uint32_t;
    dev32_t = uint32_t;
    pid32_t = int32_t;
    size32_t = uint32_t;
    ssize32_t = int32_t;
    time32_t = int32_t;


    uint16_t = cuint16;
    upad64_t  = qword;
    uintptr_t = ^cuint;
    uint_t    = cuint;


    wchar_t   = widechar;
    pwchar_t  = ^wchar_t;

    uid_t    = cuint32;         { used for user ID type        }
    TUid     = uid_t;
    pUid     = ^uid_t;

    socklen_t= cuint32;
    TSockLen = socklen_t;
    pSockLen = ^socklen_t;

  timeval     = packed record
                 tv_sec,
                 tv_usec:clong;
                end;
  ptimeval    = ^timeval;
  TTimeVal    = timeval;

  timespec    = packed record
                 tv_sec   : time_t;
                 tv_nsec  : clong;
                end;
  ptimespec   = ^timespec;
  TTimeSpec   = timespec;

  TStatfs = packed record
    fstype,            { File system type }
    bsize   : cint;    { Optimal block trensfer size }
    blocks,            { Data blocks in system }
    bfree,             { free blocks in system }
    bavail,            { Available free blocks to non-root users }
    files,             { File nodes in system }
    ffree   : clong;             { Free file nodes in system }
    fsid    : array[0..1] of cint;          { File system ID }
    namelen : clong; { Maximum name length in system }
    spare   : array [0..5] of clong; { For later use }
  end;
  PStatFS=^TStatFS;


  clock32_t = int32_t;
  timeval32 = record
{ seconds  }
       tv_sec : time32_t;
{ and microseconds  }
       tv_usec : int32_t;
    end;


  timespec32 = record
{ seconds  }
       tv_sec : time32_t;
{ and nanoseconds  }
       tv_nsec : int32_t;
    end;
  timespec32_t = timespec32;

  itimerspec32 = record
       it_interval : timespec32;
       it_value : timespec32;
    end;
  itimerspec32_t = itimerspec32;

const
   { System limits, POSIX value in parentheses, used for buffer and stack allocation }
   { took idefix' values}

    ARG_MAX        = 131072;   {4096}  { Maximum number of argument size     }
    NAME_MAX       = 255;      {14}    { Maximum number of bytes in filename }
    PATH_MAX       = 4095;     {255}   { Maximum number of bytes in pathname }
    SYS_NMLN       = 65;
    SIG_MAXSIG      = 128;      // highest signal version

 { For getting/setting priority }
  Prio_Process = 0;
  Prio_PGrp    = 1;
  Prio_User    = 2;


 type
    pthread_t = cuint;

    pthread_attr_t = record
      __pthread_attrp : pointer;
    end;

    pthread_mutexattr_t = record
      __pthread_mutexattrp : pointer;
    end;

    pthread_cond_t = record
      __pthread_cond_flags : record
        __pthread_cond_flag : array[0..3] of byte;
        __pthread_cond_type : uint16_t;
        __pthread_cond_magic : uint16_t;
      end;
      __pthread_cond_data : upad64_t;
    end;

    pthread_condattr_t = record
      __pthread_condattrp : pointer;
    end;

    pthread_key_t = cuint;

    pthread_mutex_t = record
      __pthread_mutex_flags : record
           __pthread_mutex_flag1 : word;
           __pthread_mutex_flag2 : byte;
           __pthread_mutex_ceiling : byte;
           __pthread_mutex_type : word;
           __pthread_mutex_magic : word;
        end;
      __pthread_mutex_lock : record
          case longint of
             0 : ( __pthread_mutex_lock64 : record
                  __pthread_mutex_pad : array[0..7] of byte;
               end );
             1 : ( __pthread_mutex_lock32 : record
                  __pthread_ownerpid : dword;
                  __pthread_lockword : dword;
               end );
             2 : ( __pthread_mutex_owner64 : qword );
          end;
      __pthread_mutex_data : qword;
    end;

    pthread_rwlock_t = record
      __pthread_rwlock_readers : int32_t;
      __pthread_rwlock_type : uint16_t;
      __pthread_rwlock_magic : uint16_t;
      __pthread_rwlock_mutex : pthread_mutex_t;
      __pthread_rwlock_readercv : pthread_cond_t;
      __pthread_rwlock_writercv : pthread_cond_t;
    end;

    sem_t = record
      sem_count : uint32_t;
      sem_type : uint16_t;
      sem_magic : uint16_t;
      sem_pad1 : array[0..2] of upad64_t;
      sem_pad2 : array[0..1] of upad64_t;
    end;
