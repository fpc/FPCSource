{
    $Id$
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

{$ifndef VER_1_0}               // maybe wrong (kernel vs libc)
    dev_t    = cuint64;         { used for device numbers      }
{$else}
    dev_t    = int64;
{$endif}
    TDev     = dev_t;
    pDev     = ^dev_t;

    kDev_t   = cushort;         // Linux has two different device conventions
    TkDev    = KDev_t;          // kernel and glibc. This is kernel.
    pkDev    = ^kdev_t;

    gid_t    = cuint32;         { used for group IDs           }
    TGid     = gid_t;
    pGid     = ^gid_t;

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
    TSize     = size_t;
    pSize     = ^size_t;
    TSSize    = ssize_t;
    pSSize    = ^ssize_t;
    TClock    = clock_t;
    pClock    = ^clock_t;
    TTime     = time_t;
    pTime     = ^time_t;
    ptime_t   = ^time_t;

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

  pthread_t = culong;

  sched_param = record
    __sched_priority: cint;
  end;

  pthread_attr_t = record
    __detachstate: cint;
    __schedpolicy: cint;
    __schedparam: sched_param;
    __inheritsched: cint;
    __scope: cint;
    __guardsize: size_t;
    __stackaddr_set: cint;
    __stackaddr: pointer;
    __stacksize: size_t;
  end;

  _pthread_fastlock = record
    __status: clong;
    __spinlock: cint;
  end;

  pthread_mutex_t = record
    __m_reserved: cint;
    __m_count: cint;
    __m_owner: pointer;
    __m_kind:  cint;
    __m_lock: _pthread_fastlock;
  end;

  pthread_mutexattr_t = record
    __mutexkind: cint;
  end;

  pthread_cond_t = record
    __c_lock: _pthread_fastlock;
    __c_waiting: pointer;
    __padding: array[0..48-1-sizeof(_pthread_fastlock)-sizeof(pointer)-sizeof(clonglong)] of byte;
    __align: clonglong;
  end;
    
  pthread_condattr_t = record
    __dummy: cint;
  end;

  pthread_key_t = cuint;

  pthread_rwlock_t = record
    __rw_readers: cint;
    __rw_writer: pointer;
    __rw_read_waiting: pointer;
    __rw_write_waiting: pointer;
    __rw_kind: cint;
    __rw_pshared: cint;
  end;

  pthread_rwlockattr_t = record
    __lockkind: cint;
    __pshared: cint;
  end;
  
  sem_t = record
     __sem_lock: _pthread_fastlock;
     __sem_value: cint;
     __sem_waiting: pointer;
  end;



CONST
    _PTHREAD_MUTEX_TIMED_NP      = 0;
    _PTHREAD_MUTEX_RECURSIVE_NP  = 1;
    _PTHREAD_MUTEX_ERRORCHECK_NP = 2;
    _PTHREAD_MUTEX_ADAPTIVE_NP   = 3;
  
    _PTHREAD_MUTEX_NORMAL     = _PTHREAD_MUTEX_TIMED_NP;
    _PTHREAD_MUTEX_RECURSIVE  = _PTHREAD_MUTEX_RECURSIVE_NP;
    _PTHREAD_MUTEX_ERRORCHECK = _PTHREAD_MUTEX_ERRORCHECK_NP;
    _PTHREAD_MUTEX_DEFAULT    = _PTHREAD_MUTEX_NORMAL;
    _PTHREAD_MUTEX_FAST_NP    = _PTHREAD_MUTEX_ADAPTIVE_NP;


   { System limits, POSIX value in parentheses, used for buffer and stack allocation }
   { took idefix' values}

    ARG_MAX        = 131072;   {4096}  { Maximum number of argument size     }
    NAME_MAX       = 255;      {14}    { Maximum number of bytes in filename }
    PATH_MAX       = 4095;     {255}   { Maximum number of bytes in pathname }
    SYS_NMLN       = 65;
{$ifdef FPC_USE_LIBC}
   SIG_MAXSIG      = 1024;      // highest signal version
{$else}
   SIG_MAXSIG      = 128;       // highest signal version
{$endif}

 { For getting/setting priority }
  Prio_Process = 0;
  Prio_PGrp    = 1;
  Prio_User    = 2;

{
   $Log$
   Revision 1.12  2004-11-14 12:21:08  marco
    * moved some calls from unix to baseunix. Darwin untested.

   Revision 1.11  2004/09/09 20:29:06  jonas
     * fixed definition of pthread_mutex_t for non-linux targets (and for
       linux as well, actually).
     * base libpthread definitions are now in ptypes.inc, included in unixtype
       They sometimes have an extra underscore in front of their name, in
       case they were also exported by the packages/base/pthreads unit, so
       they can keep their original name there
     * cthreadds unit now imports systuils, because it uses exceptions (it
       already did so before as well)
     * fixed many linux definitions of libpthread functions in pthrlinux.inc
       (integer -> cint etc)
     + added culonglong type to ctype.inc

   Revision 1.10  2004/05/02 01:00:07  peter
     * statfs fixed

   Revision 1.9  2004/04/26 16:53:19  peter
     * use cpu64

   Revision 1.8  2004/03/04 22:15:16  marco
    * UnixType changes. Please report problems to me.

   Revision 1.7  2004/01/11 09:56:20  jonas
     * moved tstatfs from systypes.inc to ptypes.inc to fix make cycle with
       -dFPC_USE_LIBC (systypes.inc is now completely commented out)

   Revision 1.6  2003/12/30 12:46:40  marco
    * ptime_t

   Revision 1.5  2003/09/27 13:45:58  peter
     * fpnanosleep exported in baseunix
     * fpnanosleep has pointer arguments to be C compliant

   Revision 1.4  2003/09/14 20:15:01  marco
    * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

   Revision 1.3  2002/12/18 16:43:26  marco
    * new unix rtl, linux part.....

   Revision 1.2  2002/11/12 14:28:40  marco
    * some updates

   Revision 1.1  2002/10/29 16:47:17  marco
    * Linux versions


}

