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

const 
    SEM_SAFE=255;
type

    dev_t    = cuint32;         { used for device numbers      }
    TDev     = dev_t;
    pDev     = ^dev_t;

    gid_t    = cuint32;         { used for group IDs           }
    TGid     = gid_t;
    pGid     = ^gid_t;
    TIOCtlRequest = cuLong;

    ino_t    = cuint64;         { used for inode numbers       }
    TIno     = ino_t;
    pIno     = ^ino_t;

    mode_t   = cuint16;         { used for file attributes     }
    TMode    = mode_t;
    pMode    = ^mode_t;

    nlink_t  = cuint32;         { used for link counts         }
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
    wchar_t   = cint32;
    pwchar_t  = ^wchar_t;


    clock_t  = culong;
    TClock   = clock_t;
    pClock   = ^clock_t;

    time_t   = clong;           { used for returning the time }
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

   pthread_t_rec        = record end;
   pthread_attr_t_rec   = record end;
//   pthread_mutex_t_rec   = record end;
   pthread_mutexattr_t_rec   = record end;
   pthread_cond_t_rec   = record end;
   pthread_condattr_t_rec   = record end;
   pthread_rwlock_t_rec   = record end;
   pthread_rwlockattr_t_rec   = record end;

   pthread_t            = ^pthread_t_rec;
   pthread_attr_t       = ^pthread_attr_t_rec;
   pthread_mutex_t      = {$i pmutext.inc}
   pthread_mutexattr_t  = ^pthread_mutexattr_t_rec;
   pthread_cond_t       = ^pthread_cond_t_rec;
   pthread_condattr_t   = ^pthread_condattr_t_rec;
   pthread_key_t        = cint;
   pthread_rwlock_t     = ^pthread_rwlock_t_rec;
   pthread_rwlockattr_t = ^pthread_rwlockattr_t_rec;
  
   psem_t = ^sem_t;
   ppsem_t= ^psem_t;
   semid_t= pointer;
   sem_t = record
       magic   : cuint32;
       lock    : pthread_mutex_t;
       gtzero  : pthread_cond_t;
       count   : cuint32;
       nwaiters: cuint32;
       semid   : semid_t;
       sysse   : cint;
       entry   : psem_t;
       backpointer : ppsem_t;
       spare   : array[0..SEM_SAFE] of char;

    end;

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

type
  fsid_t  = array[0..1] of cint32;

Const
     MNAMELEN   = 80;
     MFSNAMELEN = 16;	

Type TStatFS = Record
        spare2       : clong;          { placeholder }
        bsize        : clong;          { fundamental filesystem fragment size }
        iosize       : clong;          { optimal transfer block size }
        blocks       : clong;          { total data blocks in filesystem }
        bfree        : clong;          { free blocks in filesystem }
        bavail       : clong;          { free blocks avail to non-superuser }
        files        : clong;          { total file nodes in filesystem }
        ffree        : clong;          { free file nodes in filesystem }
        fsid         : fsid_t;         { filesystem id }
        fowner       : tuid;           { user that mounted the filesystem }
        ftype        : cint32;         { type of filesystem }
        fflags       : cint32;         { copy of mount exported flags }
        fsyncwrites  : clong;          { count of sync writes since mount }
        fasyncwrites : clong;          { count of async writes since mount }
        fstypename   : array[0..MFSNAMELEN-1] of char; { filesystem type name }
        mntonname    : array[0..  MNAMELEN-1] of char; { directory on which mounted }
        fsyncreads   : clong;          { count of sync reads since mount }
        fasyncreads  : clong;          { count of async reads since mount }
        fspares1     : cshort;         { unused spare }
        mntfromname  : array[0..  MNAMELEN-1] of char; { mounted filesystem }
        fspares2     : cshort;         { unused spare }
        fspares3     : clong;          { unused spare }
        fspares4     : clong;          { unused spare }
     end; 
     PStatFS=^TStatFS;

  mbstate_t = record
      case byte of
        0: (__mbstate8: array[0..127] of char);
        1: (_mbstateL: cint64); { for alignment }
    end;
    pmbstate_t = ^mbstate_t;

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
    ARG_MAX    = 262144;  {4096}  { Maximum number of argument size     }
    NAME_MAX   = 255;     {14}    { Maximum number of bytes in filename }
    PATH_MAX   = 1024;    {255}   { Maximum number of bytes in pathname }
    SYS_NMLN   = 32;              { BSD utsname struct limit, kernel mode }
    SIG_MAXSIG = 128;             { highest signal version }

  { For getting/setting priority }
   Prio_Process = 0;
   Prio_PGrp    = 1;
   Prio_User    = 2;

