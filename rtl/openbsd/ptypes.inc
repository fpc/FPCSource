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

{$I ctypes.inc}
{$packrecords c}

type

    dev_t    = cuint32;         { used for device numbers      }
    TDev     = dev_t;
    pDev     = ^dev_t;

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

    off_t    = cint64;          { used for file sizes          }
    TOff     = off_t;
    pOff     = ^off_t;

    pid_t    = cint32;          { used as process identifier   }
    TPid     = pid_t;
    pPid     = ^pid_t;

    size_t   = cuint32;         { as definied in the C standard}
    TSize    = size_t;
    pSize    = ^size_t;		

    ssize_t  = cint32;          { used by function for returning number of bytes }
    TsSize   = ssize_t;
    psSize   = ^ssize_t;		

    uid_t    = cuint32;         { used for user ID type        }
    TUid     = Uid_t;
    pUid     = ^Uid_t;

    clock_t  = culong;
    TClock   = clock_t;
    pClock   = ^clock_t;

    time_t   = clong;           { used for returning the time  }
    TTime    = time_t; 
    pTime    = ^time_t;
    ptime_t  = ^time_t;

    socklen_t= cuint32;
    TSocklen = socklen_t;
    pSocklen = ^socklen_t;

    timeval = packed record
		tv_sec,
		tv_usec : clong;
		end;
    ptimeval= ^timeval;
    TTimeval= timeval;

    timespec = packed record
      tv_sec   : time_t;
      tv_nsec  : clong;
    end;
  ptimespec= ^timespec;
  Ttimespec= timespec;

   pthread_t            = pointer;
   pthread_attr_t       = pointer;
   pthread_mutex_t      = pointer;
   pthread_mutexattr_t 	= pointer;
   pthread_cond_t       = pointer;
   pthread_condattr_t 	= pointer;
   pthread_key_t        = cint;
   pthread_rwlock_t 	= pointer;
   pthread_rwlockattr_t = pointer;

   sem_t		= pointer;

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


const
  _PTHREAD_MUTEX_DEFAULT     = _PTHREAD_MUTEX_ERRORCHECK;
  _MUTEX_TYPE_FAST          = _PTHREAD_MUTEX_NORMAL;
  _MUTEX_TYPE_COUNTING_FAST = _PTHREAD_MUTEX_RECURSIVE;

  _PTHREAD_KEYS_MAX              = 256;
  _PTHREAD_STACK_MIN             = 1024;

   { System limits, POSIX value in parentheses, used for buffer and stack allocation }
    ARG_MAX  = 256*1024;   {4096}  { Maximum number of argument size     }
    NAME_MAX = 255;        {14}    { Maximum number of bytes in filename }
    PATH_MAX = 1024;       {255}   { Maximum number of bytes in pathname }

    SYS_NMLN = 32;              {BSD utsname struct limit}

    SIG_MAXSIG      = 128;	// highest signal version
    wordsinsigset   = 4;		// words in sigset_t

{
  $Log$
  Revision 1.6  2004-09-09 20:29:06  jonas
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

  Revision 1.5  2004/08/25 21:42:11  jonas
    * fixed pthread type definitions for darwin and made them generic

  Revision 1.4  2004/01/22 13:46:14  marco
  bsd

  Revision 1.5  2004/01/04 20:08:45  jonas
    * moved SIG_MAXSIG and wordsinsigset constants from bunxtype.inc to
      ptypes.inc (already there for Darwin)

  Revision 1.4  2004/01/04 01:11:28  marco
   * a new qod port of the freebsd rtl. To be refined in the coming days.

  Revision 1.3  2003/01/17 22:13:47  marco
   * some updates

  Revision 1.5  2003/01/03 13:11:32  marco
   * split into ptypes and ctypes


}
