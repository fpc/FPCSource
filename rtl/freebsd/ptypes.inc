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

{$ifdef CPU64} 
    clock_t  = cuint32;		// 32-bit even on Athlon64
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
    tv_sec   : time_t;			// should be time_t, bug compability
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
    ARG_MAX  = 65536;   {4096}  { Maximum number of argument size     }
    NAME_MAX = 255;     {14}    { Maximum number of bytes in filename }
    PATH_MAX = 1024;    {255}   { Maximum number of bytes in pathname }

    SYS_NMLN = 32;              {BSD utsname struct limit, kernel mode}

    SIG_MAXSIG      = 128;	// highest signal version
//   wordsinsigset   = 4;		// words in sigset_t


{
  $Log$
  Revision 1.14  2004-09-09 20:29:06  jonas
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

  Revision 1.13  2004/08/25 21:42:11  jonas
    * fixed pthread type definitions for darwin and made them generic

  Revision 1.12  2004/05/18 19:33:45  marco
   * left a comment unmatched

  Revision 1.11  2004/05/17 19:18:38  marco
   * first x86-64 changes

  Revision 1.10  2004/03/04 22:15:16  marco
   * UnixType changes. Please report problems to me.

  Revision 1.9  2004/01/04 20:08:45  jonas
    * moved SIG_MAXSIG and wordsinsigset constants from bunxtype.inc to
      ptypes.inc (already there for Darwin)

  Revision 1.8  2003/12/30 12:32:30  marco
  *** empty log message ***

  Revision 1.7  2003/09/27 13:45:58  peter
    * fpnanosleep exported in baseunix
    * fpnanosleep has pointer arguments to be C compliant

  Revision 1.6  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.5  2003/01/03 13:11:32  marco
   * split into ptypes and ctypes


}
