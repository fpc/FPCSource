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

{$i ctypes.inc}
{$packrecords c}

type
    { the following type definitions are compiler dependant }
    { and system dependant                                  }

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
    ptime_t =  ^time_t;

    socklen_t= cuint32;
    TSocklen = socklen_t;
    pSocklen = ^socklen_t;

    timeval = record
        tv_sec: cint;
        tv_usec: cint;
    end;
    ptimeval = ^timeval;
    TTimeVal = timeval;

  timespec = packed record
    tv_sec   : time_t;
    tv_nsec  : clong;
  end;
  ptimespec= ^timespec;
  Ttimespec= timespec;



CONST
   { System limits, POSIX value in parentheses, used for buffer and stack allocation }
    ARG_MAX  = 65536;   {4096}  { Maximum number of argument size     }
    NAME_MAX = 255;     {14}    { Maximum number of bytes in filename }
    PATH_MAX = 1024;    {255}   { Maximum number of bytes in pathname }

    SYS_NMLN = 32;              {BSD utsname struct limit}

    SIG_MAXSIG      = 32;	// highest signal version

    MFSNAMELEN = 15;
    MNAMELEN   = 90;


type
  pthread_mutextype = (
    _PTHREAD_MUTEX_NORMAL    := 0,
    _PTHREAD_MUTEX_ERRORCHECK := 1,
    _PTHREAD_MUTEX_RECURSIVE := 2
  );


  const
     _PTHREAD_MUTEX_DEFAULT 		= _PTHREAD_MUTEX_NORMAL;
     _MUTEX_TYPE_FAST          = _PTHREAD_MUTEX_NORMAL;
     _MUTEX_TYPE_COUNTING_FAST = _PTHREAD_MUTEX_RECURSIVE;

     _PTHREAD_KEYS_MAX              = 128;
     _PTHREAD_STACK_MIN             = 8192;

  type
     fsid_t = record
          val: array[0..1] of cint32;
     end;

     tstatfs = record
          otype : cint16;
          oflags : cint16;
          bsize : cint32;
          iosize : cint32;
          blocks : cint32;
          bfree : cint32;
          bavail : cint32;
          files : cint32;
          ffree : cint32;
          fsid : fsid_t;
          fowner : uid_t;
          reserved1 : cint16;
          ftype : cint16;
          fflags : cint32;
          reserved2 : array[0..1] of cint32;
          fstypename : array[0..(MFSNAMELEN)-1] of char;
          mountpoint : array[0..(MNAMELEN)-1] of char;
          mntfromname : array[0..(MNAMELEN)-1] of char;
       end;
    pstatfs = ^tstatfs;

   pthread_t            = pointer;
   pthread_attr_t       = record sig: clong; opaque: array[0..36-1] of byte; end;
   pthread_mutex_t      = record sig: clong; opaque: array[0..40-1] of byte; end;
   pthread_mutexattr_t 	= record sig: clong; opaque: array[0..8-1] of byte; end;
   pthread_cond_t       = record sig: clong; opaque: array[0..24-1] of byte; end;
   pthread_condattr_t 	= record sig: clong; opaque: array[0..4-1] of byte; end;
   pthread_key_t        = clong;
   pthread_rwlock_t 		= record sig: clong; opaque: array[0..124-1] of byte; end;
   pthread_rwlockattr_t = record sig: clong; opaque: array[0..12-1] of byte; end;

   sem_t		= cint;

// for get/setpriority
Const
 { For getting/setting priority }
  Prio_Process = 0;
  Prio_PGrp    = 1;
  Prio_User    = 2;


{
  $Log$
  Revision 1.9  2004-11-14 12:24:33  marco
   * priority constants

  Revision 1.8  2004/09/09 20:29:06  jonas
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

  Revision 1.7  2004/08/25 21:42:11  jonas
    * fixed pthread type definitions for darwin and made them generic

  Revision 1.6  2004/03/04 22:28:08  marco
   * Darwin specific unixtype fixes.

  Revision 1.5  2004/03/01 19:50:40  jonas
    - removed wordsinfdset again, because now it's defined in a common file

  Revision 1.4  2004/02/22 23:18:24  jonas
    + added wordsinfdset constant

  Revision 1.3  2004/01/04 20:05:38  jonas
    * first working version of the Darwin/Mac OS X (for PowerPC) RTL
      Several non-essential units are still missing, but make cycle works

  Revision 1.2  2003/05/25 12:59:57  jonas
    * several fixes, addition of Mach trap numbers (thye are simply syscalls
      with a negative number)

  Revision 1.1  2002/09/08 09:00:54  jonas
    + initial revision, simply copied from FreeBSD

}
