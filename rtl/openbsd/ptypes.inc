{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001,2010 by Free Pascal development team

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

    dev_t    = cint32;          { used for device numbers      }
    TDev     = dev_t;
    pDev     = ^dev_t;

    gid_t    = cuint32;         { used for group IDs           }
    TGid     = gid_t;
    pGid     = ^gid_t;
    TIOCtlRequest = cuLong;

    ino_t    = cuint64;         { used for file serial numbers }
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
{$ifdef CPU64}
    size_t   = cuint64;
{$else}
    size_t   = cuint32;         { as definied in the C standard}
{$endif}
    TSize    = size_t;
    pSize    = ^size_t;
    pSize_t  = ^size_t;

    ssize_t  = clong;          { used by function for returning number of bytes }
    TsSize   = ssize_t;
    psSize   = ^ssize_t;

    uid_t    = cuint32;         { used for user ID type        }
    TUid     = Uid_t;
    pUid     = ^Uid_t;

    wint_t   = cint32;
    wchar_t  = cint32;
    pwchar_t = ^wchar_t;

    clock_t  = cint64;
    TClock   = clock_t;
    pClock   = ^clock_t;

    time_t   = cint64;           { used for returning the time  }
    // TTime    = time_t;    // Not allowed in system unit, -> unixtype
    pTime    = ^time_t;
    ptime_t  = ^time_t;

    socklen_t= cuint32;
    TSocklen = socklen_t;
    pSocklen = ^socklen_t;

    timeval = packed record
                tv_sec : time_t;
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

   pthread_t_rec		= record end;
   pthread_attr_t_rec		= record end;
//   See pmutext.inc
//   pthread_mutex_t_rec	= record end;
   pthread_mutexattr_t_rec	= record end;
   pthread_cond_t_rec		= record end;
   pthread_condattr_t_rec	= record end;
   pthread_rwlock_t_rec   	= record end;
   pthread_rwlockattr_t_rec	= record end;

   pthread_t            = ^pthread_t_rec;
   pthread_attr_t       = ^pthread_attr_t_rec;
   pthread_mutex_t      = {$i pmutext.inc}
   pthread_mutexattr_t  = ^pthread_mutexattr_t_rec;
   pthread_cond_t       = ^pthread_cond_t_rec;
   pthread_condattr_t   = ^pthread_condattr_t_rec;
   pthread_key_t        = cint;
   pthread_once_t_rec		= record
     state : cint;
     mutex : pthread_mutex_t;
   end;
   pthread_once_t	= pthread_once_t_rec;
   pthread_rwlock_t     = ^pthread_rwlock_t_rec;
   pthread_rwlockattr_t = ^pthread_rwlockattr_t_rec;

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
     MFSNAMELEN = 16;   // length of fs type name, including nul
     MNAMELEN   = 90;   // length of buffer for returned name

type
  fsid_t  = array[0..1] of cint32;

  ufs_args_rec		= record end;
  mfs_args_rec		= record end;
  nfs_args_rec		= record end;
  isofs_args_rec       	= record end;
  procfs_args_rec	= record end;
  msdosfs_args_rec	= record end;
  ntfs_args_rec		= record end;

  mountinfo = record
      case byte of
        0: (ufs_args:     ^ufs_args_rec);
        1: (mfs_args:     ^mfs_args_rec);
	2: (nfs_args:     ^nfs_args_rec);
	3: (isofs_args:   ^isofs_args_rec);
	4: (procfs_args:  ^procfs_args_rec);
	5: (msdosfs_args: ^msdosfs_args_rec);
	6: (ntfs_args:    ^ntfs_args_rec);
	7: (__align: array[0..159] of char); { 64-bit alignment and room to grow }
    end;

// kernel statfs from mount.h
  { new statfs structure with mount options and statvfs fields }
  TStatfs = record
    flags,                        { copy of mount flags }
    bsize,                        { filesystem block size }
    iosize            : cuint32;  { optimal transfer block size }

                                    { unit is f_bsize }
    blocks,                       { total data block in file system }
    bfree             : cuint64;  { free blocks in fs }
    bavail            : cint64;   { free blocks avail to non-superuser }

    files,                        { total file nodes in file system }
    ffree             : cuint64;  { free files nodes in fs }
    favail            : cint64;   { free file nodes avail to non-root }

    fsyncwrites,                  { count of sync writes since mount }
    fsyncreads,                   { count of sync reads since mount }
    fasyncwrites,                 { count of async writes since mount }
    fasyncreads       : cuint64;  { count of async reads since mount }

    fsid              : fsid_t;   { file system id }
    namemax           : cuint32;  { maximum filename length }
    owner             : tuid;     { user that mounted the fileystem }
    ctime             : cuint64;  { last mount [-u] time }

    fstypename : array[0..MFSNAMELEN-1] of char;      { fs type name }
    mntonname  : array[0..MNAMELEN-1] of char;        { directory on which mounted }
    mntfromname: array[0..MNAMELEN-1] of char;        { mounted file system }
    mntfromspec: array[0..MNAMELEN-1] of char;        { special for mount request }
    mount_info: mountinfo;                            { per-filesystem mount options }
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
  _PTHREAD_MUTEX_DEFAULT    = _PTHREAD_MUTEX_ERRORCHECK;
  _MUTEX_TYPE_FAST          = _PTHREAD_MUTEX_NORMAL;
  _MUTEX_TYPE_COUNTING_FAST = _PTHREAD_MUTEX_RECURSIVE;

  _PTHREAD_KEYS_MAX              = 256;
  _PTHREAD_STACK_MIN             = 1024;

   { System limits, POSIX value in parentheses, used for buffer and stack allocation }
    ARG_MAX  = 256*1024;   {4096}  { Maximum number of argument size     }
    NAME_MAX = 255;        {14}    { Maximum number of bytes in filename }
    PATH_MAX = 1024;       {255}   { Maximum number of bytes in pathname }

    SYS_NMLN = 256;                {BSD utsname struct limit}

    SIG_MAXSIG      = 33;      	   // highest signal version
    // set in ostypes.inc wordsinsigset   = 4;           // words in sigset_t

  { For getting/setting priority }
   Prio_Process = 0;
   Prio_PGrp    = 1;
   Prio_User    = 2;

{ OpenBSD 5.5 specific variants  }

{ file characteristics services }
type
   stat_55    = record { the types are real}
        st_mode       : mode_t;            // inode protection mode
        st_dev        : dev_t;             // inode's device
        st_ino        : ino_t;             // inode's number
        st_nlink      : nlink_t;           // number of hard links
        st_uid        : uid_t;             // user ID of the file's owner
        st_gid        : gid_t;             // group ID of the file's group
        st_rdev       : dev_t;             // device type
        st_atime      : time_t;            // time of last access
        st_atimensec  : clong;             // nsec of last access
        st_mtime      : time_t;            // time of last data modification
        st_mtimensec  : clong;             // nsec of last data modification
        st_ctime      : time_t;            // time of last file status change
        st_ctimensec  : clong;             // nsec of last file status change
        st_size       : off_t;             // file size, in bytes
        st_blocks     : cint64;            // blocks allocated for file
        st_blksize    : cuint32;           // optimal blocksize for I/O
        st_flags      : cuint32;           // user defined flags for file
        st_gen        : cuint32;           // file generation number
        st_birthtime  : time_t;            // File creation time
        st_birthtimensec : clong;          // nsec of file creation time
        st_qspare     : array[0..1] Of cint64;
   end;
   TStat_55 = stat_55;
   pStat_55 = ^stat_55;

  { directory services }
   dirent_55  = record
        d_fileno      : ino_t;                       
	d_off         : off_t;
        d_reclen      : cuint16;                        // length of this record
        d_type        : cuint8;                         // file type, see below
        d_namlen      : cuint8;                         // length of string in d_name
	d_padding     : array[1..4] of cuint8;
        d_name        : array[0..(255 + 1)-1] of char;  // name must be no longer than this
   end;
   TDirent_55 = dirent_55;
   pDirent_55 = ^dirent_55;

   dir_55     = record
        dd_fd     : cint;         // file descriptor associated with directory
        dd_loc    : clong;        // offset in current buffer
        dd_size   : clong;        // amount of data returned by getdirentries
        dd_buf    : pchar;        // data buffer
        dd_len    : cint;         // size of data buffer
	dd_curpos : off_t;
	dd_lock   : pointer;
	dd_rewind : clong;
   end;
   TDir_55    = dir_55;
   pDir_55    = ^dir_55;

