{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    Types and structures for baseunix unit, also used in system.

    This file implements all the types/constants which must
    be defined to port FPC to a new POSIX compliant OS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{***********************************************************************}
{                         POSIX STRUCTURES                              }
{***********************************************************************}

{$ifdef FPC_IS_SYSTEM}
  {$i ptypes.inc}
{$ENDIF}

{$ifdef cpupowerpc}
  {$ifdef netbsd}
    {$define netbsdpowerpc}
  {$endif}
{$endif}

//      CONST SYS_NMLN=65;

// Can't find these two in Posix and in FreeBSD
//CONST
//    _UTSNAME_LENGTH = ;
//    _UTSNAME_NODENAME_LENGTH = ;

CONST                           // OS specific parameters for general<fd,sig>set behaviour
   BITSINWORD      = 8*sizeof(longint);
   FD_MAXFDSET     = 1024;
   ln2bitsinword   = 5;         { 32bit : ln(32)/ln(2)=5 }
   ln2bitmask      = 1 shl ln2bitsinword - 1;
   wordsinfdset    = FD_MAXFDSET DIV BITSINWORD;        // words in fdset_t
   wordsinsigset   = SIG_MAXSIG  DIV BITSINWORD;

TYPE
   { system information services }
   utsname = record
              sysname : Array[0..SYS_NMLN-1] OF Char;   // Name of this OS
              nodename: Array[0..SYS_NMLN-1] OF Char;   // Name of this network node.
              release : Array[0..SYS_NMLN-1] OF Char;   // Release level.
              version : Array[0..SYS_NMLN-1] OF Char;   // Version level.
              machine : Array[0..SYS_NMLN-1] OF Char;   // Hardware type.
             end;
  TUtsName= utsname;
  pUtsName= ^utsname;

  { file characteristics services }
   stat    = record { the types are real}
        st_dev        : dev_t;             // inode's device
        st_ino        : ino_t;             // inode's number
        st_mode       : mode_t;            // inode protection mode
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
{$ifdef netbsdPowerpc}
        st_padd1              : cint;
{$endif}
        st_size       : off_t;             // file size, in bytes
        st_blocks     : cint64;            // blocks allocated for file
        st_blksize    : cuint32;           // optimal blocksize for I/O
        st_flags      : cuint32;           // user defined flags for file
        st_gen        : cuint32;           // file generation number
{$ifdef netbsdPowerpc}
        st_padd2              : cint;
{$endif}
{$ifndef NetBSD}
        st_lspare     : cint32;
{$endif}
        st_qspare     : array[0..1] Of cint64;
   end;
   TStat = stat;
   pStat = ^stat;

  { directory services }
   dirent  = record
        d_fileno      : cuint32;                        // file number of entry
        d_reclen      : cuint16;                        // length of this record
        d_type        : cuint8;                         // file type, see below
        d_namlen      : cuint8;                         // length of string in d_name
        d_name        : array[0..(255 + 1)-1] of char;  // name must be no longer than this
   end;
   TDirent = dirent;
   pDirent = ^dirent;

   dir     = packed record
        dd_fd     : cint;         // file descriptor associated with directory
        dd_loc    : clong;        // offset in current buffer
        dd_size   : clong;        // amount of data returned by getdirentries
        dd_buf    : pchar;        // data buffer
        dd_len    : cint;         // size of data buffer
{$ifdef netbsdpowerpc}
        dd_pad1   : cint;
        dd_seek   : cint64;        // magic cookie returned by getdirentries
{$else}
        dd_seek   : clong;        // magic cookie returned by getdirentries
{$endif}
        dd_rewind : clong;        // magic cookie for rewinding
        dd_flags  : cint;         // flags for readdir
   end;
   TDir    = dir;
   pDir    = ^dir;

   utimbuf  = record
                actime  : time_t;
                modtime : time_t;
                end;
   TUtimBuf = utimbuf;
   putimbuf = ^utimbuf;

   flock    = record
                l_start : off_t;        { starting offset }
                l_len   : off_t;        { len = 0 means until end of file }
                l_pid   : pid_t;        { lock owner }
                l_type  : cshort;       { lock type: read/write, etc. }
                l_whence: cshort;       { type of l_start }
                end;
   TFlock   = flock;
   pFlock   = ^flock;

 tms = packed record
         tms_utime  : clock_t;  { User CPU time }
         tms_stime  : clock_t;  { System CPU time }
         tms_cutime : clock_t;  { User CPU time of terminated child procs }
         tms_cstime : clock_t;  { System CPU time of terminated child procs }
         end;
 TTms= tms;
 pTms= ^tms;

 TFDSet    = ARRAY[0..(FD_MAXFDSET div 32)-1] of Cardinal;
 pFDSet    = ^TFDSet;

{***********************************************************************}
{                  POSIX CONSTANT ROUTINE DEFINITIONS                   }
{***********************************************************************}
CONST
    { access routine - these maybe OR'ed together }
    F_OK        =     0;        { test for existence of file }
    R_OK        =     4;        { test for read permission on file }
    W_OK        =     2;        { test for write permission on file }
    X_OK        =     1;        { test for execute or search permission }
    { seek routine }
    SEEK_SET    =     0;        { seek from beginning of file }
    SEEK_CUR    =     1;        { seek from current position  }
    SEEK_END    =     2;        { seek from end of file       }
    { open routine                                 }
    { File access modes for `open' and `fcntl'.    }
    O_RDONLY    =     0;        { Open read-only.  }
    O_WRONLY    =     1;        { Open write-only. }
    O_RDWR      =     2;        { Open read/write. }
    { Bits OR'd into the second argument to open.  }
    O_CREAT     =  $200;        { Create file if it doesn't exist.  }
    O_EXCL      =  $800;        { Fail if file already exists.      }
    O_TRUNC     =  $400;        { Truncate file to zero length.     }
    O_NOCTTY    = $8000;        { Don't assign a controlling terminal. }
    { File status flags for `open' and `fcntl'.  }
    O_APPEND    =     8;        { Writes append to the file.        }
    O_NONBLOCK  =     4;        { Non-blocking I/O.                 }

    { mode_t possible values                                 }
    S_IRUSR =  %0100000000;     { Read permission for owner   }
    S_IWUSR =  %0010000000;     { Write permission for owner  }
    S_IXUSR =  %0001000000;     { Exec  permission for owner  }
    S_IRGRP =  %0000100000;     { Read permission for group   }
    S_IWGRP =  %0000010000;     { Write permission for group  }
    S_IXGRP =  %0000001000;     { Exec permission for group   }
    S_IROTH =  %0000000100;     { Read permission for world   }
    S_IWOTH =  %0000000010;     { Write permission for world  }
    S_IXOTH =  %0000000001;     { Exec permission for world   }

    { Used for waitpid }
    WNOHANG   =          1;     { don't block waiting               }
    WUNTRACED =          2;     { report status of stopped children }


  { For File control mechanism }
  F_GetFd  = 1;
  F_SetFd  = 2;
  F_GetFl  = 3;
  F_SetFl  = 4;
  F_GetLk  = 5;
  F_SetLk  = 6;
  F_SetLkW = 7;
  F_SetOwn = 8;
  F_GetOwn = 9;


type
  timezone = packed record
    tz_minuteswest,
    tz_dsttime  : cint;
  end;
  ptimezone =^timezone;
  TTimeZone = timezone;

  rusage = packed record
        ru_utime    : timeval;          { user time used }
        ru_stime    : timeval;          { system time used }
        ru_maxrss   : clong;            { max resident set size }
        ru_ixrss    : clong;            { integral shared memory size }
        ru_idrss    : clong;            { integral unshared data " }
        ru_isrss    : clong;            { integral unshared stack " }
        ru_minflt   : clong;            { page reclaims }
        ru_majflt   : clong;            { page faults }
        ru_nswap    : clong;            { swaps }
        ru_inblock  : clong;            { block input operations }
        ru_oublock  : clong;            { block output operations }
        ru_msgsnd   : clong;            { messages sent }
        ru_msgrcv   : clong;            { messages received }
        ru_nsignals : clong;            { signals received }
        ru_nvcsw    : clong;            { voluntary context switches }
        ru_nivcsw   : clong;            { involuntary " }
        end;
// #define      ru_last         ru_nivcsw
// #define      ru_first        ru_ixrss

{ auto generated by a c prog, statmacr.c}

Const
  S_IFMT  = 61440;
  S_IFIFO = 4096;
  S_IFCHR = 8192;
  S_IFDIR = 16384;
  S_IFBLK = 24576;
  S_IFREG = 32768;
  S_IFLNK = 40960;
  S_IFSOCK= 49152;
  S_IFWHT = 57344;
  S_ISVTX = 512;

{
 * Resource limits from FreeBSD5. To be checked for the others.
}
  RLIMIT_CPU      = 0;              { cpu time in milliseconds }
  RLIMIT_FSIZE    = 1;              { maximum file size }
  RLIMIT_DATA     = 2;              { data size }
  RLIMIT_STACK    = 3;              { stack size }
  RLIMIT_CORE     = 4;              { core file size }
  RLIMIT_RSS      = 5;              { resident set size }
  RLIMIT_MEMLOCK  = 6;              { locked-in-memory address space }
  RLIMIT_NPROC    = 7;              { number of processes }
  RLIMIT_NOFILE   = 8;              { number of open files }
{$IFDEF FreeBSD}
  RLIMIT_SBSIZE   = 9;              { maximum size of all socket buffers }
  RLIMIT_VMEM     =10;              { virtual process size (inclusive of mmap) }
  RLIMIT_AS       = RLIMIT_VMEM;    
{$ELSE}
  RLIMIT_AS       = 5;    	    { address space= resident set size}
{$ENDIF}

  {$ifdef FreeBSD}
  RLIM_NLIMITS    =11;              { number of resource limits }
  {$endif}

  {$ifdef Darwin}   // OS X 10.3
  RLIM_NLIMITS    =9;              { number of resource limits }
  {$endif}

Type 
        TRLimit  = record
                     rlim_cur,               { current (soft) limit }
          	     rlim_max : TRLim;     { maximum value for rlim_cur }
		    end;	
        PRLimit  = ^TRLimit;
      
  iovec = record
            iov_base : pointer;
	    iov_len  : size_t;
	   end;
  tiovec=iovec;
  piovec=^tiovec;		
 
CONST
 { Constansts for MMAP }
{$ifdef FPC_IS_SYSTEM}
  MAP_PRIVATE   =2;
{$endif}
  MAP_ANONYMOUS =$1000;

const
  POLLIN      = $0001;
  POLLPRI     = $0002;
  POLLOUT     = $0004;
  POLLERR     = $0008;
  POLLHUP     = $0010;
  POLLNVAL    = $0020;

  { XOpen, XPG 4.2 }
  POLLRDNORM  = $0040;
  POLLRDBAND  = $0080;
  POLLWRNORM  = POLLOUT;
  POLLWRBAND  = $0100;

type
  pollfd = record
    fd: cint;
    events: cshort;
    revents: cshort;
  end;
  tpollfd = pollfd;
  ppollfd = ^pollfd;

    {*************************************************************************}
    {                               SIGNALS                                   }
    {*************************************************************************}

{$i signal.inc}


