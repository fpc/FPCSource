{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    Types and structures for the BaseUnix unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ***********************************************************************}

{***********************************************************************}
{                         Base Unix Structures                          }
{***********************************************************************}

{$IFDEF FPC_IS_SYSTEM}
  {$i ptypes.inc}
{$ENDIF}

{ note: all constants/types are taken from the case that the headers are parsed
  with:
    -D_LARGE_FILES
    -U_POSIX_SOURCE
    -U_ANSI_C_SOURCE
}

CONST
   FD_MAXFDSET     = 65534;
   BITSINWORD      = 8*sizeof(clong);
   wordsinsigset   = (SIG_MAXSIG+1+BITSINWORD-1) DIV BITSINWORD;         // words in sigset_t
   wordsinfdset    = (FD_MAXFDSET+1+BITSINWORD-1) DIV BITSINWORD;        // words in fdset_t
{$ifdef cpu64}
   ln2bitsinword   = 6;         { 64bit : ln(64)/ln(2)=5 }
{$else}
   ln2bitsinword   = 5;         { 32bit : ln(32)/ln(2)=5 }
{$endif}
   ln2bitmask      = 1 shl ln2bitsinword - 1;

    UTSNAME_LENGTH = 31;
    UTSNAME_NODENAME_LENGTH = 31;

TYPE
    blksize_t = {$ifdef cpu64}clong{$else}cint{$endif};
    blkcnt_t = {$ifdef cpu64}clong{$else}cint{$endif};

   { file characteristics services }
   stat = record
     st_dev : dev_t;
     st_ino : ino_t;
     st_mode : mode_t;
     st_nlink : nlink_t;
     st_flag : cushort;
     st_uid : uid_t;
     st_gid : gid_t;
     st_rdev : dev_t;
     st_ssize : cint;
     st_atime : time_t;
     st_atimens : cint;    { access time nanosecond field         }

     st_mtime : time_t;
     st_mtimens : cint;    { modification time nanosecond field   }
     st_ctime : time_t;
     st_ctimens : cint;    { modification time nanosecond field   }
     st_blksize : blksize_t;
     st_blocks : blkcnt_t;
     st_vfstype: cint;     { Type of fs (see vnode.h) }
     st_vfs    : cuint;    { Vfs number }
     st_type   : cuint;    { Vnode type }
     st_gen    : cuint;    { Inode generation number }
     st_reserved : array [0..9] of cuint;
     st_size   : off_t;    { 64 bit file size in bytes }
   end;
   TStat             = Stat;
   PStat             = ^Stat;

   flock    = record
                l_type  : cshort;       { lock type: read/write, etc. }
                l_whence: cshort;       { type of l_start }
                l_sysid : cuint;
                l_pid   : pid_t;        { lock owner }
                l_vfs   : cint;
                l_start : off_t;
                l_len   : off_t;
                end;
   TFlock   = flock;
   pFlock   = ^flock;

   TFDSetEl = culong; 
   TFDSet    = array[0..wordsinfdset-1] of TFDSetEl;
   pFDSet    = ^TFDSet;

   timezone = record
     tz_minuteswest,tz_dsttime:cint;
   end;
   ptimezone =^timezone;
   TTimeZone = timezone;

   { system information services }
   utsname = record
     sysname : array[0..UTSNAME_LENGTH] of char;
     nodename : array[0..UTSNAME_LENGTH] of char;
     release : array[0..UTSNAME_LENGTH] of char;
     version : array[0..UTSNAME_LENGTH] of char;
     machine : array[0..UTSNAME_LENGTH] of char;
   end;

   UTimBuf   = Record
                 actime  : time_t;
                 modtime : time_t;
                end;
   TUtimBuf  = UtimBuf;
   pUtimBuf  = ^UtimBuf;

   { directory services }
   pdirent = ^dirent;
   { actually dirent64 }
   dirent = record    { directory entry record }
     case integer of
       1 : (
             d_off : cuint64;                  {* offset of disk directory entry *}
             d_ino : ino64_t;                  {* "inode number" of entry *}
             d_reclen : cushort;             {* length of this record *}
             d_namelen : cushort;
             d_name : array[0..255] of char; { name of file            }
            );
       { overlay with alias }
       2 : (
            dummy    : cuint64;
            d_fileno : ino64_t;
           ); 
   end;


   pdir = ^dir;
   dir = record
    case integer of
      1 : (
           d_fd : cint;               {* file descriptor *}
           d_loc : cint;              {* offset in block *}
           d_size : cint;             {* amount of valid data *}
           d_buf : pchar;             { directory block   }
          );
      { overlay for posix compatibility }
      2 : (
           dd_fd : cint;               {* file descriptor *}
           dd_loc : cint;              {* offset in block *}
           dd_size : cint;             {* amount of valid data *}
           dd_buf : pchar;             { directory block   }
          );
   end;



{***********************************************************************}
{                  POSIX CONSTANT ROUTINE DEFINITIONS                   }
{***********************************************************************}
CONST
    { access routine - these maybe OR'ed together }
    F_OK        = 0;   { test for existence of file }
    R_OK        = 4;   { test for read permission on file }
    W_OK        = 2;   { test for write permission on file }
    X_OK        = 1;   { test for execute or search permission }
    { seek routine }
    SEEK_SET    = 0;    { seek from beginning of file }
    SEEK_CUR    = 1;    { seek from current position  }
    SEEK_END    = 2;    { seek from end of file       }
    { open routine                                 }
    { File access modes for `open' and `fcntl'.    }
    O_RDONLY    = 0;    { Open read-only.  }
    O_WRONLY    = 1;    { Open write-only. }
    O_RDWR      = 2;    { Open read/write. }
    { Bits OR'd into the second argument to open.  }
    O_CREAT     = $100; { Create file if it doesn't exist.  }
    O_EXCL      = $400; { Fail if file already ??????.      }
    O_TRUNC     = $200; { Truncate file to zero length.     }
    O_NOCTTY    = $800; { Don't assign a controlling terminal. }
    { File status flags for `open' and `fcntl'.  }
    O_APPEND    =  $08; { Writes append to the file.        }
    O_NONBLOCK  =  $04; { Non-blocking I/O.                 }


    { mode_t possible values                                 }
    S_ISUID = &4000;          { set user id on execution    }
    S_ISGID = &2000;          { set group id on execution   }
    S_IRUSR = $100;           { Read permission for owner   }
    S_IWUSR = $080;           { Write permission for owner  }
    S_IXUSR = $040;           { Exec  permission for owner  }
    S_IRGRP = $020;           { Read permission for group   }
    S_IWGRP = $010;           { Write permission for group  }
    S_IXGRP = $008;           { Exec permission for group   }
    S_IROTH = $004;           { Read permission for world   }
    S_IWOTH = $002;           { Write permission for world  }
    S_IXOTH = $001;           { Exec permission for world   }

    { Used for waitpid }
    WNOHANG   = $01;            { don't block waiting               }
    WUNTRACED = $02;            { report status of stopped children }

Const
  S_IFMT  = $F000;
  S_IFIFO = $1000;
  S_IFCHR = $2000;
  S_IFDIR = $4000;
  S_IFBLK = $6000;
  S_IFREG = $8000;
  S_IFLNK = $A000;
  S_IFSOCK= $C000;
//  S_IFWHT = 57344;
  S_ISVTX = $200;

  { For File control mechanism }
  F_GetFd  = 1;
  F_SetFd  = 2;
  F_GetFl  = 3;
  F_SetFl  = 4;
  F_GetLk  = 5;
  F_SetLk  = 6;
  F_SetLkW = 7;
  F_SetOwn = 9;
  F_GetOwn = 8;

Const
 { Constansts for MMAP }
 {$ifdef FPC_IS_SYSTEM}
  MAP_PRIVATE   =2;
 {$endif}
  MAP_ANONYMOUS =$10;

type
  rlim_t = cULong;
  PRLimit = ^TRLimit;
  TRLimit = record
    rlim_cur : rlim_t;
    rlim_max : rlim_t;
  end;

{$i signal.inc}

  iovec = record
            iov_base : pointer;
	    iov_len  : size_t;
	   end;
  tiovec=iovec;
  piovec=^tiovec;		

 tms = packed record
         tms_utime  : clock_t;  { User CPU time }
         tms_stime  : clock_t;  { System CPU time }
         tms_cutime : clock_t;  { User CPU time of terminated child procs }
         tms_cstime : clock_t;  { System CPU time of terminated child procs }
         end;
 TTms= tms;
 pTms= ^tms;

const
  POLLIN      = $0001;
  POLLPRI     = $0004;
  POLLOUT     = $0002;
  POLLERR     = $4000;
  POLLHUP     = $2000;
  POLLNVAL    = $8000;

  { XOpen, XPG 4.2 }
  POLLRDNORM  = $0010;
  POLLRDBAND  = $0020;
  POLLWRNORM  = POLLOUT;
  POLLWRBAND  = $0040;

type
  pollfd = record
    { int on 64 bit = long on 32 bit, but this is how the C header does it }
{$ifdef cpu64}
    fd: cint;
{$else}
    fd: clong;
{$endif}
    events: cshort;
    revents: cshort;
  end;
  tpollfd = pollfd;
  ppollfd = ^pollfd;


