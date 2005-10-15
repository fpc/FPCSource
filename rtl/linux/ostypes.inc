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


CONST
//  SYS_NMLM                 = 65;
    UTSNAME_LENGTH           = SYS_NMLN;
    UTSNAME_NODENAME_LENGTH  = UTSNAME_LENGTH;
    {$ifdef usedomain}
    UTSNAME_DOMAIN_LENGTH    = UTSNAME_LENGTH;
    {$endif}

   FD_MAXFDSET     = 1024;
   BITSINWORD      = 8*sizeof(cuLong);
   wordsinsigset   = SIG_MAXSIG DIV BITSINWORD;         // words in sigset_t
   wordsinfdset    = FD_MAXFDSET DIV BITSINWORD;        // words in fdset_t
   {$ifdef cpu32}
   ln2bitsinword   = 5;                                 { 32bit : ln(32)/ln(2)=5 }
   {$else cpu32}
   {$ifdef cpu64}
   ln2bitsinword   = 6;                                 { 64bit : ln(64)/ln(2)=6 }
   {$endif cpu64}
   {$endif cpu32}
   ln2bitmask      = 1 shl ln2bitsinword - 1;


TYPE
   Blksize_t  = cuint;
   Blkcnt_t   = cuint;
   Ino64_t    = cint64;
   Off64_t    = cint64;

   TBlkSize   = BlkSize_t;
   PBlkSize   = ^BlkSize_t;
   TBlkCnt    = Blkcnt_t;
   PBlkCnt    = ^Blkcnt_t;
   TIno64     = Ino64_t;
   PIno64     = ^Ino64_t;
   TOff64     = Off64_t;
   POff64     = ^Off64_t;

   { system information services }
   UtsName   = Record
                Sysname : Array[0..UTSNAME_LENGTH -1] OF Char;   // Name of this OS
                Nodename: Array[0..UTSNAME_NODENAME_LENGTH-1] OF Char;   // Name of this network node.
                Release : Array[0..UTSNAME_LENGTH -1] OF Char;   // Release level.
                Version : Array[0..UTSNAME_LENGTH -1] OF Char;   // Version level.
                Machine : Array[0..UTSNAME_LENGTH -1] OF Char;   // Hardware type.
               {$ifdef usedomain}
                Domain  : array[0..UTSNAME_DOMAIN_LENGTH-1] of char;  // Linux addition "Domain"
               {$endif}
               end;
  TUtsName   = UtsName;
  PUtsName   = TUtsName;


{ Definition of (kernel) stat type }
{ see kernel/include/asm-<cpu>/stat.h, include/linux/types.h and }
{ include /include/asm-<cpu>/posix-types.h                       }

const
  _STAT_VER_LINUX_OLD = 1;
  _STAT_VER_KERNEL = 1;
  _STAT_VER_SVR4 = 2;
  _STAT_VER_LINUX = 3;
  _STAT_VER = _STAT_VER_LINUX;

type
{$i stat.inc}

  TStat      = Stat;
  PStat      = ^Stat;


{$ifdef notused} // 64-bit support needs some work still :-)
  { file characteristics services }
   stat64    = record
        st_dev        : dev_t;             // inode's device
        pad1          : cushort;
        {$ifdef 64bitfs}                   // ??
        __st_ino      : ino_t;
        {$else}
        st_ino        : ino_t;             // inode's number
        {$endif}
        st_mode       : mode_t;            // inode protection mode
        st_nlink      : nlink_t;           // number of hard links
        st_uid        : uid_t;             // user ID of the file's owner
        st_gid        : gid_t;             // group ID of the file's group
        st_rdev       : dev_t;             // device type
        pad2          : cushort;
        {$ifdef 64bitfs}
        st_size       : off64_t;            // file size, in bytes
        {$else}
        st_size       : off_t;             // file size, in bytes
        {$endif}
        st_blksize    : blksize_t;           // optimal blocksize for I/O
        {$ifdef 64bitfs}
        st_blocks     : blkcnt64_t;            // blocks allocated for file
        {$else}
        st_blocks     : blkcnt_t;            // blocks allocated for file
        {$endif}
        st_atime      : time_t;            // time of last access
        unused1       : culong;
        st_mtime      : time_t;            // time of last data modification
        unused2       : culong;
        st_ctime      : time_t;            // time of last file status change
        unused3       : culong;
        {$ifdef 64bitfs}
        st_ino        : ino64_t
        {$else}
        unused4       : culong;
        unused5       : culong;
        {$endif}
   end;
{$endif}

  { directory services }

  Dirent     = packed record
               {$ifndef 64bitfs}
                d_fileno      : ino_t;                          // file number of entry
                d_off         : off_t;
                {$else}
                d_fileno      : ino64_t;                        // file number of entry
                d_off         : off64_t;
               {$endif}
                d_reclen      : cushort;                        // length of string in d_name
               {$ifdef Uselibc} // Libc different from kernel record!
                d_type        : cuchar;                         // file type, see below
               {$endif}
                d_name        : array[0..(255 + 1)-1] of char;  // name must be no longer than this
               end;
  TDirent    = Dirent;
  pDirent    = ^Dirent;

{$ifdef oldreaddir}
           { Still old one. This is a userland struct}

   Dir       = record
                dd_fd     : integer;
                dd_loc    : longint;
                dd_size   : integer;
                dd_buf    : pdirent;
                {The following are used in libc, but NOT in the linux kernel sources ??}
                dd_nextoff: longint;
                dd_max : integer; {size of buf. Irrelevant, as buf is of type dirent}
                dd_lock   : pointer;
               end;
{$else}
        // new libc one. NOTE that off_t must be real, so 64-bit ifdef
        // 64bitsfs
   Dir       = Record   // packing doesn't matter. This is a userland struct.
                fd      : cint;
                data    : pchar;
                allocation: size_t;
                _size     : size_t;
                offset    : size_t;
                filepos   : off_t;
                end;
{$endif}

   TDir      = Dir;
   pDir      = ^Dir;


   UTimBuf   = Record
                 actime  : time_t;
                 modtime : time_t;
                end;

   TUtimBuf  = UtimBuf;
   pUtimBuf  = ^UtimBuf;

   FLock     = Record
                l_type  : cshort;       { lock type: read/write, etc. }
                l_whence: cshort;       { type of l_start }
                {$ifdef 64bitfs}
                l_start : off64_t;      { starting offset }
                l_len   : off64_t;      { len = 0 means until end of file }
                {$else}
                l_start : off_t;        { starting offset }
                l_len   : off_t;        { len = 0 means until end of file }
                {$endif}
                l_pid   : pid_t;        { lock owner }
               End;

   tms       = packed Record
                tms_utime  : clock_t;   { User CPU time }
                tms_stime  : clock_t;   { System CPU time }
                tms_cutime : clock_t;   { User CPU time of terminated child procs }
                tms_cstime : clock_t;   { System CPU time of terminated child procs }
               end;
   TTms      = tms;
   PTms      = ^tms;

 TFDSet    = ARRAY[0..(FD_MAXFDSET div 32)-1] of Cardinal;
 pFDSet    = ^TFDSet;

  timezone = packed record
    tz_minuteswest,tz_dsttime:cint;
  end;
  ptimezone =^timezone;
  TTimeZone = timezone;

{***********************************************************************}
{                  POSIX CONSTANT ROUTINE DEFINITIONS                   }
{***********************************************************************}
CONST
    { access routine - these maybe OR'ed together }
    F_OK      =          0;        { test for existence of file }
    R_OK      =          4;        { test for read permission on file }
    W_OK      =          2;        { test for write permission on file }
    X_OK      =          1;        { test for execute or search permission }
    { seek routine }
    SEEK_SET  =          0;        { seek from beginning of file }
    SEEK_CUR  =          1;        { seek from current position  }
    SEEK_END  =          2;        { seek from end of file       }
    { open routine                                 }
    { File access modes for `open' and `fcntl'.    }
    O_RDONLY  =          0;        { Open read-only.  }
    O_WRONLY  =          1;        { Open write-only. }
    O_RDWR    =          2;        { Open read/write. }
{$ifdef sparc}
    O_APPEND  =          8;
    O_CREAT   =       $200;
    O_TRUNC   =       $400;
    O_EXCL    =       $800;
    O_SYNC    =      $2000;
    O_NONBLOCK =     $4000;
    O_NDELAY  =      O_NONBLOCK or 4;
    O_NOCTTY  =      $8000;
    O_DIRECTORY =   $10000;
    O_NOFOLLOW =    $20000;
    O_LARGEFILE =   $40000;
    O_DIRECT  =    $100000;
{$else sparc}
    O_CREAT   =        $40;
    O_EXCL    =        $80;
    O_NOCTTY  =       $100;
    O_TRUNC   =       $200;
    O_APPEND  =       $400;
    O_NONBLOCK =      $800;
    O_NDELAY  =     O_NONBLOCK;
    O_SYNC    =      $1000;
    O_DIRECT  =      $4000;
    O_LARGEFILE =    $8000;
    O_DIRECTORY =   $10000;
    O_NOFOLLOW =    $20000;
{$endif sparc}

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

  { File types }
  S_IFMT  = 61440; { type of file mask}
  S_IFIFO = 4096;  { named pipe (fifo)}
  S_IFCHR = 8192;  { character special}
  S_IFDIR = 16384; { directory }
  S_IFBLK = 24576; { block special}
  S_IFREG = 32768; { regular }
  S_IFLNK = 40960; { symbolic link }
  S_IFSOCK= 49152; { socket }

  { Constansts for MMAP }
  MAP_PRIVATE   =2;
  MAP_ANONYMOUS =$20;


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

    {*************************************************************************}
    {                               SIGNALS                                   }
    {*************************************************************************}

{$i signal.inc}

