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
   FD_MAXFDSET     = 1024;
   BITSINWORD      = 8*sizeof(longint);
   wordsinsigset   = SIG_MAXSIG DIV BITSINWORD;         // words in sigset_t
   wordsinfdset    = FD_MAXFDSET DIV BITSINWORD;        // words in fdset_t
   ln2bitsinword   = 5;         { 32bit : ln(32)/ln(2)=5 }
   ln2bitmask      = 1 shl ln2bitsinword - 1;

    UTSNAME_LENGTH = 256;   { 256 + 1 in pchar format }
    UTSNAME_NODENAME_LENGTH = 256;

    ST_FSTYPSZ = 16;           {* array size for file system type name *}

TYPE
    blksize_t = longint;
    blkcnt_t = longint;

   { file characteristics services }
   stat = packed record { verify the alignment of the members }
     st_dev : dev_t;
     st_pad1 : array[1..3] of longint;  { reserve for dev expansion }
     st_ino : ino_t;
     st_mode : mode_t;
     st_nlink : nlink_t;
     st_uid : uid_t;
     st_gid : gid_t;
     st_rdev : dev_t;
     st_pad2 : array[1..2] of longint;
     st_size : off_t;
     st_pad3 : longint;   {* reserve pad for future off_t expansion *}
     st_atime : time_t;
     st_atimens : longint;    { access time nanosecond field         }
     st_mtime : time_t;
     st_mtimens : longint;    { modification time nanosecond field   }
     st_ctime : time_t;
     st_ctimens : longint;    { modification time nanosecond field   }
     st_blksize : blksize_t;
     st_blocks : blkcnt_t;
     st_fstype : array[0..ST_FSTYPSZ-1] of char;
     st_pad4 : array[1..8] of longint;
   end;
   TStat             = Stat;
   PStat             = ^Stat;

   flock    = record
                {$ifdef 64bitfs}
                l_start : off64_t;      { starting offset }
                l_len   : off64_t;      { len = 0 means until end of file }
                {$else}
                l_start : off_t;        { starting offset }
                l_len   : off_t;        { len = 0 means until end of file }
                {$endif}
                l_pid   : pid_t;        { lock owner }
                l_type  : cshort;       { lock type: read/write, etc. }
                l_whence: cshort;       { type of l_start }
                end;
   TFlock   = flock;
   pFlock   = ^flock;

   TFDSet    = array[0..(FD_MAXFDSET div 32)-1] of Cardinal;
   pFDSet    = ^TFDSet;

   timezone = packed record
     tz_minuteswest,tz_dsttime:cint;
   end;
   ptimezone =^timezone;
   TTimeZone = timezone;

   { system information services }
   utsname = packed record   { don't forget to verify the alignment }
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
   dirent = packed record    { directory entry record - verify alignment }
     d_ino : ino_t;                  {* "inode number" of entry *}
     d_off : off_t;                  {* offset of disk directory entry *}
     d_reclen : word;                {* length of this record *}
     d_name : array[0..255] of char; { name of file            }
   end;


   pdir = ^dir;
   dir = packed record
    d_fd : cint;               {* file descriptor *}
    d_loc : cint;              {* offset in block *}
    d_size : cint;             {* amount of valid data *}
    d_buf : pchar;             { directory block   }
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
    O_NONBLOCK  =  $80; { Non-blocking I/O.                 }


    { mode_t possible values                                 }
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
    WNOHANG   = $40;            { don't block waiting               }
    WUNTRACED = $04;            { report status of stopped children }

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

CONST
 { Constansts for MMAP }
  MAP_PRIVATE   =2;
  MAP_ANONYMOUS =$1000;



{$i signal.inc}


{
  $Log: ostypes.inc,v $
  Revision 1.4  2005/02/14 17:13:31  peter
    * truncate log

  Revision 1.3  2005/02/14 16:32:41  peter
    * solaris updates

  Revision 1.2  2005/02/13 22:13:20  peter
    * get solaris back in shape

  Revision 1.1  2005/02/10 17:30:54  peter
    * renamed to solaris

}
