{
    $Id$
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
{                         Base Unix Structures				}
{***********************************************************************}

{$IFDEF FPC_IS_SYSTEM}
{$i ptypes.inc}
{$ENDIF}

CONST
    UTSNAME_LENGTH = 256;   { 256 + 1 in pchar format }
    UTSNAME_NODENAME_LENGTH = 256;

    ST_FSTYPSZ = 16;           {* array size for file system type name *}

TYPE
    { the following type definitions are compiler dependant }
    { and system dependant                                  }

    cint  = longint;    { minimum range is : 32-bit                   }
    cuint = cardinal;   { minimum range is : 32-bit                   }


    dev_t  = longint;     { used for device numbers      }
    gid_t  = longint;     { used for group IDs           }
    ino_t  = cardinal;    { used for file serial numbers }
    mode_t = cardinal;    { used for file attributes     }
    nlink_t  = cardinal;  { used for link counts         }
    off_t  = longint;     { used for file sizes          }
    pid_t  = longint;     { used as process identifier   }
    size_t = cardinal;    { as definied in the C standard }
    ssize_t = longint;    { used by function for returning number of bytes }
    uid_t = longint;      { used for user ID type        }
    time_t = longint;     { used for returning the time  }
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
   TStat	     = Stat;
   PStat	     = ^Stat;


   { system information services }
   utsname = packed record   { don't forget to verify the alignment }
     sysname : array[0..UTSNAME_LENGTH] of char;
     nodename : array[0..UTSNAME_LENGTH] of char;
     release : array[0..UTSNAME_LENGTH] of char;
     version : array[0..UTSNAME_LENGTH] of char;
     machine : array[0..UTSNAME_LENGTH] of char;
   end;


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
    O_RDONLY    = 0;	{ Open read-only.  }
    O_WRONLY    = 1;	{ Open write-only. }
    O_RDWR      = 2;	{ Open read/write. }
    { Bits OR'd into the second argument to open.  }
    O_CREAT     = $100; { Create file if it doesn't exist.  }
    O_EXCL      = $400;	{ Fail if file already ??????.      }
    O_TRUNC     = $200;	{ Truncate file to zero length.     }
    O_NOCTTY    = $800;	{ Don't assign a controlling terminal. }
    { File status flags for `open' and `fcntl'.  }
    O_APPEND    =  $08;	{ Writes append to the file.        }
    O_NONBLOCK	=  $80;	{ Non-blocking I/O.                 }


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

    { POSIX limits, used for buffer and stack allocation }
    ARG_MAX = 1048320;  { Maximum number of argument size     }
    { Maximum number of bytes in filename - depends on file system }
    { type, and no define possible, TRUE value is available through}
    { pathconf variable.                                           }
    NAME_MAX = 1024;    { put a big value , just in case      }
    PATH_MAX = 1024;    { Maximum number of bytes in pathname }


{$i signal.inc}


{
  $Log$
  Revision 1.1  2004-11-06 22:22:28  florian
    * some sunos stuff from 1.0.x merged
}
