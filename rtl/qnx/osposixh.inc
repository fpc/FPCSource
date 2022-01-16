{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Carl Eric Codere

    This file implements all the types/constants which are
    for the QNX RTP platform.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{***********************************************************************}
{                       POSIX TYPE DEFINITIONS                          }
{***********************************************************************}

type
    { the following type definitions are compiler dependant }
    { and system dependant                                  }

    cint  = longint;    { minimum range is : 32-bit                   }
    cuint = cardinal;   { minimum range is : 32-bit                   }


    dev_t  = cardinal;    { used for device numbers      }
    gid_t  = longint;     { used for group IDs           }
    ino_t  = cardinal;    { used for file serial numbers }
    mode_t = cardinal;    { used for file attributes     }
    nlink_t  = cardinal;  { used for link counts         }
    off_t  = cardinal;    { used for file sizes          }
    pid_t  = longint;     { used as process identifier   }
    size_t = cardinal;    { as definied in the C standard }
    ssize_t = longint;    { used by function for returning number of bytes }
    uid_t = longint;      { used for user ID type        }
    time_t = cardinal;    { used for returning the time  }
    blksize_t = cardinal;
    blkcnt_t = cardinal;
    

{***********************************************************************}
{                         POSIX STRUCTURES                              }
{***********************************************************************}
CONST
    _UTSNAME_LENGTH = 256;   { 256 + 1 in pchar format }
    _UTSNAME_NODENAME_LENGTH = 256;

TYPE
   { system information services }
   utsname = packed record   { don't forget to verify the alignment }
     sysname : array[0.._UTSNAME_LENGTH] of char;
     nodename : array[0.._UTSNAME_LENGTH] of char;
     release : array[0.._UTSNAME_LENGTH] of char;
     version : array[0.._UTSNAME_LENGTH] of char;
     machine : array[0.._UTSNAME_LENGTH] of char;
   end;

  { file characteristics services }
   stat = packed record { verify the alignment of the members }
{$IFDEF ENDIAN_LITTLE}
    st_ino : ino_t;              { File serial number }
    st_ino_hi : ino_t;
    st_size : off_t;
    st_size_hi : off_t;
{$ELSE}
    st_ino_hi : ino_t;
    st_ino : ino_t;
    st_size_hi : off_t;
    st_size : off_t;
{$ENDIF}   
     st_dev : dev_t;            (* ID of device containing file.        *)
     st_rdev : dev_t;           (* Device ID, for inode that is device  *)
     st_uid : uid_t;
     st_gid : gid_t;
     st_mtime : time_t;         (* Time of last data modification       *)
     st_atime : time_t;         (* Time last accessed                   *)
     st_ctime : time_t;         (* Time of last status change           *)
     st_mode : mode_t;          (* see below                            *)
     st_nlink : nlink_t;
     st_blocksize : blksize_t;  (* Size of a block used by st_nblocks   *)
     st_nblocks : longint;      (* Number of blocks st_blocksize blocks *)
     st_blksize : blksize_t;    (* Prefered I/O block size for object   *)
{$IFDEF ENDIAN_LITTLE}
    st_blocks : blkcnt_t;       (* Number of 512 byte blocks            *)
    st_blocks_hi : blkcnt_t; 
{$ELSE}
    st_blocks_hi : blkcnt_t;
    st_blocks : blkcnt_t;
{$ENDIF}    
   end;
   

  { directory services }
   pdirent = ^dirent;
   dirent = packed record    { directory entry record - verify alignment }
{$ifdef ENDIAN_LITTLE}   
    d_ino : ino_t;           (* File serial number *)
    d_ino_hi : ino_t;
    d_offset : off_t;
    d_offset_hi : off_t;
{$else}    
    d_ino_hi : ino_t;
    d_ino : ino_t;
    d_offset_hi : off_t;
    d_offset : off_t;
{$endif} 
    d_reclen : smallint;
    d_namelen : smallint;
    d_name : array[0..255] of char;
   end;

   pdir = ^dir;
   dir = packed record
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
    O_EXCL      = $400; { Fail if file already exists.      }
    O_TRUNC     = $200; { Truncate file to zero length.     }
    O_NOCTTY    = $800; { Don't assign a controlling terminal. }
    { File status flags for `open' and `fcntl'.  }
    O_APPEND    = $08;  { Writes append to the file.        }
    O_NONBLOCK  = $80;  { Non-blocking I/O.                 }

    { mode_t possible values                                 }
    S_IRUSR =  $100;        { Read permission for owner   }
    S_IWUSR =   $80;        { Write permission for owner  }
    S_IXUSR =   $40;        { Exec  permission for owner  }
    S_IRGRP =   $20;        { Read permission for group   }
    S_IWGRP =   $10;        { Write permission for group  }
    S_IXGRP =    $8;        { Exec permission for group   }
    S_IROTH =    $4;        { Read permission for world   }
    S_IWOTH =    $2;        { Write permission for world  }
    S_IXOTH =    $1;        { Exec permission for world   }

    { Used for waitpid }
    WNOHANG   = $40;               { don't block waiting               }
    WUNTRACED = $04;               { report status of stopped children }


    { POSIX limits, used for buffer and stack allocation }
    ARG_MAX = 61440;    { Maximum number of argument size     }
    NAME_MAX = 255;     { Maximum number of bytes in filename }
    PATH_MAX = 1024;    { Maximum number of bytes in pathname }

    {***********************************************************************}
    {                            signal handling                            }
    {***********************************************************************}
    
    {$i signal.inc}

