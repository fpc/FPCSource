{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    Implements roughly POSIX 1003.1 conforming interface for *BSD
    header part.

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

{$i ptypes.inc}

// Can't find these two in Posix and in FreeBSD
//CONST
//    _UTSNAME_LENGTH = ;
//    _UTSNAME_NODENAME_LENGTH = ;

TYPE
   { system information services }
   utsname = record          	
              sysname : Array[0..SYS_NMLN-1] OF Char;   // Name of this OS
              nodename: Array[0..SYS_NMLN-1] OF Char;	// Name of this network node. 
              release : Array[0..SYS_NMLN-1] OF Char;	// Release level. 
              version : Array[0..SYS_NMLN-1] OF Char; 	// Version level. 
              machine : Array[0..SYS_NMLN-1] OF Char;	// Hardware type. 
             end;

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
        st_size       : off_t;             // file size, in bytes 
        st_blocks     : cint64;            // blocks allocated for file 
        st_blksize    : cuint32;           // optimal blocksize for I/O 
        st_flags      : cuint32;           // user defined flags for file 
        st_gen        : cuint32;           // file generation number 
{$ifndef NetBSD}
        st_lspare     : cint32;
{$endif}
        st_qspare     : array[0..1] Of cint64;
   end;

  { directory services }
   pdirent = ^dirent;
   dirent  = record  
        d_fileno      : cuint32;	           	// file number of entry 
        d_reclen      : cuint16;  	         	// length of this record 
        d_type        : cuint8;     	       		// file type, see below 
        d_namlen      : cuint8;            		// length of string in d_name 
        d_name        : array[0..(255 + 1)-1] of char;	// name must be no longer than this
   end;

   pdir    = ^dir;
   dir     = packed record
        dd_fd     : cint;         // file descriptor associated with directory
        dd_loc    : clong;        // offset in current buffer 
        dd_size   : clong;        // amount of data returned by getdirentries
        dd_buf    : pchar;        // data buffer 
        dd_len    : cint;         // size of data buffer 
        dd_seek   : clong;        // magic cookie returned by getdirentries 
        dd_rewind : clong;        // magic cookie for rewinding 
        dd_flags  : cint;         // flags for readdir 
   end;


{***********************************************************************}
{                  POSIX CONSTANT ROUTINE DEFINITIONS                   }
{***********************************************************************}
CONST
    { access routine - these maybe OR'ed together }
    F_OK        =     0;	{ test for existence of file }
    R_OK        =     4;	{ test for read permission on file }
    W_OK        =     2;	{ test for write permission on file }
    X_OK        =     1;	{ test for execute or search permission }
    { seek routine }
    SEEK_SET    =     0;	{ seek from beginning of file }
    SEEK_CUR    =     1;	{ seek from current position  }
    SEEK_END    =     2;	{ seek from end of file       }
    { open routine                                 }
    { File access modes for `open' and `fcntl'.    }
    O_RDONLY    =     0;	{ Open read-only.  }
    O_WRONLY    =     1;	{ Open write-only. }
    O_RDWR      =     2;	{ Open read/write. }
    { Bits OR'd into the second argument to open.  }
    O_CREAT     =  $200;	{ Create file if it doesn't exist.  }
    O_EXCL      =  $800;	{ Fail if file already exists.      }
    O_TRUNC     =  $400;	{ Truncate file to zero length.     }
    O_NOCTTY    = $8000;	{ Don't assign a controlling terminal. }
    { File status flags for `open' and `fcntl'.  }
    O_APPEND    =     8;	{ Writes append to the file.        }
    O_NONBLOCK	=     4;	{ Non-blocking I/O.                 }

    { mode_t possible values                                 }
    S_IRUSR =  %0100000000;	{ Read permission for owner   }
    S_IWUSR =  %0010000000;	{ Write permission for owner  }
    S_IXUSR =  %0001000000;	{ Exec  permission for owner  }
    S_IRGRP =  %0000100000;	{ Read permission for group   }
    S_IWGRP =  %0000010000;	{ Write permission for group  }
    S_IXGRP =  %0000001000;	{ Exec permission for group   }
    S_IROTH =  %0000000100;  	{ Read permission for world   }
    S_IWOTH =  %0000000010;	{ Write permission for world  }
    S_IXOTH =  %0000000001;	{ Exec permission for world   }

    { Used for waitpid }
    WNOHANG   = 	 1;	{ don't block waiting               }
    WUNTRACED = 	 2;	{ report status of stopped children }

   
    {*************************************************************************}
    {                               SIGNALS                                   }
    {*************************************************************************}
    
{$i signal.inc}

// function geterrno:longint; 
// procedure seterrno(i:longint); 

{
  $Log$
  Revision 1.3  2002-08-21 07:03:16  marco
   * Fixes from Tuesday.

  Revision 1.2  2002/08/19 12:29:11  marco
   * First working POSIX *BSD system unit.

  Revision 1.1  2002/08/03 19:34:19  marco
   * Initial *BSD versions. Seems that OpenBSD doesn't need much change,
      NetBSD may need some fixes to stat record and ftruncate and lseek.
      It is all close together, and it should be doable to have just one copy
      of these for *BSD.

  Revision 1.1.2.2  2001/11/28 03:08:46  carl
  - removed signal stuff , moved to signal.inc

  Revision 1.1.2.1  2001/08/15 00:15:08  carl
  - renamed

}