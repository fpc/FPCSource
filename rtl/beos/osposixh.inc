{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file implements all the types used in POSIX for BeOS

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

    cint  = longint;       { minimum range is : 32-bit                   }
    cuint = cardinal;      { minimum range is : 32-bit                   }


    dev_t  = cint;             { used for device numbers      }
    gid_t  = cuint;            { used for group IDs           }
    ino_t  = int64;           { used for file serial numbers }
    mode_t = cuint;            { used for file attributes     }
    nlink_t  = cint;           { used for link counts         }
    off_t  = int64;           { used for file sizes          }
    pid_t  = cint;             { used as process identifier   }
    size_t = cint;             { as definied in the C standard }
    ssize_t = cint;            { used by function for returning number of bytes }
    uid_t =  cuint;            { used for user ID type        }
    time_t = cint;             { used for returning the time  }
    sigset_t = cuint;          { used for additional signal   }

{***********************************************************************}
{                         POSIX STRUCTURES                              }
{***********************************************************************}
CONST
    _UTSNAME_LENGTH = 32;
    _UTSNAME_NODENAME_LENGTH = _UTSNAME_LENGTH;

TYPE
   { system information services }
   utsname = packed record   { don't forget to verify the alignment }
     { Name of this implementation of the operating systems (POSIX) }
     sysname : array[0.._UTSNAME_LENGTH+1] of char;
     { Name of this node (POSIX) }
     nodename : array[0.._UTSNAME_NODENAME_LENGTH+1] of char;
     { Current release level of this implementation (POSIX) }
     release : array[0.._UTSNAME_LENGTH+1] of char;
     { Current version level of this release (POSX) }
     version : array[0.._UTSNAME_LENGTH+1] of char;
     { Name of the hardware type on which the system is running (POSIX) }
     machine : array[0.._UTSNAME_LENGTH+1] of char;
   end;

  { file characteristics services }
   stat = packed record { verify the alignment of the members }
    st_dev : dev_t;     { Device containing the file (POSIX) }
    st_ino : ino_t;             { File serial number (POSIX)         }
    st_mode: mode_t;    { File mode (POSIX)                  }
    st_nlink: nlink_t;  { Link count (POSIX)                 }
    st_uid: uid_t;              { User ID of the file's owner. (POSIX)}
    st_gid: gid_t;              { Group ID of the file's group.(POSIX)}
    st_size : off_t;    { Size of file, in bytes.      (POSIX)}
    st_rdev : dev_t;    { Device type (not used).            }
    st_blksize: cardinal;{ Preferred block size for I/O.     }
    st_atime: time_t;   { Time of last access (POSIX)        }
    st_mtime: time_t;   { Time of last modification (POSIX)  }
    st_ctime: time_t;   { Time of last status change (POSIX) }
    st_crtime: time_t;  { Time of creation                   }
  end;

  { directory services }
   pdirent = ^dirent;
   dirent = packed record    { directory entry record - verify alignment }
         d_dev: dev_t;
         d_pdev: dev_t;
         d_fileno: ino_t;
         d_pino: ino_t;
         d_reclen:word;
         d_name:array[0..255] of char;      { Filename in DIRENT (POSIX) }
   end;

   pdir = ^dir;
   dir = packed record
     fd : cint;         { file descriptor }
     ent : dirent;     { directory entry }
   end;

   sighandler_t = procedure (signo: cint); cdecl;

   { signal services }
   sigactionrec = packed record
     sa_handler : sighandler_t;  { pointer to a function (POSIX.1)     }
     sa_mask : sigset_t;         { additional signal masks  (POSIX.1)  }
     sa_flags : cint;             { special flags for signals (POSIX.1) }
     sa_userdata : pointer;
   end;

{***********************************************************************}
{                  POSIX CONSTANT ROUTINE DEFINITIONS                   }
{***********************************************************************}
CONST
    { access routine - these maybe OR'ed together }
    F_OK        =  0;   { test for existence of file }
    R_OK        =  4;   { test for read permission on file }
    W_OK        =  2;   { test for write permission on file }
    X_OK        =  1;   { test for execute or search permission }
    { seek routine }
    SEEK_SET    =  0;    { seek from beginning of file }
    SEEK_CUR    =  1;    { seek from current position  }
    SEEK_END    =  2;    { seek from end of file       }
    { open routine                                 }
    { File access modes for `open' and `fcntl'.    }
    O_RDONLY    =  0;   { Open read-only.  }
    O_WRONLY    =  1;   { Open write-only. }
    O_RDWR      =  2;   { Open read/write. }
    { Bits OR'd into the second argument to open.  }
    O_CREAT     =$0200; { Create file if it doesn't exist.  }
    O_EXCL      =$0100; { Fail if file already exists.      }
    O_TRUNC     =$0400; { Truncate file to zero length.     }
    O_NOCTTY    =$1000; { Don't assign a controlling terminal. }
    { File status flags for `open' and `fcntl'.  }
    O_APPEND    =$0800; { Writes append to the file.        }
    O_NONBLOCK  =$0080; { Non-blocking I/O.                 }

    { mode_t possible values                                 }
    S_IRUSR = $0100;           { Read permission for owner   }
    S_IWUSR = $0080;           { Write permission for owner  }
    S_IXUSR = $0040;           { Exec  permission for owner  }
    S_IRGRP = S_IRUSR shr 3;   { Read permission for group   }
    S_IWGRP = S_IWUSR shr 3;   { Write permission for group  }
    S_IXGRP = S_IWUSR shr 3;   { Exec permission for group   }
    S_IROTH = S_IRGRP shr 3;   { Read permission for world   }
    S_IWOTH = S_IWGRP shr 3;   { Write permission for world  }
    S_IXOTH = S_IXGRP shr 3;   { Exec permission for world   }

    { Used for waitpid }
    WNOHANG   = 1;               { don't block waiting               }
    WUNTRACED = 2;               { report status of stopped children }


    {************************ signals *****************************}
    { more can be provided. Herein are only included the required  }
    { values.                                                      }
    {**************************************************************}
    SIGABRT    =  6;     { abnormal termination           }
    SIGALRM    = 14;     { alarm clock (used with alarm() }
    SIGFPE     =  8;     { illegal arithmetic operation   }
    SIGHUP     =  1;     { Hangup                         }
    SIGILL     =  4;     { Illegal instruction            }
    SIGINT     =  2;     { Interactive attention signal   }
    SIGKILL    =  9;     { Kill, cannot be caught         }
    SIGPIPE    =  7;     { Broken pipe signal             }
    SIGQUIT    =  3;     { Interactive termination signal }
    SIGSEGV    = 11;     { Detection of invalid memory reference }
    SIGTERM    = 15;     { Termination request           }
    SIGUSR1    = 18;     { Application defined signal 1  }
    SIGUSR2    = 19;     { Application defined signal 2  }
    SIGCHLD    =  5;     { Child process terminated / stopped }
    SIGCONT    = 12;     { Continue if stopped                }
    SIGSTOP    = 10;     { Stop signal. cannot be cuaght      }
    SIGSTP     = 13;     { Interactive stop signal            }
    SIGTTIN    = 16;     { Background read from TTY           }
    SIGTTOU    = 17;     { Background write to TTY            }
    SIGBUS     = SIGSEGV; { Access to undefined memory        }


    { POSIX limits }
    ARG_MAX =  128*1024; { Maximum number of arguments           }
    NAME_MAX = 256;      { Maximum number of bytes in a filename }
    PATH_MAX = 1024;     { Maximum number of bytes in a pathname }


{
  $Log: osposixh.inc,v $
  Revision 1.3  2005/02/14 17:13:21  peter
    * truncate log

}
