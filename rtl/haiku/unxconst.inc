const
  { Things for OPEN call - after include/sys/fcntl.h,
   BSD specifies these constants in hex }
  Open_Accmode  = 3;
//  Open_RdOnly   = 0;
//  Open_WrOnly   = 1;
//  Open_RdWr     = 2;
//  Open_NonBlock = 4;
//  Open_Append   = 8;
  Open_ShLock   = $10;
  Open_ExLock   = $20;
  Open_ASync    = $40;
  Open_FSync    = $80;
  Open_NoFollow = $100;
  Open_Create   = $200;       {BSD convention}
//  Open_Creat    = $200;       {Linux convention}
//  Open_Trunc    = $400;
//  Open_Excl     = $800;
//  Open_NoCTTY   = $8000;


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
    OPEN_RDONLY    =     0;        { Open read-only.  }
    OPEN_WRONLY    =     1;        { Open write-only. }
    OPEN_RDWR      =     2;        { Open read/write. }
    { Bits OR'd into the second argument to open.  }
    OPEN_CREAT     =  $200;        { Create file if it doesn't exist.  }
    OPEN_EXCL      =  $800;        { Fail if file already exists.      }
    OPEN_TRUNC     =  $400;        { Truncate file to zero length.     }
    OPEN_NOCTTY    = $8000;        { Don't assign a controlling terminal. }
    { File status flags for `open' and `fcntl'.  }
    OPEN_APPEND    =     8;        { Writes append to the file.        }
    OPEN_NONBLOCK  =     4;        { Non-blocking I/O.                 }

    { mode_t possible values                                 }
  { Constants to check stat.mode -  checked all STAT constants with Haiku}
  STAT_IFMT   = $f000; {0170000 }
  STAT_IFSOCK = $c000; {0140000 }
  STAT_IFLNK  = $a000; {0120000 }
  STAT_IFREG  = $8000; {0100000 }
  STAT_IFBLK  = $6000; {0060000 }
  STAT_IFDIR  = $4000; {0040000 }
  STAT_IFCHR  = $2000; {0020000 }
  STAT_IFIFO  = $1000; {0010000 }

  STAT_ISUID  = $0800; {0004000 }
  STAT_ISGID  = $0400; {0002000 }
  STAT_ISVTX  = $0200; {0001000}
    
    
    STAT_IRWXU =  %0111000000;     { Read, Write, Exec permission for owner   }
    STAT_IRUSR =  %0100000000;     { Read permission for owner   }
    STAT_IWUSR =  %0010000000;     { Write permission for owner  }
    STAT_IXUSR =  %0001000000;     { Exec  permission for owner  }
    STAT_IRWXG =  %0000111000;     { Read, Write, Exec permission for group   }
    STAT_IRGRP =  %0000100000;     { Read permission for group   }
    STAT_IWGRP =  %0000010000;     { Write permission for group  }
    STAT_IXGRP =  %0000001000;     { Exec permission for group   }
    STAT_IRWXO =  %0000000111;     { Read, Write, Exec permission for world   }
    STAT_IROTH =  %0000000100;     { Read permission for world   }
    STAT_IWOTH =  %0000000010;     { Write permission for world  }
    STAT_IXOTH =  %0000000001;     { Exec permission for world   }

    { Used for waitpid }
    WAIT_NOHANG   =          1;     { don't block waiting               }
    WAIT_UNTRACED =          2;     { report status of stopped children }
