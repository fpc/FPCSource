{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

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

type
    { the following type definitions are compiler dependant }
    { and system dependant                                  }

    cint8  = shortint;
    cuint8 = byte;
    cuint16= word;
    cint16 = smallint;
    cint32 = longint;
    cuint32= cardinal;
    cint64 = int64;
{$ifndef VER_1_0}
    cuint64= qword;
{$endif}

    cint   = longint;           { minimum range is : 32-bit    }
    cuint  = Cardinal;          { minimum range is : 32-bit    }
    clong  = longint;
    culong = Cardinal;

    dev_t    = cuint32;         { used for device numbers      }
    gid_t    = cuint32;         { used for group IDs           }
    ino_t    = clong;           { used for file serial numbers }
    mode_t   = cuint16;         { used for file attributes     }
    nlink_t  = cuint16;         { used for link counts         }
    off_t    = cint64;          { used for file sizes          }
    pid_t    = cint32;          { used as process identifier   }
    size_t   = cuint32;         { as definied in the C standard}
    ssize_t  = cint32;          { used by function for returning number of bytes }
    uid_t    = cuint32;         { used for user ID type        }

    time_t   = clong;           { used for returning the time  }
    socklen_t= cuint32;

CONST
   { System limits, POSIX value in parentheses, used for buffer and stack allocation }
    ARG_MAX  = 65536;   {4096}  { Maximum number of argument size     }
    NAME_MAX = 255;     {14}    { Maximum number of bytes in filename }
    PATH_MAX = 1024;    {255}   { Maximum number of bytes in pathname }

    SYS_NMLN = 32;              {BSD utsname struct limit}
