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

{ Introduced defines
    - 64bitarch (for 64-bits Linux systems, test system was idefix (in ctypes)
    - 64bitfs   (should be on if libc switches to a 64-bit system.

All three tested systems (PPC,Alpha,2x i386) gave the same POSIX limits,
and all three 32-bit systems returned completely identical types too
(everything 32-bit except dev_t, which is assumed to be a result of devfs
introduction)
}

{$I ctypes.inc}

Type

{$ifndef VER_1_0}
    dev_t    = cuint64;         { used for device numbers      }
{$else}
    dev_t    = int64;
{$endif}
    gid_t    = cuint32;         { used for group IDs           }
    ino_t    = clong;           { used for file serial numbers }
    mode_t   = cuint32;         { used for file attributes     }
    nlink_t  = cuint32;         { used for link counts         }
  {$ifdef 64BitArch}
    off_t    = cint64;          { used for file sizes          }
  {$else}
   {$ifdef 64BitFS}
    off_t    = cint64;
   {$else}
    off_t    = cint;
    pid_t    = cint32;          { used as process identifier   }
   {$endif}
  {$endif}
   {$ifdef 64bitarch} 
    size_t   = cuint64;         { as definied in the C standard}
    ssize_t  = cint64;          { used by function for returning number of bytes }
   {$else}
    size_t   = cuint32;         { as definied in the C standard}
    ssize_t  = cint32;          { used by function for returning number of bytes }
   {$endif}
    uid_t    = cuint32;         { used for user ID type        }
  {$ifdef 64bitarch}
    clock_t  = cuint64;
    time_t   = cint64;           { used for returning the time  }
  {$else}
    clock_t  = culong;
    time_t   = clong;           { used for returning the time  }
  {$endif}
    ptime_t  = ^time_t;
    socklen_t= cuint32;

CONST
   { System limits, POSIX value in parentheses, used for buffer and stack allocation }
   { took idefix' values}

    ARG_MAX  = 131072;   {4096}  { Maximum number of argument size     }
    NAME_MAX = 255;     {14}    { Maximum number of bytes in filename }
    PATH_MAX = 4095;    {255}   { Maximum number of bytes in pathname }

{
   $Log$
   Revision 1.2  2002-11-12 14:28:40  marco
    * some updates

   Revision 1.1  2002/10/29 16:47:17  marco
    * Linux versions


}

