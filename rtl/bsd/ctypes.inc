{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    Basic types for C interfacing. Check the 64-bit defines.    

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
    - 64bitarch (for 64-bits OSS BSD systems)
}

Type
    { the following type definitions are compiler dependant }
    { and system dependant                                  }

    cInt8  = shortint;
    cUInt8 = byte;
    cUInt16= word;
    cInt16 = smallint;
    cInt32 = longint;
    cUInt32= cardinal;
    cInt64 = int64;
{$ifndef VER1_0}
    cUInt64= qword;
{$else}
    cUInt64= int64;
{$endif}

    cuchar = byte;
    cInt   = longint;           { minimum range is : 32-bit    }
    cUInt  = Cardinal;          { minimum range is : 32-bit    }
  {$ifdef CPU64}
    cLong  = int64;
  {$ifdef VER1_0}
    cuLong = int64;
  {$else}
    cuLong = qword;
   {$endif}
  {$else}
    cLong  = longint;
    cuLong = Cardinal;
  {$endif}
    clonglong = int64;
{$ifndef VER1_0}
    culonglong = qword;
{$else VER1_0}
    culonglong = int64;
{$endif VER1_0}
    cshort = integer;
    cushort= word;

    pcInt   = ^cInt;
    pcUInt  = ^cUInt;
    pcLong  = ^cLong;
    pculong = ^cuLong;
    pcshort = ^cshort;
    pcushort= ^cushort;

{   
   $Log$
   Revision 1.4  2004-09-09 20:29:06  jonas
     * fixed definition of pthread_mutex_t for non-linux targets (and for
       linux as well, actually).
     * base libpthread definitions are now in ptypes.inc, included in unixtype
       They sometimes have an extra underscore in front of their name, in
       case they were also exported by the packages/base/pthreads unit, so
       they can keep their original name there
     * cthreadds unit now imports systuils, because it uses exceptions (it
       already did so before as well)
     * fixed many linux definitions of libpthread functions in pthrlinux.inc
       (integer -> cint etc)
     + added culonglong type to ctype.inc

   Revision 1.3  2004/05/17 19:20:09  marco
    * x86_64 patches

   Revision 1.2  2003/09/16 21:20:40  marco
    * pointer's of some basetypes

   Revision 1.1  2003/01/03 11:21:32  marco
    * copied and checked from Linux.


}

