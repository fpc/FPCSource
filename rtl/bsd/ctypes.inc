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
{$ifndef VER_1_0}
    cUInt64= qword;
{$else}
    cUInt64= int64;
{$endif}

    cuchar = byte;
    cInt   = longint;           { minimum range is : 32-bit    }
    cUInt  = Cardinal;          { minimum range is : 32-bit    }
  {$ifdef 64bitarch}
    cLong  = int64;
  {$ifdef VER_1_0}
    cuLong = int64;
  {$else}
    cuLong = qword;
   {$endif}
  {$else}
    cLong  = longint;
    cuLong = Cardinal;
  {$endif}
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
   Revision 1.2  2003-09-16 21:20:40  marco
    * pointer's of some basetypes

   Revision 1.1  2003/01/03 11:21:32  marco
    * copied and checked from Linux.


}

