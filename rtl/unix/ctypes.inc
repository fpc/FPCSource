{
    $Id: ctypes.inc,v 1.4 2005/03/13 19:17:14 florian Exp $
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
    cchar  = shortint;
    cInt   = longint;           { minimum range is : 32-bit    }
    cUInt  = Cardinal;          { minimum range is : 32-bit    }
{$ifdef cpu64}
    cLong  = int64;
    cuLong = qword;
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
    cshort   = smallint;
    cushort  = word;

    pcInt    = ^cInt;
    pcUInt   = ^cUInt;
    pcLong   = ^cLong;
    pculong  = ^cuLong;
    pcshort  = ^cshort;
    pcushort = ^cushort;
    pcchar   = ^cchar;
    pcuchar  = ^cuchar;

    cunsigned = cuint;
    pcunsigned = ^cunsigned;

    { Floating point }
    cFloat    = Single;
    cDouble   = Double;
    clDouble  = Extended;
    pcFloat   = ^cFloat;
    pcDouble  = ^cDouble;
    pclDouble = ^clDouble;

{
   $Log: ctypes.inc,v $
   Revision 1.4  2005/03/13 19:17:14  florian
     * indention fixed

   Revision 1.3  2005/03/13 10:05:13  florian
     + floating point c types added

   Revision 1.2  2005/02/14 17:13:31  peter
     * truncate log

   Revision 1.1  2005/02/13 22:14:36  peter
     * new files

   Revision 1.6  2005/02/05 23:02:37  florian
     + added some missing c types

}

