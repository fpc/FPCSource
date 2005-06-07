{
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
    cUInt64= qword;

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
    culonglong = qword;
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


