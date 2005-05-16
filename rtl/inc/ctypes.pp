{
    $Id: ctypes.pp,v 1.5 2005/03/13 10:05:13 florian Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 by Marco van de Voort, member of the
    Free Pascal development team

    Implements C types for in header conversions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


 **********************************************************************}

unit ctypes;

interface

{$ifdef unix}
uses unixtype;
{$i aliasctp.inc}
{$else}

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
{$endif}

// Kylix compat types
    u_long  = culong;
    u_short = cushort;

implementation


end.

{
 $Log: ctypes.pp,v $
 Revision 1.5  2005/03/13 10:05:13  florian
   + floating point c types added

 Revision 1.4  2005/03/01 22:45:09  hajny
   * Florian's changes from ctypes.inc merged in to make xlib compilable under non-Unix again

 Revision 1.3  2005/02/14 17:13:22  peter
     * truncate log

 Revision 1.2  2005/02/12 17:35:18  marco
  * some kylix stuf

 Revision 1.1  2005/01/10 10:32:50  marco
  * initial version


}
