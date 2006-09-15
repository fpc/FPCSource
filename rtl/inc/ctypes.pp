{
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

    cint8      = shortint;
    cuint8     = byte;
    cuint16    = word;
    cint16     = smallint;
    cint32     = longint;
    cuint32    = cardinal;
    cint64     = int64;
    cuint64    = qword;

    cbool      = longbool;
    cchar      = shortint;
    cuchar     = byte;
    cint       = longint;           { minimum range is : 32-bit    }
    cuint      = cardinal;          { minimum range is : 32-bit    }

    csint      = cint;
    csigned    = cint;
    cunsigned  = cuint;

{$ifdef cpu64}
    clong      = int64;
    culong     = qword;
{$else}
    clong      = longint;
    culong     = cardinal;
{$endif}

    clonglong  = int64;
    culonglong = qword;
    cshort     = smallint;
    cushort    = word;

    pcint8     = ^cint8;
    pcuint8    = ^cuint8;
    pcint16    = ^cint16;
    pcuint16   = ^cuint16;
    pcint32    = ^cint32;
    pcuint32   = ^cuint32;
    pcint64    = ^cint64;
    pcuint64   = ^cuint64;
    pcbool     = ^cbool;
    pcchar     = ^cchar;
    pcuchar    = ^cuchar;
    pcint      = ^cint;
    pcuint     = ^cuint;
    pcsint     = ^csint;
    pcsigned   = ^csigned;
    pcunsigned = ^cunsigned;
    pclong     = ^clong;
    pculong    = ^culong;
    pcshort    = ^cshort;
    pcushort   = ^cushort;
    
    { Floating point }
    cfloat     = single;
    cdouble    = double;
    cldouble   = extended;
    pcfloat    = ^cfloat;
    pcdouble   = ^cdouble;
    pcldouble  = ^cldouble;    
{$endif}

// Kylix compat types
    u_long     = culong;
    u_short    = cushort;

implementation


end.
