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
    - 64bitarch (for 64-bits Linux systems, test system was idefix
}

Type
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
{$else}
    cuint64= int64;
{$endif}

    cuchar = byte;
    cint   = longint;           { minimum range is : 32-bit    }
    cuint  = Cardinal;          { minimum range is : 32-bit    }
  {$ifdef 64bitarch}
    clong  = int64;
  {$ifdef VER_1_0}
    culong = int64;
  {$else}
    culong = qword;
   {$endif}
  {$else}
    clong  = longint;
    culong = Cardinal;
  {$endif}
    cshort = integer;
    cushort= word;

    pcint  = ^cint;

{ 
   $Log$
   Revision 1.1  2002-11-09 22:39:28  marco
    * first version


}

