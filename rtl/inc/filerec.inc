{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team

    FileRec record definition


    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  This file contains the definition of the filerec record.
  It is put separately, so it is available outside the system
  unit without sacrificing TP compatibility.
}

{$ifndef VER0_99_5}
  {$ifndef VER0_99_6}
    {$define UNIFORM_FILEREC}
  {$endif}
{$endif}


{$ifdef UNIFORM_FILEREC}


const
  filerecnamelength = 255;
type
  FileRec = Packed Record
    Handle,
    Mode,
    RecSize   : longint;
    _private  : array[1..32] of byte;
    UserData  : array[1..16] of byte;
    name      : array[0..filerecnamelength] of char;
  End;


{$else UNIFORM_FILEREC}


{**********************************
   Old style for 0.99.5/0.99.6
**********************************}

const
  {$ifdef linux}
    filerecnamelength = 255;
  {$endif}
  {$ifdef Win32}
    filerecnamelength = 255;
  {$endif}
  {$ifdef MACOS}
    filerecnamelength = 255;
  {$endif}
  {$ifdef AMIGA}
    filerecnamelength = 255;
  {$endif}
  {$ifdef OS2}
    filerecnamelength = 79;
  {$endif}
  {$ifdef GO32V2}
    filerecnamelength = 79;
  {$endif GO32V2}
  {$ifdef GO32V1}
    filerecnamelength = 79;
  {$endif Go32v1}
  {$ifdef ATARI}
    filerecnamelength = 79;
  {$endif}

Type
{$PACKRECORDS 2}
  FileRec = Record
  {$ifdef win32}
     handle    : longint;
  {$endif win32}
  {$ifdef amiga}
     handle    : longint;
  {$endif amiga}
  {$ifdef macos}
     handle    : longint;
  {$endif macos}
  {$ifdef linux}
     handle    : word;
  {$endif}
  {$ifdef go32v1}
     handle    : word;
  {$endif go32v1}
  {$ifdef go32v2}
     handle    : word;
  {$endif go32v2}
  {$ifdef atari}
     handle    : word;
  {$endif atari}
  {$ifdef os2}
     handle    : word;
  {$endif os2}
    Mode      : word;
    RecSize   : word;
    _private  : array[1..26] of byte;
    UserData  : array[1..16] of byte;
    name      : array[0..filerecnamelength] of char;
  End;
{$endif UNIFORM_FILEREC}

{
  $Log$
  Revision 1.4  1998-09-04 18:16:13  peter
    * uniform filerec/textrec (with recsize:longint and name:0..255)

  Revision 1.3  1998/05/21 11:55:59  carl
   * works with all OS
}
