{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993-98 by the Free Pascal development team

    Textrec record definition


    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  This file contains the definition of the textrec record.
  It is put separately, so it is available outside the system
  unit without sacrificing TP compatibility.
}

{$ifndef VER0_99_5}
  {$ifndef VER0_99_6}
    {$define UNIFORM_TEXTREC}
  {$endif}
{$endif}


{$ifdef UNIFORM_TEXTREC}


const
  TextRecNameLength = 255;
type
  TextBuf = array[0..255] of char;
  TextRec = Packed Record
    Handle,
    Mode,
    bufsize,
    _private,
    bufpos,
    bufend    : longint;
    bufptr    : ^textbuf;
    openfunc,
    inoutfunc,
    flushfunc,
    closefunc : pointer;
    UserData  : array[1..16] of byte;
    name      : array[0..textrecnamelength] of char;
    buffer    : textbuf;
  End;


{$else UNIFORM_TEXTREC}


{**********************************
   Old style for 0.99.5/0.99.6
**********************************}

Const
{$ifdef linux}
  textrecnamelength = 255;
{$endif}
{$ifdef Win32}
    textrecnamelength = 255;
{$endif}
{$ifdef MACOS}
    textrecnamelength = 255;
{$endif}
{$ifdef AMIGA}
    textrecnamelength = 255;
{$endif}
{$ifdef OS2}
    textrecnamelength = 79;
{$endif}
{$ifdef Go32v1}
    textrecnamelength = 79;
{$endif Go32v1}
{$ifdef Go32v2}
    textrecnamelength = 79;
{$endif Go32v2}
{$ifdef ATARI}
    textrecnamelength = 79;
{$endif}

type
  textbuf = array[0..127] of char;

{$PACKRECORDS 2}
  textrec = record
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
{$ifdef Go32v1}
     handle    : word;
{$endif Go32v1}
{$ifdef Go32v2}
     handle    : word;
{$endif Go32v2}
{$ifdef atari}
     handle    : word;
{$endif atari}
{$ifdef os2}
     handle    : word;
{$endif os2}
     mode      : word;
     bufsize,
     _private,
     bufpos,
     bufend    : word;
     bufptr    : ^textbuf;
     openfunc,
     inoutfunc,
     flushfunc,
     closefunc : pointer;
     userdata  : array[1..16] of byte;
     name      : array[0..textrecnamelength] of char;
     buffer    : textbuf;
  end;

{$endif UNIFORM_TEXTREC}

{
  $Log$
  Revision 1.4  1998-09-04 18:16:15  peter
    * uniform filerec/textrec (with recsize:longint and name:0..255)

  Revision 1.3  1998/05/21 15:37:19  carl
   + working version now ok
}
