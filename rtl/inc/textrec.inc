{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by the Free Pascal development team

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

Const

{$ifdef linux}
  textrecnamelength = 255;
{$else}
  {$ifdef Win32}
    textrecnamelength = 255;
  {$else}
    textrecnamelength = 79;
  {$endif}
{$endif}

type       
  textbuf = array[0..127] of char;

{$PACKRECORDS 2}
  textrec = record
{$ifdef win32}
     handle    : longint;
{$else win32}
     handle    : word;
{$endif win32}
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

{
  $Log$
  Revision 1.2  1998-05-12 10:42:45  peter
    * moved getopts to inc/, all supported OS's need argc,argv exported
    + strpas, strlen are now exported in the systemunit
    * removed logs
    * removed $ifdef ver_above

}
