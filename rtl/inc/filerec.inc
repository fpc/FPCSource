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

const
{$ifdef linux}
  filerecnamelength = 255;
{$else}
  {$ifdef Win32}
    filerecnamelength = 255;
  {$else}
    filerecnamelength = 79;
  {$endif}
{$endif}

Type
{$PACKRECORDS 2}
  FileRec = Record
{$ifdef win32}
    Handle    : longint;
{$else win32}
    Handle    : word;
{$endif win32}
    Mode      : word;
    RecSize   : word;
    _private  : array[1..26] of byte;
    UserData  : array[1..16] of byte;
    name      : array[0..filerecnamelength] of char;
  End;

{
  $Log$
  Revision 1.2  1998-05-12 10:42:44  peter
    * moved getopts to inc/, all supported OS's need argc,argv exported
    + strpas, strlen are now exported in the systemunit
    * removed logs
    * removed $ifdef ver_above

}
