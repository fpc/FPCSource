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
  Revision 1.1  1998-03-25 11:18:43  root
  Initial revision

  Revision 1.7  1998/02/05 12:08:54  pierre
    * added packrecords to about dword alignment
      for structures used in dos calls

  Revision 1.6  1998/01/26 12:00:21  michael
  + Added log at the end


  
  Working file: rtl/inc/filerec.inc
  description:
  ----------------------------
  revision 1.5
  date: 1998/01/06 00:29:32;  author: michael;  state: Exp;  lines: +20 -19
  Implemented a system independent sequence of reset/rewrite/append fileopenfunc etc system \n (from Peter Vreman)
  ----------------------------
  revision 1.4
  date: 1997/12/01 12:08:03;  author: michael;  state: Exp;  lines: +13 -0
  + added copyright reference header.
  ----------------------------
  revision 1.3
  date: 1997/11/28 18:56:18;  author: pierre;  state: Exp;  lines: +2 -1
    bug fix     in ifdef win32
  ----------------------------
  revision 1.2
  date: 1997/11/27 22:49:04;  author: florian;  state: Exp;  lines: +7 -0
  - CPU.PP added
  - some bugs in DOS fixed (espsecially for go32v1)
  - the win32 system unit is now compilable
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:46;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:46;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================
}
