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
  Revision 1.1  1998-03-25 11:18:43  root
  Initial revision

  Revision 1.9  1998/02/05 12:08:55  pierre
    * added packrecords to about dword alignment
      for structures used in dos calls

  Revision 1.8  1998/01/26 12:00:01  michael
  + Added log at the end


  
  Working file: rtl/inc/textrec.inc
  description:
  ----------------------------
  revision 1.7
  date: 1998/01/06 00:29:33;  author: michael;  state: Exp;  lines: +21 -23
  Implemented a system independent sequence of reset/rewrite/append fileopenfunc etc system \n (from Peter Vreman)
  ----------------------------
  revision 1.6
  date: 1997/12/01 12:08:06;  author: michael;  state: Exp;  lines: +13 -0
  + added copyright reference header.
  ----------------------------
  revision 1.5
  date: 1997/11/28 19:45:22;  author: pierre;  state: Exp;  lines: +13 -9
    * one more bug fix with namelength
    + fixed math in fixed_math define (does not compile yet)
  ----------------------------
  revision 1.4
  date: 1997/11/28 19:15:50;  author: pierre;  state: Exp;  lines: +0 -3
     * forgot to remove the const statement
  ----------------------------
  revision 1.3
  date: 1997/11/28 18:56:17;  author: pierre;  state: Exp;  lines: +9 -10
    bug fix     in ifdef win32
  ----------------------------
  revision 1.2
  date: 1997/11/27 22:49:05;  author: florian;  state: Exp;  lines: +4 -0
  - CPU.PP added
  - some bugs in DOS fixed (espsecially for go32v1)
  - the win32 system unit is now compilable
  ----------------------------
  revision 1.1
  date: 1997/11/27 08:33:47;  author: michael;  state: Exp;
  Initial revision
  ----------------------------
  revision 1.1.1.1
  date: 1997/11/27 08:33:47;  author: michael;  state: Exp;  lines: +0 -0
  FPC RTL CVS start
  =============================================================================
}
