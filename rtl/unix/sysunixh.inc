{
    $Id$
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2001 by the Free Pascal development team

    This file contains the OS independent declarations of the system unit
    for unix styled systems

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{Platform specific information}
const
 LineEnding = #10;
 LFNSupport = true;
 DirectorySeparator = '/';
 DriveSeparator = ':';
 PathSeparator = ':';
 FileNameCaseSensitive = true;

{$ifdef m68k}
{ used for single computations }
const
  BIAS4 = $7f-1;
{$endif}

{$define newsignal}

{$I systemh.inc}
{$I heaph.inc}

const
  UnusedHandle    = -1;
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = 2;

  FileNameCaseSensitive : boolean = true;

  sLineBreak : string[1] = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsLF;

var
  argc : longint;
  argv : ppchar;
  envp : ppchar;

{
  $Log$
  Revision 1.8  2001-06-13 22:20:11  hajny
    + platform specific information

  Revision 1.7  2001/04/13 23:49:49  peter
    * fixes for the stricter compiler

  Revision 1.6  2001/04/13 22:30:55  peter
    * added filenamecasesensitive var

  Revision 1.5  2001/03/22 21:56:27  florian
    * typo fixed

  Revision 1.4  2001/03/21 23:32:13  florian
    + header and log added
}
