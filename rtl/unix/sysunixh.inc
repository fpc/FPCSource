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

  sLineBreak : string[1] = #10;

var
  argc : longint;
  argv : ppchar;
  envp : ppchar;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsLF;

{
  $Log$
  Revision 1.4  2001-03-21 23:32:13  florian
    + header and log added

}  
