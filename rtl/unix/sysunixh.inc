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

{$define newsignal}

{$I systemh.inc}

{$ifdef Unix}
type
   { the fields of this record are os dependent  }
   { and they shouldn't be used in a program     }
   { only the type TCriticalSection is important }
   TRTLCriticalSection = packed record
      DebugInfo : pointer;
      LockCount : longint;
      RecursionCount : longint;
      OwningThread : DWord;
      LockSemaphore : DWord;
      Reserved : DWord;
   end;

{ include threading stuff }
{$i threadh.inc}
{$endif unix}

{$I heaph.inc}

{$ifdef m68k}
{ used for single computations }
const
  BIAS4 = $7f-1;
{$endif}

{Platform specific information}
const
 LineEnding = #10;
 LFNSupport = true;
 DirectorySeparator = '/';
 DriveSeparator = ':';
 PathSeparator = ':';
{ FileNameCaseSensitive is defined below! }


const
  UnusedHandle    = -1;
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = 2;

  FileNameCaseSensitive : boolean = true;

  sLineBreak = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsLF;

var
  argc : longint;
  argv : ppchar;
  envp : ppchar;

{
  $Log$
  Revision 1.14  2002-07-01 16:29:05  peter
    * sLineBreak changed to normal constant like Kylix

  Revision 1.13  2001/11/08 13:56:35  marco
   * Fixed a ifdef linux to ifdef unix (related to TRTL change)

  Revision 1.12  2001/10/23 21:51:03  peter
    * criticalsection renamed to rtlcriticalsection for kylix compatibility

  Revision 1.11  2001/10/14 13:33:21  peter
    * start of thread support for linux

  Revision 1.10  2001/06/27 21:37:39  peter
    * v10 merges

  Revision 1.9  2001/06/18 14:26:16  jonas
    * move platform independent constant declarations after inclusion of
      systemh.inc

  Revision 1.8  2001/06/13 22:20:11  hajny
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
