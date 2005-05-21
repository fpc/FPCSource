{
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

{$ifdef cpum68k}
{ used for single computations }
const
  BIAS4 = $7f-1;
{$endif cpum68k}

const
 LineEnding = #10;
 LFNSupport = true;
 DirectorySeparator = '/';
 DriveSeparator = ':';
 PathSeparator = ':';
{ FileNameCaseSensitive is defined below! }
 maxExitCode = 255;
 MaxPathLen = 256; 
 
const
  UnusedHandle    = -1;
  StdInputHandle  = 0;
  StdOutputHandle = 1;
  StdErrorHandle  = 2;

  FileNameCaseSensitive : boolean = true;
  CtrlZMarksEOF: boolean = false; (* #26 not considered as end of file *)

  sLineBreak = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsLF;

{$ifndef Solaris}
{$ifndef Darwin}
var argc:longint;external name 'operatingsystem_parameter_argc';
    argv:PPchar;external name 'operatingsystem_parameter_argv';
    envp:PPchar;external name 'operatingsystem_parameter_envp';
{$endif}
{$endif}

{
  $Log: sysunixh.inc,v $
  Revision 1.27  2005/05/12 20:29:16  michael
  + Added maxpathlen constant (maximum length of filename path)

  Revision 1.26  2005/04/03 21:10:59  hajny
    * EOF_CTRLZ conditional define replaced with CtrlZMarksEOF, #26 handling made more consistent (fix for bug 2453)

  Revision 1.25  2005/02/14 17:13:31  peter
    * truncate log

  Revision 1.24  2005/02/14 16:32:41  peter
    * solaris updates

  Revision 1.23  2005/02/06 11:20:52  peter
    * threading in system unit
    * removed systhrds unit

}
