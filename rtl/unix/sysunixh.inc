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

{$I heaph.inc}

{$ifdef cpum68k}
{ used for single computations }
const
  BIAS4 = $7f-1;
{$endif cpum68k}

{Platform specific information}
type
  { fd are int in C also for 64bit targets (x86_64) }
  THandle = Longint;

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

{$ifndef Darwin}
var argc:longint;external name 'operatingsystem_parameter_argc';
    argv:PPchar;external name 'operatingsystem_parameter_argv';
    envp:PPchar;external name 'operatingsystem_parameter_envp';
{$endif}

{
  $Log$
  Revision 1.21  2004-07-03 22:52:33  daniel
    * No envp,argv,argc declaration for Darwin

  Revision 1.20  2004/07/03 21:50:31  daniel
    * Modified bootstrap code so separate prt0.as/prt0_10.as files are no
      longer necessary

  Revision 1.19  2004/05/18 20:16:23  peter
    * unix thandle is always 32bit
    * textrec fixed for 64bit

  Revision 1.18  2003/10/17 20:49:02  olle
    * Changed m68k to cpum68k

  Revision 1.17  2003/10/16 15:43:13  peter
    * THandle is platform dependent

  Revision 1.16  2002/10/15 19:30:15  peter
    * remove threadh.inc include

  Revision 1.15  2002/09/07 16:01:28  peter
    * old logs removed and tabs fixed

  Revision 1.14  2002/07/01 16:29:05  peter
    * sLineBreak changed to normal constant like Kylix

}
