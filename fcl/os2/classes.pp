{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

    Classes unit for OS/2

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}

{ determine the type of the resource/form file }
{$define Win16Res}

unit Classes;

interface

uses
  DosCalls, (* Needed here (i.e. before SysUtils) to avoid type clashes. *)
  strings,
  sysutils;

{$i classesh.inc}

implementation

uses
  typinfo;

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}

initialization
  CommonInit;

finalization
  CommonCleanup;

end.
{
  $Log$
  Revision 1.1.2.2  2000-12-19 00:44:06  hajny
    + FCL made compilable under OS/2

  Revision 1.1.2.1  2000/08/25 17:30:36  hajny
    * Cosmetic change (OS/2 instead of win32 in header)

  Revision 1.1  2000/07/13 06:33:44  michael
  + Initial import

  Revision 1.10  2000/07/01 12:28:38  sg
  * Added initialisation and finalisation code which is common to all
    platforms.

  Revision 1.9  2000/04/01 10:45:52  hajny
    OS/2 implementation started

  Revision 1.8  2000/01/07 01:24:34  peter
    * updated copyright to 2000

  Revision 1.7  2000/01/06 01:20:34  peter
    * moved out of packages/ back to topdir

  Revision 1.2  2000/01/04 18:10:03  michael
  + Added typinfo unit

  Revision 1.1  2000/01/03 19:33:09  peter
    * moved to packages dir

  Revision 1.5  1999/05/30 10:46:42  peter
    * start of tthread for linux,win32

}
