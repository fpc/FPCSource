{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by Michael Van Canneyt and Florian Klaempfl

    Classes unit for win32

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
  sysutils,
  typinfo;

{$i classesh.inc}

implementation

uses
  windows;

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}

initialization
  CommonInit;

finalization
  CommonCleanup;

end.
{
  $Log$
  Revision 1.1  2000-07-13 06:33:49  michael
  + Initial import

  Revision 1.13  2000/07/01 12:28:38  sg
  * Added initialisation and finalisation code which is common to all
    platforms.

  Revision 1.12  2000/01/07 00:01:33  peter
    * uses typinfo moved to interface

  Revision 1.11  2000/01/06 01:20:36  peter
    * moved out of packages/ back to topdir

  Revision 1.2  2000/01/04 18:08:35  michael
  + Added typinfo unit

  Revision 1.1  2000/01/03 19:33:11  peter
    * moved to packages dir

  Revision 1.9  1999/05/30 10:46:45  peter
    * start of tthread for linux,win32

}
