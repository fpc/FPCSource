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
  strings,
  sysutils;

{$i classesh.inc}

implementation

uses
  windows,typinfo;

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}

end.
{
  $Log$
  Revision 1.11  2000-01-06 01:20:36  peter
    * moved out of packages/ back to topdir

  Revision 1.2  2000/01/04 18:08:35  michael
  + Added typinfo unit

  Revision 1.1  2000/01/03 19:33:11  peter
    * moved to packages dir

  Revision 1.9  1999/05/30 10:46:45  peter
    * start of tthread for linux,win32

}
