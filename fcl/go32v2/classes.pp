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
  typinfo,
  sysutils;

{$i classesh.inc}

implementation

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}

end.
{
  $Log$
  Revision 1.7  2000-01-07 00:01:33  peter
    * uses typinfo moved to interface

  Revision 1.6  2000/01/06 01:20:32  peter
    * moved out of packages/ back to topdir

  Revision 1.2  2000/01/04 18:09:29  michael
  + Added typinfo unit

  Revision 1.1  2000/01/03 19:33:06  peter
    * moved to packages dir

  Revision 1.4  1999/05/30 10:46:37  peter
    * start of tthread for linux,win32

}
