{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

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
  typinfo,
  systhrds;

{$i classesh.inc}

implementation

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}

end.
{
  $Log$
  Revision 1.1  2003-10-06 21:01:06  peter
    * moved classes unit to rtl

  Revision 1.1  2003/03/25 17:56:19  armin
  * first fcl implementation for netware

  Revision 1.3  2002/09/07 15:15:28  peter
    * old logs removed and tabs fixed

}
