{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2002 by the Free Pascal development team

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
  Revision 1.5  2002-02-10 13:38:14  hajny
    * DosCalls dependency removed to avoid type redefinitions

  Revision 1.4  2000/12/19 00:43:07  hajny
    + FCL made compilable under OS/2

  Revision 1.3  2000/08/25 17:32:16  hajny
    * Cosmetic change (OS/2 instead of win32 in header)

  Revision 1.2  2000/07/13 11:33:01  michael
  + removed logs
 
}
