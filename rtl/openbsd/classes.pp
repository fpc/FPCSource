{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

    Classes unit for linux

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
  rtlconst,
  typinfo;

{$i classesh.inc}

implementation

uses
{$ifdef ver1_0}
  linux
{$else}
  unix
{$endif}
  ;

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}


initialization
  CommonInit;

finalization
  CommonCleanup;

  if ThreadsInited then
     DoneThreads;

end.
{
  $Log$
  Revision 1.2  2004-01-10 20:15:21  michael
  + Some more fixes to rtlconst. Const strings moved from classes to rtlconst

  Revision 1.1  2003/10/06 21:01:06  peter
    * moved classes unit to rtl

  Revision 1.2  2002/09/07 15:15:27  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/07/30 16:03:29  marco
   * Added for OpenBSD. Plain copy of NetBSD

}
