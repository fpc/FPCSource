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
  baseunix,unix,Systhrds
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

  Revision 1.1  2004/01/04 20:05:38  jonas
    * first working version of the Darwin/Mac OS X (for PowerPC) RTL
      Several non-essential units are still missing, but make cycle works

  Revision 1.4  2003/12/22 16:16:33  marco
   * small 1.0 compat fix

  Revision 1.3  2003/11/17 10:05:51  marco
   * threads for FreeBSD. Not working tho

  Revision 1.2  2003/10/09 10:55:20  marco
   * fix for moving classes to rtl while cycling with 1.0 start

  Revision 1.1  2003/10/06 21:01:06  peter
    * moved classes unit to rtl

  Revision 1.1  2003/10/06 20:33:58  peter
    * classes moved to rtl for 1.1
    * classes .inc and classes.pp files moved to fcl/classes for
      backwards 1.0.x compatiblity to have it in the fcl

  Revision 1.6  2003/09/20 12:38:29  marco
   * FCL now compiles for FreeBSD with new 1.1. Now Linux.

  Revision 1.5  2002/09/07 15:15:24  peter
    * old logs removed and tabs fixed

}
