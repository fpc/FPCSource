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
  rtlconst,
  sysutils,
  types,
  typinfo,
  windows;

{$i classesh.inc}

implementation

uses
  sysconst;

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}

initialization
  CommonInit;
  {$ifndef ver1_0}
  InitCriticalSection(SynchronizeCritSect);
  ExecuteEvent := RtlEventCreate;
  SynchronizeMethod := nil;
  {$endif}
finalization
  CommonCleanup;
  {$ifndef ver1_0}
    DoneCriticalSection(SynchronizeCritSect);
  RtlEventDestroy(ExecuteEvent);
  {$endif}
end.
{
  $Log$
  Revision 1.7  2005-02-06 13:06:20  peter
    * moved file and dir functions to sysfile/sysdir
    * win32 thread in systemunit

  Revision 1.6  2004/12/23 09:42:42  marco
   * first tthread.synchronize support (merged neli's patches)

  Revision 1.5  2004/01/22 17:11:23  peter
    * classes uses types to import TPoint and TRect

  Revision 1.4  2004/01/13 18:04:25  florian
    * make win32 fixed

  Revision 1.3  2004/01/10 19:35:18  michael
  + Moved all resource strings to rtlconst/sysconst

  Revision 1.2  2003/10/07 16:20:21  florian
    * win32 now uses aliases from the windows unit for types like trect

  Revision 1.1  2003/10/06 21:01:07  peter
    * moved classes unit to rtl

  Revision 1.1  2003/10/06 20:33:58  peter
    * classes moved to rtl for 1.1
    * classes .inc and classes.pp files moved to fcl/classes for
      backwards 1.0.x compatiblity to have it in the fcl

  Revision 1.4  2002/10/14 19:46:13  peter
    * threading switch

  Revision 1.3  2002/09/07 15:15:29  peter
    * old logs removed and tabs fixed

}
