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
  Revision 1.8  2005-02-14 17:13:32  peter
    * truncate log

  Revision 1.7  2005/02/06 13:06:20  peter
    * moved file and dir functions to sysfile/sysdir
    * win32 thread in systemunit

}
