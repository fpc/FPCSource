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
  types,
  typinfo,
  rtlconst;

{$i classesh.inc}

implementation

uses
  BaseUnix,unix
  ;

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}


initialization
  CommonInit;
finalization
  CommonCleanup;

{$ifndef VER1_0}
  if ThreadsInited then
     DoneThreads;
{$endif}
end.
{
  $Log$
  Revision 1.2  2005-02-14 17:13:31  peter
    * truncate log

  Revision 1.1  2005/02/06 12:16:52  peter
    * bsd thread updates

  Revision 1.8  2005/02/06 11:20:52  peter
    * threading in system unit
    * removed systhrds unit

}
