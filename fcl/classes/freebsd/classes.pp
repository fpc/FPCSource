{
    $Id: classes.pp,v 1.2 2005/02/14 17:13:11 peter Exp $
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
  typinfo;

{$i classesh.inc}

implementation

uses
{$ifdef ver1_0}
  linux
{$else}
  baseunix,unix
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
  $Log: classes.pp,v $
  Revision 1.2  2005/02/14 17:13:11  peter
    * truncate log

}
