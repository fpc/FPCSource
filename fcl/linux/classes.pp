{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by Michael Van Canneyt and Florian Klaempfl

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
  strings,
  sysutils;

{$i classesh.inc}

implementation

uses
  linux;
  
{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}


initialization

finalization
  if ThreadsInited then
   DoneThreads;

end.
{
  $Log$
  Revision 1.11  1999-05-30 10:46:41  peter
    * start of tthread for linux,win32

}
