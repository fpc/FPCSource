{
    $Id: classes.pp,v 1.2 2005/02/14 17:13:12 peter Exp $
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

{ Require threading }
{$ifndef ver1_0}
  {$threading on}
{$endif ver1_0}

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
  windows;

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}

initialization
  CommonInit;

finalization
  CommonCleanup;

end.
{
  $Log: classes.pp,v $
  Revision 1.2  2005/02/14 17:13:12  peter
    * truncate log

}
