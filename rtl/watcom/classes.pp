{
    $Id: classes.pp,v 1.4 2005/04/17 17:33:40 hajny Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

    Classes unit for Watcom

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
  typinfo,
  rtlconsts,
  sysutils;

{$i classesh.inc}

implementation

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}

initialization
  CommonInit;

finalization
  CommonCleanup;

end.
{
  $Log: classes.pp,v $
  Revision 1.4  2005/04/17 17:33:40  hajny
    * more rtlconst/s fixes

  Revision 1.3  2005/02/14 17:13:32  peter
    * truncate log

}
