{
    $Id: classes.pp,v 1.5 2005/03/07 17:57:25 peter Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2004 by Michael Van Canneyt and Florian Klaempfl

    Classes unit for netware libc

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
  rtlconsts,
  Libc;


{$i classesh.inc}

implementation

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}

initialization
  CommonInit;

finalization
  DoneThreads;
  CommonCleanup;

end.
{
  $Log: classes.pp,v $
  Revision 1.5  2005/03/07 17:57:25  peter
    * renamed rtlconst to rtlconsts

  Revision 1.4  2005/02/14 17:13:30  peter
    * truncate log

  Revision 1.3  2005/02/06 16:57:18  peter
    * threads for go32v2,os,emx,netware

}
