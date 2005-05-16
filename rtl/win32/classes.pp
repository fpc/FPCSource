{
    $Id: classes.pp,v 1.10 2005/04/09 07:23:07 florian Exp $
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
  rtlconsts,
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
  
finalization
  CommonCleanup;
end.
{
  $Log: classes.pp,v $
  Revision 1.10  2005/04/09 07:23:07  florian
    * applied Jesus Reyes win32 crash fixed

  Revision 1.9  2005/03/07 17:57:26  peter
    * renamed rtlconst to rtlconsts

  Revision 1.8  2005/02/14 17:13:32  peter
    * truncate log

  Revision 1.7  2005/02/06 13:06:20  peter
    * moved file and dir functions to sysfile/sysdir
    * win32 thread in systemunit

}
