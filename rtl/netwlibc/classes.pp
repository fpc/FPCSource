{
    $Id$
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
  rtlconst,
  systhrds,
  Libc;


{$i classesh.inc}

implementation

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}

end.
{
  $Log$
  Revision 1.1  2004-09-05 20:58:47  armin
  * first rtl version for netwlibc

}
