{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

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
  sysutils,
  types,
  typinfo,
  rtlconst,
  systhrds;
	

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
  $Log$
  Revision 1.5  2004-09-26 19:25:49  armin
  * exiting threads at nlm unload

  Revision 1.4  2004/08/01 20:02:48  armin
  * changed dir separator from \ to /
  * long namespace by default
  * dos.exec implemented
  * getenv ('PATH') is now supported
  * changed FExpand to global version
  * fixed heaplist growth error
  * support SysOSFree
  * stackcheck was without saveregisters
  * fpc can compile itself on netware

  Revision 1.3  2004/01/22 17:11:23  peter
    * classes uses types to import TPoint and TRect

  Revision 1.2  2004/01/10 20:15:21  michael
  + Some more fixes to rtlconst. Const strings moved from classes to rtlconst

  Revision 1.1  2003/10/06 21:01:06  peter
    * moved classes unit to rtl

  Revision 1.1  2003/03/25 17:56:19  armin
  * first fcl implementation for netware

  Revision 1.3  2002/09/07 15:15:28  peter
    * old logs removed and tabs fixed

}
