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
  typinfo;

{$i classesh.inc}

implementation

uses
{$ifdef ver1_0}
  linux
{$else}
  unix
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
  $Log$
  Revision 1.1  2002-07-30 16:03:29  marco
   * Added for OpenBSD. Plain copy of NetBSD

  Revision 1.1  2001/09/05 14:30:04  marco
   * NetBSD fcl makefile fixes. Plain copy from FreeBSD

  Revision 1.4  2001/04/08 11:26:03  peter
    * update so it can be compiled by both 1.0.x and 1.1

  Revision 1.3  2001/01/21 20:45:09  marco
   * Rename fest II  FCL version.

  Revision 1.2  2000/10/26 22:30:21  peter
    * freebsd update
    * classes.rst

  Revision 1.1.2.1  2000/10/17 13:47:43  marco
   * Copy of fcl/linux dir with adapted makefiles to ease FreeBSD 1.0.2
  packaging

  Revision 1.1  2000/07/13 06:31:32  michael
  + Initial import

  Revision 1.19  2000/07/01 09:49:02  peter
    * fixed go32v2,win32 build

  Revision 1.18  2000/06/29 16:32:06  sg
  * Changes in initialisation/finalisation for streaming support

  Revision 1.17  2000/06/27 17:17:34  lazarus
  Added code for registerclass, GetClass and RegisterClasses
  Shane

  Revision 1.16  2000/06/27 15:55:19  lazarus
  Added TThreadlist code.   Shane

  Revision 1.15  2000/01/07 01:24:34  peter
    * updated copyright to 2000

  Revision 1.14  2000/01/07 00:01:33  peter
    * uses typinfo moved to interface

  Revision 1.13  2000/01/06 01:20:33  peter
    * moved out of packages/ back to topdir

  Revision 1.2  2000/01/04 18:07:58  michael
  + Added typinfo unit

  Revision 1.1  2000/01/03 19:33:09  peter
    * moved to packages dir

  Revision 1.11  1999/05/30 10:46:41  peter
    * start of tthread for linux,win32

}
