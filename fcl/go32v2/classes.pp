{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by Michael Van Canneyt and Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE OBJFPC}

{ determine the type of the resource/form file }
{$define Win16Res}
unit Classes;

interface

uses
  strings,
  sysutils;

{$i classesh.inc}

implementation

{ OS-dependent file handling. }
{$i osfile.inc}

{ OS - independent class implementations are in /inc directory. }
{$i classes.inc}

end.

{
  $Log$
  Revision 1.2  1998-11-04 10:46:41  peter
    * exceptions work

  Revision 1.1  1998/11/04 10:15:13  peter
    * fixes to compile
}
