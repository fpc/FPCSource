{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

uses
{$ifdef ver1_0}
  Linux,
{$else}
  Unix,
{$endif}
  classes;

const
  MaxHandle = SizeOf(TFDSet) * 8 - 1;

Type

  TIOData = Record
    ReadMap, WriteMap: TFDSet;
    end;

{
  $Log$
  Revision 1.4  2001-04-08 11:26:03  peter
    * update so it can be compiled by both 1.0.x and 1.1

  Revision 1.3  2001/01/21 20:45:09  marco
   * Rename fest II  FCL version.

  Revision 1.2  2000/10/26 22:30:21  peter
    * freebsd update
    * classes.rst

  Revision 1.1.2.1  2000/10/25 12:36:24  marco
   * Split up Freebsd/Unix/Linux for Fixes FCL

  Revision 1.1  2000/07/13 06:31:32  michael
  + Initial import

  Revision 1.1  2000/02/18 23:14:10  michael
  + Initial implementation

}