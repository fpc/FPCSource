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

uses linux,classes;

const
  MaxHandle = SizeOf(TFDSet) * 8 - 1;

Type

  TIOData = Record
    ReadMap, WriteMap: TFDSet;
    end;

{
  $Log$
  Revision 1.1  2000-02-18 23:14:10  michael
  + Initial implementation

}