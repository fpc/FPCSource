{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998-2000 by the Free Pascal development team

    FileRec record definition


    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  This file contains the definition of the filerec record.
  It is put separately, so it is available outside the system
  unit without sacrificing TP compatibility.
}

const
  filerecnamelength = 255;
type
  FileRec = Packed Record
    Handle,
    Mode,
    RecSize   : longint;
    _private  : array[1..32] of byte;
    UserData  : array[1..16] of byte;
    name      : array[0..filerecnamelength] of char;
  End;

{
  $Log$
  Revision 1.6  2000-01-07 16:32:24  daniel
    * copyright 2000 added

  Revision 1.5  1998/09/14 10:48:15  peter
    * FPC_ names
    * Heap manager is now system independent

  Revision 1.4  1998/09/04 18:16:13  peter
    * uniform filerec/textrec (with recsize:longint and name:0..255)

  Revision 1.3  1998/05/21 11:55:59  carl
   * works with all OS
}
