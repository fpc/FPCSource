{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998-2000 by the Free Pascal development team

    Textrec record definition


    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  This file contains the definition of the textrec record.
  It is put separately, so it is available outside the system
  unit without sacrificing TP compatibility.
}

const
  TextRecNameLength = 256;
  TextRecBufSize    = 256;
type
  TextBuf = array[0..TextRecBufSize-1] of char;
  TextRec = Packed Record
    Handle,
    Mode,
    bufsize,
    _private,
    bufpos,
    bufend    : longint;
    bufptr    : ^textbuf;
    openfunc,
    inoutfunc,
    flushfunc,
    closefunc : pointer;
    UserData  : array[1..16] of byte;
    name      : array[0..textrecnamelength-1] of char;
    buffer    : textbuf;
  End;

{
  $Log$
  Revision 1.7  2000-01-07 16:32:25  daniel
    * copyright 2000 added

  Revision 1.6  1998/09/14 10:48:25  peter
    * FPC_ names
    * Heap manager is now system independent

  Revision 1.5  1998/09/08 10:14:07  peter
    + textrecbufsize

  Revision 1.4  1998/09/04 18:16:15  peter
    * uniform filerec/textrec (with recsize:longint and name:0..255)

  Revision 1.3  1998/05/21 15:37:19  carl
   + working version now ok
}
