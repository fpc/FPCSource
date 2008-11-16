{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

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
  TLineEndStr = string [3];
  TextBuf = array[0..TextRecBufSize-1] of AnsiChar;
  TextRec = Packed Record
    Handle    : THandle;
    Mode      : longint;
    bufsize   : SizeInt;
    _private  : SizeInt;
    bufpos,
    bufend    : SizeInt;
    bufptr    : ^textbuf;
    openfunc,
    inoutfunc,
    flushfunc,
    closefunc : pointer;
    UserData  : array[1..32] of byte;
    name      : array[0..(TextRecNameLength div SizeOf(RtlChar))-1] of RtlChar;
    LineEnd   : TLineEndStr;
    buffer    : textbuf;
  End;

