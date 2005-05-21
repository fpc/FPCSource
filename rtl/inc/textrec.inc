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
  TextBuf = array[0..TextRecBufSize-1] of char;
  TextRec = Packed Record
    Handle    : THandle;
    Mode      : longint;
    bufsize   : SizeInt;
{$IFDEF FPC_LINEEND_IN_TEXTREC}
    _private  : SizeInt;
{$ELSE FPC_LINEEND_IN_TEXTREC}
    LineEnd   : TLineEndStr;
{$ENDIF FPC_LINEEND_IN_TEXTREC}
    bufpos,
    bufend    : SizeInt;
    bufptr    : ^textbuf;
    openfunc,
    inoutfunc,
    flushfunc,
    closefunc : pointer;
{$ifndef FPC_HASUSERDATA32}    
    UserData  : array[1..16] of byte;
{$else FPC_HASUSERDATA32}    
    UserData  : array[1..32] of byte;
{$endif FPC_HASUSERDATA32}
    name      : array[0..textrecnamelength-1] of char;
{$IFDEF FPC_LINEEND_IN_TEXTREC}
    LineEnd   : TLineEndStr;
{$ENDIF FPC_LINEEND_IN_TEXTREC}
    buffer    : textbuf;
  End;

{
  $Log: textrec.inc,v $
  Revision 1.9  2005/02/26 15:42:45  florian
    * userdata in file/textrecs now 32 bytes

  Revision 1.8  2005/02/14 17:13:29  peter
    * truncate log

}
