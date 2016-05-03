{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998-2003 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
    History:

    Added the defines use_amiga_smartlink and
    use_auto_openlib. Implemented autoopening
    of the library.
    14 Jan 2003.

    Changed startcode for unit.
    nils.sjoholm@mailbox.swipnet.se Nils Sjoholm
}

UNIT translator;

INTERFACE
USES exec;

Const

    TR_NotUsed          = -1;   { This is an oft used system rc }
    TR_NoMem            = -2;   { Can't allocate memory }
    TR_MakeBad          = -4;   { Error in MakeLibrary call }

VAR TranslatorBase : pLibrary = nil;

const
    TRANSLATORNAME : PChar = 'translator.library';

FUNCTION Translate(const inputString : pCHAR location 'a0'; inputLength : LONGINT location 'd0'; outputBuffer : pCHAR location 'a1'; bufferSize : LONGINT location 'd1') : LONGINT; syscall TranslatorBase 030;

IMPLEMENTATION

const
    { Change VERSION and LIBVERSION to proper values }
    VERSION : string[2] = '0';
    LIBVERSION : longword = 0;

initialization
  TranslatorBase := OpenLibrary(TRANSLATORNAME,LIBVERSION);
finalization
  if Assigned(TranslatorBase) then
    CloseLibrary(TranslatorBase);
END. (* UNIT TRANSLATOR *)
