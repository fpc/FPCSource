{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

UNIT translator;

INTERFACE
USES exec;

Const

    TR_NotUsed          = -1;   { This is an oft used system rc }
    TR_NoMem            = -2;   { Can't allocate memory }
    TR_MakeBad          = -4;   { Error in MakeLibrary call }

VAR TranslatorBase : pLibrary;

FUNCTION Translate(inputString : pCHAR; inputLength : LONGINT; outputBuffer : pCHAR; bufferSize : LONGINT) : LONGINT;

IMPLEMENTATION

FUNCTION Translate(inputString : pCHAR; inputLength : LONGINT; outputBuffer : pCHAR; bufferSize : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L inputString,A0
    MOVE.L  inputLength,D0
    MOVEA.L outputBuffer,A1
    MOVE.L  bufferSize,D1
    MOVEA.L TranslatorBase,A6
    JSR -030(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

END. (* UNIT TRANSLATOR *)
