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
    
    nils.sjoholm@mailbox.swipnet.se Nils Sjoholm
}

{$I useamigasmartlink.inc}
{$ifdef use_amiga_smartlink}
   {$smartlink on}
{$endif use_amiga_smartlink}

UNIT translator;

INTERFACE
USES exec;

Const

    TR_NotUsed          = -1;   { This is an oft used system rc }
    TR_NoMem            = -2;   { Can't allocate memory }
    TR_MakeBad          = -4;   { Error in MakeLibrary call }

VAR TranslatorBase : pLibrary;

const
    TRANSLATORNAME : PChar = 'translator.library';

FUNCTION Translate(inputString : pCHAR; inputLength : LONGINT; outputBuffer : pCHAR; bufferSize : LONGINT) : LONGINT;

IMPLEMENTATION

uses msgbox;

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

{$I useautoopenlib.inc}
{$ifdef use_auto_openlib}
  {$Info Compiling autoopening of translator.library}

var
    translator_exit : Pointer;

procedure ClosetranslatorLibrary;
begin
    ExitProc := translator_exit;
    if TranslatorBase <> nil then begin
        CloseLibrary(TranslatorBase);
        TranslatorBase := nil;
    end;
end;

const
    { Change VERSION and LIBVERSION to proper values }

    VERSION : string[2] = '0';
    LIBVERSION : Cardinal = 0;

begin
    TranslatorBase := nil;
    TranslatorBase := OpenLibrary(TRANSLATORNAME,LIBVERSION);
    if TranslatorBase <> nil then begin
        translator_exit := ExitProc;
        ExitProc := @ClosetranslatorLibrary
    end else begin
        MessageBox('FPC Pascal Error',
        'Can''t open translator.library version ' + VERSION + #10 +
        'Deallocating resources and closing down',
        'Oops');
        halt(20);
    end;

{$else}
   {$Warning No autoopening of translator.library compiled}
   {$Info Make sure you open translator.library yourself}
{$endif use_auto_openlib}

END. (* UNIT TRANSLATOR *)
