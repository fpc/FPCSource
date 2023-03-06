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

    Added the define use_amiga_smartlink.
    13 Jan 2003.

    nils.sjoholm@mailbox.swipnet.se Nils Sjoholm
}

{$IFNDEF FPC_DOTTEDUNITS}
unit AMsgBox;
{$ENDIF FPC_DOTTEDUNITS}

interface


FUNCTION MessageBox(const tit,txt,gad:RawByteString): LongInt;
FUNCTION MessageBox(const tit,txt,gad:AnsiString): LongInt;
function MessageBox(const tit,txt,gad:PAnsiChar): LongInt;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  Amiga.Core.Intuition;
{$ELSE FPC_DOTTEDUNITS}
uses
  intuition;
{$ENDIF FPC_DOTTEDUNITS}

FUNCTION MessageBox(const tit,txt,gad:RawByteString): LongInt;
begin
  MessageBox:=MessageBox(PAnsiChar(tit),PAnsiChar(txt),PAnsiChar(gad));
end;

FUNCTION MessageBox(const tit,txt,gad:AnsiString) : LONGint;
begin
  MessageBox := MessageBox(PAnsiChar(RawByteString(tit)),PAnsiChar(RawByteString(txt)),PAnsiChar(RawByteString(gad)));
end;

FUNCTION MessageBox(const tit,txt,gad:PAnsiChar) : LONGint;
VAR
  MyStruct : tEasyStruct;
BEGIN
  MyStruct.es_StructSize:=SizeOf(tEasyStruct);
  MyStruct.es_Flags:=0;
  MyStruct.es_Title:=(tit);
  MyStruct.es_TextFormat:=(txt);
  MyStruct.es_GadgetFormat:=(gad);
  MessageBox := EasyRequestArgs(nil,@MyStruct,NIL,NIL);
END;

end.
