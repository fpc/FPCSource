{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998-2002 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit MsgBox;

interface



FUNCTION MessageBox(tit,txt,gad:string) : LONGint;
function MessageBox(tit,txt,gad:pchar):longint;

implementation

uses pastoc;
type
 pEasyStruct = ^tEasyStruct;
   tEasyStruct = record
    es_StructSize   : longint;  { should be sizeof (struct EasyStruct )}
    es_Flags        : longint;  { should be 0 for now                  }
    es_Title        : pchar;   { title of requester window            }
    es_TextFormat   : pchar;   { 'printf' style formatting string     }
    es_GadgetFormat : pchar;   { 'printf' style formatting string   }
   END;

FUNCTION EasyRequestArgs(window : pointer; easyStruct : pEasyStruct; idcmpPtr : longint; args : POINTER) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L window,A0
    MOVEA.L easyStruct,A1
    MOVEA.L idcmpPtr,A2
    MOVEA.L args,A3
    MOVEA.L _IntuitionBase,A6
    JSR -588(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION MessageBox(tit,txt,gad:string) : LONGint;
begin
    MessageBox := MessageBox(pas2c(tit),pas2c(txt),pas2c(gad));
end;

FUNCTION MessageBox(tit,txt,gad:pchar) : LONGint;
VAR
  MyStruct : tEasyStruct;
BEGIN
 MyStruct.es_StructSize:=SizeOf(tEasyStruct);
 MyStruct.es_Flags:=0;
 MyStruct.es_Title:=(tit);
 MyStruct.es_TextFormat:=(txt);
 MyStruct.es_GadgetFormat:=(gad);
 MessageBox := EasyRequestArgs(nil,@MyStruct,0,NIL);
END;

end.

{
  $Log$
  Revision 1.1  2002-11-22 21:34:59  nils

    * initial release

}

  
