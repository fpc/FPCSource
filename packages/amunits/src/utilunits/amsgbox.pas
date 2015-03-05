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
{$PACKRECORDS 2}

unit AMsgBox;

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


FUNCTION EasyRequestArgs(window : pointer location 'a0'; easyStruct : pEasyStruct location 'a1'; idcmpPtr : longint location 'a2'; args : POINTER location 'a3') : LONGINT; syscall _IntuitionBase 588;

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
