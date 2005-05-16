{
    $Id: asltest.pas,v 1.2 2005/02/14 17:13:10 peter Exp $

    Using an asl.library requester
    Free Pascal for MorphOS example

    Copyright (C) 2005 by Karoly Balogh
    Based on work of Nils Sjoholm

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ * 2005.01.30 * }
{ * Needs MorphOS RTL 2005.01.30 or later! * }

program ASLtest;

uses exec, intuition, utility, asl;


function MessageBox(title, txt, gad: String) : LongInt;
var
  tmpReq: TEasyStruct;
begin
  title:=title+#0;
  txt:=txt+#0;
  gad:=gad+#0;
  with tmpReq do begin
    es_StructSize:=SizeOf(tEasyStruct);
    es_Flags:=0;
    es_Title:=@title[1];
    es_TextFormat:=@txt[1];
    es_GadgetFormat:=@gad[1];
  end;
  MessageBox:=EasyRequestArgs(NIL,@tmpReq,NIL,NIL);
end;

var
  FileReq  : PFileRequester;
  aslResult: Boolean;
begin
  { * Opening needed libraries * }
  InitIntuitionLibrary;
  InitAslLibrary;

  FileReq:=AllocAslRequestTags(ASL_FileRequest,[
                               ASLFR_InitialPattern,DWord(PChar('#?')),
                               ASLFR_TitleText,DWord(PChar('ASL Requester Test')),
                               ASLFR_DoPatterns,DWord(True),
                               TAG_DONE]);

  if FileReq<>NIL then begin
    aslResult:=AslRequest(FileReq,NIL);
    if aslResult then
      MessageBox('ASL Test Results',
                 'The path is: '+FileReq^.rf_Dir+#10+
                 'And the file is: '+FileReq^.rf_File,
                 'OK')
    else
      MessageBox('ASL Test Result',
                 'You canceled!',
                 'OK');

   FreeAslRequest(FileReq);
  end;
end.

{
  $Log: asltest.pas,v $
  Revision 1.2  2005/02/14 17:13:10  peter
    * truncate log

  Revision 1.1  2005/01/30 20:03:43  karoly
    * initial revision

}
