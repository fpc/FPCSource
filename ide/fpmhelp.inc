{
    $Id$
    This file is part of the ffff
    Copyright (c) 1998 by Berczi Gabor

    Help menu entries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure TIDEApp.HelpContents;
var FileID: word;
    Ctx   : THelpCtx;
var Found: boolean;
begin
  CheckHelpSystem;
  Found:=HelpFacility^.TopicSearch('Table of contents',FileID,Ctx);
  if Found then
     Help(FileID,Ctx,false)
  else
     Help(0,hcContents,false);
end;

procedure TIDEApp.HelpHelpIndex;
begin
  HelpIndex('');
end;

procedure TIDEApp.HelpTopicSearch;
begin
end;

procedure TIDEApp.HelpPrevTopic;
begin
  if HelpWindow=nil then HelpContents else
  with HelpWindow^ do
  if GetState(sfVisible) then Message(HelpWindow^.HelpView,evCommand,cmPrevTopic,nil)
     else begin HelpWindow^.Show; HelpWindow^.MakeFirst; end;
end;

procedure TIDEApp.HelpUsingHelp;
begin
  Help(0,hcUsingHelp,false);
end;

type
    PHelpFileListBox = ^THelpFileListBox;
    THelpFileListBox = object(TListBox)
      function GetText(Item: sw_Integer; MaxLen: sw_Integer): String; virtual;
    end;

    PHelpFilesDialog = ^THelpFilesDialog;
    THelpFilesDialog = object(TCenterDialog)
      constructor Init;
      procedure   HandleEvent(var Event: TEvent); virtual;
      destructor  Done; virtual;
    private
      LB: PHelpFileListBox;
      C : PUnsortedStringCollection;
    end;

function THelpFileListBox.GetText(Item: sw_Integer; MaxLen: sw_Integer): String;
var S: string;
    P: integer;
begin
  S:=inherited GetText(Item,MaxLen);
  P:=Pos('|',S);
  if P>0 then S:=copy(S,P+1,255)+' - '+copy(S,1,P-1);
  GetText:=copy(S,1,MaxLen);
end;

constructor THelpFilesDialog.Init;
var R,R2: TRect;
    SB: PScrollBar;
    I: integer;
begin
  R.Assign(0,0,round(ScreenWidth*5/8),ScreenHeight-10);
  inherited Init(R, dialog_helpfiles);

  New(C, Init(20,10));

  GetExtent(R); R.Grow(-2,-2); Inc(R.A.Y); R.B.X:=R.B.X-13;
  R2.Copy(R); R2.Move(1,0); R2.A.X:=R2.B.X-1;
  New(SB, Init(R2)); Insert(SB);
  New(LB, Init(R, 1, SB));

  for I:=0 to HelpFiles^.Count-1 do
    begin
      C^.Insert(NewStr(HelpFiles^.At(I)^));
    end;

  LB^.NewList(C);
  Insert(LB);
  R2.Copy(R); Dec(R2.A.Y); R2.B.Y:=R2.A.Y+1;
  Insert(New(PLabel, Init(R2, label_helpfiles_helpfiles, LB)));

  GetExtent(R); R.Grow(-2,-2); Inc(R.A.Y); R.A.X:=R.B.X-13+1; R.B.Y:=R.A.Y+2;
  Insert(New(PButton, Init(R, button_OK, cmOK, bfDefault)));
  R.Move(0,2);
  Insert(New(PButton, Init(R, button_New, cmAddItem, bfNormal)));
  R.Move(0,2);
  Insert(New(PButton, Init(R, button_Delete, cmDeleteItem, bfNormal)));
  R.Move(0,2);
  Insert(New(PButton, Init(R, button_Cancel, cmCancel, bfNormal)));

  LB^.Select;
end;

procedure THelpFilesDialog.HandleEvent(var Event: TEvent);
var I: integer;
    D: PFileDialog;
    FileName: string;
    Re: word;
    S: string;
    LS: PFPHTMLFileLinkScanner;
    BS: PBufStream;
begin
  case Event.What of
    evKeyDown :
      case Event.KeyCode of
        kbIns :
          begin
            Message(@Self,evCommand,cmAddItem,nil);
            ClearEvent(Event);
          end;
        kbDel :
          begin
            Message(@Self,evCommand,cmDeleteItem,nil);
            ClearEvent(Event);
          end;
      end;
    evCommand :
      case Event.Command of
        cmAddItem :
          begin
            S:='';
            New(D, Init(HelpFileExts,
              dialog_installhelpfile,
              label_installhelpfile_filename,
              fdOpenButton,hidOpenHelpFile));
            Re:=Desktop^.ExecView(D);
            if Re<>cmCancel then
            begin
              D^.GetFileName(FileName);
              if UpcaseStr(ExtOf(FileName))=UpcaseStr(HTMLIndexExt) then
                begin
                  S:='HTML Index';
                end
              else
              if UpcaseStr(copy(ExtOf(FileName),1,length(HTMLExt)))=UpcaseStr(HTMLExt) then
              begin
                Re:=ConfirmBox(msg_createkeywordindexforhelpfile,nil,true);
                if Re<>cmCancel then
                if Re=cmNo then
                  Re:=InputBox(dialog_topictitle,label_topictitle_title,S,40)
                else
                  begin
                    ShowMessage(msg_pleasewaitwhilecreatingindex);
                    S:='HTML Index';
                    PushStatus(FormatStrStr(msg_buildingindexfile,FileName));
                    New(LS, Init(DirOf(FileName)));
                    LS^.ProcessDocument(FileName,[soSubDocsOnly]);
                    if LS^.GetDocumentCount=0 then
                      begin
                        ErrorBox(FormatStrStr(msg_filedoesnotcontainanylinks,FileName),nil);
                        Re:=cmCancel;
                      end
                    else
                      begin
                        FileName:=DirAndNameOf(FileName)+HTMLIndexExt;
                        if ExistsFile(FileName) then
                          if ConfirmBox(FormatStrStr(msg_filealreadyexistsoverwrite,FileName),nil,true)<>cmYes then
                            Re:=cmCancel;
                        if Re<>cmCancel then
                        begin
                          PushStatus(FormatStrStr(msg_storinghtmlindexinfile,FileName));
                          New(BS, Init(FileName, stCreate, 4096));
                          if Assigned(BS)=false then
                            begin
                              ErrorBox(FormatStrStr(msg_cantcreatefile,FileName),nil);
                              Re:=cmCancel;
                            end
                          else
                            begin
                              LS^.StoreDocuments(BS^);
                              if BS^.Status<>stOK then
                                begin
                                  ErrorBox(FormatStrInt(msg_errorstoringindexdata,BS^.Status),nil);
                                  Re:=cmCancel;
                                end;
                              Dispose(BS, Done);
                            end;
                          PopStatus;
                        end;
                      end;
                    Dispose(LS, Done);
                    PopStatus;
                    HideMessage;
                  end;
              end;
            end;
            if Re<>cmCancel then
            begin
              if S<>'' then FileName:=FileName+'|'+S;
              LB^.List^.Insert(NewStr(FileName));
              LB^.SetRange(LB^.List^.Count);
              ReDraw;
            end;
            Dispose(D, Done);
            ClearEvent(Event);
          end;
        cmDeleteItem :
          if LB^.Range>0 then
          begin
            LB^.List^.AtFree(LB^.Focused);
            LB^.SetRange(LB^.List^.Count);
            ReDraw;
            ClearEvent(Event);
          end;
        cmOK :
          begin
            HelpFiles^.FreeAll;
            for I:=0 to LB^.List^.Count-1 do
              HelpFiles^.Insert(NewStr(C^.At(I)^));
          end;
      end;
  end;
  inherited HandleEvent(Event);
end;

destructor THelpFilesDialog.Done;
begin
  if C<>nil then
    begin
      C^.FreeAll;
      Dispose(C, Done);
    end;
  inherited Done;
end;

procedure TIDEApp.HelpFiles;
var
  PHFD : PHelpFilesDialog;
begin
  New(PHFD, Init);
  if Desktop^.ExecView(PHFD)=cmOK then
  begin
    DoneHelpSystem;
    Message(Application,evBroadcast,cmHelpFilesChanged,nil);
    InitHelpSystem;
  end;
  if assigned(PHFD) then
    Dispose(PHFD, Done);
end;

procedure TIDEApp.About;
begin
  ExecuteDialog(New(PFPAboutDialog, Init), nil);
end;

{
  $Log$
  Revision 1.3  2002-03-14 17:01:54  pierre
   + find Table of contents for FPC docs

  Revision 1.2  2001/10/01 00:24:09  pierre
   * fix several help problems

  Revision 1.1  2001/08/04 11:30:23  peter
    * ide works now with both compiler versions

  Revision 1.1.2.2  2001/03/20 00:20:42  pierre
   * fix some memory leaks + several small enhancements

  Revision 1.1.2.1  2000/10/18 21:53:27  pierre
   * several Gabor fixes

  Revision 1.1  2000/07/13 09:48:35  michael
  + Initial import

  Revision 1.11  2000/05/29 10:44:57  pierre
   + New bunch of Gabor's changes: see fixes.txt

  Revision 1.10  2000/05/02 08:42:28  pierre
   * new set of Gabor changes: see fixes.txt

  Revision 1.9  2000/04/25 08:42:33  pierre
   * New Gabor changes : see fixes.txt

  Revision 1.8  2000/02/07 08:29:13  michael
  [*] the fake (!) TOKENS.PAS still contained the typo bug
       FSplit(,n,d,e) (correctly FSplit(,d,n,e))
  [*] CodeComplete had a very ugly bug - coordinates were document-relative
      (instead of being screen-relative)
  [*] TResourceStream didn't count the size of the resource names when
      determining the file size and this could lead to the last resources not
      loaded correctly


  [+] Ctrl-Enter in editor now tries to open the file at cursor
  [+] CodeComplete option added to Options|Environment|Editor
  [+] user interface for managing CodeComplete implemented
  [+] user interface for CodeTemplates implemented
  [+] CodeComplete wordlist and CodeTemplates stored in desktop file
  [+] help topic size no longer limited to 64KB when compiled with FPC

  Revision 1.7  1999/05/22 13:44:31  peter
    * fixed couple of bugs

  Revision 1.6  1999/03/01 15:41:56  peter
    + Added dummy entries for functions not yet implemented
    * MenuBar didn't update itself automatically on command-set changes
    * Fixed Debugging/Profiling options dialog
    * TCodeEditor converts spaces to tabs at save only if efUseTabChars is
 set
    * efBackSpaceUnindents works correctly
    + 'Messages' window implemented
    + Added '$CAP MSG()' and '$CAP EDIT' to available tool-macros
    + Added TP message-filter support (for ex. you can call GREP thru
      GREP2MSG and view the result in the messages window - just like in TP)
    * A 'var' was missing from the param-list of THelpFacility.TopicSearch,
      so topic search didn't work...
    * In FPHELP.PAS there were still context-variables defined as word instead
      of THelpCtx
    * StdStatusKeys() was missing from the statusdef for help windows
    + Topic-title for index-table can be specified when adding a HTML-files

  Revision 1.5  1999/02/22 02:15:16  peter
    + default extension for save in the editor
    + Separate Text to Find for the grep dialog
    * fixed redir crash with tp7

  Revision 1.4  1999/01/21 11:54:18  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.3  1999/01/12 14:29:34  peter
    + Implemented still missing 'switch' entries in Options menu
    + Pressing Ctrl-B sets ASCII mode in editor, after which keypresses (even
      ones with ASCII < 32 ; entered with Alt+<###>) are interpreted always as
      ASCII chars and inserted directly in the text.
    + Added symbol browser
    * splitted fp.pas to fpide.pas

  Revision 1.2  1998/12/28 15:47:48  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.2  1998/12/22 10:39:47  peter
    + options are now written/read
    + find and replace routines

}
