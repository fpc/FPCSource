{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    File menu entries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure TIDEApp.NewEditor;
begin
  OpenEditorWindow(nil,'',0,0);
end;


procedure TIDEApp.NewFromTemplate;
var D: PCenterDialog;
    R,R2: TRect;
    SB: PScrollBar;
    LB: PAdvancedListBox;
    I: integer;
    C: PUnsortedStringCollection;
    TE: PSourceWindow;
begin
  if GetTemplateCount=0 then
     begin InformationBox(msg_notemplatesavailable,nil); Exit; end;
  New(C, Init(10,10));
  R.Assign(0,0,40,14);
  New(D, Init(R, dialog_newfromtemplate));
  with D^ do
  begin
    HelpCtx:=hcNewFromTemplate;
    GetExtent(R); R.Grow(-2,-2); Inc(R.A.Y); Dec(R.B.X,12);
    R2.Copy(R); R2.Move(1,0); R2.A.X:=R2.B.X-1;
    New(SB, Init(R2)); Insert(SB);

    New(LB, Init(R,1,SB));
    LB^.Default:=true;
    for I:=0 to GetTemplateCount-1 do
      C^.Insert(NewStr(GetTemplateName(I)));
    LB^.NewList(C);
    Insert(LB);
    Dec(R.A.Y); R.B.Y:=R.A.Y+1;
    Insert(New(PLabel, Init(R, label_availabletemplates, LB)));

    GetExtent(R2); R2.Grow(-2,-3); R2.A.X:=R.B.X+2; R2.B.Y:=R2.A.Y+2;
    Insert(New(PButton, Init(R2, button_OK, cmOK, bfDefault)));
    R2.Move(0,2);
    Insert(New(PButton, Init(R2, button_Cancel, cmCancel, bfNormal)));
  end;
  LB^.Select;
  if Desktop^.ExecView(D)=cmOK then
  begin
{    Desktop^.Lock;}
    TE:=OpenEditorWindow(nil,'',0,0);
    if TE<>nil then
    begin
      StartTemplate(LB^.Focused,TE^.Editor);
      TE^.Editor^.SetModified(false); { if nothing changes, we don't need to save it }
(*      TE^.Hide; { we need this trick to get the editor updated }
      TE^.Show;*)
    end;
{    Desktop^.UnLock;}
  end;
  Dispose(D, Done);
  Dispose(C, Done);
end;


procedure TIDEApp.Open(FileName: string);
var D: PFileDialog;
    OpenIt: boolean;
    DriveNumber : byte;
    StoreDir,StoreDir2 : DirStr;
    NewPSW : PSourceWindow;
begin
  OpenIt:=FileName<>'';
  DriveNumber:=0;
  if not OpenIt then
   begin
     GetDir(0,StoreDir);
     if (Length(FileDir)>1) and (FileDir[2]=':') then
       begin
         { does not assume that lowercase are greater then uppercase ! }
         if (FileDir[1]>='a') and (FileDir[1]<='z') then
           DriveNumber:=Ord(FileDir[1])-ord('a')+1
         else
           DriveNumber:=Ord(FileDir[1])-ord('A')+1;
         GetDir(DriveNumber,StoreDir2);
{$ifndef FPC}
         ChDir(Copy(FileDir,1,2));
           { sets InOutRes in win32 PM }
{$endif not FPC}
       end;
     if (FileDir<>'') and ExistsDir(FileDir) then
       ChDir(TrimEndSlash(FileDir));
     New(D, Init(OpenExts,dialog_openafile,label_filetoopen,fdOpenButton,hidOpenSourceFile));
     D^.HelpCtx:=hcOpen;
     OpenIt:=Desktop^.ExecView(D)<>cmCancel;
     { if I go to root under go32v2 and there is no
       floppy I get a InOutRes = 152
       get rid of it ! }
     EatIO;
     if OpenIt then
       Begin
         D^.GetFileName(FileName);
         OpenExts:=D^.WildCard;
         if ExistsDir(DirOf(FExpand(FileName))) then
           FileDir:=DirOf(FExpand(FileName));
       End;
     Dispose(D, Done);
     if DriveNumber<>0 then
       ChDir(TrimEndSlash(StoreDir2));
{$ifndef FPC}
     if (Length(StoreDir)>1) and (StoreDir[2]=':') then
       ChDir(Copy(StoreDir,1,2));
{$endif not FPC}
     ChDir(TrimEndSlash(StoreDir));
   end;
  if OpenIt then
   begin
     FileName:=FExpand(LocatePasFile(FileName));
     if ExistsFile(FileName) then
     { like for BP unexistant files should  be created PM }
       OpenEditorWindow(nil,FileName,0,0)
     else
       {ErrorBox(FormatStrStr(msg_cantfindfile,FileName),nil);}
       begin
         NewPSW:=OpenEditorWindow(nil,'',0,0);
         NewPSW^.Editor^.FileName:=FileName;
         NewPSW^.SetTitle(FileName);
         Message(Application,evBroadcast,cmFileNameChanged,NewPSW^.Editor);
       end;
   end;
end;

function TIDEApp.OpenSearch(FileName: string) : boolean;
var D: PFileDialog;
    OpenIt: boolean;
    P : PString;
    Dir,S : String;
begin
  OpenIt:=False;
  if not OpenIt then
   begin
     ClearFormatParams; AddFormatParamStr(FileName);
     FormatStr(S,label_lookingfor,FormatParams);
     New(D, Init(FileName,dialog_openafile,S,fdOpenButton,hidOpenSourceFile));
     D^.HelpCtx:=hcOpen;
     OpenIt:=Desktop^.ExecView(D)<>cmCancel;
     if OpenIt then
       Begin
         D^.GetFileName(FileName);
       End;
     Dispose(D, Done);
   end;
  if OpenIt then
   begin
     FileName:=FExpand(LocatePasFile(FileName));
     Dir:=DirOf(FileName);
     P:=@Dir;
     If Pos(Dir+';',GetSourceDirectories)=0 then
       if ConfirmBox(msg_confirmsourcediradd,@P,false)=cmYes then
         begin
           SourceDirs:=SourceDirs+';'+Dir;
           if assigned(Debugger) then
             Debugger^.SetDirectories;
         end;
     OpenEditorWindow(nil,FileName,0,0);
   end;
  OpenSearch:=OpenIt;
end;


procedure TIDEApp.OpenRecentFile(RecentIndex: integer);
begin
  with RecentFiles[RecentIndex] do
  if OpenEditorWindow(nil,FileName,LastPos.X,LastPos.Y)<>nil then
     RemoveRecentFile(RecentIndex);
end;

function TIDEApp.AskSaveAll: boolean;
function CanClose(P: PView): boolean; {$ifndef FPC}far;{$endif}
begin
  CanClose:=not P^.Valid(cmAskSaveAll);
end;
begin
  AskSaveAll:=Desktop^.FirstThat(@CanClose)=nil;
end;

function TIDEApp.SaveAll: boolean;

  procedure SendSave(P: PView); {$ifndef FPC}far;{$endif}
  begin
    Message(P,evCommand,cmSave,nil);
  end;

begin
  SaveCancelled:=false;
  Desktop^.ForEach(@SendSave);
  SaveAll:=not SaveCancelled;
end;


procedure TIDEApp.ChangeDir;
var
  D : PChDirDialog;
begin
  New(D, Init(cdNormal, hisChDirDialog));
  D^.HelpCtx:=hcChangeDir;
  ExecuteDialog(D,nil);
  CurDirChanged;
end;

{
  $Log$
  Revision 1.3  2002-04-10 22:38:11  pierre
   * allow to open non existant file

  Revision 1.2  2001/09/12 09:49:35  pierre
   + Call to TDebugController.SetDirectories when SourceDirs changes

  Revision 1.1  2001/08/04 11:30:23  peter
    * ide works now with both compiler versions

  Revision 1.1.2.4  2001/06/20 22:57:26  pierre
   * fix bug 1527

  Revision 1.1.2.3  2000/12/30 22:52:27  peter
    * check modified while in debug mode. But placed it between a
      conditional again as it reports also if the file was already modified
      before the first compile.
    * remove unsaved file checks when compiling without primary file so it
      works the same as with a primary file set.

  Revision 1.1.2.2  2000/11/23 16:33:31  pierre
   * fix Alt-X problem and set HelpCtx for most dialogs

  Revision 1.1.2.1  2000/10/18 21:53:27  pierre
   * several Gabor fixes

  Revision 1.1  2000/07/13 09:48:35  michael
  + Initial import

  Revision 1.21  2000/06/16 08:50:41  pierre
   + new bunch of Gabor's changes

  Revision 1.20  2000/05/02 08:42:28  pierre
   * new set of Gabor changes: see fixes.txt

  Revision 1.19  2000/04/25 08:42:33  pierre
   * New Gabor changes : see fixes.txt

  Revision 1.18  2000/03/21 23:29:52  pierre
   + Use TrimEndSlash by Gabor

  Revision 1.17  1999/12/01 16:48:09  pierre
   * avoid problems after going to root directory

  Revision 1.16  1999/11/30 17:15:32  pierre
   * avoid chdir('d:') in FPC

  Revision 1.15  1999/10/29 13:47:00  pierre
   * typo error corrected

  Revision 1.14  1999/10/27 10:44:08  pierre
   * Avoid ChDir('') gives sometimes DosError 3 !!

  Revision 1.13  1999/08/16 18:25:20  peter
    * Adjusting the selection when the editor didn't contain any line.
    * Reserved word recognition redesigned, but this didn't affect the overall
      syntax highlight speed remarkably (at least not on my Amd-K6/350).
      The syntax scanner loop is a bit slow but the main problem is the
      recognition of special symbols. Switching off symbol processing boosts
      the performance up to ca. 200%...
    * The editor didn't allow copying (for ex to clipboard) of a single character
    * 'File|Save as' caused permanently run-time error 3. Not any more now...
    * Compiler Messages window (actually the whole desktop) did not act on any
      keypress when compilation failed and thus the window remained visible
    + Message windows are now closed upon pressing Esc
    + At 'Run' the IDE checks whether any sources are modified, and recompiles
      only when neccessary
    + BlockRead and BlockWrite (Ctrl+K+R/W) implemented in TCodeEditor
    + LineSelect (Ctrl+K+L) implemented
    * The IDE had problems closing help windows before saving the desktop

  Revision 1.12  1999/08/03 20:22:34  peter
    + TTab acts now on Ctrl+Tab and Ctrl+Shift+Tab...
    + Desktop saving should work now
       - History saved
       - Clipboard content saved
       - Desktop saved
       - Symbol info saved
    * syntax-highlight bug fixed, which compared special keywords case sensitive
      (for ex. 'asm' caused asm-highlighting, while 'ASM' didn't)
    * with 'whole words only' set, the editor didn't found occourences of the
      searched text, if the text appeared previously in the same line, but didn't
      satisfied the 'whole-word' condition
    * ^QB jumped to (SelStart.X,SelEnd.X) instead of (SelStart.X,SelStart.Y)
      (ie. the beginning of the selection)
    * when started typing in a new line, but not at the start (X=0) of it,
      the editor inserted the text one character more to left as it should...
    * TCodeEditor.HideSelection (Ctrl-K+H) didn't update the screen
    * Shift shouldn't cause so much trouble in TCodeEditor now...
    * Syntax highlight had problems recognizing a special symbol if it was
      prefixed by another symbol character in the source text
    * Auto-save also occours at Dos shell, Tool execution, etc. now...

  Revision 1.11  1999/06/25 00:35:54  pierre
   + uses weditor FileDir var to remember Directory for Open/Save

  Revision 1.10  1999/03/23 15:11:32  peter
    * desktop saving things
    * vesa mode
    * preferences dialog

  Revision 1.9  1999/02/19 18:43:47  peter
    + open dialog supports mask list

  Revision 1.8  1999/02/05 12:11:57  pierre
    + SourceDir that stores directories for sources that the
      compiler should not know about
      Automatically asked for addition when a new file that
      needed filedialog to be found is in an unknown directory
      Stored and retrieved from INIFile
    + Breakpoints conditions added to INIFile
    * Breakpoints insterted and removed at debin and end of debug session

  Revision 1.7  1999/02/04 13:32:05  pierre
    * Several things added (I cannot commit them independently !)
    + added TBreakpoint and TBreakpointCollection
    + added cmResetDebugger,cmGrep,CmToggleBreakpoint
    + Breakpoint list in INIFile
    * Select items now also depend of SwitchMode
    * Reading of option '-g' was not possible !
    + added search for -Fu args pathes in TryToOpen
    + added code for automatic opening of FileDialog
      if source not found

  Revision 1.6  1999/02/02 16:41:41  peter
    + automatic .pas/.pp adding by opening of file
    * better debuggerscreen changes

  Revision 1.5  1999/01/21 11:54:17  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.4  1999/01/14 21:42:21  peter
    * source tracking from Gabor

  Revision 1.3  1999/01/04 11:49:46  peter
   * 'Use tab characters' now works correctly
   + Syntax highlight now acts on File|Save As...
   + Added a new class to syntax highlight: 'hex numbers'.
   * There was something very wrong with the palette managment. Now fixed.
   + Added output directory (-FE<xxx>) support to 'Directories' dialog...
   * Fixed some possible bugs in Running/Compiling, and the compilation/run
     process revised

  Revision 1.2  1998/12/28 15:47:47  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.4  1998/12/24 08:27:12  gabor
    + displaying 'opening source file...' text while reading
  Revision 1.3  1998/12/22 10:39:46  peter
    + options are now written/read
    + find and replace routines

}
