{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Tools menu entries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure TIDEApp.Messages;
begin
  if MessagesWindow=nil then
    Desktop^.Insert(New(PMessagesWindow, Init))
  else
    MessagesWindow^.Focus;
end;

procedure TIDEApp.DoAsciiTable;
begin
  if ASCIIChart=nil then
    begin
      New(ASCIIChart, Init);
      Desktop^.Insert(ASCIIChart);
    end
  else
    ASCIIChart^.Focus;
end;


procedure TIDEApp.Calculator;
begin
  with CalcWindow^ do
  begin
    if GetState(sfVisible)=false then Show;
    MakeFirst;
  end;
end;

procedure TIDEApp.ExecuteTool(Idx: integer);
var Title,ProgramPath,Params: string;
    W: PSourceWindow;
    ToFocus : sw_integer;
    Wo: word;
    Err: integer;
    CaptureFile: string;
    ErrText : Text;
    ExecMode: TExecType;
    Executed: boolean;
begin
  if (Idx<1) or (Idx>GetToolCount) then Exit;
  GetToolParams(Idx-1,Title,ProgramPath,Params,Wo);
  InitToolTempFiles;
  Err:=ParseToolParams(Params,false);
  if Err=-1 then
  begin
    DoneToolTempFiles;
    Exit;
  end;
  if Err<>0 then
    begin ErrorBox(msg_errorparsingtoolparams,nil); Exit; end;
  if CaptureToolTo<>capNone then
    begin
      if ToolOutput<>'' then
        begin
          CaptureFile:=ToolOutput;
        end
      else
        CaptureFile:=ToolCaptureName;
      ExecMode:=exNoSwap;
    end
  else
    begin
      CaptureFile:='';
      ExecMode:=exNormal;
    end;

  EraseFile(CaptureFile);
  EraseFile(FilterCaptureName);

  if CaptureToolTo=capMessageWindow then
    begin
      AddToolCommand(ProgramPath+' '+Params);
      ToFocus:=ToolMessages^.count-1;
    end
  else
    ToFocus:=-1;
  if CaptureToolTo<>capNone then
    ShowMessage(FormatStrStr(msg_executingtool,KillTilde(Title)));

  Executed:=DoExecute(ProgramPath,Params,'',CaptureFile,ToolCaptureErr,ExecMode);

  if CaptureToolTo<>capNone then
    HideMessage;

  if Executed then
    begin
      if (DosError=0) {and (DosExitCode=0)} then
        begin
          if CaptureToolTo=capEditWindow then
            begin
              if ToolOutput<>'' then
                W:=OpenEditorWindow(nil,ToolOutput,0,0)
              else
                begin
                  W:=OpenEditorWindow(nil,'',0,0);
                  if W<>nil then
                    if StartEditor(W^.Editor,CaptureFile)=false then
                      ErrorBox(msg_errorreadingoutput,nil);
                end;
            end
          else if ToolFilter<>'' then
            begin
              ShowMessage(FormatStrStr(msg_executingfilterfor,KillTilde(Title)));
              DoExecute(ToolFilter,'',CaptureFile,FilterCaptureName,'',exNoSwap);
              HideMessage;
              if (DosError=0) and (DosExitCode=0) then
                begin
                  if ExistsFile(FilterCaptureName)=false then
                    ErrorBox(msg_cantfindfilteredoutput,nil)
                  else
                    if ProcessMessageFile(FilterCaptureName)=false then
                      ErrorBox(msg_errorprocessingfilteredoutput,nil);
                end;
              if (DosError<>0) then
                ErrorBox(FormatStrStr(msg_errorexecutingfilter,KillTilde(GetToolName(Idx-1))),nil) else
              if DosExitCode<>0 then
                ErrorBox(FormatStrInt(msg_filterexecutionsuccessfulexitcodeis,DosExitCode),nil);
              UpdateToolMessages;
              if (ToFocus<>-1) then
                if Assigned(MessagesWindow) then
                  MessagesWindow^.FocusItem(ToFocus);
              if DosError=0 then
                Messages;
            end;
        end;
    end;
  if (DosError<>0) or (DosExitCode<>0) then
    begin
      if (DosError<>0) then
        ErrorBox(FormatStrStr(msg_errorexecutingtool,KillTilde(GetToolName(Idx-1))),nil) else
      if DosExitCode<>0 then
        ErrorBox(FormatStrInt(msg_toolexecutionsuccessfulexitcodeis,DosExitCode),nil);
{$i-}
      Assign(ErrText,ToolCaptureErr);
      Reset(ErrText);
      while not eof(ErrText) do
        begin
          Readln(ErrText,Params);
          AddToolCommand(Params);
        end;
    end;
{$ifndef DEBUG}
  if ToolOutput='' then
    begin
      EraseFile(CaptureFile);
      ToolOutput:='';
    end;
  EraseFile(FilterCaptureName);
  EraseFile(ToolCaptureErr);
{$endif}
  { In case we have something that the compiler touched }
  AskToReloadAllModifiedFiles;
  DoneToolTempFiles;
end;


procedure TIDEApp.DoGrep;
Const
  GrepExeName = 'grep'+ExeExt;
var
  PGrepDialog : PCenterDialog;
  R,R1,R2 : TRect;
  Control : PView;
  IL1,IL2 : PInputLine;
  s : string;
  p,lineNb,GrepOutputLine : longint;
  error : word;
  PosText : longint;
  showmsg,error_in_reading,FirstMsg : boolean;
  ToFocus : sw_integer;
  searchword,
  GrepExe,GrepArgs,Line,ModuleName : String;
  GrepOut : text;
  Params : Array[0..4] of longint;
begin
  showmsg:=false;
  ToFocus:=-1;
{ Find grep.exe }
  GrepExe:=GrepExeName;
  If not LocateExeFile(GrepExe) then
    Begin
      ErrorBox(msg_grepprogramnotfound,nil);
      Exit;
    End;
{ Try to load the word from the editor }
  If not(DeskTop^.Current=nil) and
     (DeskTop^.Current^.HelpCtx=hcSourceWindow) then
    Searchword:=PSourceWindow(DeskTop^.Current)^.Editor^.GetCurrentWord
  else
    Searchword:='';
  { Don't use the listseparator in the file list else it's seen as 1 file
    (at least under linux }
  s:=highlightexts;
  ReplaceStr(s,';',' ');
  { add "" for args with spaces }
  { WARNING : text must still be entered in usual grep syntax }
  { -n is always added later because otherwise
    we don't get the line info PM }
  GrepArgs:=' -i "$TEXT" '+s;
{ Dialog }
  R.Assign(0,0,50,8);
  new(PGrepDialog,Init(R,dialog_greparguments));
  with PGrepDialog^ do
    begin
      R2.A.Y:=R.A.Y+3;
      R2.B.Y:=R2.A.Y+1;
      R2.A.X:=R.A.X+3;
      R2.B.X:=R.B.X-6;
      New(IL1, Init(R2, 128));
      IL1^.Data^:=SearchWord;
      Insert(IL1);
      R2.Move(0,-1);
      Insert(New(PLabel, Init(R2, label_grep_texttofind, IL1)));
      R1.Assign(R2.B.X, R2.A.Y+1, R2.B.X+3, R2.B.Y+1);
      Control := New(PHistory, Init(R1, IL1, TextGrepId));
      Insert(Control);

      R2.Move(0,4);
      New(IL2, Init(R2, 128));
      IL2^.Data^:=GrepArgs;
      Insert(IL2);
      R2.Move(0,-1);
      Insert(New(PLabel, Init(R2, label_grep_greparguments, IL2)));
      R1.Assign(R2.B.X, R2.A.Y+1, R2.B.X+3, R2.B.Y+1);
      Control := New(PHistory, Init(R1, IL2, GrepArgsId));
      Insert(Control);
    end;

    InsertButtons(PGrepDialog);
    IL1^.Select;

    if Desktop^.ExecView(PGrepDialog)=cmOK then
    begin
      SearchWord:=IL1^.Data^;
      if SearchWord<>'' then
       begin
         GrepArgs:=IL2^.Data^;
         { Remember last used Grep extensions }
         PosText:=pos('"$TEXT" ',GrepArgs);
         if PosText>0 then
           HighlightExts:=Trim(Copy(GrepArgs,PosText+Length('"$TEXT" '),Length(GrepArgs)));
         { change spaces back into ';' again }
         ReplaceStr(HighlightExts,' ',';');
         { Replace search string }
         ReplaceStr(GrepArgs,'$TEXT',SearchWord);
         { Linux ? }
         AddToolCommand(GrepExe+' -n '+GrepArgs);
         ToFocus:=ToolMessages^.count-1;
         UpdateToolMessages;
         if Assigned(MessagesWindow) then
           MessagesWindow^.FocusItem(ToFocus);
         showmsg:=true;
         Messages;
         PushStatus(FormatStrStr(msg_runninggrepwithargs,GrepArgs));
         if not ExecuteRedir(GrepExe,'-n '+GrepArgs,'',GrepOutName,GrepErrName) then
           Begin
              PopStatus;
              { 2 as exit code just means that
                some file vwere not found ! }
              if (IOStatus<>0) or (ExecuteResult<>2) then
                begin
                  Params[0]:=IOStatus;
                  Params[1]:=ExecuteResult;
                  WarningBox(msg_errorrunninggrep,@Params);
                end;
           End
         else
           PopStatus;
         {$I-}
         Assign(GrepOut,GrepOutName);
         Reset(GrepOut);
         error_in_reading:=false;
         FirstMsg:=True;
         GrepOutputLine:=0;
         While not eof(GrepOut) do
           begin
             readln(GrepOut,Line);
             Inc(GrepOutputLine);
             p:=pos(':',line);
             if p>0 then
               begin
                 ModuleName:=copy(Line,1,p-1);
                 Line:=Copy(Line,p+1,255);
                 p:=pos(':',Line);
                 val(copy(Line,1,p-1),lineNb,error);
                 if error=0 then
                   begin
                     AddToolMessage(ModuleName,Copy(Line,p+1,255),LineNb,1);
                     if FirstMsg then
                       begin
                         Inc(ToFocus);
                         FirstMsg:=false;
                       end;
                   end
                 else
                   error_in_reading:=true;
               end;
           end;
         Close(GrepOut);
         if not error_in_reading then
           Erase(GrepOut)
         else
           begin
             ClearFormatParams;
             AddFormatParamInt(GrepOutputLine);
             AddFormatParamStr(GrepOutName);
             WarningBox(msg_errorreadinggrepoutput,@FormatParams);
           end;
       { Delete also grep$$.err }
         if not error_in_reading then
           begin
             Assign(GrepOut,GrepErrName);
             Erase(GrepOut);
           end;
         {$I+}
         EatIO;
       end;
    end;
  Dispose(PGrepDialog, Done);
  UpdateToolMessages;
  if (ToFocus<>-1) then
    if Assigned(MessagesWindow) then
      begin
        MessagesWindow^.Lock;
        MessagesWindow^.FocusItem(ToolMessages^.count-1);
        if ToFocus>0 then
          MessagesWindow^.FocusItem(ToFocus-1);
        MessagesWindow^.FocusItem(ToFocus);
        MessagesWindow^.UnLock;
      end;
  if showmsg then
    Messages;
end;


{
  $Log$
  Revision 1.3  2002-05-29 22:38:13  pierre
   Asciitab now in fvision

  Revision 1.2  2001/08/05 12:23:00  peter
    * Automatically support for fvision or old fv

  Revision 1.1  2001/08/04 11:30:23  peter
    * ide works now with both compiler versions

  Revision 1.1.2.7  2001/03/08 16:43:01  pierre
     + add $CAP EDIT(filename) capability
     * fix horizontal scrolling

  Revision 1.1.2.6  2001/02/19 10:40:50  pierre
   * Check for changed files after Running tool or shell

  Revision 1.1.2.5  2000/12/13 16:59:09  pierre
   * ErrFile filed added to DoExecute method

  Revision 1.1.2.4  2000/11/23 16:33:31  pierre
   * fix Alt-X problem and set HelpCtx for most dialogs

  Revision 1.1.2.3  2000/11/21 17:44:12  pierre
   * better message positioning after grep

  Revision 1.1.2.2  2000/11/13 16:57:04  pierre
   * some grep improovements

  Revision 1.1.2.1  2000/07/20 11:02:15  michael
  + Fixes from gabor. See fixes.txt

  Revision 1.1  2000/07/13 09:48:35  michael
  + Initial import

  Revision 1.21  2000/06/16 08:50:41  pierre
   + new bunch of Gabor's changes

  Revision 1.20  2000/05/02 08:42:28  pierre
   * new set of Gabor changes: see fixes.txt

  Revision 1.19  2000/03/13 20:32:56  pierre
   + Grep running also in Status line

  Revision 1.18  2000/03/02 22:33:36  pierre
   * Grep improoved

  Revision 1.17  2000/02/10 00:48:02  pierre
   * avoid crash for empty string

  Revision 1.16  2000/02/02 22:49:44  pierre
    * use desktop^.current for current word in grep

  Revision 1.15  1999/10/08 15:25:25  pierre
   + Grep read check added

  Revision 1.14  1999/09/22 16:17:31  pierre
   + HistList for Grep

  Revision 1.13  1999/08/03 20:22:35  peter
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

  Revision 1.12  1999/07/12 13:14:20  pierre
    * LineEnd bug corrected, now goes end of text even if selected
    + Until Return for debugger
    + Code for Quit inside GDB Window

  Revision 1.11  1999/03/02 13:48:30  peter
    * fixed far problem is fpdebug
    * tile/cascading with message window
    * grep fixes

  Revision 1.10  1999/02/22 12:46:57  peter
    * small fixes for linux and grep

  Revision 1.9  1999/02/22 11:29:37  pierre
    + added col info in MessageItem
    + grep uses HighLightExts and should work for linux

  Revision 1.8  1999/02/22 02:15:17  peter
    + default extension for save in the editor
    + Separate Text to Find for the grep dialog
    * fixed redir crash with tp7

  Revision 1.7  1999/02/20 15:18:31  peter
    + ctrl-c capture with confirm dialog
    + ascii table in the tools menu
    + heapviewer
    * empty file fixed
    * fixed callback routines in fpdebug to have far for tp7

  Revision 1.6  1999/02/05 13:51:42  peter
    * unit name of FPSwitches -> FPSwitch which is easier to use
    * some fixes for tp7 compiling

  Revision 1.5  1999/02/05 12:11:59  pierre
    + SourceDir that stores directories for sources that the
      compiler should not know about
      Automatically asked for addition when a new file that
      needed filedialog to be found is in an unknown directory
      Stored and retrieved from INIFile
    + Breakpoints conditions added to INIFile
    * Breakpoints insterted and removed at debin and end of debug session

  Revision 1.4  1999/02/04 15:59:08  pierre
   * grep$$$.out was not closed

  Revision 1.3  1999/02/04 13:32:09  pierre
    * Several things added (I cannot commit them independently !)
    + added TBreakpoint and TBreakpointCollection
    + added cmResetDebugger,cmGrep,CmToggleBreakpoint
    + Breakpoint list in INIFile
    * Select items now also depend of SwitchMode
    * Reading of option '-g' was not possible !
    + added search for -Fu args pathes in TryToOpen
    + added code for automatic opening of FileDialog
      if source not found

  Revision 1.2  1999/01/21 11:54:21  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.1  1998/12/22 14:27:54  peter
    * moved

  Revision 1.2  1998/12/22 10:39:49  peter
    + options are now written/read
    + find and replace routines

}
