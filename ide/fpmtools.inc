{
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
    TextFile : Text;
    ExecMode: TExecType;
    Executed: boolean;
begin
  if (Idx<1) or (Idx>GetToolCount) then Exit;
  InitToolProcessing;
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
  EraseFile(ToolCaptureErr);

  if CaptureToolTo=capMessageWindow then
    begin
      Messages;
      AddToolCommand(ProgramPath+' '+Params);
      UpdateToolMessages;
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
          else if (ToolFilter<>'') or (CaptureToolTo=capMessageWindow) then
            begin
              if ToolFilter<>'' then
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
                end
              else
                begin
                  if ExistsFile(CaptureFile) then
                    begin
                      Assign(TextFile,CaptureFile);
                      Reset(TextFile);
                      while not eof(TextFile) do
                        begin
                          Readln(TextFile,Params);
                          AddToolCommand(Params);
                        end;
                      Close(TextFile);
                      UpdateToolMessages;
                      if Assigned(MessagesWindow) then
                        MessagesWindow^.MakeFirst;
                    end;
                end;
              UpdateToolMessages;
              if (ToFocus<>-1) then
                if Assigned(MessagesWindow) then
                  MessagesWindow^.FocusItem(ToFocus);
              if DosError=0 then
                Messages;
            end;
        end;
    end;
  if (DosError<>0) or (DosExitCode<>0) or (SizeOfFile(ToolCaptureErr)>0) then
    begin
      if (DosError<>0) then
        ErrorBox(FormatStrStr(msg_errorexecutingtool,KillTilde(GetToolName(Idx-1))),nil) else
      if DosExitCode<>0 then
        ErrorBox(FormatStrInt(msg_toolexecutionsuccessfulexitcodeis,DosExitCode),nil);
{$i-}
      Assign(TextFile,ToolCaptureErr);
      Reset(TextFile);
      while not eof(TextFile) do
        begin
          Readln(TextFile,Params);
          AddToolCommand(Params);
        end;
      Close(TextFile);
      UpdateToolMessages;
      if Assigned(MessagesWindow) then
        begin
          MessagesWindow^.MakeFirst;
          if (ToFocus<>-1) then
            MessagesWindow^.FocusItem(ToFocus);
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
  IL1,IL2 : PEditorInputLine;
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


