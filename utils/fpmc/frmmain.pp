{$mode objfpc}
{$h+}
unit frmmain;

interface

uses gdk,gtk,fpgtk,fpgtkext,classes,sysutils;

Type
  TMainForm = Class(TFPGtkWindow)
    FModified : Boolean;
    FFileName : String;
    FUnitName : String;
    FLanguageID : Integer;
    FSubLanguageID : Integer;
    FVerbose,
    FCreateMsg,
    FCreatePas,
    FCreateRC,
    FEscapePath : Boolean;
    FMsgLabel : TFPgtkLabel;
    FMsgList : TFPgtkScrollList;
    FMsgVBox,
    FVBox : TFPGtkVBox;
    FVPaned : TFPgtkVPaned;
    FFile,
    FFileNew,
    FFileOpen,
    FFileSave,
    FFileSaveAs,
    FFileExit,
    FEdit,
    FEditCut,
    FEditCopy,
    FEditPaste,
    FProject,
    FProjectCompile,
    FProjectOptions,
    FHelp,
    FHelpAbout : TFPGtkMenuItem;
    FMainMenu : TFPGtkMenuBar;
    FEditor : TFPGtkScrollText;
    Procedure CreateWindow;
    Function CheckSaved : Boolean;
    Procedure SetCaption;
    Function GetFileName(ATitle : String) : String;
    // Callback functions.
    Procedure DialogSetFilename(Sender : TFPGtkWindow;Data : Pointer; Action : Integer;Initiator : TFPGtkObject);
    Procedure SaveOptions(Sender : TFPGtkWindow;Data : Pointer; Action : Integer;Initiator : TFPGtkObject);
    Function  OnDeleteEvent(Sender:TFPgtkWidget; Event:PGdkEvent; data:pointer): boolean;
    Procedure FileNewClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure FileSaveClick(Sender : TFPgtkObject; Data : Pointer);
    Procedure FileSaveAsClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure FileOpenClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure FileExitClick(Sender : TFPgtkObject ; Data : Pointer);
    Procedure EditCCPClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure ProjectCompileClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure ProjectOptionsClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure HelpAboutClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure EditorChanged(Sender : TFPgtkObject; Data : Pointer);
    Procedure DoError(Sender : TObject; Msg : String);
    Procedure DoVerbose(Sender : TObject; Msg : String);
  Public
    Constructor Create;
    Procedure Compile;
    Procedure SetOptions;
    Procedure LoadFromFile(FN : String);
    Procedure SaveToFile(FN : String);
    Procedure NewFile;
    Procedure EditCut;
    Procedure EditCopy;
    Procedure EditPaste;
    Property Modified : Boolean Read FModified;
    Property FileName : String Read FFileName;
  end;

Implementation

uses frmabout,frmoptions,msgcomp;

ResourceString
  SMenuFile           = '_File';
  SMenuFileNew        = '_New';
  SMenuFileOpen       = '_Open';
  SMenuFileSave       = '_Save';
  SMenuFileSaveAs     = 'Save _as';
  SMenuFileExit       = 'E_xit';
  SMenuEdit           = '_Edit';
  SMenuEditCut        = 'C_ut';
  SMenuEditCopy       = '_Copy';
  SMenuEditPaste      = '_Paste';
  SMenuProject        = '_Project';
  SMenuProjectCompile = '_Compile';
  SMenuProjectoptions = '_Options';
  SMenuHelp           = '_Help';
  SMenuHelpAbout      = '_About';

  SCaption        = 'Free Pascal message compiler';
  SFileModified  = 'File has changed. Save changes ?';
  SSaveFile      = 'Save file as';
  SOpenFile      = 'Select file to open';
  SModified      = '(modified)';
  SCompilerMessages = 'Compile messages';
  SErrsCompiling    = 'Encountered %d errors while compiling.';
  SSuccesCompiling = 'Succesfully compiled messages.';
  SErrUnexpected  = 'The following unexpected error occurred when compiling:%s';

{ ---------------------------------------------------------------------
    Form Creation
  ---------------------------------------------------------------------}

Constructor TMainForm.Create;

begin
  Inherited create (gtk_window_dialog);
  FCreateMsg:=True;
  FCreatePas:=True;
  FCreateRC:=True;
  FEscapePath:=True;
  FVerbose:=True;
  Createwindow;
  If ParamCount>0 then
    LoadFromFile(Paramstr(1));
end;

Procedure TMainForm.CreateWindow;

Var
  FAccelGroup : Integer;

begin
  FVBox:=TFPgtkVBox.Create;
  FAccelGroup:=AccelGroupNew;
  FFileNew:=NewMenuItem(SMenuFileNew,'','', MakeAccelKeyDef(Self,FaccelGroup,GDK_N,[amcontrol]),@FileNewClick,Nil);
  FFileOpen:=NewMenuItem(SMenuFileOpen,'','', MakeAccelKeyDef(Self,FaccelGroup,GDK_O,[amcontrol]),@FileOpenClick,Nil);
  FFileSave:=NewMenuItem(SMenuFileSave,'','', MakeAccelKeyDef(Self,FaccelGroup,GDK_S,[amcontrol]),@FileSaveClick,Nil);
  FFileSaveAs:=NewMenuItem(SMenuFileSaveAs,'','', @FileSaveAsClick,Nil);
  FFileExit:=NewMenuItem(SMenuFileExit,'','', MakeAccelKeyDef(Self,FaccelGroup,GDK_Q,[amcontrol]),@FileExitClick,Nil);
  FFile:=NewSubMenu(SmenuFile,'','',[FFileNew,FFileOpen,FFileSave,FFileSaveAs,NewLine,FFileExit]);
  FEditCut:=NewMenuItem(SMenuEditCut,'','', MakeAccelKeyDef(Self,FaccelGroup,GDK_X,[amcontrol]),@EditCCPClick,Nil);
  FEditCopy:=NewMenuItem(SMenuEditCopy,'','', MakeAccelKeyDef(Self,FaccelGroup,GDK_C,[amcontrol]),@EditCCPClick,Nil);
  FEditPaste:=NewMenuItem(SMenuEditPaste,'','', MakeAccelKeyDef(Self,FaccelGroup,GDK_V,[amcontrol]),@EditCCPClick,Nil);
  FEdit:=NewSubMenu(SMenuEdit,'','',[FEditCut,FEditCopy,FEditPaste]);
  FProjectCompile:=NewMenuItem(SMenuProjectCompile,'','', MakeAccelKeyDef(Self,FaccelGroup,GDK_F9,[amcontrol]),@ProjectCompileClick,Nil);
  FProjectOptions:=NewMenuItem(SMenuProjectOptions,'','', MakeAccelKeyDef(Self,FaccelGroup,GDK_F11,[amcontrol,amshift]),@ProjectOptionsClick,Nil);
  FProject := NewSubMenu(SMenuProject,'','',[FProjectCompile,FProjectoptions]);
  FHelpAbout:=NewMenuItem(SMenuHelpAbout ,'','',@HelpAboutClick,Nil);
  FHelp := NewSubMenu(SMenuHelp,'','',[FHelpAbout]);
  FMainMenu:=NewMenuBar([FFile,FEdit,FProject,FHelp]);
  FEditor:=TFPgtkScrollText.Create;
  Feditor.TheText.ConnectChanged(@EditorChanged,Nil);
  // Compiling messages
  FMsgLabel:=TFPgtkLabel.Create(SCompilerMessages);
  FMsgList:=TFPgtkScrollList.Create;
  FMsgVBox:=TFPgtkVbox.Create;
  FMsgVBox.PackStart(FMsgLabel,False,False,0);
  FMsgVBox.PackStart(FMsgList,True,True,0);
  FVPaned:=TFPgtkVPaned.Create;
  FVPaned.Add1(FEditor);
  FVPaned.Add2(FMsgVBox);
  FVPaned.Position:=350;
  FVBox.PackStart(FmainMenu,False,False,0);
  FVBox.PackStart(FVPaned,true, true, 0);
  ConnectDeleteEvent(@OnDeleteEvent,Nil);
  Add(FVBox);
  SetUSize(640,480);
  SetCaption;
  FEditor.TheText.GrabFocus;
end;

{ ---------------------------------------------------------------------
    Callback events
  ---------------------------------------------------------------------}


Procedure TMainForm.FileNewClick(Sender : TFPGtkObject; Data : Pointer);

begin
  If CheckSaved then
    NewFile;
end;


Function TMainForm.OnDeleteEvent(Sender:TFPgtkWidget; Event:PGdkEvent; data:pointer): boolean;

begin
  Result:=Not CheckSaved;
end;


Procedure TMainForm.FileSaveClick(Sender : TFPgtkObject; Data : Pointer);

begin
  If (FFileName='') then
    FileSaveAsClick(Sender,Data)
  else
    SaveToFile(FFileName);
end;


Procedure TMainForm.FileSaveAsClick(Sender : TFPGtkObject; Data : Pointer);

Var
  FN : String;

begin
  FN:=GetFileName(SSaveFile);
  If (FN<>'') then
    SavetoFile(FN);
end;


Procedure TMainForm.FileOpenClick(Sender : TFPGtkObject; Data : Pointer);

Var
  FN : String;

begin
  FN:=GetFileName(SOpenFile);
  If (FN<>'') then
    LoadFromFile(FN);
end;


Procedure TMainForm.EditorChanged(Sender : TFPgtkObject; Data : Pointer);

begin
  If FModified<>True then
    begin
    FModified:=True;
    SetCaption;
    end;
end;

Procedure TMainForm.EditCCPClick(Sender : TFPGtkObject; Data : Pointer);

begin
  If Sender=FEditCut then
    EditCut
  else if Sender=FEditCopy then
    EditCopy
  else
    EditPaste;
end;

Procedure TMainForm.FileExitClick(Sender : TFPgtkObject;  Data : Pointer);

begin
  If CheckSaved then
    Close;
end;


Procedure TMainForm.HelpAboutClick(Sender : TFPGtkObject; Data : Pointer);

begin
  With TAboutForm.Create do
    Execute(Nil,Nil,Nil);
end;

Procedure TMainForm.ProjectCompileClick(Sender : TFPGtkObject; Data : Pointer);

begin
  Compile;
end;

Procedure TMainForm.ProjectOptionsClick(Sender : TFPGtkObject; Data : Pointer);

begin
  SetOptions;
end;

Procedure TMainform.DoError(Sender : TObject; Msg : String);

begin
  FMsgList.list.Add(TFPGtkListItem.CreateWithLabel(Msg));
end;

Procedure TMainform.DoVerbose(Sender : TObject; Msg : String);

begin
  FMsgList.list.Add(TFPGtkListItem.CreateWithLabel(Msg));
end;


{ ---------------------------------------------------------------------
    Auxiliary methods
  ---------------------------------------------------------------------}

Procedure TMainForm.SetCaption;

Var
  S : String;

begin
  S:=SCaption;
  If (FFileName<>'') then
    S:=S+' : '+ExtractFileName(FFileName);
  If FModified then
    S:=S+' '+SModified;
  Title:=S;
end;

Function TMainForm.CheckSaved : Boolean;

begin
  Result:=Not FModified;
  If Not Result then
    Case MessageDlg(SFileModified,mtInformation,mbYesNoCancel,0) of
      mrYes : begin
              FileSaveClick(Self,Nil);
              Result:=True;
              end;
      mrNo  : Result:=True;
      mrCancel : Result:=False;
    end;
end;

Function TMainForm.GetFileName(ATitle : String) : String;

var
  FS : TFPgtkFileSelection;

begin
  Result:='';
  FS := TFPgtkFileSelection.Create (gtk_window_dialog);
  with FS do
    begin
    Title:=ATitle;
    OKButton.ConnectClicked (@(CloseWithResult), inttopointer(drOk));
    CancelButton.ConnectClicked (@(CloseWindow), nil);
    if Not execute (nil, @Result, @DialogSetFilename) = drOk then
      Result:='';
    end;
end;

Procedure TMainForm.DialogSetFilename(Sender : TFPGtkWindow;Data : Pointer; Action : Integer;Initiator : TFPGtkObject);

type
  PString = ^AnsiString;

begin
  PString(Data)^:=(Sender as TFPgtkFileSelection).Filename;
end;


{ ---------------------------------------------------------------------
    Public methods
  ---------------------------------------------------------------------}


Procedure TMainForm.LoadFromFile(FN : String);

Var
  S : TStringList;

begin
  S:=TStringList.Create;
  try
    S.LoadFromFile(FN);
    FEditor.TheText.Text:=S.Text;
    FModified:=False;
  Finally
    S.Free;
  end;
  FFileName:=FN;
  SetCaption;
end;


Procedure TMainForm.SaveToFile(FN : String);

begin
  FFileName:=FN;
  FEditor.TheText.Lines.SaveToFile(FN);
  FModified:=False;
  SetCaption;
end;


Procedure TMainForm.EditCut;

begin
  FEditor.TheText.CutClipBoard;
end;


Procedure TMainForm.EditCopy;

begin
  FEditor.TheText.CopyCLipBoard;
end;


Procedure TMainForm.EditPaste;

begin
  FEditor.TheText.PasteClipBoard;
end;


Procedure TMainForm.NewFile;

begin
  Feditor.TheText.Clear;
end;

Procedure TMainForm.Compile;

Var
  M,P,R,I : TStream;
  S,MsgFileName : String;

  Procedure SetupStreams;

  begin
    I:=TFileStream.Create(FFileName,fmOpenRead);
    If FCreatePas then
      P:=TFileStream.Create(ChangeFileExt(FFileName,'.pp'),fmCreate);
    If FCreateMsg then
      begin
      MsgFileName:=ChangeFileExt(FFileName,'.msg');
      M:=TFileStream.Create(MsgFileName,fmCreate);
      end;
    If FCreateRC then
      R:=TFileStream.Create(ChangeFileExt(FFileName,'.rc'),fmCreate);
  end;

  Procedure CloseStreams;

  begin
    M.Free;
    P.Free;
    R.Free;
    I.Free;
  end;

begin
  FileSaveClick(Self,Nil);
  If (FUnitName='') then
    FUnitName:=ExtractFileName(FFileName);
  FMsgList.List.ClearAll;
  Try
    SetupStreams;
    Try
    With TMessageCompiler.Create do
      Try
        Msg:=M;
        MC:=I;
        RC:=R;
        Pas:=P;
        OnError:=@DoError;
        If FVerbose then
          OnVerbose:=@DoVerbose;
        UnitName:=FUnitName;
        MessageFileName:=MsgFileName;
        EscapeNeeded:=FEscapePath;
        If (FLanguageID<>-1) then
          LocaleID:=FLanguageID;
        If (FSubLanguageID<>-1) then
          SubLocaleID:=FSubLanguageID;
        If Compile then
          DoVerbose(Nil,SSuccesCompiling)
        else
          begin
          S:=Format(SErrsCompiling,[Errors]);
          DoVerbose(Nil,S);
          MessageDlg(S,mtError,[mbOK],0);
          end;
      Finally
        Free;
      end;
    Finally
      CloseStreams;
    end;
  except
    On E : Exception do
      MessageDlg(SErrUnexpected,[E.Message],mtError,[mbOK],0);
  end;
end;

Procedure TMainForm.SaveOptions(Sender : TFPGtkWindow;Data : Pointer; Action : Integer;Initiator : TFPGtkObject);

begin
  With TOptionsForm(Data) do
    begin
    FUnitName:=UnitName;
    FLanguageID:=StrToIntDef(Trim(Locale),0);
    FSubLanguageID:=StrToIntDef(Trim(SubLocale),0);
    FVerbose:=Verbose;
    FCreateMsg:=CreateMsgFile;
    FCreatePas:=CreatePasFile;
    FCreateRC:=CreateRCFile;
    FEscapePath:=EscapePath;
    end;
end;


Procedure TMainForm.SetOptions;

Var
  F : TOptionsForm;

begin
  If (FUnitName='') and (FFileName<>'') then
    FUnitName:=ExtractFileName(FFileName);
  F:=TOptionsForm.Create;
  With F do
    begin
    UnitName:=FUnitName;
    Locale:=IntToStr(FLanguageID);
    SubLocale:=IntToStr(FSubLanguageID);
    Verbose:=Fverbose;
    CreateMsgFile:=FCreateMsg;
    CreatePasFile:=FCreatePas;
    CreateRCFile:=FCreateRC;
    EscapePath:=FEscapePath;
    Execute(Nil,F,@SaveOptions);
    end;
end;


end.
