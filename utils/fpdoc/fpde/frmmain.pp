{$mode objfpc}
{$h+}
unit frmmain;

interface

uses
  gtk,gdk,fpgtk,fpgtkext,pgEditor,frmlink,sysutils,classes,fpdeopts;

Const
  DefaultTooltips = True;
  DefaultToolBarStyle = GTK_TOOLBAR_ICONS;
  DefaultToolbarRelief = GTK_RELIEF_NONE;
  SFileTemplate = 'template.xml';

Type
  TNodeType = (ntfile,ntPackage,ntModule,ntElement,ntTopic);

  TMainForm = Class(TFPGtkWindow)
  Private
    FRecent : TStrings;
    FPages : TFPGtkNoteBook;
    FVBox : TFPGTKVBox;
    FMenu : TFpGTKMenuBar;
    FNewImage,
    FOpenImage,
    FSaveImage,
    FSaveAsImage,
    FNewPackageImage,
    FNewModuleImage,
    FNewTopicImage,
    FNewElementImage,
    FBoldImage,
    FUnderlineImage,
    FItalicsImage,
    FParagraphImage,
    FVarImage,
    FLinkImage,
    FRemarkImage,
    FTableImage : TFPGtkPixmap;
    FHelpMenu,
    FExtraMenu,
    FInsertMenu,
    FFormatMenu,
    FFileMenu : TFPGtkMenuItem;
    FToolbarFrame : TFPGtkFrame;
    FFormatToolBar,
    FToolsToolBar,
    FMainToolBar : TFPGtkToolbar;
    FFileOpen,
    FFileNew,
    FFileNewFromFile,
    FFileSave,
    FFileSaveAs,
    FFileRecent,
    FFileClose,
    FFileExit,
    FInsertPackage,
    FInsertModule,
    FInsertTopic,
    FInsertElement,
    FInsertLink,
    FInsertTable,
    FFormatParagraph,
    FFormatBold,
    FFormatItalic,
    FFormatUnderline,
    FFormatRemark,
    FFormatVariable,
    FFormatCode,
    FFormatFile,
    FExtraoptions,
    FHelpAbout : TFPGtkMenuItem;
    // Utility functions
    Function  FormatMenuItem(ACaption : String; tt : TTagType) : TFPgtkMenuItem;
    Function  FormatMenuItem(ACaption : String; tt : TTagType; Accel : PAccelKeyDef) : TFPgtkMenuItem;
    // Editor functions.
    procedure BuildReopenList;
    Procedure AddTorecent(FN : String);
    Procedure OpenFile(FN : String);
    Procedure SaveEditorAs(E : TEditorPage);
    Procedure SaveEditor(E : TEditorPage);
    Function  CloseEditor(E : TEditorPage) : Boolean;
    Procedure LoadCommandLine;
    Procedure LoadRecent;
    Procedure SaveRecent;
    Procedure CreateForm;
    Function  CreatePage : TEditorPage;
    Function  Currenteditor : TEditorPage;
    Function  AllowClose : Boolean;
    // Callbacks
    Procedure TagClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure FileReopen(Sender: TFPGTKObject;data : Pointer);
    Procedure FileOpenClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure FileNewClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure FileNewFromFileClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure FileSaveAsClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure FileSaveClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure FileExitClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure FileCloseClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure OptionsClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure HelpAboutClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure InsertNodeClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure LinkClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure TableClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure GetLinkData(Sender : TFPGtkWindow;Data : Pointer; Action : Integer;Initiator : TFPGtkObject);
    Procedure GetTableData(Sender : TFPGtkWindow;Data : Pointer; Action : Integer;Initiator : TFPGtkObject);
    Procedure GetNodeData(Sender : TFPGtkWindow;Data : Pointer; Action : Integer;Initiator : TFPGtkObject);
    Procedure GetSkeletonData(Sender : TFPGtkWindow;Data : Pointer; Action : Integer;Initiator : TFPGtkObject);
    Procedure DialogSetFilename(Sender : TFPGtkWindow;Data : Pointer; Action : Integer;Initiator : TFPGtkObject);
    Function OnDeleteEvent(Sender:TFPgtkWidget; Event:PGdkEvent; data:pointer): boolean;
  Public
    Constructor create;
    Destructor Destroy; override;
  end;

Const
  NodeNames : Array[TNodeType] of String
            = ('file','package','module','element','topic');

implementation

uses fpdemsg,frmnewnode,frmtable,frmmakeskel,process,frmOptions,frmAbout,inifiles,xpms;


Function TMainForm.CreatePage : TEditorPage;

begin
  Result:=TEditorPage.Create;
  Result.FileNameLabel:=TFPgtkLabel.Create(SNewDocument);
  FPages.AppendPage(Result, REsult.FileNameLabel);
  FPages.Page:=Result;
end;

Function PixmapFromFile (FN : String; Const ImgArray : Array of string) :TFPGtkPixmap;

begin
  Result:=TFPGtkPixmap.CReate;
{$ifdef debug}
  writeln('loading ','bitmaps'+directoryseparator+FN);
{$endif}
  If FileExists('bitmaps'+directoryseparator+FN) then
    Result.LoadFromFile('bitmaps'+directoryseparator+FN)
  else
    Result.loadFromArray(ImgArray);

end;

Function TMainForm.FormatMenuItem(ACaption : String; tt : TTagType) : TFPgtkMenuItem;

begin
  Result:=NewMenuItem(ACaption,FormatHint(ACaption),'',@TagClick,Pointer(tt));
end;

Function TMainForm.FormatMenuItem(ACaption : String; tt : TTagType; Accel : PAccelKeyDef) : TFPgtkMenuItem;

begin
  Result:=NewMenuItem(ACaption,FormatHint(ACaption),'',Accel,@TagClick,Pointer(tt));
end;

Procedure TMainForm.CreateForm;

Var
  V : TFPGtkHBox;
  FAccelGroup: Integer;

begin
  Title:=SFPDE;
  FAccelGroup:=AccelGroupNew;
  {  File menu }
{$ifdef debug}
  Writeln('Menu');
{$endif}
//anAG : integer; aKey : guint; aMods
  FFileOpen:=NewMenuItem(SMenuOpen,'','',MakeAccelKeyDef(Self,FaccelGroup,GDK_O,[amcontrol]),@FileOpenClick,Nil);
  FFileNew:=NewMenuItem(SMenuNew,'','',MakeAccelKeyDef(Self,FaccelGroup,GDK_N,[amcontrol]),@FileNewClick,Nil);
  FFileNewFromFile:=NewMenuItem(SMenuNewFromSource,'','',@FileNewFromFileClick,Nil);
  FFileSave:=NewMenuItem(SMenuSave,'','',MakeAccelKeyDef(Self,FaccelGroup,GDK_S,[amcontrol]),@FileSaveClick,Nil);
  FFileSaveAs:=NewMenuItem(SMenuSaveAs,'','',@FileSaveAsClick,Nil);
  FFileRecent:=NewSubMenu(SMenuRecent,'','',[]);
  FFileClose:=NewMenuItem(SMenuClose,'','',MakeAccelKeyDef(Self,FaccelGroup,GDK_W,[amcontrol]),@FileCLoseClick,Nil);
  FFileExit:=NewMenuItem(SMenuExit,'','',MakeAccelKeyDef(Self,FaccelGroup,GDK_Q,[amcontrol]),@FileExitClick,Nil);
  FFileMenu:=NewSubMenu(SMenuFile,'','',[FFileNew,FFileNewFromFile,FFileOpen,FFileRecent,FFileSave,FFileSaveAs,FFileClose,NewLine,FFileExit]);

  {  Insert menu }
  FInsertPackage:=NewMenuItem(SMenuInsertPackage,SHintInsertPackage,'',MakeAccelKeyDef(Self,FaccelGroup,GDK_P,[amcontrol]),@InsertNodeClick,Pointer(ntpackage));
  FInsertModule:=NewMenuItem(SMenuInsertModule,SHintInsertModule,'',MakeAccelKeyDef(Self,FaccelGroup,GDK_M,[amcontrol]),@InsertNodeClick,Pointer(ntmodule));
  FInsertTopic:=NewMenuItem(SMenuInsertTopic,SHintInsertTopic,'',MakeAccelKeyDef(Self,FaccelGroup,GDK_M,[amcontrol]),@InsertNodeClick,Pointer(ntTopic));
  FInsertElement:=NewMenuItem(SMenuInsertElement,SHintInsertElement,'',MakeAccelKeyDef(Self,FaccelGroup,GDK_E,[amcontrol]),@InsertNodeClick,Pointer(ntElement));
  FinsertLink:=NewMenuItem(SMenuInsertLink,SHintInsertLink,'',MakeAccelKeyDef(Self,FaccelGroup,GDK_L,[amcontrol]),@LinkClick,Nil);
  FinsertTable:=NewMenuItem(SMenuInsertTable,SHintInsertTable,'',MakeAccelKeyDef(Self,FaccelGroup,GDK_T,[amcontrol]),@TableClick,Nil);
  FInsertMenu:=NewSubMenu(SMenuInsert,'','',[FInsertPackage,FInsertModule,FInsertElement,NewLine,FInsertLink,FinsertTable]);

  { Format menu }

  FFormatBold:=FormatMenuItem(SMenuFormatBold,ttBold,MakeAccelKeyDef(Self,FaccelGroup,GDK_B,[amcontrol]));
  FFormatItalic:=FormatMenuItem(SMenuFormatItalics,ttItalic,MakeAccelKeyDef(Self,FaccelGroup,GDK_I,[amcontrol]));
  FFormatUnderline:=FormatMenuItem(SMenuFormatUnderline,ttUnderline,MakeAccelKeyDef(Self,FaccelGroup,GDK_U,[amcontrol]));
  FFormatVariable:=FormatMenuItem(SMenuFormatVariable,ttVariable,MakeAccelKeyDef(Self,FaccelGroup,GDK_R,[amcontrol]));
  FFormatFile:=FormatMenuItem(SMenuFormatFile,ttFile);
  FFormatParagraph:=FormatMenuItem(SMenuFormatParagraph,ttParagraph);
  FFormatRemark:=FormatMenuItem(SMenuFormatRemark,ttRemark);
  FFormatCode:=FormatMenuItem(SMenuFormatCode,ttCode);
  FFormatMenu:=NewSubMenu(SMenuFormat,'','',[FFormatBold,FFormatItalic,FFormatUnderline,FFormatVariable,FFormatFile,
                                             NewLine,FFormatParagraph,FFormatRemark,FFormatCode]);

  { Extra menu }
  FExtraOptions:=NewMenuItem(SMenuExtraOptions,SHMenuExtraOptions,'',@OptionsClick,Nil);
  FExtraMenu:=NewSubMenu(SMenuExtra,'','',[FExtraOptions]);

  { Help menu }
  FHelpAbout:=NewMenuItem(SMenuHelpAbout,SHMenuHelpAbout,'',@HelpAboutClick,Nil);
  FHelpMenu:=NewSubMenu(SMenuHelp,'','',[FHelpAbout]);

  FMenu:=NewMenuBar([FFileMenu,FInsertMenu,FFormatMenu,FExtraMenu,FHelpMenu]);
{$ifdef debug}
  Writeln('Toolbar pixmaps');
{$endif}

  { Tool bar bitmaps }
  FNewImage:=PixMapFromFile('new.xpm',ImgNew);
  FOpenImage:=PixmapFromFile('open.xpm',ImgOpen);
  FSaveImage:=PixmapFromFile('save.xpm',ImgSave);
  FSaveAsImage:=PixMapFromFile('saveas.xpm',ImgSaveAs);
  FNewPackageImage:=PixMapFromFile('newpackage.xpm',ImgNewPackage);
  FNewModuleImage:=PixMapFromFile('newmodule.xpm',ImgNewModule);
  FNewTopicImage:=PixMapFromFile('newtopic.xpm',ImgNewTopic);
  FNewElementImage:=PixMapFromFile('newelement.xpm',ImgNewElement);
  FBoldImage:=PixMapFromFile('bold.xpm',ImgBold);
  FUnderlineImage:=PixMapFromFile('underline.xpm',ImgUnderline);
  FItalicsImage:=PixMapFromFile('italic.xpm',ImgItalic);
  FParagraphImage:=PixMapFromFile('para.xpm',ImgPara);
  FVarImage:=PixMapFromFile('var.xpm',ImgVar);
  FRemarkImage:=PixMapFromFile('remark.xpm',ImgRemark);
  FLinkImage:=PixMapFromFile('link.xpm',ImgLink);
  FTableImage:=PixMapFromFile('table.xpm',ImgTable);

  { Tool bars }
{$ifdef debug}
  Writeln('Main toolbar');
{$endif}

  FMainToolBar:=TFPGtkToolbar.Create;
  With FMainToolbar do
    begin
    Style:=DefaultToolbarStyle;
    ButtonRelief:=DefaultToolbarRelief;
    EnableTooltips:=DefaultTooltips;
// AppendElement (ButtonType:TGtkToolbarChildType; PrevRadioBut:TFPgtkWidget; Text:string; TooltipText:string; TooltipPrivate:string; Icon:TFPgtkWidget; CallBack:TFPgtkSignalProcedure; data:pointer) : TFPgtkWidget;
    AppendItem(SMenuNew,SHintFileNew,'',FNewImage,@FileNewClick,Nil);
    AppendItem(SMenuOpen,SHintFileOpen,'',FOpenImage,@FileOpenClick,Nil);
    AppendItem(SmenuSave,SHintFileSave,'',FSaveImage,@FileSaveClick,Nil);
    AppendItem(SMenuSaveAs,SHintFileSaveAs,'',FSaveAsImage,@FileSaveAsClick,Nil);
    AppendSpace;
    AppendItem(SMenuInsertPackage,SHintInsertPackage,'',FNewPackageImage,@InsertNodeClick,Pointer(ntPackage));
    AppendItem(SMenuInsertModule,SHintInsertModule,'',FNewModuleImage,@InsertNodeClick,Pointer(ntModule));
    AppendItem(SMenuInsertTopic,SHintInsertTopic,'',FNewTopicImage,@InsertNodeClick,Pointer(ntTopic));
    AppendItem(SMenuInsertEleMent,SHintInsertElement,'',FNewElementImage,@InsertNodeClick,Pointer(ntElement));
    AppendSpace;
    end;
{$ifdef debug}
  Writeln('Format Toolbars');
{$endif}

  FFormatToolBar:=TFPGtkToolbar.Create;
  With FFormatToolbar do
    begin
    Style:=DefaultToolBarStyle;
    ButtonRelief:=DefaultToolbarRelief;
    EnableTooltips:=DefaultTooltips;
    AppendItem(SMenuFormatParaGraph,Format(SMarkSelection,[SMenuFormatParaGraph]),'',FParagraphImage,@TagClick,Pointer(ttParagraph));
    AppendItem(SMenuFormatBold,Format(SMarkSelection,[SMenuFormatBold]),'',FBoldImage,@TagClick,Pointer(ttBold));
    AppendItem(SMenuFormatItalics,Format(SMarkSelection,[SMenuFormatItalics]),'',FItalicsImage,@TagClick,Pointer(ttItalic));
    AppendItem(SMenuFormatUnderline,Format(SMarkSelection,[SMenuFormatUnderline]),'',FUnderlineImage,@TagClick,Pointer(ttUnderline));
    AppendItem(SMenuFormatVariable,Format(SMarkSelection,[SMenuFormatVariable]),'',FVarImage,@TagClick,Pointer(ttVariable));
    AppendItem(SmenuFormatRemark,Format(SMarkSelection,[SmenuFormatRemark]),'',FRemarkImage,@TagClick,Pointer(ttRemark));
    FFormatToolbar.AppendSpace;
    end;
{$ifdef debug}
  Writeln('Tools toolbar');
{$endif}

  FToolsToolBar:=TFPGtkToolbar.Create;
  With FToolsToolbar do
    begin
    Style:=DefaultToolBarStyle;
    ButtonRelief:=DefaultToolBarRelief;
    EnableTooltips:=DefaultToolTips;
    AppendItem(SMenuInsertLink,SHintInsertLink,'',FlinkImage,@LinkCLick,Nil);
    AppendItem(SMenuInsertTable,SHintInsertTable,'',FTableImage,@TableClick,Nil);
    AppendSpace;
    end;

{$ifdef debug}
  Writeln('end Toolbars');
{$endif}

  V:=TFPGtkHBox.Create;
  V.PackStart(FMainToolbar,False,False,2);
  V.PackStart(FFormatToolBar,False,False,2);
  V.Packstart(FToolsToolbar,True,True,2);

  FToolbarFrame:=TFPGtkFrame.Create;
  FtoolbarFrame.Add(V);

  { Pages }

  FPages:=TFPGtkNotebook.Create;

  { Place everything on form }
  FVBox:=TFPGTKVBox.create;
  FVBox.PackStart(FMenu,false, true, 0);
  FVBox.PackStart(FToolBarFrame,false, true, 0);
  FVBox.PackStart(FPages,true, true, 0);
  Self.add(FVBox);
  setusize(640,480);
  ConnectDeleteEvent(@OnDeleteEvent,Nil);
end;

Constructor TMainForm.create;

begin
  FRecent:=TStringList.Create;
  Inherited Create(gtk_window_dialog);
  CreateForm;
  LoadCommandLine;
  LoadOptions;
  LoadRecent;
end;

Destructor TMainForm.Destroy;

begin
  SaveRecent;
  FRecent.Free;
  Inherited;
end;

Procedure TMainForm.LoadCommandLine;

Var
  I : Integer;

begin
  I:=1;
  While I<=ParamCount do
    begin
    If FileExists(ParamStr(i)) then
      OpenFile(Paramstr(I));
    Inc(I);
    end;
end;


Procedure TMainForm.LoadRecent;

Var
  I,Count : Integer;
  S : String;

begin
  FRecent.Clear;
  With TInifile.Create(GetoptionFileName) do
    begin
    Count:=ReadInteger('Recent','Count',0);
    For I:=1 to Count do
      begin
      S:=ReadString('Recent','File'+IntToStr(i),'');
      If S<>'' then
        FRecent.Add(S);
      end;
    end;
  BuildReopenList;
end;

Procedure TMainForm.SaveRecent;

Var
  I,Count : Integer;
  S : String;

begin
  With TInifile.Create(GetoptionFileName) do
    try
      EraseSection('Recent');
      WriteInteger('Recent','Count',FRecent.Count);
      For I:=1 to FRecent.Count do
        WriteString('Recent','File'+IntToStr(i),FRecent[i-1]);
      UpdateFile;
    Finally
      Free;
    end;
end;


{
  Menu handlers
}


Procedure TMainForm.DialogSetFilename(Sender : TFPGtkWindow;Data : Pointer; Action : Integer;Initiator : TFPGtkObject);

type
  PString = ^AnsiString;

begin
  PString(Data)^:=(Sender as TFPgtkFileSelection).Filename;
end;

Procedure TMainForm.AddTorecent(FN : String);

Var
  Index : Integer;

begin
  FN:=ExpandFileName(FN);
  With FRecent do
    begin
    Index:=IndexOf(FN);
    If Index<>-1 then
      Delete(Index);
    Insert(0,FN);
    While Count>MaxRecentUsed do
      Delete(Count-1);
    end;
  BuildReopenList;
end;

Procedure TMainForm.OpenFile(FN : String);

Var
  EFN : String;

begin
  IF (FN<>'') then
    begin
    If FileExists(FN) then
      With CreatePage do
        begin
        LoadFromFile(FN);
        AddToRecent(Fn);
        end;
    end;
end;

Procedure TMainForm.FileOpenClick(Sender : TFPGtkObject; Data : Pointer);

var
  fs : TFPgtkFileSelection;
  FN : String;
begin
  fs := TFPgtkFileSelection.Create (gtk_window_dialog);
  FN:='';
  with fs do
    begin
    Title:=SOpenFileTitle;
    OKButton.ConnectClicked (@(CloseWithResult), inttopointer(drOk));
    CancelButton.ConnectClicked (@(CloseWindow), nil);
    if execute (nil, @FN, @DialogSetFilename) = drOk then
      OpenFile(FN);
    end;
end;

Procedure TMainForm.FileNewClick(Sender : TFPGtkObject; Data : Pointer);

Const
  template = '<?xml version="1.0" encoding="ISO-8859-1"?>'+LineEnding+
             '<fpdoc-descriptions>'+LineEnding+
             '</fpdoc-descriptions>'+LineEnding;

Var
  S : TStringStream;

begin
  With CreatePage do
    begin
    If FileExists(SFileTemplate) then
      LoadFromFile(SFileTemplate)
    else
      begin
      S:=TStringStream.Create(Template);
      Try
        LoadFromStream(S)
      finally
        S.Free;
      end;
      end;
    end;
end;

Type
  TSkeletonData = Record
    InputFile,
    OutputFile,
    PackageName,
    AdditionalOptions : String;
    DisableArguments,
    DisableResults,
    DisablePrivate,
    DisableProtected,
    DisableSeeAlso,
    DisableErrors : Boolean;
  end;
  PSkeletonData = ^TSkeletonData;

Procedure TMainForm.GetSkeletonData(Sender : TFPGtkWindow;Data : Pointer; Action : Integer;Initiator : TFPGtkObject);

begin
  With (Sender as TMakeSkelForm), PSkeletonData(Data)^ do
    begin
    InputFile:=FInputFile.FileName;
    OutputFile:=FOutputFile.FileName;
    PackageName:=FPackageName.Text;
    AdditionalOptions:=FadditionalOptions.Text;
    DisableArguments:=FDisableArguments.Active;
    DisableResults:=FDisableResults.Active;
    DisablePrivate:=FDisablePrivate.Active;
    DisableProtected:=FDisableProtected.Active;
    DisableSeeAlso:=FDisableSeeAlso.Active;
    DisableErrors:=FDisableErrors.Active;
    end;
end;

Function CreateSkeletonFile(Const S : TSkeletonData) : Boolean;

Var
  Cmd : String;

begin
  With S do
    begin
    cmd:='makeskel ';
    cmd:=cmd+format('--input=''%s %s''',[Inputfile,Additionaloptions]);
    cmd:=cmd+' --output='+OutputFile;
    cmd:=cmd+' --package='+PackageName;
    If DisableErrors then
      cmd:=cmd+' --disable-errors';
    If DisableSeeAlso then
      cmd:=cmd+' --disable-seealso';
    If DisableProtected then
      cmd:=cmd+' --disable-protected'
    else if DisablePrivate then
      cmd:=cmd+' --disable-private';
    If DisableResults then
      cmd:=cmd+' --disable-function-results';
    If DisableArguments then
      cmd:=cmd+' --disable-arguments';
    Writeln(cmd);
    With TProcess.Create(Nil) do
      try
        CommandLine:=cmd;
        options:=[poWaitOnExit];
        Execute;
        If (ExitStatus<>0) then
          begin
          If FileExists(OutputFile) then
            Result:=MessageDlg(SSkelErrorWithFile,[ExitStatus],mtWarning,mbYesNo,0)=mrYes
          else
            begin
            MessageDlg(SSkelErrorWithoutFile,[ExitStatus],mtError,[mbOk],0);
            Result:=False;
            end;
          end
        else
          Result:=FileExists(OutputFile);
      finally
        Free;
      end;
    end;
end;



Procedure TMainForm.FileNewFromFileClick(Sender : TFPGtkObject; Data : Pointer);

Var
  SkeletonData : TSkeletonData;
  CmdLine : String;

begin
  With TMakeSkelform.Create do
    begin
    Title:=SMakeSkelFromSource;
    If Execute(Nil,@SkeletonData,@GetSkeletonData)=drOK Then
      If CreateSkeletonFile(SkeletonData) then
        OpenFile(SkeletonData.OutPutFile)
    end;
end;

Procedure TMainForm.SaveEditor(E : TEditorPage);

begin
  With E do
    begin
    if (FileName=SNewDocument) then
      SaveEditorAs(E)
    else
      SaveToFile(FileName);
    end;
end;

Procedure TMainForm.SaveEditorAs(E : TEditorPage);

var
  fs : TFPgtkFileSelection;
  FN : String;

begin
  fs := TFPgtkFileSelection.Create (gtk_window_dialog);
  FN:='';
  with fs do
    begin
    Title:=SSaveFileTitle;
    OKButton.ConnectClicked (@(CloseWithResult), inttopointer(drOk));
    CancelButton.ConnectClicked (@(CloseWindow), nil);
    FileName:=E.FileName;
    if execute (nil, @FN, @DialogSetFilename) = drOk then
      begin
      IF (FN<>'') then
        begin
        If ExtractFileExt(FN)='' then
          FN:=FN+DefaultExtension;
        E.SaveToFile(FN);
        AddToRecent(FN);
        end;
      end;
    end;
end;

Function TMainForm.CloseEditor(E : TEditorPage) : Boolean;

begin
  Result:=Not E.Modified;
  If Not Result then
    Case MessageDlg(SFileModified,[E.FileName],mtConfirmation,mbYesNoCancel,0) of
      mrYes : begin
              SaveEditor(E);
              E.Free;
              Result:=True;
              end;
      mrNo  : begin
              E.Free;
              Result:=True;
              end;
    end
  else
    E.Free;
end;

Procedure TMainForm.FileSaveClick(Sender : TFPGtkObject; Data : Pointer);

begin
  If Assigned(CurrentEditor) then
    SaveEditor(CurrentEditor);
end;

Procedure TMainForm.FileSaveAsClick(Sender : TFPGtkObject; Data : Pointer);

begin
  If Assigned(CurrentEditor) then
    SaveEditorAs(CurrentEditor);
end;


Procedure TMainForm.FileExitClick(Sender : TFPGtkObject; Data : Pointer);

begin
  If AllowClose then
    Close;//(Sender,Data);
end;


Procedure TMainForm.FileCloseClick(Sender : TFPGtkObject; Data : Pointer);

begin
  If Assigned(CurrentEditor) then
    CloseEditor(CurrentEditor)
  else
    Writeln('No current editor to close')  ;
end;

Function TMainForm.Currenteditor : TEditorPage;

begin
  With FPages do
    Result:=(Page as TEditorPage);
end;

Procedure TMainForm.GetNodeData(Sender : TFPGtkWindow;Data : Pointer; Action : Integer;Initiator : TFPGtkObject);

type
  PString = ^AnsiString;

begin
  With (Sender as TNewNodeForm) do
    PString(Data)^:=FENodeName.Text;
end;


Procedure TMainForm.InsertNodeClick(Sender : TFPGtkObject; Data : Pointer);

Var
  S : AnsiString;
  Nt : TNodeType;

begin
  If (CurrentEditor<>Nil) then
    begin
    With TNewNodeForm.Create do
      begin
      nt:=TNodeType(Data);
      S:=SNew+' '+NodeNames[nt];
      Case nt of
        ntPackage : S:=S+SForFile+ExtractFileName(CurrentEditor.FileName);
        ntModule: If (CurrentEditor.CurrentPackage<>Nil) then
                     S:=S+SForPackage+CurrentEditor.CurrentPackage['name'];
        ntElement: begin
                   If (CurrentEditor.CurrentModule<>Nil) then
                      S:=S+SForModule+CurrentEditor.CurrentModule['name'];
                   If Assigned(CurrentEditor.CurrentElement) then
                     FENodeName.Selection:=CurrentEditor.CurrentElement['name'];
                   end;
        ntTopic : begin
                  if (CurrentEditor.CurrentTopic<>Nil) then
                      S:=S+SForTopic+CurrentEditor.CurrentPackage['name']
                  else if (CurrentEditor.CurrentModule<>Nil) then
                     S:=S+SForModule+CurrentEditor.CurrentModule['name']
                  else if (CurrentEditor.CurrentPackage<>Nil) then
                      S:=S+SForPackage+CurrentEditor.CurrentPackage['name']
                  end;
      end;
      Title:=S;
      S:='';
      If Execute(Nil,@S,@GetNodeData)=drOK Then
        Case nt of
          ntPackage : CurrentEditor.NewPackage(S);
          ntModule  : CurrentEditor.NewModule(S);
          ntElement : CurrentEditor.NewElement(S);
          ntTopic   : CurrentEditor.NewTopic(S);
        end;
      end;
    end;
end;

Procedure TMainForm.TagClick(Sender : TFPGtkObject; Data : Pointer);

begin
  CurrentEditor.InsertTag(TTagType(Data));
end;

Type
  TLinkData = Record
    LText,LTarget : String;
  end;
  PLinkData = ^TLinkData;

  TTableData = Record
    Cols,Rows : Integer;
    UseHeader : Boolean;
  end;
  PTableData = ^TTableData;

Procedure TMainForm.GetLinkData(Sender : TFPGtkWindow;Data : Pointer; Action : Integer;Initiator : TFPGtkObject);

begin
  With (Sender as TLinkForm),PlinkData(Data)^ do
    begin
    LText:=FlinkText.Text;
    LTarget:=FLinkTarget.Entry.Text;
    end;
end;

Procedure TMainForm.GetTableData(Sender : TFPGtkWindow;Data : Pointer; Action : Integer;Initiator : TFPGtkObject);

begin
  With (Sender as TTableForm),PTableData(Data)^ do
    begin
    Rows:=FTableRows.AsInteger;
    Cols:=FTableCols.AsInteger;
    UseHeader:=FUSeHeader.Active;
    end;
end;

Procedure TMainForm.LinkClick(Sender : TFPGtkObject; Data : Pointer);

Var
  LinkData : TLinkData;
  S : TstringList;
  L : TFPgtkListItemGroup;

begin
  If Assigned(CurrentEditor) then
    begin
    L:=TFpGtkListItemGroup.Create;
    Try
      S:=TStringList.Create;
      Try
        S.Sorted:=True;
        CurrentEditor.GetElementList(S);
        L.FillFromList(S);
      finally
        S.Free;
      end;
      With TLinkForm.Create do
        begin
        Title:=SInsertLink;
        FLinkText.Text:=CurrentEditor.CurrentSelection;
        FLinkTarget.List.AppendItems(L);
        If Execute(Nil,@LinkData,@GetLinkData)=drOK Then
          With LinkData do
          CurrentEditor.InsertLink(LTarget,LText);
        end;
    finally
      L.Free;
    end;
    end;
end;

Procedure TMainForm.TableClick(Sender : TFPGtkObject; Data : Pointer);

Var
  TableData : TTableData;

begin
  With TTableForm.Create do
    begin
    Title:=SInsertTable;
    FTableRows.AsInteger:=3;
    FTableCols.AsInteger:=3;
    If Execute(Nil,@TableData,@GetTableData)=drOK Then
      With TableData do
        CurrentEditor.InsertTable(Cols,Rows,UseHeader);
    end;
end;


Function TMainForm.AllowClose : Boolean;

Var
  P : TFPgtkWidget;

begin
  P:=FPages.Page;
  Result:=True;
  While (P<>Nil) and Result do
    begin
    Result:=CloseEditor(P as TEditorPage);
    P:=FPages.Page;
    end;
end;

Function TMainForm.OnDeleteEvent(Sender:TFPgtkWidget; Event:PGdkEvent; data:pointer): boolean;

begin
  Result:=Not AllowClose;
end;

Procedure TMainForm.OptionsClick(Sender : TFPGtkObject; Data : Pointer);

begin
  With TOptionsForm.Create do
    Execute(Nil,Nil,Nil);
end;

Procedure TMainForm.HelpAboutClick(Sender : TFPGtkObject; Data : Pointer);

begin
  With TAboutForm.Create do
    Execute(Nil,Nil,Nil);
end;

Type
  TRecentMenuItem = Class (TFPGtkMenuItem)
    FileName : String;
  end;

Procedure TMainForm.FileReopen(Sender: TFPGTKObject;data : Pointer);

begin
  OpenFile((Sender as TRecentMenuItem).FileName);
end;



procedure TMainForm.BuildReopenList;

  Function NewRecentMenuItem (Nr : Integer;AFileName : string) : TRecentMenuItem;

  begin
    If Nr<10 then
      result := TRecentMenuItem.CreateWithLabel('_'+IntToStr(Nr)+' '+AFileName)
    else
      result := TRecentMenuItem.CreateWithLabel(AFileName);
    result.FileName:=AFileName;
    Result.ConnectActivate(@FileReopen,Nil);
  end;

var I : integer;
    mi : TFPgtkMenuItem;

begin
  with FRecent do
    begin
    with FFileRecent do
      begin
      if assigned(SubMenu) then
        SubMenu.Free;
      SubMenu := TFPgtkMenu.Create;
      with (submenu as TFPgtkMenu) do
        for I := FRecent.count-1 downto 0 do
          begin
          mi := NewRecentMenuItem (I,FRecent[I]);
          Append (mi);
          end;
      end;
    end;
end;

end.
