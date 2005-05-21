{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Main form of GTK debugserver

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}

unit frmmain;

Interface

uses
  fpgtk,fpgtkext,glib,gtk,gdk,debugserverintf,sysutils,msgintf,classes,inifiles;

Type
  TClientCList = Class(TFPGtkScrollClist)
  Protected
    FTabLabel : TFPgtkLabel;
    FClient : TClient;
  Public
    Constructor Create (AClient : TClient);
    Property Client : TClient Read FClient Write FClient;
    Property TabLabel : TFPgtkLabel Read FTabLabel Write FTabLabel;
  end;

  TMainForm = Class (TFPGtkWindow)
  Private
    // 'real' Variables
    FCreating,
    FPaused,
    FInsertNew,
    FCloseOnDisconnect,
    FLoadSinglePage,
    FSinglePage : Boolean;
    FIdleHandle : gint;
    FMsgImages : Array[-1..3] of PgdkPixmap;
    FMsgMasks : Array[-1..3] of Pgdkbitmap;
    // Widgets
    FVBox : TFPGtkVBox;
    FMainMenu : TFpGtkMenuBar;
    FFile,     // File menu
    FFileClear,
    FFileSave,
    FFileExit,
    FFileClose,
    FOptions,
    FHelp,     // Help menu
    FHelpAbout : TFPGtkMenuItem;
    FOptionsInsertNew,
    FOptionsSinglePage,
    FOptionsCloseOnDisconnect,
    FFilePause : TFPGtkCheckMenuItem;
    FConnectImage,
    FDisconnectImage,
    FInfoImage,
    FWarningImage,
    FErrorImage,
    FClearImage,
    FSaveImage,
    FCloseImage,
    FPauseImage,
    FRunImage,
    FRunButtonImage : TFPGtkPixmap;
    FMainToolbar : TFPGtkToolbar;
    FToolbarFrame : TFPGtkFrame;
    FPages : TfpGTKNotebook;
    FList  : TClientCList;
    FRunButton : TFPgtkButton;
    FRunImages : Array[Boolean] of PgdkPixmap;
    FRunMasks : Array[Boolean] of Pgdkbitmap;
  Public
    // General
    Constructor Create;
    Procedure CreateWindow;
    Procedure LoadSettings;
    Procedure SaveSettings;
    Function  SettingsFilename : String;
    Procedure SetupDebugServer;
    Procedure ShutdownDebugServer;
    // List/Client management
    Procedure SaveToFile(C : TClientCList; FN : String);
    Function  NewList(Client : TClient) : TClientCList;
    Function  GetList(Client : TClient) : TClientClist;
    Procedure HaveData(Handle : Gint);
    Procedure AddEventToList(Event : TDebugEvent);
    Function  CheckForNewClient : TClient;
    Function  CurrentList : TClientCList;
    Function  GetClientPageIndex(Client : TClient) : Integer;
    Procedure ConnectionClosed(C : TCLient);
    Procedure SetRunButtonBitmaps;
    Function  GetFileName(ATitle : String) : String;
    Procedure DialogSetFilename(Sender : TFPGtkWindow;Data : Pointer; Action : Integer;Initiator : TFPGtkObject);
    // User callbacks
    Procedure FileClearClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure FileSaveClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure FileCopyClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure FilePauseClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure FileCloseClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure FileExitClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure OptionsClick(Sender : TFPGtkObject; Data : Pointer);
    Procedure HelpAboutClick(Sender : TFPGtkObject; Data : Pointer);
    Function  OnDeleteEvent(Sender:TFPgtkWidget; Event:PGdkEvent; data:pointer): boolean;
  end;


Implementation

uses frmabout,bitmapdata;

ResourceString
  SCaption        = 'FPC Debug server';

  SMenuFile       = '_File';
  SMenuFileClear  = 'Clea_r';
  SMenuFileSave   = '_Save';
  SMenuFileCopy   = '_Copy';
  SMenuFilePause  = '_Pause';
  SMenuFileClose  = 'C_lose';
  SMenuFileExit   = 'E_xit';
  SMenuOptions    = '_Options';
  SMenuOptionsInsertNew         = '_New messages first';
  SMenuOptionsSinglePage        = '_All clients on 1 page';
  SMenuOptionsCloseOnDisconnect = '_Close client on disconnect';
  SMenuHelp       = '_Help';
  SMenuHelpAbout  = '_About';

  SHintFileClear  = 'Clear current list';
  SHintFileSave   = 'Save current list to disk';
  SHintFilePause  = 'Discard new messages';
  SHintFileClose  = 'Close current list';

  SNewClient      = 'New client';
  SMessageFrom    = '[%s] : %s';
  SSaveFileTitle  = 'Save list to file:';
  SNeedsRestart   = 'Restart debugserver for this option to take effect.';

Const
  SKeyParameters        = 'Parameters';
  SKeySinglePage        = 'SinglePage';
  SKeyCloseOnDisconnect = 'CloseOnDisconnect';
  SkeyInsertNew         = 'InsertNewMessages';
  SConfigFile           = 'debugserver.cnf';

  DefaultSinglePage     = True;
  DefaultCloseOnConnect = False;
  DefaultInsertNew      = True;

Const
  DefaultTooltips = True;
  DefaultToolBarStyle = GTK_TOOLBAR_ICONS;
  DefaultToolbarRelief = GTK_RELIEF_NONE;


{ ---------------------------------------------------------------------
    TClientCList
  ---------------------------------------------------------------------}

Constructor TClientCList.Create(Aclient : TClient);

begin
  Inherited Create(3);
  FClient:=AClient;
  Clist.ShowTitles;
  CList.PassiveTitles;
  Clist.ColumnTitle[0]:='*';
  Clist.ColumnTitle[1]:='Time';
  Clist.ColumnTitle[2]:='Message';
  CLIST.SetColumnWidth(0,20);
  CList.SetColumnResizeable(0,False);
  CLIST.SetColumnWidth(1,120);
end;

{ ---------------------------------------------------------------------
    MainForm - General
  ---------------------------------------------------------------------}

// Cludge to go around TFPgtkBin.Child not being public...

Type
  TMyBin = Class(TFPgtkBin)
  Public
    Property Child;
  end;

Procedure TMainForm.CreateWindow;

Var
  V : TFPGtkHBox;
  FAccelGroup: Integer;

begin

  { File Menu }
  Title:=SCaption;
  FAccelGroup:=AccelGroupNew;
  FFileClear := NewMenuItem(SMenuFileClear  ,'','',MakeAccelKeyDef(Self,FaccelGroup,GDK_L,[amcontrol]),@FileClearClick,Nil);
  FFileSave  := NewMenuItem(SMenuFileSave   ,'','',MakeAccelKeyDef(Self,FaccelGroup,GDK_S,[amcontrol]),@FileSaveClick,Nil);
  FFilePause := NewCheckMenuItem(SMenuFilePause  ,'','',MakeAccelKeyDef(Self,FaccelGroup,GDK_P,[amcontrol]),@FilePauseClick,Nil);
  If Not FSinglePage then
    FFileClose := NewMenuItem(SMenuFileClose ,'','',MakeAccelKeyDef(Self,FaccelGroup,GDK_W,[amcontrol]),@FileCloseClick,Nil);
  FFileExit  := NewMenuItem(SMenuFileExit   ,'','',MakeAccelKeyDef(Self,FaccelGroup,GDK_Q,[amcontrol]),@FileExitClick,Nil);
  If FSinglePage then
    FFile  := NewSubMenu(SMenuFile,'','',[FFileClear,FFileSave,FFilePause,NewLine,FFileExit])
  else
    FFile  := NewSubMenu(SMenuFile,'','',[FFileClear,FFileSave,FFilePause,FFileClose,NewLine,FFileExit]);

  { Options menu }

  FOptionsInsertNew := NewCheckMenuItem(SMenuOptionsInsertNew,'','', @OptionsClick,@FInsertNew);
  FOptionsInsertNew.Active:=FInsertNew;
  FOptionsSinglePage := NewCheckMenuItem(SMenuOptionsSinglePage,'','', @OptionsClick,@FLoadSinglePage);
  FOptionsSinglePage.Active:=FSinglePage;
  FOptionsCloseOnDisconnect := NewCheckMenuItem(SMenuOptionsCloseOnDisconnect,'','', @OptionsClick,@FCloseOnDisconnect);
  FOptionsCloseOnDisconnect.active:=FCloseOnDisconnect;
  FOptions := NewSubMenu(SMenuOptions,'','',[FOptionsInsertNew,FOptionsSinglePage,FOptionsCloseOnDisconnect]);

  { Help Menu }

  FHelpAbout:=NewMenuItem(SMenuHelpAbout ,'','',@HelpAboutClick,Nil);
  FHelp := NewSubMenu(SMenuHelp,'','',[FHelpAbout]);

  { Menu bar }

  FMainMenu:=NewMenuBar([FFile,FOptions,FHelp]);

  { Toolbar images }

  FClearImage:=PixmapFromFile('clear');
  FPauseImage:=PixmapFromFile('pause');
  FRunImage:=PixmapFromFile('run');
  FCloseImage:=PixMapFromFile('close');
  FSaveImage:=PixmapFromFile('save');
  FRunButtonImage:=TFPGtkPixmap.Create;
  FRunImages[False]:=FPauseImage.PixMap;
  FRunMasks[False]:=FPauseImage.Mask;
  FRunImages[True]:=FRunImage.PixMap;
  FRunMasks[True]:=FRunImage.Mask;
  SetRunButtonBitmaps;

  { Message images }

  FConnectImage:=PixMapFromFile('connect');
  FDisConnectImage:=PixMapFromFile('disconnect');
  FinfoImage:=PixmapFromFile('info');
  FWarningImage:=PixmapFromFile('warning');
  FErrorImage:=PixmapFromFile('error');

  { Save references for quick lookup}

  FMsgImages[-1]:=FDisconnectImage.PixMap;
  FMsgImages[0]:=FinfoImage.PixMap;
  FMsgImages[1]:=FWarningImage.PixMap;
  FMsgImages[2]:=FErrorImage.PixMap;
  FMsgImages[3]:=FConnectImage.PixMap;
  FMsgMasks[-1]:=FDisconnectImage.Mask;
  FMsgMasks[0]:=FinfoImage.Mask;
  FMsgMasks[1]:=FWarningImage.Mask;
  FMsgMasks[2]:=FErrorImage.Mask;
  FMsgMasks[3]:=FConnectImage.Mask;

  { Toolbar }

  FMainToolBar:=TFPGtkToolbar.Create;
  With FMainToolbar do
    begin
    Style:=DefaultToolbarStyle;
    ButtonRelief:=DefaultToolbarRelief;
    EnableTooltips:=DefaultTooltips;
    AppendItem(SMenuFileClear,SHintFileClear,'',FClearImage,@FileClearClick,Nil);
    AppendItem(SMenuFileSave,SHintFileSave,'',FSaveImage,@FileSaveClick,Nil);
    FRunButton:=AppendItem(SMenuFilePause,SHintFilePause,'',FRunButtonImage,@FilePauseClick,Nil) as TFPgtkButton;
    AppendItem(SMenuFileClose,SHintFileClose,'',FCloseImage,@FileCloseClick,Nil);
    end;

  { place left aligned on frame }
  V:=TFPGtkHBox.Create;
  V.PackStart(FMainToolbar,False,False,2);
  FToolbarFrame:=TFPGtkFrame.Create;
  FtoolbarFrame.Add(V);

  { For Pages }

//  FPages:=TFPGtkNoteBook.Create;
  If FSinglePage then
    FList:=NewList(Nil)
  else
    FPages:=TFPGtkNoteBook.Create;
  { Place on form }
  FVBox:=TFPGTKVBox.create;
  FVBox.PackStart(FMainMenu,false, true, 0);
  FVBox.PackStart(FToolBarFrame,false, true, 0);
  If FSinglePage then
    FVBox.PackStart(FList,true, true, 0)
  else
    FVBox.PackStart(FPages,true, true, 0);
  Self.add(FVBox);
  Setusize(640,480);
  ConnectDeleteEvent(@OnDeleteEvent,Nil);
end;

Constructor TMainForm.Create;

begin
  Inherited Create(gtk_window_dialog);
  LoadSettings;
  FSinglePage:=FLoadSinglePage;
  FPaused:=False;
  FCreating:=True;
  Try
    CreateWindow;
  Finally
    FCreating:=False;
  end;
  SetUpDebugServer;
end;

Procedure TMainForm.SetRunButtonBitmaps;

begin
  FRunButtonImage.SetPixMap(FRunImages[FPaused],FRunMasks[FPaused]);
end;

Function TMainForm.SettingsFileName : String;

Var
  Home : String;

begin
  Home:=GetEnvironmentVariable('HOME');
  If (Home<>'') then
    Result:=IncludeTrailingBackslash(Home)+'.'+SConfigFile
  else
    Result:=SConfigFile;
end;

Procedure TMainForm.LoadSettings;

begin
  With TIniFile.Create(SettingsFileName) do
    Try
      FLoadSinglePage:=ReadBool(SKeyParameters,SkeySinglePage,DefaultSinglePage);
      FCloseOnDisconnect:=ReadBool(SKeyParameters,SkeyCloseOnDisconnect,DefaultCloseOnConnect);
      FInsertNew:=ReadBool(SKeyParameters,SKeyInsertNew,DefaultInsertNew);
    finally
      Free;
    end;
end;

Procedure TMainForm.SaveSettings;

begin
  With TIniFile.Create(SettingsFileName) do
    Try
      WriteBool(SKeyParameters,SkeySinglePage,FLoadSinglePage);
      WriteBool(SKeyParameters,SkeyCloseOnDisconnect,FCloseOnDisconnect);
      WriteBool(SKeyParameters,SKeyInsertNew,FinsertNew);
      UpdateFile;
    finally
      Free;
    end;
end;

{ ---------------------------------------------------------------------
    Event handling.
  ---------------------------------------------------------------------}

// GTK idle callback, to check for new connections during idle time.

function GtkIdle (Data:pointer) : gint; Cdecl;

begin
  With (TObject(Data) as TMainForm) do
    CheckForNewClient;
end;

// When input appears on one of the handles, we come here through the gtk
// input watch.

Procedure GtkHaveInput (Data : GPointer; Source : gint; Condition : TGDKInputCondition);cdecl;

begin
  With (TObject(Data) as TMainForm) do
    HaveData(Source);
end;

// Real methods.

Procedure TMainForm.SetupDebugServer;

begin
  OpenDebugServer;
  FIdleHandle:=gtk_idle_add(@GtkIdle,Self);
  CloseObjConnectionCallBack:=@Self.ConnectionClosed;
end;

Procedure TMainForm.ShutdownDebugServer;

begin
  CloseObjConnectionCallBack:=Nil;
  gtk_idle_remove(FIdleHandle);
  CloseDebugServer;
end;


// tell gdk not to watch this handle any more.
// Clean up of the associated page happens in log event.

Procedure TMainForm.ConnectionClosed(C : TCLient);

begin
  gdk_input_remove(gint(C.Data));
end;

// We get here when data is present on socket.

Procedure TMainForm.HaveData(Handle : Gint);

Var
  Event : TDebugEvent;

begin
  ReadMessageEvent(Handle,Event);
  AddEventToList(Event);
end;

// Check if a new client has reported ?

Function TMainForm.CheckForNewClient : TClient;

Var
  IHandle : gint;
  L : TClientCList;

begin
//  Writeln('Checking new client');
  Result:=CheckNewConnection;
  If Result<>Nil then
    begin
    If Not FSinglePage then
      begin
      L:=NewList(Result);
      FPages.AppendPage(L,L.TabLabel);
      FPages.Page:=L;
      // Force draw.
      Fpages.Draw(Nil);
      L.ShowNow;
      L.Draw(Nil);
      L.Clist.draw(Nil);
      end;
    IHandle:=gdk_input_add(Result.Handle,GDK_INPUT_READ,@GtkHaveInput,self);
    Result.Data:=Pointer(IHandle);
    end;
end;

// Add event data to the appropriate list.

Procedure TMainForm.AddEventToList(Event : TDebugEvent);

Const
  MsgTypeStrings : Array[-1..3] of String = ('*','?','!','!','.');

Var
  T,D,S : String;
  L : TClientCList;
  TL : TFPgtkWidget;
  SL : TStringList;
  Index : Integer;

begin
  With Event do
    begin
    T:=MsgTypeStrings[LogCode];
    D:=DateTimeToStr(TimeStamp);
    If FSinglePage or (logcode<>lctIdentify) then
      S:=Format(SMessageFrom,[Client.Peer,Event])
    else
      S:=Event;
    L:=Nil;
    If Assigned(Client) then
      L:=GetList(Client);
    If L=Nil then
      Writeln('No list found for event ',s)
    else
      begin
      If (LogCode=lctIdentify) then
        If Not FSinglePage then
          L.TabLabel.Text:=Event;
      If Not FPaused then
        begin
        If FInsertNew then
          begin
          Index:=0;
          L.CList.Prepend([T,D,S]);
          end
        else
          Index:=L.CList.Append([T,D,S]);
        L.Clist.SetPixMap(Index,0,FMsgImages[logCode],FmsgMasks[LogCode]);
        end;
      If LogCode=lctStop then
        begin
        L.Client:=Nil; // New clients MAY have the same addresspointer as existing;
        If (Not FSinglePage) and FCloseOnDisconnect then
          L.Free;
        end;
      end;
    end;
  CheckForNewClient;
end;

// Create new list.

Function  TMainForm.NewList(Client : TClient) : TClientCList;

begin
  Result:=TClientCList.Create(Client);
  If Client<>Nil then
    Result.TabLabel:=TfpGtkLabel.Create(SNewClient);
end;

// Get page index on which messages for client are shown.
// Only call when FSinglePage is not true

Function TMainForm.GetClientPageIndex(Client : TClient) : Integer;

Var
  P : TFPgtkWidget;

begin
  With FPages,Children do
    begin
    Result:=Count-1;
    While Result>=0 do
      begin
      P:=GetChildOnPage(Result);
      if Not(P is TClientClist) or
         (TClientClist(P).Client<>Client) Then
         Dec(Result)
      end
    end;
end;

// Get list on which messages for client are shown.
// Only call when FSinglePage is not true

Function  TMainForm.GetList(Client : TClient) : TClientCList;

Var
  I : Integer;
  P : TFPgtkWidget;

begin
  If FSinglePage then
    Result:=FList
  else
    begin
    With FPages,Children do
      For I:=0 to Count-1 do
        begin
        P:=GetChildOnPage(i);
        if P is TClientClist then
          If TClientClist(P).Client=Client Then
            begin
            Result:=TClientClist(P);
            exit
            end;
       end;
    end;
end;

// Return current list.

Function  TMainForm.CurrentList : TClientClist;

begin
  If FSinglePage then
    Result:=FList
  else
    Result:=FPages.Page as TClientCList;
end;

// Write list contents to file.

Procedure TMainForm.SaveToFile(C : TClientCList; FN : String);

Var
  F : System.text;
  I,J : Integer;
  S,T : String;
  P : PGdkPixmap;
  M : PGdkBitmap;

begin
  Assign(F,FN);
  rewrite(F);
  Try
    With C.Clist do
      For I:=0 To count-1 do
        begin
        GetPixmap(I,0,P,M);
        For J:=-1 to 3 do
          If (FMsgImages[J]=P) then
            T:=MsgTypes[J];
        S:=T+' '+CellText[i,1]+' '+Celltext[I,2];
        Writeln(F,S);
        end;
  finally
    System.Close(f);
  end;
end;


{ ---------------------------------------------------------------------
    Callbacks for user events.
  ---------------------------------------------------------------------}


Procedure TMainForm.FileClearClick(Sender : TFPGtkObject; Data : Pointer);

Var
  L:TClientClist;

begin
  L:=CurrentList;
  If L<>Nil then
    L.Clist.Clear;
end;

Procedure TMainForm.DialogSetFilename(Sender : TFPGtkWindow;Data : Pointer; Action : Integer;Initiator : TFPGtkObject);

type
  PString = ^AnsiString;

begin
  PString(Data)^:=(Sender as TFPgtkFileSelection).Filename;
end;

Function TMainForm.GetFileName(ATitle : String) : String;

var
  FS : TFPgtkFileSelection;

begin
  FS := TFPgtkFileSelection.Create (gtk_window_dialog);
  Result:='';
  with FS do
    begin
    Title:=ATitle;
    OKButton.ConnectClicked (@(CloseWithResult), inttopointer(drOk));
    CancelButton.ConnectClicked (@(CloseWindow), nil);
    if Not execute (nil, @Result, @DialogSetFilename) = drOk then
       Result:='';
    end;
end;

Procedure TMainForm.FileSaveClick(Sender : TFPGtkObject; Data : Pointer);

Var
  L:TClientClist;
  FN : String;

begin
  L:=CurrentList;
  If L<>Nil then
    begin
    FN:=GetFileName(SSaveFileTitle);
    If (FN<>'') then
      SaveToFile(L,Fn);
    end;
end;

Procedure TMainForm.FileCopyClick(Sender : TFPGtkObject; Data : Pointer);

// not used for the moment; need to figure out a way to do this.

begin
end;

Procedure TMainForm.FilePauseClick(Sender : TFPGtkObject; Data : Pointer);

begin
  If (Sender<>FFilePause) then
    With FFilePause do
      Active:=Not Active;
  FPaused:=FFilePause.Active;
  SetRunButtonBitmaps;
end;

Procedure TMainForm.FileCloseClick(Sender : TFPGtkObject; Data : Pointer);

Var
  L : TClientCList;

begin
  L:=CurrentList;
  If (L<>Nil) and (L.Client=Nil) then
    L.Free;
end;

Procedure TMainForm.FileExitClick(Sender : TFPGtkObject; Data : Pointer);

begin
  ShutdownDebugServer;
  Close;
end;

Procedure TMainForm.OptionsClick(Sender : TFPGtkObject; Data : Pointer);

Type
  PBoolean = ^boolean;

begin
  If not FCreating then
    begin
    PBoolean(data)^:=Not PBoolean(data)^;
    SaveSettings;
    If (@FLoadSinglePage=Data) then
      ShowMessage(SCaption,SNeedsRestart);
    end;
end;

Procedure TMainForm.HelpAboutClick(Sender : TFPGtkObject; Data : Pointer);

begin
  With TAboutForm.Create do
    Execute(Nil,Nil,Nil);
end;

Function TMainForm.OnDeleteEvent(Sender:TFPgtkWidget; Event:PGdkEvent; data:pointer): boolean;

begin
  ShutdownDebugServer;
  Result:=True;
end;

end.
