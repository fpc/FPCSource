unit frmmain;

{$mode objfpc}

Interface

Uses gtk,gdk,glib,sysutils,classes;

Type
  TMainWindow = Record
    FDir,
    FMask : String;
    Window : PGtkWindow;
    Menu : PGtkMenuBar;
    Toolbar : PGtkToolBar;
    DirTree : PGtkTree;
    FileList : PGtkClist;
    Pane : PGtkPaned;
    StatusBar : PGtkStatusBar;
    FilesHeader,DirHeader : PGtkLabel;
    // helper objects - Menu
    Accel : PGtkAccelGroup;
    MFile,
    MView,
    MColumns,
    MHelp,
    // Main menu items
    PMFiles : PGtkMenu;
    MIFile,
    MIFileProperties,
    MIFileDelete,
    MIExit,
    MIColumns,
    MIMask,
    MIAbout,
    MIHelp : PGtkMenuItem;
    MIShowTitles,
    MIShowExt,
    MIShowSize,
    MiShowDate,
    MIShowAttrs : PGtkCheckMenuItem;
    // Files PopupMenu Items:
    PMIFileProperties,
    PMIFileDelete : PGtkMenuItem;
    // Packing boxes
    VBox,
    LeftBox,
    RightBox : PGtkBox;
    // Scroll boxes
    TreeScrollWindow,
    ListScrollWindow : PGtkScrolledWindow;
    // Tree root node.
    RootNode : PGtkTreeItem;

  end;
  PMainWindow = ^TMainWindow;

Function NewMainForm : PMainWindow;
Function NewMainMenu(MainWindow : PMainWindow) : PGtkMenuBar;
Function NewToolbar(MainWindow : PMainWindow) : PGtkToolbar;
Function NewDirtree(MainWindow : PMainWindow) : PGtkTree;
Function NewFileList(MainWindow : PMainWindow) : PGtkClist;
Procedure ShowDir (Window : PMainWindow; Dir : String);
Function NewFilePopupMenu (MainWindow : PMainWindow) : PGtkMenu;

Implementation

uses menus,futils,frmabout,fxbitmaps,frmprops,frmmask;

Const
  SFileExplorer : PChar = 'File explorer';
  SFilesindir = 'Files in directory %s';
  SDirTree : Pchar = 'Directory tree';

{$i filelist.inc}
{$i dirlist.inc}
{$i factions.inc}

procedure destroy(widget : pGtkWidget ; Window : PMainWindow); cdecl;
begin
  gtk_clist_clear(Window^.FileList);
  dispose(Window);
  gtk_main_quit();
end;

procedure DoAbout(widget : pGtkWidget ; data: pgpointer ); cdecl;
begin
  ShowAboutForm(NewAboutform);
end;

Procedure ApplyMask(Mask : String; Data : Pointer);

begin
  PMainWindow(data)^.FMask:=Mask;
  RefreshFileView(PMainWindow(Data));
end;

procedure DoMask(Widget : PGtkWidget ; MainForm : PMainWindow ); cdecl;

Var
  S : AnsiString;

begin
  With NewMaskForm^ do
    begin
    S:=MainForm^.FMask;
    gtk_entry_set_text(EMask,PChar(S));
    CallBack:=@ApplyMask;
    CallBackData:=MainForm;
    gtk_widget_show_all(PgtkWidget(Window));
    end;
end;

Function NewMainForm : PMainWindow;

begin
  Result:=New(PMainWindow);
  With Result^ do
    begin
    FMask:='*.*';
    Window:=PgtkWindow(gtk_window_new(GTK_WINDOW_TOPLEVEL));
    gtk_window_set_title(Window,SFileExplorer);
    gtk_widget_set_usize(PgtkWidget(Window),640,480);
    gtk_signal_connect (PGTKOBJECT (window), 'destroy',
                    GTK_SIGNAL_FUNC (@destroy), Result);
    gtk_widget_realize(PgtkWidget(window));
    Menu:=NewMainMenu(Result);
    ToolBar:=NewToolbar(Result);
    Pane:=PgtkPaned(gtk_hpaned_new);
    StatusBar:=PgtkStatusBar(gtk_statusbar_new);
    FileList:=NewFileList(Result);
    DirTree:=NewDirtree(Result);
    PMFiles:=NewFilePopupMenu(Result);
    FilesHeader:=PgtkLabel(gtk_label_new(pchar(SFilesInDir)));
    DirHeader:=PgtkLabel(gtk_label_new(pchar(SDirTree)));
    LeftBox:=PGtkBox(gtk_vbox_new(false,0));
    gtk_box_pack_start(Leftbox,PGtkWidget(DirHeader),False,False,0);
    gtk_box_pack_start(Leftbox,PgtkWidget(TreeScrollWindow),true,True,0);
    gtk_paned_add1(pane,PGtkWidget(Leftbox));
    RightBox:=PGtkBox(gtk_vbox_new(false,0));
    gtk_box_pack_start(Rightbox,PGtkWidget(FilesHeader),False,False,0);
    gtk_box_pack_start(Rightbox,PGtkWidget(ListScrollWindow),true,True,0);
    gtk_paned_add2(pane,PGtkWidget(Rightbox));
    VBox:=PGtkBox(gtk_vbox_new(false,0));
    gtk_container_add(PGtkContainer(Window),PgtkWidget(VBox));
    gtk_box_pack_start(vbox,PGtkWidget(Menu),False,False,0);
    gtk_widget_show_all(PGtkWidget(vbox));
    gtk_box_pack_start(vbox,PGtkWidget(ToolBar),False,False,0);
    gtk_box_pack_start(vbox,PGtkWidget(Pane),true,true,0);
    gtk_box_pack_start(vbox,PGtkWidget(StatusBar),false,false,0);
    end;
end;



Function NewMainMenu(MainWindow : PMainWindow) : PGtkMenuBar;

begin
  With MainWindow^ do
    begin
    Result:=pgtkmenubar(gtk_menu_bar_new);
    Accel:=gtk_accel_group_new;
    gtk_window_add_accel_group(Window,accel);
    MFile:=AddMenuToMenuBar(Result,accel,'_File',Nil,Nil,False,MIFile);
    MIFileProperties:=AddItemToMenu(MFile,accel,'_Properties','<alt>p',TgtkSignalFunc(@DoProperties),MainWindow);
    MIFileDelete:=AddItemToMenu(MFile,accel,'_Delete','',TgtkSignalFunc(@DeleteFile),MainWindow);
    AddSeparatorToMenu(MFile);
    MIExit:=AddItemToMenu(MFile,accel,'E_xit','<alt>x',TgtkSignalFunc(@destroy),MainWindow);
    MView:=AddMenuToMenuBar(Result,accel,'_View',Nil,Nil,False,MIFile);
    MIShowTitles:=AddCheckItemToMenu(MView,accel,'Hide titles','',TgtkSignalFunc(@ToggleFileListTitles),MainWindow);
    MIColumns:=AddItemToMenu(MView,accel,'Hide columns','',Nil,MainWindow);
    MIMask:=AddItemToMenu(MView,accel,'File Mask','',TGtkSignalFunc(@DoMask),MainWindow);
    MColumns:=PgtkMenu(gtk_menu_new);
    gtk_menu_item_set_submenu(MIColumns, PgtkWidget(MColumns));
    MIShowExt:=AddCheckItemToMenu(MColumns,accel,'Extension','',TgtkSignalFunc(@ToggleFileListColumns),MainWindow);
    MIShowSize:=AddCheckItemToMenu(MColumns,accel,'Size','',TgtkSignalFunc(@ToggleFileListColumns),MainWindow);
    MIShowDate:=AddCheckItemToMenu(MColumns,accel,'Date','',TgtkSignalFunc(@ToggleFileListColumns),MainWindow);
    MIShowAttrs:=AddCheckItemToMenu(MColumns,accel,'Attributes','',TgtkSignalFunc(@ToggleFileListColumns),MainWindow);
    MHelp:=AddMenuToMenuBar(Result,accel,'_Help',Nil,Nil,True,MIHelp);
    MIAbout:=AddItemToMenu(MHelp,accel,'_About','',TgtkSignalFunc(@DoAbout),Nil);
    gtk_widget_show(PgtkWidget(result));
    end;
end;

Function NewToolbar (MainWindow : PMainWindow) : PGtkToolbar;

begin
  Result:=pGtkToolBar(gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL,GTK_TOOLBAR_ICONS));
  gtk_toolbar_append_item(result,
                          Nil,
                          'File Properties',
                          nil,
                          CreateWidgetFromXPm(PgtkWidget(MainWindow^.Window),@PropertiesXPM),
                          TgtkSignalFunc(@DoProperties),
                          MainWindow);
  gtk_toolbar_append_item(result,
                          Nil,
                          'Delete File',
                          Nil,
                          CreateWidgetFromXPm(PgtkWidget(MainWindow^.Window),@DeleteXPM),
                          TgtkSignalFunc(@DeleteFile),
                          MainWindow);
end;



Procedure ShowDir (Window : PMainWindow; Dir : String);

begin
  With Window^ do
    begin
    FDir:=Dir;
    FillList(FileList,Dir,FMask);
    gtk_label_set_text(FilesHeader,pchar(Format(SFilesInDir,[Dir])));
    end;
end;

Procedure PMFilesActivate(Widget : PGtkWidget; Window : PMainWindow); cdecl;

Var State : TGtkStateType;

begin
  if GetFileSelectionCount(Window^.FileList)>1 then
    State:=GTK_STATE_INSENSITIVE
  else
    State:=GTK_STATE_Normal;
  gtk_widget_set_state(PgtkWidget(Window^.PMIFileProperties),State);
end;


Function NewFilePopupMenu (MainWindow : PMainWindow) : PGtkMenu;

begin
  result:=PGtkMenu(gtk_menu_new);
  gtk_signal_connect(PGtkObject(result),'show',
                     TGtkSignalFunc(@PMFilesActivate),MainWindow);
  With MainWindow^ do
    begin
    PMIFileProperties:=AddItemToMenu(Result,Accel,'_Properties','',TgtkSignalFunc(@DoProperties),MainWindow);
    PMIFileDelete:=AddItemToMenu(Result,Accel,'_Delete','<ctrl>d',TgtkSignalFunc(@DeleteFile),MainWindow);
    end;
end;

end.