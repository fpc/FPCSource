program ex3;

{$mode objfpc}

uses
 gdk,glib,gtk;


procedure destroy(widget : pGtkWidget ; data: pgpointer ); cdecl;
begin
  gtk_main_quit();
end;

procedure menu(widget : pGtkWidget ; data: pgpointer ); cdecl;

Var
  TheLabel : PgtkWidget;
  LabelText : Pchar;
  S : AnsiString;

begin
  If Data<>Nil then
    begin
    TheLabel  := g_list_nth_data(gtk_container_children(GTK_CONTAINER(Widget)),0);
    gtk_label_get(gtk_Label(theLabel),@LabelText);
    S := 'Chosen menu : ' + Strpas(Labeltext);
    gtk_label_set_text(GTK_LABEL(data),pchar(S));
    end;
end;

Function AddMenuToMenuBar (MenuBar : PGtkMenuBar;
                           ShortCuts : PGtkAccelGroup;
                           Caption : AnsiString;
                           CallBack : TgtkSignalFunc;
                           CallBackdata : Pointer;
                           AlignRight : Boolean;
                           Var MenuItem : PgtkMenuItem
                           ) : PGtkMenu;

Var
  Key : guint;
  TheLabel : PGtkLabel;

begin
  MenuItem:=pgtkmenuitem(gtk_menu_item_new_with_label(''));
  If AlignRight Then
    gtk_menu_item_right_justify(MenuItem);
  TheLabel:=GTK_LABEL(GTK_BIN(MenuItem)^.child);
  Key:=gtk_label_parse_uline(TheLabel,Pchar(Caption));
  If Key<>0 then
    gtk_widget_add_accelerator(PGtkWidget(MenuItem),'activate_item',
                               Shortcuts,Key,
                               GDK_MOD1_MASK,GTK_ACCEL_LOCKED);
  Result:=PGtkMenu(gtk_menu_new);
  If CallBack<>Nil then
    gtk_signal_connect(PGtkObject(result),'activate',
                        CallBack,CallBackdata);
  gtk_widget_show(PgtkWidget(MenuItem));
  gtk_menu_item_set_submenu(MenuItem, PgtkWidget(Result));
  gtk_menu_bar_append(MenuBar,PgtkWidget(MenuItem));
end;

Function AddItemToMenu (Menu : PGtkMenu;
                        ShortCuts : PGtkAccelGroup;
                        Caption : AnsiString;
                        ShortCut : AnsiString;
                        CallBack : TgtkSignalFunc;
                        CallBackdata : Pointer
                       ) : PGtkMenuItem;

Var
  Key,Modifiers : guint;
  LocalAccelGroup : PGtkAccelGroup;
  TheLabel : PGtkLabel;

begin
  Result:=pgtkmenuitem(gtk_menu_item_new_with_label(''));
  TheLabel:=GTK_LABEL(GTK_BIN(Result)^.child);
  Key:=gtk_label_parse_uline(TheLabel,Pchar(Caption));
  If Key<>0 then
     begin
     LocalAccelGroup:=gtk_menu_ensure_uline_accel_group(Menu);
     gtk_widget_add_accelerator(PGtkWidget(result),'activate_item',
                                LocalAccelGroup,Key,
                                0,TGtkAccelFlags(0));
     end;
  gtk_menu_append(Menu,pgtkWidget(result));
  If (ShortCut<>'') and (ShortCuts<>Nil) then
    begin
    gtk_accelerator_parse (pchar(ShortCut), @key, @modifiers);
    gtk_widget_add_accelerator(PGtkWidget(result),'activate_item',
                               ShortCuts,Key,
                               modifiers, GTK_ACCEL_VISIBLE);
    end;
  If CallBack<>Nil then
    gtk_signal_connect(PGtkObject(result),'activate',
                       CallBack,CallBackdata);
  gtk_widget_show(PgtkWidget(result));
end;

Function AddImageItemToMenu (Menu : PGtkMenu;
                        ShortCuts : PGtkAccelGroup;
                        Caption : AnsiString;
                        ShortCut : AnsiString;
                        Bitmap : AnsiString;
                        CallBack : TgtkSignalFunc;
                        CallBackdata : Pointer
                       ) : PGtkMenuItem;

Var
  Key,Modifiers : guint;
  LocalAccelGroup : PGtkAccelGroup;
  TheLabel : PGtkLabel;
  Image : PGtkPixmap;
  hbox : PGtkHBox;
  pixmap : PGdkPixmap;
  BitMapdata : PGdkBitmap;

begin
  Result:=pgtkmenuitem(gtk_menu_item_new);
  hbox:=PGtkHBox(gtk_hbox_new(false,0));
  gtk_container_add(pgtkcontainer(result),pgtkWidget(hbox));
  pixmap:=gdk_pixmap_create_from_xpm(Nil,@BitmapData,Nil,pchar(BitMap));
  Image := PgtkPixMap(gtk_pixmap_new(Pixmap,BitmapData));
  gtk_box_pack_start(PGtkBox(hbox),pgtkWidget(image),false,false,0);
  TheLabel:=PgtkLabel(gtk_label_new(''));
  gtk_box_pack_start(PGtkBox(hbox),pgtkWidget(TheLabel),True,True,0);
  Key:=gtk_label_parse_uline(TheLabel,Pchar(Caption));
  If Key<>0 then
     begin
     LocalAccelGroup:=gtk_menu_ensure_uline_accel_group(Menu);
     gtk_widget_add_accelerator(PGtkWidget(result),'activate_item',
                                LocalAccelGroup,Key,
                                0,TGtkAccelFlags(0));
     end;
  gtk_menu_append(Menu,pgtkWidget(result));
  If (ShortCut<>'') and (ShortCuts<>Nil) then
    begin
    gtk_accelerator_parse (pchar(ShortCut), @key, @modifiers);
    gtk_widget_add_accelerator(PGtkWidget(result),'activate_item',
                               ShortCuts,Key,
                               modifiers, GTK_ACCEL_VISIBLE);
    end;
  If CallBack<>Nil then
    gtk_signal_connect(PGtkObject(result),'activate',
                       CallBack,CallBackdata);
  gtk_widget_show_all(PgtkWidget(result));
end;

Function AddSeparatorToMenu(Menu : PgtkMenu) : PgtkMenuItem;

begin
  Result:=pgtkmenuitem(gtk_menu_item_new());
  gtk_menu_append(Menu,pgtkWidget(result));
  gtk_widget_show(PgtkWidget(result));
end;

var
  Window       : PGtkWidget;
  WinBox       : PGtkWidget;
  MenuBar      : PGtkMenuBar;
  FileMenu     : PGtkMenu;
  TempMenuItem : PgtkMenuItem;
  DisplayLabel : PgtkWidget;
  HelpMenu     : PgtkMenu;
  Accel        : PGtkAccelGroup;

begin
  gtk_init (@argc, @argv);
  window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW(Window),'Menu made manually');
  gtk_widget_set_usize (Window, 300, 200);
  gtk_signal_connect (PGTKOBJECT (window), 'destroy',
                    GTK_SIGNAL_FUNC (@destroy), NULL);
  accel:=gtk_accel_group_new;
  gtk_window_add_accel_group(GTK_Window(Window),accel);
  WinBox := gtk_vbox_new(False,0);
  gtk_widget_show(WinBox);
  Gtk_container_add(GTK_CONTAINER(Window),WinBox);
  MenuBar := PgtkMenuBar(gtk_menu_bar_new);
  gtk_widget_show(PgtkWidget(MenuBar));
  Gtk_box_pack_start(GTK_BOX(Winbox),PgtkWidget(MenuBar),False,False,2);
  DisplayLabel := gtk_label_new('Chosen menu : (none)');
  gtk_widget_show(DisplayLabeL);
  Gtk_box_pack_start(GTK_BOX(WinBox),DisplayLabel,TRUE,TRUE,10);
  FileMenu:=AddMenuToMenuBar(MenuBar,accel,'_File',Nil,Nil,False,TempMenuItem);
  AddImageItemToMenu(FileMenu,accel,'_New','<control>N','filenew.xpm',TgtkSignalFunc(@menu),Nil);
  AddImageItemToMenu(FileMenu,accel,'_Open','<control>O','fileopen.xpm',TgtkSignalFunc(@menu),Nil);
  AddImageItemToMenu(FileMenu,accel,'_Save','<control>S','filesave.xpm',TgtkSignalFunc(@menu),Nil);
  AddSeparatorToMenu(PGtkMenu(FileMenu));
  AddItemToMenu(FileMenu,accel,'_Quit','<control>Q',TgtkSignalFunc(@destroy),Nil);
  HelpMenu:=AddMenuToMenuBar(MenuBar,Accel,'_Help',Nil,Nil,True,TempMenuItem);
  AddItemToMenu(HelpMenu,accel,'_Contents','F1',TgtkSignalFunc(@menu),DisplayLabel);
  AddItemToMenu(HelpMenu,accel,'_About','<alt>A',TgtkSignalFunc(@menu),DisplayLabel);
  gtk_widget_show (window);
  gtk_main ();
end.
