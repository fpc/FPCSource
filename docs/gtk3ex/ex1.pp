program ex1;

{$mode objfpc}

uses
 glib,gtk;

var
  Window       : PGtkWidget;
  WinBox       : PGtkWidget;
  MenuBar      : PGtkWidget;
  DisplayLabel : PgtkWidget;

procedure destroy(widget : pGtkWidget ; data: pgpointer ); cdecl;
begin
  gtk_main_quit();
end;

procedure menu(Data : GPointer; Action : Guint; Widget : pGtkWidget); cdecl;

Var
  TheLabel : PgtkWidget;
  LabelText : Pchar;
  S : AnsiString;

begin
  TheLabel  := g_list_nth_data(gtk_container_children(GTK_CONTAINER(Widget)),0);
  gtk_label_get(gtk_Label(theLabel),@LabelText);
  S := 'Chosen menu : ' + Strpas(Labeltext);
  gtk_label_set_text(GTK_LABEL(DisplayLabel),pchar(S));
end;

{
 TGtkItemFactoryEntry = record
    path : Pgchar;
    accelerator : Pgchar;
    callback : TGtkItemFactoryCallback;
    callback_action : guint;
    item_type : Pgchar;
 end;
}

Type
  FC = TGtkItemFactoryCallback;

Const
  NrMenuItems = 21;
  TheMenu : Array[1..NrMenuItems] of TGtkItemFactoryEntry = (
    (path:'/_File';Accelerator:Nil;Callback:Nil;Callback_action:1;item_type:'<Branch>'),
    (path:'/File/_New';Accelerator:'<ctrl>N';Callback:FC(@Menu);Callback_action:1;item_type:Nil),
    (path:'/File/_Open';Accelerator:'<control>O';Callback:FC(@Menu);Callback_action:1;item_type:Nil),
    (path:'/File/_Save';Accelerator:'<control>S';Callback:FC(@Menu);Callback_action:1;item_type:Nil),
    (path:'/File/sep1';Accelerator:Nil;Callback:Nil;Callback_action:1;item_type:'<Separator>'),
    (path:'/File/E_xit';Accelerator:'<alt>X';Callback:FC(@destroy);Callback_action:1;item_type:Nil),
    (path:'/_Radio';Accelerator:Nil;Callback:Nil;Callback_action:1;item_type:'<Branch>'),
    (path:'/Radio/Radio _1';Accelerator:Nil;Callback:Nil;Callback_action:1;item_type:'<RadioItem>'),
    (path:'/Radio/Radio _2';Accelerator:Nil;Callback:Nil;Callback_action:1;item_type:'<RadioItem>'),
    (path:'/Radio/Radio _3';Accelerator:Nil;Callback:Nil;Callback_action:1;item_type:'<RadioItem>'),
    (path:'/_Check';Accelerator:Nil;Callback:Nil;Callback_action:1;item_type:'<Branch>'),
    (path:'/Check/Check _1';Accelerator:Nil;Callback:Nil;Callback_action:1;item_type:'<CheckItem>'),
    (path:'/Check/Check _2';Accelerator:Nil;Callback:Nil;Callback_action:1;item_type:'<CheckItem>'),
    (path:'/Check/Check _3';Accelerator:Nil;Callback:Nil;Callback_action:1;item_type:'<CheckItem>'),
    (path:'/_Toggle';Accelerator:Nil;Callback:Nil;Callback_action:1;item_type:'<Branch>'),
    (path:'/Toggle/Toggle _1';Accelerator:Nil;Callback:Nil;Callback_action:1;item_type:'<CheckItem>'),
    (path:'/Toggle/Toggle _2';Accelerator:Nil;Callback:Nil;Callback_action:1;item_type:'<CheckItem>'),
    (path:'/Toggle/Toggle _3';Accelerator:Nil;Callback:Nil;Callback_action:1;item_type:'<CheckItem>'),
    (path:'/_Help';Accelerator:Nil;Callback:Nil;Callback_action:1;item_type:'<LastBranch>'),
    (path:'/Help/Contents';Accelerator:'F1';Callback:FC(@Menu);Callback_action:1;item_type:Nil),
    (path:'/Help/About';Accelerator:'<mod1>A';Callback:FC(@Menu);Callback_action:1;item_type:Nil)
    );


Procedure MakeMenu;

Var
  Factory : PGtkItemFactory;
  Accel  : PGtkAccelGroup;

begin
  accel:=gtk_accel_group_new;
  factory :=gtk_item_factory_new(GTK_MENU_BAR_TYPE,'<main>',accel);
  gtk_item_factory_create_items(Factory,NrMenuItems,@TheMenu,Nil);
  gtk_window_add_accel_group(GTK_Window(Window),accel);
  MenuBar:=gtk_item_factory_get_widget (Factory, '<main>');
end;


begin
  gtk_init (@argc, @argv);
  window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW(Window),'Menu using an item factory');
  gtk_widget_set_usize (Window, 300, 200);
  gtk_signal_connect (PGTKOBJECT (window), 'destroy',
                      GTK_SIGNAL_FUNC (@destroy), NULL);
  WinBox := gtk_vbox_new(False,0);
  gtk_widget_show(WinBox);
  Gtk_container_add(GTK_CONTAINER(Window),WinBox);
  MakeMenu;
  gtk_widget_show(MenuBar);
  Gtk_box_pack_start(GTK_BOX(Winbox),MenuBar,False,False,2);
  DisplayLabel := gtk_label_new('Chosen menu : (none)');
  gtk_widget_show(DisplayLabeL);
  Gtk_box_pack_start(GTK_BOX(WinBox),DisplayLabel,TRUE,TRUE,10);
  gtk_widget_show (window);
  gtk_main ();
end.
