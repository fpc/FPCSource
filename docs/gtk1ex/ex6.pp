program ex6;

{$mode objfpc}

uses
 glib,gtk;

Type
  TButtonSignalState = Record
    Obj : PgtkObject;
    SignalID : longint;
    Disable : Boolean;
  end;
  PButtonSignalState = ^TButtonSignalState;

procedure destroy(widget : pGtkWidget ; data: pgpointer ); cdecl;
begin
  gtk_main_quit();
end;


procedure disablesignal(widget : pGtkWidget ; data: pgpointer ); cdecl;


begin
 With PButtonSignalState(Data)^ do
   begin
   If Disable then
     gtk_signal_handler_block(Obj,SignalID)
   else
     gtk_signal_handler_unblock(Obj,SignalID);
   disable:=Not disable;
   end;
end;

var
  window : PGtkWidget;
  quitbutton : PGtkWidget;
  disablebutton : PGTKWidget;
  windowbox : PGTKWidget;
  quitsignal : guint;
  QuitState : TButtonSignalState;

begin
  gtk_init (@argc, @argv);
  window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
  quitbutton := gtk_button_new_with_label('Quit program');
  disablebutton := gtk_button_new_with_label('Disable button');
  windowbox:=gtk_vbox_new(TRUE,10);
  gtk_box_pack_start(GTK_BOX(windowbox),disablebutton,True,false,0);
  gtk_box_pack_start(GTK_BOX(windowbox),quitbutton,True,false,0);
  gtk_container_set_border_width(GTK_CONTAINER(Window),10);
  gtk_container_add(GTK_Container(window),windowbox);
  gtk_signal_connect (PGTKOBJECT (window), 'destroy',
                      GTK_SIGNAL_FUNC (@destroy), NULL);
  With QuitState do
    begin
    Obj:=PGTKObject(QuitButton);
    SignalID:=gtk_signal_connect_object(Obj,'clicked',
                      GTK_SIGNAL_FUNC(@gtk_widget_destroy),
                      PGTKOBJECT(window));
    Disable:=True;
    end;
  gtk_signal_connect(PGTKOBJECT(disablebutton),'clicked',
                      GTK_SIGNAL_FUNC(@disablesignal),@QuitState);
  gtk_widget_show (quitbutton);
  gtk_widget_show (disablebutton);
  gtk_widget_show (windowbox);
  gtk_widget_show (window);
  gtk_main ();
end.
