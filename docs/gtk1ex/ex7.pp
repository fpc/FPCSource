program ex2;

uses
 gdk,glib,gtk,strings;

procedure destroy(widget : pGtkWidget ; data: pgpointer ); cdecl;
begin
  gtk_main_quit();
end;

Const
  Inside  : PChar ='Mouse is over label';
  OutSide : PChar ='Mouse is not over label';

var
  OverLabel : Boolean;
  window, box1, box2, stackbox, label1, Label2  : PGtkWidget;

Procedure ChangeLabel(P : PGtkWidget;
                      Event : PGdkEventCrossing;
                      Var Data : Boolean);cdecl;

begin
 If Not Data then
    gtk_label_set_text(PGTKLABEL(Label2),Inside)
  else
    gtk_label_set_text(PGTKLABEL(Label2),Outside);
 Data := Not Data;
end;

begin
  gtk_init (@argc, @argv);
  window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
  stackbox:=gtk_vbox_new(TRUE,10);
  box1 := gtk_event_box_new();
  label1 := gtk_label_new(strnew('Move mouse over label'));
  gtk_container_add(GTK_CONTAINER(box1),label1);
  box2 := gtk_event_box_new();
  label2 := gtk_label_new(strNew(OutSide));
  gtk_container_add(GTK_CONTAINER(box2),label2);
  gtk_box_pack_start(GTK_BOX(stackbox),box1,TRUE,TRUE,0);
  gtk_box_pack_start(GTK_BOX(stackbox),box2,TRUE,TRUE,0);
  gtk_container_set_border_width(GTK_CONTAINER(Window),5);
  gtk_container_add(GTK_Container(window),stackbox);
  gtk_signal_connect(PGTKOBJECT (window), 'destroy',
                     GTK_SIGNAL_FUNC (@destroy), NULL);
  overlabel:=False;
  gtk_signal_connect(PGTKOBJECT(box1),'enter_notify_event',
                     GTK_SIGNAL_FUNC (@ChangeLabel), @Overlabel);
  gtk_signal_connect(PGTKOBJECT(box1),'leave_notify_event',
                     GTK_SIGNAL_FUNC (@ChangeLabel), @Overlabel);
  gtk_widget_show_all (window);
  gtk_main ();
end.
