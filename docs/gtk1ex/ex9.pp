program ex9;

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
  window, button1,Button2, Alabel,stackbox : PGtkWidget;
  buttonstyle : pgtkstyle;
  OverButton : boolean;

Procedure ChangeLabel(P : PGtkWidget;
                      Event : PGdkEventCrossing;
                      Var Data : Boolean);cdecl;

begin
 If Not Data then
    gtk_label_set_text(PGTKLABEL(ALabel),Inside)
  else
    gtk_label_set_text(PGTKLABEL(ALabel),Outside);
 Data := Not Data;
end;

begin
  gtk_init (@argc, @argv);
  window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
  stackbox:=gtk_vbox_new(TRUE,10);
  button1 := gtk_button_new_with_label(strnew('Move mouse over button'));
  buttonstyle := gtk_style_copy(gtk_widget_get_style(Button1));
    With ButtonStyle^.bg[GTK_STATE_PRELIGHT] do
      begin
      pixel:=0;
      red:=$ffff;
      blue:=0;
      green:=0;
      end;
  gtk_widget_set_style(button1,buttonstyle);
  button2 := gtk_button_new;
  ALabel:=gtk_label_new(Outside);
  gtk_container_add(GTK_CONTAINER(button2),ALAbel);
  gtk_box_pack_start(GTK_BOX(stackbox),button1,TRUE,TRUE,0);
  gtk_box_pack_start(GTK_BOX(stackbox),button2,TRUE,TRUE,0);
  gtk_container_set_border_width(GTK_CONTAINER(Window),5);
  gtk_container_add(GTK_Container(window),stackbox);
  gtk_signal_connect(PGTKOBJECT (window), 'destroy',
                     GTK_SIGNAL_FUNC (@destroy), NULL);
  overbutton:=False;
  gtk_signal_connect(PGTKOBJECT(button1),'enter_notify_event',
                     GTK_SIGNAL_FUNC (@ChangeLabel), @OverButton);
  gtk_signal_connect(PGTKOBJECT(button1),'leave_notify_event',
                     GTK_SIGNAL_FUNC (@ChangeLabel), @OverButton);
  gtk_widget_show_all (window);
  gtk_main ();
end.
