program ex8;

uses
 glib,gtk;

procedure destroy(widget : pGtkWidget ; data: pgpointer ); cdecl;
begin
  gtk_main_quit();
end;

var
  window, stackbox, label1, Label2  : PGtkWidget;
  labelstyle : pgtkstyle;

begin
  gtk_init (@argc, @argv);
  window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
  stackbox:=gtk_vbox_new(TRUE,10);
  label1 := gtk_label_new('Red label text');
  labelstyle := gtk_style_copy(gtk_widget_get_style(label1));
  With LabelStyle^.fg[GTK_STATE_NORMAL] do
    begin
    pixel:=0;
    red:=$ffff;
    blue:=0;
    green:=0;
    end;
  gtk_widget_set_style(label1,labelstyle);
  // Uncomment this to see the effect of setting the default style.
  // gtk_widget_set_default_style(labelstyle);
  label2 := gtk_label_new('Black label text');
  gtk_box_pack_start(GTK_BOX(stackbox),label1,TRUE,TRUE,0);
  gtk_box_pack_start(GTK_BOX(stackbox),label2,TRUE,TRUE,0);
  gtk_container_set_border_width(GTK_CONTAINER(Window),5);
  gtk_container_add(GTK_Container(window),stackbox);
  gtk_signal_connect(PGTKOBJECT (window), 'destroy',
                     GTK_SIGNAL_FUNC (@destroy), NULL);
  gtk_widget_show_all (window);
  gtk_main ();
end.
