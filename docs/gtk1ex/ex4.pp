program ex4;

{$mode objfpc}

uses
 glib,gtk;

function newbutton(ALabel : PChar) : PGtkWidget;

begin
  Result:=gtk_button_new_with_label(ALabel);
  gtk_widget_show(result);
end;

procedure destroy(widget : pGtkWidget ; data: pgpointer ); cdecl;
begin
  gtk_main_quit();
end;

var
  window,
  maintable:  PgtkWidget;

procedure AddToTable(Widget : PGtkWidget;
                     Left,Right, Top,Bottom : guint);
begin
  gtk_table_attach_defaults (GTK_TABLE(MainTable),Widget,
                             Left,right,top,bottom);
end;

begin
  gtk_init (@argc, @argv);
  window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
  Maintable := gtk_table_new(6,6,True);
  gtk_widget_show(MainTable);
  AddToTable(newbutton('1,1 At 1,1'),1,2,1,2);
  AddToTable(newbutton('2,2 At 3,1'),3,5,1,3);
  AddToTable(newbutton('4,1 At 4,1'),1,5,4,5);
  // Put all in window
  gtk_container_set_border_width(GTK_CONTAINER(Window),5);
  gtk_container_add(GTK_Container(window),maintable);
  gtk_signal_connect (PGTKOBJECT (window), 'destroy',
                      GTK_SIGNAL_FUNC (@destroy), NULL);
  gtk_widget_show (window);
  gtk_main ();
end.
