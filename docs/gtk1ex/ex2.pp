program ex2;

{$mode objfpc}

uses
 glib,gtk;

procedure destroy(widget : pGtkWidget ; data: pgpointer ); cdecl;
begin
  gtk_main_quit();
end;

var
  window : PGtkWidget;
  button : PGtkWidget;

begin
  gtk_init (@argc, @argv);
  window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
  button := gtk_button_new_with_label('Click me');
  gtk_container_set_border_width(GTK_CONTAINER(Window),5);
  gtk_container_add(GTK_Container(window),button);
  gtk_signal_connect (PGTKOBJECT (window), 'destroy',
                      GTK_SIGNAL_FUNC (@destroy), NULL);
  gtk_widget_show (button);
  gtk_widget_show (window);
  gtk_main ();
end.
