program ex1;

{$mode objfpc}

uses
 glib,gtk;

procedure destroy(widget : pGtkWidget ; data: pgpointer ); cdecl;
begin
  gtk_main_quit();
end;

var
  window :  pGtkWidget;

begin
  gtk_init (@argc, @argv);
  window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_signal_connect (pGTKOBJECT (window), 'destroy',
                    GTK_SIGNAL_FUNC (@destroy), NULL);
  gtk_widget_show (window);
  gtk_main ();
end.
