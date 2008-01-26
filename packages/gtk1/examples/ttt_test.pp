{

  Converted from C to Pascal by Frank Loemker
  <floemker@techfak.uni-bielefeld.de>
}
program ttt_test;
uses
  glib,gdk,gtk,
  tictactoe;

procedure win (widget : pGtkWidget ; data: gpointer); cdecl;
begin
  writeln ('Yay!');
  tictactoe_clear (pTICTACTOE (widget));
end;


var
  window,ttt : pGtkWidget;
begin
  gtk_init (@argc, @argv);

  window := gtk_window_new (GTK_WINDOW_TOPLEVEL);

  gtk_window_set_title (pGTKWINDOW (window), 'Aspect Frame');

  gtk_signal_connect (pGTKOBJECT (window), 'destroy',
                      GTK_SIGNAL_FUNC (@gtk_exit), NIL);

  gtk_container_set_border_width (pGTKCONTAINER (window), 10);

  ttt := tictactoe_new ();

  gtk_container_add (pGTKCONTAINER (window), ttt);
  gtk_widget_show (ttt);

  gtk_signal_connect (pGTKOBJECT (ttt), 'tictactoe',
                      GTK_SIGNAL_FUNC (@win), NIL);

  gtk_widget_show (window);

  gtk_main ();
end.
