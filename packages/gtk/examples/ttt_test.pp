{
  $Id$

  Converted from C to Pascal by Frank Loemker
  <floemker@techfak.uni-bielefeld.de>
}
program ttt_test;
uses
  glib,gdk,gtk,
  tictactoe;

{$ifndef win32}
  {$linklib Xext}
{$endif}

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
{
  $Log$
  Revision 1.1  2000-07-13 06:34:01  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:33  peter
    * moved to packages dir

  Revision 1.4  1999/10/05 09:28:26  peter
    * patches from Frank Loemker

  Revision 1.3  1999/06/10 20:00:18  peter
    * fixed tictactoe

  Revision 1.2  1999/05/10 19:18:16  peter
    * more fixes for the examples to work

  Revision 1.1  1999/05/10 09:02:36  peter
    * gtk 1.2 port working

  Revision 1.1  1998/10/21 22:27:01  peter
    + initial version

}
