{

  This file extracted from the GTK tutorial.
  pixmap.c

  Converted from C to Pascal by Frank Loemker
  <floemker@techfak.uni-bielefeld.de>
}
program pixmap;
uses
  glib,gdk,gtk;

{ XPM data of Open-File icon }

const
  xpm_data:array[0..19] of pchar =
  ('16 16 3 1',
   '       c None',
   '.      c #000000000000',
   'X      c #FFFFFFFFFFFF',
   '                ',
   '   ......       ',
   '   .XXX.X.      ',
   '   .XXX.XX.     ',
   '   .XXX.XXX.    ',
   '   .XXX.....    ',
   '   .XXXXXXX.    ',
   '   .XXXXXXX.    ',
   '   .XXXXXXX.    ',
   '   .XXXXXXX.    ',
   '   .XXXXXXX.    ',
   '   .XXXXXXX.    ',
   '   .XXXXXXX.    ',
   '   .........    ',
   '                ',
   '                ');
{ when invoked (via signal delete_event), terminates the application. }
procedure close_application(widget : pGtkWidget ; event: pGdkEvent ; data: pgpointer); cdecl;
begin
  gtk_main_quit();
end;

{ is invoked when the button is clicked.  It just prints a message. }
procedure button_clicked(widget : pGtkWidget ; data: pgpointer); cdecl;
begin
  writeln ('button clicked');
end;

{ GtkWidget is the storage type for widgets }
var window, pixmapwid, button : pGtkWidget;
  thepixmap                   : pGdkPixmap;
  mask                        : pGdkBitmap;
  style                       : pGtkStyle;
begin
  { create the main window, and attach delete_event signal to terminating
   the application }
  gtk_init( @argc, @argv );
  gtk_rc_init;

  window := gtk_window_new( GTK_WINDOW_TOPLEVEL );
  gtk_signal_connect (pGTKOBJECT (window), 'delete_event',
                      GTK_SIGNAL_FUNC (@close_application), NIL );
  gtk_container_set_border_width( pGTKCONTAINER (window), 10 );
  gtk_widget_show( window );

  { now for the pixmap from gdk }
  style := gtk_widget_get_style( window );
  thepixmap := gdk_pixmap_create_from_xpm_d( window^.window,  @mask,
                                        @style^.bg[GTK_STATE_NORMAL],
                                        ppgchar (xpm_data ));

  { a pixmap widget to contain the pixmap }
  pixmapwid := gtk_pixmap_new( thepixmap, mask );
  gtk_widget_show( pixmapwid );

  { a button to contain the pixmap widget }
  button := gtk_button_new();
  gtk_container_add( pGTKCONTAINER(button), pixmapwid );
  gtk_container_add( pGTKCONTAINER(window), button );
  gtk_widget_show( button );

  gtk_signal_connect (pGTKOBJECT(button), 'clicked',
                      GTK_SIGNAL_FUNC(@button_clicked), NIL );

  { show the window }
  gtk_main ();
end.
