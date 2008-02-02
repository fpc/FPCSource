{

  Converted from C to Pascal by Artur Bac with liitle additions by me
  <arturbac@@poczta.onet.pl>
  Reda Poland

}
{$MODE objfpc}
{$H+}
{$S+}
{$HINTS ON}
{$ifdef win32}
  {$define extdecl := stdcall;}
  {$APPTYPE GUI}
{$endif}
{$ifdef unix}
  {$define extdecl := cdecl;}
{$endif}

Program Rulers;

Uses glib,gdk,gtk;

Const
 XSIZE = 600;
 YSIZE = 400;

// This routine gets control when the close button is clicked
Function close_application( widget : PGtkWidget ;
                        event : PGdkEvent;
                        data : gpointer) : boolean; cdecl;
Begin
    gtk_main_quit();
    close_application := false;  //if something go wrong we will know that gtk didn't quit
End;

// The main routine
Var
window ,table, area, hrule, vrule : PGtkWidget;

Begin
    // Initialize GTK and create the main window
    gtk_set_locale ();
    gtk_init( @argc, @argv );
    gtk_rc_init;

    window := gtk_window_new( GTK_WINDOW_TOPLEVEL );
    gtk_signal_connect (GTK_OBJECT (window), 'delete_event',
            GTK_SIGNAL_FUNC( @close_application ), NIL);
    gtk_container_set_border_width (GTK_CONTAINER (window), 10);

    // Create a table for placing the ruler and the drawing area
    table := gtk_table_new( 3, 2, FALSE );
    gtk_container_add( GTK_CONTAINER(window), table );

    area := gtk_drawing_area_new();
    gtk_drawing_area_size( Gtk_Drawing_Area (area), XSIZE, YSIZE );
    gtk_table_attach( GTK_TABLE(table), area, 1, 2, 1, 2,
                      GTK_EXPAND or GTK_FILL, GTK_FILL, 0, 0 );
    gtk_widget_set_events( area, GDK_POINTER_MOTION_MASK or
                                 GDK_POINTER_MOTION_HINT_MASK );

    { The horizontal ruler goes on top. As the mouse moves across the
     * drawing area, a motion_notify_event is passed to the
     * appropriate event handler for the ruler. }
    hrule := gtk_hruler_new();
    gtk_ruler_set_metric( GTK_RULER(hrule), GTK_PIXELS );
    gtk_ruler_set_range( GTK_RULER(hrule), 7, 13, 0, 20 );

    gtk_signal_connect_object( GTK_OBJECT(area), 'motion_notify_event',
                     Gtk_Signal_Func (GTK_WIDGET_CLASS(GTK_OBJECT(hrule)^.klass)
                                    ^.motion_notify_event),
                                        GTK_OBJECT(hrule));

    gtk_table_attach( GTK_TABLE(table), hrule, 1, 2, 0, 1,
                      GTK_EXPAND or GTK_SHRINK  or GTK_FILL, GTK_FILL, 0, 0 );

    {* The vertical ruler goes on the left. As the mouse moves across
     * the drawing area, a motion_notify_event is passed to the
     * appropriate event handler for the ruler. */}

    vrule := gtk_vruler_new();
    gtk_ruler_set_metric( GTK_RULER(vrule), GTK_PIXELS );
    gtk_ruler_set_range( GTK_RULER(vrule), 0, YSIZE, 10, YSIZE );
    gtk_signal_connect_object( GTK_OBJECT(area), 'motion_notify_event',
                               Gtk_Signal_Func
                                  (GTK_WIDGET_CLASS(GTK_OBJECT(vrule)^.klass)
                                  ^.motion_notify_event),
                               GTK_OBJECT(vrule) );


    gtk_table_attach( GTK_TABLE(table), vrule, 0, 1, 1, 2,
                      GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0 );

    // Now show everything changed a little by Me
    {gtk_widget_show( area );
    gtk_widget_show( hrule );
    gtk_widget_show( vrule );
    gtk_widget_show( table );}
    gtk_widget_show_all( window ); //This will show all childs of window
    gtk_main();
End.
