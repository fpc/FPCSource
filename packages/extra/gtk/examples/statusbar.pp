{

  Converted from C to Pascal by Artur Bac with liitle additions changes
  <arturbac@poczta.onet.pl>
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

Program statusbar;
Uses glib,gtk;
Type
    PGInt = ^gint;
Var
    status_bar : PGtkWidget;
    count         : longint;
Procedure push_item( widget : PGtkWidget;
                data : PGInt); cdecl;
    Var
      buff          : ansistring;
      ptr_buff      : PGChar;
    Begin
      Inc(count);
      Str(count,buff);
      buff := 'Item ' + buff;

      Ptr_buff := PChar(buff); //changing type from ansistring to PGChar == PChar
      gtk_statusbar_push(GTK_STATUSBAR(status_bar), data^, Ptr_buff);
    End;
Procedure pop_item( widget : PGtkWidget;
               data : PGint); cdecl; //i used pointer to gint , not gpointer becouse we can
                                     // read value directly from an adress specified in data
Begin
  gtk_statusbar_pop( GTK_STATUSBAR(status_bar), data^);
End;

Var
    window,
    vbox,
    button1,button2 : PGtkWidget;

    context_id : gint;
Begin
    gtk_set_locale ();
    gtk_init (@argc, @argv);
    gtk_rc_init;
    count:=1;
    //* create a new window
    window := gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_widget_set_usize( GTK_WIDGET (window), 200, 100); // GTK_WIDGET == PGtkWidget
    gtk_window_set_title(GTK_WINDOW (window), 'GTK Statusbar Example');

    vbox := gtk_vbox_new(FALSE, 1);
    gtk_container_add(GTK_CONTAINER(window), vbox);

    status_bar := gtk_statusbar_new();
    gtk_box_pack_start (GTK_BOX (vbox), status_bar, TRUE, TRUE, 0);

    context_id := gtk_statusbar_get_context_id(
                          GTK_STATUSBAR(status_bar), 'Statusbar example');

    button1 := gtk_button_new_with_label('push item');
    gtk_box_pack_start(GTK_BOX(vbox), button1, TRUE, TRUE, 2);

    button2 := gtk_button_new_with_label('pop last item');
    gtk_box_pack_start(GTK_BOX(vbox), button2, TRUE, TRUE, 2);


    gtk_signal_connect(GTK_OBJECT (window), 'delete_event',
                       Gtk_Signal_Func (@gtk_exit), NULL);
    gtk_signal_connect(GTK_OBJECT(button1), 'clicked',
        GTK_SIGNAL_FUNC (@push_item), @context_id );
    gtk_signal_connect(GTK_OBJECT(button2), 'clicked',
        GTK_SIGNAL_FUNC (@pop_item), @context_id );

    gtk_widget_show_all (window);
    gtk_main ();
End.
