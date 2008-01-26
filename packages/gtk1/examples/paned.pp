{

  Converted from C to Pascal by Artur Bac
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
Program panned;
uses glib,gtk;
Function create_list : PGtkWidget; cdecl;
Var
    scrolled_window : PGtkWidget;
    list            : PGtkWidget;
    list_item       : PGtkWidget;
    i               : longint;
    buffer          : ansistring;
    Ptr_Buffer      : PChar ;
Begin
    // Create a new scrolled window, with scrollbars only if needed
    scrolled_window := gtk_scrolled_window_new (NULL, NULL);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
        GTK_POLICY_AUTOMATIC,GTK_POLICY_AUTOMATIC);

    // Create a new list and put it in the scrolled window
    list := gtk_list_new ();
    gtk_scrolled_window_add_with_viewport (
               GTK_SCROLLED_WINDOW (scrolled_window), list);
    gtk_widget_show (list);

    // Add some messages to the window
    for i:=0 to 10 do Begin
        Str (i,buffer);
        buffer := 'Message #' + buffer;
        Ptr_buffer := PChar (buffer);
        list_item := gtk_list_item_new_with_label (Ptr_buffer);
        gtk_container_add (GTK_CONTAINER(list), list_item);
        gtk_widget_show (list_item);
    End;

    create_list := scrolled_window;
End;

{ Add some text_ to our text_ widget - this is a callback that is invoked
when our window is realized. We could also force our window to be
realized with gtk_widget_realize, but it would have to be part of
a hierarchy first }

Procedure realize_text( text_ : PGtkWidget ;
                   data : gpointer); cdecl;
Var
    style   : PGtkStyle;
Begin
    gtk_text_freeze (GTK_text (text_));
    style := gtk_widget_get_style (text_);
    gtk_text_insert (GTK_text (text_), NULL, @style^.black, NULL,
    'From: pathfinder@nasa.gov'+#10+
    'To: mom@nasa.gov'+#10+
    'Subject: Made it!'+#10+
    +#10+
    'We just got in this morning. The weather has been'+#10+
    'great - clear but cold, and there are lots of fun sights.'+#10+
    'Sojourner says hi. See you soon.'+#10+
    ' -Path'+#10, -1);

    gtk_text_thaw (GTK_text (text_));
End;

{ Create a scrolled text area that displays a 'message' }
Function create_text : PGtkWidget; cdecl;
Var
    table,
    text_,
    hscrollbar,
    vscrollbar : PGtkWidget;
Begin
    { Create a table to hold the text_ widget and scrollbars }
    table:= gtk_table_new (2, 2, FALSE);

    { Put a text_ widget in the upper left hand corner. Note the use of
      GTK_SHRINK in the y direction }
    text_ := gtk_text_new (NULL, NULL);
    gtk_table_attach (GTK_TABLE (table), text_, 0, 1, 0, 1,
            GTK_FILL  OR  GTK_EXPAND,
            GTK_FILL  OR  GTK_EXPAND  OR  GTK_SHRINK, 0, 0);
    gtk_widget_show (text_);

    { Put a HScrollbar in the lower left hand corner }
    hscrollbar := gtk_hscrollbar_new (GTK_text (text_)^.hadj);
    gtk_table_attach (GTK_TABLE (table), hscrollbar, 0, 1, 1, 2,
            GTK_EXPAND  OR  GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (hscrollbar);

    { And a VScrollbar in the upper right }
    vscrollbar := gtk_vscrollbar_new (GTK_text (text_)^.vadj);
    gtk_table_attach (GTK_TABLE (table), vscrollbar, 1, 2, 0, 1,
            GTK_FILL, GTK_EXPAND  OR  GTK_FILL  OR  GTK_SHRINK, 0, 0);
    gtk_widget_show (vscrollbar);

    { Add a handler to put a message in the text_ widget when it is realized }
    gtk_signal_connect (GTK_OBJECT (text_), 'realize',
            GTK_SIGNAL_FUNC (@realize_text), NULL);

    create_text := table;
End;

Var
    window,
    vpaned,
    list,
    text_ : PGtkWidget;
Begin
    gtk_set_locale ();
    gtk_init (@argc, @argv);
    gtk_rc_init;

    window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title (GTK_WINDOW (window), 'Paned Windows');
    gtk_signal_connect (GTK_OBJECT (window), 'destroy',
            GTK_SIGNAL_FUNC (@gtk_main_quit), NULL);
    gtk_container_set_border_width (GTK_CONTAINER (window), 10);
    gtk_widget_set_usize (GTK_WIDGET(window), 450, 400);

    { create a vpaned widget and add it to our toplevel window }

    vpaned := gtk_vpaned_new ();
    gtk_container_add (GTK_CONTAINER(window), vpaned);
    gtk_paned_set_handle_size (GTK_PANED(vpaned),
                               10);
//    gtk_paned_set_gutter_size (GTK_PANED(vpaned), 15);
    gtk_widget_show (vpaned);

    { Now create the contents of the two halves of the window }

    list := create_list ();
    gtk_paned_add1 (GTK_PANED(vpaned), list);
    gtk_widget_show (list);

    text_ := create_text ();
    gtk_paned_add2 (GTK_PANED(vpaned), text_);
    gtk_widget_show (text_);
    gtk_widget_show (window);
    gtk_main ();
End.
