program glade_test;
{$mode objfpc} {$H+}

{$IFDEF GTK2_0}{$FATAL this demo needs gtk 2.4}{$ENDIF}
{$IFDEF GTK2_2}{$FATAL this demo needs gtk 2.4}{$ENDIF}

uses
  cmem,   { because of: ... ld.so: dl-minimal.c: 134: realloc: Assertion `ptr == alloc_last_block' failed! }
  Glib2, Gdk2, Gtk2, LibGlade2;

procedure open_callback (widget : PGtkWidget;
                         data   : gpointer); cdecl; forward;


function delete_event (widget: PGtkWidget;
                       event : PGdkEvent;
                       data  : gpointer): gboolean;cdecl; forward;


procedure show_glade_file (filename : Pgchar);
var
    xml         : PGladeXML;
    fatal_mask  : TGLogLevelFlags;
    toplevel    : PGtkWidget;
    open        : PGtkWidget;
    exit        : PGtkWidget;

begin
    fatal_mask := g_log_set_always_fatal (G_LOG_FATAL_MASK);

    g_log_set_always_fatal (fatal_mask or G_LOG_LEVEL_WARNING or G_LOG_LEVEL_CRITICAL);

    if filename <> NULL  then begin
        { read the glade xml file }
        xml := glade_xml_new (filename, NULL, NULL);

        { get the pointers to the widgets }

        toplevel := glade_xml_get_widget (xml, 'MainWindow');

        exit := glade_xml_get_widget (xml, 'exit');
        open := glade_xml_get_widget (xml, 'open');

        { If the widgets where found in the xml code... }

        { ... connect the signals to the buttons }
        if open <> NULL then
                g_signal_connect (G_OBJECT (open), 'clicked',
                                  G_CALLBACK (@open_callback), toplevel);

        if exit <> NULL then
                g_signal_connect (G_OBJECT (exit), 'clicked',
                                  G_CALLBACK (@delete_event), NULL);

        { and show them all }

        if toplevel <> NULL then
                gtk_widget_show_all (toplevel);

        g_object_unref (G_OBJECT (xml));

   end; { filename }
end;


procedure open_callback (widget : PGtkWidget;
                         data   : gpointer); cdecl;
var
  dialog   : PGtkWidget;
  window   : PGtkWindow;
  action   : gint;
  filename : Pgchar;
  filter   : PGtkFileFilter;

begin
  { Get a pointer to the main window }
  window := GTK_WINDOW (data);

  { create the filechooser dialog }
  dialog := gtk_file_chooser_dialog_new ('Open Glade XML',
                                          window,
                                         GTK_FILE_CHOOSER_ACTION_OPEN,
                                         GTK_STOCK_OPEN, [GTK_RESPONSE_ACCEPT,
                                         GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                         NULL]);

  filter := gtk_file_filter_new; { creates a new GtkFileFilter }

  gtk_file_filter_add_pattern (filter, '*.glade'); { and allow only *.glade files }

  { We now use this filter to display only *.glade files in the filechooser }

  gtk_file_chooser_add_filter ( GTK_FILE_CHOOSER(dialog), filter);

  if gtk_dialog_run (GTK_DIALOG (dialog)) = GTK_RESPONSE_ACCEPT then
  begin
     { get selected file }
     filename := gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));

     { and do something with it }
     show_glade_file (filename);

     g_free (filename);
  end;

  gtk_widget_destroy (dialog);
end;




function delete_event (widget: PGtkWidget;
                       event : PGdkEvent;
                       data  : gpointer): gboolean;cdecl;
begin
  gtk_main_quit;
  delete_event := FALSE;
end;


var
  window,
  button,
  box1     : PGtkWidget;      (* GtkWidget is the storage type for widgets *)

begin

    gtk_init (@argc, @argv);

    window := gtk_window_new (GTK_WINDOW_TOPLEVEL);

    gtk_window_set_title (GTK_WINDOW (window), 'GtkFileChooser and LibGlade  Demo');

    g_signal_connect (G_OBJECT (window), 'delete_event',
                              G_CALLBACK (@delete_event), NULL);


    gtk_container_set_border_width (GTK_CONTAINER (window), 10);

    box1 := gtk_hbox_new (FALSE, 0);

    (* Put the box into the main window. *)
    gtk_container_add (GTK_CONTAINER (window), box1);

    button := gtk_button_new_from_stock (GTK_STOCK_OPEN);


    g_signal_connect (G_OBJECT (button), 'clicked',
                              G_CALLBACK (@open_callback), window);

    gtk_box_pack_start (GTK_BOX(box1), button, TRUE, TRUE, 10);

    (* Always remember this step, this tells GTK that our preparation for
     * this button is complete, and it can now be displayed. *)
    gtk_widget_show (button);


    gtk_widget_show (box1);

    gtk_widget_show (window);


    gtk_main ();
end.
