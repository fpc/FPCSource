{

  This file extracted from the Gtk tutorial.
  entry.c

  Converted from C to Pascal by Frank Loemker
  <floemker@techfak.uni-bielefeld.de>
}
program entry;
uses
  glib,Gdk,Gtk;

procedure enter_callback(widget,entry : PGtkWidget);cdecl;
var
  entry_text : Pgchar;
begin
  entry_text := gtk_entry_get_text(PGtkEntry(entry));
  writeln ('Entry contents: ',entry_text);
end;


procedure entry_toggle_editable (checkbutton, entry: PGtkWidget);cdecl;
begin
  gtk_entry_set_editable(PGtkEntry(entry),
                         gboolean(active(PGtkToggleButton(checkbutton)^)));
end;


procedure entry_toggle_visibility (checkbutton,entry : PGtkWidget);cdecl;
begin
  gtk_entry_set_visibility(PGtkENTRY(entry),
                           gboolean(active(PGtkToggleButton(checkbutton)^)));
end;


var window, vbox, hbox,
  fentry, button, check : PGtkWidget;
begin
  gtk_init (@argc, @argv);

  { create a new window }
  window := gtk_window_new(gtk_WINDOW_TOPLEVEL);
  gtk_widget_set_usize( PGtkWIDGET (window), 200, 100);
  gtk_window_set_title(PGtkWINDOW (window), 'Gtk Entry');
  gtk_signal_connect(PGtkOBJECT (window), 'delete_event',
                     gtk_SIGNAL_FUNC (@gtk_exit), NIL);

  vbox := gtk_vbox_new (false, 0);
  gtk_container_add (PGtkCONTAINER (window), vbox);
  gtk_widget_show (vbox);

  fentry := gtk_entry_new_with_max_length (50);
  gtk_signal_connect(PGtkOBJECT(fentry), 'activate',
                     gtk_SIGNAL_FUNC(@enter_callback),
                     fentry);
  gtk_entry_set_text (PGtkENTRY (fentry), 'hello');
  gtk_entry_append_text (PGtkENTRY (fentry), ' world');
  gtk_entry_select_region (PGtkENTRY (fentry),
                           0, PGtkENTRY(fentry)^.text_length);
  gtk_box_pack_start (PGtkBOX (vbox), fentry, true, true, 0);
  gtk_widget_show (fentry);

  hbox := gtk_hbox_new (false, 0);
  gtk_container_add (PGtkCONTAINER (vbox), hbox);
  gtk_widget_show (hbox);

  check := gtk_check_button_new_with_label('Editable');
  gtk_box_pack_start (PGtkBOX (hbox), check, true, true, 0);
  gtk_signal_connect (PGtkOBJECT(check), 'toggled',
                      gtk_SIGNAL_FUNC(@entry_toggle_editable), fentry);
  gtk_toggle_button_set_active(PGtkTOGGLEBUTTON(check), true);
  gtk_widget_show (check);

  check := gtk_check_button_new_with_label('Visible');
  gtk_box_pack_start (PGtkBOX (hbox), check, true, true, 0);
  gtk_signal_connect (PGtkOBJECT(check), 'toggled',
                      gtk_SIGNAL_FUNC(@entry_toggle_visibility), fentry);
  gtk_toggle_button_set_active(PGtkTOGGLEBUTTON(check), true);
  gtk_widget_show (check);

  button := gtk_button_new_with_label ('Close');
  gtk_signal_connect_object (PGtkOBJECT (button), 'clicked',
                             gtk_SIGNAL_FUNC(@gtk_exit),
                             PGtkOBJECT (window));
  gtk_box_pack_start (PGtkBOX (vbox), button, true, true, 0);

  GTK_WIDGET_SET_FLAGS (button, gtk_CAN_DEFAULT);
  gtk_widget_grab_default (button);
  gtk_widget_show (button);

  gtk_widget_show(window);

  gtk_main();
end.
