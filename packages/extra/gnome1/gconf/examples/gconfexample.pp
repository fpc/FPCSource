Program gconfexample;

Uses glib, gtk, gconf, gconfclient;

Const
  PATH : PChar = '/apps/GNOMEnclature/gconf_example';
  KEY : PChar= '/apps/GNOMEnclature/gconf_example/my_option';

{ Update the GConf key when the user toggles the check button. }
Procedure button_toggled_cb (button : PGtkWidget; data : gpointer); cdecl;
var
  client : PGConfClient;
  thevalue : gboolean;
begin
  client := gconf_client_get_default ();

  thevalue := gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (button));

  gconf_client_set_bool (client, KEY, thevalue, nil);
end;

{ This is called when our key is changed. }
Procedure gconf_notify_func (client : PGConfClient; cnxn_id : guint;
                   entry : PGConfEntry; user_data : gpointer); cdecl;
var
  check_button : PGtkWidget;
  thevalue : PGConfValue;
  checked : gboolean;
begin
  check_button := GTK_WIDGET (user_data);

  thevalue := gconf_entry_get_value (entry);

  checked := gconf_value_get_bool (thevalue);

  { Update the check button accordingly. }
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (check_button),
                                checked);
end;

var
  window : PGtkWidget;
  check_button : PGTKWidget;
  client : PGConfClient;
  thevalue : gboolean;

begin
  gtk_init (@argc, @argv);
  gconf_init(argc, argv, nil);

  client := gconf_client_get_default ();

  window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_signal_connect(GTK_OBJECT (window), 'destroy',
                   @gtk_main_quit, nil);

  { Get the initial value from GConf. }
  thevalue := gconf_client_get_bool (client, KEY, nil);

  check_button := gtk_check_button_new_with_label ('My option');
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (check_button),
                                      thevalue);

  gtk_signal_connect (GTK_OBJECT (check_button), 'toggled',
                          gtk_SIGNAL_FUNC(@button_toggled_cb), nil);

  gtk_container_add (GTK_CONTAINER (window), check_button);

  gtk_widget_show_all (window);

  (* Add our directory to the list of directories the GConfClient will
   * watch.
   *)

  gconf_client_add_dir (client, PATH, GCONF_CLIENT_PRELOAD_NONE, nil);

  { Listen to changes to our key. }

  gconf_client_notify_add (client, KEY, @gconf_notify_func, check_button, nil, nil);

  gtk_main ();

end.
