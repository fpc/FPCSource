Program gconfcallback2;

{$Mode ObjFpc}

Uses glib, gtk, gconf, gconfclient;

Procedure entry_activated_callback(entry : PGtkWidget; user_data : gpointer); cdecl;
var
  client : PGConfClient;
  str : Pgchar;
begin
  client := PGConfClient(user_data);

  str := gtk_editable_get_chars(GTK_EDITABLE(entry), 0, -1);

  gconf_client_set_string(client, '/extra/test/directory/key',
                          str, nil);

  g_free(str);
end;

var
  window : PGtkWidget;
  entry : PGtkWidget;
  client : PGConfClient;
begin
  gtk_init(@argc, @argv);
  gconf_init(argc, argv, nil);

  window := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_signal_connect(PGtkOBJECT (window), 'delete_event',
                     gtk_SIGNAL_FUNC (@gtk_exit), NIL);

  entry := gtk_entry_new();

  gtk_container_add(GTK_CONTAINER(window), entry);

  client := gconf_client_get_default;

  gconf_client_add_dir(client,
                       '/extra/test/directory',
                       GCONF_CLIENT_PRELOAD_NONE,
                       NULL);


  gtk_signal_connect(GTK_OBJECT(entry), 'activate',
                     GTK_SIGNAL_FUNC(@entry_activated_callback),
                     client);

  gtk_widget_show_all(window);

  gtk_main();
end.
