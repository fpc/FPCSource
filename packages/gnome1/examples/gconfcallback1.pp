program gconfcallback1;

{$Mode ObjFPC}

Uses glib, gtk, gconf, gconfclient;

Procedure key_changed_callback(client : PGConfClient;
                     cnxn_id : guint;
                     entry : PGConfEntry;
                     user_data: gpointer); cdecl;

var
  thelabel : PGtkWidget;
  thevalue : PGConfValue;
begin
  thelabel := GTK_WIDGET(user_data);

  if (entry = nil) then
    gtk_label_set_text(GTK_LABEL(thelabel), '<unset>')
  else begin
    theValue := gconf_entry_get_value (entry);
    if (thevalue^.thetype = GCONF_VALUE_STRING) then
      gtk_label_set_text(GTK_LABEL(thelabel), gconf_value_get_string(thevalue))
    else
      gtk_label_set_text(GTK_LABEL(thelabel), '<wrong type>');
  end;
end;

var
  window : PGtkWidget;
  thelabel : PGtkWidget;
  client : PGConfClient;
  str : Pgchar;

begin
  gtk_init(@argc, @argv);
  gconf_init(argc, argv, nil);

  client := gconf_client_get_default;

  window := gtk_window_new(GTK_WINDOW_TOPLEVEL);

  gtk_signal_connect(PGtkOBJECT (window), 'delete_event',
                     gtk_SIGNAL_FUNC (@gtk_exit), NIL);


  str := gconf_client_get_string(client, '/extra/test/directory/key',nil);

  If Str <> nil then
    thelabel := gtk_label_new(str)
  else
    thelabel := gtk_label_new('<unset>');

  gtk_container_add(GTK_CONTAINER(window), thelabel);

  gconf_client_add_dir(client,
                       '/extra/test/directory',
                       GCONF_CLIENT_PRELOAD_NONE,
                       nil);

  gconf_client_notify_add(client, '/extra/test/directory/key',
                          @key_changed_callback,
                          thelabel,
                          nil, nil);

  gtk_widget_show_all(window);

  gtk_main();
end.
