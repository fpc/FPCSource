(* Application main window
 *
 * Demonstrates a typical application window, with menubar, toolbar, statusbar.
 *)

var
  appwindow_registered : gboolean;

procedure menuitem_cb (callback_data   : gpointer;
                       callback_action : guint;
                       widget          : PGtkWidget);cdecl;
var
  dialog : PGtkWidget;


begin
  dialog := gtk_message_dialog_new (GTK_WINDOW (callback_data),
                                    GTK_DIALOG_DESTROY_WITH_PARENT,
                                    GTK_MESSAGE_INFO,
                                    GTK_BUTTONS_CLOSE,
                                    'You selected or toggled the menu item: "%s"',
                                    [gtk_item_factory_path_from_widget (widget)]);

  // Close dialog on user response
  g_signal_connect (G_OBJECT (dialog),
                    'response',
                    G_CALLBACK (@gtk_widget_destroy),
                    NULL);

  gtk_widget_show (dialog);
end;



procedure toolbar_cb (button : PGtkWidget;
                      data   : gpointer); cdecl;
var
  dialog: PGtkWidget;
begin

  dialog := gtk_message_dialog_new (GTK_WINDOW (data),
                                    GTK_DIALOG_DESTROY_WITH_PARENT,
                                    GTK_MESSAGE_INFO,
                                    GTK_BUTTONS_CLOSE,
                                    'You selected a toolbar button');

  (* Close dialog on user response *)
  g_signal_connect (G_OBJECT (dialog),
                    'response',
                    G_CALLBACK (@gtk_widget_destroy),
                    NULL);

  gtk_widget_show (dialog);
end;

const

  menu_items :  array [1..18] of TGtkItemFactoryEntry = (
  ( path: '/_File';     accelerator:  NULL;          callback: nil;
    callback_action: 0;  item_type : '<Branch>'; extra_data: NULL),

  ( path: '/File/_New'; accelerator:  '<control>N' ; callback: TGtkItemfactoryCallback(@menuitem_cb);
    callback_action: 0;  item_type : '<StockItem>'; extra_data: NULL{GTK_STOCK_NEW}),

  ( path: '/File/_Open'; accelerator:  '<control>O' ; callback: TGtkItemfactoryCallback(@menuitem_cb);
    callback_action: 0;  item_type : '<StockItem>'; extra_data: NULL {GTK_STOCK_OPEN}),

  ( path: '/File/_Save'; accelerator:  '<control>S' ; callback: TGtkItemfactoryCallback(@menuitem_cb);
    callback_action: 0;  item_type : '<StockItem>'; extra_data: NULL {GTK_STOCK_SAVE}),

  ( path: '/File/Save _As'; accelerator:  NULL ; callback: TGtkItemfactoryCallback(@menuitem_cb);
    callback_action: 0;  item_type : '<StockItem>'; extra_data: NULL {GTK_STOCK_SAVE_AS}),

  ( path: '/File/sep1'; accelerator:  NULL; callback: TGtkItemfactoryCallback(@menuitem_cb);
    callback_action: 0;  item_type : '<Separator>'; extra_data: NULL),

  ( path: '/File/_Quit'; accelerator:  '<control>Q' ; callback: TGtkItemfactoryCallback(@menuitem_cb);
    callback_action: 0;  item_type : '<StockItem>'; extra_data: NULL),

  ( path: '/Preferences'; accelerator:  NULL ; callback: nil;
    callback_action: 0;  item_type : '<Branch>'; extra_data: NULL),

  ( path: '/Preferences/_Color'; accelerator:  NULL ; callback: nil;
    callback_action: 0;  item_type : '<Branch>'; extra_data: NULL),

  ( path: '/Preferences/Color/_Red'; accelerator:  NULL ; callback: TGtkItemfactoryCallback(@menuitem_cb);
    callback_action: 0;  item_type : '<RadioItem>'; extra_data: NULL),

  ( path: '/Preferences/Color/_Green'; accelerator:  NULL ; callback: TGtkItemfactoryCallback(@menuitem_cb);
    callback_action: 0;  item_type : '/Preferences/Color/Red'; extra_data: NULL),

  ( path: '/Preferences/Color/_Blue'; accelerator:  NULL ; callback: TGtkItemfactoryCallback(@menuitem_cb);
    callback_action: 0;  item_type : '/Preferences/Color/Red'; extra_data: NULL),

  ( path: '/Preferences/_Shape'; accelerator:  NULL ; callback: nil;
    callback_action: 0;  item_type : '<Branch>'; extra_data: NULL),

  ( path: '/Preferences/Shape/_Square'; accelerator:  NULL ; callback: TGtkItemfactoryCallback(@menuitem_cb);
    callback_action: 0;  item_type : '<RadioItem>'; extra_data: NULL),

  ( path: '/Preferences/Shape/_Rectangle'; accelerator:  NULL ; callback: TGtkItemfactoryCallback(@menuitem_cb);
    callback_action: 0;  item_type : '/Preferences/Shape/Square'; extra_data: NULL),

  ( path: '/Preferences/Shape/_Oval'; accelerator:  NULL ; callback: TGtkItemfactoryCallback(@menuitem_cb);
    callback_action: 0;  item_type : '/Preferences/Shape/Rectangle'; extra_data: NULL),


  (* If you wanted this to be right justified you would use "<LastBranch>", not "<Branch>".
   * Right justified help menu items are generally considered a bad idea now days.
   *)

  ( path: '/_Help'; accelerator:  NULL ; callback: nil;
    callback_action: 0;  item_type : '<Branch>'; extra_data: NULL),

  ( path: '/Help/_About'; accelerator:  NULL ; callback: nil;
    callback_action: 0;  item_type : NULL; extra_data: NULL)

    );


var
  application_window  : PGtkWidget;      // global variable (originally called window)



(* This function registers our custom toolbar icons, so they can be themed.
 *
 * It's totally optional to do this, you could just manually insert icons
 * and have them not be themeable, especially if you never expect people
 * to theme your app.
 *)

const
  items :array [1..1] of TGtkStockItem = (
       ( stock_id: 'demo-gtk-logo'; _label: '_GTK!';
         modifier: 0; keyval: 0; translation_domain : NULL)
         );

procedure register_stock_icons;

var
  pixbuf    : PGdkPixbuf;
  factory   : PGtkIconFactory;
  filename  : pgchar;

  icon_set    : PGtkIconSet;
  transparent : PGdkPixbuf;

begin
  if not appwindow_registered then
  begin
      appwindow_registered := TRUE;

      (* Register our stock items *)
      gtk_stock_add (@items[1], high(items));

      (* Add our custom icon factory to the list of defaults *)
      factory := gtk_icon_factory_new ();
      gtk_icon_factory_add_default (factory);

      (* demo_find_file() looks in the the current directory first,
       * so you can run gtk-demo without installing GTK, then looks
       * in the location where the file is installed.
       *)
      pixbuf := NULL;
      filename := demo_find_file ('gtk-logo-rgb.gif', NULL);

      if filename <> NULL then begin
        pixbuf := gdk_pixbuf_new_from_file (filename, NULL);
        g_free (filename);
      end;

      (* Register icon to accompany stock item *)
      if pixbuf <> NULL then
      begin
          (* The gtk-logo-rgb icon has a white background, make it transparent *)
          transparent := gdk_pixbuf_add_alpha (pixbuf, TRUE, $ff, $ff, $ff);

          icon_set := gtk_icon_set_new_from_pixbuf (transparent);
          gtk_icon_factory_add (factory, 'demo-gtk-logo', icon_set);
          gtk_icon_set_unref (icon_set);
          g_object_unref (G_OBJECT (pixbuf));
          g_object_unref (G_OBJECT (transparent));
      end
      else
        g_warning ('failed to load GTK logo for toolbar');

      (* Drop our reference to the factory, GTK will hold a reference. *)
      g_object_unref (G_OBJECT (factory));
    end;
end;

procedure update_statusbar ( buffer    : PGtkTextBuffer;
                             statusbar : PGtkStatusbar);
var
  msg   : pgchar;
  row,
  col   : gint;
  count : gint;
  iter  : TGtkTextIter;

begin

  gtk_statusbar_pop (statusbar, 0); (* clear any previous message, underflow is allowed *)

  count := gtk_text_buffer_get_char_count (buffer);

  gtk_text_buffer_get_iter_at_mark (buffer,
                                    @iter,
                                    gtk_text_buffer_get_insert (buffer));

  row := gtk_text_iter_get_line (@iter);
  col := gtk_text_iter_get_line_offset (@iter);

  msg := g_strdup_printf ('Cursor at row %d column %d - %d chars in document',
                         [row, col, count]);

  gtk_statusbar_push (statusbar, 0, msg);

  g_free (msg);
end;

procedure mark_set_callback (buffer       : PGtkTextBuffer;
                             new_location : PGtkTextIter;
                             mark         : PGtkTextMark;
                             data         : gpointer); cdecl;
begin
  update_statusbar (buffer, GTK_STATUSBAR (data));
end;

function do_appwindow      : PGtkWidget;
var
  table,
  toolbar,
  statusbar,
  contents,
  sw           : PGtkWidget;

  buffer       : PGtkTextBuffer;
  accel_group  : PGtkAccelGroup;
  item_factory : PGtkItemFactory;

begin
  if application_window  = NULL then
  begin
    register_stock_icons ();

    (* Create the toplevel window
     *)

    application_window  := gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title (GTK_WINDOW (application_window ), 'Application Window');


    (* NULL window variable when window is closed *)
    g_signal_connect (G_OBJECT (application_window ), 'destroy',
                        G_CALLBACK (@gtk_widget_destroyed),
                        @application_window );

    table := gtk_table_new (1, 4, FALSE);

    gtk_container_add (GTK_CONTAINER (application_window ), table);

    (* Create the menubar
     *)

    accel_group := gtk_accel_group_new ();
    gtk_window_add_accel_group (GTK_WINDOW (application_window), accel_group);
    g_object_unref (accel_group);

    item_factory := gtk_item_factory_new (GTK_TYPE_MENU_BAR, '<main>', accel_group);

    (* Set up item factory to go away with the window *)
    g_object_ref (item_factory);
    gtk_object_sink (GTK_OBJECT (item_factory));
    g_object_set_data_full (G_OBJECT (application_window ),
                              '<main>',
                              item_factory,
                              TGDestroyNotify (@g_object_unref));

    (* create menu items *)
    menu_items[2].extra_data:=PChar(GTK_STOCK_NEW);
    menu_items[3].extra_data:=PChar(GTK_STOCK_OPEN);
    menu_items[4].extra_data:=PChar(GTK_STOCK_SAVE);
    menu_items[5].extra_data:=PChar(GTK_STOCK_SAVE_AS);
    menu_items[7].extra_data:=PChar(GTK_STOCK_QUIT);

    gtk_item_factory_create_items (item_factory, high (menu_items),
                                   @menu_items[1], application_window );

    gtk_table_attach (GTK_TABLE (table),
                        gtk_item_factory_get_widget (item_factory, '<main>'),
                        (* X direction *)          (* Y direction *)
                        0, 1,                      0, 1,
                        GTK_EXPAND or GTK_FILL,    0,
                        0,                         0);

    (* Create the toolbar
     *)
    toolbar := gtk_toolbar_new ();

    gtk_toolbar_insert_stock (GTK_TOOLBAR (toolbar),
                              GTK_STOCK_OPEN,
                              'This is a demo button with an ''open'' icon',
                              NULL,
                              G_CALLBACK (@toolbar_cb),
                              application_window , (* user data for callback *)
                              -1);  (* -1 means "append" *)

    gtk_toolbar_insert_stock (GTK_TOOLBAR (toolbar),
                              GTK_STOCK_QUIT,
                              'This is a demo button with a ''quit'' icon',
                              NULL,
                              G_CALLBACK (@toolbar_cb),
                              application_window , (* user data for callback *)
                              -1);  (* -1 means "append" *)

    gtk_toolbar_append_space (GTK_TOOLBAR (toolbar));
    gtk_toolbar_insert_stock (GTK_TOOLBAR (toolbar),
                              'demo-gtk-logo',
                              'This is a demo button with a ''gtk'' icon',
                              NULL,
                              G_CALLBACK (@toolbar_cb),
                              application_window , (* user data for callback *)
                              -1);  (* -1 means "append" *)

    gtk_table_attach (GTK_TABLE (table),
                      toolbar,
                      (* X direction *)       (* Y direction *)
                      0, 1,                   1, 2,
                      GTK_EXPAND or GTK_FILL, 0,
                      0,                      0);

    (* Create document
     *)

    sw := gtk_scrolled_window_new (NULL, NULL);

    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
                                    GTK_POLICY_AUTOMATIC,
                                    GTK_POLICY_AUTOMATIC);

    gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (sw),
                                         GTK_SHADOW_IN);

    gtk_table_attach (GTK_TABLE (table),
                      sw,
                      (* X direction *)       (* Y direction *)
                      0, 1,                   2, 3,
                      GTK_EXPAND or GTK_FILL,  GTK_EXPAND or GTK_FILL,
                      0,                      0);

    gtk_window_set_default_size (GTK_WINDOW (application_window ),
                                 200, 200);

    contents := gtk_text_view_new ();

    gtk_container_add (GTK_CONTAINER (sw),
                       contents);

    (* Create statusbar *)

    statusbar := gtk_statusbar_new ();
    gtk_table_attach (GTK_TABLE (table),
                      statusbar,
                      (* X direction *)       (* Y direction *)
                        0, 1,                   3, 4,
                        GTK_EXPAND or GTK_FILL,  0,
                        0,                      0);

    (* Show text widget info in the statusbar *)
    buffer := gtk_text_view_get_buffer (GTK_TEXT_VIEW (contents));

    g_signal_connect_object (buffer,
                             'changed',
                             G_CALLBACK (@update_statusbar),
                             statusbar,
                             0);

    g_signal_connect_object (buffer,
                             'mark_set', (* cursor moved *)
                             G_CALLBACK (@mark_set_callback),
                             statusbar,
                             0);

    update_statusbar (buffer, GTK_STATUSBAR (statusbar));
  end;

  if not (GTK_WIDGET_VISIBLE (application_window )) then
    gtk_widget_show_all (application_window )
  else begin
    gtk_widget_destroy (application_window);
    application_window  := NULL;
  end;

  do_appwindow := application_window ;
end;


