(* Item Factory
 *
 * The GtkItemFactory object allows the easy creation of menus
 * from an array of descriptions of menu items.
 *)

var
  if_window : PGtkWidget;

procedure gtk_ifactory_cb (callback_data   : gpointer;
                           callback_action : guint;
                           widget          : PGTkWidget); cdecl;
begin
  g_message ('ItemFactory: activated "%s"',
             [gtk_item_factory_path_from_widget (widget) ]);
end;


const
  if_menu_items : array [1..19] of TGtkItemFactoryEntry = (
  ( path: '/_File';     accelerator:  NULL;          callback: nil;
    callback_action: 0;  item_type : '<Branch>'; extra_data: NULL),

  ( path: '/File/tearoff1'; accelerator:  NULL;  callback: TGtkItemfactoryCallback(@gtk_ifactory_cb);
    callback_action: 0;  item_type : '<Tearoff>'; extra_data: NULL),

  ( path: '/File/_New'; accelerator: '<control>N'; callback: TGtkItemfactoryCallback(@gtk_ifactory_cb);
    callback_action: 0;  item_type : NULL; extra_data: NULL),

  ( path: '/File/_Open'; accelerator: '<control>O'; callback: TGtkItemfactoryCallback(@gtk_ifactory_cb);
    callback_action: 0;  item_type : NULL; extra_data: NULL),

  ( path: '/File/_Save'; accelerator: '<control>S'; callback: TGtkItemfactoryCallback(@gtk_ifactory_cb);
    callback_action: 0;  item_type : NULL; extra_data: NULL),

  ( path: '/File/Save _As'; accelerator: NULL; callback: TGtkItemfactoryCallback(@gtk_ifactory_cb);
    callback_action: 0;  item_type : NULL; extra_data: NULL),

  ( path: '/File/sep1'; accelerator: NULL; callback: TGtkItemfactoryCallback(@gtk_ifactory_cb);
    callback_action: 0;  item_type : '<Separator>'; extra_data: NULL),

  ( path: '/File/_Quit'; accelerator: '<control>Q'; callback: TGtkItemfactoryCallback(@gtk_ifactory_cb);
    callback_action: 0;  item_type : NULL; extra_data: NULL),

  ( path: '/_Preferences'; accelerator:  NULL ; callback: nil;
    callback_action: 0;  item_type : '<Branch>'; extra_data: NULL),

  ( path: '/Preferences/_Color'; accelerator:  NULL ; callback: nil;
    callback_action: 0;  item_type : '<Branch>'; extra_data: NULL),

  ( path: '/Preferences/Color/_Red'; accelerator:  NULL ; callback: TGtkItemfactoryCallback(@gtk_ifactory_cb);
    callback_action: 0;  item_type : '<RadioItem>'; extra_data: NULL),

  ( path: '/Preferences/Color/_Green'; accelerator:  NULL ; callback:TGtkItemfactoryCallback(@gtk_ifactory_cb);
    callback_action: 0;  item_type : '/Preferences/Color/Red'; extra_data: NULL),

  ( path: '/Preferences/Color/_Blue'; accelerator:  NULL ; callback: TGtkItemfactoryCallback(@gtk_ifactory_cb);
    callback_action: 0;  item_type : '/Preferences/Color/Red'; extra_data: NULL),

  ( path: '/Preferences/_Shape'; accelerator:  NULL ; callback: nil;
    callback_action: 0;  item_type : '<Branch>'; extra_data: NULL),

  ( path: '/Preferences/Shape/_Square'; accelerator:  NULL ; callback: TGtkItemfactoryCallback(@gtk_ifactory_cb);
    callback_action: 0;  item_type : '<RadioItem>'; extra_data: NULL),

  ( path: '/Preferences/Shape/_Rectangle'; accelerator:  NULL ; callback: TGtkItemfactoryCallback(@gtk_ifactory_cb);
    callback_action: 0;  item_type : '/Preferences/Shape/Square'; extra_data: NULL),

  ( path: '/Preferences/Shape/_Oval'; accelerator:  NULL ; callback: TGtkItemfactoryCallback(@gtk_ifactory_cb);
    callback_action: 0;  item_type : '/Preferences/Shape/Rectangle'; extra_data: NULL),


  (* If you wanted this to be right justified you would use "<LastBranch>", not "<Branch>".
   * Right justified help menu items are generally considered a bad idea now days.
   *)

  ( path: '/_Help'; accelerator:  NULL ; callback: nil;
    callback_action: 0;  item_type : '<Branch>'; extra_data: NULL),

  ( path: '/Help/_About'; accelerator:  NULL ; callback: nil;
    callback_action: 0;  item_type : NULL; extra_data: NULL)
    );

const
  nmenu_items = sizeof (if_menu_items) div sizeof (if_menu_items[1]);


function do_item_factory   : PGtkWidget;
var
  box1,
  box2,
  separator,
  thelabel,
  button       : PGtkWidget;

  accel_group  : PGtkAccelGroup;
  item_factory : PGtkItemFactory;

begin

  if if_window = NULL then
  begin
    if_window := gtk_window_new (GTK_WINDOW_TOPLEVEL);

    g_signal_connect (if_window, 'destroy',
                        G_CALLBACK (@gtk_widget_destroyed), @if_window);

    g_signal_connect (if_window, 'delete-event',
                        G_CALLBACK (@gtk_true), NULL);

    accel_group := gtk_accel_group_new ();

    item_factory := gtk_item_factory_new (GTK_TYPE_MENU_BAR, '<main>', accel_group);

    g_object_set_data_full (G_OBJECT (if_window), '<main>',
                              item_factory, @g_object_unref);
    gtk_window_add_accel_group (GTK_WINDOW (if_window), accel_group);

    gtk_window_set_title (GTK_WINDOW (if_window), 'Item Factory');
    gtk_container_set_border_width (GTK_CONTAINER (if_window), 0);
    gtk_item_factory_create_items (item_factory, nmenu_items, @if_menu_items[1], NULL);

    (* preselect /Preferences/Shape/Oval over the other radios
     *)
    gtk_check_menu_item_set_active
                (GTK_CHECK_MENU_ITEM (gtk_item_factory_get_item (item_factory,
                 '/Preferences/Shape/Oval')),
                 TRUE);

    box1 := gtk_vbox_new (FALSE, 0);
    gtk_container_add (GTK_CONTAINER (if_window), box1);

    gtk_box_pack_start (GTK_BOX (box1),
                          gtk_item_factory_get_widget (item_factory, '<main>'),
                          FALSE, FALSE, 0);

    thelabel := gtk_label_new ('Type'#13#10'<alt>'#13#10'to start');
    gtk_widget_set_size_request (thelabel, 200, 200);
    gtk_misc_set_alignment (GTK_MISC (thelabel), 0.5, 0.5);
    gtk_box_pack_start (GTK_BOX (box1), thelabel, TRUE, TRUE, 0);


    separator := gtk_hseparator_new ();
    gtk_box_pack_start (GTK_BOX (box1), separator, FALSE, TRUE, 0);

    box2 := gtk_vbox_new (FALSE, 10);
    gtk_container_set_border_width (GTK_CONTAINER (box2), 10);
    gtk_box_pack_start (GTK_BOX (box1), box2, FALSE, TRUE, 0);

    button := gtk_button_new_with_label ('close');
    g_signal_connect_swapped (button, 'clicked',
                                G_CALLBACK (@gtk_widget_destroy), if_window);


    gtk_box_pack_start (GTK_BOX (box2), button, TRUE, TRUE, 0);

    GTK_WIDGET_SET_FLAGS  (button, GTK_CAN_DEFAULT);

    gtk_widget_grab_default (button);

    gtk_widget_show_all (if_window);
  end else
  begin
    gtk_widget_destroy (if_window);
    if_window := NULL;
  end;

  do_item_factory := if_window;
end;
