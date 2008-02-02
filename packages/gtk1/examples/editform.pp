{

  This file was created with Glade and comes originally from
  the examples which are delivered with Glade
}

program editform;
uses
  glib,gdk,gtk;

function get_widget(widget:PGtkWidget;widget_name:pchar):PGtkWidget;
var
  found_widget : PGtkWidget;
begin
  if assigned(widget^.parent) then
    widget := gtk_widget_get_toplevel (widget);
  found_widget := gtk_object_get_data (PGtkObject(widget),widget_name);
  {if not assigned(found_widget) then
    g_warning ("Widget not found: %s", widget_name);}
  get_widget := found_widget;
end;

{ This is an internally used function to set notebook tab widgets. }
procedure set_notebook_tab(notebook:PGtkWidget;page_num:gint;widget:PGtkWidget);
var
  page : PGtkNotebookPage;
  notebook_page : PGtkWidget;
begin
  page := g_list_nth (PGtkNoteBook(notebook)^.children, page_num)^.data;
  notebook_page := page^.child;
  gtk_widget_ref (notebook_page);
  gtk_notebook_remove_page (PGtkNoteBook(notebook), page_num);
  gtk_notebook_insert_page (PGtkNoteBook(notebook), notebook_page, widget, page_num);
  gtk_widget_unref (notebook_page);
end;

Function create_window1:PGtkWidget;
var
  tooltips : PGtkTooltips;
  window1 : PGtkWidget;
  scrolledwindow1 : PGtkWidget;
  table1 : PGtkWidget;
  menubar1 : PGtkWidget;
  checkbutton1 : PGtkWidget;
  frame5 : PGtkWidget;
  table4 : PGtkWidget;
  text7 : PGtkWidget;
  entry7 : PGtkWidget;
  label15 : PGtkWidget;
  frame4 : PGtkWidget;
  text6 : PGtkWidget;
  frame3 : PGtkWidget;
  table3 : PGtkWidget;
  text5 : PGtkWidget;
  entry6 : PGtkWidget;
  label14 : PGtkWidget;
  frame2 : PGtkWidget;
  table2 : PGtkWidget;
  entry5 : PGtkWidget;
  label13 : PGtkWidget;
  label12 : PGtkWidget;
  optionmenu6 : PGtkWidget;
  optionmenu6_menu : PGtkWidget;
  glade_menuitem : PGtkWidget;
  optionmenu4 : PGtkWidget;
  optionmenu4_menu : PGtkWidget;
  frame1 : PGtkWidget;
  text2 : PGtkWidget;
  optionmenu2 : PGtkWidget;
  optionmenu2_menu : PGtkWidget;
  label1 : PGtkWidget;
  optionmenu1 : PGtkWidget;
  optionmenu1_menu : PGtkWidget;
begin
  tooltips:=gtk_tooltips_new();
  window1 := gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_object_set_data (GTK_OBJECT (window1), 'window1', window1);
  gtk_widget_set_usize (window1, 600, 400);
  gtk_window_set_title (GTK_WINDOW (window1), 'Auslandszahlung');
  gtk_window_set_policy (GTK_WINDOW (window1), gint(true), gint(true), gint(false));
  gtk_signal_connect(GTK_OBJECT(window1),'destroy',GTK_SIGNAL_FUNC(@gtk_main_quit),nil);

  scrolledwindow1 := gtk_scrolled_window_new (nil, nil);
  gtk_object_set_data (GTK_OBJECT (window1), 'scrolledwindow1', scrolledwindow1);
  gtk_widget_show (scrolledwindow1);
  gtk_container_add (GTK_CONTAINER (window1), scrolledwindow1);
  gtk_viewport_set_shadow_type (GTK_VIEWPORT (GTK_SCROLLED_WINDOW (scrolledwindow1)^.viewport), GTK_SHADOW_NONE);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolledwindow1), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  table1 := gtk_table_new (9, 3, gint(false));
  gtk_object_set_data (GTK_OBJECT (window1), 'table1', table1);
  gtk_widget_show (table1);
  gtk_container_add (GTK_CONTAINER (scrolledwindow1), table1);

  menubar1 := gtk_menu_bar_new ();
  gtk_object_set_data (GTK_OBJECT (window1), 'menubar1', menubar1);
  gtk_widget_show (menubar1);
  gtk_table_attach (GTK_TABLE (table1), menubar1, 0, 3, 0, 1,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);

  checkbutton1 := gtk_check_button_new_with_label ('Wahreneinfuhr');
  gtk_object_set_data (GTK_OBJECT (window1), 'checkbutton1', checkbutton1);
  gtk_widget_show (checkbutton1);
  gtk_table_attach (GTK_TABLE (table1), checkbutton1, 0, 3, 8, 9,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);
  gtk_toggle_button_set_state (GTK_TOGGLE_BUTTON (checkbutton1), gint(true));

  frame5 := gtk_frame_new ('Beg'#252'nstigter');
  gtk_object_set_data (GTK_OBJECT (window1), 'frame5', frame5);
  gtk_widget_show (frame5);
  gtk_table_attach (GTK_TABLE (table1), frame5, 0, 3, 5, 6,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);
  gtk_frame_set_label_align (GTK_FRAME (frame5), 0.1, 0.5);

  table4 := gtk_table_new (2, 2, gint(false));
  gtk_object_set_data (GTK_OBJECT (window1), 'table4', table4);
  gtk_widget_show (table4);
  gtk_container_add (GTK_CONTAINER (frame5), table4);

  text7 := gtk_text_new (nil, nil);
  gtk_object_set_data (GTK_OBJECT (window1), 'text7', text7);
  gtk_widget_show (text7);
  gtk_table_attach (GTK_TABLE (table4), text7, 0, 2, 1, 2,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);
  gtk_text_set_editable (GTK_TEXT (text7), gint(true));

  entry7 := gtk_entry_new ();
  gtk_object_set_data (GTK_OBJECT (window1), 'entry7', entry7);
  gtk_widget_show (entry7);
  gtk_table_attach (GTK_TABLE (table4), entry7, 1, 2, 0, 1,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);

  label15 := gtk_label_new ('Konto-Nr.:');
  gtk_object_set_data (GTK_OBJECT (window1), 'label15', label15);
  gtk_widget_show (label15);
  gtk_table_attach (GTK_TABLE (table4), label15, 0, 1, 0, 1,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);

  frame4 := gtk_frame_new ('Bank des Beg'#252'nstigten');
  gtk_object_set_data (GTK_OBJECT (window1), 'frame4', frame4);
  gtk_widget_show (frame4);
  gtk_table_attach (GTK_TABLE (table1), frame4, 0, 3, 4, 5,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);
  gtk_frame_set_label_align (GTK_FRAME (frame4), 0.1, 0.5);

  text6 := gtk_text_new (nil, nil);
  gtk_object_set_data (GTK_OBJECT (window1), 'text6', text6);
  gtk_widget_show (text6);
  gtk_container_add (GTK_CONTAINER (frame4), text6);
  gtk_text_set_editable (GTK_TEXT (text6), gint(true));

  frame3 := gtk_frame_new ('Auftraggeber');
  gtk_object_set_data (GTK_OBJECT (window1), 'frame3', frame3);
  gtk_widget_show (frame3);
  gtk_table_attach (GTK_TABLE (table1), frame3, 0, 3, 3, 4,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);
  gtk_frame_set_label_align (GTK_FRAME (frame3), 0.1, 0.5);

  table3 := gtk_table_new (2, 2, gint(false));
  gtk_object_set_data (GTK_OBJECT (window1), 'table3', table3);
  gtk_widget_show (table3);
  gtk_container_add (GTK_CONTAINER (frame3), table3);

  text5 := gtk_text_new (nil, nil);
  gtk_object_set_data (GTK_OBJECT (window1), 'text5', text5);
  gtk_widget_show (text5);
  gtk_table_attach (GTK_TABLE (table3), text5, 0, 2, 1, 2,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);
  gtk_text_set_editable (GTK_TEXT (text5), gint(true));
  gtk_widget_realize (text5);
  gtk_text_insert (GTK_TEXT (text5), nil, nil, nil,
                   'Adresse', 7);

  entry6 := gtk_entry_new ();
  gtk_object_set_data (GTK_OBJECT (window1), 'entry6', entry6);
  gtk_widget_show (entry6);
  gtk_table_attach (GTK_TABLE (table3), entry6, 1, 2, 0, 1,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);

  label14 := gtk_label_new ('Konto-Nr.:');
  gtk_object_set_data (GTK_OBJECT (window1), 'label14', label14);
  gtk_widget_show (label14);
  gtk_table_attach (GTK_TABLE (table3), label14, 0, 1, 0, 1,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);

  frame2 := gtk_frame_new ('Betrag');
  gtk_object_set_data (GTK_OBJECT (window1), 'frame2', frame2);
  gtk_widget_show (frame2);
  gtk_table_attach (GTK_TABLE (table1), frame2, 0, 3, 2, 3,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);
  gtk_frame_set_label_align (GTK_FRAME (frame2), 0.1, 0.5);

  table2 := gtk_table_new (2, 2, gint(false));
  gtk_object_set_data (GTK_OBJECT (window1), 'table2', table2);
  gtk_widget_show (table2);
  gtk_container_add (GTK_CONTAINER (frame2), table2);

  entry5 := gtk_entry_new ();
  gtk_object_set_data (GTK_OBJECT (window1), 'entry5', entry5);
  gtk_widget_show (entry5);
  gtk_table_attach (GTK_TABLE (table2), entry5, 1, 2, 0, 1,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);
  gtk_tooltips_set_tip (tooltips, entry5, 'Betrag in der jeweiligen W'#228'hrung', nil);
  gtk_entry_set_text (GTK_ENTRY (entry5), 'Some Text');

  label13 := gtk_label_new ('Betrag in Worten');
  gtk_object_set_data (GTK_OBJECT (window1), 'label13', label13);
  gtk_widget_show (label13);
  gtk_table_attach (GTK_TABLE (table2), label13, 1, 2, 1, 2,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);
  gtk_tooltips_set_tip (tooltips, label13, 'wird automatisch erstellt', nil);

  label12 := gtk_label_new ('in Worten');
  gtk_object_set_data (GTK_OBJECT (window1), 'label12', label12);
  gtk_widget_show (label12);
  gtk_table_attach (GTK_TABLE (table2), label12, 0, 1, 1, 2,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);
  gtk_tooltips_set_tip (tooltips, label12, 'Betrag in Worten, wird automatisch gef'#252'llt', nil);

  optionmenu6 := gtk_option_menu_new ();
  gtk_object_set_data (GTK_OBJECT (window1), 'optionmenu6', optionmenu6);
  gtk_widget_show (optionmenu6);
  gtk_table_attach (GTK_TABLE (table2), optionmenu6, 0, 1, 0, 1,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);
  gtk_tooltips_set_tip (tooltips, optionmenu6, 'W'#228'hrung', nil);
  optionmenu6_menu := gtk_menu_new ();
  glade_menuitem := gtk_menu_item_new_with_label ('DEM');
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu6_menu), glade_menuitem);
  gtk_option_menu_set_menu (GTK_OPTION_MENU (optionmenu6), optionmenu6_menu);

  optionmenu4 := gtk_option_menu_new ();
  gtk_object_set_data (GTK_OBJECT (window1), 'optionmenu4', optionmenu4);
  gtk_widget_show (optionmenu4);
  gtk_table_attach (GTK_TABLE (table1), optionmenu4, 0, 3, 7, 8,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);
  optionmenu4_menu := gtk_menu_new ();
  glade_menuitem := gtk_menu_item_new_with_label ('Ihre Kosten/Spesen zu Lasten des Auftraggebers');
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu4_menu), glade_menuitem);
  glade_menuitem := gtk_menu_item_new_with_label ('Ihre Kosten/Spesen zu Lasten des Beg'#252'nstigten');
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu4_menu), glade_menuitem);
  glade_menuitem := gtk_menu_item_new_with_label ('Fremde Kosten zu Lasten des Auftraggebers');
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu4_menu), glade_menuitem);
  glade_menuitem := gtk_menu_item_new_with_label ('Fremde Kosten zu Lasten des Beg'#252'nstigten');
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu4_menu), glade_menuitem);
  gtk_option_menu_set_menu (GTK_OPTION_MENU (optionmenu4), optionmenu4_menu);

  frame1 := gtk_frame_new ('Verwendungszweck');
  gtk_object_set_data (GTK_OBJECT (window1), 'frame1', frame1);
  gtk_widget_show (frame1);
  gtk_table_attach (GTK_TABLE (table1), frame1, 0, 3, 6, 7,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);
  gtk_frame_set_label_align (GTK_FRAME (frame1), 0.1, 0.5);

  text2 := gtk_text_new (nil, nil);
  gtk_object_set_data (GTK_OBJECT (window1), 'text2', text2);
  gtk_widget_show (text2);
  gtk_container_add (GTK_CONTAINER (frame1), text2);
  gtk_text_set_editable (GTK_TEXT (text2), gint(true));

  optionmenu2 := gtk_option_menu_new ();
  gtk_object_set_data (GTK_OBJECT (window1), 'optionmenu2', optionmenu2);
  gtk_widget_show (optionmenu2);
  gtk_table_attach (GTK_TABLE (table1), optionmenu2, 2, 3, 1, 2,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);
  optionmenu2_menu := gtk_menu_new ();
  glade_menuitem := gtk_menu_item_new_with_label ('DM-Kontos');
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu2_menu), glade_menuitem);
  glade_menuitem := gtk_menu_item_new_with_label ('W'#228'hrungs-Kontos');
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu2_menu), glade_menuitem);
  glade_menuitem := gtk_menu_item_new_with_label ('W'#228'hrungs-Termin-Kontos');
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu2_menu), glade_menuitem);
  gtk_option_menu_set_menu (GTK_OPTION_MENU (optionmenu2), optionmenu2_menu);

  label1 := gtk_label_new ('zu Lasten des');
  gtk_object_set_data (GTK_OBJECT (window1), 'label1', label1);
  gtk_widget_show (label1);
  gtk_table_attach (GTK_TABLE (table1), label1, 1, 2, 1, 2,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);

  optionmenu1 := gtk_option_menu_new ();
  gtk_object_set_data (GTK_OBJECT (window1), 'optionmenu1', optionmenu1);
  gtk_widget_show (optionmenu1);
  gtk_table_attach (GTK_TABLE (table1), optionmenu1, 0, 1, 1, 2,
                    GTK_EXPAND or GTK_SHRINK or GTK_FILL, GTK_EXPAND or GTK_SHRINK or GTK_FILL, 0, 0);
  optionmenu1_menu := gtk_menu_new ();
  glade_menuitem := gtk_menu_item_new_with_label ('Zahlung');
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu1_menu), glade_menuitem);
  glade_menuitem := gtk_menu_item_new_with_label ('Akkreditiv');
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu1_menu), glade_menuitem);
  glade_menuitem := gtk_menu_item_new_with_label ('Inkasso-Einl'#246'sung');
  gtk_widget_show (glade_menuitem);
  gtk_menu_append (GTK_MENU (optionmenu1_menu), glade_menuitem);
  gtk_option_menu_set_menu (GTK_OPTION_MENU (optionmenu1), optionmenu1_menu);

  exit(window1);
end;

var
  window1 : PGtkWidget;
begin
  gtk_set_locale ();
  gtk_init (@argc, @argv);
  gtk_rc_init;

  {
    The following code was added by Glade to create one of each component
    (except popup menus), just so that you see something after building
    the project. Delete any components that you don't want shown initially.
  }
  window1 := create_window1 ();
  gtk_widget_show (window1);

  gtk_main ();
end.
