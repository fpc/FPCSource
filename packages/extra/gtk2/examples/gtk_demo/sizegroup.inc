(* Size Groups
 *
 * GtkSizeGroup provides a mechanism for grouping a number of
 * widgets together so they all request the same amount of space.
 * This is typically useful when you want a column of widgets to
 * have the same size, but you can't use a GtkTable widget.
 *
 * Note that size groups only affect the amount of space requested,
 * not the size that the widgets finally receive. If you want the
 * widgets in a GtkSizeGroup to actually be the same size, you need
 * to pack them in such a way that they get the size they request
 * and not more. For example, if you are packing your widgets
 * into a table, you would not include the GTK_FILL flag.
 *)

var
  sg_window : PGtkWidget;


const
  color_options : array [0..3] of pchar = ('Red', 'Green', 'Blue', NULL);

  dash_options  : array [0..3] of pchar = ('Solid', 'Dashed', 'Dotted', NULL);

  end_options   : array [0..3] of pchar = ('Square', 'Round', 'Arrow', NULL);


(* Convenience function to create an option menu holding a number of strings
 *)

function create_option_menu (strings : ppchar): PGtkWidget;
var
  menu,
  menu_item,
  option_menu : PGtkWidget;

  str         : ppchar;

begin
  menu := gtk_menu_new ();

  str := strings;
  while str^ <> NULL do
  begin
    menu_item := gtk_menu_item_new_with_label ( str[0]);
    gtk_widget_show (menu_item);

    gtk_menu_shell_append (pGtkMenuShell(menu), menu_item);
    inc(str);
  end;

  option_menu := gtk_option_menu_new ();
  gtk_option_menu_set_menu (pGtkOptionMenu(option_menu), menu);

  create_option_menu := option_menu;
end;

procedure add_row (table      : PGtkTable;
                   row        : integer;
                   size_group : PGtkSizeGroup;
                   label_text : pchar;
                   options    : ppchar);
var
  option_menu : PGtkWidget;
  thelabel    : PGtkWidget;

begin
  thelabel := gtk_label_new_with_mnemonic (label_text);
  gtk_misc_set_alignment (pGtkMisc(thelabel), 0, 1);
  gtk_table_attach (pGtkTable(table), thelabel,
                    0, 1,                   row, row + 1,
                    GTK_EXPAND or GTK_FILL, 0,
                    0,                      0);

  option_menu := create_option_menu (options);
  gtk_label_set_mnemonic_widget (pGtkLabel(thelabel), option_menu);
  gtk_size_group_add_widget (size_group, option_menu);
  gtk_table_attach (pGtkTable(table), option_menu,
                    1, 2,                  row, row + 1,
                    0,                     0,
                    0,                     0);
end;

procedure toggle_grouping (check_button : PGtkToggleButton;
                           size_group   : PGtkSizeGroup); cdecl;
var
  new_mode  : TGtkSizeGroupMode;

begin
  (* GTK_SIZE_GROUP_NONE is not generally useful, but is useful
   * here to show the effect of GTK_SIZE_GROUP_HORIZONTAL by
   * contrast.
   *)

  if gtk_toggle_button_get_active (check_button) then
    new_mode := GTK_SIZE_GROUP_HORIZONTAL
  else
    new_mode := GTK_SIZE_GROUP_NONE;

  gtk_size_group_set_mode (size_group, new_mode);
end;




function do_sizegroup      : PGtkWidget;
var
  table,
  frame,
  vbox,
  check_button  : PGtkWidget;

  size_group    : PGtkSizeGroup;

begin
  if sg_window = NULL then
  begin
    sg_window := gtk_dialog_new_with_buttons ('Size Groups',
                        NULL, 0,
                                            GTK_STOCK_CLOSE,
                                           [ GTK_RESPONSE_NONE,
                                            NULL]);

    gtk_window_set_resizable (pGtkWindow(sg_window), FALSE);

    g_signal_connect (sg_window, 'response',
                        TGCallback(@gtk_widget_destroy), NULL);
    g_signal_connect (sg_window, 'destroy',
                        TGCallback(@gtk_widget_destroyed), @sg_window);

    vbox := gtk_vbox_new (FALSE, 5);
    gtk_box_pack_start (pGtkBox(pGtkDialog (sg_window)^.vbox), vbox, TRUE, TRUE, 0);
    gtk_container_set_border_width (pGtkContainer(vbox), 5);

    size_group := gtk_size_group_new (GTK_SIZE_GROUP_HORIZONTAL);

    (* Create one frame holding color options
     *)
    frame := gtk_frame_new ('Color Options');
    gtk_box_pack_start (pGtkBox(vbox), frame, TRUE, TRUE, 0);

    table := gtk_table_new (2, 2, FALSE);
    gtk_container_set_border_width (pGtkContainer(table), 5);
    gtk_table_set_row_spacings (pGtkTable(table), 5);
    gtk_table_set_col_spacings (pGtkTable(table), 10);
    gtk_container_add (pGtkContainer(frame), table);

    add_row (pGtkTable(table), 0, size_group, '_Foreground', @color_options[0]);
    add_row (pGtkTable(table), 1, size_group, '_Background', @color_options[0]);

    (* And another frame holding line style options
     *)
    frame := gtk_frame_new ('Line Options');
    gtk_box_pack_start (pGtkBox(vbox), frame, FALSE, FALSE, 0);

    table := gtk_table_new (2, 2, FALSE);
    gtk_container_set_border_width (pGtkContainer(table), 5);
    gtk_table_set_row_spacings (pGtkTable(table), 5);
    gtk_table_set_col_spacings (pGtkTable(table), 10);
    gtk_container_add (pGtkContainer(frame), table);

    add_row (pGtkTable(table), 0, size_group, '_Dashing', @dash_options[0]);
    add_row (pGtkTable(table), 1, size_group, '_Line ends', @end_options[0]);

    (* And a check button to turn grouping on and off *)
    check_button := gtk_check_button_new_with_mnemonic ('_Enable grouping');
    gtk_box_pack_start (pGtkBox(vbox), check_button, FALSE, FALSE, 0);

    gtk_toggle_button_set_active (pGtkToggleButton(check_button), TRUE);
    g_signal_connect (check_button, 'toggled',
             TGCallback (@toggle_grouping), size_group);
  end;

  if not GTK_WIDGET_VISIBLE (sg_window) then
    gtk_widget_show_all (sg_window)
  else begin
    gtk_widget_destroy (sg_window);
    sg_window := NULL;
  end;

  do_sizegroup := sg_window;
end;
