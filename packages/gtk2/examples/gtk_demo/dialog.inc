(* Dialog and Message Boxes
 *
 * Dialog widgets are used to pop up a transient window for user feedback.
 *)




var
  dialog_entry1,
  dialog_entry2,
  dialog_window  : PGtkWidget;
  dialog_i       : gint;

procedure message_dialog_clicked (button    : PGtkButton;
                                  user_data : gpointer);
var
  dialog : PGtkWidget;

begin
  dialog := gtk_message_dialog_new (GTK_WINDOW (dialog_window),
                                   GTK_DIALOG_MODAL or GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_INFO,
                                   GTK_BUTTONS_OK,
                                   'This message box has been popped up the following'#13#10+
                                   'number of times:'#13#10#13#10+
                                   '%d', [dialog_i]);

  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
  inc(dialog_i);
end;

procedure  interactive_dialog_clicked ( button    : PGtkButton;
                                        user_data : gpointer);
var
  dialog,
  hbox,
  stock,
  table,
  local_entry1,
  local_entry2,
  thelabel       : PGtkWidget;
  response       : gint;

begin

  dialog := gtk_dialog_new_with_buttons ('Interactive Dialog',
                                                             GTK_WINDOW (dialog_window),
                                         GTK_DIALOG_MODAL or
                                         GTK_DIALOG_DESTROY_WITH_PARENT,
                                         GTK_STOCK_OK,
                                        [ GTK_RESPONSE_OK,
                                         '_Non-stock Button',
                                          GTK_RESPONSE_CANCEL,
                                        NULL]);

  hbox := gtk_hbox_new (FALSE, 8);
  gtk_container_set_border_width (GTK_CONTAINER (hbox), 8);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)^.vbox), hbox, FALSE, FALSE, 0);

  stock := gtk_image_new_from_stock (GTK_STOCK_DIALOG_QUESTION, GTK_ICON_SIZE_DIALOG);
  gtk_box_pack_start (GTK_BOX (hbox), stock, FALSE, FALSE, 0);

  table := gtk_table_new (2, 2, FALSE);
  gtk_table_set_row_spacings (GTK_TABLE (table), 4);
  gtk_table_set_col_spacings (GTK_TABLE (table), 4);
  gtk_box_pack_start (GTK_BOX (hbox), table, TRUE, TRUE, 0);
  thelabel := gtk_label_new_with_mnemonic ('_Entry 1');
  gtk_table_attach_defaults (GTK_TABLE (table),
                                         thelabel,
                                         0, 1, 0, 1);
  local_entry1 := gtk_entry_new ();
  gtk_entry_set_text (GTK_ENTRY (local_entry1), gtk_entry_get_text (GTK_ENTRY (dialog_entry1)));
  gtk_table_attach_defaults (GTK_TABLE (table), local_entry1, 1, 2, 0, 1);
  gtk_label_set_mnemonic_widget (GTK_LABEL (thelabel), local_entry1);

  thelabel := gtk_label_new_with_mnemonic ('E_ntry 2');
  gtk_table_attach_defaults (GTK_TABLE (table),
                                         thelabel,
                                         0, 1, 1, 2);

  local_entry2 := gtk_entry_new ();
  gtk_entry_set_text (GTK_ENTRY (local_entry2), gtk_entry_get_text (GTK_ENTRY (dialog_entry2)));
  gtk_table_attach_defaults (GTK_TABLE (table), local_entry2, 1, 2, 1, 2);
  gtk_label_set_mnemonic_widget (GTK_LABEL (thelabel), local_entry2);

  gtk_widget_show_all (hbox);
  response := gtk_dialog_run (GTK_DIALOG (dialog));

  if response = GTK_RESPONSE_OK then
  begin
    gtk_entry_set_text (GTK_ENTRY (dialog_entry1), gtk_entry_get_text (GTK_ENTRY (local_entry1)));
    gtk_entry_set_text (GTK_ENTRY (dialog_entry2), gtk_entry_get_text (GTK_ENTRY (local_entry2)));
  end;

  gtk_widget_destroy (dialog);
end;


function do_dialog         : PGtkWidget;

var
  frame,
  vbox,
  vbox2,
  hbox,
  button,
  table,
  thelabel  : PGtkWidget;

begin
  dialog_i := 1;

  if dialog_window = NULL then
  begin
    dialog_window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title (GTK_WINDOW (dialog_window), 'Dialogs');

    g_signal_connect (dialog_window, 'destroy', G_CALLBACK (@gtk_widget_destroyed), @dialog_window);
    gtk_container_set_border_width (GTK_CONTAINER (dialog_window), 8);

    frame := gtk_frame_new ('Dialogs');
    gtk_container_add (GTK_CONTAINER (dialog_window), frame);

    vbox := gtk_vbox_new (FALSE, 8);
    gtk_container_set_border_width (GTK_CONTAINER (vbox), 8);
    gtk_container_add (GTK_CONTAINER (frame), vbox);

    (* Standard message dialog *)
    hbox := gtk_hbox_new (FALSE, 8);
    gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);

    button := gtk_button_new_with_mnemonic ('_Message Dialog');
    g_signal_connect (button, 'clicked',
                        G_CALLBACK (@message_dialog_clicked), @dialog_window);

    gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, FALSE, 0);

    gtk_box_pack_start (GTK_BOX (vbox), gtk_hseparator_new (), FALSE, FALSE, 0);

    (* Interactive dialog*)
    hbox := gtk_hbox_new (FALSE, 8);
    gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);
    vbox2 := gtk_vbox_new (FALSE, 0);

    button := gtk_button_new_with_mnemonic ('_Interactive Dialog');
    g_signal_connect (button, 'clicked',
                        G_CALLBACK (@interactive_dialog_clicked), @dialog_window);
    gtk_box_pack_start (GTK_BOX (hbox), vbox2, FALSE, FALSE, 0);
    gtk_box_pack_start (GTK_BOX (vbox2), button, FALSE, FALSE, 0);

    table := gtk_table_new (2, 2, FALSE);
    gtk_table_set_row_spacings (GTK_TABLE (table), 4);
    gtk_table_set_col_spacings (GTK_TABLE (table), 4);
    gtk_box_pack_start (GTK_BOX (hbox), table, FALSE, FALSE, 0);

    thelabel := gtk_label_new_with_mnemonic ('_Entry 1');
    gtk_table_attach_defaults (GTK_TABLE (table),
                                 thelabel,
                                      0, 1, 0, 1);

    dialog_entry1 := gtk_entry_new ();
    gtk_table_attach_defaults (GTK_TABLE (table), dialog_entry1, 1, 2, 0, 1);
    gtk_label_set_mnemonic_widget (GTK_LABEL (thelabel), dialog_entry1);

    thelabel := gtk_label_new_with_mnemonic ('E_ntry 2');

    gtk_table_attach_defaults (GTK_TABLE (table),
                                 thelabel,
                                 0, 1, 1, 2);

    dialog_entry2 := gtk_entry_new ();
    gtk_table_attach_defaults (GTK_TABLE (table), dialog_entry2, 1, 2, 1, 2);
    gtk_label_set_mnemonic_widget (GTK_LABEL (thelabel), dialog_entry2);
  end;

  if not GTK_WIDGET_VISIBLE (dialog_window) then
    gtk_widget_show_all (dialog_window)

  else begin
    gtk_widget_destroy (dialog_window);
    dialog_window := NULL;
  end;

  do_dialog := dialog_window;
end;


