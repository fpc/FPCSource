(* Color Selector
 *
 * GtkColorSelection lets the user choose a color. GtkColorSelectionDialog is
 * a prebuilt dialog containing a GtkColorSelection.
 *
 *)

var
  colorsel_window,
  colorsel_da,
  colorsel_frame    : PGtkWidget;
  colorsel_color    : TGdkColor;


procedure change_color_callback (button : PGtkWidget;
                                 data   : gpointer); cdecl;
var
  dialog    : PGtkWidget;
  colorsel  : PGtkColorSelection;
  response  : gint;

begin
  dialog := gtk_color_selection_dialog_new ('Changing color');

  gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (colorsel_window));

  colorsel := PGtkColorSelection ( PGtkColorSelectionDialog(dialog)^.colorsel );

  gtk_color_selection_set_previous_color (colorsel, @colorsel_color);
  gtk_color_selection_set_current_color (colorsel, @colorsel_color);
  gtk_color_selection_set_has_palette (colorsel, TRUE);

  response := gtk_dialog_run (GTK_DIALOG (dialog));

  if response = GTK_RESPONSE_OK then
  begin
    gtk_color_selection_get_current_color (colorsel, @colorsel_color);

    gtk_widget_modify_bg (colorsel_da, GTK_STATE_NORMAL, @colorsel_color);
  end;

  gtk_widget_destroy (dialog);
end;


function do_colorsel       : PGtkWidget;
var
  vbox,
  button,
  alignment  : PGtkWidget;


 begin
  if colorsel_window = NULL then
  begin
    colorsel_color.red   := $0;
    colorsel_color.blue  := $ffff;
    colorsel_color.green := $0;

    colorsel_window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title (GTK_WINDOW (colorsel_window), 'Color Selection');

    g_signal_connect (colorsel_window, 'destroy',
                          G_CALLBACK (@gtk_widget_destroyed), @colorsel_window);

    gtk_container_set_border_width (GTK_CONTAINER (colorsel_window), 8);

    vbox := gtk_vbox_new (FALSE, 8);
    gtk_container_set_border_width (GTK_CONTAINER (vbox), 8);
    gtk_container_add (GTK_CONTAINER (colorsel_window), vbox);

    (*
     * Create the color swatch area
     *)


    colorsel_frame := gtk_frame_new (NULL);
    gtk_frame_set_shadow_type (GTK_FRAME (colorsel_frame), GTK_SHADOW_IN);
    gtk_box_pack_start (GTK_BOX (vbox), colorsel_frame, TRUE, TRUE, 0);

    colorsel_da := gtk_drawing_area_new ();
    (* set a minimum size *)
    gtk_widget_set_size_request (colorsel_da, 200, 200);
    (* set the color *)
    gtk_widget_modify_bg (colorsel_da, GTK_STATE_NORMAL, @colorsel_color);

    gtk_container_add (GTK_CONTAINER (colorsel_frame), colorsel_da);

    alignment := gtk_alignment_new (1.0, 0.5, 0.0, 0.0);

    button := gtk_button_new_with_mnemonic ('_Change the above color');
    gtk_container_add (GTK_CONTAINER (alignment), button);

    gtk_box_pack_start (GTK_BOX (vbox), alignment, FALSE, FALSE, 0);

    g_signal_connect (button, 'clicked',
                        G_CALLBACK (@change_color_callback), NULL);
  end;

  if not GTK_WIDGET_VISIBLE (colorsel_window) then
    gtk_widget_show_all (colorsel_window)
  else begin
    gtk_widget_destroy (colorsel_window);
    colorsel_window := NULL;
  end;

  do_colorsel := colorsel_window;
end;
