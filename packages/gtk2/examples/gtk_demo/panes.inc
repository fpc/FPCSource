(* Paned Widgets
 *
 * The GtkHPaned and GtkVPaned Widgets divide their content
 * area into two panes with a divider in between that the
 * user can adjust. A separate child is placed into each
 * pane.
 *
 * There are a number of options that can be set for each pane.
 * This test contains both a horizontal (HPaned) and a vertical
 * (VPaned) widget, and allows you to adjust the options for
 * each side of each widget.
 *)

var
  panes_window  : PGtkWidget;


procedure toggle_resize (widget  : PGtkWidget;
                         child   : PGtkWidget); cdecl;
var
  paned      : PGtkPaned;
  is_child1,
  resize,
  shrink     : gboolean;

begin
  paned := PGtkPaned (child^.parent);
  is_child1 := (child = paned^.child1);

  if is_child1 then
  begin
    resize := child1_resize (paned^) <> 0;
    shrink := child1_shrink (paned^) <> 0;
  end else
  begin
    resize := child2_resize (paned^) <> 0;
    shrink := child2_shrink (paned^) <> 0;
  end;

  gtk_widget_ref (child);
  gtk_container_remove (pGtkContainer (child^.parent), child);

  if is_child1 then
    gtk_paned_pack1 (paned, child, not resize, shrink)
  else
    gtk_paned_pack2 (paned, child, not resize, shrink);

  gtk_widget_unref (child);
end;



procedure toggle_shrink (widget  : PGtkWidget;
                         child   : PGtkWidget); cdecl;
var
  paned      : PGtkPaned;
  is_child1,
  resize,
  shrink     : gboolean;

begin
  paned := PGtkPaned (child^.parent);
  is_child1 := (child = paned^.child1);

  if is_child1 then
  begin
    resize := child1_resize (paned^) <> 0;
    shrink := child1_shrink (paned^) <> 0;
  end else
  begin
    resize := child2_resize (paned^) <> 0;
    shrink := child2_shrink (paned^) <> 0;
  end;

  gtk_widget_ref (child);
  gtk_container_remove (pGtkContainer (child^.parent), child);
  if is_child1 then
    gtk_paned_pack1 (paned, child, resize, not shrink)
  else
    gtk_paned_pack2 (paned, child, resize, not shrink);

  gtk_widget_unref (child);
end;

function create_pane_options (paned       : PGtkPaned;
                              frame_label : pgchar;
                              label1      : pgchar;
                              label2      : pgchar): PGtkWidget; cdecl;
var
  frame,
  table,
  thelabel,
  check_button : PGtkWidget;

begin
  frame := gtk_frame_new (frame_label);
  gtk_container_set_border_width (pGtkContainer(frame), 4);

  table := gtk_table_new (3, 2, TRUE);
  gtk_container_add (pGtkContainer(frame), table);

  thelabel := gtk_label_new (label1);
  gtk_table_attach_defaults (pGtkTable(table), thelabel, 0, 1, 0, 1);

  check_button := gtk_check_button_new_with_mnemonic ('_Resize');
  gtk_table_attach_defaults (pGtkTable(table), check_button,  0, 1, 1, 2);

  g_signal_connect (check_button, 'toggled',
                    TGCallback (@toggle_resize), paned^.child1);

  check_button := gtk_check_button_new_with_mnemonic ('_Shrink');
  gtk_table_attach_defaults (pGtkTable(table), check_button,  0, 1, 2, 3);

  gtk_toggle_button_set_active (pGtkToggleButton(check_button), TRUE);

  g_signal_connect (check_button, 'toggled', TGCallback(@toggle_shrink), paned^.child1);

  thelabel := gtk_label_new (label2);
  gtk_table_attach_defaults (pGtkTable(table), thelabel, 1, 2, 0, 1);

  check_button := gtk_check_button_new_with_mnemonic ('_Resize');
  gtk_table_attach_defaults (pGtkTable(table), check_button, 1, 2, 1, 2);

  gtk_toggle_button_set_active (pGtkToggleButton(check_button), TRUE);

  g_signal_connect (check_button, 'toggled', TGCallback(@toggle_resize), paned^.child2);

  check_button := gtk_check_button_new_with_mnemonic ('_Shrink');
  gtk_table_attach_defaults (pGtkTable(table), check_button, 1, 2, 2, 3);

  gtk_toggle_button_set_active (pGtkToggleButton(check_button), TRUE);
  g_signal_connect (check_button, 'toggled', TGCallback(@toggle_shrink), paned^.child2);

  create_pane_options := frame;
end;

function do_panes          : PGtkWidget;
var
  frame,
  hpaned,
  vpaned,
  button,
  vbox      : PGtkWidget;

begin
  if panes_window = NULL then
  begin
    panes_window := gtk_window_new (GTK_WINDOW_TOPLEVEL);

    g_signal_connect (panes_window, 'destroy',
                        TGCallback(@gtk_widget_destroyed), @panes_window);

    gtk_window_set_title (pGtkWindow(panes_window), 'Panes');
    gtk_container_set_border_width (pGtkContainer(panes_window), 0);

    vbox := gtk_vbox_new (FALSE, 0);
    gtk_container_add (pGtkContainer(panes_window), vbox);

    vpaned := gtk_vpaned_new ();
    gtk_box_pack_start (pGtkBox(vbox), vpaned, TRUE, TRUE, 0);
    gtk_container_set_border_width (pGtkContainer(vpaned), 5);

    hpaned := gtk_hpaned_new ();
    gtk_paned_add1 (pGtkPaned(vpaned), hpaned);

    frame := gtk_frame_new (NULL);
    gtk_frame_set_shadow_type (pGtkFrame(frame), GTK_SHADOW_IN);
    gtk_widget_set_size_request (frame, 60, 60);
    gtk_paned_add1 (pGtkPaned(hpaned), frame);

    button := gtk_button_new_with_mnemonic ('_Hi there');
    gtk_container_add (pGtkContainer(frame), button);

    frame := gtk_frame_new (NULL);
    gtk_frame_set_shadow_type (pGtkFrame(frame), GTK_SHADOW_IN);
    gtk_widget_set_size_request (frame, 80, 60);
    gtk_paned_add2 (pGtkPaned(hpaned), frame);

    frame := gtk_frame_new (NULL);
    gtk_frame_set_shadow_type (pGtkFrame(frame), GTK_SHADOW_IN);
    gtk_widget_set_size_request (frame, 60, 80);
    gtk_paned_add2 (pGtkPaned(vpaned), frame);

    (* Now create toggle buttons to control sizing *)

    gtk_box_pack_start (pGtkBox(vbox),
              create_pane_options (pGtkPaned(hpaned),
                        'Horizontal',
                        'Left',
                        'Right'),
              FALSE, FALSE, 0);

    gtk_box_pack_start (pGtkBox(vbox),
                          create_pane_options (pGtkPaned(vpaned),
                           'Vertical',
                           'Top',
                           'Bottom'),
              FALSE, FALSE, 0);

    gtk_widget_show_all (vbox);
  end;

  if not GTK_WIDGET_VISIBLE (panes_window) then
    gtk_widget_show (panes_window)
  else begin
    gtk_widget_destroy (panes_window);
    panes_window := NULL;
  end;

  do_panes  := panes_window;
end;

