(* Button Boxes
 *
 * The Button Box widgets are used to arrange buttons with padding.
 *)

function create_bbox (horizontal : gboolean;
                      title      : pgchar;
                      spacing    : gint;
                      layout     : TGtkButtonBoxStyle): PGtkWidget;
var
  frame,
  bbox,
  button  : PGtkWidget;

begin
  frame := gtk_frame_new (title);

  if horizontal then
    bbox := gtk_hbutton_box_new ()
  else
    bbox := gtk_vbutton_box_new ();

  gtk_container_set_border_width (GTK_CONTAINER (bbox), 5);
  gtk_container_add (GTK_CONTAINER (frame), bbox);

  gtk_button_box_set_layout (GTK_BUTTON_BOX (bbox), layout);
  gtk_box_set_spacing (GTK_BOX (bbox), spacing);

  button := gtk_button_new_from_stock (GTK_STOCK_OK);
  gtk_container_add (GTK_CONTAINER (bbox), button);

  button := gtk_button_new_from_stock (GTK_STOCK_CANCEL);
  gtk_container_add (GTK_CONTAINER (bbox), button);

  button := gtk_button_new_from_stock (GTK_STOCK_HELP);
  gtk_container_add (GTK_CONTAINER (bbox), button);

  create_bbox := frame;
end;

var bbox_window  : PGtkWidget;


function do_button_box     : PGtkWidget;
var
  main_vbox,
  vbox,
  hbox,
  frame_horz,
  frame_vert    : PGtkWidget;

begin

  if bbox_window = NULL then
  begin
    bbox_window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title (GTK_WINDOW (bbox_window), 'Button Boxes');

    g_signal_connect (bbox_window, 'destroy',
                      G_CALLBACK (@gtk_widget_destroyed),
                      @bbox_window);

    gtk_container_set_border_width (GTK_CONTAINER (bbox_window), 10);

    main_vbox := gtk_vbox_new (FALSE, 0);
    gtk_container_add (GTK_CONTAINER (bbox_window), main_vbox);

    frame_horz := gtk_frame_new ('Horizontal Button Boxes');
    gtk_box_pack_start (GTK_BOX (main_vbox), frame_horz, TRUE, TRUE, 10);

    vbox := gtk_vbox_new (FALSE, 0);
    gtk_container_set_border_width (GTK_CONTAINER (vbox), 10);
    gtk_container_add (GTK_CONTAINER (frame_horz), vbox);

    gtk_box_pack_start (GTK_BOX (vbox),
                        create_bbox (TRUE, 'Spread', 40, GTK_BUTTONBOX_SPREAD),
                        TRUE, TRUE, 0);

    gtk_box_pack_start (GTK_BOX (vbox),
                        create_bbox (TRUE, 'Edge', 40, GTK_BUTTONBOX_EDGE),
                        TRUE, TRUE, 5);

    gtk_box_pack_start (GTK_BOX (vbox),
                        create_bbox (TRUE, 'Start', 40, GTK_BUTTONBOX_START),
                        TRUE, TRUE, 5);

    gtk_box_pack_start (GTK_BOX (vbox),
                        create_bbox (TRUE, 'End', 40, GTK_BUTTONBOX_END),
                        TRUE, TRUE, 5);

    frame_vert := gtk_frame_new ('Vertical Button Boxes');
    gtk_box_pack_start (GTK_BOX (main_vbox), frame_vert, TRUE, TRUE, 10);

    hbox := gtk_hbox_new (FALSE, 0);
    gtk_container_set_border_width (GTK_CONTAINER (hbox), 10);
    gtk_container_add (GTK_CONTAINER (frame_vert), hbox);

    gtk_box_pack_start (GTK_BOX (hbox),
                        create_bbox (FALSE, 'Spread', 30, GTK_BUTTONBOX_SPREAD),
                        TRUE, TRUE, 0);

    gtk_box_pack_start (GTK_BOX (hbox),
                        create_bbox (FALSE, 'Edge', 30, GTK_BUTTONBOX_EDGE),
                        TRUE, TRUE, 5);

    gtk_box_pack_start (GTK_BOX (hbox),
                        create_bbox (FALSE, 'Start', 30, GTK_BUTTONBOX_START),
                        TRUE, TRUE, 5);

    gtk_box_pack_start (GTK_BOX (hbox),
                        create_bbox (FALSE, 'End', 30, GTK_BUTTONBOX_END),
                        TRUE, TRUE, 5);
  end;

  if not GTK_WIDGET_VISIBLE (bbox_window) then
      gtk_widget_show_all (bbox_window)
  else
  begin
      gtk_widget_destroy (bbox_window);
      bbox_window := NULL;
  end;

  do_button_box := bbox_window;
end;
