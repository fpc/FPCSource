{

  This file extracted from the GTK 1.2 tutorial.
  Section 8.5

  Converted from C to Pascal by Thomas E. Payne
}
program Tut8_5;

{$mode objfpc}

 uses
  glib,gdk,gtk,sysutils;

    //* example-start rangewidgets rangewidgets.c *//
 var
   hscale, vscale : pGtkWidget;

   procedure cb_pos_menu_select( item : pGtkWidget;
                             pos : TGtkPositionType );cdecl;
   begin
        //* Set the value position on both scale widgets *//
        gtk_scale_set_value_pos (GTK_SCALE (hscale), pos);
        gtk_scale_set_value_pos (GTK_SCALE (vscale), pos);
    end;

   procedure cb_update_menu_select( item : pGtkWidget;
                                policy : TGtkUpdateType);cdecl;
   begin
        //* Set the update policy for both scale widgets *//
        gtk_range_set_update_policy (GTK_RANGE (hscale), policy);
        gtk_range_set_update_policy (GTK_RANGE (vscale), policy);
   end;

   procedure cb_digits_scale( adj :pGtkAdjustment );cdecl;
   begin
        //* Set the number of decimal places to which adj->value is rounded *//
        gtk_scale_set_digits (GTK_SCALE (hscale), round(adj^.value));
        gtk_scale_set_digits (GTK_SCALE (vscale), round(adj^.value));
   end;

   procedure cb_page_size( get : pGtkAdjustment; set_ : pGtkAdjustment );cdecl;
   begin
        //* Set the page size and page increment size of the sample
        //* adjustment to the value specified by the 'Page Size' scale *//
        set_^.page_size := get^.value;
        set_^.page_increment := get^.value;
        //* Now emit the 'changed' signal to reconfigure all the widgets that
        //* are attached to this adjustment *//
        gtk_signal_emit_by_name (GTK_OBJECT (set_), 'changed');
   end;

    procedure cb_draw_value( button : pGtkToggleButton );cdecl;
    begin
        //* Turn the value display on the scale widgets off or on depending
        //*  on the state of the checkbutton *//
        gtk_scale_set_draw_value (GTK_SCALE (hscale), active(button^)<>0);
        gtk_scale_set_draw_value (GTK_SCALE (vscale), active(button^)<>0);
    end;

    //* Convenience functions *//

    function make_menu_item( name : pgchar;
                             callback : TGtkSignalFunc;
                             data :  gint ) : pGtkWidget;
    var
      item : pGtkWidget ;
    begin

        item := gtk_menu_item_new_with_label (name);
        gtk_signal_connect (GTK_OBJECT (item), 'activate',
                            callback, gpointer(data));
        gtk_widget_show (item);

        make_menu_item := item;
    end;

    function GINT_TO_POINTER( t : gint): gpointer;
    var
      temp : gint;
    begin
      temp := t;
      GINT_TO_POINTER := @temp;
    end;

    procedure scale_set_default_values( scale : pGtkScale);
    begin
        gtk_range_set_update_policy (GTK_RANGE (scale),
                                     GTK_UPDATE_CONTINUOUS);
        gtk_scale_set_digits (scale, 1);
        gtk_scale_set_value_pos (scale, GTK_POS_TOP);
        gtk_scale_set_draw_value (scale, TRUE);
    end;

    //* makes the sample window *//

    procedure create_range_controls;
    var
        window,
        box1, box2, box3,
        button,
        scrollbar,
        separator,
        opt, menu, item,
        label_,
        scale  : pGtkWidget;
        adj1, adj2 : pGtkObject;
    begin

        //* Standard window-creating stuff *//
        window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
        gtk_signal_connect (GTK_OBJECT (window), 'destroy',
                            GTK_SIGNAL_FUNC(@gtk_main_quit),
                            Nil);
        gtk_window_set_title (GTK_WINDOW (window), 'range controls');

        box1 := gtk_vbox_new (FALSE, 0);
        gtk_container_add (GTK_CONTAINER (window), box1);
        gtk_widget_show (box1);

        box2 := gtk_hbox_new (FALSE, 10);
        gtk_container_set_border_width (GTK_CONTAINER (box2), 10);
        gtk_box_pack_start (GTK_BOX (box1), box2, TRUE, TRUE, 0);
        gtk_widget_show (box2);

        //* calue, lower, upper, step_increment, page_increment, page_size *//
        //* Note that the page_size value only makes a difference for
        //* scrollbar widgets, and the highest value you'll get is actually
        //* (upper - page_size). *//
        adj1 := gtk_adjustment_new (0.0, 0.0, 101.0, 0.1, 1.0, 1.0);

        vscale := gtk_vscale_new (GTK_ADJUSTMENT (adj1));
        scale_set_default_values (GTK_SCALE (vscale));
        gtk_box_pack_start (GTK_BOX (box2), vscale, TRUE, TRUE, 0);
        gtk_widget_show (vscale);

        box3 := gtk_vbox_new (FALSE, 10);
        gtk_box_pack_start (GTK_BOX (box2), box3, TRUE, TRUE, 0);
        gtk_widget_show (box3);

        //* Reuse the same adjustment *//
        hscale := gtk_hscale_new (GTK_ADJUSTMENT (adj1));
        gtk_widget_set_usize (GTK_WIDGET (hscale), 200, 30);
        scale_set_default_values (GTK_SCALE (hscale));
        gtk_box_pack_start (GTK_BOX (box3), hscale, TRUE, TRUE, 0);
        gtk_widget_show (hscale);

        //* Reuse the same adjustment again *//
        scrollbar := gtk_hscrollbar_new (GTK_ADJUSTMENT (adj1));
        //* Notice how this causes the scales to always be updated
        // * continuously when the scrollbar is moved *//
        gtk_range_set_update_policy (GTK_RANGE (scrollbar),
                                     GTK_UPDATE_CONTINUOUS);
        gtk_box_pack_start (GTK_BOX (box3), scrollbar, TRUE, TRUE, 0);
        gtk_widget_show (scrollbar);

        box2 := gtk_hbox_new (FALSE, 10);
        gtk_container_set_border_width (GTK_CONTAINER (box2), 10);
        gtk_box_pack_start (GTK_BOX (box1), box2, TRUE, TRUE, 0);
        gtk_widget_show (box2);

        //* A checkbutton to control whether the value is displayed or not *//
        button := gtk_check_button_new_with_label('Display value on scale widgets');
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);
        gtk_signal_connect (GTK_OBJECT (button), 'toggled',
                            GTK_SIGNAL_FUNC(@cb_draw_value), Nil);
        gtk_box_pack_start (GTK_BOX (box2), button, TRUE, TRUE, 0);
        gtk_widget_show (button);

        box2 := gtk_hbox_new (FALSE, 10);
        gtk_container_set_border_width (GTK_CONTAINER (box2), 10);

        //* An option menu to change the position of the value *//
        label_ := gtk_label_new ('Scale Value Position:');
        gtk_box_pack_start (GTK_BOX (box2), label_, FALSE, FALSE, 0);
        gtk_widget_show (label_);

        opt := gtk_option_menu_new();
        menu := gtk_menu_new();

        item := make_menu_item ('Top',
                               GTK_SIGNAL_FUNC(@cb_pos_menu_select),
                               GTK_POS_TOP);
        gtk_menu_append (GTK_MENU (menu), item);

        item := make_menu_item ('Bottom', GTK_SIGNAL_FUNC (@cb_pos_menu_select),
                               GTK_POS_BOTTOM);
        gtk_menu_append (GTK_MENU (menu), item);

        item := make_menu_item ('Left', GTK_SIGNAL_FUNC (@cb_pos_menu_select),
                               GTK_POS_LEFT);
        gtk_menu_append (GTK_MENU (menu), item);

        item := make_menu_item ('Right', GTK_SIGNAL_FUNC (@cb_pos_menu_select),
                                GTK_POS_RIGHT);
        gtk_menu_append (GTK_MENU (menu), item);

        gtk_option_menu_set_menu (GTK_OPTION_MENU (opt), menu);
        gtk_box_pack_start (GTK_BOX (box2), opt, TRUE, TRUE, 0);
        gtk_widget_show (opt);

        gtk_box_pack_start (GTK_BOX (box1), box2, TRUE, TRUE, 0);
        gtk_widget_show (box2);

        box2 := gtk_hbox_new (FALSE, 10);
        gtk_container_set_border_width (GTK_CONTAINER (box2), 10);

        //* Yet another option menu, this time for the update policy of the
        // * scale widgets *//
        label_ := gtk_label_new ('Scale Update Policy:');
        gtk_box_pack_start (GTK_BOX (box2), label_, FALSE, FALSE, 0);
        gtk_widget_show (label_);

        opt := gtk_option_menu_new();
        menu := gtk_menu_new();

        item := make_menu_item ('Continuous',
                               GTK_SIGNAL_FUNC (@cb_update_menu_select),
                               GTK_UPDATE_CONTINUOUS);
        gtk_menu_append (GTK_MENU (menu), item);

        item := make_menu_item ('Discontinuous',
                                GTK_SIGNAL_FUNC (@cb_update_menu_select),
                                GTK_UPDATE_DISCONTINUOUS);
        gtk_menu_append (GTK_MENU (menu), item);

        item := make_menu_item ('Delayed',
                               GTK_SIGNAL_FUNC (@cb_update_menu_select),
                               GTK_UPDATE_DELAYED);
        gtk_menu_append (GTK_MENU (menu), item);

        gtk_option_menu_set_menu (GTK_OPTION_MENU (opt), menu);
        gtk_box_pack_start (GTK_BOX (box2), opt, TRUE, TRUE, 0);
        gtk_widget_show (opt);

        gtk_box_pack_start (GTK_BOX (box1), box2, TRUE, TRUE, 0);
        gtk_widget_show (box2);

        box2 := gtk_hbox_new (FALSE, 10);
        gtk_container_set_border_width (GTK_CONTAINER (box2), 10);

        //* A GtkHScale widget for adjusting the number of digits on the
        //* sample scales. *//
        label_ := gtk_label_new ('Scale Digits:');
        gtk_box_pack_start (GTK_BOX (box2), label_, FALSE, FALSE, 0);
        gtk_widget_show (label_);

        adj2 := gtk_adjustment_new (1.0, 0.0, 5.0, 1.0, 1.0, 0.0);
        gtk_signal_connect (GTK_OBJECT (adj2), 'value_changed',
                            GTK_SIGNAL_FUNC (@cb_digits_scale), Nil);
        scale := gtk_hscale_new (GTK_ADJUSTMENT (adj2));
        gtk_scale_set_digits (GTK_SCALE (scale), 0);
        gtk_box_pack_start (GTK_BOX (box2), scale, TRUE, TRUE, 0);
        gtk_widget_show (scale);

        gtk_box_pack_start (GTK_BOX (box1), box2, TRUE, TRUE, 0);
        gtk_widget_show (box2);

        box2 := gtk_hbox_new (FALSE, 10);
        gtk_container_set_border_width (GTK_CONTAINER (box2), 10);

        //* And, one last GtkHScale widget for adjusting the page size of the
        // * scrollbar. *//
        label_ := gtk_label_new ('Scrollbar Page Size:');
        gtk_box_pack_start (GTK_BOX (box2), label_, FALSE, FALSE, 0);
        gtk_widget_show (label_);

        adj2 := gtk_adjustment_new (1.0, 1.0, 101.0, 1.0, 1.0, 0.0);
        gtk_signal_connect (GTK_OBJECT (adj2), 'value_changed',
                            GTK_SIGNAL_FUNC (@cb_page_size), adj1);
        scale := gtk_hscale_new (GTK_ADJUSTMENT (adj2));
        gtk_scale_set_digits (GTK_SCALE (scale), 0);
        gtk_box_pack_start (GTK_BOX (box2), scale, TRUE, TRUE, 0);
        gtk_widget_show (scale);

        gtk_box_pack_start (GTK_BOX (box1), box2, TRUE, TRUE, 0);
        gtk_widget_show (box2);

        separator := gtk_hseparator_new ();
        gtk_box_pack_start (GTK_BOX (box1), separator, FALSE, TRUE, 0);
        gtk_widget_show (separator);

        box2 := gtk_vbox_new (FALSE, 10);
        gtk_container_set_border_width (GTK_CONTAINER (box2), 10);
        gtk_box_pack_start (GTK_BOX (box1), box2, FALSE, TRUE, 0);
        gtk_widget_show (box2);

        button := gtk_button_new_with_label ('Quit');
        gtk_signal_connect_object (GTK_OBJECT (button), 'clicked',
                                   GTK_SIGNAL_FUNC(@gtk_main_quit),
                                   Nil);
        gtk_box_pack_start (GTK_BOX (box2), button, TRUE, TRUE, 0);
        GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
        gtk_widget_grab_default (button);
        gtk_widget_show (button);

        gtk_widget_show (window);
    end;

begin
  gtk_init(@argc, @argv);

  create_range_controls();

  gtk_main();
end.
