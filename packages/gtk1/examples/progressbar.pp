{

  This is the pascal translation of the progressbar sample from the
  GTK-1.2 library.

  Converted from C to Pascal by Stefan Hille
  <stoppok@osibisa.ms.sub.org>

}


(* example-start progressbar progressbar.c *)
program progressbar;

uses glib, gtk, strings;

type
    ProgressData = record
        window : PGtkWidget;
        pbar   : PGtkWidget;
        timer  : integer;
    end;
    PProgressData = ^ProgressData;

(* Update the value of the progress bar so that we get
 * some movement *)
function progress_timeout( data : gpointer ) : gint; cdecl;
var
    new_val : gfloat;
    adj     : PGtkAdjustment;
begin

    (* Calculate the value of the progress bar using the
     * value range set in the adjustment object *)

    new_val := gtk_progress_get_value( GTK_PROGRESS(data) ) + 1;

    adj := GTK_PROGRESS (data)^.adjustment;
    if (new_val > adj^.upper) then
      new_val := adj^.lower;

    (* Set the new value *)
    gtk_progress_set_value (GTK_PROGRESS (data), new_val);

    (* As this is a timeout function, return TRUE so that it
     * continues to get called *)
    progress_timeout := 1;
end;


(* Callback that toggles the text display within the progress
 * bar trough *)
procedure toggle_show_text( widget : PGtkWidget;
pdata : PProgressData);cdecl;
begin
    gtk_progress_set_show_text (GTK_PROGRESS (pdata^.pbar),
                                active(PGtkToggleButton(widget)^));
end;

(* Callback that toggles the activity mode of the progress
 * bar *)
procedure toggle_activity_mode( widget : PGtkWidget;
                       pdata : PProgressData); cdecl;
begin
    gtk_progress_set_activity_mode (GTK_PROGRESS (pdata^.pbar),
                                active(PGtkToggleButton(widget)^));
end;

(* Callback that toggles the continuous mode of the progress
 * bar *)
procedure set_continuous_mode( widget : PGtkWidget;
                       pdata : PProgressData); cdecl;
begin
    gtk_progress_bar_set_bar_style (GTK_PROGRESS_BAR (pdata^.pbar),
                                    GTK_PROGRESS_CONTINUOUS);
end;

(* Callback that toggles the discrete mode of the progress
 * bar *)
procedure set_discrete_mode( widget : PGtkWidget;
                       pdata : PProgressData); cdecl;
begin
    gtk_progress_bar_set_bar_style (GTK_PROGRESS_BAR (pdata^.pbar),
                                    GTK_PROGRESS_DISCRETE);
end;

(* Clean up allocated memory and remove the timer *)
procedure destroy_progress( widget : PGtkWidget;
                       pdata : PProgressData); cdecl;
begin
    gtk_timeout_remove (pdata^.timer);
    pdata^.timer := 0;
    pdata^.window := NULL;
    g_free(pdata);
    gtk_main_quit();
end;

var
    pdata : PProgressData;
    align : PGtkWidget;
    separator : PGtkWidget;
    table : PGtkWidget;
    adj   : PGtkAdjustment;
    button: PGtkWidget;
    check : PGtkWidget;
    vbox  : PGtkWidget;

begin
    gtk_init (@argc, @argv);

    (* Allocate memory for the data that is passwd to the callbacks *)
    pdata := g_malloc( sizeof(ProgressData) );

    pdata^.window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_window_set_policy (GTK_WINDOW (pdata^.window), 0{FALSE}, 1{TRUE}, 1{TRUE});

    gtk_signal_connect (GTK_OBJECT (pdata^.window), 'destroy',
                        GTK_SIGNAL_FUNC (@destroy_progress),
                        pdata);

    gtk_window_set_title (GTK_WINDOW (pdata^.window),'GtkProgressBar' );
    gtk_container_set_border_width (GTK_CONTAINER (pdata^.window), 0);

    vbox := gtk_vbox_new (FALSE, 5);
    gtk_container_set_border_width (GTK_CONTAINER (vbox), 10);
    gtk_container_add (GTK_CONTAINER (pdata^.window), vbox);

    (* Create a centering alignment object *)
    align := gtk_alignment_new (0.5, 0.5, 0, 0);
    gtk_box_pack_start (GTK_BOX (vbox), align, FALSE, FALSE, 5);

    (* Create a GtkAdjusment object to hold the range of the
     * progress bar *)
    adj := PGtkAdjustment( gtk_adjustment_new (0, 1, 150, 0, 0, 0));

    (* Create the GtkProgressBar using the adjustment *)
    pdata^.pbar := gtk_progress_bar_new_with_adjustment (adj);

    (* Set the format of the string that can be displayed in the
     * trough of the progress bar:
     * %p - percentage
     * %v - value
     * %l - lower range value
     * %u - upper range value *)
    gtk_progress_set_format_string (GTK_PROGRESS (pdata^.pbar),
                                     '%v from [%l-%u] (=%p%%)');
    gtk_container_add (GTK_CONTAINER (align), pdata^.pbar);

    (* Add a timer callback to update the value of the progress bar *)
    pdata^.timer := gtk_timeout_add (100, TGtkFunction(@progress_timeout), pdata^.pbar);

    separator := gtk_hseparator_new ();
    gtk_box_pack_start (GTK_BOX (vbox), separator, FALSE, FALSE, 0);

    (* rows, columns, homogeneous *)
    table := gtk_table_new (2, 3, FALSE);
    gtk_box_pack_start (GTK_BOX (vbox), table, FALSE, TRUE, 0);

    (* Add a check button to select displaying of the trough text *)
    check := gtk_check_button_new_with_label ('Show text');
    gtk_table_attach (GTK_TABLE (table), check, 0, 1, 0, 1,
                      GTK_EXPAND or GTK_FILL, GTK_EXPAND or GTK_FILL,
                      5, 5);
    gtk_signal_connect (GTK_OBJECT (check), 'clicked',
                        GTK_SIGNAL_FUNC (@toggle_show_text),
                        pdata);

    (* Add a check button to toggle activity mode *)
    check := gtk_check_button_new_with_label ( 'Activity mode');
    gtk_table_attach (GTK_TABLE (table), check, 0, 1, 1, 2,
                      GTK_EXPAND or GTK_FILL, GTK_EXPAND or GTK_FILL,
                      5, 5);
    gtk_signal_connect (GTK_OBJECT (check), 'clicked',
                        GTK_SIGNAL_FUNC (@toggle_activity_mode),
                        pdata);

    separator := gtk_vseparator_new ();
    gtk_table_attach (GTK_TABLE (table), separator, 1, 2, 0, 2,
                      GTK_EXPAND or GTK_FILL, GTK_EXPAND or GTK_FILL,
                      5, 5);

    (* Add a radio button to select continuous display mode *)
    button := gtk_radio_button_new_with_label (NULL,  'Continuous');
    gtk_table_attach (GTK_TABLE (table), button, 2, 3, 0, 1,
                      GTK_EXPAND or GTK_FILL, GTK_EXPAND or GTK_FILL,
                      5, 5);
    gtk_signal_connect (GTK_OBJECT (button), 'clicked',
                        GTK_SIGNAL_FUNC (@set_continuous_mode),
                        pdata);

    (* Add a radio button to select discrete display mode *)
    button := gtk_radio_button_new_with_label(
               gtk_radio_button_group (GTK_RADIO_BUTTON (button)),
               'Discrete');
    gtk_table_attach (GTK_TABLE (table), button, 2, 3, 1, 2,
                      GTK_EXPAND or GTK_FILL, GTK_EXPAND or GTK_FILL,
                      5, 5);
    gtk_signal_connect (GTK_OBJECT (button), 'clicked',
                        GTK_SIGNAL_FUNC (@set_discrete_mode),
                        pdata);

    separator := gtk_hseparator_new ();
    gtk_box_pack_start (GTK_BOX (vbox), separator, FALSE, FALSE, 0);

    (* Add a button to exit the program *)
    button := gtk_button_new_with_label ('close');
    gtk_signal_connect_object (GTK_OBJECT (button), 'clicked',
                               Gtk_Signal_Func (@gtk_widget_destroy),
                               GTK_OBJECT (pdata^.window));
    gtk_box_pack_start (GTK_BOX (vbox), button, FALSE, FALSE, 0);

    (* This makes it so the button is the default. *)
    GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);

    (* This grabs this button to be the default button. Simply hitting
     * the 'Enter' key will cause this button to activate. *)
    gtk_widget_grab_default (button);

    gtk_widget_show_all (pdata^.window);

    gtk_main ();

end.
