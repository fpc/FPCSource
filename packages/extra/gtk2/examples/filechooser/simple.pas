program simple_filechooser;

(* basing upon helloworld2 example*)
{$mode objfpc} {$H+}

{$IFDEF GTK2_0}{$FATAL this demo needs gtk 2.4}{$ENDIF}
{$IFDEF GTK2_2}{$FATAL this demo needs gtk 2.4}{$ENDIF}

uses
  Glib2, Gdk2, Gtk2;

const
   ACTION_OPEN = 1;
   ACTION_SAVE = 2;

   MAIN_WINDOW_KEY = 'main_window';     { uses with g_object_(get/set)_data as key }


(* File dialog-callback. *)
procedure dialog_callback (widget : PGtkWidget;
                           data   : gpointer); cdecl;
var
  dialog   : PGtkWidget;
  window   : PGtkWindow;
  action   : gint;
  filename : Pgchar;

begin
  { Get a pointer to the main window }
  window := g_object_get_data (G_OBJECT(widget), MAIN_WINDOW_KEY);

  action := gint (data);

  case action of
        ACTION_OPEN:
                begin
                  dialog := gtk_file_chooser_dialog_new ('Open File',
                                                         window,
                                                         GTK_FILE_CHOOSER_ACTION_OPEN,
                                                         GTK_STOCK_OPEN, [GTK_RESPONSE_ACCEPT,
                                                         GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                                         NULL]);

                end; { ACTION_OPEN}
        ACTION_SAVE:
                begin
                  dialog := gtk_file_chooser_dialog_new ('Save File',
                                                         window,
                                                         GTK_FILE_CHOOSER_ACTION_SAVE,
                                                         GTK_STOCK_SAVE, [GTK_RESPONSE_ACCEPT,
                                                         GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                                         NULL]);

                end; { ACTION_SAVE }
        else begin
                 { This should never happen }
                g_print ('Something is wrong here!!!.'#13#10);
                g_print ('No dialog created.'#13#10);
                {writeln crashes on my system running linux --- check why }
                exit;
             end;
  end; { case }


  if gtk_dialog_run (GTK_DIALOG (dialog)) = GTK_RESPONSE_ACCEPT then
  begin
     filename := gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
     g_print ('Filename %s selected.'#13#10, [filename]);
 //    writeln ('File ', filename, ' selected.');
 //  ToDO:
 //  writeln crashes... check why

     g_free (filename);
  end;

  gtk_widget_destroy (dialog);
end;




(* another callback *)
function delete_event (widget: PGtkWidget;
                       event : PGdkEvent;
                       data  : gpointer): gboolean;cdecl;
begin
  gtk_main_quit;
  delete_event := FALSE;
end;


var
  window,
  button,
  box1     : PGtkWidget;      (* GtkWidget is the storage type for widgets *)

begin

    (* This is called in all GTK applications. Arguments are parsed
     * from the command line and are returned to the application. *)

    gtk_init (@argc, @argv);

    (* Create a new window *)
    window := gtk_window_new (GTK_WINDOW_TOPLEVEL);

    (* This is a new call, which just sets the title of our
     * new window to "Hello Buttons!" *)
    gtk_window_set_title (GTK_WINDOW (window), 'GtkFileChooser Demo');

    (* Here we just set a handler for delete_event that immediately
     * exits GTK. *)
    g_signal_connect (G_OBJECT (window), 'delete_event',
                              G_CALLBACK (@delete_event), NULL);

    (* Sets the border width of the window. *)
    gtk_container_set_border_width (GTK_CONTAINER (window), 10);

    (* We create a box to pack widgets into.  This is described in detail
     * in the "packing" section. The box is not really visible, it
     * is just used as a tool to arrange widgets. *)
    box1 := gtk_hbox_new (FALSE, 0);

    (* Put the box into the main window. *)
    gtk_container_add (GTK_CONTAINER (window), box1);


    button := gtk_button_new_with_label ('Open');

    (* Now when the button is clicked, we call the "callback" function
     * with a pointer to the main window as its argument *)
    g_object_set_data (G_OBJECT(button), MAIN_WINDOW_KEY, window);

    g_signal_connect (G_OBJECT (button), 'clicked',
                              G_CALLBACK (@dialog_callback), pointer(ACTION_OPEN));

    (* Instead of gtk_container_add, we pack this button into the invisible
     * box, which has been packed into the window. *)
    gtk_box_pack_start (GTK_BOX(box1), button, TRUE, TRUE, 0);

    (* Always remember this step, this tells GTK that our preparation for
     * this button is complete, and it can now be displayed. *)
    gtk_widget_show (button);

    (* Do these same steps again to create a second button *)
    button := gtk_button_new_with_label ('Save');

    g_object_set_data (G_OBJECT(button), MAIN_WINDOW_KEY, window);

    g_signal_connect (G_OBJECT (button), 'clicked',
                              G_CALLBACK (@dialog_callback), pointer(ACTION_SAVE));

    gtk_box_pack_start(GTK_BOX (box1), button, TRUE, TRUE, 0);

    (* The order in which we show the buttons is not really important, but I
     * recommend showing the window last, so it all pops up at once. *)
    gtk_widget_show (button);

    gtk_widget_show (box1);

    gtk_widget_show (window);


    (* Rest in gtk_main and wait for the fun to begin! *)
    gtk_main ();
end.
