{

  Converted from C to Pascal by Javier Ros <jros@unavarra.es>
}
program filesel;

uses
 glib,gdk,gtk;

(* Get the selected filename and print it to the console *)
procedure file_ok_sel( w:PGtkWidget;
                  fs:PGtkFileSelection );cdecl;
begin
    writeln ( gtk_file_selection_get_filename (GTK_FILE_SELECTION (fs)));
end;

procedure destroy( widget:PGtkWidget;
              data: gpointer);cdecl;
begin
    gtk_main_quit ();
end;


var
 filew:PGtkWidget ;


begin

    gtk_init (@argc, @argv);

    (* Create a new file selection widget *)
    filew := gtk_file_selection_new ('File selection');

    gtk_signal_connect (GTK_OBJECT (filew), 'destroy',
                        GTK_SIGNAL_FUNC (@destroy), @filew);
    (* Connect the ok_button to file_ok_sel function *)
    gtk_signal_connect (GTK_OBJECT (GTK_FILE_SELECTION (filew)^.ok_button),
                        'clicked', GTK_SIGNAL_FUNC (@file_ok_sel), filew );

    (* Connect the cancel_button to destroy the widget *)
    gtk_signal_connect_object (GTK_OBJECT (GTK_FILE_SELECTION
                                            (filew)^.cancel_button),
                               'clicked', GTK_SIGNAL_FUNC (@gtk_widget_destroy),
                               GTK_OBJECT (filew));

    (* Lets set the filename, as if this were a save dialog, and we are giving
     a default filename *)
    gtk_file_selection_set_filename (GTK_FILE_SELECTION(filew),
                                     'filesel.pp');

    gtk_widget_show(filew);
    gtk_main ();
end.
