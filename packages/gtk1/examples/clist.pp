{

  This file extracted from the Gtk tutorial.
  clist.c

  Converted from C to Pascal by Frank Loemker
  <floemker@techfak.uni-bielefeld.de>
}
program clist;
uses
  glib,Gdk,Gtk;

{ User clicked the 'Add List' button. }
procedure button_add_clicked (data: PGtkCList ); cdecl;
{ Something silly to add to the list. 4 rows of 2 columns each }
const drink : array[0..3,0..1] of Pgchar =
  (('Milk', '3 Oz'),
   ('Water', '6 l'),
   ('Carrots', '2'),
   ('Snakes', '55'));
var indx : integer ;
begin
  { Here we do the actual adding of the text. It's done once for
    each row. }
  for indx:=0 to 3 do
    gtk_clist_append (data, @drink[indx]);
end;

{ User clicked the 'Clear List' button. }
procedure button_clear_clicked (data : PGtkCList ); cdecl;
begin
  { Clear the list using gtk_clist_clear. This is much faster than
    calling gtk_clist_remove once for each row. }
  gtk_clist_clear (data);
end;

{ The user clicked the 'Hide/Show titles' button. }
procedure button_hide_show_clicked (data : PGtkCList ); cdecl;
const flag:integer = 0;
begin
  { Just a flag to remember the status. 0 = currently visible }

  if flag = 0 then begin
        { Hide the titles and set the flag to 1 }
        gtk_clist_column_titles_hide (data);
        inc (flag);
  end else begin
    { Show the titles and reset flag to 0 }
    gtk_clist_column_titles_show (data);
    dec (flag);
  end;
end;

{ If we come here, then the user has selected a row in the list. }
procedure selection_made (thelist : PGtkCLIST ; row, column: gint;
                          event :  PGdkEventButton ; data : gpointer); cdecl;
var text : Pgchar;
begin
  { Get the text that is stored in the selected row and column
    which was clicked in. We will receive it as a pointer in the
    argument text. }
  gtk_clist_get_text(thelist, row, column, @text);

  { Just prints some information about the selected row }
  writeln ('You selected row ',row,
           '. More specifically you clicked in column ',column,
           ', and the text in this cell is ',text,#10);
end;

const
  titles: array[0..1] of Pgchar = ('Ingredients','Amount');
var
  window,vbox,hbox,scroll, thelist,
  button_add, button_clear,button_hide_show : PGtkWidget;
begin
  gtk_init (@argc, @argv);
  gtk_rc_init;

  window := gtk_window_new(gtk_WINDOW_TOPLEVEL);
  gtk_widget_set_usize(PGtkWIDGET(window), 300, 150);

  gtk_window_set_title(PGtkWINDOW(window), 'GtkCList Example');
  gtk_signal_connect(PGtkOBJECT(window),'destroy',
                     tGtksignalfunc(@gtk_main_quit),
                     NIL);

  vbox := gtk_vbox_new(false, 5);
  gtk_container_set_border_width(PGtkCONTAINER(vbox), 5);
  gtk_container_add(PGtkCONTAINER(window), vbox);

  { Create the ScrolledWindow to pack the CList in. }
  scroll := gtk_scrolled_window_new (NULL,NULL);
  gtk_scrolled_window_set_policy (PGtkSCROLLEDWINDOW(scroll),
                                  GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
  gtk_box_pack_start(PGtkBOX(vbox), scroll, true, true, 0);

  { Create the GtkCList. For this example we use 2 columns }
  thelist := gtk_clist_new_with_titles (2,titles);
  gtk_container_add (PGtkContainer(scroll),thelist);

  { When a selection is made, we want to know about it. The callback
    used is selection_made, and it's code can be found above }
  gtk_signal_connect(PGtkOBJECT(thelist), 'select_row',
                     tGtksignalfunc(@selection_made),
                     NIL);

  { It isn't necessary to shadow the border, but it looks nice :) }
  gtk_clist_set_shadow_type(PGtkCLIST(thelist), gtk_SHADOW_OUT);

  { What however is important, is that we set the column widths as
    they will never be right otherwise. Note that the columns are
    numbered from 0 and up (to 1 in this case). }
  gtk_clist_set_column_width (PGtkCLIST(thelist), 0, 150);
  gtk_clist_set_column_width (PGtkCLIST(thelist), 1, 100);

  { Create the buttons and add them to the window. See the button
    tutorial for more examples and comments on this. }
  hbox := gtk_hbox_new(false, 0);
  gtk_box_pack_start(PGtkBOX(vbox), hbox, false, true, 0);

  button_add := gtk_button_new_with_label('Add List');
  button_clear := gtk_button_new_with_label('Clear List');
  button_hide_show := gtk_button_new_with_label('Hide/Show titles');

  gtk_box_pack_start (PGtkBOX(hbox), button_add, true, true, 0);
  gtk_box_pack_start (PGtkBOX(hbox), button_clear, true, true, 0);
  gtk_box_pack_start (PGtkBOX(hbox), button_hide_show, true, true, 0);

  { Connect our callbacks to the three buttons }
  gtk_signal_connect_object(PGtkOBJECT(button_add), 'clicked',
                            tGtksignalfunc(@button_add_clicked),
                            gpointer(thelist));
  gtk_signal_connect_object(PGtkOBJECT(button_clear), 'clicked',
                            tGtksignalfunc(@button_clear_clicked),
                            gpointer (thelist));
  gtk_signal_connect_object(PGtkOBJECT(button_hide_show), 'clicked',
                            tGtksignalfunc(@button_hide_show_clicked),
                            gpointer (thelist));

  { The interface is completely set up so we show all the widgets and
    enter the gtk_main loop }
  gtk_widget_show_all(window);
  gtk_main();
end.
