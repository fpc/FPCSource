{

  This file extracted from the GTK tutorial.
  notebook.c

  Converted from C to Pascal by Frank Loemker
  <floemker@techfak.uni-bielefeld.de>
}
program notebook;
uses
 glib,gdk,gtk;

Function itos (I : Longint) : String;
Var S : String[15];
begin
  Str (I,S);
  itos:=S;
end;

{ This function rotates the position of the tabs }
procedure rotate_book (notebook:pGtkNotebook ); cdecl;
begin
  gtk_notebook_set_tab_pos (notebook, TGtkPositionType((tab_pos(notebook^) +1) mod 4));
end;

{ Add/Remove the page tabs and the borders }
procedure tabsborder_book (notebook:pGtkNotebook ); cdecl;
var tval, bval : gboolean;
begin
  tval := false;
  bval := false;
  if show_tabs(notebook^) = 0 then
    tval := true;
  if show_border(notebook^) = 0 then
    bval := true;

  gtk_notebook_set_show_tabs (notebook, tval);
  gtk_notebook_set_show_border (notebook, bval);
end;

{ Remove a page from the notebook }
procedure remove_book (notebook: pGtkNotebook ); cdecl;
var page : gint ;
begin
  page := gtk_notebook_get_current_page(notebook);
  gtk_notebook_remove_page (notebook, page);
  { Need to refresh the widget --
   This forces the widget to redraw itself. }
  gtk_widget_draw(pGTKWIDGET(notebook), NIL);
end;

procedure delete (widget : pGtkWidget ; event: pGdkEvent; data: pgpointer ); cdecl;
begin
  gtk_main_quit ();
end;

var
  window, button, table, thenotebook, frame, thelabel, checkbutton : pGtkWidget;
  i                                                               : integer;
  bufferf, bufferl                                                : string[33];
begin
  gtk_init (@argc, @argv);

  window := gtk_window_new (GTK_WINDOW_TOPLEVEL);

  gtk_signal_connect (pGTKOBJECT (window), 'delete_event',
                      GTK_SIGNAL_FUNC (@delete), NIL);

  gtk_container_set_border_width (pGTKCONTAINER (window), 10);

  table := gtk_table_new(2,6,FALSE);
  gtk_container_add (pGTKCONTAINER (window), table);

  { Create a new notebook, place the position of the tabs }
  thenotebook := gtk_notebook_new ();
  gtk_notebook_set_tab_pos (pGTKNOTEBOOK (thenotebook), GTK_POS_TOP);
  gtk_table_attach_defaults(pGTKTABLE(table), thenotebook, 0,6,0,1);

  { lets append a bunch of pages to the notebook }
  for i:=0 to 4 do begin
    bufferf := 'Append Frame '+itos(i+1)+#0;
    bufferl := 'Page '+itos(i+1)+#0;

    frame := gtk_frame_new (pchar(@bufferf[1]));
    gtk_container_set_border_width (pGTKCONTAINER (frame), 10);
    gtk_widget_set_usize (frame, 100, 75);

    thelabel := gtk_label_new (pchar(@bufferf[1]));
    gtk_container_add (pGTKCONTAINER (frame), thelabel);

    thelabel := gtk_label_new (pchar(@bufferl[1]));
    gtk_notebook_append_page (pGTKNOTEBOOK (thenotebook), frame, thelabel);
  end;

  { now lets add a page to a specific spot }
  checkbutton := gtk_check_button_new_with_label ('Check me please!');
  gtk_widget_set_usize(checkbutton, 100, 75);

  thelabel := gtk_label_new ('Add page');
  gtk_notebook_insert_page (pGTKNOTEBOOK (thenotebook), checkbutton, thelabel, 2);

  { Now finally lets prepend pages to the notebook }
  for i:=0 to 4 do begin
    bufferf := 'Prepend Frame '+itos(i+1)+#0;
    bufferl := 'PPage '+itos(i+1)+#0;

    frame := gtk_frame_new (pchar(@bufferf[1]));
    gtk_container_set_border_width (pGTKCONTAINER (frame), 10);
    gtk_widget_set_usize (frame, 100, 75);

    thelabel := gtk_label_new (pchar(@bufferf[1]));
    gtk_container_add (pGTKCONTAINER (frame), thelabel);

    thelabel := gtk_label_new (pchar(@bufferl[1]));
    gtk_notebook_prepend_page (pGTKNOTEBOOK(thenotebook), frame, thelabel);
        gtk_widget_show (frame);
  end;

  { Set what page to start at (page 4) }
  gtk_notebook_set_page (pGTKNOTEBOOK(thenotebook), 3);

  { create a bunch of buttons }
  button := gtk_button_new_with_label ('close');
  gtk_signal_connect_object (pGTKOBJECT (button), 'clicked',
                             GTK_SIGNAL_FUNC (@delete), NIL);
  gtk_table_attach(pGTKTABLE(table), button, 0,1,1,2,
                   GTK_FILL or GTK_EXPAND,GTK_FILL,0,0);

  button := gtk_button_new_with_label ('next page');
  gtk_signal_connect_object (pGTKOBJECT (button), 'clicked',
                             GTK_SIGNAL_FUNC (@gtk_notebook_next_page),
                             pGTKOBJECT (thenotebook));
  gtk_table_attach(pGTKTABLE(table), button, 1,2,1,2,
                   GTK_FILL or GTK_EXPAND,GTK_FILL,0,0);

  button := gtk_button_new_with_label ('prev page');
  gtk_signal_connect_object (pGTKOBJECT (button), 'clicked',
                             GTK_SIGNAL_FUNC (@gtk_notebook_prev_page),
                             pGTKOBJECT (thenotebook));
  gtk_table_attach(pGTKTABLE(table), button, 2,3,1,2,
                   GTK_FILL or GTK_EXPAND,GTK_FILL,0,0);

  button := gtk_button_new_with_label ('tab position');
  gtk_signal_connect_object (pGTKOBJECT (button), 'clicked',
                             GTK_SIGNAL_FUNC (@rotate_book), pGTKOBJECT(thenotebook));
  gtk_table_attach(pGTKTABLE(table), button, 3,4,1,2,
                   GTK_FILL or GTK_EXPAND,GTK_FILL,0,0);

  button := gtk_button_new_with_label ('tabs/border on/off');
  gtk_signal_connect_object (pGTKOBJECT (button), 'clicked',
                             GTK_SIGNAL_FUNC (@tabsborder_book),
                             pGTKOBJECT (thenotebook));
  gtk_table_attach(pGTKTABLE(table), button, 4,5,1,2,
                   GTK_FILL or GTK_EXPAND,GTK_FILL,0,0);

  button := gtk_button_new_with_label ('remove page');
  gtk_signal_connect_object (pGTKOBJECT (button), 'clicked',
                             GTK_SIGNAL_FUNC (@remove_book),
                             pGTKOBJECT(thenotebook));
  gtk_table_attach(pGTKTABLE(table), button, 5,6,1,2,
                   GTK_FILL ,GTK_FILL,0,0);

  gtk_widget_show_all(window);

  gtk_main ();
end.
