{

  This file extracted from the GTK tutorial.
  list.c

  Converted from C to Pascal by Frank Loemker
  <floemker@techfak.uni-bielefeld.de>
}
program list;
uses
  glib,gdk,gtk;

{ this is our data identification string to store data in list items }
const
  list_item_data_key : pgchar = 'list_item_data';

Function itos (I : Longint) : String;
Var S : String[15];
begin
  Str (I,S);
  itos:=S;
end;

{ this is the signal handler that got connected to button
  press/release events of the GtkList }
procedure sigh_button_event (gtklist: pGtkWidget;event:pGdkEventButton;frame:pGtkWidget);cdecl;
var
  dlist, free_list : pGList;
  new_prisoner     : pGtkWidget;
  list_item        : pGtkWidget;
begin
  { we only do something if the third (rightmost mouse button
    was released }
  if (event^.thetype=GDK_BUTTON_RELEASE) and
     (event^.button=1) then
   begin
      { fetch the currently selected list item which
        will be our next prisoner ;) }
      dlist := pGTKLIST(gtklist)^.selection;
      if dlist<>nil then
        new_prisoner := pGTKWIDGET(dlist^.data)
      else
        new_prisoner := nil;

      { look for already prisoned list items, we
        will put them back into the list
        remember to free the doubly linked list that
        gtk_container_children() returns }
      dlist := gtk_container_children(pGTKCONTAINER(frame));
      free_list := dlist;
      while dlist<>nil do
       begin
         list_item := dlist^.data;
         gtk_widget_reparent(list_item, gtklist);
         dlist := dlist^.next;
       end;
      g_list_free(free_list);

      { if we have a new prisoner, remove him from the
        GtkList and put him into the frame 'Prison'
        we need to unselect the item before }
      if new_prisoner<>nil then
       begin
         gtk_list_unselect_child(pGTKLIST(gtklist),new_prisoner);
         gtk_widget_reparent(new_prisoner, frame);
       end;
    end;
end;

{ this is the signal handler that gets called if GtkList
  emits the 'selection_changed' signal }
procedure sigh_print_selection (gtklist   : pGtkWidget;func_data : gpointer);cdecl;
var dlist          : pGList;
  list_item        : pGtkObject;
  item_data_string : pgchar;
begin
  { fetch the doubly linked list of selected items
    of the GtkList, remember to treat this as read-only! }
  dlist := pGTKLIST(gtklist)^.selection;

  { if there are no selected items there is nothing more
    to do than just telling the user so }
  if dlist=nil then
   writeln ('Selection cleared')
  else
   begin
     { ok, we got a selection and so we print it }
     write ('The selection is a ');

     { get the list item from the doubly linked list
       and then query the data associated with list_item_data_key
       we then just print it }
     while dlist<>nil do
      begin
        list_item := pGTKOBJECT(dlist^.data);
        item_data_string := gtk_object_get_data(list_item,list_item_data_key);
        write (pchar(item_data_string),' ');
        dlist := dlist^.next;
      end;
     writeln;
   end;
end;

{ main function to set up the user interface }
var
  separator, window, vbox, scrolled_window,
  frame, thelist, button, list_item : pGtkWidget;
  dlist                             : pGList;
  i                                 : guint;
  buffer                            : array [0..63] of gchar;
  thelabel                          : pGtkWidget;
  str                               : pgchar;
begin
  { initialize gtk+ (and subsequently gdk) }
  gtk_init(@argc, @argv);
  gtk_rc_init;

  { create a window to put all the widgets in
    connect gtk_main_quit() to the 'destroy' event of
    the window to handle window manager close-window-events }
  window := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(pGTKWINDOW(window), 'GtkList Example');
  gtk_signal_connect(pGTKOBJECT(window),'destroy',GTK_SIGNAL_FUNC(@gtk_main_quit),nil);

  { inside the window we need a box to arrange the widgets
    vertically }
  vbox := gtk_vbox_new(false, 5);
  gtk_container_set_border_width(pGTKCONTAINER(vbox), 5);
  gtk_container_add(pGTKCONTAINER(window), vbox);

  { this is the scolled window to put the GtkList widget inside }
  scrolled_window := gtk_scrolled_window_new(nil, nil);
  gtk_widget_set_usize(scrolled_window, 250, 150);
  gtk_box_pack_start (pGTKBOX(vbox), scrolled_window, true, true, 0);

  { create the GtkList widget
    connect the sigh_print_selection() signal handler
    function to the 'selection_changed' signal of the GtkList
    to print out the selected items each time the selection
    has changed }
  thelist := gtk_list_new();
  gtk_list_set_selection_mode (pGtkList(thelist),GTK_SELECTION_BROWSE);
  gtk_scrolled_window_add_with_viewport (pGtkScrolledWindow(scrolled_window), thelist);
  gtk_signal_connect(pGTKOBJECT(thelist),'selection_changed',GTK_SIGNAL_FUNC(@sigh_print_selection),nil);

  { we create a 'Prison' to put a list item in ;) }
  frame := gtk_frame_new('Prison');
  gtk_widget_set_usize(frame, 200, 50);
  gtk_container_set_border_width(pGTKCONTAINER(frame), 5);
  gtk_frame_set_shadow_type(pGTKFRAME(frame), GTK_SHADOW_OUT);
  gtk_box_pack_start (pGTKBOX(vbox), frame, false, true, 0);

  { connect the sigh_button_event() signal handler to the GtkList
    wich will handle the 'arresting' of list items }
  gtk_signal_connect(pGTKOBJECT(thelist),'button_release_event',GTK_SIGNAL_FUNC(@sigh_button_event),frame);

  { create a separator }
  separator := gtk_hseparator_new();
  gtk_box_pack_start (pGTKBOX(vbox), separator, false, true, 0);

  { finaly create a button and connect it's 'clicked' signal
    to the destroyment of the window }
  button := gtk_button_new_with_label('Close');
  gtk_box_pack_start (pGTKBOX(vbox), button, false, true, 0);
  gtk_signal_connect_object(pGTKOBJECT(button),'clicked',GTK_SIGNAL_FUNC(@gtk_widget_destroy),pGTKOBJECT(window));

  { now we create 5 list items, each having it´s own
    label and add them to the GtkList using gtk_container_add()
    also we query the text string from the label and
    associate it with the list_item_data_key for each list item }
  for i := 0 to 4 do
   begin
     buffer:='ListItemContainer with Label #'+itos(i)+#0;
     thelabel := gtk_label_new(buffer);
     list_item := gtk_list_item_new();
     gtk_container_add(pGTKCONTAINER(list_item), thelabel);
     gtk_container_add(pGTKCONTAINER(thelist), list_item);
     gtk_label_get(pGTKLABEL(thelabel), @str);
     gtk_object_set_data(pGTKOBJECT(list_item),list_item_data_key,str);
   end;

  { here, we are creating another 5 labels, this time
    we use gtk_list_item_new_with_label() for the creation
    we can't query the text string from the label because
    we don't have the labels pointer and therefore
    we just associate the list_item_data_key of each
    list item with the same text string
    for adding of the list items we put them all into a doubly
    linked list (GList), and then add them by a single call to
    gtk_list_append_items()
    because we use g_list_prepend() to put the items into the
    doubly linked list, their order will be descending (instead
    of ascending when using g_list_append()) }
  dlist := nil;
  for i:=5 to 9 do
   begin
     buffer:='List Item with Label '+itos(i)+#0;
     list_item := gtk_list_item_new_with_label(buffer);
     dlist := g_list_prepend(dlist, list_item);
     gtk_object_set_data(pGTKOBJECT(list_item),list_item_data_key,pchar('ListItem with integrated Label'));
   end;
  gtk_list_append_items(pGTKLIST(thelist), dlist);

  { finaly we want to see the window, don't we? ;) }
  gtk_widget_show_all(window);

  { fire up the main event loop of gtk }
  gtk_main();

  { we get here after gtk_main_quit() has been called which
    happens if the main window gets destroyed }
end.
