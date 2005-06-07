{

  This file extracted from the GTK 1.2 tutorial.
  Section 6.2

  Converted from C to Pascal by Thomas E. Payne
}
 program Tut6_2;

{$mode objfpc}

 uses
  glib,gdk,gtk,sysutils;

   //* Our usual callback function */

 procedure toggle_callback( widget : pGtkWidget; data : pgpointer   );cdecl;
   begin
     writeln ('Hello again - '+pchar(data)+' was pressed');
          if active(GTK_TOGGLE_BUTTON(widget)^)<>0 then
            //* If control reaches here, the toggle button is down */
            writeln('Toggle button is down')
          else
            //* If control reaches here, the toggle button is up */
            writeln('Toggle button is up');
   end;
  var
    //* GtkWidget is the storage type for widgets */
    window,button,box1,label_ : pGtkWidget;
  begin
     gtk_init (@argc, @argv);
     //* Create a new window */
     window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
     gtk_window_set_title (GTK_WINDOW (window), 'Toggle Buttons!');
     //* It's a good idea to do this for all windows. */
     gtk_signal_connect (GTK_OBJECT (window), 'destroy',
                         GTK_SIGNAL_FUNC (@gtk_exit), Nil);
     gtk_signal_connect (GTK_OBJECT (window), 'delete_event',
                         GTK_SIGNAL_FUNC (@gtk_exit), Nil);
     //* Sets the border width of the window. */
     gtk_container_set_border_width (GTK_CONTAINER (window), 10);
     gtk_widget_realize(window);
     //* Create a new button */
     button := gtk_toggle_button_new ();
     //* Connect the "clicked" signal of the button to our callback */
     gtk_signal_connect (GTK_OBJECT (button), 'clicked',
                         GTK_SIGNAL_FUNC (@toggle_callback), pchar('toggle button'));
     //* This calls our box creating function */
     box1 := gtk_hbox_new(False, 0);
     gtk_container_set_border_width (GTK_CONTAINER (box1), 2);
     //* Create a label for the button */
     label_ := gtk_label_new ('toggle button');
     gtk_box_pack_start (GTK_BOX (box1), label_, FALSE, FALSE, 3);
     //* Pack and show all our widgets */
     gtk_widget_show(label_);
     gtk_widget_show(box1);
     gtk_container_add (GTK_CONTAINER (button), box1);
     gtk_widget_show(button);
     gtk_container_add (GTK_CONTAINER (window), button);
     gtk_widget_show (window);
     //* Rest in gtk_main and wait for the fun to begin! */
     gtk_main ();
 end.
