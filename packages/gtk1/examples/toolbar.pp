{

  Converted from C to Pascal by Artur Bac
  <arturbac@poczta.onet.pl>
  Reda Poland
}
{$MODE objfpc}
{$H+}
{$S+}
{$HINTS ON}
{$ifdef win32}
  {$define extdecl := stdcall;}
  {$APPTYPE GUI}
{$endif}
{$ifdef Unix}
  {$define extdecl := cdecl;}
{$endif}

Program tool_bar;
uses glib,gdk,gtk;
Const
//* XPM */
gtk_xpm : array[0..44]of pgchar = (
'32 39 5 1',
'.      c none',
'+      c black',
'@      c #3070E0',
'#      c #F05050',
'$      c #35E035',
'................+...............',
'..............+++++.............',
'............+++++@@++...........',
'..........+++++@@@@@@++.........',
'........++++@@@@@@@@@@++........',
'......++++@@++++++++@@@++.......',
'.....+++@@@+++++++++++@@@++.....',
'...+++@@@@+++@@@@@@++++@@@@+....',
'..+++@@@@+++@@@@@@@@+++@@@@@++..',
'.++@@@@@@+++@@@@@@@@@@@@@@@@@@++',
'.+#+@@@@@@++@@@@+++@@@@@@@@@@@@+',
'.+##++@@@@+++@@@+++++@@@@@@@@$@.',
'.+###++@@@@+++@@@+++@@@@@++$$$@.',
'.+####+++@@@+++++++@@@@@+@$$$$@.',
'.+#####+++@@@@+++@@@@++@$$$$$$+.',
'.+######++++@@@@@@@++@$$$$$$$$+.',
'.+#######+##+@@@@+++$$$$$$@@$$+.',
'.+###+++##+##+@@++@$$$$$$++$$$+.',
'.+###++++##+##+@@$$$$$$$@+@$$@+.',
'.+###++++++#+++@$$@+@$$@++$$$@+.',
'.+####+++++++#++$$@+@$$++$$$$+..',
'.++####++++++#++$$@+@$++@$$$$+..',
'.+#####+++++##++$$++@+++$$$$$+..',
'.++####+++##+#++$$+++++@$$$$$+..',
'.++####+++####++$$++++++@$$$@+..',
'.+#####++#####++$$+++@++++@$@+..',
'.+#####++#####++$$++@$$@+++$@@..',
'.++####++#####++$$++$$$$$+@$@++.',
'.++####++#####++$$++$$$$$$$$+++.',
'.+++####+#####++$$++$$$$$$$@+++.',
'..+++#########+@$$+@$$$$$$+++...',
'...+++########+@$$$$$$$$@+++....',
'.....+++######+@$$$$$$$+++......',
'......+++#####+@$$$$$@++........',
'.......+++####+@$$$$+++.........',
'.........++###+$$$@++...........',
'..........++##+$@+++............',
'...........+++++++..............',
'.............++++...............');
{ This function is connected to the Close button or
  closing the window from the WM }
function BOOL_TO_GINT(data : boolean) : gint;
Begin
    if data then
       BOOL_TO_GINT:=1
    else
        BOOL_TO_GINT:=0;
end;
function delete_event (widget  : PGtkWidget ;
                event : PGdkEvent;
                data : gpointer) : boolean ; cdecl;
Begin
  gtk_main_quit ();
  delete_event :=FALSE;
end;

{The above beginning seems for sure familiar to you if it's not your first GTK program.
There is one additional thing though,
we include a nice XPM picture to serve as an icon for all of the buttons.
}
Var
    close_button : PGtkWidget; { This button will emit signal to close application }
    tooltips_button : PGtkWidget; { to enable/disable tooltips }
    text_button  : PGtkWidget;
    icon_button  : PGtkWidget;
    both_button  : PGtkWidget; { radio buttons for toolbar style }
    entry        : PGtkWidget; { a text entry to show packing any widget into toolbar }

//In fact not all of the above widgets are needed here,
//but to make things clearer I put them all together.

{ that's easy... when one of the buttons is toggled, we just
 * check which one is active and set the style of the toolbar
 * accordingly
 * ATTENTION: our toolbar is passed as data to callback ! }
Procedure radio_event (widget : PGTkWidget;  data : gpointer); cdecl;

Begin
  if (gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON (text_button))) then
    gtk_toolbar_set_style(GTK_TOOLBAR ( data ), GTK_TOOLBAR_TEXT)
  else begin
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (icon_button))) then
        gtk_toolbar_set_style(GTK_TOOLBAR ( data ), GTK_TOOLBAR_ICONS);
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON (both_button))) then
        gtk_toolbar_set_style(GTK_TOOLBAR ( data ), GTK_TOOLBAR_BOTH);
    end;
End;

{ even easier, just check given toggle button and enable/disable
 * tooltips }
Procedure toggle_event (widget : PGtkWidget; data : gpointer); cdecl;
Begin
{Due to gtk specification

void        gtk_toolbar_set_tooltips        (GtkToolbar *toolbar,
                                             gboolean enable);
In Pasal unit this functioni is implemented as
procedure gtk_toolbar_set_tooltips (toolbar:PGtkToolbar;
                         enable:gint);
                         cdecl;external gtkdll name 'gtk_toolbar_set_tooltips';
so we have to change boolean to gint with function BOOL_TO_GINT implemented
on the top of source
}
  gtk_toolbar_set_tooltips (GTK_TOOLBAR ( data ),
                            BOOL_TO_GINT(
                                gtk_toggle_button_get_active(
                                    GTK_TOGGLE_BUTTON (widget))));
End;

{The above are just two callback functions that will be called
when one of the buttons on a toolbar is pressed.
You should already be familiar with things like this if you've already used toggle buttons
and radio buttons.
}
Var
  { Here is our main window (a dialog) and a handle for the handlebox }
  dialog : PGtkWidget;
  handlebox : PGtkWidget;

  { Ok, we need a toolbar, an icon with a mask (one for all of
     the buttons) and an icon widget to put this icon in (but
     we'll create a separate widget for each button) }
  toolbar : PGtkWidget;
  icon : PGdkPixmap;
  mask : PGdkBitmap;
  iconw : PGtkWidget;
  style : PGtkStyle;
Begin
  { this is called in all GTK application. }
  gtk_set_locale (); //It important for apps taht use local language specific characters
  gtk_init (@argc, @argv);
  gtk_rc_init;

  { create a new window with a given title, and nice size }
  dialog := gtk_dialog_new ();
  gtk_window_set_title ( GTK_WINDOW ( dialog ) , 'GTKToolbar Tutorial');
  gtk_widget_set_usize( GTK_WIDGET ( dialog ) , 600 , 300 );
  set_allow_shrink(PGtkWindow(dialog)^,BM_ALLOW_SHRINK);

  { typically we quit if someone tries to close us }
  gtk_signal_connect ( GTK_OBJECT ( dialog ), 'delete_event',
                       GTK_SIGNAL_FUNC ( @delete_event ), NULL);

  { we need to realize the window because we use pixmaps for
   * items on the toolbar in the context of it }
  gtk_widget_realize ( dialog );

  { to make it nice we'll put the toolbar into the handle box,
   * so that it can be detached from the main window }
  handlebox := gtk_handle_box_new ();
  gtk_box_pack_start ( GTK_BOX ( GTK_DIALOG(dialog)^.vbox ),
                       handlebox, FALSE, FALSE, 5 );

{The above should be similar to any other GTK application.
Just initialization of GTK, creating the window, etc.
There is only one thing that probably needs some explanation:
a handle box. A handle box is just another box that can be used to pack widgets in to.
 The difference between it and typical boxes is that it can be detached from
 a parent window (or, in fact, the handle box remains in the parent,
 but it is reduced to a very small rectangle, while all of its contents
  are reparented to a new freely floating window). It is usually nice
  to have a detachable toolbar, so these two widgets occur together quite often.
   toolbar will be horizontal, with both icons and text, and
   * with 5pxl spaces between items and finally,
   * we'll also put it into our handlebox }
  toolbar := gtk_toolbar_new ( GTK_ORIENTATION_HORIZONTAL,
                              GTK_TOOLBAR_BOTH );
  gtk_container_set_border_width ( GTK_CONTAINER ( toolbar ) , 5 );
  gtk_toolbar_set_space_size ( GTK_TOOLBAR ( toolbar ), 5 );
  gtk_container_add ( GTK_CONTAINER ( handlebox ) , toolbar );

  { now we create icon with mask: we'll reuse it to create
   * icon widgets for toolbar items }
  style := gtk_widget_get_style( dialog );
  icon := gdk_pixmap_create_from_xpm_d ( dialog^.window, @mask,
      @style^.fg_gc, gtk_xpm );

{Well, what we do above is just a straightforward initialization of the toolbar widget
 and creation of a GDK pixmap with its mask.
  If you want to know something more about using pixmaps,
   refer to GDK documentation or to the Pixmaps section earlier in this tutorial.}

  { our first item is <close> button }
  iconw := gtk_pixmap_new ( icon, mask ); { icon widget }
  close_button :=
  gtk_toolbar_append_item ( GTK_TOOLBAR (toolbar), { our toolbar }
                              'Close',               { button label }
                              'Closes this app',     { this button's tooltip }
                              'Private',             { tooltip private info }
                              iconw,                 { icon widget }
                              GTK_SIGNAL_FUNC (@delete_event), { a signal }
                               NULL );
  gtk_toolbar_append_space ( GTK_TOOLBAR ( toolbar ) ); { space after item }

{In the above code you see the simplest case: adding a button to toolbar.
 Just before appending a new item, we have to construct a pixmap widget
 to serve as an icon for this item; this step will have to be repeated for each new item.
 Just after the item we also add a space, so the following items will not touch each other.
 As you see gtk_toolbar_append_item returns a pointer to our newly created button widget,
  so that we can work with it in the normal way.}

  { now, let's make our radio buttons group... }
  iconw := gtk_pixmap_new ( icon, mask );
  icon_button := gtk_toolbar_append_element(
                    GTK_TOOLBAR(toolbar),
                    GTK_TOOLBAR_CHILD_RADIOBUTTON, { a type of element }
                    NULL,                          { pointer to widget }
                    'Icon',                        { label }
                    'Only icons in toolbar',       { tooltip }
                    'Private',                     { tooltip private string }
                    iconw,                         { icon }
                    GTK_SIGNAL_FUNC (@radio_event), { signal }
                    toolbar);                      { data for signal }
  gtk_toolbar_append_space ( GTK_TOOLBAR ( toolbar ) );

{Here we begin creating a radio buttons group.
To do this we use gtk_toolbar_append_element.
In fact, using this function one can also +add simple items or even spaces
(type := GTK_TOOLBAR_CHILD_SPACE or +GTK_TOOLBAR_CHILD_BUTTON).
In the above case we start creating a radio group.
In creating other radio buttons for this group a pointer to the previous button in the group
is required, so that a list of buttons can be easily constructed
(see the section on  Radio Buttons  earlier in this tutorial).

   following radio buttons refer to previous ones }
  iconw := gtk_pixmap_new ( icon, mask );
  text_button :=
    gtk_toolbar_append_element(GTK_TOOLBAR(toolbar),
                               GTK_TOOLBAR_CHILD_RADIOBUTTON,
                               icon_button,
                               'Text',
                               'Only texts in toolbar',
                               'Private',
                               iconw,
                               GTK_SIGNAL_FUNC (@radio_event),
                               toolbar);
  gtk_toolbar_append_space ( GTK_TOOLBAR ( toolbar ) );

  iconw := gtk_pixmap_new ( icon, mask );
  both_button :=
    gtk_toolbar_append_element(GTK_TOOLBAR(toolbar),
                               GTK_TOOLBAR_CHILD_RADIOBUTTON,
                               text_button,
                               'Both',
                               'Icons and text in toolbar',
                               'Private',
                               iconw,
                               GTK_SIGNAL_FUNC (@radio_event),
                               toolbar);
  gtk_toolbar_append_space ( GTK_TOOLBAR ( toolbar ) );
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(both_button),TRUE);

{In the end we have to set the state of one of the buttons manually
(otherwise they all stay in active state, preventing us from switching between them).}

  { here we have just a simple toggle button }
  iconw := gtk_pixmap_new ( icon, mask );
  tooltips_button :=
    gtk_toolbar_append_element(GTK_TOOLBAR(toolbar),
                               GTK_TOOLBAR_CHILD_TOGGLEBUTTON,
                               NULL,
                               'Tooltips',
                               'Toolbar with or without tips',
                               'Private',
                               iconw,
                               GTK_SIGNAL_FUNC (@toggle_event),
                               toolbar);
  gtk_toolbar_append_space ( GTK_TOOLBAR ( toolbar ) );
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(tooltips_button),TRUE);

{A toggle button can be created in the obvious way
 (if one knows how to create radio buttons already).}

  { to pack a widget into toolbar, we only have to
    create it and append it with an appropriate tooltip }
  entry := gtk_entry_new ();
  gtk_toolbar_append_widget( GTK_TOOLBAR (toolbar),
                             entry,
                             'This is just an entry',
                             'Private' );

  { well, it isn't created within thetoolbar, so we must still show it }
  gtk_widget_show ( entry );

{As you see, adding any kind of widget to a toolbar is simple.
The one thing you have to remember is that this widget must be shown manually
 (contrary to other items which will be shown together with the toolbar).}

  { that's it ! let's show everything. }
  gtk_widget_show ( toolbar );
  gtk_widget_show (handlebox);
  gtk_widget_show ( dialog );

  { rest in gtk_main and wait for the fun to begin! }
  gtk_main ();

End.
