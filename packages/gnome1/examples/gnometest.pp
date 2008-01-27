Program gnometest;

uses glib, gdk, gtk, libgnome, libgnomeui;

const
  Authors : Array[0..2] of Pchar = ('me', 'myself', 'I');
var
  AboutBox : PGTKWidget;
  App : PGTKWidget;
  Appbar : PGTKWidget;
  Clock : PGTKWidget;
  calc : PGTKWIdget;
begin
  gnome_init('libgnometest', '0.1',argc, argv);
  App := gnome_app_new('libgnometest', 'gnome-test #1');
  AppBar := gnome_appbar_new(False, True,GNOME_PREFERENCES_USER);
  GTK_Widget_show(AppBar);
  gnome_app_set_statusbar (PGnomeApp(App), Appbar);
  Clock := gtk_clock_new(GTK_CLOCK_REALTIME);
  gtk_clock_set_update_interval(GTK_Clock(Clock), 1);
  gtk_widget_show(Clock);
  gtk_clock_set_format(GTK_Clock(Clock), '%H:%M:%S');
  gnome_app_set_contents(Gnome_App(App), Clock);
  GTK_Widget_Show(App);
  AboutBox := gnome_about_new(gnome_app_id, nil, 'none', @Authors[0],'blah, blah, blah','/usr/share/icons/dialog_box.xpm');
  GTK_Widget_Show(AboutBox);
  gtk_signal_connect(GTK_OBJECT (AboutBox), 'destroy',
                   @gtk_main_quit, nil);
  gtk_main;
end.
