program GettingStarted;

{$mode objfpc}{$H+}

uses
  GTK2;

var
  MainWindow: PGtkWidget;

begin
  gtk_init (@argc, @argv);

  MainWindow := gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_widget_show  (MainWindow);

  gtk_main;
end.
