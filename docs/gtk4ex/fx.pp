program fx;

uses glib,gdk,gtk,frmmain,sysutils;

Var
  MainWindow : PMainWindow;

begin
  gtk_init(@argc,@argv);
  MainWindow:=NewMainForm;
  gtk_widget_show_all(PGtkWidget(MainWindow^.Window));
  ShowDir(mainwindow,Extractfilepath(Paramstr(0)){'/usr/bin/'});
  gtk_main;
end.