{$mode objfpc}
program TestGTK;

uses SysUtils, classes, FPgtk, FPgtkExt, lister;

begin
  try
    writeln ('Creating application');
    application := TFPgtkApplication.Create;
    writeln ('Setting mainwindow');
    application.MainWindow := TlistWindow.Create;
    writeln ('Running GTK');
    application.Run;
    writeln ('Everything Closed');
    application.Free;
    writeln ('Cleaned up everything');
  except
    on e : Exception do
      writeln ('UNEXPECTED ERORR: ',e.message);
  end;
end.
