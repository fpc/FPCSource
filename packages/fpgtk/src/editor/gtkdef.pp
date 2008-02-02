{$mode objfpc}{$h+}
program GtkDef;

uses sysutils,  // exception
     FPgtkExt,  // Application, TFPgtkApplication
     settingsrec,
     GTKEditor;  // Mainwindow: TGtkEditorWindow

begin
  try
    Log ('Creating application');
    application := TFPgtkApplication.Create;
    Log ('Setting mainwindow');
    application.MainWindow := TGtkEditorWindow.Create;
    Log ('Running GTK');
    application.Run;
    Log ('Everything Closed');
    application.Free;
    Log ('Cleaned up everything');
  except
    on e : Exception do
      begin
      writeln ('UNEXPECTED ERROR: ', e.message);
      ShowMessage ('UNEXPECTED ERROR ', e.message);
      end;
  end;

end.
