{$mode objfpc}{$h+}
program GtkDef;

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils,  // exception
     Fpgtkext,  // Application, TFPgtkApplication
     Settingsrec,
     Gtkeditor;  // Mainwindow: TGtkEditorWindow
{$ELSE FPC_DOTTEDUNITS}
uses sysutils,  // exception
     FPgtkExt,  // Application, TFPgtkApplication
     settingsrec,
     GTKEditor;  // Mainwindow: TGtkEditorWindow
{$ENDIF FPC_DOTTEDUNITS}

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
