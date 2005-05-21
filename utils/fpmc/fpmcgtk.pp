{$mode objfpc}
{$H+}
{$apptype gui}
program fpmcgtk;

uses fpgtk,fpglib,fpgtkext,frmmain;

begin
  application := TFPgtkApplication.Create;
  application.MainWindow := TMainForm.Create;
  application.Run;
  application.Free;
end.
