program fpde;
{$mode objfpc}
{$H+}
{ $apptype gui}

uses fpgtk,fpglib,fpgtkext,frmmain,pgeditor;

begin
  application := TFPgtkApplication.Create;
  application.MainWindow := TMainForm.Create;
  application.Run;
  application.Free;
end.
