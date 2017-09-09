program webdemo;

{$mode objfpc}{$H+}

uses
  fphttpapp, regreports, wmreports;

begin
  Application.Port:=8080;
  Application.AllowDefaultModule:=True;
  Application.DefaultModuleName:='Page';
  if IsConsole then
    Writeln('Point your browser to http://localhost:',Application.Port,'/Page  or http://localhost:',Application.Port);
  Application.PreferModuleName:=True;
  Application.Initialize;
  Application.Run;
end.

