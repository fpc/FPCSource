program extdemo;

{$mode objfpc}{$H+}

uses
  fphttpapp, wmext;

{$R *.res}

begin
  Application.Port:=8080;
  Application.Title:='Ext.Direct demo application';
  Application.Initialize;
  Application.Run;
end.

