program extdemo;

{$mode objfpc}{$H+}

uses
  fpCGI, wmext;

{$R *.res}

begin
  Application.Title:='Ext.Direct demo application';
  Application.Initialize;
  Application.Run;
end.

