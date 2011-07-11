program simpletemplate;

{$mode objfpc}{$H+}

uses
  fpFCGI, webmodule;

{$R *.res}

begin
  Application.Port:=2015;//Port the FCGI application is listening on
  Application.Initialize;
  Application.Run;
end.

