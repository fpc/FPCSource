program helloworld;

{$mode objfpc}{$H+}

uses
  fpFCGI, webmodule;

{$R *.res}

begin
  Application.Port:=2015;
  Application.Initialize;
  Application.Run;
end.

