program echo;

{$mode objfpc}{$H+}

uses
  fpFCGI, wmecho;

{$R *.res}

begin
  Application.Port:=2015;//Port the FCGI application is listening on
  Application.Initialize;
  Application.Run;
end.

