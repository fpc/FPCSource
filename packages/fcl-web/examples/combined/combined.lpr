program combined;

{$mode objfpc}{$H+}

uses
  fpCGI, wmusers, httpdefs, websession,wmlogin;

{$R *.res}

begin
  Application.Title:='Combined RPC/Webdata example';
  Application.Initialize;
  Application.Run;
end.

