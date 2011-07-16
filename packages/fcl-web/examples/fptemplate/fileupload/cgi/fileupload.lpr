program fileupload;

{$mode objfpc}{$H+}

uses
  fpCGI, webmodule;

{$R *.res}

begin
  Application.Initialize;
  Application.Run;
end.