program echo;

{$mode objfpc}{$H+}

uses
  fpCGI, wmecho;

{$R *.res}

begin
  Application.Initialize;
  Application.Run;
end.

