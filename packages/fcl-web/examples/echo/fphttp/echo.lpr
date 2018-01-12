program echo;

{$mode objfpc}{$H+}

uses
  fphttpapp, wmecho;

begin
  Application.Port:=2018;
  Application.Initialize;
  Application.Run;
end.

