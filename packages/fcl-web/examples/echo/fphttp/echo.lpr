program echo;

{$mode objfpc}{$H+}

uses
  SysUtils, fphttpapp, wmecho;

begin
  Application.Port:=StrToIntDef(ParamStr(1), 8080);
  Application.Initialize;
  Application.Run;
end.

