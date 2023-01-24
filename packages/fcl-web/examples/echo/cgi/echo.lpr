program echo;

{$mode objfpc}{$H+}

uses
{$ifdef unix}
  cwstring,
{$endif}
  fpCGI, wmecho;

{$R *.res}

begin
  Application.Initialize;
  Application.Title:='Echo demo';
  Application.Run;
end.

