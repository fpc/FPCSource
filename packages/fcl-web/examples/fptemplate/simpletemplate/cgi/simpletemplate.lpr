program simpletemplate;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}
  fpwidestring, unicodeducet,
  {$endif}

  fpCGI, webmodule;

{$R *.res}

begin
  {$ifdef unix}
  SetActiveCollation('DUCET');
  {$endif}
  Application.Initialize;
  Application.Run;
end.

