program echo;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}
  fpwidestring, unicodeducet,
  {$endif}
  fpFCGI, custfcgi, wmecho;

{$R *.res}

begin
  {$IFDEF UNIX}
  SetActiveCollation('DUCET');
  {$ENDIF}
  Application.Port:=2015;//Port the FCGI application is listening on
  Application.PathInfoHandling:=pihLastScriptComponent; // Assume url is of form /scriptname/echo
  Application.Initialize;
  Application.Run;
end.

