program testyaml;

{$mode objfpc}{$H+}

uses
  ConsoleTestRunner, utyamlparser, fpyaml.data, fpyaml.scanner, utyamlscanner, fpyaml.types, utyamldata, fpyaml.parser, fpyaml.json,
  fpyaml.strings;

{$R *.res}

var
  Application : TTestRunner;
begin
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  Application:=TTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.

