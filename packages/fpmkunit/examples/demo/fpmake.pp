{
  Demo package for fpmkunit: a unit, a program, examples and fpdoc documentation.
  "./fpmake build" compiles unit and program into units/ and bin/, "-be" adds the examples.
  "./fpmake doc" writes the HTML documentation of demogreet to docs/fpmkdemo/.
}
program fpmake;

{$mode objfpc}{$H+}

uses
  fpmkunit;

var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('fpmkdemo');
    P.Version:='1.0.0';
    P.SourcePath.Add('src');
    P.ExamplePath.Add('examples');

    T:=P.Targets.AddUnit('demogreet.pp');
    T:=P.Targets.AddProgram('greet.pp');
    T.Dependencies.AddUnit('demogreet');

    P.FPDocFormat:=[ffHtml];
    T:=P.Targets.AddFPDoc('src/demogreet.pp','xml/demogreet.xml');

    T:=P.Targets.AddExampleUnit('exampleframe.pp');
    T:=P.Targets.AddExampleProgram('framedgreet.pp');
    T.Dependencies.AddUnit('demogreet');
    T.Dependencies.AddUnit('exampleframe');

    Run;
    end;
end.
