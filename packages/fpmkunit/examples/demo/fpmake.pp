{
  Demo package for fpmkunit: a unit and a program, plus an example unit and an example program.
  "./fpmake build" compiles the unit and the program into units/ and bin/.
  "./fpmake build -be" also compiles the examples, into the examples directory of units/.
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

    T:=P.Targets.AddExampleUnit('exampleframe.pp');
    T:=P.Targets.AddExampleProgram('framedgreet.pp');
    T.Dependencies.AddUnit('demogreet');
    T.Dependencies.AddUnit('exampleframe');

    Run;
    end;
end.
