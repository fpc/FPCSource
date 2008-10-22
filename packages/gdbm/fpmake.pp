{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('gdbm');
{$ifdef ALLPACKAGES}
    P.Directory:='gdbm';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('gdbm.pp');

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('tests/testgdbm.pp');
    P.Targets.AddExampleProgram('tests/testgdbm2.pp');


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
