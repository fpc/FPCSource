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

    P:=AddPackage('tcl');
{$ifdef ALLPACKAGES}
    P.Directory:='tcl';
{$endif ALLPACKAGES}
    P.Version:='2.7.1';
    P.SourcePath.Add('src');
    P.OSes := AllUnixOSes+AllWindowsOSes+[os2,emx]-[qnx];

    T:=P.Targets.AddUnit('tcl80.pp');

    P.ExamplePath.Add('tests/');
    P.Targets.AddExampleProgram('tcl_demo.pp');
    // 'test.tcl

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
