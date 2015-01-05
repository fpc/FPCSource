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
    P.Description := 'Interface unit for invoking Tcl interpreter using a library.';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.0.1';
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
