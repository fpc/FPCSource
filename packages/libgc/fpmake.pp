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

    P:=AddPackage('libgc');
    P.ShortName:='libgc';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.1';
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');
    P.OSes := [linux];
    T:=P.Targets.AddUnit('gcmem.pp');
    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('testcmem.pp');
    P.Targets.AddExampleProgram('create_leak.pp');
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
