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

    P:=AddPackage('libfontconfig');
    P.ShortName:='lfcg';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.2';
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');
    P.OSes := [linux,freebsd, darwin]; // Darwin was tested!
    T:=P.Targets.AddUnit('libfontconfig.pp');
    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('testfc.pp');
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
