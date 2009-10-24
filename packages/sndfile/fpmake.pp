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

    P:=AddPackage('sndfile');
{$ifdef ALLPACKAGES}
    P.Directory:='sndfile';
{$endif ALLPACKAGES}
    P.Version:='2.4.0rc1';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('sndfile.pp');

    P.Sources.AddSrc('README');

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('sfplay.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
