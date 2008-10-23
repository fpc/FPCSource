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

    P:=AddPackage('openal');
{$ifdef ALLPACKAGES}
    P.Directory:='openal';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

  T:=P.Targets.AddUnit('openal.pas');
  with T.Dependencies do
    begin
      AddInclude('alh.inc');
      AddInclude('alch.inc');
      AddInclude('alexth.inc');
    end;

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('madopenal.pas');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
