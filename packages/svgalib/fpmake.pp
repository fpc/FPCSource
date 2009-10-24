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

    P:=AddPackage('svgalib');
{$ifdef ALLPACKAGES}
    P.Directory:='svgalib';
{$endif ALLPACKAGES}
    P.Version:='2.4.0rc1';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('svgalib.pp');
    T:=P.Targets.AddUnit('vgamouse.pp');

    P.Sources.AddSrc('README');
 
    P.ExamplePath.Add('tests/');
    P.Targets.AddExampleProgram('testvga.pp');
    P.Targets.AddExampleProgram('vgatest.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
