{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('openal');
    P.ShortName:='oal';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    if Defaults.CPU = i386 then
      P.OSes := [linux,win32,darwin]
    else
      P.OSes := [linux,win32,win64];
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


    P.NamespaceMap:='namespaces.lst';

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
