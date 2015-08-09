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

    P:=AddPackage('fuse');
{$ifdef ALLPACKAGES}
    P.Directory:='fuse';
{$endif ALLPACKAGES}
    P.Version:='3.0.0rc1';
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

  T:=P.Targets.AddUnit('fuse.pas');

  P.ExamplePath.Add('tests');
  P.Targets.AddExampleProgram('fusetest.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
