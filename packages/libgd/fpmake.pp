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

    P:=AddPackage('gd');
{$ifdef ALLPACKAGES}
    P.Directory:='libgd';
{$endif ALLPACKAGES}
    P.Version:='2.4.0rc1';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('gd.pas');

    P.ExamplePath.Add('tests');
    P.Targets.AddExampleProgram('gdtestcgi.pp');
    P.Targets.AddExampleProgram('gdtest.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
