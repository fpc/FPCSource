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

    P:=AddPackage('libgd');
{$ifdef ALLPACKAGES}
    P.Directory:='libgd';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('gd.pp');

    P.ExamplePath.Add('tests');
    P.Targets.AddExampleProgram('gdtestcgi.pp');
    P.Targets.AddExampleProgram('gdtest.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
