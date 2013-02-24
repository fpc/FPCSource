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

    P:=AddPackage('uuid');
{$ifdef ALLPACKAGES}
    P.Directory:='uuid';
{$endif ALLPACKAGES}
    P.Version:='2.6.3';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('libuuid.pp');
    T:=P.Targets.AddUnit('macuuid.pp');

    P.Sources.AddSrc('README.txt');

    P.ExamplePath.Add('tests/');
    P.Targets.AddExampleProgram('testlibuid.pp');
    P.Targets.AddExampleProgram('testuid.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
