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
    P.ShortName:='lgd';
    P.Description := 'Interface unit for library libgd - image processing';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.1.1';
    P.SourcePath.Add('src');
    P.OSes := P.OSes - [embedded,nativent,msdos];

    T:=P.Targets.AddUnit('gd.pas');

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('gdtestcgi.pp');
    P.Targets.AddExampleProgram('gdtest.pp');
    P.Sources.AddExampleFiles('examples/*',false,'.');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
