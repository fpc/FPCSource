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

    P:=AddPackage('syslog');
{$ifdef ALLPACKAGES}
    P.Directory:='syslog';
{$endif ALLPACKAGES}
    P.Version:='2.7.1';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('systemlog.pp');

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('testlog.pp');
    P.Sources.AddExampleFiles('examples/*',false,'.');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
