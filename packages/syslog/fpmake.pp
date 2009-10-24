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
    P.Version:='2.4.0rc1';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('systemlog.pp');

    P.ExamplePath.Add('tests/');
    P.Targets.AddExampleProgram('testlog.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
