{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  T : TTarget;

begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    StartPackage('fpmkunit');
{$ifdef ALLPACKAGES}
    Directory:='fpmkunit';
{$endif ALLPACKAGES}
    AddDependency('paszlib');
    AddDependency('fcl-process');
    Version:='2.2.0';
    Description:='Free Pascal Make Tool';
    T:=Targets.AddUnit('src/fpmkunit.pp');
    EndPackage;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
