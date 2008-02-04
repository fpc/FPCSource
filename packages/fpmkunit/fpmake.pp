{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  D : TDependency;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('fpmkunit');
{$ifdef ALLPACKAGES}
    P.Directory:='fpmkunit';
{$endif ALLPACKAGES}
    P.Version:='2.2.1-1';
    P.Description:='Free Pascal Make Tool';
    D:=P.Dependencies.Add('paszlib');
      D.Version:='2.2.1-0';
    D:=P.Dependencies.Add('fcl-process');
      D.Version:='2.2.1-0';
    P.Targets.AddUnit('src/fpmkunit.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
