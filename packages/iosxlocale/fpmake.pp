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

    P:=AddPackage('iosxlocale');
    P.ShortName := 'iosl';
    P.Dependencies.Add('univint');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.4-rc1';
    P.SourcePath.Add('src');
    P.OSes:=[darwin,iphonesim,ios];

    T:=P.Targets.AddUnit('iosxlocale.pp');
    T:=P.Targets.AddUnit('iosxwstr.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
