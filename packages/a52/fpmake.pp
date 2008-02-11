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

    P:=AddPackage('a52');
{$ifdef ALLPACKAGES}
    P.Directory:='a52';
{$endif ALLPACKAGES}
    P.Version:='2.2.1';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('a52.pas');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
