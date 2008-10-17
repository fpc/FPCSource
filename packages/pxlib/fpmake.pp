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
    P:=AddPackage('pxlib');
{$ifdef ALLPACKAGES}
    P.Directory:='pxlib';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    T:=P.Targets.AddUnit('src/pxlib.pp');
      T.OSes:=[Linux];
    T:=P.Targets.AddExampleunit('examples/ppxview.pp');
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
