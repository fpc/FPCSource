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

    P:=AddPackage('imlib');
{$ifdef ALLPACKAGES}
    P.Directory:='imlib';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.OSes:=AllUnixOSes;
    
    P.Dependencies.Add('gtk1');
    P.Dependencies.Add('x11');

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('gdk_imlib.pp');
    T:=P.Targets.AddUnit('imlib.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
