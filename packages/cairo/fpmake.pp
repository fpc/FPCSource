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

    P:=AddPackage('cairo');
{$ifdef ALLPACKAGES}
    P.Directory:='cairo';
{$endif ALLPACKAGES}
    P.Version:='2.2.1';
    P.SourcePath.Add('src');

    P.Dependencies.Add('x11');
    P.Dependencies.Add('fcl-image');
    
    T:=P.Targets.AddUnit('cairo.pp');


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
