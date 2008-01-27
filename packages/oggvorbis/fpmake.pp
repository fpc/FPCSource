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

    P:=AddPackage('xforms');
{$ifdef ALLPACKAGES}
    P.Directory:='xforms';
{$endif ALLPACKAGES}
    P.Version:='2.0.0';
    P.SourcePath.Add('src');
//    P.Dependencies.Add('x11');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
