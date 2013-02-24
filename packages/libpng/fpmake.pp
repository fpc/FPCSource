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

    P:=AddPackage('libpng');
{$ifdef ALLPACKAGES}
    P.Directory:='libpng';
{$endif ALLPACKAGES}
    P.Version:='2.6.3';
    P.Dependencies.Add('zlib');
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('png.pp');
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
