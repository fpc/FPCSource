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
    P.ShortName:='lpng';
    P.Description := 'Interface unit for libpng - working with PNG image format.';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='2.7.1';
    P.Dependencies.Add('zlib');
    P.SourcePath.Add('src');
    P.OSes := AllUnixOSes-[qnx]+[win32,os2,emx];

    T:=P.Targets.AddUnit('png.pp');
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
