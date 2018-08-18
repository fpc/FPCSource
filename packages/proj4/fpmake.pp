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

    P:=AddPackage('proj4');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.0-beta';
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');
    P.OSes := [linux];

  T:=P.Targets.AddUnit('proj.pas');
  with T.Dependencies do
    begin
    end;

    // 'Makefile
    // 'Makefile.fpc
    // 'test1.xml
    // 'test2.xml

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
