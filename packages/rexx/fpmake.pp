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

    P:=AddPackage('rexx');
{$ifdef ALLPACKAGES}
    P.Directory:='rexx';
{$endif ALLPACKAGES}
    P.Version:='2.4.0rc1';
    P.SourcePath.Add('src');
    P.Oses:=[emx,os2];
    p.Targets.AddUnit('rexxsaa.pp',[OS2]);

    P.Sources.AddSrc('readme.txt');

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('callrexx.pas',[OS2]);
    // 'backward.fnc

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}


