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

    P:=AddPackage('cocoaint');
{$ifdef ALLPACKAGES}
    P.Directory:='cocoaint';
{$endif ALLPACKAGES}
    P.Version:='2.7.1';
    P.OSes:=[darwin,iphonesim];
    P.Dependencies.Add('univint');
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('CocoaAll.pp');
    T:=P.Targets.AddUnit('WebKit.pp');
    T:=P.Targets.AddUnit('CoreData.pp');

    T:=P.Targets.AddImplicitUnit('AnonClassDefinitionsQuartzcore.pp');
    T:=P.Targets.AddImplicitUnit('AnonClassDefinitionsWebkit.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
