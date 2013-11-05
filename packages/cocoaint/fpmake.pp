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
    P.Version:='2.6.4rc1';
    P.OSes:=[darwin,iphonesim];
    P.Dependencies.Add('univint');
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('CocoaAll.pas');
    T:=P.Targets.AddUnit('WebKit.pas');
    T:=P.Targets.AddUnit('CoreData.pas');

    T:=P.Targets.AddImplicitUnit('AnonClassDefinitionsQuartzcore.pas');
    T:=P.Targets.AddImplicitUnit('AnonClassDefinitionsWebkit.pas');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
