{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
    P:=AddPackage('pluginpack');
    P.Version:='3.2.3';

    P.Author := 'Joost van der Sluis';
    P.License := 'GPL';
    P.HomepageURL := 'www.freepascal.org';
    P.Description := 'A fpmake-plugin with a dependency';

    P.Dependencies.Add('fpmkunit');
    P.Dependencies.Add('plugindependency');

    P.SourcePath.Add('src');

    P.IsFPMakeAddIn := True;

    T:=P.Targets.AddUnit('pluginunit.pas');
    T.IsFPMakePlugin := True;
    Run;
    end;
end.
