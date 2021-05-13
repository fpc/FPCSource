{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
    P:=AddPackage('plugindependency');
    P.Version:='3.2.2';

    P.Author := 'Joost van der Sluis';
    P.License := 'GPL';
    P.HomepageURL := 'www.freepascal.org';
    P.Description := 'Package on which the plugin depends';

    P.Dependencies.Add('fpmkunit');
 
    P.SourcePath.Add('src');
 
    T:=P.Targets.AddUnit('plugindependencyunit.pas');
    Run;
    end;
end.
