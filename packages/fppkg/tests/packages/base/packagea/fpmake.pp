{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
    P:=AddPackage('packagea');
    P.Version:='3.2.1';

    P.Author := 'Joost vam der Sluis';
    P.License := 'GPL';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'First basic test-package';
 
    P.SourcePath.Add('src');
 
    T:=P.Targets.AddUnit('PackageAUnitA.pas');
    Run;
    end;
end.
