{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
    P:=AddPackage('packageb2');
    P.Version:='3.2.3';

    P.Author := 'Joost vam der Sluis';
    P.License := 'GPL';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Another transmit-options test-package that depends on PackageA';

    P.Dependencies.Add('packagea');

    P.SourcePath.Add('src');
 
    T:=P.Targets.AddUnit('PackageB2UnitB.pas');
    Run;
    end;
end.
