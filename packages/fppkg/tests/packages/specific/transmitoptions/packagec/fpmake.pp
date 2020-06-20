{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;

{$R *.res}

begin
  With Installer do
    begin
    P:=AddPackage('packagec');
    P.Version:='3.2.1';

    P.Author := 'Joost van der Sluis';
    P.License := 'GPL';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Transmit-options test-package that depends on PackageB1 and PackageB2';

    P.Dependencies.Add('packageb1');
    P.Dependencies.Add('packageb2');

    P.SourcePath.Add('src');
 
    T:=P.Targets.AddProgram('PackageC.pas');
    Run;
    end;
end.
