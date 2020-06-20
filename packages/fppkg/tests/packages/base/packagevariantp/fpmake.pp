{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
    P:=AddPackage('packagevariantp');
    P.Version:='3.2.1';

    P.Author := 'Joost van der Sluis';
    P.License := 'GPL';
    P.HomepageURL := 'www.freepascal.org';
    P.Description := 'Application of which the output depends on the used flavour of the packagevarianta package';

    P.Dependencies.Add('packagevarianta');

    P.SourcePath.Add('src');
 
    T:=P.Targets.AddProgram('packagevariantp.pp');
    Run;
    end;
end.
