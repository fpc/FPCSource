{$mode objfpc}{$H+}
program fpmake;

This will not compile.

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
    P:=AddPackage('brokenpackage');
    P.Version:='3.2.0-beta';

    P.Author := 'Joost van der Sluis';
    P.License := 'GPL';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Package that does not work at all';
 
    Run;
    end;
end.
