{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_fpcreslipo(const ADirectory: string);

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('utils-fpcreslipo');
    P.ShortName:='fpcreslipo';

    P.Author := 'Giulio Bernardi';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';

    P.Directory:=ADirectory;
    P.Version:='2.7.1';
    P.Dependencies.Add('fcl-res');

    P.OSes:=[darwin, iphonesim];

    P.Targets.AddImplicitUnit('msghandler.pp').install := false;
    P.Targets.AddImplicitUnit('paramparser.pp').install := false;
    P.Targets.AddImplicitUnit('sourcehandler.pp').install := false;

    T:=P.Targets.AddProgram('fpcreslipo.pp');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_fpcreslipo('');
  Installer.Run;
end.
{$endif ALLPACKAGES}




