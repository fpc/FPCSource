{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_importtl;

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('importtl');

    P.Author := '<various>';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Reads type information from "file" and converts it into a freepascal binding.';
    P.NeedLibC:= false;
    P.Dependencies.Add('winunits-base');
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-registry');

{$ifdef ALLPACKAGES}
    P.Directory:='importtl';
{$endif ALLPACKAGES}
    P.Version:='2.7.1';

    P.OSes:=[win32,win64];

    T:=P.Targets.AddProgram('importtl.pas');
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_rmwait;
  Installer.Run;
end.
{$endif ALLPACKAGES}




