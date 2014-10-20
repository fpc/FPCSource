{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_importtl(const ADirectory: string);

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

    P.Directory:=ADirectory;
    P.Version:='2.7.1';
    P.SeparateArchive:=false;

    P.OSes:=[win32,win64];

    T:=P.Targets.AddProgram('importtl.pas');
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_importtl('');
  Installer.Run;
end.
{$endif ALLPACKAGES}




