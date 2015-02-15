{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  D : TDependency;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('fpmkunit');
    P.ShortName:='fpmk';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.1.1';
    P.Description:='Free Pascal Make Tool';
    P.Author := 'Peter Vreman';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Basic library of the fpmake/fppkg build system.';
    P.NeedLibC:= false;  // true for headers that indirectly link to libc?
    P.OSes := P.OSes - [embedded,nativent,msdos];

    // All dependencies (including implicit) are listed
    // here to be able to update all requirements to
    // compile fpmake from a single place
    D:=P.Dependencies.Add('hash');
      D.Version:='3.1.1';
    D:=P.Dependencies.Add('paszlib');
      D.Version:='3.1.1';
    D:=P.Dependencies.Add('fcl-process');
      D.Version:='3.1.1';
    D:=P.Dependencies.Add('libtar');
      D.Version:='3.1.1';

    with P.Targets.AddUnit('src/fpmkunit.pp') do
      ResourceStrings:=true;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
