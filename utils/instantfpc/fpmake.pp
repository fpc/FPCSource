{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_instantfpc(const ADirectory: string);

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('utils-instantfpc');
    P.ShortName:='ifpc';
    P.OSes:=AllOSes-[embedded,msdos,win16,macosclassic,palmos];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.Author := '<various>';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'A tool to execute pascal programs as unix scripts.';
    P.NeedLibC:= false;

    P.Directory:=ADirectory;
    P.Version:='3.2.3';

    P.Dependencies.Add('fcl-process');
    P.Options.Add('-S2h');

    P.OSes := [win32,win64,wince,haiku,linux,freebsd,openbsd,netbsd,darwin,iphonesim,ios,solaris,aix];

    T:=P.Targets.AddProgram('instantfpc.pas');
    T.Dependencies.AddUnit('instantfptools');

    P.Targets.AddUnit('instantfptools.pas').Install:=False;
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_instantfpc('');
  Installer.Run;
end.
{$endif ALLPACKAGES}




