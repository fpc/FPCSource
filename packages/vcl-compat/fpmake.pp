{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('vcl-compat');
    P.ShortName:='vcl';
    P.Author := 'Michael Van Canneyt';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Various non-visual VCL compatibility units.';
    P.OSes := [beos,haiku,freebsd,darwin,iphonesim,ios,solaris,netbsd,openbsd,linux,win32,win64,wince,aix,amiga,aros,morphos,dragonfly,android];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.4-rc1';
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-xml');
    P.Dependencies.Add('fcl-web');
    P.Dependencies.Add('rtl-extra'); 
    P.Dependencies.Add('rtl-objpas'); 
    P.Dependencies.Add('rtl-generics');
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');


    T:=P.Targets.AddUnit('system.netencoding.pp');
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('system.ioutils.pp');
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('system.ansistrings.pp');
    T:=P.Targets.AddUnit('system.imagelist.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
