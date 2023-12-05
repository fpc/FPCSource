{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;

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
    P.Version:='3.3.1';
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-xml');
    P.Dependencies.Add('fcl-web');
    P.Dependencies.Add('rtl-extra'); 
    P.Dependencies.Add('rtl-objpas'); 
    P.Dependencies.Add('rtl-generics');
    P.Dependencies.Add('fcl-json');
    P.Dependencies.Add('fcl-hash');
    P.Dependencies.Add('hash');
    P.Dependencies.Add('libpcre',[Win64,Linux,darwin]);
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');


    T:=P.Targets.AddUnit('system.permissions.pp');
    T:=P.Targets.AddUnit('system.messaging.pp');
    T:=P.Targets.AddUnit('system.netencoding.pp');
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('system.ioutils.pp');
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('system.devices.pp');
    T:=P.Targets.AddUnit('system.analytics.pp');
    T:=P.Targets.AddUnit('system.ansistrings.pp');
    T:=P.Targets.AddUnit('system.imagelist.pp');
    T:=P.Targets.AddUnit('system.diagnostics.pp');
    T:=P.Targets.AddUnit('system.notification.pp');
    T.Dependencies.AddUnit('system.messaging');
    T:=P.Targets.AddUnit('system.json.pp');
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('system.pushnotifications.pp');
    T.ResourceStrings := True;
    T.Dependencies.AddUnit('system.messaging');
    T.Dependencies.AddUnit('system.json');
    T:=P.Targets.AddUnit('system.hash.pp');
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('system.regularexpressionsconsts.pp',[Win64,Linux,darwin]);
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('system.regularexpressionscore.pp',[Win64,Linux,darwin]);
    T.Dependencies.AddUnit('system.regularexpressionsconsts',[Win64,Linux,darwin]);
    T:=P.Targets.AddUnit('system.regularexpressions.pp',[Win64,Linux,darwin]);
    T.Dependencies.AddUnit('system.regularexpressionscore',[Win64,Linux,darwin]);


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
