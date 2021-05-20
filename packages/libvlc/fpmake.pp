{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  T : TTarget;
  P : TPackage;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}
    P:=AddPackage('libvlc');
    P.ShortName := 'lvlc';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.OSes := [win32, win64, linux, freebsd];
    P.Dependencies.Add('fcl-base');
    P.Version:='3.2.3';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := 'michael@freepascal.org';
    P.Description := 'VLC library (version 2 or higher) interface and component.';
    T:=P.Targets.AddUnit('src/libvlc.pp',[linux,win32,win64]);
    T:=P.Targets.AddUnit('src/vlc.pp',[linux,win32,win64]);
    with T.Dependencies do
      begin
      AddUnit('libvlc');
      end;
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif}
