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
{$ifdef ALLPACKAGES}
    P.Directory:='libvlc';
{$endif ALLPACKAGES}
    P.Version:='1.0';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := 'michael@freepascal.org';
    P.Description := 'VLC library (version 2 or higher) interface and component.';
    T:=P.Targets.AddUnit('src/libvlc.pp');
    T:=P.Targets.AddUnit('src/vlc.pp');
    with T.Dependencies do
      begin
      AddUnit('libvlc');
      end;
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif}