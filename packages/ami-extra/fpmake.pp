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

    P:=AddPackage('ami-extra');
    P.ShortName := 'ami';
    P.Author := 'FPC core team';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Description := 'ami-extra, additional units for Amiga-like systems';

    P.Dependencies.Add('morphunits',[morphos]);
    P.Dependencies.Add('arosunits',[aros]);
    if Defaults.CPU=m68k then
      P.Dependencies.Add('amunits',[amiga]);
    if Defaults.CPU=powerpc then
      P.Dependencies.Add('os4units',[amiga]);

{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.3';
    P.SourcePath.Add('src');

    P.OSes:=AllAmigaLikeOSes;

    T:=P.Targets.AddUnit('amsgbox.pas');
    T:=P.Targets.AddUnit('cliputils.pas');
    T:=P.Targets.AddUnit('pcq.pas');
    T:=P.Targets.AddUnit('muihelper.pas');

    P.ExamplePath.Add('examples');
    T:=P.Targets.AddExampleProgram('muihelloworld.pas');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
