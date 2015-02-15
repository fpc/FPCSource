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

    P.Author := 'FPC core team';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Description := 'ami-extra, additional units for Amiga-like systems';

    P.Dependencies.Add('morphunits',[morphos]);
    P.Dependencies.Add('arosunits',[aros]);
    P.Dependencies.Add('amunits',[amiga]);

{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.1.1';
    P.SourcePath.Add('src');

    P.OSes:=AllAmigaLikeOSes;

    T:=P.Targets.AddUnit('cliputils.pas');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
