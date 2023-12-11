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

    P:=AddPackage('tosunits');
    P.ShortName := 'tos';

    P.Author := 'FPC core team';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Description := 'tosunits, OS interface units for Atari TOS/GEM';

{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.SourcePath.Add('src');

    P.OSes:=[atari];

    T:=P.Targets.AddUnit('gemdos.pas');
    T:=P.Targets.AddUnit('xbios.pas');
    T:=P.Targets.AddUnit('bios.pas');
    T:=P.Targets.AddUnit('tos.pas');
    T:=P.Targets.AddUnit('vdi.pas');
    T:=P.Targets.AddUnit('aes.pas');
    T:=P.Targets.AddUnit('gem.pas');
    T:=P.Targets.AddUnit('gemcmmn.pas');
    T:=P.Targets.AddUnit('nf_ops.pas');
    T:=P.Targets.AddUnit('metados.pas');

    P.ExamplePath.Add('examples');
    T:=P.Targets.AddExampleProgram('higem.pas');
    T:=P.Targets.AddExampleProgram('gemwin.pas');
    T:=P.Targets.AddExampleProgram('gemcube.pas');
    T:=P.Targets.AddExampleProgram('showpic.pas');

    P.Sources.AddDoc('README.txt');

    P.NamespaceMap:='namespaces.lst';

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
