{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;

Var
  T : TTarget;
  P : TPackage;
  D : TDependency;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('fcl-css');
    P.ShortName:='fclcss';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Options.Add('-S2h');
    D:=P.Dependencies.Add('fcl-base');

    P.Author := 'Michael Van Canneyt';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'CSS parsing and utility functions.';
    P.NeedLibC:= false;
    P.OSes:=AllOSes-[embedded,msdos,win16,macosclassic,palmos,zxspectrum,msxdos,amstradcpc,sinclairql,human68k];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('fpcsstree.pp');
    T:=P.Targets.AddUnit('fpcssscanner.pp');
    T.Dependencies.AddUnit('fpcsstree');
    T.ResourceStrings:=True;
    T:=P.Targets.AddUnit('fpcssparser.pp');
    T.ResourceStrings:=True;
      with T.Dependencies do
        begin
          AddUnit('fpcsstree');
          AddUnit('fpcssscanner');
        end;
    T:=P.Targets.AddUnit('fpcssresolver.pas');
      with T.Dependencies do
        AddUnit('fpcssparser');
    T:=P.Targets.AddUnit('fpcssutils.pp');
      with T.Dependencies do
        begin
          AddUnit('fpcsstree');
          AddUnit('fpcssscanner');
          AddUnit('fpcssparser');
        end;
    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('examples/cssmin.lpr');
    P.Targets.AddExampleProgram('examples/extractcssclasses.lpr');


    P.NamespaceMap:='namespaces.lst';

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}

