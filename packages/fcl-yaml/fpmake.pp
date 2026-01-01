{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;

Var
  T : TTarget;
  P : TPackage;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('fcl-yaml');
    P.ShortName:='openapi';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('rtl-objpas');
    P.Dependencies.Add('fcl-fpcunit');
    P.Dependencies.Add('fcl-json');
    P.Author := 'Michael van Canneyt';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'YAML file reader and data structures. YAML to JSON converter.';
    P.NeedLibC:= false;
    P.OSes:=AllOSes-[embedded,msdos,win16,macosclassic,palmos,zxspectrum,msxdos,amstradcpc,sinclairql,human68k,ps1,wasip2];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('fpyaml.strings.pp');
//    T.ResourceStrings:=true;

    T:=P.Targets.AddUnit('fpyaml.types.pp');
    with T.Dependencies do
      AddUnit('fpyaml.strings');

    T:=P.Targets.AddUnit('fpyaml.data.pp');
    with T.Dependencies do
      begin
      AddUnit('fpyaml.strings');
      AddUnit('fpyaml.types');
      end;

    T:=P.Targets.AddUnit('fpyaml.scanner.pp');
    with T.Dependencies do
      begin
      AddUnit('fpyaml.strings');
      AddUnit('fpyaml.types');
      AddUnit('fpyaml.data');
      end;
    T:=P.Targets.AddUnit('fpyaml.parser.pp');
    with T.Dependencies do
      begin
      AddUnit('fpyaml.strings');
      AddUnit('fpyaml.types');
      AddUnit('fpyaml.data');
      AddUnit('fpyaml.scanner');
      end;

   T:=P.Targets.AddUnit('fpyaml.json.pp');
   with T.Dependencies do
     begin
     AddUnit('fpyaml.strings');
     AddUnit('fpyaml.types');
     AddUnit('fpyaml.data');
     end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}



