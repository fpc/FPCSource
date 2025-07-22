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

    P:=AddPackage('fcl-ebnf');
    P.ShortName:='fclebnf';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('rtl-objpas');
    P.Dependencies.Add('fcl-fpcunit');
    P.Author := 'Michael van Canneyt';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'EBNF grammar parser';
    P.NeedLibC:= false;
    P.OSes:=AllOSes-[embedded,msdos,win16,macosclassic,palmos,zxspectrum,msxdos,amstradcpc,sinclairql,human68k,ps1,wasip2];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('ebnf.tree.pp');
    
    T:=P.Targets.AddUnit('ebnf.scanner.pp');
    T.ResourceStrings:=true;

    T:=P.Targets.AddUnit('ebnf.parser.pp');
    T.ResourceStrings:=true;
    with T.Dependencies do
      begin
      AddUnit('ebnf.tree');
      AddUnit('ebnf.scanner');
      end;
      
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}



