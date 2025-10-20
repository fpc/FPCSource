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

    P:=AddPackage('regexpr');
    P.ShortName:='reg';
    P.Description := 'Library for working with regular expressions.';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.OSes:=P.OSes-[embedded,win16,macosclassic,palmos,zxspectrum,msxdos,amstradcpc,sinclairql,ps1,wasip2,oric];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.Dependencies.Add('rtl-objpas');
    P.SourcePath.Add('src');

    // Sorokin's RegExpr
    T:=P.Targets.AddUnit('regexpr.pas');
    T:=P.Targets.AddUnit('uregexpr.pp');

    // RegEx from Joost
    T:=P.Targets.AddUnit('oldregexpr.pp');
    T:=P.Targets.AddUnit('regex.pp');

    T.ResourceStrings := True;

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('testreg1.pp');
    P.Sources.AddExampleFiles('examples/*',P.Directory,false,'.');


    P.NamespaceMap:='namespaces.lst';

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
