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

    P:=AddPackage('fcl-syntax');
    P.ShortName:='fclsh';
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
    P.Description := 'Syntax highlighter.';
    P.NeedLibC:= false;
    P.OSes:=AllOSes-[embedded,msdos,win16,macosclassic,palmos,zxspectrum,msxdos,amstradcpc,sinclairql,human68k,ps1,wasip2,oric];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('syntax.highlighter.pp');
    T:=P.Targets.AddUnit('syntax.pascal.pp');
    T.Dependencies.AddUnit('syntax.highlighter');
    T:=P.Targets.AddUnit('syntax.bash.pp');
    T.Dependencies.AddUnit('syntax.highlighter');
    T:=P.Targets.AddUnit('syntax.json.pp');
    T.Dependencies.AddUnit('syntax.highlighter');
    T:=P.Targets.AddUnit('syntax.css.pp');
    T.Dependencies.AddUnit('syntax.highlighter');
    T:=P.Targets.AddUnit('syntax.javascript.pp');
    T.Dependencies.AddUnit('syntax.highlighter');
    T:=P.Targets.AddUnit('syntax.ini.pp');
    T.Dependencies.AddUnit('syntax.highlighter');
    T:=P.Targets.AddUnit('syntax.sql.pp');
    T.Dependencies.AddUnit('syntax.highlighter');
    T:=P.Targets.AddUnit('syntax.html.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.css');
      AddUnit('syntax.javascript');
      end;
    T:=P.Targets.AddUnit('syntax.htmlrender.pp');
    T.Dependencies.AddUnit('syntax.highlighter');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}



