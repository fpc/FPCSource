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
    P.OSes:=AllOSes-[embedded,msdos,win16,macosclassic,palmos,zxspectrum,msxdos,amstradcpc,sinclairql,human68k,ps1,wasip2];
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
    T:=P.Targets.AddUnit('syntax.go.pp');
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
    T:=P.Targets.AddUnit('syntax.lfm.pp');
    T.Dependencies.AddUnit('syntax.highlighter');
    T:=P.Targets.AddUnit('syntax.table.pp');
    T.Dependencies.AddUnit('syntax.highlighter');
    T:=P.Targets.AddUnit('syntax.c.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.cpp.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      AddUnit('syntax.c');
      end;
    T:=P.Targets.AddUnit('syntax.csharp.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.java.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.kotlin.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.swift.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.rust.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.zig.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.d.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.typescript.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.ruby.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.lua.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.php.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.perl.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.basic.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.toml.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.assembly.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.batch.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.powershell.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.dockerfile.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.fortran.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.ada.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.modula2.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.oberon.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.python.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.xml.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.yaml.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.diff.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.makefile.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.barye.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.markdown.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;
    T:=P.Targets.AddUnit('syntax.rst.pp');
    With T.Dependencies do
      begin
      AddUnit('syntax.highlighter');
      AddUnit('syntax.table');
      end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}



