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

    P:=AddPackage('fcl-md');
    P.ShortName:='fclmd';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('rtl-objpas');
    P.Dependencies.Add('fcl-fpcunit');
    P.Dependencies.Add('regexpr');
    P.Author := 'Michael van Canneyt';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Extensible markdown parsing and rendering.';
    P.NeedLibC:= false;
    P.OSes:=AllOSes-[embedded,msdos,win16,macosclassic,palmos,zxspectrum,msxdos,amstradcpc,sinclairql,human68k,ps1,wasip2];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('markdown.utils.pas');
    T:=P.Targets.AddUnit('markdown.htmlentities.pas');

    T:=P.Targets.AddUnit('markdown.elements.pas');
    with T.Dependencies do
      begin
      AddUnit('markdown.utils');
     end;

    T:=P.Targets.AddUnit('markdown.line.pas');
    with T.Dependencies do
      begin
      AddUnit('markdown.utils');
     end;

    T:=P.Targets.AddUnit('markdown.scanner.pas');
    with T.Dependencies do
      begin
      AddUnit('markdown.utils');
      AddUnit('markdown.elements');
      end;
      
    T:=P.Targets.AddUnit('markdown.inlinetext.pas');
    with T.Dependencies do
      begin
      AddUnit('markdown.utils');
      AddUnit('markdown.scanner');
      AddUnit('markdown.elements');
     end;

    T:=P.Targets.AddUnit('markdown.parser.pas');
    with T.Dependencies do
      begin
      AddUnit('markdown.elements');
      AddUnit('markdown.utils');
      AddUnit('markdown.scanner');
      AddUnit('markdown.line');
      AddUnit('markdown.inlinetext');
      AddUnit('markdown.htmlentities');
      end;
        
    T:=P.Targets.AddUnit('markdown.render.pas');
    with T.Dependencies do
      begin
      AddUnit('markdown.elements');
      AddUnit('markdown.utils');
      end;
        
    T:=P.Targets.AddUnit('markdown.htmlrender.pas');
    with T.Dependencies do
      begin
      AddUnit('markdown.elements');
      AddUnit('markdown.utils');
      AddUnit('markdown.render');
      end;

    T:=P.Targets.AddUnit('markdown.latexrender.pas');
    with T.Dependencies do
      begin
      AddUnit('markdown.elements');
      AddUnit('markdown.utils');
      AddUnit('markdown.render');
      end;

    T:=P.Targets.AddUnit('markdown.fpdocrender.pas');
    with T.Dependencies do
      begin
      AddUnit('markdown.elements');
      AddUnit('markdown.utils');
      AddUnit('markdown.render');
      end;
    
    P.ExamplePath.Add('demo');
    T:=P.Targets.AddExampleProgram('demomd.lpr');
    T:=P.Targets.AddExampleProgram('md2html.lpr');
    T:=P.Targets.AddExampleProgram('md2fpdoc.lpr');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}



