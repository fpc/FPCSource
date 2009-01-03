{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  T : TTarget;
  P : TPackage;
  D : TDependency;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('fcl-xml');
{$ifdef ALLPACKAGES}
    P.Directory:='fcl-xml';
{$endif ALLPACKAGES}
    P.Version:='2.2.4-0';

    D:=P.Dependencies.Add('paszlib');
      D.Version:='2.2.4-0';
    D:=P.Dependencies.Add('fcl-base');
      D.Version:='2.2.4-0';

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('dom_html.pp');
      with T.Dependencies do
        begin
          AddUnit('dom');
        end;
    T:=P.Targets.AddUnit('dom.pp');
      with T.Dependencies do
        begin
          AddUnit('xmlutils');
        end;
    T:=P.Targets.AddUnit('htmldefs.pp');
    T:=P.Targets.AddUnit('htmlelements.pp');
      with T.Dependencies do
        begin
          AddInclude('tagsintf.inc');
          AddInclude('tagsimpl.inc');
          AddUnit('dom');
          AddUnit('htmldefs');
        end;
    T:=P.Targets.AddUnit('htmlwriter.pp');
      with T.Dependencies do
        begin
          AddInclude('wtagsintf.inc');
          AddInclude('wtagsimpl.inc');
          AddUnit('dom');
          AddUnit('htmlelements');
          AddUnit('htmldefs');
        end;
    T:=P.Targets.AddUnit('htmwrite.pp');
      with T.Dependencies do
        begin
          AddUnit('dom');
          AddUnit('htmldefs');
        end;
    T:=P.Targets.AddUnit('sax_html.pp');
      with T.Dependencies do
        begin
          AddUnit('sax');
          AddUnit('dom');
          AddUnit('dom_html');
          AddUnit('htmldefs');
        end;
    T:=P.Targets.AddUnit('sax.pp');
    T:=P.Targets.AddUnit('xhtml.pp');
      with T.Dependencies do
        begin
          AddUnit('dom');
          AddUnit('dom_html');
        end;
    T:=P.Targets.AddUnit('xmlcfg.pp');
      with T.Dependencies do
        begin
          AddUnit('dom');
          AddUnit('xmlread');
          AddUnit('xmlwrite');
        end;
    T:=P.Targets.AddUnit('xmlconf.pp');
      with T.Dependencies do
        begin
          AddUnit('dom');
          AddUnit('xmlread');
          AddUnit('xmlwrite');
        end;
    T:=P.Targets.AddUnit('xmlread.pp');
      with T.Dependencies do
        begin
          AddUnit('dom');
          AddUnit('xmlutils');
        end;
    T:=P.Targets.AddUnit('xmlstreaming.pp');
      with T.Dependencies do
        begin
          AddUnit('dom');
        end;
    T:=P.Targets.AddUnit('xmlutils.pp');
      with T.Dependencies do
        begin
          AddInclude('names.inc');
        end;
    T:=P.Targets.AddUnit('xmlwrite.pp');
      with T.Dependencies do
        begin
          AddUnit('dom');
        end;
    T:=P.Targets.AddUnit('xpath.pp');
      with T.Dependencies do
        begin
          AddUnit('dom');
        end;
    T:=P.Targets.AddUnit('sax_xml.pp');
      with T.Dependencies do
        begin
          AddUnit('sax');
          AddUnit('dom');
          AddUnit('htmldefs');
        end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
