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

    P:=AddPackage('libxml2');
{$ifdef ALLPACKAGES}
    P.Directory:='libxml';
{$endif ALLPACKAGES}
    P.Version:='2.6.32';
    P.SourcePath.Add('src');

  T:=P.Targets.AddUnit('libxml2.pas');
  with T.Dependencies do
    begin
      AddInclude('xinclude.h');
      AddInclude('xpointer.h');
      AddInclude('HTMLparser.h');
      AddInclude('schemasInternals.h');
      AddInclude('SAX2.h');
      AddInclude('xmlversion.h');
      AddInclude('globals.h');
      AddInclude('xmlexports.h');
      AddInclude('nanoftp.h');
      AddInclude('SAX.h');
      AddInclude('uri.h');
      AddInclude('debugXML.h');
      AddInclude('xmlunicode.h');
      AddInclude('DOCBparser.h');
      AddInclude('xmlIO.h');
      AddInclude('xmlsave.h');
      AddInclude('HTMLtree.h');
      AddInclude('parserInternals.h');
      AddInclude('chvalid.h');
      AddInclude('xmlwriter.h');
      AddInclude('relaxng.h');
      AddInclude('threads.h');
      AddInclude('list.h');
      AddInclude('encoding.h');
      AddInclude('catalog.h');
      AddInclude('pattern.h');
      AddInclude('xmlregexp.h');
      AddInclude('xmlerror.h');
      AddInclude('xpath.h');
      AddInclude('xmlautomata.h');
      AddInclude('entities.h');
      AddInclude('xmlreader.h');
      AddInclude('xmlstring.h');
      AddInclude('xmlmemory.h');
      AddInclude('xmlmodule.h');
      AddInclude('xmlschemas.h');
      AddInclude('hash.h');
      AddInclude('nanohttp.h');
      AddInclude('parser.h');
      AddInclude('tree.h');
      AddInclude('dict.h');
      AddInclude('xlink.h');
      AddInclude('valid.h');
      AddInclude('xpathInternals.h');
      AddInclude('xmlschemastypes.h');
      AddInclude('c14n.h');
      AddInclude('schematron.h');
    end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
