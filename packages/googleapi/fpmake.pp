{$ifndef ALLPACKAGES}
program fpmake;

{$mode objfpc}{$h+}

uses {$ifdef unix}cthreads,{$endif} fpmkunit;
{$endif}

Procedure add_googleapi(ADirectory : string);

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('googleapi');
    P.ShortName:='gapi';
    P.Author := 'Michael Van Canneyt';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Google Discovery to Pascal converter and Google API client support.';
    P.NeedLibC:= false;
    P.OSes := [beos,haiku,freebsd,darwin,iphonesim,ios,solaris,netbsd,openbsd,linux,win32,win64,wince,aix,amiga,aros,morphos,dragonfly];
    P.Directory:=ADirectory;
    P.Version:='3.3.1';
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('rtl-objpas');
    P.Dependencies.Add('fcl-json');
    P.Dependencies.Add('fcl-net');
    P.Dependencies.Add('openssl');
    P.Dependencies.Add('fcl-web');
    P.Dependencies.Add('regexpr');
    P.Dependencies.Add('fcl-jsonschema');
    P.Dependencies.Add('fcl-openapi');
    P.SourcePath.Add('src');

    // Runtime support for generated clients
    T:=P.Targets.AddUnit('googleapi.client.pp');
    T:=P.Targets.AddUnit('googleapi.oauthredirect.pp');

    // Discovery to Pascal converter
    T:=P.Targets.AddUnit('googlediscovery.types.pas');
    T:=P.Targets.AddUnit('googlediscovery.logging.pas');
    T.Dependencies.AddUnit('googlediscovery.types');
    T:=P.Targets.AddUnit('googlediscovery.json.pas');
    T.Dependencies.AddUnit('googlediscovery.types');
    T:=P.Targets.AddUnit('googlediscovery.http.pas');
    with T.Dependencies do
      begin
      AddUnit('googlediscovery.types');
      AddUnit('googlediscovery.json');
      AddUnit('googlediscovery.logging');
      end;
    T:=P.Targets.AddUnit('googlediscovery.parser.pas');
    with T.Dependencies do
      begin
      AddUnit('googlediscovery.types');
      AddUnit('googlediscovery.json');
      AddUnit('googlediscovery.logging');
      end;
    T:=P.Targets.AddUnit('googlediscovery.transform.pas');
    T.Dependencies.AddUnit('googlediscovery.types');
    T:=P.Targets.AddUnit('googlediscovery.config.pas');
    T.Dependencies.AddUnit('googlediscovery.types');
    T:=P.Targets.AddUnit('googlediscovery.tagging.pas');
    with T.Dependencies do
      begin
      AddUnit('googlediscovery.types');
      AddUnit('googlediscovery.transform');
      AddUnit('googlediscovery.config');
      end;
    T:=P.Targets.AddUnit('googlediscovery.generate.pas');
    with T.Dependencies do
      begin
      AddUnit('googlediscovery.types');
      AddUnit('googlediscovery.json');
      AddUnit('googlediscovery.parser');
      AddUnit('googlediscovery.transform');
      AddUnit('googlediscovery.tagging');
      AddUnit('googlediscovery.logging');
      end;
    T:=P.Targets.AddUnit('googlediscovery.yaml.pas');
    T.Dependencies.AddUnit('googlediscovery.transform');
    T:=P.Targets.AddUnit('googlediscovery.servicemap.pas');
    T.Dependencies.AddUnit('googlediscovery.transform');
    T:=P.Targets.AddUnit('googlediscovery.main.pas');
    with T.Dependencies do
      begin
      AddUnit('googlediscovery.types');
      AddUnit('googlediscovery.json');
      AddUnit('googlediscovery.http');
      AddUnit('googlediscovery.config');
      AddUnit('googlediscovery.parser');
      AddUnit('googlediscovery.generate');
      AddUnit('googlediscovery.yaml');
      AddUnit('googlediscovery.logging');
      end;
    T:=P.Targets.AddUnit('googleapi.compat.generator.pp');

    T:=P.Targets.AddProgram('discovery2pas.pp');
    with T.Dependencies do
      begin
      AddUnit('googlediscovery.types');
      AddUnit('googlediscovery.http');
      AddUnit('googlediscovery.parser');
      AddUnit('googlediscovery.generate');
      AddUnit('googlediscovery.main');
      AddUnit('googlediscovery.logging');
      AddUnit('googlediscovery.servicemap');
      AddUnit('googleapi.compat.generator');
      end;
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_googleapi('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
