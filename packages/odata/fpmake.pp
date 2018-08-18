{$ifndef ALLPACKAGES}
program fpmake;

{$mode objfpc}{$h+}

uses fpmkunit;
{$endif}

Procedure Add_OData(ADirectory : string);

  function StdDep(T : TTarget) : TTarget;
  begin
    T.Dependencies.AddUnit('odatabase');
    T.Dependencies.AddUnit('odataservice');
    Result:=T;
  end;

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('odata');
    P.ShortName:='odata';
    P.Author := 'Michael Van Canneyt';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'OData client base libraries, Microsoft Office365 clients';
    P.NeedLibC:= false;
    P.OSes := [beos,haiku,freebsd,darwin,iphonesim,solaris,netbsd,openbsd,linux,win32,win64,wince,aix,amiga,aros,morphos,dragonfly];
    P.Directory:=ADirectory;
    P.Version:='3.2.0-beta';
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('rtl-extra');
    P.Dependencies.Add('rtl-objpas');
    P.Dependencies.Add('fcl-json');
    P.Dependencies.Add('fcl-web');
    P.SourcePath.Add('src');
    T:=P.Targets.AddUnit('odatabase.pp');
    T:=P.Targets.AddUnit('odataservice.pp');
    T.Dependencies.AddUnit('odatabase');
    T:=StdDep(P.Targets.AddUnit('msgraph.pp'));
    T:=StdDep(P.Targets.AddUnit('sharepoint.pp'));
    T:=P.Targets.AddUnit('office365client.pp');
    end;
end;

{$ifndef ALLPACKAGES}
begin
  Add_OData('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
