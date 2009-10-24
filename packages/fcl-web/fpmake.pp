{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  T : TTarget;
  P : TPackage;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('fcl-web');
{$ifdef ALLPACKAGES}
    P.Directory:='fcl-web';
{$endif ALLPACKAGES}
    P.Version:='2.4.0rc1';
    P.Dependencies.Add('fcl-db');
    P.Dependencies.Add('fcl-xml');
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-process');
    P.Dependencies.Add('fastcgi');

    P.Author := 'FreePascal development team';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Web(app) related parts of Free Component Libraries (FCL), FPC''s OOP library.';
    P.NeedLibC:= false;

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('cgiapp.pp');
    T:=P.Targets.AddUnit('custcgi.pp');
      with T.Dependencies do
        begin
          AddUnit('httpdefs');
        end;
    T:=P.Targets.AddUnit('ezcgi.pp');
    T:=P.Targets.AddUnit('fpcgi.pp');
      with T.Dependencies do
        begin
          AddUnit('httpdefs');
          AddUnit('fphttp');
        end;
    T:=P.Targets.AddUnit('fpdatasetform.pp');
      with T.Dependencies do
        begin
          AddUnit('fphtml');
        end;
    T:=P.Targets.AddUnit('fphtml.pp');
      with T.Dependencies do
        begin
          AddUnit('httpdefs');
          AddUnit('fphttp');
        end;
    T:=P.Targets.AddUnit('fphttp.pp');
      with T.Dependencies do
        begin
          AddUnit('httpdefs');
        end;
    T:=P.Targets.AddUnit('fptemplate.pp');
    T:=P.Targets.AddUnit('fpweb.pp');
      with T.Dependencies do
        begin
          AddUnit('httpdefs');
          AddUnit('fphttp');
          AddUnit('fptemplate');
          AddUnit('websession');
        end;
    T:=P.Targets.AddUnit('httpdefs.pp');
    T:=P.Targets.AddUnit('websession.pp');
      with T.Dependencies do
        begin
          AddUnit('fphttp');
          AddUnit('httpdefs');
        end;
    T:=P.Targets.AddUnit('webutil.pp');
      with T.Dependencies do
        begin
          AddUnit('httpdefs');
        end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
