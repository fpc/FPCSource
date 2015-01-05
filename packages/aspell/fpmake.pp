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

    P:=AddPackage('aspell');

{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}

    P.Version:='3.0.1';

    P.Author := 'header:Aleš Katona, library: Kevin Atkinson';
    P.License := 'header: LGPL with modification, library: LGPL 2.0 or 2.1';
    P.HomepageURL := 'www.freepascal.org';
    P.OSes := [beos,haiku,freebsd,darwin,iphonesim,netbsd,openbsd,linux,win32,aix,dragonfly];
    P.Email := '';
    P.Description := 'The New Aspell, spelling library';
    P.NeedLibC:= true;

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('aspell.pp');
    T:=P.Targets.AddUnit('aspelldyn.pp');
    T:=P.Targets.AddUnit('spellcheck.pp');
    with T.Dependencies do
      begin
        AddUnit('aspell');
       end;

    P.Sources.AddSrc('LICENSE');
    P.Sources.AddSrc('LICENSE.ADDON');

    P.ExamplePath.Add('examples');
    T:=P.Targets.AddExampleProgram('example.pas');
    
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
