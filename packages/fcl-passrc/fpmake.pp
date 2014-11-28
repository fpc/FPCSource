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

    P:=AddPackage('fcl-passrc');
    P.ShortName:='fcls';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='2.7.1';
    P.Dependencies.Add('fcl-base');
    P.Author := 'Sebastian Guenther';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Pascal parsing parts of Free Component Libraries (FCL), FPC''s OOP library.';
    P.NeedLibC:= false;
    P.OSes:=AllOSes-[embedded,msdos];

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('pastree.pp');
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('pscanner.pp');
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('pparser.pp');
      with T.Dependencies do
        begin
          AddUnit('pastree');
          AddUnit('pscanner');
        end;
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('pastounittest.pp');
      with T.Dependencies do
        begin
          AddUnit('pparser');
          AddUnit('pastree');
          AddUnit('pscanner');
        end;
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('passrcutil.pp');
      with T.Dependencies do
        begin
          AddUnit('pparser');
          AddUnit('pastree');
          AddUnit('pscanner');
        end;
    T.ResourceStrings := False;

    T:=P.Targets.AddUnit('paswrite.pp');
      with T.Dependencies do
        begin
          AddUnit('pastree');
        end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
