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
{$ifdef ALLPACKAGES}
    P.Directory:='fcl-passrc';
{$endif ALLPACKAGES}
    P.Version:='2.4.0rc1';
    P.Dependencies.Add('fcl-base');
    P.Author := 'Sebastian Guenther';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Pascal parsing parts of Free Component Libraries (FCL), FPC''s OOP library.';
    P.NeedLibC:= false;

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('pastree.pp');
    T:=P.Targets.AddUnit('paswrite.pp');
      with T.Dependencies do
        begin
          AddUnit('pastree');
        end;
    T:=P.Targets.AddUnit('pparser.pp');
      with T.Dependencies do
        begin
          AddUnit('pastree');
          AddUnit('pscanner');
        end;
    T:=P.Targets.AddUnit('pscanner.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
