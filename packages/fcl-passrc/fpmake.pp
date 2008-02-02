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
    P.Version:='2.2.1';

    P.Dependencies.Add('fcl-base');

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
