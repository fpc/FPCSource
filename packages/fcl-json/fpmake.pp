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

    P:=AddPackage('fcl-json');
{$ifdef ALLPACKAGES}
    P.Directory:='fcl-json';
{$endif ALLPACKAGES}
    P.Version:='2.2.1';

    P.Dependencies.Add('fcl-base');

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('fpjson.pp');
    T:=P.Targets.AddUnit('jsonconf.pp');
      with T.Dependencies do
        begin
          AddUnit('fpjson');
          AddUnit('jsonparser');
        end;
    T:=P.Targets.AddUnit('jsonparser.pp');
      with T.Dependencies do
        begin
          AddUnit('fpjson');
          AddUnit('jsonscanner');
        end;
    T:=P.Targets.AddUnit('jsonscanner.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
