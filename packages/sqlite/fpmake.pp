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

    P:=AddPackage('sqlite');
{$ifdef ALLPACKAGES}
    P.Directory:='sqlite';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('sqlite3db.pas');
      with T.Dependencies do
        begin
          AddUnit('sqlite3');
        end;
    T:=P.Targets.AddUnit('sqlite3dyn.pp');
      with T.Dependencies do
        begin
          AddInclude('sqlite3.inc');
        end;
    T:=P.Targets.AddUnit('sqlite3.pp');
      with T.Dependencies do
        begin
          AddInclude('sqlite3.inc');
        end;
    T:=P.Targets.AddUnit('sqlitedb.pas');
      with T.Dependencies do
        begin
          AddUnit('sqlite');
        end;
    T:=P.Targets.AddUnit('sqlite.pp');


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
