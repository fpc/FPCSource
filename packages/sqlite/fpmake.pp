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
    P.ShortName:='sqlt';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.4-rc1';
    P.OSes := AllUnixOSes+AllWindowsOSes-[qnx,win16];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

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
    T.ResourceStrings := True;
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
    T:=P.Targets.AddUnit('sqlite3ext.pp');
      T.Dependencies.AddUnit('sqlite');
 
    P.ExamplePath.Add('tests/');
    P.Targets.AddExampleProgram('testapiv3x.pp');
    P.Targets.AddExampleProgram('test.pas');
    // 'testapiv3x.README

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
