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

    P:=AddPackage('odbc');
{$ifdef ALLPACKAGES}
    P.Directory:='odbc';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('odbcsqldyn.pas');
      with T.Dependencies do
        begin
          AddInclude('odbcsql.inc');
        end;
    T:=P.Targets.AddUnit('odbcsql.pas');
      with T.Dependencies do
        begin
          AddInclude('odbcsql.inc');
        end;

    P.ExamplePath.Add('tests');
    P.Targets.AddExampleProgram('testodbc.pp');
    // 'testodbc.mdb

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
