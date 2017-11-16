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
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.0.5';
    P.OSes := AllUnixOSes+AllWindowsOSes-[qnx];
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

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('testodbc.pp');
    P.Sources.AddExampleFiles('examples/*',P.Directory,false,'.');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
