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

    P:=AddPackage('postgres');
    P.ShortName:='pgr';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.0.5';
    P.SourcePath.Add('src');
    P.OSes := AllUnixOSes-[qnx]+AllWindowsOSes;

    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('dllistdyn.pp');
      with T.Dependencies do
        begin
          AddInclude('dllisttypes.inc');
        end;
    T:=P.Targets.AddUnit('dllist.pp');
      with T.Dependencies do
        begin
          AddInclude('dllisttypes.inc');
        end;
    T:=P.Targets.AddUnit('postgres3dyn.pp');
      with T.Dependencies do
        begin
          AddInclude('postgres3types.inc');
          AddUnit('dllistdyn');
        end;
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('postgres3.pp');
      with T.Dependencies do
        begin
          AddInclude('postgres3types.inc');
          AddUnit('dllist');
        end;
    T:=P.Targets.AddUnit('postgres.pp');
      with T.Dependencies do
        begin
          AddUnit('dllist');
        end;

    P.Sources.AddSrc('README.txt');

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('testpg2.pp');
    P.Targets.AddExampleProgram('testpg1.pp');
    P.Sources.AddExampleFiles('examples/*',P.Directory,false,'.');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
