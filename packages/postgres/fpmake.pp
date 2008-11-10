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
{$ifdef ALLPACKAGES}
    P.Directory:='postgres';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.SourcePath.Add('src');
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

    P.Sources.AddSrc('README');

    P.ExamplePath.Add('tests/');
    P.Targets.AddExampleProgram('testpg2.pp');
    P.Targets.AddExampleProgram('testpg1.pp');
  
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
