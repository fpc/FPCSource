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

    P:=AddPackage('symbolic');
{$ifdef ALLPACKAGES}
    P.Directory:='symbolic';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

//    P.Dependencies.Add('x11');

    T:=P.Targets.AddUnit('symbolic.pas');
      with T.Dependencies do
        begin
          AddInclude('exprstrs.inc');
          AddInclude('parsexpr.inc');
          AddInclude('symbexpr.inc');
          AddInclude('teval.inc');
          AddInclude('rearrang.inc');
        end;

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('evaltest.pas');
    P.Targets.AddExampleProgram('rpnthing.pas');


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
