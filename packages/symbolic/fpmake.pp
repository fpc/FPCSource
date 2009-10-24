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
    P.Version:='2.4.0rc1';
    P.Author := 'Marco van de Voort';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Expression parser with support for fast evaluation';
    P.NeedLibC:= false;

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

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
    P.Sources.AddDoc('doc/optimization.txt');
    P.Sources.AddDoc('doc/symbolic.txt');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
