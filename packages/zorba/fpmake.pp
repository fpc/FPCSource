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

    P:=AddPackage('zorba');
{$ifdef ALLPACKAGES}
    P.Directory:='zorba';
{$endif ALLPACKAGES}
    P.Version:='0.9.9';
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('zorbadyn.pas');
      with T.Dependencies do
        begin
          AddInclude('zorba.inc');
          AddInclude('zorba_error.inc');
          AddInclude('zorba_options.inc');
          AddInclude('zorba_static_context_consts.inc');
        end;
    T:=P.Targets.AddUnit('zorba.pas');
      with T.Dependencies do
        begin
          AddInclude('zorba.inc');
          AddInclude('zorba_error.inc');
          AddInclude('zorba_options.inc');
          AddInclude('zorba_static_context_consts.inc');
        end;

//    P.ExamplePath.Add('tests/');
//    P.Targets.AddExampleProgram('testapiv3x.pp');
//    P.Targets.AddExampleProgram('test.pas');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
