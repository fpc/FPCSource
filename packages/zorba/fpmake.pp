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
    P.ShortName := 'zorb';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.3';
    P.OSes := [linux,win32];
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('xqc.pas');
      with T.Dependencies do
        begin
          AddInclude('xqc_error.inc');
          AddInclude('xqc_static_context_consts.inc');
        end;
    T:=P.Targets.AddUnit('zorbadyn.pas');
      with T.Dependencies do
        begin
          AddUnit('xqc');
          AddInclude('zorba.inc');
          AddInclude('zorba_options.inc');
        end;
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('zorba.pas');
      with T.Dependencies do
        begin
          AddUnit('xqc');
          AddInclude('zorba.inc');
          AddInclude('zorba_options.inc');
        end;

//    P.ExamplePath.Add('tests/');
//    P.Targets.AddExampleProgram('testapiv3x.pp');
//    P.Targets.AddExampleProgram('test.pas');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
