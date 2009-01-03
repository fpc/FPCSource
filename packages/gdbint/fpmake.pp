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

    P:=AddPackage('gdbint');
{$ifdef ALLPACKAGES}
    P.Directory:='gdbint';
{$endif ALLPACKAGES}
    P.Version:='2.2.4-0';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('gdbcon.pp');
      with T.Dependencies do
        begin
          AddUnit('gdbint');
        end;
    T:=P.Targets.AddUnit('gdbint.pp');
      with T.Dependencies do
        begin
          AddInclude('gdbver.inc');
        end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
