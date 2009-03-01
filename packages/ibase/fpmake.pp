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

    P:=AddPackage('ibase');
{$ifdef ALLPACKAGES}
    P.Directory:='ibase';
{$endif ALLPACKAGES}
    P.Version:='2.2.4';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('ibase40.pp');
    T:=P.Targets.AddUnit('ibase60dyn.pp');
      with T.Dependencies do
        begin
          AddInclude('ibase60.inc');
        end;
    T:=P.Targets.AddUnit('ibase60.pp');
      with T.Dependencies do
        begin
          AddInclude('ibase60.inc');
        end;


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
