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

    P:=AddPackage('ggi');
{$ifdef ALLPACKAGES}
    P.Directory:='ggi';
{$endif ALLPACKAGES}
    P.Version:='2.2.4';
    P.SourcePath.Add('src');
//    P.Dependencies.Add('x11');

    T:=P.Targets.AddUnit('ggi2d.pp');
      with T.Dependencies do
        begin
          AddUnit('ggi');
        end;
    T:=P.Targets.AddUnit('ggi.pp');
      with T.Dependencies do
        begin
          AddUnit('gii');
        end;
    T:=P.Targets.AddUnit('gii.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
