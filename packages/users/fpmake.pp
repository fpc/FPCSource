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

    P:=AddPackage('users');
{$ifdef ALLPACKAGES}
    P.Directory:='users';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('grp.pp');
    T:=P.Targets.AddUnit('pwd.pp');
    T:=P.Targets.AddUnit('shadow.pp');
    T:=P.Targets.AddUnit('users.pp');
      with T.Dependencies do
        begin
          AddUnit('pwd');
          AddUnit('shadow');
          AddUnit('grp');
        end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
