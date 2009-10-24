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

    P:=AddPackage('lua');
{$ifdef ALLPACKAGES}
    P.Directory:='lua';
{$endif ALLPACKAGES}
    P.Version:='2.4.0rc1';
    P.SourcePath.Add('src');
//    P.Dependencies.Add('x11');

    T:=P.Targets.AddUnit('lauxlib.pas');
      with T.Dependencies do
        begin
          AddUnit('lua');
        end;
    T:=P.Targets.AddUnit('lualib.pas');
      with T.Dependencies do
        begin
          AddUnit('lua');
        end;
    T:=P.Targets.AddUnit('lua.pas');


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
