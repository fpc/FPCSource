{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('lua');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.SourcePath.Add('src');
//    P.Dependencies.Add('x11');
    P.OSes := [linux,win32];

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



    P.NamespaceMap:='namespaces.lst';

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
