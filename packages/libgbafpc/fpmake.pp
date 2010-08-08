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

    P:=AddPackage('libgbafpc');
{$ifdef ALLPACKAGES}
    P.Directory:='libgbafpc';
{$endif ALLPACKAGES}
    P.Version:='2.2.4a';
    P.SourcePath.Add('src');
//    P.Dependencies.Add('x11');

    // not linux compilable, skip

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
