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

    P:=AddPackage('rsvg');
{$ifdef ALLPACKAGES}
    P.Directory:='librsvg';
{$endif ALLPACKAGES}
    P.Version:='2.6.3';
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');
    P.Dependencies.Add('gtk2');

  T:=P.Targets.AddUnit('rsvg.pas');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
