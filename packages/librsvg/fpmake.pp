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
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.OSes := [beos,haiku,freebsd,netbsd,openbsd,linux,win32,win64,aix,dragonfly];
    // Do not build x11 on iPhone (=arm-darwin)
    if Defaults.CPU<>arm then
      P.OSes := P.OSes + [darwin];
    P.Version:='3.2.3';
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');
    P.Dependencies.Add('gtk2');

  T:=P.Targets.AddUnit('rsvg.pas');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
