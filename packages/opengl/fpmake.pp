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

    P:=AddPackage('opengl');
{$ifdef ALLPACKAGES}
    P.Directory:='opengl';
{$endif ALLPACKAGES}
    P.Version:='2.2.4';
    P.OSes:=AllUnixOSes+[Win32,Win64];

    P.Dependencies.Add('x11',AllUnixOSes);

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('glext.pp');
    T:=P.Targets.AddUnit('gl.pp');
    T:=P.Targets.AddUnit('glu.pp');
    T:=P.Targets.AddUnit('glut.pp');
    T:=P.Targets.AddUnit('glx.pp',AllUnixOSes);

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
