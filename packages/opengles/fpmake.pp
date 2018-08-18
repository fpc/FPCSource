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

    P:=AddPackage('opengles');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.0-beta';
    P.Author := 'Free Pascal Development team';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.OSes := [darwin,iphonesim,linux,win32,win64,wince];

    P.Dependencies.Add('x11',AllUnixOSes-[darwin,iphonesim]);
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('gles11.pp',[darwin,iphonesim]);
    T:=P.Targets.AddUnit('gles20.pas',[linux,win32,win64,wince,darwin]);

    P.Targets.AddExampleProgram('examples/es2example1.pas');
    P.Sources.AddExampleFiles('examples/*',P.Directory,false,'.');


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
