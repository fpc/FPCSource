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

    P:=AddPackage('libgd');
    P.ShortName:='lgd';
    P.Description := 'Interface unit for library libgd - image processing';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.4-rc1';
    P.SourcePath.Add('src');
    P.OSes := P.OSes - [embedded,nativent,msdos,wii,win16,atari,macosclassic,palmos,symbian];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    T:=P.Targets.AddUnit('gd.pas');

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('gdtestcgi.pp');
    P.Targets.AddExampleProgram('gdtest.pp');
    P.Sources.AddExampleFiles('examples/*',P.Directory,false,'.');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
