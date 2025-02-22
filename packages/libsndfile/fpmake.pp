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

    P:=AddPackage('libsndfile');
    P.ShortName:='lsndfile';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Author := 'Library: libsndfile';
    P.License := 'Library: LGPL';
    P.HomepageURL := 'https://libsndfile.github.io/libsndfile/';
    P.Email := '';
    P.Description := 'Headers for the libsndfile library';
    P.NeedLibC:= true;  // true for headers that indirectly link to libc?
    P.OSes := AllUnixOSes-[qnx];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [android];
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('sndfile.pp');

    P.Sources.AddSrc('README');

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('sfplay.pp');

    P.NamespaceMap:='namespaces.lst';

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
