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

    P:=AddPackage('ggi');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.4-rc1';
    P.Author := 'Library: ?, header: Sebastian Guenther';
    P.License := 'Library: ?, header: LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'a project that aims to develop a reliable, stable and fast graphics system that works everywhere.';
    P.NeedLibC:= true;  // true for headers that indirectly link to libc?
    P.OSes := [beos,haiku,freebsd,solaris,netbsd,openbsd,linux,dragonfly];
   
    // note that this package may be severely outdated. Header copyright
    // lists 1999, and ggi itself is still developed, and major releases
    // have happened since?

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('ggi2d.pp');
      with T.Dependencies do
        begin
          AddUnit('ggi');
        end;
    T:=P.Targets.AddUnit('ggi.pp');
      with T.Dependencies do
        begin
          AddUnit('gii');
        end;
    T:=P.Targets.AddUnit('gii.pp');

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('ggi1.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
