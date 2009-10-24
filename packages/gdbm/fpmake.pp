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

    P:=AddPackage('gdbm');
{$ifdef ALLPACKAGES}
    P.Directory:='gdbm';
{$endif ALLPACKAGES}
    P.Version:='2.4.0rc1';
    P.Author := 'Library: GNU, header: Michael van Canneyt';
    P.License := 'Library: GPL2 (or later?), header: LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'GNU dbm is a set of database routines that use extensible hashing.';
    P.NeedLibC:= true;  // true for headers that indirectly link to libc?

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('gdbm.pp');

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('tests/testgdbm.pp');
    P.Targets.AddExampleProgram('tests/testgdbm2.pp');


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
