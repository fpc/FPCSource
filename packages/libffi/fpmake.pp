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

    P:=AddPackage('libffi');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.0-beta';
    P.Author := 'Anthony Green and others';
    P.License := 'MIT license';
    P.HomepageURL := 'https://sourceware.org/libffi/';
    P.Email := 'libffi-discuss@sourceware.org';
    P.Description := 'Headers for the libFFI library (Foreign Function Interface)';
    P.NeedLibC:= true;  // true for headers that indirectly link to libc?
    P.OSes := [linux];
    P.CPUs := [x86_64];
    P.Dependencies.Add('rtl-objpas');

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('ffi.pp');
    T:=P.Targets.AddUnit('ffi.manager.pp');
    T.Dependencies.AddUnit('ffi');

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('simple.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
