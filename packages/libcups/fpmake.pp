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

    P:=AddPackage('libcups');
    P.ShortName := 'lcup';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.4-rc1';

    P.Author := 'Header: Olivier GUILBAUD';
    P.License := 'Header: LGPL with modi ';
    P.HomepageURL := 'www.freepascal.org';
    P.OSes := [beos,haiku,freebsd,darwin,iphonesim,ios,solaris,netbsd,openbsd,linux,aix,dragonfly];
    P.Email := '';
    P.Description := 'This package provides an interface with Common UNIX Printing System.';
    P.NeedLibC:= false;  // true for headers that indirectly link to libc?
    
    P.Dependencies.Add('rtl-extra');
    
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('cupsdyn.pp');

    P.ExamplePath.Add('examples');
//    P.Targets.AddExampleProgram('testcups.pp');
//    P.Sources.AddExampleFiles('examples/*',P.Directory,false,'.');


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
