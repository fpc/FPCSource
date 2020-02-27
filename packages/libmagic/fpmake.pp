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

    P:=AddPackage('libmagic');
    P.ShortName:='magic';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.0-beta';
    P.Author := 'Library: libmagic';
    P.License := 'Library: GPL, header: LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Headers for the magic library (library to determine file type)';
    P.NeedLibC:= true;  // true for headers that indirectly link to libc?
    P.OSes := AllUnixOSes-[qnx];
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');
    
    T:=P.Targets.AddUnit('libmagic.pp');
    
    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('basic.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
