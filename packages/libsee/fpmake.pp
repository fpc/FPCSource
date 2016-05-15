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

    P:=AddPackage('libsee');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.1.1';
    P.Author := 'David Leonard';
    P.License := 'BSD style';
    P.HomepageURL := 'http://www.adaptive-enterprises.com.au/~d/software/see/';
    P.Email := 'leonard@users.sourceforge.net';
    P.Description := 'Headers for the libSEE library (javascript engine)';
    P.NeedLibC:= true;  // true for headers that indirectly link to libc?
    P.OSes := [linux,win32];

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('libsee.pas');

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleUnit('mod_stream.pp');
    P.Targets.AddExampleProgram('tlibsee.pp');
    P.Targets.AddExampleProgram('testwrite.pp');
    P.Targets.AddExampleProgram('teststream.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
