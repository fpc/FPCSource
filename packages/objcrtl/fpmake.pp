{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}
    P:=AddPackage('objcrtl');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.0-beta';
    P.OSes:=[darwin,iphonesim];
    P.Author := 'Library: Apple, header: Dmitry "skalogryz" Boyarintsev';
    P.License := 'Library: Apple, header: LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Objective-C Runtime-Library.';
    P.NeedLibC:= false;  // true for headers that indirectly link to libc?

    P.SourcePath.Add('src');

    P.Targets.AddUnit('objcrtl.pas');
    P.Targets.AddUnit('objcrtl10.pas');
    P.Targets.AddUnit('objcrtl20.pas');
    P.Targets.AddUnit('objcrtliphoneos.pas');
    P.Targets.AddUnit('objcrtlmacosx.pas');
    P.Targets.AddUnit('objcrtlutils.pas');

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('examples/objcrtltest.pas');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
