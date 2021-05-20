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

    P:=AddPackage('ibase');
    P.ShortName:='ibas';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.3';
    P.Author := 'Library: (Codegear?), header: ?';
    P.License := 'Library: Interbase License, header: LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Headers for the Interbase/Firebird RDBMS';
    P.NeedLibC:= true;  // true for headers that indirectly link to libc?
    P.OSes := AllUnixOSes+AllWindowsOSes-[qnx];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('ibase40.pp');
    T:=P.Targets.AddUnit('ibase60dyn.pp');
      with T.Dependencies do
        begin
          AddInclude('ibase60.inc');
        end;
    T.ResourceStrings := True;
    T:=P.Targets.AddUnit('ibase60.pp');
      with T.Dependencies do
        begin
          AddInclude('ibase60.inc');
        end;

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('testib40.pp');
    P.Targets.AddExampleProgram('testib60.pp');
    P.Sources.AddExampleFiles('examples/*',P.Directory,false,'.');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
