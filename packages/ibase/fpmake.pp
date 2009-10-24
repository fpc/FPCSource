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
{$ifdef ALLPACKAGES}
    P.Directory:='ibase';
{$endif ALLPACKAGES}
    P.Version:='2.4.0rc1';
    P.Author := 'Library: (Codegear?), header: ?';
    P.License := 'Library: Interbase License, header: LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Headers for the Interbase/Firebird RDBMS';
    P.NeedLibC:= true;  // true for headers that indirectly link to libc?

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('ibase40.pp');
    T:=P.Targets.AddUnit('ibase60dyn.pp');
      with T.Dependencies do
        begin
          AddInclude('ibase60.inc');
        end;
    T:=P.Targets.AddUnit('ibase60.pp');
      with T.Dependencies do
        begin
          AddInclude('ibase60.inc');
        end;

    P.ExamplePath.Add('tests');
    P.Targets.AddExampleProgram('testib40.pp');
    P.Targets.AddExampleProgram('testib60.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
