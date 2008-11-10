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

    P:=AddPackage('fcl-registry');
{$ifdef ALLPACKAGES}
    P.Directory:='fcl-registry';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';

    P.Author := 'FPC development team';
    P.License := 'LGPL with modification, ';
    P.ExternalURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Windows registry + emulation parts of Free Component Libraries (FCL), FPC''s OOP library.';
    P.NeedLibC:= false;

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');
    T:=P.Targets.AddUnit('registry.pp');
      with T.Dependencies do
        begin
          AddInclude('regdef.inc');
          AddInclude('xregreg.inc');
          AddInclude('regini.inc');
          AddUnit('inifiles');
          AddUnit('xmlreg');
        end;
    T:=P.Targets.AddUnit('xmlreg.pp');
      with T.Dependencies do
        begin
          AddUnit('dom');
          AddUnit('xmlread');
          AddUnit('xmlwrite');
        end;

    P.ExamplePath.Add('examples');
    P.Targets.AddExampleProgram('tests/testbasics.pp');
    P.Targets.AddExampleProgram('tests/regtestframework.pp');
    // 'tests/Makefile
    // 'tests/Makefile.fpc

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
