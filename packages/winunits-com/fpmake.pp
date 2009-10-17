{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  T : TTarget;
  P : TPackage;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('winunits-com');
{$ifdef ALLPACKAGES}
    P.Directory:='winunits-com';
{$endif ALLPACKAGES}
    P.Version:='2.4.0-0';
    P.OSes:=[win32,win64];
    P.Author := 'Florian Klaempfl, Marco van de Voort';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'COM related units that need FCL';
    P.NeedLibC:= true;

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('buildwinunitscom.pp');
      T.Install:=False;
      with T.Dependencies do
        begin
          AddUnit('comobj');
          AddUnit('comserv');
        
        end;
    T:=P.Targets.AddImplicitUnit('comobj.pp');
    T:=P.Targets.AddImplicitUnit('comserv.pp');
        
    P.ExamplePath.Add('examples/');
    P.Targets.AddExampleProgram('testcom1.pp');
    P.Targets.AddExampleProgram('OOTest.pp');
    P.Targets.AddExampleProgram('OOHelper.pp');
    P.Targets.AddExampleProgram('testver.pp');
    P.Targets.AddExampleProgram('testcom2.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
