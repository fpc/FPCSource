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

    P:=AddPackage('fcl-extra');
    P.ShortName := 'fcex';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.2';
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-res');
    P.OSes:=[Win32,Win64]+AllUnixOSes;
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.Dependencies.Add('winunits-jedi',[Win32,Win64]);
    P.Dependencies.Add('winunits-base',[Win32,Win64]);
    P.Dependencies.Add('univint',[darwin,iPhoneSim,ios]);

    P.Author := '<various>';
    P.License := 'LGPL with modification, ';
    P.Email := '';
    P.Description := 'Extra libraries of Free Component Libraries(FCL), FPC''s OOP library.';
    P.NeedLibC:= false;

    P.SourcePath.Add('src');
    P.SourcePath.Add('src/win',AllWindowsOSes);
    P.IncludePath.Add('src/$(OS)',AllOSes-AllWindowsOSes-AllUnixOSes);
    P.IncludePath.Add('src/unix',AllUnixOSes);
    P.IncludePath.Add('src/win',AllWindowsOSes);
    P.IncludePath.Add('src/dummy',AllOSes);

    T:=P.Targets.AddUnit('daemonapp.pp',AllWindowsOSes+AllUnixOSes);
      with T.Dependencies do
        begin
          AddInclude('daemonapp.inc');
        end;
      T.ResourceStrings:=true;

    T:=P.Targets.AddUnit('fileinfo.pp');
      T.ResourceStrings:=true;

    // Windows units
    T:=P.Targets.AddUnit('ServiceManager.pas',[Win32,Win64]);
      T.ResourceStrings:=true;

    // Examples
    P.ExamplePath.Add('examples');
      T:=P.Targets.AddExampleProgram('showver.pp');

    // example data files.
    // showver.rc
    // showver.res

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}

