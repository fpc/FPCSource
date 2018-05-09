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

    P:=AddPackage('pastojs');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}

    P.Version:='3.1.1';
    P.OSes:=AllUnixOSes+AllBSDOSes+AllWindowsOSes-[WinCE];
    P.Dependencies.Add('paszlib');
    P.Dependencies.Add('fcl-js');
    P.Dependencies.Add('fcl-json');
    P.Dependencies.Add('fcl-passrc');
    Defaults.Options.Add('-Sc');

    P.Author := 'Free Pascal development team';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');
    P.SupportBuildModes := [bmOneByOne];

    P.Options.Add('-S2h');

    T:=P.Targets.AddUnit('pas2jsfiler.pp');
    T:=P.Targets.AddUnit('fppas2js.pp');
    T:=P.Targets.AddUnit('fppjssrcmap.pp');
    T:=P.Targets.AddUnit('pas2jsfilecache.pp');
    T:=P.Targets.AddUnit('pas2jsfileutils.pp');
    T.Dependencies.AddInclude('pas2js_defines.inc');
    T.Dependencies.AddInclude('pas2jsfileutilsunix.inc',AllUnixOSes);
    T.Dependencies.AddInclude('pas2jsfileutilswin.inc',AllWindowsOSes);
    T:=P.Targets.AddUnit('pas2jslogger.pp');
    T:=P.Targets.AddUnit('pas2jspparser.pp');
    T:=P.Targets.AddUnit('pas2jscompiler.pp');
    T:=P.Targets.AddUnit('pas2jslibcompiler.pp');
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
