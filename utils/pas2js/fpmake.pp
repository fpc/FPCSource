{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_pas2js(const ADirectory: string);

Var
  P : TPackage;
  PT,T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('utils-pas2js');

    P.Author := 'Free Pascal Team';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Description := 'Convert pascal sources to javascript.';
    P.Email := 'michael@freepascal.org';
    Defaults.Options.Add('-Sc');
    P.NeedLibC:= false;

    P.Directory:=ADirectory;
    P.Version:='3.1.1';
    P.Dependencies.Add('fcl-js');
    P.Dependencies.Add('fcl-passrc');
    P.Dependencies.Add('pastojs');
    T:=P.Targets.AddUnit('pas2jscompiler.pp');
    T:=P.Targets.AddUnit('pas2jsfilecache.pp');
    T:=P.Targets.AddUnit('pas2jsfileutils.pp');
    T.Dependencies.AddInclude('pas2jsfileutilsunix.inc',AllUnixOSes);
    T.Dependencies.AddInclude('pas2jsfileutilswin.inc',AllWindowsOSes);
    T:=P.Targets.AddUnit('pas2jslogger.pp');
    T:=P.Targets.AddUnit('pas2jspparser.pp');
    PT:=P.Targets.AddProgram('pas2js.pp');
    PT.Dependencies.AddUnit('pas2jscompiler');
    PT.Dependencies.AddUnit('pas2jsfileutils');
    PT.Dependencies.AddUnit('pas2jsfilecache');
    PT.Dependencies.AddUnit('pas2jslogger');
    PT.Dependencies.AddUnit('pas2jspparser');
    PT:=P.Targets.AddLibrary('pas2jslib.pp');
    PT.Dependencies.AddUnit('pas2jscompiler');
    PT.Dependencies.AddUnit('pas2jsfileutils');
    PT.Dependencies.AddUnit('pas2jsfilecache');
    PT.Dependencies.AddUnit('pas2jslogger');
    PT.Dependencies.AddUnit('pas2jspparser');
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_pas2js('');
  Installer.Run;
end.
{$endif ALLPACKAGES}




