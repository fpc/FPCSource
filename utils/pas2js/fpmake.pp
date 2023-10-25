{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses 
  {$ifdef unix}
  cthreads,
  {$endif}
  fpmkunit;
{$endif ALLPACKAGES}

procedure add_pas2js(const ADirectory: string);

Const
  DefaultOSes = AllUnixOSes+AllBSDOSes+AllWindowsOSes-[WinCE];
  AllPas2JSOses = DefaultOSes+[wasi];

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
    P.NeedLibC:= false;
    P.ShortName:='p2js';

    P.Directory:=ADirectory;
    P.Version:='3.3.1';
    P.OSes:=AllPas2JSOses;
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];
    P.Dependencies.Add('fcl-json');
    P.Dependencies.Add('fcl-js');
    P.Dependencies.Add('fcl-passrc');
    P.Dependencies.Add('pastojs');
    P.Dependencies.Add('fcl-web',DefaultOSes);
    P.Dependencies.Add('webidl');
    PT:=P.Targets.AddProgram('pas2js.pp');
    PT:=P.Targets.AddLibrary('pas2jslib.pp');
    PT:=P.Targets.AddUnit('dirwatch.pp',DefaultOSes);
    PT:=P.Targets.AddUnit('httpcompiler.pp',DefaultOSes);
    PT.Dependencies.AddUnit('dirwatch');
    PT:=P.Targets.AddProgram('compileserver.pp',DefaultOSes);
    PT.Dependencies.AddUnit('httpcompiler');
    PT:=P.Targets.AddProgram('webidl2pas.pp');
    PT:=P.Targets.AddProgram('dts2pas.pp');
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_pas2js('');
  Installer.Run;
end.
{$endif ALLPACKAGES}




