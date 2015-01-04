{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_pas2js(const ADirectory: string);

Var
  P : TPackage;
  T : TTarget;

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

    P.Directory:=ADirectory;
    P.Version:='2.7.1';
    P.Dependencies.Add('fcl-js');
    P.Dependencies.Add('fcl-passrc');
    P.Dependencies.Add('pastojs');

    T:=P.Targets.AddProgram('pas2js.pp');
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_pas2js('');
  Installer.Run;
end.
{$endif ALLPACKAGES}




