{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_pas2ut(const ADirectory: string);

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('utils-pas2ut');

    P.Author := 'Free Pascal Team';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Description := 'Pascal source to FPC Unit test generator program';
    P.Email := '';
    P.NeedLibC:= false;

    P.Directory:=ADirectory;
    P.Version:='2.7.1';
    P.Dependencies.Add('fcl-passrc');

    T:=P.Targets.AddProgram('pas2ut.pp');
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_pas2ut('');
  Installer.Run;
end.
{$endif ALLPACKAGES}




