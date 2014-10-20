{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_pas2fpm(const ADirectory: string);

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('pas2fpm');

    P.Author := 'Free Pascal Team';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Description := 'Generate fpmake.pp for Pascal source.';
    P.Email := '';
    P.NeedLibC:= false;
    P.SeparateArchive:=false;

    P.Directory:=ADirectory;
    P.Version:='2.7.1';
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-passrc');

    T:=P.Targets.AddProgram('pas2fpm.pp');
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_pas2fpm('');
  Installer.Run;
end.
{$endif ALLPACKAGES}




