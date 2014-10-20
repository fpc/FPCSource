{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_rmwait(const ADirectory: string);

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('rmwait');

    P.Author := 'Tomas Hajny';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Tool to remove (delete) file(s) with optional retries';
    P.NeedLibC:= false;

    P.SeparateArchive:=false;
    P.Directory:=ADirectory;
    P.Version:='2.7.1';

    P.OSes:=[win32,win64,wince,os2,emx,go32v2];

    T:=P.Targets.AddProgram('rmwait.pas');
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_rmwait;
  Installer.Run;
end.
{$endif ALLPACKAGES}




