{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_dxegen(const ADirectory: string);

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('utils-dxegen');
    P.ShortName:='dxeg';

    P.Author := 'Charles Sandmann and others';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := 'sandmann@clio.rice.edu';
    P.Description := 'DXEGEN converts COFF object files to .DXE files that can be loaded and '+
                     'relocated runtime.';
    P.NeedLibC:= false;

    P.OSes:=[go32v2];

    P.Directory:=ADirectory;
    P.Version:='2.7.1';

    T:=P.Targets.AddProgram('dxegen.pp');
    T.Dependencies.AddUnit('coff');

    P.Targets.AddUnit('coff.pp').install:=false;
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_dxegen('');
  Installer.Run;
end.
{$endif ALLPACKAGES}




