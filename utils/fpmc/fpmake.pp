{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit, sysutils;
{$endif ALLPACKAGES}

procedure add_fpmc(const ADirectory: string);

Var
  P : TPackage;
  T : TTarget;
  gtkOSes: TOSes;
  gtkdll: string;

begin
  With Installer do
    begin
    P:=AddPackage('utils-fpmc');

    P.Author := '<various>';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Free Pascal Message Compiler.';
    P.NeedLibC:= false;

    P.Directory:=ADirectory;
    P.Version:='2.7.1';

    P.OSes := [win32, win64, os2, emx];

    gtkOSes:=[linux];
    // Only compile fpmcgtk on win32 when the gtk-library is found in the path
    gtkdll:=FileSearch('libgtk-0.dll',GetEnvironmentVariable('PATH'),[sfoImplicitCurrentDir, sfoStripQuotes]);
    if gtkdll<>'' then
      include(gtkOSes,win32);

    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fpgtk',gtkOSes);

    T:=P.Targets.AddProgram('fpmc.pp');
    T.Dependencies.AddUnit('msgcomp');

    T:=P.Targets.AddProgram('fpmcgtk.pp',gtkOSes);
    T.Dependencies.AddUnit('msgcomp');
    T.Dependencies.AddUnit('frmmain', gtkOSes);
    T.Dependencies.AddUnit('frmabout', gtkOSes);
    T.Dependencies.AddUnit('frmoptions', gtkOSes);

    T:=P.Targets.AddUnit('frmmain.pp', gtkOSes);
    T.install:=false;
    T.ResourceStrings:=true;

    T:=P.Targets.AddUnit('frmabout.pp', gtkOSes);
    T.install:=false;
    T.ResourceStrings:=true;

    T:=P.Targets.AddUnit('frmoptions.pp', gtkOSes);
    T.install:=false;
    T.ResourceStrings:=true;

    T:=P.Targets.AddUnit('msgcomp.pp');
    T.install:=false;
    T.ResourceStrings:=true;
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_fpmc;
  Installer.Run;
end.
{$endif ALLPACKAGES}




