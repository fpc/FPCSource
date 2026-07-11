{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses
{$ifdef unix}
  cthreads,
{$endif}
  fpmkunit, sysutils;
{$endif ALLPACKAGES}

procedure add_fpsonar(const ADirectory: string);

Var
  P : TPackage;
  T : TTarget;
  Bin2Obj : string;

begin
  With Installer do
    begin
    P:=AddPackage('utils-fpsonar');
    P.ShortName:='fpdc';
    P.Author := '<various>';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Free Pascal documentation generation utility.';
    P.NeedLibC:= false;

    P.OSes:=AllOSes-[embedded,msdos,win16,go32v2,nativent,macosclassic,palmos,atari,zxspectrum,msxdos,amstradcpc,watcom,sinclairql,wasip1,wasip1threads,wasip2,human68k,ps1];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-xml');
    P.Dependencies.Add('fcl-passrc');
    P.Dependencies.Add('fcl-process');
    P.Dependencies.Add('fcl-sonar');

    P.Directory:=ADirectory;
    P.Version:='3.3.1';

    P.Options.Add('-S2h');

    P.Targets.AddUnit('fpsonar.cli.options.pp');
    T:=P.Targets.AddProgram('fpsonar.pp');
    T.Dependencies.AddUnit('fpsonar.cli.options');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_fpsonar('');
  Installer.Run;
end.
{$endif ALLPACKAGES}




