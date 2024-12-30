{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses 
{$ifdef unix}
  cthreads,
{$endif}
  fpmkunit, sysutils;
{$endif ALLPACKAGES}

procedure add_openapi2pas(const ADirectory: string);

Var
  P : TPackage;
  T : TTarget;
  Bin2Obj : string;

begin
  With Installer do
    begin
    P:=AddPackage('utils-openapi2pas');
    P.ShortName:='oapi2p';
    P.Author := '<various>';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Free Pascal OpenAPI to pascal code generation utility.';
    P.NeedLibC:= false;

    P.OSes:=AllOSes-[embedded,msdos,win16,go32v2,nativent,macosclassic,palmos,atari,zxspectrum,msxdos,amstradcpc,watcom,sinclairql,wasi,human68k,ps1];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-json');
    P.Dependencies.Add('fcl-yaml');
    P.Dependencies.Add('fcl-openapi');
    P.Dependencies.Add('fcl-jsonschema');

    P.Directory:=ADirectory;
    P.Version:='3.3.1';

    P.Options.Add('-S2h');

    T:=P.Targets.AddProgram('openapi2pas.pp');
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_openapi2pas('');
  Installer.Run;
end.
{$endif ALLPACKAGES}




