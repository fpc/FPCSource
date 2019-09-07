{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('vcl-compat');
    P.ShortName:='vclcomp';
    P.Author := 'Michael Van Canneyt';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Various non-visual VCL compatibility units.';
    P.OSes := P.OSes - [embedded,palmos,atari,msdos,go32v2];

{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-xml');
    P.Dependencies.Add('fcl-web');
    P.Dependencies.Add('rtl-extra'); 

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');


    T:=P.Targets.AddUnit('System.NetEncoding.pp');
    T.ResourceStrings := True;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
