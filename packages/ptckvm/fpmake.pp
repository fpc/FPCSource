{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('ptckvm');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';

    P.Author := 'Nikolay Nikolov';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'https://sourceforge.net/projects/fpterm/';
    P.Description := 'Driver for the video, keyboard and mouse units, built on top of the PTCPas graphics library.';
    p.OSes:=[linux,win32,win64,go32v2,macosx,openbsd,freebsd,netbsd];

    P.SourcePath.Add('src');

    P.Dependencies.Add('ptc');
    p.Dependencies.Add('rtl-console');

    T:=P.Targets.AddUnit('ptckvm.pas');

    P.NamespaceMap:='namespaces.lst';

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
