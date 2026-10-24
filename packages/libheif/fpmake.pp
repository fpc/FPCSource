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

    P:=AddPackage('libheif');
    P.ShortName:='lheif';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Author := 'Library: libheif';
    P.License := 'Library: LGPL, header: LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Headers for the libheif library (library to read HEIF/HEIC graphic formats)';
    P.NeedLibC:= true;  // true for headers that indirectly link to libc?
    P.OSes := [darwin,win32,win64,linux,freebsd,netbsd,openbsd];
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('libheif.pp');

    P.NamespaceMap:='namespaces.lst';

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
