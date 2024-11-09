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
      P:=AddPackage('dblib');
      P.ShortName := 'dblb';
      P.Directory:=ADirectory;
      P.Version:='3.3.1';
      P.Author := 'Library: (FreeTDS/Microsoft), header: Ladislav Karrach';
      P.License := 'Library: FreeTDS License, header: LGPL with modification, ';
      P.HomepageURL := 'www.freepascal.org';
      P.Email := '';
      P.Description := 'Headers for the MS SQL Server RDBMS';
      P.NeedLibC:= true;  // true for headers that indirectly link to libc?

      P.SourcePath.Add('src');
      P.IncludePath.Add('src');

      P.OSes := [linux,freebsd,netbsd,openbsd,solaris,win32,win64,haiku,android,dragonfly,beos];
      if Defaults.CPU=jvm then
        P.OSes := P.OSes - [android];

      T:=P.Targets.AddUnit('dblib.pp',P.OSes);

      P.NamespaceMap:='namespaces.lst';

{$ifndef ALLPACKAGES}
      Run;
    end;
end.
{$endif ALLPACKAGES}
