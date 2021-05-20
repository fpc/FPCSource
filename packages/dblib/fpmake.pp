{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

{$endif not ALLPACKAGES}

procedure add_dblib(const ADirectory: string);

Const
  DBLibOSes         = [linux,freebsd,netbsd,openbsd,solaris,win32,win64,haiku,android,dragonfly,beos];

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
      P:=AddPackage('dblib');
      P.ShortName := 'dblb';
      P.Directory:=ADirectory;
      P.Version:='3.2.3';
      P.Author := 'Library: (FreeTDS/Microsoft), header: Ladislav Karrach';
      P.License := 'Library: FreeTDS License, header: LGPL with modification, ';
      P.HomepageURL := 'www.freepascal.org';
      P.Email := '';
      P.Description := 'Headers for the MS SQL Server RDBMS';
      P.NeedLibC:= true;  // true for headers that indirectly link to libc?

      P.SourcePath.Add('src');
      P.IncludePath.Add('src');

      P.OSes := DBLibOSes;
      if Defaults.CPU=jvm then
        P.OSes := P.OSes - [android];

      T:=P.Targets.AddUnit('dblib.pp',DBLibOSes);
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_dblib('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
