{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Const
  DBLibOSes         = [linux,freebsd,netbsd,openbsd,win32,win64,haiku];
  
Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('dblib');
{$ifdef ALLPACKAGES}
    P.Directory:='dblib';
{$endif ALLPACKAGES}
    P.Version:='1.0';
    P.Author := 'Library: (FreeTDS/Microsoft), header: Lacack2';
    P.License := 'Library: FreeTDS License, header: LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Headers for the MS SQL Server RDBMS';
    P.NeedLibC:= true;  // true for headers that indirectly link to libc?

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('dblib.pp',DBLibOSes);

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
