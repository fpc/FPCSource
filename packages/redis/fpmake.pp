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

    P:=AddPackage('redis');
    P.ShortName:='redis';
    P.Author := 'Mario Ray Mahardhika';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Redis interface unit.';
    // Keep this lis the same as fcl-net.
    P.OSes:=AllUnixOSes+AllWindowsOSes+AllAmigaLikeOSes+[OS2,EMX];

{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Dependencies.Add('fcl-net');

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('redis.pp');
    T.ResourceStrings := True;

    P.NamespaceMap:='namespaces.lst';
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
