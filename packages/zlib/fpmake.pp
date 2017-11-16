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

    P:=AddPackage('zlib');
    P.Description := 'Interface units for the ZLIB library - support for deflate compression method used for GZIP, PNG, ZIP, etc.';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.0.5';
    P.OSes := AllUnixOSes+AllWindowsOSes+[os2,emx,netware,netwlibc]-[qnx];
    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('zlib.pp');


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
