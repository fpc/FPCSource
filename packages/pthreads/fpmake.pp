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

    P:=AddPackage('pthreads');
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.2.0-beta';
    P.OSes := [beos,haiku,freebsd,darwin,iphonesim,solaris,netbsd,openbsd,linux,aix,dragonfly,android];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('pthreads.pp');
    with T.Dependencies do
      begin
        AddInclude('pthrandroid.inc',[Android]);
        AddInclude('pthrlinux.inc',[Linux]);
        AddInclude('pthrbeos.inc',[Beos]);
        AddInclude('pthrsnos.inc',[Solaris]);
        AddInclude('pthrbsd.inc',AllBSDOses);
        AddInclude('pthraix.inc',[AIX]);
      end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
