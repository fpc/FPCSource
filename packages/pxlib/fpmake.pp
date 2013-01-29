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
    P:=AddPackage('pxlib');
{$ifdef ALLPACKAGES}
    P.Directory:='pxlib';
{$endif ALLPACKAGES}
    P.OSes:=[Linux,beos,win32,darwin,freebsd,openbsd,netbsd];
    P.Version:='2.6.2';
    T:=P.Targets.AddUnit('src/pxlib.pp');
      T.OSes:=[Linux,beos,win32,darwin,freebsd,openbsd,netbsd];
    T:=P.Targets.AddExampleunit('examples/ppxview.pp');
      T.OSes:=[Linux,beos,win32,darwin,freebsd,openbsd,netbsd];
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
