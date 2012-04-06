{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_fpcm;

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('fpcm');

    P.Author := '<various>';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Tool to generate Makefile''s out of Makefile.fpc files';
    P.NeedLibC:= false;

{$ifdef ALLPACKAGES}
    P.Directory:='fpcm';
{$endif ALLPACKAGES}
    P.Version:='2.7.1';

    P.Dependencies.Add('fcl-base');

    T:=P.Targets.AddProgram('fpcmake.pp');

    T:=P.Targets.AddUnit('fpcmmain.pp');
    T.install:=false;
    T.ResourceStrings:=true;
    P.Targets.AddUnit('fpcmdic.pp').install:=false;
    P.Targets.AddUnit('fpcmwr.pp').install:=false;
    P.Targets.AddUnit('fpcmpkg.pp').install:=false;

    P.Sources.AddSrc('fpcmake.ini');
    P.Sources.AddSrc('fpcmake.inc');
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_fpcm;
  Installer.Run;
end.
{$endif ALLPACKAGES}




