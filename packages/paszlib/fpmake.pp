{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  T : TTarget;

begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    StartPackage('paszlib');
{$ifdef ALLPACKAGES}
    Directory:='paszlib';
{$endif ALLPACKAGES}
    Version:='2.2.0';
    Dependencies.Add('hash');
    T:=Targets.AddUnit('src/paszlib.pas');
    T:=Targets.AddUnit('src/zip.pas');
    T:=Targets.AddUnit('src/unzip.pas');
    T:=Targets.AddUnit('src/zipper.pp');
    T:=Targets.AddImplicitUnit('src/adler.pas');
    T:=Targets.AddImplicitUnit('src/gzio.pas');
    T:=Targets.AddImplicitUnit('src/infblock.pas');
    T:=Targets.AddImplicitUnit('src/infcodes.pas');
    T:=Targets.AddImplicitUnit('src/inffast.pas');
    T:=Targets.AddImplicitUnit('src/inftrees.pas');
    T:=Targets.AddImplicitUnit('src/infutil.pas');
    T:=Targets.AddImplicitUnit('src/trees.pas');
    T:=Targets.AddImplicitUnit('src/zcompres.pas');
    T:=Targets.AddImplicitUnit('src/zdeflate.pas');
    T:=Targets.AddImplicitUnit('src/zinflate.pas');
    T:=Targets.AddImplicitUnit('src/zbase.pas');
    T:=Targets.AddImplicitUnit('src/zuncompr.pas');
    T:=Targets.AddImplicitUnit('src/ziputils.pas');
    T:=Targets.AddImplicitUnit('src/zstream.pp');
    T:=Targets.AddExampleProgram('tests/example.pas');
    T:=Targets.AddExampleProgram('tests/minigzip.pas');
    T:=Targets.AddExampleProgram('tests/miniunz.pas');
    T:=Targets.AddExampleProgram('tests/minizip.pas');
    EndPackage;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}

