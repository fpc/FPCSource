{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  T : TTarget;
  P : TPackage;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=StartPackage('paszlib');
{$ifdef ALLPACKAGES}
    P.Directory:='paszlib';
{$endif ALLPACKAGES}
    P.Version:='2.2.0';
    P.Dependencies.Add('hash');
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');
    T:=Targets.AddUnit('paszlib.pas');
      T.Dependencies.AddUnit('adler');
      T.Dependencies.AddUnit('gzio');
      T.Dependencies.AddUnit('infblock');
      T.Dependencies.AddUnit('infcodes');
      T.Dependencies.AddUnit('inffast');
      T.Dependencies.AddUnit('inftrees');
      T.Dependencies.AddUnit('infutil');
      T.Dependencies.AddUnit('trees');
      T.Dependencies.AddUnit('zbase');
      T.Dependencies.AddUnit('zcompres');
      T.Dependencies.AddUnit('zdeflate');
      T.Dependencies.AddUnit('zinflate');
      T.Dependencies.AddUnit('zuncompr');
    T:=Targets.AddUnit('zip.pas');
      T.Dependencies.AddUnit('paszlib');
      T.Dependencies.AddUnit('ziputils');
    T:=Targets.AddUnit('unzip.pas');
      T.Dependencies.AddUnit('paszlib');
      T.Dependencies.AddUnit('ziputils');
    T:=Targets.AddUnit('zipper.pp');
      T.Dependencies.AddUnit('paszlib');
    T:=Targets.AddImplicitUnit('adler.pas');
      T.Dependencies.AddInclude('zconf.inc');
    T:=Targets.AddImplicitUnit('gzio.pas');
    T:=Targets.AddImplicitUnit('infblock.pas');
    T:=Targets.AddImplicitUnit('infcodes.pas');
    T:=Targets.AddImplicitUnit('inffast.pas');
    T:=Targets.AddImplicitUnit('inftrees.pas');
    T:=Targets.AddImplicitUnit('infutil.pas');
    T:=Targets.AddImplicitUnit('trees.pas');
    T:=Targets.AddImplicitUnit('zbase.pas');
    T:=Targets.AddImplicitUnit('zcompres.pas');
    T:=Targets.AddImplicitUnit('zdeflate.pas');
    T:=Targets.AddImplicitUnit('zinflate.pas');
    T:=Targets.AddImplicitUnit('zuncompr.pas');
    T:=Targets.AddImplicitUnit('ziputils.pas');
    T:=Targets.AddImplicitUnit('zstream.pp');
    T:=Targets.AddExampleProgram('examples/example.pas');
    T:=Targets.AddExampleProgram('examples/minigzip.pas');
    T:=Targets.AddExampleProgram('examples/miniunz.pas');
    T:=Targets.AddExampleProgram('examples/minizip.pas');
    EndPackage;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}

