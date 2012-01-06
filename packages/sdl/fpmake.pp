{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

const
  SDLUtilsOSes  = [linux,freebsd,win32,darwin,iphonesim];
  SDLUtilsProcs = [i386,powerpc];

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('sdl');
{$ifdef ALLPACKAGES}
    P.Directory:='sdl';
{$endif ALLPACKAGES}
    P.Version:='2.7.1';
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');
    P.Dependencies.Add('x11',SDLUtilsProcs,SDLUtilsOSes);
    P.Dependencies.Add('pthreads',SDLUtilsProcs,SDLUtilsOSes);

    T:=P.Targets.AddUnit('logger.pas');
      with T.Dependencies do
        begin
          AddInclude('jedi-sdl.inc');
        end;
    T:=P.Targets.AddUnit('sdl_gfx.pas');
      with T.Dependencies do
        begin
          AddInclude('jedi-sdl.inc');
          AddUnit('sdl');
        end;
    T:=P.Targets.AddUnit('sdl_image.pas');
      with T.Dependencies do
        begin
          AddInclude('jedi-sdl.inc');
          AddUnit('sdl');
        end;
    T:=P.Targets.AddUnit('sdl_mixer_nosmpeg.pas');
      with T.Dependencies do
        begin
          AddInclude('jedi-sdl.inc');
          AddUnit('sdl');
        end;
    T:=P.Targets.AddUnit('sdl_mixer.pas');
      with T.Dependencies do
        begin
          AddInclude('jedi-sdl.inc');
          AddUnit('smpeg');
          AddUnit('sdl');
        end;
    T:=P.Targets.AddUnit('sdl_net.pas');
      with T.Dependencies do
        begin
          AddInclude('jedi-sdl.inc');
          AddUnit('sdl');
        end;
    T:=P.Targets.AddUnit('sdl.pas');
      with T.Dependencies do
        begin
          AddInclude('jedi-sdl.inc');
        end;
    T:=P.Targets.AddUnit('sdl_ttf.pas');
      with T.Dependencies do
        begin
          AddInclude('jedi-sdl.inc');
          AddUnit('sdl');
        end;
    T:=P.Targets.AddUnit('sdlutils.pas',SDLUtilsProcs,SDLUtilsOSes);
      with T.Dependencies do
        begin
          AddInclude('jedi-sdl.inc');
          AddUnit('sdl');
        end;
    T:=P.Targets.AddUnit('smpeg.pas');
      with T.Dependencies do
        begin
          AddInclude('jedi-sdl.inc');
          AddUnit('sdl');
        end;
    P.Sources.AddSrc('LGPL');
    P.Sources.AddSrc('LGPL.addon');
    P.Sources.AddSrc('MPL-1.1');
    P.Sources.AddSrc('README.txt');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}




 
