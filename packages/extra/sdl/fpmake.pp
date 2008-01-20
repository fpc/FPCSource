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

    P:=AddPackage('sdl');
{$ifdef ALLPACKAGES}
    P.Directory:='sdl';
{$endif ALLPACKAGES}
    P.Version:='1.2.12';
    T:=P.Targets.AddUnit('jedi-sdl.inc');
    T:=P.Targets.AddUnit('powersdl.inc');
    T:=P.Targets.AddUnit('sdl.pas');
    T:=P.Targets.AddUnit('sdl_gfx.pas');
    T:=P.Targets.AddUnit('sdl_image.pas');
    T:=P.Targets.AddUnit('sdl_mixer.pas');
    T:=P.Targets.AddUnit('sdl_ttf.pas');
    T:=P.Targets.AddUnit('sdl_net.pas');
    T:=P.Targets.AddUnit('sdl_mixer_nosmpeg.pas');
    T:=P.Targets.AddUnit('sdlutils.pas');
    T:=P.Targets.AddUnit('smpeg.pas');
    T:=P.Targets.AddUnit('logger.pas');
      T.OSes:=[linux,freebsd,win32,win64,darwin];

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
