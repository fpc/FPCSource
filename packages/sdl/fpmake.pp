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
    P.Version:='2.2.2-0';
    P.SourcePath.Add('src');
    P.Dependencies.Add('x11');
    P.Dependencies.Add('pthreads');

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
          AddUnit('pthreads');
          AddUnit('xlib');
        end;
    T:=P.Targets.AddUnit('sdl_ttf.pas');
      with T.Dependencies do
        begin
          AddInclude('jedi-sdl.inc');
          AddUnit('sdl');
        end;
    T:=P.Targets.AddUnit('sdlutils.pas');
      with T.Dependencies do
        begin
          AddInclude('jedi-sdl.inc');
          AddUnit('xlib');
          AddUnit('sdl');
        end;
    T:=P.Targets.AddUnit('smpeg.pas');
      with T.Dependencies do
        begin
          AddInclude('jedi-sdl.inc');
          AddUnit('sdl');
        end;

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
