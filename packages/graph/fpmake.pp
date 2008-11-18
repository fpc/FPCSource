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

    P:=AddPackage('graph');
{$ifdef ALLPACKAGES}
    P.Directory:='graph';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';
    P.Author := 'FPC team';
    P.License := 'LGPL with modification, ';
    P.ExternalURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'A portable, yet usable substitute for the Turbo Pascal Graph unit.';
    P.NeedLibC:= false;  // true for headers that indirectly link to libc? OS specific?

    P.CPUs:=[i386,powerpc];
    P.OSes:=[win32,linux,freebsd,darwin];

    P.Dependencies.Add('sdl',[i386,powerpc],[win32,linux,freebsd,darwin]);

    P.SourcePath.Add('src');
    P.SourcePath.Add('src/macosx',[darwin]);
    P.SourcePath.Add('src/amiga',[amiga]);
    P.SourcePath.Add('src/go32v2',[go32v2]);
    P.SourcePath.Add('src/win32',[win32,win64]);
    P.SourcePath.Add('src/unix',[freebsd,linux]);  // Darwin has own.

    P.IncludePath.Add('src/inc');
    P.IncludePath.Add('src/go32v2',[go32v2]);
    P.IncludePath.Add('src/unix',[freebsd,linux]);  // Darwin has own.
    P.IncludePath.Add('src/go32v2',[go32v2]);

    T:=P.Targets.AddUnit('ggigraph.pp',[linux,freebsd]);
      with T.Dependencies do
        begin
          AddInclude('graphh.inc');
          AddInclude('graph.inc');
          AddInclude('fontdata.inc');
          AddInclude('clip.inc');
          AddInclude('palette.inc');
          AddInclude('modes.inc');
          AddInclude('fills.inc');
          AddInclude('gtext.inc');
        end;
    T:=P.Targets.AddUnit('graph.pp');
      with T.Dependencies do
        begin
          AddInclude('graphh.inc');
          AddInclude('graph.inc');
          AddInclude('fontdata.inc');
          AddInclude('clip.inc');
          AddInclude('palette.inc');
          AddInclude('modes.inc');
          AddInclude('fills.inc');
          AddInclude('gtext.inc');
          AddInclude('graph16.inc',[freebsd,linux]);
        end;
    T:=P.Targets.AddUnit('src/sdlgraph/sdlgraph.pp',[i386,powerpc],[win32,linux,freebsd,darwin]);
      with T.Dependencies do
        begin
          AddInclude('graphh.inc');
          AddInclude('graph.inc');
          AddInclude('fontdata.inc');
          AddInclude('clip.inc');
          AddInclude('palette.inc');
          AddInclude('modes.inc');
          AddInclude('fills.inc');
          AddInclude('gtext.inc');
          AddUnit('sdl');
          AddUnit('sdlutils');
          AddUnit('logger');
        end;
    T:=P.Targets.AddUnit('wincrt.pp',[win32]);
      with T.Dependencies do
        begin
          AddUnit('graph');
        end;
    T:=P.Targets.AddUnit('winmouse.pp',[win32]);
      with T.Dependencies do
        begin
          AddUnit('graph');
        end;


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
