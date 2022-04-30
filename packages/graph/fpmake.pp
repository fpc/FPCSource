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
    P.ShortName:='grph';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Author := 'FPC team';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'A portable, yet usable substitute for the Turbo Pascal Graph unit.';
    P.NeedLibC:= false;  // true for headers that indirectly link to libc? OS specific?

    P.CPUs:=[i386,x86_64,powerpc,i8086,aarch64];
    P.OSes:=[go32v2,win32,win64,linux,freebsd,openbsd,darwin,msdos];

    P.Dependencies.Add('sdl',[i386,powerpc],[win32,linux,freebsd,darwin]);
    P.Dependencies.Add('ptc',[win32,win64,linux,darwin,openbsd,freebsd]);

    // Dependencies for ptc, due to fpcmake bug:
    P.Dependencies.Add('fcl-base',[win32,win64,linux,openbsd,freebsd,darwin]);
    P.Dependencies.Add('x11',[freebsd,openbsd,linux]); // ptc only depends on bsd and linux on x11
    P.Dependencies.Add('hermes',[win32,win64,linux,openbsd,freebsd,darwin]);
    P.Dependencies.Add('opengl',[win32,win64,linux,openbsd,freebsd]);

    P.SourcePath.Add('src');
    P.SourcePath.Add('src/ptcgraph');
    P.SourcePath.Add('src/macosx',[darwin]);
    P.SourcePath.Add('src/amiga',[amiga]);
    P.SourcePath.Add('src/go32v2',[go32v2]);
    P.SourcePath.Add('src/msdos',[msdos]);
    P.SourcePath.Add('src/win32',[win32,win64]);
    P.SourcePath.Add('src/unix',[freebsd,linux,openbsd]);  // Darwin has own.

    P.IncludePath.Add('src/inc');
    P.IncludePath.Add('src/go32v2',[go32v2]);
    P.IncludePath.Add('src/msdos',[msdos]);
    P.IncludePath.Add('src/unix',[freebsd,linux,openbsd]);  // Darwin has own.
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
    // Graph unit, restricted to i8086, i386 and x86_64 CPUs 
    T:=P.Targets.AddUnit('graph.pp',[i8086,i386,x86_64],[go32v2,freebsd,linux,msdos]);
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
          AddInclude('vesa.inc',[go32v2,msdos]);
          AddInclude('vesah.inc',[go32v2,msdos]);
        end;

    // Graph unit, for win32 and win64 target 
    T:=P.Targets.AddUnit('graph.pp',[win32,win64]);
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
        end;
    T:=P.Targets.AddUnit('wincrt.pp',[win32, win64]);
      with T.Dependencies do
        begin
          AddUnit('graph');
        end;
    T:=P.Targets.AddUnit('winmouse.pp',[win32, win64]);
      with T.Dependencies do
        begin
          AddUnit('graph');
        end;
    T:=P.Targets.AddUnit('ptcgraph.pp',[win32,win64,linux,openbsd,freebsd,darwin]);
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
    T:=P.Targets.AddUnit('ptccrt.pp',[win32,win64,linux,openbsd,freebsd,darwin]);
      with T.Dependencies do
        begin
          AddUnit('ptcgraph');
        end;
    T:=P.Targets.AddUnit('ptcmouse.pp',[win32,win64,linux,openbsd,freebsd,darwin]);
      with T.Dependencies do
        begin
          AddUnit('ptcgraph');
        end;


{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
