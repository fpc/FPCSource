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

    P:=AddPackage('ptc');
{$ifdef ALLPACKAGES}
    P.Directory:='ptc';
{$endif ALLPACKAGES}
    P.Version:='2.7.1';

    P.Author := 'Nikolay Nikolov, Glenn Fiedler, Christian Nentwich';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'http://ptcpas.sourceforge.net/';
    P.Description := 'A free, portable framebuffer library.';
    p.OSes:=[linux,win32,win64];

    P.SourcePath.Add('src');
    P.SourcePath.Add('src/ptcwrapper');
    P.SourcePath.Add('src/win32/directx', [win32, win64]);
    P.IncludePath.Add('src');
    P.IncludePath.Add('src/core');
    P.IncludePath.Add('src/dos',[go32v2]);
    P.IncludePath.Add('src/dos/base',[go32v2]);
    P.SourcePath.Add('src/dos/cga',[go32v2]);
    P.SourcePath.Add('src/dos/fakemode',[go32v2]);
    P.SourcePath.Add('src/dos/textfx2',[go32v2]);
    P.SourcePath.Add('src/dos/timeunit',[go32v2]);
    P.SourcePath.Add('src/dos/vesa',[go32v2]);
    P.IncludePath.Add('src/dos/cga',[go32v2]);
    P.IncludePath.Add('src/dos/fakemode',[go32v2]);
    P.IncludePath.Add('src/dos/textfx2',[go32v2]);
    P.IncludePath.Add('src/dos/timeunit',[go32v2]);
    P.IncludePath.Add('src/dos/vesa',[go32v2]);
    P.IncludePath.Add('src/win32',[win32,win64]);
    P.IncludePath.Add('src/win32/base', [win32, win64]);
    P.IncludePath.Add('src/win32/directx', [win32, win64]);
    P.IncludePath.Add('src/win32/gdi', [win32, win64]);
    P.IncludePath.Add('src/wince',[wince]);
    P.IncludePath.Add('src/x11',AllUnixOSes);

  P.Dependencies.Add('hermes');
  P.Dependencies.Add('x11',AllUnixOSes);
  P.Dependencies.Add('opengl',AllUnixOSes + [win32, win64]);
  P.Dependencies.Add('fcl-base');

  T:=P.Targets.AddUnit('p_ddraw.pp', [win32, win64]);

  T:=P.Targets.AddUnit('ptc.pp');
  with T.Dependencies do
    begin
      AddInclude('aread.inc');
      AddInclude('areai.inc');
      AddInclude('baseconsoled.inc');
      AddInclude('baseconsolei.inc');
      AddInclude('basesurfaced.inc');
      AddInclude('basesurfacei.inc');
      AddInclude('cleard.inc');
      AddInclude('cleari.inc');
      AddInclude('clipperd.inc');
      AddInclude('clipperi.inc');
      AddInclude('colord.inc');
      AddInclude('colori.inc');
      AddInclude('consoled.inc');
      AddInclude('consolei.inc');
      AddInclude('copyd.inc');
      AddInclude('copyi.inc');
      AddInclude('coreimplementation.inc');
      AddInclude('coreinterface.inc');
      AddInclude('errord.inc');
      AddInclude('errori.inc');
      AddInclude('eventd.inc');
      AddInclude('eventi.inc');
      AddInclude('formatd.inc');
      AddInclude('formati.inc');
      AddInclude('keyeventd.inc');
      AddInclude('keyeventi.inc');
      AddInclude('log.inc');
      AddInclude('moded.inc');
      AddInclude('modei.inc');
      AddInclude('mouseeventd.inc');
      AddInclude('mouseeventi.inc');
      AddInclude('openglattributesd.inc');
      AddInclude('openglattributesi.inc');
      AddInclude('paletted.inc');
      AddInclude('palettei.inc');
      AddInclude('surfaced.inc');
      AddInclude('surfacei.inc');
      AddInclude('timerd.inc');
      AddInclude('timeri.inc');
      AddInclude('includes.inc',allunixoses+[WinCE]);
      AddInclude('extensions.inc',allunixoses);
      AddInclude('x11modesd.inc',allunixoses);
      AddInclude('x11imaged.inc',allunixoses);
      AddInclude('x11displayd.inc',allunixoses);
      AddInclude('x11windowdisplayd.inc',allunixoses);
      AddInclude('x11dga1displayd.inc',allunixoses);
      AddInclude('x11dga2displayd.inc',allunixoses);
      AddInclude('x11consoled.inc',allunixoses);
      AddInclude('check.inc',allunixoses + [win32, win64]);
      AddInclude('x11modesi.inc',allunixoses);
      AddInclude('x11imagei.inc',allunixoses);
      AddInclude('x11displayi.inc',allunixoses);
      AddInclude('xunikey.inc',allunixoses);
      AddInclude('x11windowdisplayi.inc',allunixoses);
      AddInclude('x11dga1displayi.inc',allunixoses);
      AddInclude('x11dga2displayi.inc',allunixoses);
      AddInclude('x11consolei.inc',allunixoses);
      AddInclude('glxfbconfigd.inc',allunixoses);
      AddInclude('glxfbconfigi.inc',allunixoses);
      AddInclude('cursor.inc', [win32, win64]);
      AddInclude('cursord.inc', [win32, win64]);
      AddInclude('cursormoded.inc', [win32, win64]);
      AddInclude('event.inc', [win32, win64]);
      //AddInclude('eventd.inc', [win32, win64]);
      AddInclude('hook.inc', [win32, win64]);
      AddInclude('hookd.inc', [win32, win64]);
      AddInclude('kbd.inc', [win32, win64]);
      AddInclude('kbdd.inc', [win32, win64]);
      AddInclude('monitor.inc', [win32, win64]);
      AddInclude('monitord.inc', [win32, win64]);
      AddInclude('moused.inc', [win32, win64]);
      AddInclude('mousei.inc', [win32, win64]);
      AddInclude('window.inc', [win32, win64]);
      AddInclude('windowd.inc', [win32, win64]);
      AddInclude('directxconsoled.inc', [win32, win64]);
      AddInclude('directxconsolei.inc', [win32, win64]);
      AddInclude('display.inc', [win32, win64]);
      AddInclude('displayd.inc', [win32, win64]);
      AddInclude('hook.inc', [win32, win64]);
      AddInclude('hookd.inc', [win32, win64]);
      AddInclude('library.inc', [win32, win64]);
      AddInclude('libraryd.inc', [win32, win64]);
      AddInclude('primary.inc', [win32, win64]);
      AddInclude('primaryd.inc', [win32, win64]);
      AddInclude('translate.inc', [win32, win64]);
      AddInclude('gdiconsoled.inc', [win32, win64]);
      AddInclude('gdiconsolei.inc', [win32, win64]);
      AddInclude('gdihookd.inc', [win32, win64]);
      AddInclude('gdihooki.inc', [win32, win64]);
      AddInclude('win32dibd.inc', [win32, win64]);
      AddInclude('win32dibi.inc', [win32, win64]);
      AddInclude('win32modesetterd.inc', [win32, win64]);
      AddInclude('win32modesetteri.inc', [win32, win64]);
      AddInclude('win32openglwindowd.inc', [win32, win64]);
      AddInclude('win32openglwindowi.inc', [win32, win64]);
      AddUnit('p_gx',[Wince]);
      AddUnit('textfx2',[Go32v2]);
      AddUnit('cga',[Go32v2]);
      AddUnit('timeunit',[Go32v2]);
      AddUnit('vesa',[Go32v2]);
      AddUnit('vga',[Go32v2]);
     end;
    T:=P.Targets.AddUnit('ptceventqueue.pp');
    with T.Dependencies do
      begin
        AddUnit('ptc');
      end;
    T:=P.Targets.AddUnit('ptcwrapper.pp');
    with T.Dependencies do
      begin
        AddUnit('ptc');
        AddUnit('ptceventqueue');
      end;

    P.ExamplePath.Add('examples/');
    P.Targets.AddExampleProgram('random.pp');
    P.Targets.AddExampleProgram('palette.pp');
    P.Targets.AddExampleProgram('pixel.pp');
    P.Targets.AddExampleProgram('console.pp');
    P.Targets.AddExampleProgram('image.pp');
    P.Targets.AddExampleProgram('flower.pp');
    P.Targets.AddExampleProgram('save.pp');
    P.Targets.AddExampleProgram('hicolor.pp');
    P.Targets.AddExampleProgram('stretch.pp');
    P.Targets.AddExampleProgram('tunnel.pp');
    P.Targets.AddExampleProgram('modes.pp');
    P.Targets.AddExampleProgram('buffer.pp');
    P.Targets.AddExampleProgram('texwarp.pp');
    P.Targets.AddExampleProgram('keyboard.pp');
    P.Targets.AddExampleProgram('timer.pp');
    P.Targets.AddExampleProgram('clip.pp');
    P.Targets.AddExampleProgram('lights.pp');
    P.Targets.AddExampleProgram('fire.pp');
    P.Targets.AddExampleProgram('mojo.pp');
    P.Targets.AddExampleProgram('land.pp');
    P.Targets.AddExampleProgram('keyboard2.pp');
    P.Targets.AddExampleProgram('keyboard3.pp');
    P.Targets.AddExampleProgram('clear.pp');
    P.Targets.AddExampleProgram('con_info.pp');
    P.Targets.AddExampleProgram('area.pp');
    P.Targets.AddExampleProgram('tunnel3d.pp');
    P.Targets.AddExampleProgram('ptcgl.pp', AllUnixOSes + [win32, win64]);
    P.Targets.AddExampleProgram('ptcgl2.pp', AllUnixOSes + [win32, win64]);
    P.Sources.AddExampleFiles('examples/*',false,'.');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}
