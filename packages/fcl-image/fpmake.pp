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

    P:=AddPackage('fcl-image');
{$ifdef ALLPACKAGES}
    P.Directory:='fcl-image';
{$endif ALLPACKAGES}
    P.Version:='2.2.2-0';

    P.Dependencies.Add('pasjpeg');
    P.Dependencies.Add('paszlib');
    P.Dependencies.Add('fcl-base');

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');

    T:=P.Targets.AddUnit('bmpcomn.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimgcmn');
        end;
    T:=P.Targets.AddUnit('clipping.pp');
    T:=P.Targets.AddUnit('ellipses.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('fpcanvas');
        end;
    T:=P.Targets.AddUnit('extinterpolation.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('fpcanvas');
        end;
    T:=P.Targets.AddUnit('fpcanvas.pp');
      with T.Dependencies do
        begin
          AddInclude('fphelper.inc');
          AddInclude('fpfont.inc');
          AddInclude('fppen.inc');
          AddInclude('fpbrush.inc');
          AddInclude('fpinterpolation.inc');
          AddInclude('fpcanvas.inc');
          AddInclude('fpcdrawh.inc');
          AddUnit('fpimage');
          AddUnit('clipping');
        end;
    T:=P.Targets.AddUnit('fpcolhash.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
        end;
    T:=P.Targets.AddUnit('fpditherer.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('fpcolhash');
        end;
    T:=P.Targets.AddUnit('fpimage.pp');
      with T.Dependencies do
        begin
          AddInclude('fpcolors.inc');
          AddInclude('fpimage.inc');
          AddInclude('fphandler.inc');
          AddInclude('fppalette.inc');
          AddInclude('fpcolcnv.inc');
        end;
    T:=P.Targets.AddUnit('fpimgcanv.pp');
      with T.Dependencies do
        begin
          AddUnit('fppixlcanv');
          AddUnit('fpimage');
          AddUnit('clipping');
        end;
    T:=P.Targets.AddUnit('fpimgcmn.pp');
    T:=P.Targets.AddUnit('fppixlcanv.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('fpcanvas');
          AddUnit('pixtools');
          AddUnit('ellipses');
          AddUnit('clipping');
        end;
    T:=P.Targets.AddUnit('fpquantizer.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('fpcolhash');
        end;
    T:=P.Targets.AddUnit('fpreadbmp.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('bmpcomn');
        end;
    T:=P.Targets.AddUnit('fpreadjpeg.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
        end;
    T:=P.Targets.AddUnit('fpreadpcx.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('pcxcomn');
        end;
    T:=P.Targets.AddUnit('fpreadpng.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('fpimgcmn');
          AddUnit('pngcomn');
        end;
    T:=P.Targets.AddUnit('fpreadpnm.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
        end;
    T:=P.Targets.AddUnit('fpreadtga.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('targacmn');
        end;
    T:=P.Targets.AddUnit('fpreadxpm.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
        end;
    T:=P.Targets.AddUnit('fpwritebmp.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('bmpcomn');
        end;
    T:=P.Targets.AddUnit('fpwritejpeg.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('fpreadjpeg');
        end;
    T:=P.Targets.AddUnit('fpwritepcx.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('pcxcomn');
        end;
    T:=P.Targets.AddUnit('fpwritepng.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('fpimgcmn');
          AddUnit('pngcomn');
        end;
    T:=P.Targets.AddUnit('fpwritepnm.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
        end;
    T:=P.Targets.AddUnit('fpwritetga.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('targacmn');
        end;
    T:=P.Targets.AddUnit('fpwritexpm.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
        end;
    T:=P.Targets.AddUnit('freetypeh.pp');
    T:=P.Targets.AddUnit('freetype.pp');
      with T.Dependencies do
        begin
          AddUnit('freetypeh');
          AddUnit('fpimgcmn');
        end;
    T:=P.Targets.AddUnit('ftfont.pp');
      with T.Dependencies do
        begin
          AddUnit('fpcanvas');
          AddUnit('fpimgcmn');
          AddUnit('freetype');
          AddUnit('freetypeh');
          AddUnit('fpimage');
        end;
    T:=P.Targets.AddUnit('pcxcomn.pas');
    T:=P.Targets.AddUnit('pixtools.pp');
      with T.Dependencies do
        begin
          AddUnit('fpcanvas');
          AddUnit('fpimage');
          AddUnit('clipping');
          AddUnit('ellipses');
        end;
    T:=P.Targets.AddUnit('pngcomn.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('fpimgcmn');
        end;
    T:=P.Targets.AddUnit('pscanvas.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('fpcanvas');
        end;
    T:=P.Targets.AddUnit('targacmn.pp');

    P.ExamplePath.Add('examples');
    T:=P.Targets.AddExampleProgram('drawing.pp');
    T:=P.Targets.AddExampleProgram('imgconv.pp');

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}

