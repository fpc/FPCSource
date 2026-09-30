{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;

Var
  T : TTarget;
  P : TPackage;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('fcl-image');
    P.ShortName:='fcli';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Dependencies.Add('pasjpeg');
    P.Dependencies.Add('paszlib');
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('libheif', [darwin,win32,win64,linux,freebsd,netbsd,openbsd]);

    P.Author := 'Michael Van Canneyt of the Free Pascal development team';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Image loading and conversion parts of Free Component Libraries (FCL), FPC''s OOP library.';
    P.NeedLibC:= false;
    P.OSes := P.OSes - [embedded,nativent,msdos,win16,macosclassic,palmos,zxspectrum,msxdos,amstradcpc,sinclairql,human68k,ps1,wasip2];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.SourcePath.Add('src');
    P.IncludePath.Add('src');
    T:=P.Targets.AddUnit('bmpcomn.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimgcmn');
        end;
    T:=P.Targets.AddUnit('fptiffcmn.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
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
    T:=P.Targets.AddUnit('polygonfilltools.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('fpcanvas');
          AddUnit('pixtools');
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
          AddInclude('fpcompactimg.inc');
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
          AddUnit('polygonfilltools');
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
    T:=P.Targets.AddUnit('jpegcomn.pas');
    T:=P.Targets.AddUnit('fpreadjpeg.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          Addunit('jpegcomn');
          AddUnit('fpimgexif');
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
          AddUnit('fpimagelist');
          AddUnit('fpimgexif');
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
    T:=P.Targets.AddUnit('fpreadtiff.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('fptiffcmn');
        end;
    T:=P.Targets.AddUnit('fpreadxpm.pp');
      with T.Dependencies do
        begin
          AddInclude('x11colors.inc');
          AddUnit('fpimage');
        end;
    T:=P.Targets.AddUnit('fpreadgif.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
        end;
    T:=P.Targets.AddUnit('psdcomn.pas');
    T:=P.Targets.AddUnit('fpreadpsd.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('psdcomn');
          AddUnit('fpcolorspace');
        end;
    T:=P.Targets.AddUnit('xwdfile.pp');
    T:=P.Targets.AddUnit('fpreadxwd.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('xwdfile');
        end;
    T:=P.Targets.AddUnit('fpwritebmp.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('bmpcomn');
        end;
    T:=P.Targets.AddUnit('fpwritegif.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('fpquantizer');
        end;
    T:=P.Targets.AddUnit('fpwritejpeg.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('jpegcomn');
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
    T:=P.Targets.AddUnit('fpwritetiff.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('fptiffcmn');
        end;
    T:=P.Targets.AddUnit('fpwritexpm.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
        end;
    T:=P.Targets.AddUnit('freetypeh.pp',[solaris,iphonesim,ios,darwin,freebsd,openbsd,netbsd,linux,haiku,beos,win32,win64,aix,dragonfly,android]);
      T.CPUS:=T.CPUS-[wasm32];
      T.Dependencies.AddInclude('libfreetype.inc');
    T:=P.Targets.AddUnit('freetypehdyn.pp',[solaris,iphonesim,ios,darwin,freebsd,openbsd,netbsd,linux,haiku,beos,win32,win64,aix,dragonfly,android]);
      T.ResourceStrings:=true;
      T.CPUS:=T.CPUS-[wasm32];
      T.Dependencies.AddInclude('libfreetype.inc');
    T:=P.Targets.AddUnit('freetype.pp',[solaris,iphonesim,ios,darwin,freebsd,openbsd,netbsd,linux,haiku,beos,win32,win64,aix,dragonfly,android]);
      with T.Dependencies do
        begin
          AddUnit('freetypeh');
          AddUnit('fpimgcmn');
        end;
    T:=P.Targets.AddUnit('ftfont.pp',[solaris,iphonesim,ios,darwin,freebsd,openbsd,netbsd,linux,haiku,beos,win32,win64,aix,dragonfly,android]);
      with T.Dependencies do
        begin
          AddUnit('fpcanvas');
          AddUnit('fpimgcmn');
          AddUnit('freetype');
          AddUnit('freetypeh');
          AddUnit('freetypehdyn');
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
      T.ResourceStrings:=true;
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('fpcanvas');
          AddUnit('fpimgcanv');
          AddInclude('pscorefonts.inc');
        end;
    T:=P.Targets.AddUnit('targacmn.pp');
    T:=P.Targets.AddUnit('fpimggauss.pp');
    With T.Dependencies do
      AddUnit('fpimage');

    T:=P.Targets.AddUnit('fpbarcode.pp');
    T:=P.Targets.AddUnit('fpimgbarcode.pp');
    With T.Dependencies do
      begin
      AddUnit('fpimage');
      AddUnit('fpcanvas');
      Addunit('fpimgcmn');
      AddUnit('fpbarcode');
      end;
    T:=P.Targets.AddUnit('fpqrcodegen.pp');
    T:=P.Targets.AddUnit('fpimgqrcode.pp');
    With T.Dependencies do
      begin
      AddUnit('fpimage');
      AddUnit('fpcanvas');
      Addunit('fpimgcmn');
      AddUnit('fpqrcodegen');
      end;
    // qoi
    T:=P.Targets.AddUnit('qoicomn.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('fpimgcmn');
        end;
    T:=P.Targets.AddUnit('fpreadqoi.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('qoicomn');
        end;
    T:=P.Targets.AddUnit('fpwriteqoi.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('qoicomn');
        end;
    T:=P.Targets.AddUnit('fpimagelist.pp');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
        end;
    T:=P.Targets.AddUnit('fpimgexif.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
        end;
    // webp
    T:=P.Targets.AddUnit('webpcomn.pas');
    T:=P.Targets.AddUnit('fpwebpvp8l.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
        end;
    T:=P.Targets.AddUnit('fpwebpvp8.pas');
      with T.Dependencies do
        begin
          AddInclude('fpwebpvp8tables.inc');
          AddUnit('fpimage');
          AddUnit('fpwebpvp8l');
        end;
    T:=P.Targets.AddUnit('fpreadwebp.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('fpimagelist');
          AddUnit('webpcomn');
          AddUnit('fpwebpvp8l');
          AddUnit('fpwebpvp8');
          AddUnit('fpimgexif');
        end;
    T:=P.Targets.AddUnit('fpwritewebp.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('webpcomn');
          AddUnit('fpwebpvp8l');
        end;
    // radiance hdr
    T:=P.Targets.AddUnit('hdrcomn.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
        end;
    T:=P.Targets.AddUnit('fpreadhdr.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('hdrcomn');
        end;
    T:=P.Targets.AddUnit('fpwritehdr.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('hdrcomn');
        end;
    // dds
    T:=P.Targets.AddUnit('fpreaddds.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
        end;
    // heif, through libheif
    T:=P.Targets.AddUnit('heifcomn.pas', [darwin,win32,win64,linux,freebsd,netbsd,openbsd]);
      with T.Dependencies do
        begin
          AddUnit('fpimage');
        end;
    T:=P.Targets.AddUnit('fpreadheif.pas', [darwin,win32,win64,linux,freebsd,netbsd,openbsd]);
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('fpimgexif');
          AddUnit('heifcomn');
        end;
    T:=P.Targets.AddUnit('fpwriteheif.pas', [darwin,win32,win64,linux,freebsd,netbsd,openbsd]);
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('heifcomn');
        end;
    // ico
    T:=P.Targets.AddUnit('icocomn.pas');
    T:=P.Targets.AddUnit('fpreadico.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('bmpcomn');
          AddUnit('fpreadbmp');
          AddUnit('fpreadpng');
          AddUnit('icocomn');
        end;
    T:=P.Targets.AddUnit('fpwriteico.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
          AddUnit('bmpcomn');
          AddUnit('fpwritepng');
          AddUnit('icocomn');
        end;
    T:=P.Targets.AddUnit('fpcolorspace.pas');
      with T.Dependencies do
        begin
          AddInclude('fpspectraldata.inc');
          AddUnit('fpimage');
        end;
    T:=P.Targets.AddUnit('fpunitofmeasure.pas');
      with T.Dependencies do
        begin
          AddUnit('fpimage');
        end;
    T:=P.Targets.AddUnit('fppapers.pas');
      with T.Dependencies do
        begin
          AddUnit('fpunitofmeasure');
          AddUnit('fpimage');
        end;

    P.ExamplePath.Add('examples');
    T:=P.Targets.AddExampleProgram('drawing.pp');
    T:=P.Targets.AddExampleProgram('imgconv.pp');
    T:=P.Targets.AddExampleProgram('createbarcode.lpr');
    T:=P.Targets.AddExampleProgram('wrpngf.pas');
    T:=P.Targets.AddExampleProgram('wrqoif.pas');
    T:=P.Targets.AddExampleProgram('canvasdemo.pp');
    T:=P.Targets.AddExampleProgram('convertframes.pp');

    P.NamespaceMap:='namespaces.lst';

{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}

