{ Make image directory and all subdirectories }
{ FULLFCL will be defined if included from main FCL directory }

Targets.ResetDefaults;
Targets.DefaultDir:='image';
T.ResourceStrings:=True;
T:=Targets.AddUnit('fpimgcmn');
T:=Targets.AddUnit('fpimage');
T:=Targets.AddUnit('pngcomn');
T:=Targets.AddUnit('fpreadpng');
T:=Targets.AddUnit('fpwritepng');
T:=Targets.AddUnit('fpreadxpm');
T:=Targets.AddUnit('fpwritexpm');
T:=Targets.AddUnit('clipping');
T:=Targets.AddUnit('fpcanvas');
T:=Targets.AddUnit('pixtools');
T:=Targets.AddUnit('fppixlcanv');
T:=Targets.AddUnit('fpimgcanv');
T:=Targets.AddUnit('pscanvas');
T.ResourceStrings:=True;
T:=Targets.AddUnit('fpwritebmp');
T:=Targets.AddUnit('fpreadbmp');
T:=Targets.AddUnit('bmpcomn');
T:=Targets.AddUnit('fpreadpnm');
T:=Targets.AddUnit('fpwritepnm');
T:=Targets.AddUnit('fpreadjpeg');
T:=Targets.AddUnit('fpwritejpeg');
T:=Targets.AddUnit('targacmn');
T:=Targets.AddUnit('fpreadtga');
T:=Targets.AddUnit('fpwritetga');
T:=Targets.AddUnit('ellipses');
T:=Targets.AddUnit('freetypeh');
T.OS:=[win32,linux,freebsd];
T:=Targets.AddUnit('freetype');
T.OS:=[win32,linux,freebsd];
T:=Targets.AddUnit('ftfont');
T.OS:=[win32,linux,freebsd];
Targets.AddExampleProgram('imgconv');
