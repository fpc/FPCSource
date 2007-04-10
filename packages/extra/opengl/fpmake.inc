    StartPackage('opengl');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/opengl';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='opengl';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,win32,darwin,win64];
    Dependencies.Add('x11');
    T:=Targets.AddUnit('gl');
    T:=Targets.AddUnit('glu');
    T:=Targets.AddUnit('glut');
    T:=Targets.AddUnit('glext');
    T:=Targets.AddUnit('dllfuncs');
    T.OS:=[linux,freebsd,netbsd,openbsd,darwin];
    T:=Targets.AddUnit('glx');
    T.OS:=[linux,freebsd,netbsd,openbsd,darwin];
    EndPackage;
