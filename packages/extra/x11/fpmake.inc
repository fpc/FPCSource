    StartPackage('x11');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/x11';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='x11';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,os2,emx,darwin];
    T:=Targets.AddUnit('x');
    T:=Targets.AddUnit('xlib');
    T:=Targets.AddUnit('xutil');
    T:=Targets.AddUnit('xresource');
    T:=Targets.AddUnit('xcms');
    T:=Targets.AddUnit('xshm');
    T:=Targets.AddUnit('xrender');
    T:=Targets.AddUnit('keysym');
    T:=Targets.AddUnit('xi');
    T:=Targets.AddUnit('xkb');
    T:=Targets.AddUnit('xkblib');
    T:=Targets.AddUnit('xatom');
    T:=Targets.AddUnit('xinerama');
    EndPackage;
