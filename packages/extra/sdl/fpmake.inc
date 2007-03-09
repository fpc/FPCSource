    StartPackage('sdl');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/sdl';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='sdl';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,win32];
    Dependencies.Add('pthreads');
    Dependencies.Add('x11');
    Dependencies.Add('smpeg');
    T:=Targets.AddUnit('sdl');
    T:=Targets.AddUnit('sdl_image');
    T:=Targets.AddUnit('sdl_mixer');
    T:=Targets.AddUnit('sdl_ttf');
    T:=Targets.AddUnit('sdl_gfx');
    T:=Targets.AddUnit('sdl_net');
    T:=Targets.AddUnit('smpeg');
    EndPackage;
