
    StartPackage('gnome');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/gnome1/gnome';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='gnome1/gnome';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,darwin];
    Dependencies.Add('gtk');
    Dependencies.Add('imlib');
    T:=Targets.AddUnit('libart_lgpl/libart');
    T:=Targets.AddUnit('libgnome/libgnome');
    T:=Targets.AddUnit('libgnomeui/libgnomeui');
    EndPackage;

