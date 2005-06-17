    StartPackage('gconf');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/gnome1/gconf';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='gnome1/gconf';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,darwin];
    Dependencies.Add('gtk');
    T:=Targets.AddUnit('gconf/gconf');
    T:=Targets.AddUnit('gconfclient/gconfclient');
    EndPackage;
