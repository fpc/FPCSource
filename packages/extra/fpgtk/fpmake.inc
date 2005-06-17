    StartPackage('fpgtk');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/fpgtk';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='fpgtk';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,win32,os2,emx,darwin];
    Dependencies.Add('fcl');
    Dependencies.Add('gtk');
    T:=Targets.AddUnit('fpglib');
    T:=Targets.AddUnit('fpgtk');
    T.ResourceStrings:=True;
    T:=Targets.AddUnit('fpgtkext');
    T.ResourceStrings:=True;
    EndPackage;
