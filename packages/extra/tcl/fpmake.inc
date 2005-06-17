    StartPackage('tcl');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/tcl';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='tcl';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,win32,os2,emx,darwin];
    Dependencies.Add('fcl');
    T:=Targets.AddUnit('tcl80');
    T:=Targets.AddExampleunit('tcl_demo');
    EndPackage;
