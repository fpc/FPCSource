    StartPackage('smpeg');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/smpeg';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='smpeg';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,win32];
    T:=Targets.AddUnit('smpeg');
    EndPackage;
