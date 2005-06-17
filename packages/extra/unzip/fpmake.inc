    StartPackage('unzip');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/unzip';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='unzip';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,win32,os2,emx,go32v2,netware,netwlibc,darwin];
    T:=Targets.AddUnit('ziptypes');
    T:=Targets.AddUnit('unzip');
    T:=Targets.AddUnit('unzipdll');
    T.OS:=[emx,os2];
    EndPackage;
