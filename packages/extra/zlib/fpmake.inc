    StartPackage('zlib');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/zlib';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='zlib';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,win32,os2,emx,netware,netwlibc,darwin];
    T:=Targets.AddUnit('zlib');
    EndPackage;
