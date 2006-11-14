    StartPackage('openssl');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/openssl';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='openssl';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,win32,win64,darwin,solaris];
    T:=Targets.AddUnit('openssl');
    EndPackage;
