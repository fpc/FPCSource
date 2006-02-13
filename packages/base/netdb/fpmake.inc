    StartPackage('netdb');
    {$IF defined(ALLPACKAGES)}
    Directory:='base/netdb';
    {$ELSEIF defined(BASEPACKAGES)}
    Directory:='netdb';
    {$ENDIF}
    T:=Targets.AddUnit('uriparser');
    T:=Targets.AddUnit('netdb');
    T.OS:=[linux,freebsd,openbsd,netbsd,darwin,solaris];
    T:=Targets.AddExampleunit('testdns');
    T.OS:=[linux,freebsd,openbsd,netbsd,darwin,solaris];
    T:=Targets.AddExampleunit('testhst');
    T.OS:=[linux,freebsd,openbsd,netbsd,darwin,solaris];
    T:=Targets.AddExampleunit('testsvc');
    T.OS:=[linux,freebsd,openbsd,netbsd,darwin,solaris];
    T:=Targets.AddExampleunit('testnet');
    T.OS:=[linux,freebsd,openbsd,netbsd,darwin,solaris];
    T:=Targets.AddExampleunit('testuri');
    EndPackage;
