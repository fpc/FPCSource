    StartPackage('gdbint');
    {$IF defined(ALLPACKAGES)}
    Directory:='base/gdbint';
    {$ELSEIF defined(BASEPACKAGES)}
    Directory:='gdbint';
    {$ENDIF}
    OS:=[linux,win32,go32v2,netbsd,openbsd,freebsd];
    T:=Targets.AddUnit('gdbint');
    T:=Targets.AddUnit('gdbcon');
    T:=Targets.AddExampleunit('testgdb');
    T:=Targets.AddExampleunit('symify');
    EndPackage;
