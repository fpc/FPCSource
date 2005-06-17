    StartPackage('gdbm');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/gdbm';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='gdbm';
    {$ENDIF}    
    OS:=[linux,netbsd,freebsd,openbsd,darwin];
    T:=Targets.AddUnit('gdbm');
    T:=Targets.AddExampleunit('testgdbm');
    T:=Targets.AddExampleunit('testgdbm2');
    EndPackage;
