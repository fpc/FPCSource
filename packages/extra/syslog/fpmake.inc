    StartPackage('syslog');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/syslog';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='syslog';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,darwin];
    T:=Targets.AddUnit('systemlog');
    T:=Targets.AddExampleunit('testlog');
    EndPackage;
