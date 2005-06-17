    StartPackage('utmp');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/utmp';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='utmp';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,darwin];
    T:=Targets.AddUnit('utmp');
    T:=Targets.AddExampleunit('testutmp');
    EndPackage;
