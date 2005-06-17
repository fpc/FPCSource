    StartPackage('postgres');
    {$IF defined(ALLPACKAGES)}
    Directory:='base/postgres';
    {$ELSEIF defined(BASEPACKAGES)}
    Directory:='postgres';
    {$ENDIF}
    OS:=[linux,win32,netbsd,openbsd,freebsd,darwin];
    T:=Targets.AddUnit('dllist');
    T:=Targets.AddUnit('dllistdyn');
    T:=Targets.AddUnit('postgres');
    T:=Targets.AddUnit('postgres3');
    T:=Targets.AddUnit('postgres3dyn');
    T:=Targets.AddExampleunit('testpg1');
    T:=Targets.AddExampleunit('testpg2');
    EndPackage;
