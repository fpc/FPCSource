    StartPackage('ibase');
    {$IF defined(ALLPACKAGES)}
    Directory:='base/ibase';
    {$ELSEIF defined(BASEPACKAGES)}
    Directory:='ibase';
    {$ENDIF}
    OS:=[linux,win32,win64,netbsd,openbsd,freebsd,darwin];
    T:=Targets.AddUnit('ibase40');
    T:=Targets.AddUnit('ibase60');
    T:=Targets.AddUnit('ibase60dyn');
    T:=Targets.AddExampleunit('testib40');
    T:=Targets.AddExampleunit('testib60');
    EndPackage;
