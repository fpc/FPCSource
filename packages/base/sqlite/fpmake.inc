    StartPackage('sqlite');
    {$IF defined(ALLPACKAGES)}
    Directory:='base/sqlite';
    {$ELSEIF defined(BASEPACKAGES)}
    Directory:='sqlite';
    {$ENDIF}
    OS:=[linux,win32,freebsd,darwin];
    T:=Targets.AddUnit('sqlite');
    EndPackage;
