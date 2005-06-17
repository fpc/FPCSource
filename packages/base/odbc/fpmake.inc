    StartPackage('odbc');
    {$IF defined(ALLPACKAGES)}
    Directory:='base/odbc';
    {$ELSEIF defined(BASEPACKAGES)}
    Directory:='odbc';
    {$ENDIF}
    OS:=[linux,win32,netbsd,openbsd,freebsd,darwin];
    T:=Targets.AddUnit('odbcsql');
    T:=Targets.AddExampleunit('testodbc');
    EndPackage;
