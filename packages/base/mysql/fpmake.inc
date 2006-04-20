    StartPackage('mysql');
    {$IF defined(ALLPACKAGES)}
    Directory:='base/mysql';
    {$ELSEIF defined(BASEPACKAGES)}
    Directory:='mysql';
    {$ENDIF}
    OS:=[linux,win32,win64,netbsd,openbsd,freebsd,darwin];
    T:=Targets.AddUnit('mysql4_com');
    T:=Targets.AddUnit('mysql4_version');
    T:=Targets.AddUnit('mysql4');
    T:=Targets.AddUnit('mysql4dyn');
    T:=Targets.AddUnit('mysql4_comdyn');
    T:=Targets.AddUnit('mysql3_com');
    T:=Targets.AddUnit('mysql3_version');
    T:=Targets.AddUnit('mysql3');
    T:=Targets.AddUnit('mysql3_comdyn');
    T:=Targets.AddUnit('mysql3dyn');
    T:=Targets.AddExampleunit('testdb4');
    T:=Targets.AddExampleunit('testdb3');
    EndPackage;
