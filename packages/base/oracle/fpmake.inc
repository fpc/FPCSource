    StartPackage('oracle');
    {$IF defined(ALLPACKAGES)}
    Directory:='base/oracle';
    {$ELSEIF defined(BASEPACKAGES)}
    Directory:='oracle';
    {$ENDIF}
    OS:=[linux,netbsd,openbsd,freebsd,darwin];
    T:=Targets.AddUnit('oraoci');
    EndPackage;
