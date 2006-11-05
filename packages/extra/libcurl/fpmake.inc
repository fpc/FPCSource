    StartPackage('libcurl');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/libcurl';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='libcurl';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,darwin];
    T:=Targets.AddUnit('libcurl');
    T:=Targets.AddExample('testcurl');
    T:=Targets.AddExample('teststream');
    EndPackage;
