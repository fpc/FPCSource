    StartPackage('libgd');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/libgd';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='libgd';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,darwin];
    T:=Targets.AddUnit('gd');
    T:=Targets.AddExampleunit('gdtest');
    EndPackage;
