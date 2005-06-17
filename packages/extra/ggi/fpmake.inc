    StartPackage('ggi');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/ggi';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='ggi';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,darwin];
    T:=Targets.AddUnit('gii');
    T:=Targets.AddUnit('ggi');
    T:=Targets.AddUnit('ggi2d');
    T:=Targets.AddExampleunit('ggi1');
    EndPackage;
