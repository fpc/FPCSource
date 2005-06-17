    StartPackage('pthreads');
    {$IF defined(ALLPACKAGES)}
    Directory:='base/pthreads';
    {$ELSEIF defined(BASEPACKAGES)}
    Directory:='pthreads';
    {$ENDIF}
    OS:=[linux,freebsd,darwin];
    T:=Targets.AddUnit('pthreads');
    EndPackage;
