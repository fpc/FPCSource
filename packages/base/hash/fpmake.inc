    StartPackage('md5');
    {$IF defined(ALLPACKAGES)}
    Directory:='base/md5';
    {$ELSEIF defined(BASEPACKAGES)}
    Directory:='md5';
    {$ENDIF}
    Version:='2.0.0';
    T:=Targets.AddUnit('md5');
    T:=Targets.AddExampleunit('md5test');
    EndPackage;
