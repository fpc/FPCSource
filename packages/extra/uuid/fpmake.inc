    StartPackage('uuid');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/uuid';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='uuid';
    {$ENDIF}
    OS:=[linux];
    T:=Targets.AddUnit('uuid');
    T:=Targets.AddUnit('libuuid');
    T:=Targets.AddExampleunit('testuid');
    T:=Targets.AddExampleunit('testlibuid');
    EndPackage;
