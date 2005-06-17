    StartPackage('newt');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/newt';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='newt';
    {$ENDIF}
    OS:=[linux];
    T:=Targets.AddUnit('newt');
    T:=Targets.AddExampleunit('newt1');
    T:=Targets.AddExampleunit('newt2');
    T:=Targets.AddExampleunit('newt3');
    EndPackage;
