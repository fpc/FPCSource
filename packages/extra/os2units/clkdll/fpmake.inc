    StartPackage('clkdll');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/os2units/clkdll';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='os2units/clkdll';
    {$ENDIF}
    OS:=[os2,emx];
    T:=Targets.AddUnit('clkdll');
    T:=Targets.AddExampleunit('clktest');
    EndPackage;
