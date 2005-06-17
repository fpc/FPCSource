    StartPackage('ftpapi');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/os2units/ftpapi';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='os2units/ftpapi';
    {$ENDIF}
    OS:=[os2,emx];
    T:=Targets.AddUnit('ftpapi');
    T:=Targets.AddExampleunit('ftptest');
    EndPackage;
