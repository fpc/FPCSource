    StartPackage('rexx');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/rexx';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='rexx';
    {$ENDIF}
    OS:=[os2,emx];
    T:=Targets.AddUnit('rexxsaa');
    EndPackage;
