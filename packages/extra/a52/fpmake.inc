    StartPackage('a52');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/a52';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='a52';
    {$ENDIF}
    OS:=[linux,win32];
    T:=Targets.AddUnit('a52');
    EndPackage;
