    StartPackage('hwvideo');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/os2units/hwvideo';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='os2units/hwvideo';
    {$ENDIF}
    OS:=[os2,emx];
    T:=Targets.AddUnit('hwvideo');
    EndPackage;
