    StartPackage('mmtk');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/os2units/mmtk';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='os2units/mmtk';
    {$ENDIF}
    OS:=[os2,emx];
    T:=Targets.AddUnit('mmbase');
    T:=Targets.AddUnit('sw');
    T:=Targets.AddUnit('dive');
    T:=Targets.AddUnit('mci');
    T:=Targets.AddUnit('mciapi');
    T:=Targets.AddUnit('mcidrv');
    T:=Targets.AddUnit('mmio');
    EndPackage;
