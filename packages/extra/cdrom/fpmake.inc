    StartPackage('cdrom');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/cdrom';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='cdrom';
    {$ENDIF}
    OS:=[linux,win32];
    T:=Targets.AddUnit('cdrom');
    T:=Targets.AddUnit('discid');
    T:=Targets.AddUnit('scsidefs');
    T.OS:=[win32];
    T:=Targets.AddUnit('wnaspi32');
    T.OS:=[win32];
    T:=Targets.AddUnit('cdromioctl');
    T.OS:=[win32];
    T:=Targets.AddUnit('wincd');
    T.OS:=[win32];
    T:=Targets.AddUnit('major');
    T.OS:=[linux];
    T:=Targets.AddUnit('lincd');
    T.OS:=[linux];
    T:=Targets.AddExampleunit('showcds');
    T:=Targets.AddExampleunit('getdiscid');
    EndPackage;
