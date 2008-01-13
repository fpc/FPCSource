    StartPackage('winunits-base');
    {$IF defined(ALLPACKAGES)}
    Directory:='winunits-base';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='winunits-base';
    {$ENDIF}
    OS:=[win32,win64];
    T:=Targets.AddUnit('buildwinutils');
    T.Install:=False;
    T:=Targets.AddUnit('activex');
    T:=Targets.AddUnit('comconst');
    T:=Targets.AddUnit('commctrl');
    T:=Targets.AddUnit('comobj');
    T:=Targets.AddUnit('flatsb');
    T:=Targets.AddUnit('mmsystem');
    T:=Targets.AddUnit('ole2');
    T:=Targets.AddUnit('oleserver');
    T:=Targets.AddUnit('richedit');
    T:=Targets.AddUnit('shellapi');
    T:=Targets.AddUnit('shfolder');
    T:=Targets.AddUnit('shlobj');
    T:=Targets.AddUnit('winver');
    T:=Targets.AddExampleunit('examples');
    EndPackage;
